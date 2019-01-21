#include "rt-lisp.h"

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

static
rtl_Generation *mkGeneration(int genNbr)
{
  rtl_Generation *gen;
  size_t         capacity;

  // We have 27 address bits available to us.
  capacity = __rtl_genCapacity(genNbr);

  gen = malloc(sizeof(rtl_Generation)
	       + capacity*sizeof(rtl_Word));

  gen->nbr      = genNbr;
  gen->fillPtr  = 0;
  gen->capacity = capacity;
  gen->marks    = rtl_newBitMap(capacity);

  return gen;
}

void rtl_initHeap(rtl_Heap *h)
{
  memset(h->gen, 0, sizeof(rtl_Generation *)*RTL_MAX_GENERATIONS);
}

static inline
rtl_Word *reifyPtr(rtl_Machine *M, rtl_Word ptr)
{
  uint32_t       genNum,
                 offs;
  rtl_Generation *gen;

  genNum = __rtl_ptrGen(ptr);

  assert(genNum < RTL_MAX_GENERATIONS);
  assert(NULL != M->heap.gen[genNum]);

  gen = M->heap.gen[genNum];

  offs = __rtl_ptrOffs(ptr);

  assert(offs < gen->capacity);

  return gen->words + offs;
}

rtl_Word const *rtl_reifyTuple(rtl_Machine *M, rtl_Word tpl, size_t *len) {
  rtl_Word *backing;

#ifndef NDEBUG
  if (!len) {
    M->error = RTL_ERR_INVALID_OPERATION;
    return NULL;
  }
#endif

  if (!rtl_isTuple(tpl)) {
    M->error = RTL_ERR_EXPECTED_TUPLE;
    *len = 0;
    return NULL;
  }

  backing = reifyPtr(M, tpl);
  if (backing) {
    *len = (size_t)((*backing) >> 4);
    return backing + 1;
  } else {
    *len = 0;
    return NULL;
  }
}

rtl_Word const *rtl_reifyCons(rtl_Machine *M, rtl_Word cons)
{
  if (!rtl_isCons(cons)) {
    M->error = RTL_ERR_EXPECTED_CONS;
    return NULL;
  }

  return reifyPtr(M, cons);
}

rtl_Word rtl_car(rtl_Machine *M, rtl_Word cons)
{
  rtl_Word const *ptr;

  if (cons == RTL_NIL) return RTL_NIL;

  ptr = rtl_reifyCons(M, cons);

  if (!ptr) return RTL_NIL;

  return ptr[0];
}

rtl_Word rtl_cdr(rtl_Machine *M, rtl_Word cons)
{
  rtl_Word const *ptr;

  if (cons == RTL_NIL) return RTL_NIL;

  ptr = rtl_reifyCons(M, cons);

  if (!ptr) return RTL_NIL;

  return ptr[1];
}

static
rtl_Word mkPtr(rtl_WordType t, uint32_t gen, uint32_t offs)
{
  assert(offs < __rtl_genCapacity(gen));

  return ((__rtl_genCapacity(gen) | offs) << 4) | t;
}

// Mark the memory w points to, for generation g.
static
void markWord(rtl_Machine *M, rtl_Generation *gen, rtl_Word w) {
  uint32_t       wOffs;
  size_t         i,
                 len;
  rtl_Word const *fields;
  rtl_Generation *wGen;

  wOffs = __rtl_ptrOffs(w);

  // Don't bother with pointers into other generations.
  if (__rtl_ptrGen(w) != gen->nbr) return;

  switch (rtl_typeOf(w)) {
  case RTL_TUPLE:
    fields = rtl_reifyTuple(M, w, &len);

    // Mark the length word ..
    rtl_bmpSetBit(gen->marks, wOffs, true);

    // .. then each of the element words.
    for (i = 0; i < len; i++) {
      if (!rtl_bmpSetBit(gen->marks, wOffs + i + 1, true)) {
	markWord(M, gen, fields[i]);
      }
    }
    break;

  case RTL_STRING:
    // TODO: Implement strings.
    break;

  case RTL_RECORD:
    // TODO: Implement records.
    break;

  case RTL_CONS:
    fields = rtl_reifyCons(M, w);

    // Mark CAR then CDR
    wGen = M->heap.gen[__rtl_ptrGen(w)];
    if (!rtl_bmpSetBit(wGen->marks, wOffs, true)) {
      markWord(M, gen, fields[0]);
    }
    if (!rtl_bmpSetBit(wGen->marks, wOffs + 1, true)) {
      markWord(M, gen, fields[1]);
    }
    break;
  }
}

static
rtl_Word moveWord(rtl_Machine *M, int highestGen, rtl_Word w)
{
  int g;
  rtl_WordType type;
  uint32_t oldOffs, newOffs;
  rtl_Generation *gen, *nextGen;

  if (!rtl_isPtr(w)) return w;

  g = __rtl_ptrGen(w);
  if (g > highestGen) return w;

  type = rtl_typeOf(w);

  gen = M->heap.gen[g];
  assert(gen);

  nextGen = M->heap.gen[g + 1];
  if (!nextGen)
    nextGen = M->heap.gen[g + 1] = mkGeneration(g + 1);

  oldOffs = __rtl_ptrOffs(w);
  newOffs = rtl_bmpRank(gen->marks, oldOffs)
          + nextGen->preMoveFillPtr;

  return mkPtr(type, g + 1, newOffs);
}

// Returns the number of the highest generation that was collected.
static
int collectGen(rtl_Machine *M, int g)
{
  size_t i, j, k, count, space;
  rtl_Generation *gen, *youngerGen, *nextGen;
  rtl_BitMap *bmp;
  int highest;
  rtl_Word **pp;

  // Next generation needs to exist in order for us to collect. Maybe in the
  // future we could add the ability to compact the maximum generation in-place,
  // but for now we just consider ourselves OOM if the last generation needs to
  // be collected.
  if (g + 1 >= RTL_MAX_GENERATIONS) {
    M->error = RTL_ERR_OUT_OF_MEMORY;
    return -1;
  }

  gen = M->heap.gen[g];
  if (!gen) {
    // If this generation doesn't exist yet, then it's already empty so there's
    // no need to collect.
    gen = M->heap.gen[g] = mkGeneration(g);
    return g - 1;
  }

  // Clear any marks from previous collections and start w/ a clean slate.
  rtl_bmpClearAll(gen->marks);

  // Mark any pointers into this generation in ..

  // .. the current environment frame ..
  markWord(M, gen, M->envFrame);

  // .. any words on the value stack ..
  for (i = 0; i < M->vStackLen; i++) {
    markWord(M, gen, M->vStack[i]);
  }

  // .. any environment frames on the return stack ..
  for (i = 0; i < M->rStackLen; i++) {
    markWord(M, gen, M->rStack[i].envFrame);
  }

  // .. any words in live working sets ..
  for (i = 0; i < M->wsStackLen; i++) {
    for (pp = M->wsStack[i]; *pp != NULL; pp++) {
      markWord(M, gen, **pp);
    }
  }

  // .. and any live words in younger generations.
  for (i = 0; i < g; i++) {
    youngerGen = M->heap.gen[i];
    assert(youngerGen); // Shouldn't be any un-populated younger generations, I
			// think.
    bmp = youngerGen->marks;
    for (j = 0; j < bmp->nbrOnes; j++) {
      k = rtl_bmpSelect(bmp, j);
      markWord(M, gen, youngerGen->words[k]);
    }
  }

  // Compute rank/select tables (etc..) for the mark bitmap.
  rtl_bmpTabulate(gen->marks);

  // Get the next generation (into which we're gonna move all live words from
  // this generation).
  nextGen = M->heap.gen[g + 1];
  if (!nextGen) {
    nextGen = M->heap.gen[g + 1] = mkGeneration(g + 1);
  }

  if (gen->marks->nbrOnes > nextGen->capacity - nextGen->fillPtr) {
    // Not enough room in the next generation -- need to collect it too.
    highest = collectGen(M, g + 1);
  } else {
    highest = g;
  }

  // If there was an error, just return up the stack.
  if (highest < 0) return highest;

  // This is where we record the pre-move fill ptr, which makes all of the
  // rank/select math work consistently even after we start adjusting the
  // real fill ptr to add items to the generation.
  nextGen->preMoveFillPtr = nextGen->fillPtr;

  for (i = 0; i < gen->marks->nbrOnes; i++) {
    rtl_Word old, new;

    k = rtl_bmpSelect(gen->marks, i);

    nextGen->words[nextGen->fillPtr++] = new
                                       = moveWord(M,
						  highest,
						  old = gen->words[k]);
  }

  gen->fillPtr = 0;

  // This is the last generation to be moved, now we need to fix-up the pointers
  // in the machine.
  if (g == 0) {
    rtl_Word old, new;

    // .. the current environment frame ..
    M->envFrame = moveWord(M, highest, M->envFrame);

    // .. any words on the value stack ..
    for (i = 0; i < M->vStackLen; i++) {
      M->vStack[i] = new = moveWord(M, highest, old = M->vStack[i]);
    }

    // .. any environment frames on the return stack ..
    for (i = 0; i < M->rStackLen; i++) {
      M->rStack[i].envFrame = new = moveWord(M, highest, old = M->rStack[i].envFrame);
    }
  }

  return highest;
}

rtl_Word *rtl_allocGC(rtl_Machine *M, rtl_WordType t, rtl_Word *w, size_t nbr)
{
  rtl_Heap       *heap;
  rtl_Generation *gen0;
  size_t         offs;

  // Validate t, ensure it's one of the accepted types.
  switch (t) {
  case RTL_TUPLE:
  case RTL_STRING:
  case RTL_RECORD:
  case RTL_CONS:
    break;

  default:
    M->error = RTL_ERR_INVALID_OPERATION;
    return NULL;
  }

  heap = &M->heap;
  gen0 = heap->gen[0];

  // Allocate this generation if it doesn't exist.
  if (!gen0) gen0 = heap->gen[0] = mkGeneration(0);

  if (gen0->capacity - gen0->fillPtr < nbr) {
    collectGen(M, 0);
  }

  offs = gen0->fillPtr;
  gen0->fillPtr += nbr;

  *w = mkPtr(t, 0, offs);

  return gen0->words + offs;
}

void rtl_initMachine(rtl_Machine *M)
{
  rtl_initHeap(&M->heap);

  M->envFrame = RTL_NIL;

  M->vStack    = malloc(64*sizeof(rtl_Word));
  M->vStackLen = 0;
  M->vStackCap = 64;

  M->rStack    = malloc(64*sizeof(rtl_RetAddr));
  M->rStackLen = 0;
  M->rStackCap = 64;

  M->pc = NULL;

  M->wsStack    = NULL;
  M->wsStackLen = 0;
  M->wsStackCap = 0;

  M->pages    = NULL;
  M->pagesLen = 0;
  M->pagesCap = 0;

  M->error = RTL_OK;
}

static
void pushVStack(rtl_Machine *M, rtl_Word w)
{
  if (M->vStackLen == M->vStackCap) {
    M->vStackCap = M->vStackCap == 0 ? 64 : M->vStackCap * 2;
    M->vStack = realloc(M->vStack, M->vStackCap * sizeof(rtl_Word));
  }

  M->vStack[M->vStackLen++] = w;
}

static
rtl_Word popVStack(rtl_Machine *M)
{
  if (M->vStackLen > 0) {
    return M->vStack[--M->vStackLen];
  }

  M->error = RTL_ERR_STACK_UNDERFLOW;
  return RTL_NIL;
}

rtl_Word rtl_cons(rtl_Machine *M, rtl_Word car, rtl_Word cdr)
{
  rtl_Word w, *ptr;

  pushVStack(M, cdr);
  pushVStack(M, car);

  ptr = rtl_allocGC(M, RTL_CONS, &w, 2);

  ptr[0] = popVStack(M);
  ptr[1] = popVStack(M);

  return w;
}

void rtl_testGarbageCollector(size_t count)
{
  rtl_Machine    M;
  rtl_Word       w;
  rtl_Word const *ptr;
  int            i;

  rtl_initMachine(&M);

  M.vStackLen = 1;

  // Cons up a gigantic linked list.
  for (i = 0, M.vStack[0] = RTL_NIL; i < count; i++) {
    M.vStack[0] = rtl_cons(&M, rtl_int28(i), M.vStack[0]);
    if (rtl_getError(&M)) {
      printf("There's an error!\n");
      abort();
    }
  }

  for (i = count - 1, ptr = rtl_reifyCons(&M, M.vStack[0]);
       !rtl_isNil(ptr[1]);
       i--, ptr = rtl_reifyCons(&M, ptr[1]))
  {
    if (i != rtl_int28Value(ptr[0])) {
      printf("reifying cons, expected Int28 %d, got %s %d\n",
	     i, rtl_typeNameOf(ptr[0]), (int)rtl_int28Value(ptr[0]));
    }
  }

  // Create a couple of stray cons cells pointing to each cell in a list.

  rtl_Word straggler0, straggler1;

  for (i = 0, M.vStack[0] = RTL_NIL; i < count; i++) {
    M.vStack[0] = rtl_cons(&M, rtl_int28(i), M.vStack[0]);

    straggler0 = rtl_cons(&M, rtl_int28(-1), M.vStack[0]);
    straggler1 = rtl_cons(&M, rtl_int28(-2), straggler0);

    if (rtl_getError(&M)) {
      printf("There's an error!\n");
      abort();
    }
  }

  for (i = count - 1, ptr = rtl_reifyCons(&M, M.vStack[0]);
       !rtl_isNil(ptr[1]);
       i--, ptr = rtl_reifyCons(&M, ptr[1]))
  {
    if (i != rtl_int28Value(ptr[0])) {
      printf("reifying cons, expected Int28 %d, got %s %d\n",
	     i, rtl_typeNameOf(ptr[0]), (int)rtl_int28Value(ptr[0]));
    }
  }
}

char const *rtl_typeName(rtl_WordType type)
{
  switch (type) {
  case RTL_NIL:
    return "Nil";

  case RTL_SYMBOL:
    return "Symbol";

  case RTL_SELECTOR:
    return "Selector";

  case RTL_INT28:
    return "Int28";

  case RTL_FIX14:
    return "Fix14";

  case RTL_TUPLE:
    return "Tuple";

  case RTL_STRING:
    return "String";

  case RTL_RECORD:
    return "Record";

  case RTL_CONS:
    return "Cons";

  case RTL_TOP:
    return "Top";

  case RTL_LENGTH:
    return "[Length (impl detail)]";

  case RTL_ADDR:
    return "[Addr (impl detail)]";

  default:
    return "[Unknown RTL type]";
  }
}

// These macros are to be used for operating on the VM from the main
// loop. Hopefully this gives the compiler more freedom to keep things in
// registers, etc...

#define likely(x)       __builtin_expect(!!(x),1)
#define unlikely(x)     __builtin_expect((x),0)

#define VPUSH(W) ({							\
      if (unlikely(vStackLen == vStackCap)) {				\
	vStackCap = vStackCap == 0 ? 64 : vStackCap * 2;		\
	vStack    = realloc(vStack, sizeof(rtl_Word)*vStackCap);	\
      }									\
									\
      vStack[vStackLen++] = (W);					\
    })									\
  // End of multi-line macro

#define VSTACK_ASSERT_LEN(N) ({			\
      if (unlikely((N) > vStackLen)) {		\
	err = RTL_ERR_STACK_UNDERFLOW;		\
	goto interp_cleanup;			\
      }						\
    })						\
  // End of multi-line macro

#define RSTACK_ASSERT_LEN(N) ({			\
      if (unlikely((N) > rStackLen)) {		\
	err = RTL_ERR_STACK_UNDERFLOW;		\
	goto interp_cleanup;			\
      }						\
    })						\
  // End of multi-line macro

#define VPOP() (vStack[--vStackLen])

#define VPOPK(K) ({ vStackLen -= (K); })

#define VPEEK(N) (vStack[vStackLen - ((N) + 1)])

#define SAVE_MACHINE() ({			\
      M->pc = pc;				\
						\
      M->vStack    = vStack;			\
      M->vStackLen = vStackLen;			\
      M->vStackCap = vStackCap;			\
						\
      M->rStack    = rStack;			\
      M->rStackLen = rStackLen;			\
      M->rStackCap = rStackCap;			\
						\
      M->error = err;				\
    })						\
  // End of multi-line macro

#define RESTORE_MACHINE() ({			\
      pc = M->pc;				\
						\
      vStack    = M->vStack;			\
      vStackLen = M->vStackLen;			\
      vStackCap = M->vStackCap;			\
						\
      rStack    = M->rStack;			\
      rStackLen = M->rStackLen;			\
      rStackCap = M->rStackCap;			\
						\
      err = M->error;				\
    })						\
  // End of multi-line macro


// This is the decode/dispatch template for all hard-coded binary operators.
#define BINARY_OP(INAME, OP, TYPE_TEST, TYPE_ERR, TYPE_MK, TYPE_VAL)	\
      case RTL_OP_##INAME:						\
	VSTACK_ASSERT_LEN(2);						\
									\
	a = VPOP();							\
	b = VPOP();							\
									\
	if (unlikely(!TYPE_TEST(a)) || unlikely(!TYPE_TEST(b))) {	\
	  err = TYPE_ERR;						\
	  goto interp_cleanup;						\
	}								\
									\
	VPUSH(TYPE_MK(TYPE_VAL(a) OP TYPE_VAL(b)));			\
	break;								\
  // end of multi-line macro

rtl_Error rtl_run(rtl_Machine *M, rtl_Word addr)
{
  uint8_t  *pc;
  rtl_Word *vStack;
  size_t   vStackLen,
           vStackCap;

  rtl_RetAddr *rStack;
  size_t       rStackLen,
               rStackCap;

  rtl_Error err;

  rtl_Word literal;

  // Some scratch space:
  rtl_Word a, b, c, d;
  rtl_Word f, g;
  rtl_Word const *ptr;

  M->pc = rtl_resolveAddr(M, addr);

  // Load up all of the "register" values.
  RESTORE_MACHINE();

  while (err == RTL_OK) {
    rtl_disasm(pc);

    switch (*pc) {
    case RTL_OP_NOP:
      break;

    case RTL_OP_CONST:
      literal = (uint32_t)pc[1] << 0
	      | (uint32_t)pc[2] << 8
	      | (uint32_t)pc[3] << 16
	      | (uint32_t)pc[4] << 24 ;

      pc += 4;

      VPUSH(literal);
      break;

    case RTL_OP_CONST_NIL:
      VPUSH(RTL_NIL);
      break;

    case RTL_OP_CONST_TOP:
      VPUSH(RTL_TOP);
      break;

    case RTL_OP_CONS:
      VSTACK_ASSERT_LEN(2);

      b = VPEEK(0);
      a = VPEEK(1);

      SAVE_MACHINE();

      c = rtl_cons(M, a, b);

      RESTORE_MACHINE();

      VPOPK(2);
	
      VPUSH(c);
      break;

    case RTL_OP_CAR:
      VSTACK_ASSERT_LEN(1);

      // Don't need to SAVE_MACHINE() here, since reifyCons doesn't look at any
      // of the un-saved fields of M.
      ptr = rtl_reifyCons(M, VPOP());
      if (unlikely(!ptr)) {
	err = M->error;
	goto interp_cleanup;
      }

      VPUSH(ptr[0]);
      break;

    case RTL_OP_CDR:
      VSTACK_ASSERT_LEN(1);

      ptr = rtl_reifyCons(M, VPOP());
      if (unlikely(!ptr)) {
	err = M->error;
	goto interp_cleanup;
      }

      VPUSH(ptr[1]);
      break;

    case RTL_OP_POP:
      VSTACK_ASSERT_LEN(1);

      VPOPK(1);
      break;

    case RTL_OP_SWAP:
      VSTACK_ASSERT_LEN(2);

      b = VPOP();
      a = VPOP();

      VPUSH(b);
      VPUSH(a);
      break;

    case RTL_OP_DUP:
      VSTACK_ASSERT_LEN(1);

      VPUSH(VPEEK(0));
      break;

    case RTL_OP_IS_INT28:
      VSTACK_ASSERT_LEN(1);

      a = VPOP();
      VPUSH(rtl_isInt28(a) ? RTL_TOP : RTL_NIL);
      break;

    case RTL_OP_IS_FIX14:
      VSTACK_ASSERT_LEN(1);

      a = VPOP();
      VPUSH(rtl_isFix14(a) ? RTL_TOP : RTL_NIL);
      break;

    case RTL_OP_IS_SYMBOL:
      VSTACK_ASSERT_LEN(1);

      a = VPOP();
      VPUSH(rtl_isSymbol(a) ? RTL_TOP : RTL_NIL);
      break;

    // `nil?' and `not' are actually the same function.
    case RTL_OP_IS_NIL:
    case RTL_OP_NOT:
      VSTACK_ASSERT_LEN(1);

      a = VPOP();
      VPUSH(rtl_isNil(a) ? RTL_TOP : RTL_NIL);
      break;

    case RTL_OP_IS_CONS:
      VSTACK_ASSERT_LEN(1);

      a = VPOP();
      VPUSH(rtl_isCons(a) ? RTL_TOP : RTL_NIL);
      break;

    case RTL_OP_IS_TUPLE:
      VSTACK_ASSERT_LEN(1);

      a = VPOP();
      VPUSH(rtl_isTuple(a) ? RTL_TOP : RTL_NIL);
      break;

    case RTL_OP_CJMP:
      VSTACK_ASSERT_LEN(1);

      if (rtl_isNil(VPOP())) {
	break;
      }

      // Intentionally fallthrough into JMP ...

    case RTL_OP_JMP:
      literal = (uint32_t)pc[1] << 0
	      | (uint32_t)pc[2] << 8
	      | (uint32_t)pc[3] << 16
	      | (uint32_t)pc[4] << 24 ;

      pc = rtl_resolveAddr(M, literal);
      break;

    case RTL_OP_CALL:
      VSTACK_ASSERT_LEN(1);
      f = VPOP();

      printf("rtl: CALL instruction not yet implemented!\n");

      if (unlikely(rtl_isSymbol(f))) {
	// TODO: Lookup functions by name
      }

      switch (rtl_typeOf(f)) {
      case RTL_SELECTOR:
      case RTL_RECORD:
      case RTL_ADDR:
      case RTL_BUILTIN:
      case RTL_CLOSURE:
      default:
	break;
      }

      break;

    case RTL_OP_UNDEFINED_FUNCTION:
      literal = (rtl_Word)pc[1] << 0
	      | (rtl_Word)pc[2] << 8
	      | (rtl_Word)pc[3] << 16
	      | (rtl_Word)pc[4] << 24 ;

      printf("tried to call undefined function: '%s'\n",
	     rtl_symbolName(literal));

      break;

    case RTL_OP_RETURN:
      // For now, the RETURN instruction just exits the interpreter -- we'll
      // handle function calls later.
      goto interp_cleanup;


    // Generate code for binary operations using the BINARY_OP macro.
    BINARY_OP(IADD, +, rtl_isInt28, RTL_ERR_EXPECTED_INT28, rtl_int28, rtl_int28Value);
    BINARY_OP(ISUB, -, rtl_isInt28, RTL_ERR_EXPECTED_INT28, rtl_int28, rtl_int28Value);
    BINARY_OP(IMUL, *, rtl_isInt28, RTL_ERR_EXPECTED_INT28, rtl_int28, rtl_int28Value);
    BINARY_OP(IDIV, /, rtl_isInt28, RTL_ERR_EXPECTED_INT28, rtl_int28, rtl_int28Value);
    BINARY_OP(IMOD, %, rtl_isInt28, RTL_ERR_EXPECTED_INT28, rtl_int28, rtl_int28Value);

    BINARY_OP(FADD, +, rtl_isFix14, RTL_ERR_EXPECTED_FIX14, rtl_fix14, rtl_fix14Value);
    BINARY_OP(FSUB, -, rtl_isFix14, RTL_ERR_EXPECTED_FIX14, rtl_fix14, rtl_fix14Value);
    BINARY_OP(FMUL, *, rtl_isFix14, RTL_ERR_EXPECTED_FIX14, rtl_fix14, rtl_fix14Value);
    BINARY_OP(FDIV, /, rtl_isFix14, RTL_ERR_EXPECTED_FIX14, rtl_fix14, rtl_fix14Value);

    default:
      printf("Unhandled instruction: opcode %d\n", (int)*pc);
      break;
    }

    if (likely(err == RTL_OK)) pc++;
  }

 interp_cleanup:
  SAVE_MACHINE();

  return rtl_getError(M);
}

rtl_Error rtl_runSnippet(rtl_Machine *M, uint8_t *code, uint16_t len)
{
  uint16_t pageID = rtl_newPageID(M);

  for (uint16_t i = 0; i < len; i++) {
    rtl_emitByteToPage(M, pageID, code[i]);
  }

  return rtl_run(M, rtl_addr(pageID, 0));
}

uint16_t rtl_newPageID(rtl_Machine *M)
{
  rtl_Page *page;

  if (M->pagesCap == M->pagesLen) {
    M->pagesCap = M->pagesCap == 0 ? 32 : 2*M->pagesCap;
    M->pages    = realloc(M->pages, sizeof(rtl_Page *)*M->pagesCap);
  }

  page = malloc(sizeof(rtl_Page));

  page->len     = 0;
  page->cap     = 0;
  page->version = 0;

  M->pages[M->pagesLen] = page;

  return M->pagesLen++;
}

void rtl_installPage(rtl_Machine *M, uint16_t id, rtl_Page *page)
{
  assert(id < M->pagesLen);

  page->version = M->pages[id]->version + 1;

  free(M->pages[id]);
  M->pages[id] = page;
}

void rtl_newPageVersion(rtl_Machine *M, uint16_t pageID)
{
  rtl_Page *oldPage, *newPage;

  oldPage = M->pages[pageID];
  newPage = malloc(sizeof(rtl_Page));

  newPage->len     = 0;
  newPage->cap     = 0;
  newPage->version = oldPage->version + 1;

  // TODO: In the future, maybe we'd want to refcount here and share pages
  //       between machines?
  //
  //       Just a thought... definitely not something I have time for right
  //       now...
  free(oldPage);

  M->pages[pageID] = newPage;
}

/* static */
/* uint16_t rtl_installNewPage(rtl_Machine *M, rtl_Page *page) */
/* { */
/*   uint16_t id; */

/*   id = rtl_newPageID(M); */

/*   rtl_installPage(M, id, page); */

/*   return id; */
/* } */

rtl_Page *rtl_getPageByID(rtl_Machine *M, uint16_t id)
{
  assert(id < M->pagesLen);
  return M->pages[id];
}

void rtl_emitByteToPage(rtl_Machine *M, uint16_t pageID, uint8_t b)
{
  rtl_Page *page;
  rtl_Page *newPage;

  assert(pageID < M->pagesLen);
  page = M->pages[pageID];

  if (unlikely(page->cap == page->len)) {
    page->cap = !page->cap ? 32 : 4*page->cap/3;
    newPage   = malloc(sizeof(rtl_Page) + page->cap);

    newPage->len     = page->len;
    newPage->cap     = page->cap;
    newPage->version = page->version;

    memcpy(newPage->code, page->code, page->len);

    free(page);
    page = M->pages[pageID]
         = newPage;
  }

  page->code[page->len++] = b;
}

void rtl_emitWordToPage(rtl_Machine *M, uint16_t pageID, rtl_Word w)
{
  rtl_emitByteToPage(M, pageID, (w >>  0) & 0xFF);
  rtl_emitByteToPage(M, pageID, (w >>  8) & 0xFF);
  rtl_emitByteToPage(M, pageID, (w >> 16) & 0xFF);
  rtl_emitByteToPage(M, pageID, (w >> 24) & 0xFF);
}

rtl_Word rtl_reverseListImproper(rtl_Machine *M, rtl_Word ls, rtl_Word last)
{
  rtl_Word result = RTL_NIL;

  RTL_PUSH_WORKING_SET(M, &ls, &last, &result);

  for (result = last; ls != RTL_NIL; ls = rtl_cdr(M, ls)) {
    result = rtl_cons(M, rtl_car(M, ls), result);
  }

  return result;
}

void rtl_pushWorkingSet(rtl_Machine *M, rtl_WorkingSet ws)
{
  if (M->wsStackLen == M->wsStackCap) {
    M->wsStackCap = !M->wsStackCap ? 8 : 2*M->wsStackCap;
    M->wsStack = realloc(M->wsStack, M->wsStackCap*sizeof(rtl_WorkingSet *));
  }

  M->wsStack[M->wsStackLen++] = ws;
}

void rtl_popWorkingSet(rtl_Machine *M)
{
  assert(M->wsStackLen > 0);

  M->wsStackLen--;
}
