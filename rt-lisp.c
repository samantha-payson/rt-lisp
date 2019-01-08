#include "rt-lisp.h"

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

// Return the number of words in generation genNbr. This will always return a
// power of 2.
static
size_t genCapacity(int genNbr)
{
  return 1 << ((27 + genNbr) - RTL_MAX_GENERATIONS);
}

static
rtl_Generation *mkGeneration(int genNbr)
{
  rtl_Generation *gen;
  size_t         capacity;

  // We have 27 address bits available to us.
  capacity = genCapacity(genNbr);

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

// Return the generation number of a pointer type.
static inline
int ptrGen(rtl_Word ptr) {
  return RTL_MAX_GENERATIONS - __builtin_clz(ptr);
}

static inline
uint32_t ptrOffs(rtl_Word ptr) {
  return ~genCapacity(ptrGen(ptr)) & (ptr >> 4);
}

static inline
rtl_Word *reifyPtr(rtl_Machine *M, rtl_Word ptr)
{
  uint32_t       genNum,
                 offs;
  rtl_Generation *gen;

  genNum = ptrGen(ptr);

  assert(genNum < RTL_MAX_GENERATIONS);
  assert(NULL != M->heap.gen[genNum]);

  gen = M->heap.gen[genNum];

  offs = ptrOffs(ptr);

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

static
rtl_Word mkPtr(rtl_WordType t, uint32_t gen, uint32_t offs)
{
  assert(offs < genCapacity(gen));

  return ((genCapacity(gen) | offs) << 4) | t;
}

// Mark the memory w points to, for generation g.
static
void markWord(rtl_Machine *M, rtl_Generation *gen, rtl_Word w) {
  uint32_t       wOffs;
  size_t         i,
                 len;
  rtl_Word const *fields;
  rtl_Generation *wGen;

  wOffs = ptrOffs(w);

  // Don't bother with pointers into other generations.
  if (ptrGen(w) != gen->nbr) return;

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
    wGen = M->heap.gen[ptrGen(w)];
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

  g = ptrGen(w);
  if (g > highestGen) return w;

  type = rtl_typeOf(w);

  gen = M->heap.gen[g];
  assert(gen);

  nextGen = M->heap.gen[g + 1];
  if (!nextGen)
    nextGen = M->heap.gen[g + 1] = mkGeneration(g + 1);

  oldOffs = ptrOffs(w);
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

  M->error = RTL_OK;
}

static
void pushVStack(rtl_Machine *M, rtl_Word w)
{
  if (M->vStackLen == M->vStackCap) {
    M->vStackCap = M->vStackCap * 2;
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

