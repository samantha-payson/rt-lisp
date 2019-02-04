#include "rt-lisp.h"

#include <stdlib.h>
#include <string.h>
#include <stdio.h>


// These macros are to be used for operating on the VM from the main
// loop. Hopefully this gives the compiler more freedom to keep things in
// registers, etc...

#define likely(x)       __builtin_expect(!!(x),1)
#define unlikely(x)     __builtin_expect((x),0)

// Reviewed
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

  gen->preMoveFillPtr = 0;

  return gen;
}
// Reviewed
void rtl_initHeap(rtl_Heap *h)
{
  memset(h->gen, 0, sizeof(rtl_Generation *)*RTL_MAX_GENERATIONS);
}

// Reviewed
rtl_Word *__rtl_reifyPtr(rtl_Machine *M, rtl_Word ptr)
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

// Reviewed
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

  backing = __rtl_reifyPtr(M, tpl);
  if (backing) {
    *len = (size_t)((*backing) >> 4);
    return backing + 1;
  } else {
    *len = 0;
    return NULL;
  }
}

// Reviewed
rtl_Word const *rtl_reifyCons(rtl_Machine *M, rtl_Word cons)
{
  if (!rtl_isCons(cons)) {
    M->error = RTL_ERR_EXPECTED_CONS;
    return NULL;
  }

  return __rtl_reifyPtr(M, cons);
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

// Reviewed
static
rtl_Word mkPtr(rtl_WordType t, uint32_t gen, uint32_t offs)
{
  assert(offs < __rtl_genCapacity(gen));

  return ((__rtl_genCapacity(gen) | offs) << 4) | t;
}

static
void markWord(rtl_Machine *M, rtl_Generation *gen, rtl_Word w);

// Reviewed
static
void markMap(rtl_Machine *M, rtl_Generation *gen, rtl_Word map, uint32_t mask)
{
  size_t         i,
                 len;

  rtl_Word const *entry,
                 *rptr;

  rtl_Generation *mapGen;

  bool alreadyMarked;

  uint32_t offs, entryOffs;

  if (rtl_isEmptyMap(map) || !rtl_isMap(map)) return;

  mapGen = M->heap.gen[__rtl_ptrGen(map)];
  if (mapGen->nbr > gen->nbr) return;

  offs = __rtl_ptrOffs(map);
  rptr  = __rtl_reifyPtr(M, map);
  len   = __builtin_popcount(mask);

  for (i = 0; i < len; i++) {
    entry     = rptr + 2*i;
    entryOffs = offs + 2*i;

    alreadyMarked  = rtl_bmpSetBit(mapGen->marks, entryOffs, true);
    alreadyMarked &= rtl_bmpSetBit(mapGen->marks, entryOffs + 1, true);

    if (rtl_isHeader(entry[0])) {
      if (mapGen->nbr != gen->nbr || !alreadyMarked) {
	markMap(M, gen, entry[1], rtl_headerValue(entry[0]));
      }
    } else if (!alreadyMarked) {
      markWord(M, gen, entry[0]);
      markWord(M, gen, entry[1]);

    }
  }
}

// Reviewed
//
// Mark the memory w points to, for generation g.
static
void markWord(rtl_Machine *M, rtl_Generation *gen, rtl_Word w) {
  uint32_t       wOffs;

  size_t         i,
                 len;

  rtl_Word const *fields;

  wOffs = __rtl_ptrOffs(w);

  // Don't bother with pointers into other generations.
  if (!rtl_isMap(w) && __rtl_ptrGen(w) != gen->nbr) return;

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

  case RTL_MAP:
    markMap(M, gen, w, 1);
    break;

  case RTL_CONS:
    fields = rtl_reifyCons(M, w);

    // Mark CAR then CDR
    if (!rtl_bmpSetBit(gen->marks, wOffs, true)) {
      markWord(M, gen, fields[0]);
    }
    if (!rtl_bmpSetBit(gen->marks, wOffs + 1, true)) {
      markWord(M, gen, fields[1]);
    }
    break;
  }
}

// Reviewed
static
rtl_Word moveWord(rtl_Machine *M, int highestGen, rtl_Word w)
{
  int g;
  rtl_WordType type;
  uint32_t oldOffs, newOffs;
  rtl_Generation *gen, *nextGen;

  if (!rtl_isPtr(w)) {
    return w;
  }

  g = __rtl_ptrGen(w);
  if (g > highestGen) {
    return w;
  }

  type = rtl_typeOf(w);

  gen = M->heap.gen[g];
  assert(gen);

  nextGen = M->heap.gen[g + 1];
  assert(nextGen);

  oldOffs = __rtl_ptrOffs(w);
  newOffs = rtl_bmpRank(gen->marks, oldOffs)
          + nextGen->preMoveFillPtr;

  return mkPtr(type, g + 1, newOffs);
}

// Still needs reviewing.......... too drunk right now <(^_^)>
//
// Returns the number of the highest generation that was collected.
static
int collectGen(rtl_Machine *M, int g)
{
  size_t i, j, k;
  rtl_Generation *gen, *youngerGen, *nextGen;
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
    //
    // We still need to allocate it though, so the previous generation can move
    // words into it.
    gen = M->heap.gen[g] = mkGeneration(g);
    return g - 1;
  }

  // Clear any marks from previous collections and start w/ a clean slate.
  rtl_bmpClearAll(gen->marks);

  // Mark any pointers into this generation in ..

  // .. the current environment frame ..
  markWord(M, gen, M->env);

  // .. any words on the value stack ..
  for (i = 0; i < M->vStackLen; i++) {
    markWord(M, gen, M->vStack[i]);
  }

  // .. any environment frames on the return stack ..
  for (i = 0; i < M->rStackLen; i++) {
    markWord(M, gen, M->rStack[i].env);
  }

  // .. any words in live working sets ..
  for (i = 0; i < M->wsStackLen; i++) {
    for (pp = M->wsStack[i], j = 0; pp[j] != NULL; j++) {
      if (unlikely(rtl_isHeader(*pp[j]))) {
	rtl_Word const *header, *map;
	header = pp[j++];
	map    = pp[j];

	markMap(M, gen, *map, rtl_headerValue(*header));
      } else {
	markWord(M, gen, *pp[j]);
      }
    }
  }

  // .. and any live words in younger generations.
  for (i = 0; i < g; i++) {
    youngerGen = M->heap.gen[i];
    assert(youngerGen); // Shouldn't be any un-populated younger generations, I
			// think.
    for (j = 0; j < youngerGen->marks->nbrOnes; j++) {
      k = rtl_bmpSelect(youngerGen->marks, j);
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
                                       = moveWord(M, highest, old = gen->words[k]);

  }

  gen->fillPtr = 0;

  // This is the last generation to be moved, now we need to fix-up the pointers
  // in the machine.
  if (g == 0) {
    // .. the current environment frame ..
    M->env = moveWord(M, highest, M->env);

    // .. any words on the value stack ..
    for (i = 0; i < M->vStackLen; i++) {
      M->vStack[i] = moveWord(M, highest, M->vStack[i]);
    }

    // .. any environment frames on the return stack ..
    for (i = 0; i < M->rStackLen; i++) {
      M->rStack[i].env = moveWord(M, highest, M->rStack[i].env);
    }

    // .. any words in live working sets ..
    for (i = 0; i < M->wsStackLen; i++) {
      for (pp = M->wsStack[i], j = 0; pp[j] != NULL; j++) {
	*pp[j] = moveWord(M, highest, *pp[j]);
      }
    }
  }

  return highest;
}

rtl_Word *rtl_allocGC(rtl_Machine *M, rtl_WordType t, rtl_Word *w, size_t nbr)
{
  rtl_Heap       *heap;
  rtl_Generation *gen0;
  size_t         offs;
  rtl_Word       *ptr;

  // Validate t, ensure it's one of the accepted types.
  switch (t) {
  case RTL_TUPLE:
  case RTL_STRING:
  case RTL_MAP:
  case RTL_CONS:
  case RTL_CLOSURE:
    break;

  default:
    M->error = RTL_ERR_INVALID_OPERATION;
    return NULL;
  }

  heap = &M->heap;
  gen0 = heap->gen[0];

  // Allocate this generation if it doesn't exist.
  if (!gen0) gen0 = heap->gen[0] = mkGeneration(0);

  if (gen0->fillPtr + nbr >= gen0->capacity) {
    collectGen(M, 0);
  }

  offs = gen0->fillPtr;
  gen0->fillPtr += nbr;

  *w = mkPtr(t, 0, offs);

  ptr = gen0->words + offs;

  #ifndef NDEBUG
  // In most cases, this should leave uninitialized GC-allocated memory as an
  // invalid tuple.
  memset(ptr, RTL_TUPLE, nbr*sizeof(rtl_Word));
  #endif

  return ptr;
}

rtl_Word *rtl_allocTuple(rtl_Machine *M, rtl_Word *w, size_t len)
{
  rtl_Word *ptr = rtl_allocGC(M, RTL_TUPLE, w, len + 1);
  ptr[0] = (len << 4) | RTL_HEADER;
  return ptr + 1;
}



uint32_t mask32(unsigned k) {
  return (1 << k) - 1;
}

// IMPORTANT: Always returns 0 at depth 0.
uint32_t hashKey(uint32_t key, uint32_t depth) {
  return (key ^ key*(depth + 1)) % 28;
}

rtl_Word __rtl_mapInsert(rtl_Machine *M,
			 uint32_t    *newMask,
			 uint32_t    mask,
			 rtl_Word    map,
			 rtl_Word    key,
			 rtl_Word    val,
			 uint32_t    depth)
{
  rtl_Word const *backing,
                 *entry;

  rtl_Word *newBacking,
           *newEntry;

  rtl_Word newMap      = RTL_NIL,
           newInnerMap = RTL_NIL;

  rtl_Word newMapTmpMask      = RTL_HEADER,
           newInnerMapTmpMask = RTL_HEADER;

  uint32_t index,
           hash,
           otherHash,
           len,
           innerMask,
           newInnerMask;

  RTL_PUSH_WORKING_SET(M, &map, &key, &val,
		       &newMapTmpMask,      &newMap,
		       &newInnerMapTmpMask, &newInnerMap);

  hash    = hashKey(key, depth);
  len     = __builtin_popcount(mask);
  index   = __builtin_popcount(mask32(hash) & mask);
  backing = __rtl_reifyPtr(M, map);
  entry   = backing + 2*index;

  if (mask & (1 << hash)) { // There's something in this slot already
    if (rtl_isHeader(entry[0])) { // It's a sub-map.
      innerMask = rtl_headerValue(entry[0]);

      newInnerMap = __rtl_mapInsert(M,
				    &newInnerMask,
				    innerMask,
				    entry[1],
				    key,
				    val,
				    depth + 1);
      newInnerMapTmpMask = rtl_header(newInnerMask);

      newBacking = rtl_allocGC(M, RTL_MAP, &newMap, 2*len);
      *newMask   = mask; // Mask doesn't change

      newMapTmpMask = rtl_header(mask);

      backing = __rtl_reifyPtr(M, map);

      memcpy(newBacking, backing, sizeof(rtl_Word)*2*len);
      newEntry = newBacking + 2*index;

      newEntry[0] = rtl_header(newInnerMask);
      newEntry[1] = newInnerMap;

    } else if (entry[0] == key) { // It's an entry with the same key.
      newBacking = rtl_allocGC(M, RTL_MAP, &newMap, 2*len);
      *newMask   = mask; // Mask doesn't change
      newMapTmpMask = rtl_header(mask);

      backing = __rtl_reifyPtr(M, map);
      entry   = backing + 2*index;

      memcpy(newBacking, backing, sizeof(rtl_Word)*2*len);
      newEntry = newBacking + 2*index;

      newEntry[1] = val;

    } else { // It's an entry with a different key.

      // Create a new singleton map containing only the old element.
      newBacking = rtl_allocGC(M, RTL_MAP, &newInnerMap, 2);
      otherHash  = hashKey(entry[0], depth + 1);
      innerMask  = 1 << otherHash;
      newInnerMapTmpMask = rtl_header(innerMask);

      backing = __rtl_reifyPtr(M, map);
      entry   = backing + 2*index;

      newBacking[0] = entry[0];
      newBacking[1] = entry[1];

      // Insert key/val into the singleton map.
      newInnerMap = __rtl_mapInsert(M,
				    &newInnerMask,
				    innerMask,
				    newInnerMap,
				    key,
				    val,
				    depth + 1);
      newInnerMapTmpMask = rtl_header(newInnerMask);

      newBacking = rtl_allocGC(M, RTL_MAP, &newMap, 2*len);
      *newMask   = mask;
      newMapTmpMask = rtl_header(mask);

      backing = __rtl_reifyPtr(M, map);
      entry   = backing + 2*index;

      memcpy(newBacking, backing, sizeof(rtl_Word)*2*len);
      newEntry = newBacking + 2*index;

      newEntry[0] = rtl_header(newInnerMask);
      newEntry[1] = newInnerMap;
    }
  } else { // There's nothing in this slot yet
    newBacking = rtl_allocGC(M, RTL_MAP, &newMap, 2*(len + 1));
    *newMask   = mask | (1 << hash);

    backing = __rtl_reifyPtr(M, map);
    entry   = backing + 2*index;

    // Copy everything before the new slot ..
    memcpy(newBacking, backing, sizeof(rtl_Word)*2*index);
    newEntry = newBacking + 2*index;

    // .. then fill the new slot ..
    newEntry[0] = key;
    newEntry[1] = val;

    // .. then copy everything after the new slot.
    memcpy(newBacking + 2*index + 2,
	   backing + 2*index,
	   sizeof(rtl_Word)*2*(len - index));
  }

  rtl_popWorkingSet(M);

  return newMap;
}


rtl_Word rtl_mapInsert(rtl_Machine *M,
		       rtl_Word    map,
		       rtl_Word    key,
		       rtl_Word    val)
{
  uint32_t dummy;
  rtl_Word newMap;
  rtl_Word *newBacking;

  if (rtl_isEmptyMap(map)) {
    newBacking = rtl_allocGC(M, RTL_MAP, &newMap, 2);

    newBacking[0] = key;
    newBacking[1] = val;

    return newMap;
  }

  return __rtl_mapInsert(M, &dummy, 1, map, key, val, 0);
}

rtl_Word __rtl_mapLookup(rtl_Machine *M,
			 rtl_Word    map,
			 rtl_Word    key,
			 uint32_t    mask,
			 uint32_t    depth)
{
  uint32_t hash,
           index;

  rtl_Word const *backing,
                 *entry;

  hash = hashKey(key, depth);

  if (!rtl_isEmptyMap(map) && (mask & (1 << hash))) {
    index   = __builtin_popcount(mask32(hash) & mask);
    backing = __rtl_reifyPtr(M, map);
    entry   = backing + 2*index;

    if (rtl_isHeader(entry[0])) {
      return __rtl_mapLookup(M,
			     entry[1],
			     key,
			     rtl_headerValue(entry[0]),
			     depth + 1);
    } else {
      return entry[1];
    }
  } else {
    return RTL_NIL;
  }
}

rtl_Word rtl_mapLookup(rtl_Machine *M, rtl_Word map, rtl_Word key)
{
  return __rtl_mapLookup(M, map, key, 1, 0);
}

void rtl_initMachine(rtl_Machine *M)
{
  rtl_initHeap(&M->heap);

  M->env = RTL_NIL;

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

rtl_Word rtl_cons(rtl_Machine *M, rtl_Word car, rtl_Word cdr)
{
  rtl_Word w = RTL_NIL, *ptr;

  RTL_PUSH_WORKING_SET(M, &w, &car, &cdr);

  ptr = rtl_allocGC(M, RTL_CONS, &w, 2);

  ptr[0] = car;
  ptr[1] = cdr;

  rtl_popWorkingSet(M);

  return w;
}

void rtl_testGarbageCollector(size_t count)
{
  rtl_Machine    M;
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

  (void)straggler0;
  (void)straggler1;

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

  case RTL_MAP:
    return "Map";

  case RTL_CONS:
    return "Cons";

  case RTL_TOP:
    return "Top";

  case RTL_CLOSURE:
    return "Closure";

  case RTL_HEADER:
    return "[Header (impl detail)]";

  case RTL_ADDR:
    return "[Addr (impl detail)]";

  default:
    return "[Unknown RTL type]";
  }
}

#define VPUSH(W) ({							\
      if (unlikely(M->vStackLen == M->vStackCap)) {			\
	M->vStackCap = M->vStackCap == 0 ? 64 : M->vStackCap * 2;	\
	M->vStack    = realloc(M->vStack, sizeof(rtl_Word)*M->vStackCap); \
      }									\
									\
      M->vStack[M->vStackLen++] = (W);					\
    })									\
  // End of multi-line macro

#define VSTACK_ASSERT_LEN(N) ({			\
      if (unlikely((N) > M->vStackLen)) {	\
	M->error = RTL_ERR_STACK_UNDERFLOW;	\
	goto interp_cleanup;			\
      }						\
    })						\
  // End of multi-line macro

#define RSTACK_ASSERT_LEN(N) ({			\
      if (unlikely((N) > M->rStackLen)) {	\
	M->error = RTL_ERR_STACK_UNDERFLOW;	\
	goto interp_cleanup;			\
      }						\
    })						\
  // End of multi-line macro

#define VPOP() (M->vStack[--M->vStackLen])

#define VPOPK(K) ({ M->vStackLen -= (K); })

#define VPEEK(N) (M->vStack[M->vStackLen - ((N) + 1)])

// This is the decode/dispatch template for all hard-coded binary operators.
#define BINARY_OP(INAME, OP, TYPE_TEST, TYPE_ERR, TYPE_MK, TYPE_VAL)	\
      case RTL_OP_##INAME:						\
	VSTACK_ASSERT_LEN(2);						\
									\
	b = VPOP();							\
	a = VPOP();							\
									\
	if (unlikely(!TYPE_TEST(a)) || unlikely(!TYPE_TEST(b))) {	\
	  M->error = TYPE_ERR;						\
	  goto interp_cleanup;						\
	}								\
									\
	VPUSH(TYPE_MK(TYPE_VAL(a) OP TYPE_VAL(b)));			\
	break;								\
  // end of multi-line macro

#define CMP_OP(INAME, NUMERIC_CMP)		\
      case RTL_OP_##INAME:			\
	VSTACK_ASSERT_LEN(2);			\
						\
	b = VPOP();				\
	a = VPOP();				\
						\
	if (rtl_cmp(M, a, b) NUMERIC_CMP) {	\
	  VPUSH(RTL_TOP);			\
	} else {				\
	  VPUSH(RTL_NIL);			\
	}					\
	break;					\
  // end of multiline macro

static
uint8_t *readWord(uint8_t *pc, rtl_Word *out)
{
  *out = (rtl_Word)pc[0] << 0
       | (rtl_Word)pc[1] << 8
       | (rtl_Word)pc[2] << 16
       | (rtl_Word)pc[3] << 24 ;

  return pc + 4;
}

static
uint8_t *readShort(uint8_t *pc, uint16_t *out)
{
  *out = (rtl_Word)pc[0] << 0
       | (rtl_Word)pc[1] << 8 ;

  return pc + 2;
}

int rtl_cmp(rtl_Machine *M, rtl_Word a, rtl_Word b)
{
  rtl_WordType   aType,
                 bType;

  int32_t        aI32,
                 bI32;

  size_t         aLen,
                 bLen,
                 i;

  rtl_Word const *aPtr,
                 *bPtr;

  int subResult;

  aType = rtl_typeOf(a);
  bType = rtl_typeOf(b);

  if (aType < bType) {
    return -1;

  } else if (aType > bType) {
    return 1;

  } else {
    switch (aType) {
    case RTL_NIL:
    case RTL_TOP:
      return 0;

    case RTL_INT28:
    case RTL_FIX14:
      aI32 = (int32_t)a >> 4;
      bI32 = (int32_t)b >> 4;
      return aI32 - bI32;

    case RTL_ADDR:
      aI32 = (int32_t)(a >> 4);
      bI32 = (int32_t)(b >> 4);
      return aI32 - bI32;

    case RTL_SYMBOL:
    case RTL_UNRESOLVED_SYMBOL:
      aI32 = rtl_symbolID(a);
      bI32 = rtl_symbolID(b);
      return aI32 - bI32;

    case RTL_TUPLE:
      aPtr = rtl_reifyTuple(M, a, &aLen);
      bPtr = rtl_reifyTuple(M, b, &bLen);
      for (i = 0; i < aLen && i < bLen; i++) {
	subResult = rtl_cmp(M, aPtr[i], bPtr[i]);
	if (subResult != 0) return subResult;
      }

      return (int)aLen - (int)bLen;

    case RTL_CONS:
      aPtr = rtl_reifyCons(M, a);
      bPtr = rtl_reifyCons(M, b);

      subResult = rtl_cmp(M, aPtr[0], bPtr[0]);
      if (subResult != 0) {
	return rtl_cmp(M, aPtr[1], bPtr[1]);
      }

      return subResult;

    default:
      abort();
    }
  }
}

rtl_Word rtl_run(rtl_Machine *M, rtl_Word addr)
{
  uint8_t opcode;

  uint16_t frame, idx, size;
  size_t    len;
  ssize_t   i;

  rtl_Word literal;

  // Some scratch space:
  rtl_Word a = RTL_NIL,
           b = RTL_NIL,
           c = RTL_NIL,
           d = RTL_NIL;

  rtl_Word f = RTL_NIL,
           g = RTL_NIL;

  rtl_Word const *rptr;
  rtl_Word *wptr;

  // Ensure these words are involved in any garbage collection that may happen.
  RTL_PUSH_WORKING_SET(M, &a, &b, &c, &d, &f, &g);

  M->pc = rtl_resolveAddr(M, addr);

  while (M->error == RTL_OK) {
    /* printf("VSTACK:"); */
    /* for (i = 0; i < M->vStackLen; i++) { */
    /*   printf(" "); */
    /*   rtl_formatExpr(M, M->vStack[i]); */
    /* } */
    /* printf("\n"); */

    /* printf("env: "); */
    /* rtl_formatExpr(M, M->env); */
    /* printf("\n"); */
    /* printf("RSTACK:\n"); */
    /* for (i = 0; i < M->rStackLen; i++) { */
    /*   rtl_formatExpr(M, M->rStack[i].env); */
    /*   printf(" "); */
    /* } */

    /* printf("\n"); */

    // rtl_disasm(M->pc);

    switch (opcode = *M->pc++) {
    case RTL_OP_NOP:
      break;

    case RTL_OP_CONST:
      M->pc = readWord(M->pc, &literal);

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

      c = rtl_cons(M, a, b);

      VPOPK(2);
	
      VPUSH(c);
      break;

    case RTL_OP_CAR:
      VSTACK_ASSERT_LEN(1);

      // Don't need to SAVE_MACHINE() here, since reifyCons doesn't look at any
      // of the un-saved fields of M.
      rptr = rtl_reifyCons(M, VPOP());
      if (unlikely(!rptr)) {
	goto interp_cleanup;
      }

      VPUSH(rptr[0]);
      break;

    case RTL_OP_CDR:

      VSTACK_ASSERT_LEN(1);

      rptr = rtl_reifyCons(M, VPOP());
      if (unlikely(!rptr)) {
	goto interp_cleanup;
      }

      VPUSH(rptr[1]);
      break;

    case RTL_OP_TUPLE:
      M->pc = readShort(M->pc, &size);

      VSTACK_ASSERT_LEN(size);

      wptr = rtl_allocTuple(M, &a, size);

      memcpy(wptr, M->vStack + M->vStackLen - size, sizeof(rtl_Word)*size);

      VPOPK(size);

      VPUSH(a);
      break;

    case RTL_OP_GET:
      VSTACK_ASSERT_LEN(2);

      i    = rtl_int28Value(VPOP());
      rptr = rtl_reifyTuple(M, VPOP(), &len);

      assert(i < (int)len);

      VPUSH(rptr[i]);
      break;

    case RTL_OP_LEN:
      VSTACK_ASSERT_LEN(1);

      rtl_reifyTuple(M, VPOP(), &len);

      VPUSH(rtl_int28(len));
      break;

    case RTL_OP_MAP:
      VPUSH(rtl_emptyMap());
      break;

    case RTL_OP_INSERT:
      VSTACK_ASSERT_LEN(3);

      c = VPOP(); // Value
      b = VPOP(); // Key
      a = VPOP(); // Map

      VPUSH(rtl_mapInsert(M, a, b, c));
      break;

    case RTL_OP_LOOKUP:
      VSTACK_ASSERT_LEN(2);

      b = VPOP(); // Key
      a = VPOP(); // Map

      VPUSH(rtl_mapLookup(M, a, b));
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

      a = VPEEK(0);

      VPUSH(a);
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

      M->pc = readWord(M->pc, &literal);

      if (rtl_isNil(VPOP())) break;

      M->pc = rtl_resolveAddr(M, literal);
      break;

    case RTL_OP_JMP:
      M->pc = readWord(M->pc, &literal);

      M->pc = rtl_resolveAddr(M, literal);
      break;

    case RTL_OP_VAR:
      M->pc = readShort(M->pc, &frame);
      M->pc = readShort(M->pc, &idx);

      rptr = rtl_reifyTuple(M, M->env, &len);
      assert(frame < len);

      rptr = rtl_reifyTuple(M, rptr[frame], &len);
      assert(idx < len);

      VPUSH(rptr[idx]);
      break;

    case RTL_OP_CLOSURE:
      M->pc = readWord(M->pc, &literal);

      wptr = rtl_allocGC(M, RTL_CLOSURE, &f, 2);

      wptr[0] = literal;
      wptr[1] = M->env;

      VPUSH(f);
      break;

    case RTL_OP_CALL:
      M->pc = readShort(M->pc, &size);

      VSTACK_ASSERT_LEN(1+ size);

      wptr = rtl_allocTuple(M, &a, size);

      memcpy(wptr, M->vStack + M->vStackLen - size, sizeof(rtl_Word)*size);
      VPOPK(size);

      f = VPOP();

      switch (rtl_typeOf(f)) {
      case RTL_ADDR:
	wptr = rtl_allocTuple(M, &b, 1);
	wptr[0] = a;

	if (unlikely(M->rStackLen == M->rStackCap)) {
	  M->rStackCap = M->rStackCap*2;
	  M->rStack    = realloc(M->rStack, M->rStackCap * sizeof(rtl_RetAddr));
	}

	M->rStack[M->rStackLen++] = (rtl_RetAddr) {
	  .pc  = M->pc,
	  .env = M->env,
	};

	M->env = b;
	M->pc = rtl_resolveAddr(M, f);
	break;

      case RTL_CLOSURE:
	rptr = __rtl_reifyPtr(M, f);

	f = rptr[0];

	if (rptr[1] != RTL_NIL) {
	  rptr = rtl_reifyTuple(M, rptr[1], &len);
	} else {
	  len  = 0;
	  rptr = NULL;
	}

	wptr      = rtl_allocTuple(M, &b, len + 1);
	wptr[0] = a;
	memcpy(wptr + 1, rptr, sizeof(rtl_Word)*len);

	M->rStack[M->rStackLen++] = (rtl_RetAddr) {
	  .pc  = M->pc,
	  .env = M->env,
	};

	M->env = b;
	M->pc  = rtl_resolveAddr(M, f);

	break;

      default:
	printf(" error: Can't call object of type '%s'!\n",
	       rtl_typeNameOf(f));
	goto interp_cleanup;

      } break;

    case RTL_OP_STATIC_CALL:
      M->pc = readWord(M->pc, &f);
      M->pc = readShort(M->pc, &size);

      VSTACK_ASSERT_LEN(size);

      wptr = rtl_allocTuple(M, &a, size);

      memcpy(wptr, M->vStack + M->vStackLen - size, sizeof(rtl_Word)*size);
      VPOPK(size);

      wptr = rtl_allocTuple(M, &b, 1);
      wptr[0] = a;

      if (unlikely(M->rStackLen == M->rStackCap)) {
	M->rStackCap = M->rStackCap*2;
	M->rStack    = realloc(M->rStack, M->rStackCap * sizeof(rtl_RetAddr));
      }

      M->rStack[M->rStackLen++] = (rtl_RetAddr) {
	.pc  = M->pc,
	.env = M->env,
      };

      M->env = b;
      M->pc = rtl_resolveAddr(M, f);
      break;

    case RTL_OP_APPLY_LIST:
      VSTACK_ASSERT_LEN(2);
      a = rtl_listToTuple(M, VPOP());

      VPUSH(a);

      // fallthrough ..

    case RTL_OP_APPLY_TUPLE:
      VSTACK_ASSERT_LEN(2);

      a = VPOP();
      f = VPOP();

      switch (rtl_typeOf(f)) {
      case RTL_ADDR:
	wptr = rtl_allocTuple(M, &b, 1);
	wptr[0] = a;

	if (unlikely(M->rStackLen == M->rStackCap)) {
	  M->rStackCap = M->rStackCap*2;
	  M->rStack    = realloc(M->rStack, M->rStackCap * sizeof(rtl_RetAddr));
	}

	M->rStack[M->rStackLen++] = (rtl_RetAddr) {
	  .pc  = M->pc,
	  .env = M->env,
	};

	M->env = b;
	M->pc = rtl_resolveAddr(M, f);

	break;

      case RTL_CLOSURE:
	rptr = __rtl_reifyPtr(M, f);

	f = rptr[0];

	if (rptr[1] != RTL_NIL) {
	  rptr = rtl_reifyTuple(M, rptr[1], &len);
	} else {
	  len  = 0;
	  rptr = NULL;
	}

	wptr      = rtl_allocTuple(M, &b, len + 1);
	wptr[0] = a;
	memcpy(wptr + 1, rptr, sizeof(rtl_Word)*len);

	M->rStack[M->rStackLen++] = (rtl_RetAddr) {
	  .pc  = M->pc,
	  .env = M->env,
	};

	M->env = b;
	M->pc  = rtl_resolveAddr(M, f);

	break;

      default:
	printf(" error: Can't call object of type '%s'!\n",
	       rtl_typeNameOf(f));
	goto interp_cleanup;

      } break;

    case RTL_OP_UNDEFINED_FUNCTION:
      M->pc = readWord(M->pc, &literal);

      printf("tried to call undefined function: '%s:%s'\n",
	     rtl_symbolPackageName(literal),
	     rtl_symbolName(literal));
      abort();

    case RTL_OP_UNDEFINED_VAR:
      M->pc = readWord(M->pc, &literal);

      printf("tried to load undefined function: '%s:%s'\n",
	     rtl_symbolPackageName(literal),
	     rtl_symbolName(literal));
      abort();

    case RTL_OP_RETURN:
      if (M->rStackLen == 0) {
	// If the return stack is empty, return means exit the interpreter.
	goto interp_cleanup;
      }

      M->rStackLen--;

      M->pc  = M->rStack[M->rStackLen].pc;
      M->env = M->rStack[M->rStackLen].env;

      break;

    case RTL_OP_REST:
      M->pc = readShort(M->pc, &idx);

      rptr = rtl_reifyTuple(M, M->env, &len);
      rptr = rtl_reifyTuple(M, rptr[0], &len);
      assert(0 <= idx && idx <= len);

      a = RTL_NIL;
      for (i = (ssize_t)len - 1; i >= idx; i--) {
	a = rtl_cons(M, rptr[i], a);
      }

      wptr = rtl_allocTuple(M, &b, idx + 1);
      memcpy(wptr, rptr, sizeof(rtl_Word)*idx);
      wptr[idx] = a;

      rptr = rtl_reifyTuple(M, M->env, &len);
      wptr = rtl_allocTuple(M, &c, len);
      memcpy(wptr, rptr, sizeof(rtl_Word)*len);
      wptr[0] = b;

      M->env = c;

      break;


    // Generate code for binary operations using the BINARY_OP macro.
    BINARY_OP(IADD, +, rtl_isInt28, RTL_ERR_EXPECTED_INT28,
	      rtl_int28, rtl_int28Value);
    BINARY_OP(ISUB, -, rtl_isInt28, RTL_ERR_EXPECTED_INT28,
	      rtl_int28, rtl_int28Value);
    BINARY_OP(IMUL, *, rtl_isInt28, RTL_ERR_EXPECTED_INT28,
	      rtl_int28, rtl_int28Value);
    BINARY_OP(IDIV, /, rtl_isInt28, RTL_ERR_EXPECTED_INT28,
	      rtl_int28, rtl_int28Value);
    BINARY_OP(IMOD, %, rtl_isInt28, RTL_ERR_EXPECTED_INT28,
	      rtl_int28, rtl_int28Value);

    BINARY_OP(FADD, +, rtl_isFix14, RTL_ERR_EXPECTED_FIX14,
	      rtl_fix14, rtl_fix14Value);
    BINARY_OP(FSUB, -, rtl_isFix14, RTL_ERR_EXPECTED_FIX14,
	      rtl_fix14, rtl_fix14Value);
    BINARY_OP(FMUL, *, rtl_isFix14, RTL_ERR_EXPECTED_FIX14,
	      rtl_fix14, rtl_fix14Value);
    BINARY_OP(FDIV, /, rtl_isFix14, RTL_ERR_EXPECTED_FIX14,
	      rtl_fix14, rtl_fix14Value);

    CMP_OP(LT,  <  0);
    CMP_OP(LEQ, <= 0);
    CMP_OP(GT,  >  0);
    CMP_OP(GEQ, >= 0);
    CMP_OP(ISO, == 0);

    case RTL_OP_EQ:
      VSTACK_ASSERT_LEN(2);

      b = VPOP();
      a = VPOP();
      if (a == b) {
	VPUSH(RTL_TOP);
      } else {
	VPUSH(RTL_NIL);
      } break;

    case RTL_OP_NEQ:
      VSTACK_ASSERT_LEN(2);

      b = VPOP();
      a = VPOP();
      if (a != b) {
	VPUSH(RTL_TOP);
      } else {
	VPUSH(RTL_NIL);
      } break;
      

    default:
      printf("Unhandled instruction: opcode %d\n", (int)opcode);
      break;
    }
  }

 interp_cleanup:
  rtl_popWorkingSet(M);

  return M->vStackLen ? VPOP() : RTL_NIL;
}

rtl_Word rtl_listToTuple(rtl_Machine *M, rtl_Word list)
{
  size_t   len;
  rtl_Word *ptr;
  rtl_Word tuple;
  size_t   i;

  assert(rtl_isCons(list) || rtl_isNil(list));

  len = rtl_listLength(M, list);
  ptr = rtl_allocTuple(M, &tuple, len);

  for (i = 0; list != RTL_NIL; list = rtl_cdr(M, list), i++) {
    ptr[i] = rtl_car(M, list);
  }

  return tuple;
}

rtl_Word rtl_applyList(rtl_Machine *M, rtl_Word fn, rtl_Word argList)
{
  rtl_Word *ptr;
  rtl_Word args, env;

  args = rtl_listToTuple(M, argList);

  ptr    = rtl_allocTuple(M, &env, 1);
  ptr[0] = args;

  M->env = env;
  return rtl_run(M, fn);
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

rtl_Word rtl_emitByteToPage(rtl_Machine *M, uint16_t pageID, uint8_t b)
{
  rtl_Page *page;
  rtl_Page *newPage;
  rtl_Word addr;

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

  addr = rtl_addr(pageID, page->len);

  page->code[page->len++] = b;

  return addr;
}

rtl_Word rtl_emitShortToPage(rtl_Machine *M, uint16_t pageID, uint16_t u16)
{
  rtl_Word addr;

  addr = rtl_emitByteToPage(M, pageID, (u16 >> 0) & 0xFF);
  rtl_emitByteToPage(M, pageID, (u16 >> 8) & 0xFF);

  return addr;
}

rtl_Word rtl_emitWordToPage(rtl_Machine *M, uint16_t pageID, rtl_Word w)
{
  rtl_Word addr;

  addr = rtl_emitByteToPage(M, pageID, (w >>  0) & 0xFF);
  rtl_emitByteToPage(M, pageID, (w >>  8) & 0xFF);
  rtl_emitByteToPage(M, pageID, (w >> 16) & 0xFF);
  rtl_emitByteToPage(M, pageID, (w >> 24) & 0xFF);

  return addr;
}

rtl_Word rtl_reverseListImproper(rtl_Machine *M, rtl_Word ls, rtl_Word last)
{
  rtl_Word result = RTL_NIL;

  RTL_PUSH_WORKING_SET(M, &ls, &last, &result);

  for (result = last; ls != RTL_NIL; ls = rtl_cdr(M, ls)) {
    result = rtl_cons(M, rtl_car(M, ls), result);
  }

  rtl_popWorkingSet(M);

  return result;
}

size_t rtl_listLength(rtl_Machine *M, rtl_Word ls)
{
  size_t n;

  for (n = 0; ls != RTL_NIL; n++) {
    ls = rtl_cdr(M, ls);
  }

  return n;
}

void __rtl_pushWorkingSet(rtl_Machine *M, rtl_WorkingSet ws, char const *fName)
{

#ifdef RTL_TRACE_WORKING_SETS
  size_t i;
  for (i = 0; i < M->wsStackLen; i++) printf("  ");
  printf("> PUSH '%s'\n", fName);
#endif

  if (M->wsStackLen == M->wsStackCap) {
    M->wsStackCap = !M->wsStackCap ? 8 : 2*M->wsStackCap;
    M->wsStack = realloc(M->wsStack, M->wsStackCap*sizeof(rtl_WorkingSet *));
  }

  M->wsStack[M->wsStackLen++] = ws;
}

void __rtl_popWorkingSet(rtl_Machine *M, char const *fName)
{

#ifdef RTL_TRACE_WORKING_SETS
  size_t i;
  for (i = 1; i < M->wsStackLen; i++) printf("  ");
  printf("< POP  '%s'\n", fName);
#endif

  assert(M->wsStackLen > 0);

  M->wsStackLen--;
}

rtl_Word rtl_nextAddrInPage(rtl_Machine *M, uint16_t pageID)
{
  rtl_Page *page;

  assert(pageID < M->pagesLen);
  page = M->pages[pageID];

  return rtl_addr(pageID, page->len);
}
