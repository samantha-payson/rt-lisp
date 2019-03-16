// This file is part of RT Lisp.
//
// RT Lisp is free software: you can redistribute it and/or modify it under the
// terms of the GNU Lesser General Public License as published by the Free
// Software Foundation, either version 3 of the License, or (at your option) any
// later version.
//
// RT Lisp is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
// A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more
// details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with RT Lisp.  If not, see <https://www.gnu.org/licenses/>.

#include "rt-lisp.h"

#include <stdlib.h>
#include <string.h>
#include <stdio.h>


// These macros are to be used for operating on the VM from the main
// loop. Hopefully this gives the compiler more freedom to keep things in
// registers, etc...

#define likely(x)       __builtin_expect(!!(x),1)
#define unlikely(x)     __builtin_expect((x),0)

static
rtl_Generation *mkGeneration(int genNbr)
{
  rtl_Generation *gen;
  size_t         capacity;

  // We have 27 address bits available to us.
  capacity = __rtl_genCapacity(genNbr);

  gen = malloc(sizeof(rtl_Generation) + capacity*sizeof(rtl_Word));

  gen->nbr      = genNbr;
  gen->fillPtr  = 0;
  gen->capacity = capacity;
  gen->marks    = rtl_newBitMap(capacity);

  gen->preMoveFillPtr = 0;

  return gen;
}

void rtl_initCodeBase(rtl_CodeBase *codeBase)
{
  codeBase->fns    = NULL;
  codeBase->fnsLen = 0;
  codeBase->fnsCap = 0;

  memset(codeBase->fnsByName, 0, sizeof(codeBase->fnsByName));
}

static
rtl_Word addBuiltin(rtl_CodeBase *cb, rtl_Word name, rtl_BuiltinFn cFn)
{
  size_t fnID;

  fnID = rtl_newFuncID(cb, name);

  cb->fns[fnID]->isBuiltin      = true;
  cb->fns[fnID]->as.builtin.cFn = cFn;

  return rtl_function(fnID);
}

rtl_Word rtl_registerBuiltin(rtl_Compiler  *C,
                             rtl_Word      name,
                             rtl_BuiltinFn cFn)
{
  rtl_FnDef    *def;
  size_t       idx;
  uint32_t     fnID;
  rtl_CodeBase *codeBase;
  rtl_Function *func;

  codeBase = C->M->codeBase;

  idx = rtl_symbolID(name) % RTL_CODE_BASE_FN_HASH_SIZE;

  for (def = codeBase->fnsByName[idx]; def != NULL; def = def->next) {
    if (def->name == name) {
      if (def->fn != RTL_NIL) {
        fnID = rtl_functionID(def->fn);

        rtl_newFuncVersion(codeBase, fnID);
      } else {
        fnID = rtl_newFuncID(codeBase, name);
      }

      func = codeBase->fns[fnID];

      func->as.builtin.cFn = cFn;
      func->isBuiltin      = true;

      rtl_resolveCallSites(C, name, def->fn);

      return def->fn;
    }
  }

  def = malloc(sizeof(rtl_FnDef));

  def->name    = name;
  def->fn      = addBuiltin(codeBase, name, cFn);
  def->macro   = RTL_NIL;

  def->next                = codeBase->fnsByName[idx];
  codeBase->fnsByName[idx] = def;

  rtl_resolveCallSites(C, name, def->fn);

  return def->fn;
}

void rtl_initHeap(rtl_Heap *h)
{
  memset(h->gen, 0, sizeof(rtl_Generation *)*RTL_MAX_GENERATIONS);
}

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

rtl_Word const *rtl_reifyTuple(rtl_Machine *M, rtl_Word tpl, size_t *len) {
  rtl_Word *backing;

  if (unlikely(!len)) {
    abort();
  }

  if (!rtl_isTuple(tpl)) {
    rtl_triggerFault(M, "expected-tuple",
                     "Passed non-tuple to rtl_reifyTuple!");

    *len = 0;
    return NULL;
  } else if (tpl == RTL_TUPLE) {
    *len = 0;
    return NULL;
  } else {
    backing = __rtl_reifyPtr(M, tpl);
    if (backing) {
      *len = (size_t)((*backing) >> 4);
      return backing + 1;
    } else {
      *len = 0;
      return NULL;
    }
  }
}

rtl_Word const *rtl_reifyCons(rtl_Machine *M, rtl_Word cons)
{
  if (!rtl_isCons(cons)) {
    rtl_triggerFault(M, "expected-cons",
                     "Passed non-cons to rtl_reifyCons!");

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

static
rtl_Word mkPtr(rtl_WordType t, uint32_t gen, uint32_t offs)
{
  assert(offs < __rtl_genCapacity(gen));

  return ((__rtl_genCapacity(gen) | offs) << 4) | t;
}

// Mark the memory w points to, for generation g.
static
void markWord(rtl_Machine *M, rtl_Generation *gen, rtl_Word w) {
  uint32_t       wOffs,
                 mask;

  size_t         i,
                 len;

  rtl_Word const *fields;

  // Don't bother with pointers into other generations.
  if (__rtl_ptrGen(w) != gen->nbr)
    return;

  // Zero pointers don't need marking.
  if (rtl_isZeroValue(w))
    return;

  wOffs = __rtl_ptrOffs(w);

  switch (rtl_typeOf(w)) {
  case RTL_TUPLE:
    fields = rtl_reifyTuple(M, w, &len);

    // First mark the header ..
    if (rtl_bmpSetBit(gen->marks, wOffs, true)) {
      // If this bit is set, all of them are. Otherwise, none are.
      break;
    }

    // .. then each of the element words.
    for (i = 0; i < len; i++) {
      rtl_bmpSetBit(gen->marks, (wOffs + 1) + i, true);
      markWord(M, gen, fields[i]);
    }
    break;

  case RTL_NATIVE:
    len = rtl_sizeOfNative(M, w);
    for (i = 0; i < (len + 2) / 3 + 1; i++) {
      if (rtl_bmpSetBit(gen->marks, wOffs + i, true)) {
        // If any bit is set, then all of them are.
        break;
      }
    } break;

  case RTL_MAP:
    fields = __rtl_reifyPtr(M, w);

    mask = rtl_headerValue(fields[0]);
    len  = __builtin_popcount(mask);

    // First mark the mask ..
    if (rtl_bmpSetBit(gen->marks, wOffs, true)) {
      // If this bit is set, all of them are. Otherwise, none are.
      break;
    }

    // .. then each of the entries.
    for (i = 0; i < 2*len; i++) {
      rtl_bmpSetBit(gen->marks, wOffs + 1 + i, true);
      markWord(M, gen, fields[i + 1]);
    }

    break;

  case RTL_CLOSURE:
    fields = __rtl_reifyPtr(M, w);

    if (!rtl_bmpSetBit(gen->marks, wOffs, true)) {
      markWord(M, gen, fields[0]);
    }

    if (!rtl_bmpSetBit(gen->marks, wOffs + 1, true)) {
      markWord(M, gen, fields[1]);
    }
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

// Returns true if there is currently a mark for the word pointed at by ptr in
// the bitmap for its generation.
static
bool wordIsMarked(rtl_Machine *M, rtl_Word ptr)
{
  rtl_Generation *gen;
  int g;
  uint32_t offs;

  g = __rtl_ptrGen(ptr);

  gen = M->heap.gen[g];

  assert(gen);

  offs = __rtl_ptrOffs(ptr);

  return rtl_bmpGetBit(gen->marks, offs);
}

// Move a word as if it were a non-zero pointer, no matter its type. Used for
// moving elements of the backSet ...
static
rtl_Word moveWord(rtl_Machine *M, int highestGen, rtl_Word w)
{
  int g;
  rtl_WordType type;
  uint32_t oldOffs, newOffs;
  rtl_Generation *gen, *nextGen;

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

static
rtl_Word movePtr(rtl_Machine *M, int highestGen, rtl_Word w)
{
  if (!rtl_isPtr(w) || rtl_isZeroValue(w)) {
    return w;
  }

  return moveWord(M, highestGen, w);
}

// Returns the number of the highest generation that was collected.
static
int collectGen(rtl_Machine *M, int g)
{
  size_t i, j, k;
  rtl_Generation *gen, *youngerGen, *nextGen;
  int highest;
  rtl_Word **pp;
  rtl_Word *ptr;
  rtl_Word w;

  // Next generation needs to exist in order for us to collect. Maybe in the
  // future we could add the ability to compact the maximum generation in-place,
  // but for now we just consider ourselves OOM if the last generation needs to
  // be collected.
  if (g + 1 >= RTL_MAX_GENERATIONS) {
    printf("    --------> Lisp Machine OUT OF MEMORY <--------\n");
    abort();
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

  // .. the dynamic environment ..
  markWord(M, gen, M->dynamic);

  // .. any words on the value stack ..
  for (i = 0; i < M->vStackLen; i++) {
    markWord(M, gen, M->vStack[i]);
  }

  // .. any words on the dynamic var stack ..
  for (i = 0; i < M->dStackLen; i++) {
    markWord(M, gen, M->dStack[i]);
  }

  // .. any environment frames on the return stack ..
  for (i = 0; i < M->rStackLen; i++) {
    markWord(M, gen, M->rStack[i].env);
  }

  // .. any words in live working sets ..
  for (i = 0; i < M->wsStackLen; i++) {
    for (pp = M->wsStack[i], j = 0; pp[j] != NULL; j++) {
      markWord(M, gen, *pp[j]);
    }
  }

  // .. any words pointed to from the backSet ..
  for (i = 0; i < M->backSetLen; i++) {
    ptr = __rtl_reifyPtr(M, M->backSet[i]);

    markWord(M, gen, *ptr);
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
                                       = movePtr(M, highest, old = gen->words[k]);

  }

  gen->fillPtr = 0;

  // This is the last generation to be moved, now we need to fix-up the pointers
  // in the machine.
  if (g == 0) {
    // .. the current environment frame ..
    M->env = movePtr(M, highest, M->env);

    // .. the dynamic environment ..
    M->dynamic = movePtr(M, highest, M->dynamic);

    // .. any words on the value stack ..
    for (i = 0; i < M->vStackLen; i++) {
      M->vStack[i] = movePtr(M, highest, M->vStack[i]);
    }

    // .. any words on the value stack ..
    for (i = 0; i < M->dStackLen; i++) {
      M->dStack[i] = movePtr(M, highest, M->dStack[i]);
    }

    // .. any environment frames on the return stack ..
    for (i = 0; i < M->rStackLen; i++) {
      M->rStack[i].env = movePtr(M, highest, M->rStack[i].env);
    }

    // .. and any words in live working sets.
    for (i = 0; i < M->wsStackLen; i++) {
      for (pp = M->wsStack[i], j = 0; pp[j] != NULL; j++) {
        *pp[j] = movePtr(M, highest, *pp[j]);
      }
    }


    // As for the backSet: first we need to move the backSet pointers
    // themselves ..
    for (i = j = 0; i < M->backSetLen; i++) {
      w = M->backSet[i];

      if (__rtl_ptrGen(w) > highest) {
        // w's generation wasn't moved, copy it as is.
        M->backSet[j++] = w;
        ptr = __rtl_reifyPtr(M, w);

        *ptr = movePtr(M, highest, *ptr);

      } else if (wordIsMarked(M, w)) {
        // w is from one of the moved generations, copy and move it.
        w = moveWord(M, highest, w);
        ptr = __rtl_reifyPtr(M, w);

        // It's possible that w was moved into the same generation as its
        // target, in which case it no longer needs to be managed from the
        // backSet (intra-generation pointers work on their own).
        if (__rtl_ptrGen(*ptr) < __rtl_ptrGen(w)) {
          *ptr = movePtr(M, highest, *ptr); // Is this redundant?
          M->backSet[j++] = w;
        } 
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

#ifndef NDEBUG
  if (M->fault) {
    printf(" error: Performing allocation with a pending fault!\n");
    abort();
  }
#endif

  // Validate t, ensure it's one of the accepted types.
  switch (t) {
  case RTL_TUPLE:
  case RTL_NATIVE:
  case RTL_MAP:
  case RTL_CONS:
  case RTL_CLOSURE:
    break;

  default:
    rtl_triggerFault(M, "invalid-allocation",
                     "Trying to allocate non-pointer type!\n");
    return NULL;
  }

  heap = &M->heap;
  gen0 = heap->gen[0];

  // Allocate this generation if it doesn't exist.
  if (unlikely(!gen0)) gen0 = heap->gen[0] = mkGeneration(0);

  if (unlikely(gen0->fillPtr + nbr >= gen0->capacity)) {
    collectGen(M, 0);
#ifndef NDEBUG
    if (rtl_debugCheckForCycles(M)) {
      rtl_dumpHeap(M);
      asm("int3");
    }
#endif
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
  rtl_Word *ptr;

  if (len == 0) {
    *w = RTL_TUPLE;

    return NULL;
  } else {
    ptr    = rtl_allocGC(M, RTL_TUPLE, w, len + 1);
    ptr[0] = (len << 4) | RTL_HEADER;
    return ptr + 1;
  }
}

rtl_Word rtl_native(rtl_Machine *M, void const *data, uint32_t size)
{
  rtl_Word w;
  size_t   wLen, i;
  rtl_Word *wBacking;

  uint8_t const *u8;

  u8   = (uint8_t const *)data;
  wLen = (size + 2) / 3;

  wBacking    = rtl_allocGC(M, RTL_NATIVE, &w, wLen + 1);

  memset(wBacking, 0, sizeof(rtl_Word)*(wLen + 1));

  wBacking[0] = rtl_header(size);

  for (i = 0; i < wLen; i++) {
    switch (size - i*3) {
    case 1:
      wBacking[i + 1] = ((rtl_Word)u8[i*3 + 0] << 8)
                      | RTL_HEADER;
      break;

    case 2:
      wBacking[i + 1] = ((rtl_Word)u8[i*3 + 0] << 8)
                      | ((rtl_Word)u8[i*3 + 1] << 16)
                      | RTL_HEADER;
      break;

    default:
      wBacking[i + 1] = ((rtl_Word)u8[i*3 + 0] <<  8)
                      | ((rtl_Word)u8[i*3 + 1] << 16)
                      | ((rtl_Word)u8[i*3 + 2] << 24)
                      | RTL_HEADER;
      break;
    }
  }

  return w;
}

uint32_t rtl_sizeOfNative(rtl_Machine *M, rtl_Word n)
{
  rtl_Word const *rPtr;

  assert(rtl_isNative(n));

  rPtr = __rtl_reifyPtr(M, n);

  return rtl_headerValue(rPtr[0]);
}

void rtl_reifyNative(rtl_Machine *M, rtl_Word w, void *out, uint32_t outSize)
{
  rtl_Word const *rPtr;
  uint32_t i, size;
  uint8_t  *u8;

  assert(rtl_isNative(w));

  rPtr = __rtl_reifyPtr(M, w);
  size = rtl_headerValue(rPtr[0]);
  u8   = (uint8_t *)out;

  for (i = 0; i < size; i++) {
    u8[i] = (rPtr[1 + i/3] >> (((i % 3) + 1)*8)) & 0xFF;
  }
}

uint32_t mask32(unsigned k) {
  return (1 << k) - 1;
}

// IMPORTANT: Always returns 0 at depth 0.
uint32_t hashKey(uint32_t key, uint32_t depth) {
  return (key >> depth) % 28;
}

rtl_Word __rtl_mapInsert(rtl_Machine *M,
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

  uint32_t mask,
           newMask,
           index,
           hash,
           otherHash,
           len,
           innerMask;

  RTL_PUSH_WORKING_SET(M, &map, &key, &val, &newMap, &newInnerMap);

  hash    = hashKey(key, depth);
  backing = __rtl_reifyPtr(M, map);

  mask = rtl_headerValue(backing[0]);

  len   = __builtin_popcount(mask);
  index = __builtin_popcount(mask32(hash) & mask);

  entry = backing + 1 + 2*index;

  if (mask & (1 << hash)) { // There's something in this slot already
    if (rtl_isHeader(entry[0])) { // It's a sub-map.
      newInnerMap = __rtl_mapInsert(M,
                                    entry[1],
                                    key,
                                    val,
                                    depth + 1);

      newBacking = rtl_allocGC(M, RTL_MAP, &newMap, 1 + 2*len);

      backing = __rtl_reifyPtr(M, map);

      memcpy(newBacking, backing, sizeof(rtl_Word)*(1 + 2*len));
      newEntry = newBacking + 1 + 2*index;

      newEntry[0] = rtl_header(0); // Placeholder, to indicate submap.
      newEntry[1] = newInnerMap;

    } else if (entry[0] == key) { // It's an entry with the same key.
      if (entry[1] == val) {
        newMap = map; // Same key and same value, no need for a new map...
      } else {
        newBacking = rtl_allocGC(M, RTL_MAP, &newMap, 1 + 2*len);

        backing = __rtl_reifyPtr(M, map);
        entry   = backing + 1 + 2*index;

        memcpy(newBacking, backing, sizeof(rtl_Word)*(1 + 2*len));
        newEntry = newBacking + 1 + 2*index;

        newEntry[1] = val;
      }
    } else { // It's an entry with a different key.

      // Create a new singleton map containing only the old element.
      newBacking = rtl_allocGC(M, RTL_MAP, &newInnerMap, 3);
      otherHash  = hashKey(entry[0], depth + 1);
      innerMask  = 1 << otherHash;

      // Reload backing -- allocGC may have moved it.
      backing = __rtl_reifyPtr(M, map);
      entry   = backing + 1 + 2*index;

      newBacking[0] = rtl_header(innerMask);

      newBacking[1] = entry[0];
      newBacking[2] = entry[1];

      // Insert key/val into the singleton map.
      newInnerMap = __rtl_mapInsert(M,
                                    newInnerMap,
                                    key,
                                    val,
                                    depth + 1);

      newBacking = rtl_allocGC(M, RTL_MAP, &newMap, 1 + 2*len);

      // Again, reload backing -- allocGC may have moved it.
      backing = __rtl_reifyPtr(M, map);
      entry   = backing + 1 + 2*index;

      memcpy(newBacking, backing, sizeof(rtl_Word)*(1 + 2*len));
      newEntry = newBacking + 1 + 2*index;

      newEntry[0] = rtl_header(0); // Marker
      newEntry[1] = newInnerMap;
    }
  } else { // There's nothing in this slot yet
    
    newBacking = rtl_allocGC(M, RTL_MAP, &newMap, 1 + 2*(len + 1));
    newMask    = mask | (1 << hash);

    // Reload backing -- allocGC may have moved it.
    backing = __rtl_reifyPtr(M, map);
    entry   = backing + 1 + 2*index;

    // Copy everything before the new slot ..
    memcpy(newBacking + 1, backing + 1, sizeof(rtl_Word)*2*index);
    newEntry = newBacking + 1 + 2*index;

    // .. then fill the new slot ..
    newEntry[0] = key;
    newEntry[1] = val;

    // .. then copy everything after the new slot.
    memcpy(newBacking + 1 + 2*index + 2,
           backing + 1 + 2*index,
           sizeof(rtl_Word)*2*(len - index));

    newBacking[0] = rtl_header(newMask);
}

  rtl_popWorkingSet(M);

  return newMap;
}


rtl_Word rtl_mapInsert(rtl_Machine *M,
                       rtl_Word    map,
                       rtl_Word    key,
                       rtl_Word    val)
{
  rtl_Word *backing;
  uint32_t hash, mask;

  if (map == RTL_MAP) {
    RTL_PUSH_WORKING_SET(M, &map, &key, &val);

    backing = rtl_allocGC(M, RTL_MAP, &map, 3);

    hash = hashKey(key, 0);
    mask = 1 << hash;

    backing[0] = rtl_header(mask);
    backing[1] = key;
    backing[2] = val;

    rtl_popWorkingSet(M);

    return map;
  } else {
    return __rtl_mapInsert(M, map, key, val, 0);
  }
}

rtl_Word __rtl_mapLookup(rtl_Machine *M,
                         rtl_Word    map,
                         rtl_Word    key,
                         rtl_Word    def,
                         uint32_t    depth)
{
  uint32_t mask,
           hash,
           index;

  rtl_Word const *backing,
                 *entry;

  hash = hashKey(key, depth);

  if (!rtl_isEmptyMap(map)) {
    backing = __rtl_reifyPtr(M, map);
    mask    = rtl_headerValue(backing[0]);

    if ((mask & (1 << hash)) == 0) {
      return def;
    }

    index = __builtin_popcount(mask32(hash) & mask);
    entry = backing + 1 + 2*index;

    if (rtl_isHeader(entry[0])) {
      return __rtl_mapLookup(M,
                             entry[1],
                             key,
                             def,
                             depth + 1);
    } else if (entry[0] == key) {
      return entry[1];
    } else {
      return def;
    }
  } else {
    return def;
  }
}

rtl_Word rtl_mapLookup(rtl_Machine *M, rtl_Word map, rtl_Word key, rtl_Word def)
{
  if (unlikely(!rtl_isMap(map))) {
    rtl_triggerFault(M, "expected-map",
                     "second argument of rtl_mapLookup must be a map!");
    return def;
  }

  return __rtl_mapLookup(M, map, key, def, 0);
}

void rtl_visitMap(rtl_Machine      *M,
                  void             *accum,
                  rtl_MapVisitorFn fn,
                  rtl_Word         map)
{
  uint32_t mask,
           len,
           i;

  rtl_Word const *backing,
                 *entry;

  if (rtl_isZeroValue(map)) {
    return;
  }

  RTL_PUSH_WORKING_SET(M, &map);

  backing = __rtl_reifyPtr(M, map);
  mask    = rtl_headerValue(backing[0]);
  len     = __builtin_popcount(mask);

  for (i = 0; i < len; i++) {
    backing = __rtl_reifyPtr(M, map);
    entry   = backing + 1 + 2*i;

    if (rtl_isHeader(entry[0])) {
      rtl_visitMap(M, accum, fn, entry[1]);
    } else {
      fn(M, accum, entry[0], entry[1]);
    }
  }

  rtl_popWorkingSet(M);
}

rtl_Word rtl_std_handleFault(rtl_Machine    *M,
                             rtl_Word const *args,
                             size_t         argsLen)
{
  assert(argsLen == 1);

  printf("unhandled fault:\n     ");
  rtl_formatExpr(M, args[0]);
  printf("\n\n");

  rtl_stackTrace(M);

  M->fault = true;

  return RTL_NIL;
}

struct foldMapAccum {
  rtl_Word init, fn;
};

static
void foldEntry(rtl_Machine  *M,
               void         *vaccum,
               rtl_Word     key,
               rtl_Word     val)
{
  struct foldMapAccum *acc = (struct foldMapAccum *)vaccum;

  acc->init = rtl_callWithArgs(M, acc->fn, acc->init, key, val);
}

rtl_Word rtl_std_foldMap(rtl_Machine     *M,
                         rtl_Word const  *args,
                         size_t          argsLen)
{
  struct foldMapAccum acc;

  rtl_Word map;

  if (argsLen != 3) {
    rtl_triggerFault(M, "arg-count",
                     "fold-map expects exactly 3 arguments.");
    return RTL_NIL;
  }

  acc.fn   = args[0];
  acc.init = args[1];
  map      = args[2];

  RTL_PUSH_WORKING_SET(M, &acc.fn, &acc.init, &map);

  rtl_visitMap(M, &acc, foldEntry, map);

  rtl_popWorkingSet(M);

  return acc.init;
}

static
rtl_Word rtl_std_listToTuple(rtl_Machine     *M,
                             rtl_Word const  *args,
                             size_t          argsLen)
{
  if (argsLen != 1) {
    rtl_triggerFault(M, "arg-count",
                     "std:list->tuple expects exactly 1 argument, a list.");
    return rtl_internSelector("error", "arg-count");
  }

  return rtl_listToTuple(M, args[0]);
}

void rtl_initMachine(rtl_Machine *M, rtl_CodeBase *codeBase)
{
  // Required to register the initial handleFault builtin.
  rtl_Compiler C;

  rtl_Word handleFault;

  rtl_initHeap(&M->heap);

  M->env = RTL_TUPLE;

  M->dynamic = RTL_MAP;

  M->pc = NULL;

  M->dStack    = malloc(16*sizeof(rtl_Word));
  M->dStackLen = 0;
  M->dStackCap = 16;

  M->vStack    = malloc(64*sizeof(rtl_Word));
  M->vStackLen = 0;
  M->vStackCap = 64;

  M->rStack    = malloc(64*sizeof(rtl_RetAddr));
  M->rStackLen = 0;
  M->rStackCap = 64;

  M->wsStack    = NULL;
  M->wsStackLen = 0;
  M->wsStackCap = 0;

  M->backSet    = malloc(32*sizeof(rtl_Word));
  M->backSetLen = 0;
  M->backSetCap = 32;

  M->codeBase = codeBase;

  M->fault = false;

  M->yield = false;

  rtl_initCompiler(&C, M);

  handleFault = rtl_registerBuiltin(&C, rtl_intern("std", "handle-fault"),
                                    rtl_std_handleFault);

  rtl_setVar(M, rtl_intern("std", "*error-handler*"),
             handleFault);

  rtl_registerBuiltin(&C, rtl_intern("std", "fold-map"),
                      rtl_std_foldMap);
  rtl_registerBuiltin(&C, rtl_intern("std", "list->tuple"),
                      rtl_std_listToTuple);
}

void rtl_resetMachine(rtl_Machine *M)
{
  M->vStackLen = M->dStackLen
               = M->rStackLen
               = 0;

  M->env = RTL_TUPLE;

  M->fault = M->yield = false;
}

rtl_Word rtl_cons(rtl_Machine *M, rtl_Word car, rtl_Word cdr)
{
  rtl_Word w = RTL_NIL, *ptr;

  if (unlikely(M->fault))
    return RTL_NIL;

  RTL_PUSH_WORKING_SET(M, &w, &car, &cdr);

  ptr = rtl_allocGC(M, RTL_CONS, &w, 2);

  ptr[0] = car;
  ptr[1] = cdr;

  rtl_popWorkingSet(M);

  rtl_debugCheckAlloc(M, w);

  return w;
}

void rtl_testGarbageCollector(size_t count)
{
  rtl_Machine    M;
  rtl_Word const *ptr;
  int            i;

  rtl_initMachine(&M, NULL);

  M.vStackLen = 1;

  // Cons up a gigantic linked list.
  for (i = 0, M.vStack[0] = RTL_NIL; i < count; i++) {
    M.vStack[0] = rtl_cons(&M, rtl_int28(i), M.vStack[0]);
    if (rtl_checkFault(&M)) {
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

    if (rtl_checkFault(&M)) {
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

  case RTL_CHAR:
    return "Char";

  case RTL_NATIVE:
    return "Native";

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

  case RTL_FUNCTION:
    return "[Function (impl detail)]";

  default:
    return "[Unknown RTL type]";
  }
}

#define VPUSH(W) ({                                                     \
      if (unlikely(M->vStackLen == M->vStackCap)) {                     \
        M->vStackCap = M->vStackCap == 0 ? 64 : M->vStackCap * 2;       \
        M->vStack    = realloc(M->vStack, sizeof(rtl_Word)*M->vStackCap); \
      }                                                                 \
                                                                        \
      M->vStack[M->vStackLen++] = (W);                                  \
    })                                                                  \
  // End of multi-line macro

#define VSTACK_ASSERT_LEN(N) ({                                 \
      if (unlikely((N) > M->vStackLen)) {                       \
        rtl_triggerFault(M, "stack-underflow",                  \
                         "VStack underflow in interpreter.");   \
        goto interp_cleanup;                                    \
      }                                                         \
    })                                                          \
  // End of multi-line macro

#define RSTACK_ASSERT_LEN(N) ({                                 \
      if (unlikely((N) > M->rStackLen)) {                       \
        rtl_triggerFault(M, "stack-underflow",                  \
                         "RStack underflow in interpreter.");   \
        goto interp_cleanup;                                    \
      }                                                         \
    })                                                          \
  // End of multi-line macro

#ifdef RTL_TRACE_FN_CALLS
# define CALL_TRACE_ENTER()                                             \
  for (size_t i = 1; i < M->rStackLen; i++) printf(" ");                \
  printf("> '%s:%s'\n",                                                 \
         rtl_symbolPackageName(M->rStack[M->rStackLen - 1].fn),         \
         rtl_symbolName(M->rStack[M->rStackLen - 1].fn));               \
                                                                        \
  printf("CALL STACK: ");                                               \
                                                                        \
  for (size_t i = 0; i < M->rStackLen; i++) {                           \
    printf("%s:%s ",                                                    \
           rtl_symbolPackageName(M->rStack[i].fn),                      \
           rtl_symbolName(M->rStack[i].fn));                            \
  }                                                                     \
  printf("\n");                                                         \
  // End of multi-line macro
#else
# define CALL_TRACE_ENTER()
#endif

#ifdef RTL_TRACE_FN_CALLS
# define CALL_TRACE_EXIT()                                              \
  for (size_t i = 1; i < M->rStackLen; i++) printf(" ");                \
  printf("< '%s:%s'\n",                                                 \
         rtl_symbolPackageName(M->rStack[M->rStackLen - 1].fn),         \
         rtl_symbolName(M->rStack[M->rStackLen - 1].fn));               \
                                                                        \
  printf("CALL STACK: ");                                               \
                                                                        \
  for (size_t i = 0; i < M->rStackLen; i++) {                           \
    printf("%s:%s ",                                                    \
           rtl_symbolPackageName(M->rStack[i].fn),                      \
           rtl_symbolName(M->rStack[i].fn));                            \
  }                                                                     \
  printf("\n");                                                         \
  // End of multi-line macro
#else
# define CALL_TRACE_EXIT()
#endif

#ifdef RTL_TRACE_FN_CALLS
# define CALL_TRACE_TAIL(FNAME)                                         \
  for (size_t i = 1; i < M->rStackLen; i++) printf(" ");                \
  printf("| '%s:%s' -> '%s:%s'\n",                                      \
         rtl_symbolPackageName(M->rStack[M->rStackLen - 1].fn),         \
         rtl_symbolName(M->rStack[M->rStackLen - 1].fn),                \
         rtl_symbolPackageName(FNAME),                                  \
         rtl_symbolName(FNAME));                                        \
                                                                        \
  printf("CALL STACK: ");                                               \
                                                                        \
  for (size_t i = 0; i < M->rStackLen; i++) {                           \
    printf("%s:%s ",                                                    \
           rtl_symbolPackageName(M->rStack[i].fn),                      \
           rtl_symbolName(M->rStack[i].fn));                            \
  }                                                                     \
  printf("\n");                                                         \
  // End of multi-line macro
#else
# define CALL_TRACE_TAIL(FNAME)
#endif

#define RPUSH(FN) ({                                                    \
      if (unlikely(M->rStackLen == M->rStackCap)) {                     \
        M->rStackCap = M->rStackCap*2;                                  \
        M->rStack    = realloc(M->rStack,                               \
                               M->rStackCap * sizeof(rtl_RetAddr));     \
      }                                                                 \
                                                                        \
      M->rStack[M->rStackLen++] = (rtl_RetAddr) {                       \
        .pc  = M->pc,                                                   \
        .env = M->env,                                                  \
        .fn  = FN,                                                      \
      };                                                                \
      CALL_TRACE_ENTER()                                                \
    })                                                                  \
  // End of multi-line macro

#define RPOP() ({                             \
      M->rStackLen--;                         \
                                              \
      M->pc  = M->rStack[M->rStackLen].pc;    \
      M->env = M->rStack[M->rStackLen].env;   \
    })                                        \
  // End of multi-line macro.

#define TAIL(FN) ({                           \
      CALL_TRACE_TAIL(FN);                    \
      M->rStack[M->rStackLen - 1].fn = FN;    \
    })                                        \
  // End of multi-line macro

#define VPOP() (M->vStack[--M->vStackLen])

#define VPOPK(K) ({ M->vStackLen -= (K); })

#define VPEEK(N) (M->vStack[M->vStackLen - ((N) + 1)])

// This is the decode/dispatch template for all hard-coded binary operators.
#define BINARY_OP(INAME, OP, TYPE_TEST, TYPE_NAME, TYPE_MK, TYPE_VAL)   \
      case RTL_OP_##INAME:                                              \
        VSTACK_ASSERT_LEN(2);                                           \
                                                                        \
        b = VPOP();                                                     \
        a = VPOP();                                                     \
                                                                        \
        if (unlikely(unlikely(!TYPE_TEST(a)) || unlikely(!TYPE_TEST(b)))) { \
          c = RTL_MAP;                                                  \
          c = rtl_recordSet(M, c, "type",                               \
                            rtl_internSelector(NULL, "type-mismatch")); \
          c = rtl_recordSet(M, c, "message",                            \
                            rtl_string(M, "Type mismatch for binop "    \
                                       "instruction '" #INAME "', "     \
                                       "expected both " TYPE_NAME));    \
          c = rtl_recordSet(M, c, "lhs", a);                            \
          c = rtl_recordSet(M, c, "rhs", b);                            \
          __rtl_triggerFault(M, c);                                     \
          goto interp_cleanup;                                          \
        }                                                               \
                                                                        \
        VPUSH(TYPE_MK(TYPE_VAL(a) OP TYPE_VAL(b)));                     \
        break;                                                          \
  // end of multi-line macro

#define CMP_OP(INAME, NUMERIC_CMP)              \
      case RTL_OP_##INAME:                      \
        VSTACK_ASSERT_LEN(2);                   \
                                                \
        b = VPOP();                             \
        a = VPOP();                             \
                                                \
        if (rtl_cmp(M, a, b) NUMERIC_CMP) {     \
          VPUSH(RTL_TOP);                       \
        } else {                                \
          VPUSH(RTL_NIL);                       \
        }                                       \
        break;                                  \
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

    case RTL_SYMBOL:
    case RTL_UNRESOLVED_SYMBOL:
    case RTL_SELECTOR:
    case RTL_FUNCTION:
    case RTL_CHAR:
      aI32 = (int32_t)(a >> 4);
      bI32 = (int32_t)(b >> 4);
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

rtl_Word rtl_call(rtl_Machine *M, rtl_Word fn)
{
  rtl_Function    *func;
  rtl_Word const  *rptr;
  size_t          len;


  func = rtl_reifyFunction(M->codeBase, fn);

  if (func->isBuiltin) {
    rptr = rtl_reifyTuple(M, M->env, &len);
    if (len == 0) {
      VPUSH(func->as.builtin.cFn(M, NULL, 0));

    } else {
      rptr = rtl_reifyTuple(M, rptr[len - 1], &len);
      VPUSH(func->as.builtin.cFn(M, rptr, len));

    }
  } else {
    M->pc = NULL;
    RPUSH(fn);

    M->pc = func->as.lisp.code;

    rtl_run(M);
  }

  return M->vStackLen ? VPOP() : RTL_NIL;
}

void rtl_resume(rtl_Machine *M)
{
  if (M->yield) {
    M->yield = false;
    rtl_run(M);
  } else {
    rtl_triggerFault(M, "invalid-resume",
                     "rtl_resume called on a machine that hadn't yielded.");
  }
}

// A more streamlined implementation of rtl_run.
void rtl_run(rtl_Machine *M)
{
  rtl_OpEncoding enc;

  uint8_t     imm8;

  uint16_t    imm16,
              frame,
              index;

  rtl_Word    immW;

  uint32_t    gen,
              offs;

  rtl_Opcode  opcode;

  size_t      len,
              i;

  rtl_Word    a = RTL_NIL,
              b = RTL_NIL,
              c = RTL_NIL,
              d = RTL_NIL,
              e = RTL_NIL,
              f = RTL_NIL;


  rtl_Word const *rptr,
                 *sptr;

  rtl_Word       *wptr;

  rtl_Function   *func;

  RTL_PUSH_WORKING_SET(M, &a, &b, &c, &d, &e, &f);

  while (!M->fault) {
    // printf("       VSTACK: ");
    // for (i = 0; i < M->vStackLen; i++) {
    //   printf(" ");
    //   rtl_formatExprShallow(M->vStack[i]);
    // }
    // printf("\n");
    // rtl_stackTrace(M);
    // printf("\n\n");

    rtl_disasm(M->codeBase, M->pc);

    opcode = *M->pc++;
    enc = (rtl_OpEncoding)(opcode >> 4);

    // Stage 1: Decode the instruction
    switch (enc) {
    case RTL_OP_ENC_NULLARY_NOARG:
    case RTL_OP_ENC_UNARY_NOARG:
    case RTL_OP_ENC_BINARY_NOARG_A:
    case RTL_OP_ENC_BINARY_NOARG_B:
    case RTL_OP_ENC_TERNARY_NOARG:
      break;

    case RTL_OP_ENC_NULLARY_BYTE:
    case RTL_OP_ENC_UNARY_BYTE:
      imm8 = *M->pc++;
      break;

    case RTL_OP_ENC_NULLARY_SHORT:
    case RTL_OP_ENC_UNARY_SHORT:
    case RTL_OP_ENC_DYNAMIC_SHORT:
    case RTL_OP_ENC_FUNCTION_SHORT:
      M->pc = readShort(M->pc, &imm16);
      break;

    case RTL_OP_ENC_NULLARY_VAR:
    case RTL_OP_ENC_UNARY_VAR:
      M->pc = readShort(M->pc, &frame);
      M->pc = readShort(M->pc, &index);
      break;

    case RTL_OP_ENC_NULLARY_WORD:
    case RTL_OP_ENC_UNARY_WORD:
      M->pc = readWord(M->pc, &immW);
      break;

    case RTL_OP_ENC_STATIC:
      M->pc = readWord(M->pc, &immW);
      M->pc = readShort(M->pc, &imm16);
      break;
    }

    // Labels is a (weird and rare) special case.
    if (unlikely(opcode == RTL_OP_LABELS)) {
      if (M->vStackLen < imm16) {
        rtl_triggerFault(M, "stack-underflow",
                         "VStack underflowed by labels instruction!");

        continue;
      }

      sptr = M->vStack + M->vStackLen - imm16;

      rtl_reifyTuple(M, M->env, &len);

      wptr    = rtl_allocGC(M, RTL_TUPLE, &a, 1+len+1 + 1+imm16 + 2*imm16);
      wptr[0] = rtl_header(len + 1);

      // Get rptr after we call rtl_allocGC, in case the buffer got moved.
      rptr = rtl_reifyTuple(M, M->env, &len);

      for (i = 0; i < imm16; i++) {
        wptr[1+len+1 + 1+imm16 + i*2 + 0] = sptr[i];
        wptr[1+len+1 + 1+imm16 + i*2 + 1] = a;
      }

      for (i = 0; i < len; i++) {
        wptr[i + 1] = rptr[i];
      }

      gen  = __rtl_ptrGen(a);
      offs = __rtl_ptrOffs(a);

      wptr[1 + len] = mkPtr(RTL_TUPLE, gen, offs + 1+len+1);

      wptr[1+len+1] = rtl_header(imm16);

      for (i = 0; i < imm16; i++) {
        wptr[1+len+1 + 1 + i] = mkPtr(RTL_CLOSURE,
                                      gen,
                                      offs + 1+len+1 + 1+imm16 + i*2);
      }

      VPOPK(imm16);

      M->env = a;

      continue;
    }

    // Stage 2: Load stack arguments into "registers", allocating tuples where
    //          necessary.
    switch (enc) {
    case RTL_OP_ENC_NULLARY_NOARG:
    case RTL_OP_ENC_NULLARY_BYTE:
    case RTL_OP_ENC_NULLARY_SHORT:
    case RTL_OP_ENC_NULLARY_VAR:
    case RTL_OP_ENC_NULLARY_WORD:
      break;

    case RTL_OP_ENC_UNARY_NOARG:
    case RTL_OP_ENC_UNARY_BYTE:
    case RTL_OP_ENC_UNARY_SHORT:
    case RTL_OP_ENC_UNARY_VAR:
    case RTL_OP_ENC_UNARY_WORD:
      if (unlikely(M->vStackLen < 1)) {
        rtl_triggerFault(M, "stack-underflow",
                         "VStack underflowed by unary instruction!");
        continue;
      }

      a = VPOP();
      break;

    case RTL_OP_ENC_BINARY_NOARG_A:
    case RTL_OP_ENC_BINARY_NOARG_B:
      if (unlikely(M->vStackLen < 2)) {
        rtl_triggerFault(M, "stack-underflow",
                         "VStack underflowed by binary instruction!");
        continue;
      }

      b = VPOP();
      a = VPOP();
      break;

    case RTL_OP_ENC_TERNARY_NOARG:
      if (unlikely(M->vStackLen < 3)) {
        rtl_triggerFault(M, "stack-underflow",
                         "VStack underflowed by ternary instruction!");
        continue;
      }

      c = VPOP();
      b = VPOP();
      a = VPOP();
      break;

    case RTL_OP_ENC_STATIC:
      f = immW;

      // fallthrough ...

    case RTL_OP_ENC_DYNAMIC_SHORT:
      if (unlikely(M->vStackLen < imm16)) {
        rtl_triggerFault(M, "stack-underflow",
                         "VStack underflowed by dynamic instruction!");
        continue;
      }

      wptr = rtl_allocTuple(M, &a, imm16);
      memcpy(wptr, M->vStack + M->vStackLen - imm16, sizeof(rtl_Word)*imm16);
      M->vStackLen -= imm16;
      break;

    case RTL_OP_ENC_FUNCTION_SHORT:
      if (unlikely(M->vStackLen < (imm16 + 1))) {
        rtl_triggerFault(M, "stack-underflow",
                         "VStack underflowed by dynamic instruction!");
        continue;
      }

      wptr = rtl_allocTuple(M, &a, imm16);
      memcpy(wptr, M->vStack + M->vStackLen - imm16, sizeof(rtl_Word)*imm16);
      VPOPK(imm16);

      f = VPOP();
      break;
    }

    // Stage 3: Execute instruction.
    //
    // For instructions which represent a function calls, this stage prepares
    // the argument tuple and stores it in `a', and places the function (or
    // closure, map, selector, tuple, etc..) object in `f'.
    switch (opcode) {
    case RTL_OP_NOP:
      break;

    case RTL_OP_MAP:
      VPUSH(RTL_MAP);
      break;

    case RTL_OP_NIL:
      VPUSH(RTL_NIL);
      break;

    case RTL_OP_TOP:
      VPUSH(RTL_TOP);
      break;

    case RTL_OP_GENSYM:
      VPUSH(rtl_gensym());
      break;

    case RTL_OP_RET:
      assert(M->rStackLen > 0);

      RPOP();
      CALL_TRACE_EXIT();

      if (M->pc == NULL) goto cleanup;

      break;

    case RTL_OP_END_LABELS:
      M->env = rtl_tuplePopLast(M, M->env);
      break;

    case RTL_OP_CAR:
      VPUSH(rtl_car(M, a));
      break;

    case RTL_OP_CDR:
      VPUSH(rtl_cdr(M, a));
      break;

    case RTL_OP_POP:
      break;

    case RTL_OP_DUP:
      VPUSH(a);
      VPUSH(a);
      break;

    case RTL_OP_INT28P:
      VPUSH(rtl_isInt28(a) ? RTL_TOP : RTL_NIL);
      break;

    case RTL_OP_FIX14P:
      VPUSH(rtl_isFix14(a) ? RTL_TOP : RTL_NIL);
      break;

    case RTL_OP_SYMBOLP:
      VPUSH(rtl_isSymbol(a) ? RTL_TOP : RTL_NIL);
      break;

    case RTL_OP_SELECTORP:
      VPUSH(rtl_isSelector(a) ? RTL_TOP : RTL_NIL);
      break;

    case RTL_OP_MAPP:
      VPUSH(rtl_isMap(a) ? RTL_TOP : RTL_NIL);
      break;

    case RTL_OP_CHARP:
      VPUSH(rtl_isChar(a) ? RTL_TOP : RTL_NIL);
      break;

    case RTL_OP_NILP:
    case RTL_OP_NOT:
      VPUSH(rtl_isNil(a) ? RTL_TOP : RTL_NIL);
      break;

    case RTL_OP_TOPP:
      VPUSH(rtl_isTop(a) ? RTL_TOP : RTL_NIL);
      break;

    case RTL_OP_CONSP:
      VPUSH(rtl_isCons(a) ? RTL_TOP : RTL_NIL);
      break;

    case RTL_OP_TUPLEP:
      VPUSH(rtl_isTuple(a) ? RTL_TOP : RTL_NIL);
      break;

    case RTL_OP_LEN:
      VPUSH(rtl_int28(rtl_tupleLen(M, a)));
      break;

    case RTL_OP_IADD:
      if (unlikely(unlikely(!rtl_isInt28(a)) || unlikely(!rtl_isInt28(b)))) {
        rtl_triggerFault(M, "expected-int",
                         "iadd instruction expects two int28 arguments.");
        continue;
      }

      VPUSH(rtl_int28(rtl_int28Value(a) + rtl_int28Value(b)));
      break;

    case RTL_OP_ISUB:
      if (unlikely(unlikely(!rtl_isInt28(a)) || unlikely(!rtl_isInt28(b)))) {
        rtl_triggerFault(M, "expected-int",
                         "isub instruction expects two int28 arguments.");
        continue;
      }

      VPUSH(rtl_int28(rtl_int28Value(a) - rtl_int28Value(b)));
      break;

    case RTL_OP_IMUL:
      if (unlikely(unlikely(!rtl_isInt28(a)) || unlikely(!rtl_isInt28(b)))) {
        rtl_triggerFault(M, "expected-int",
                         "imul instruction expects two int28 arguments.");
        continue;
      }

      VPUSH(rtl_int28(rtl_int28Value(a) * rtl_int28Value(b)));
      break;

    case RTL_OP_IDIV:
      if (unlikely(unlikely(!rtl_isInt28(a)) || unlikely(!rtl_isInt28(b)))) {
        rtl_triggerFault(M, "expected-int",
                         "idiv instruction expects two int28 arguments.");
        continue;
      }

      VPUSH(rtl_int28(rtl_int28Value(a) / rtl_int28Value(b)));
      break;

    case RTL_OP_IMOD:
      if (unlikely(unlikely(!rtl_isInt28(a)) || unlikely(!rtl_isInt28(b)))) {
        rtl_triggerFault(M, "expected-int",
                         "imod instruction expects two int28 arguments.");
        continue;
      }

      VPUSH(rtl_int28(rtl_int28Value(a) % rtl_int28Value(b)));
      break;

    case RTL_OP_LT:
      VPUSH(rtl_cmp(M, a, b) <  0 ? RTL_TOP : RTL_NIL);
      break;

    case RTL_OP_LEQ:
      VPUSH(rtl_cmp(M, a, b) <= 0 ? RTL_TOP : RTL_NIL);
      break;

    case RTL_OP_GT:
      VPUSH(rtl_cmp(M, a, b) >  0 ? RTL_TOP : RTL_NIL);
      break;

    case RTL_OP_GEQ:
      VPUSH(rtl_cmp(M, a, b) >= 0 ? RTL_TOP : RTL_NIL);
      break;

    case RTL_OP_EQ:
      VPUSH(a == b ? RTL_TOP : RTL_NIL);
      break;

    case RTL_OP_NEQ:
      VPUSH(a != b ? RTL_TOP : RTL_NIL);
      break;

    case RTL_OP_ISO:
      VPUSH(rtl_cmp(M, a, b) == 0 ? RTL_TOP : RTL_NIL);
      break;

    case RTL_OP_FADD:
      if (unlikely(unlikely(!rtl_isFix14(a)) || unlikely(!rtl_isFix14(b)))) {
        rtl_triggerFault(M, "expected-fix14",
                         "fadd instruction expects two fix14 arguments.");
        continue;
      }

      VPUSH(rtl_fix14(rtl_fix14Value(a) + rtl_fix14Value(b)));
      break;

    case RTL_OP_FSUB:
      if (unlikely(unlikely(!rtl_isFix14(a)) || unlikely(!rtl_isFix14(b)))) {
        rtl_triggerFault(M, "expected-fix14",
                         "fsub instruction expects two fix14 arguments.");
        continue;
      }

      VPUSH(rtl_fix14(rtl_fix14Value(a) - rtl_fix14Value(b)));
      break;

    case RTL_OP_FMUL:
      if (unlikely(unlikely(!rtl_isFix14(a)) || unlikely(!rtl_isFix14(b)))) {
        rtl_triggerFault(M, "expected-fix14",
                         "fmul instruction expects two fix14 arguments.");
        continue;
      }

      VPUSH(rtl_fix14(rtl_fix14Value(a) * rtl_fix14Value(b)));
      break;

    case RTL_OP_FDIV:
      if (unlikely(unlikely(!rtl_isFix14(a)) || unlikely(!rtl_isFix14(b)))) {
        rtl_triggerFault(M, "expected-fix14",
                         "fdiv instruction expects two fix14 arguments.");
        continue;
      }

      VPUSH(rtl_fix14(rtl_fix14Value(a) / rtl_fix14Value(b)));
      break;

    case RTL_OP_SET_CAR:
      rtl_writeCar(M, a, b);

      VPUSH(b);
      break;

    case RTL_OP_SET_CDR:
      rtl_writeCdr(M, a, b);

      VPUSH(b);
      break;

    case RTL_OP_CONS:
      c = rtl_cons(M, a, b);

      VPUSH(c);
      break;

    case RTL_OP_SWAP:
      VPUSH(b);
      VPUSH(a);
      break;

    case RTL_OP_PUSH_FIRST:
      c = rtl_tuplePushFirst(M, a, b);

      VPUSH(c);
      break;

    case RTL_OP_PUSH_LAST:
      c = rtl_tuplePushLast(M, a, b);

      VPUSH(c);
      break;

    case RTL_OP_CONCAT:
      c = rtl_tupleConcat(M, a, b);

      VPUSH(c);
      break;

    case RTL_OP_GET:
      if (unlikely(!rtl_isTuple(a))) {
        rtl_triggerFault(M, "expected-tuple",
                         "get instruction expects an tuple as its first argument");
        continue;

      } else if (unlikely(!rtl_isInt28(b))) {
        rtl_triggerFault(M, "expected-int",
                         "get instruction expects an int28 as its second argument");
        continue;

      }

      rptr = rtl_reifyTuple(M, a, &len);
      if (unlikely(unlikely(rtl_int28Value(b) >= len) || unlikely(rtl_int28Value(b) < 0))) {
        rtl_triggerFault(M, "out-of-bounds",
                         "tuple index out of bounds in get instruction.");
        continue;

      }

      VPUSH(rptr[rtl_int28Value(b)]);
      break;


    case RTL_OP_INSERT:
      d = rtl_mapInsert(M, a, b, c);

      VPUSH(d);
      break;

    case RTL_OP_LOOKUP:
      VPUSH(rtl_mapLookup(M, a, b, c));
      break;

    case RTL_OP_SET_ELEM:
      if (unlikely(!rtl_isInt28(b))) {
        rtl_triggerFault(M, "expected-int",
                         "Second argument to set-elem instruction must be an int28.");
        continue;
      }

      rtl_writeTupleElem(M, a, rtl_int28Value(b), c);

      VPUSH(c);
      break;

    case RTL_OP_SLICE:
      if (unlikely(unlikely(!rtl_isInt28(b)) || unlikely(!rtl_isInt28(c)))) {
        rtl_triggerFault(M, "expected-int",
                         "beg and end arguments to slice instruction must "
                         "both be int28s.");
        continue;
      }

      d = rtl_tupleSlice(M, a, rtl_int28Value(b), rtl_int28Value(c));

      VPUSH(d);
      break;

    case RTL_OP_JMP8:
      M->pc += (int8_t)imm8;
      break;

    case RTL_OP_CJMP8:
      if (!rtl_isNil(a))
        M->pc += (int8_t)imm8;

      break;

    case RTL_OP_SYMBOL8:
      VPUSH(rtl_symbol(imm8));
      break;

    case RTL_OP_SELECTOR8:
      VPUSH(rtl_selector(imm8));
      break;

    case RTL_OP_INT8:
      VPUSH(rtl_int28(imm8));
      break;

    case RTL_OP_FIX8:
      VPUSH((imm8 << 4) | RTL_FIX14);
      break;

    case RTL_OP_CHAR8:
      VPUSH(rtl_char(imm8));
      break;

    case RTL_OP_FN8:
      VPUSH(rtl_function(imm8));
      break;

    case RTL_OP_UNRES8:
      VPUSH(rtl_unresolvedSymbol(imm8));
      break;

    case RTL_OP_CLOSURE8:
      wptr = rtl_allocGC(M, RTL_CLOSURE, &f, 2);

      wptr[0] = rtl_function(imm8);
      wptr[1] = M->env;

      VPUSH(f);
      break;

    case RTL_OP_JMP16:
      M->pc += (int16_t)imm16;
      break;

    case RTL_OP_CJMP16:
      if (!rtl_isNil(a))
        M->pc += (int16_t)imm16;

      break;

    case RTL_OP_SYMBOL16:
      VPUSH(rtl_symbol(imm16));
      break;

    case RTL_OP_SELECTOR16:
      VPUSH(rtl_selector(imm16));
      break;

    case RTL_OP_INT16:
      VPUSH(rtl_int28(imm16));
      break;

    case RTL_OP_FIX16:
      VPUSH((imm16 << 4) | RTL_FIX14);
      break;

    case RTL_OP_CHAR16:
      VPUSH(rtl_char(imm16));
      break;

    case RTL_OP_FN16:
      VPUSH(rtl_function(imm16));
      break;

    case RTL_OP_UNRES16:
      VPUSH(rtl_unresolvedSymbol(imm16));
      break;

    case RTL_OP_CLOSURE16:
      wptr = rtl_allocGC(M, RTL_CLOSURE, &f, 2);

      wptr[0] = rtl_function(imm16);
      wptr[1] = M->env;

      VPUSH(f);
      break;

    case RTL_OP_REST:
      // Load the current args tuple into a
      rptr = rtl_reifyTuple(M, M->env, &len);
      a    = rptr[len - 1];

      rtl_reifyTuple(M, a, &len);

      // Cons up the end of that tuple into a list.
      b = RTL_NIL;
      for (i = len; i > imm16; i--) {
        // Reload rptr on each iteration, in-case GC has moved the tuple.
        rptr = rtl_reifyTuple(M, a, &len);

        b = rtl_cons(M, rptr[i - 1], b);
      }

      // Allocate a new args tuple with the first elements of the old one and
      // our new list element.
      wptr = rtl_allocTuple(M, &c, imm16 + 1);
      rptr = rtl_reifyTuple(M, a, &len); // Reload again ...

      memcpy(wptr, rptr, sizeof(rtl_Word)*imm16);
      wptr[imm16] = b;

      // Finally create a new env tuple with our new args tuple at the end.
      rtl_reifyTuple(M, M->env, &len);
      wptr = rtl_allocTuple(M, &d, len);
      rptr = rtl_reifyTuple(M, M->env, &len); // Reload again ...

      memcpy(wptr, rptr, sizeof(rtl_Word)*(len - 1));
      wptr[len - 1] = c;

      M->env = d;
      break;


    case RTL_OP_APPLY_LIST:
      b = rtl_listToTuple(M, b);

      // fallthrough ...

    case RTL_OP_APPLY_TUPLE:
      f = a;
      a = b;

      // fallthrough ...

    case RTL_OP_CALL:
    case RTL_OP_TAIL:
    case RTL_OP_STATIC_CALL:
    case RTL_OP_STATIC_TAIL:
      switch (rtl_typeOf(f)) {
      case RTL_SELECTOR:
        rptr = rtl_reifyTuple(M, a, &len);
        if (unlikely(len != 1)) {
          rtl_triggerFault(M, "arg-count",
                           "selector expects a single argument when called as function.");
          continue;
        }

        VPUSH(rtl_mapLookup(M, rptr[0], f, RTL_NIL));
        break;

      case RTL_TUPLE:
        rptr = rtl_reifyTuple(M, a, &len);
        if (unlikely(len != 1)) {
          rtl_triggerFault(M, "arg-count",
                           "tuple expects a single argument when called as a function.");
          continue;

        } else if (unlikely(!rtl_isInt28(rptr[0]))) {
          rtl_triggerFault(M, "expected-int",
                           "tuple expects a single int28 argument when called as a "
                           "function.");
          continue;

        }

        a = rptr[0];
        rptr = rtl_reifyTuple(M, f, &len);
        if (unlikely(unlikely(rtl_int28Value(a) >= len) || unlikely(rtl_int28Value(a) < 0))) {
          rtl_triggerFault(M, "out-of-bounds",
                           "tuple index out of bounds when calling tuple as function.");
          continue;

        }

        VPUSH(rptr[rtl_int28Value(a)]);
        break;

      case RTL_MAP:
        rptr = rtl_reifyTuple(M, a, &len);
        if (unlikely(len != 1)) {
          rtl_triggerFault(M, "arg-count",
                           "tuple expects a single argument when called as a function.");
          continue;

        }

        VPUSH(rtl_mapLookup(M, f, rptr[0], RTL_NIL));
        break;

      case RTL_CLOSURE:
        rptr = __rtl_reifyPtr(M, f);
        f    = rptr[0];
        b    = rptr[1];

        if (opcode != RTL_OP_TAIL && opcode != RTL_OP_STATIC_TAIL) {
          RPUSH(f);
        } else {
          TAIL(f);
        }

        func   = rtl_reifyFunction(M->codeBase, f);
        M->pc  = func->as.lisp.code;
        M->env = rtl_tuplePushLast(M, b, a);
        break;

      case RTL_FUNCTION:
        func = rtl_reifyFunction(M->codeBase, f);
        if (func->isBuiltin) {
          RPUSH(f);

          rptr = rtl_reifyTuple(M, a, &len);
          b    = func->as.builtin.cFn(M, rptr, len);
          VPUSH(b);

          RPOP();
        } else {
          if (opcode != RTL_OP_TAIL && opcode != RTL_OP_STATIC_TAIL) {
            RPUSH(f);
          } else {
            TAIL(f);
          }
  
          M->pc = func->as.lisp.code;
          wptr  = rtl_allocTuple(M, &b, 1);

          wptr[0] = a;
          M->env  = b;
        }
        break;

      default:
        rtl_triggerFault(M, "not-callable",
                         "Trying to call an uncallable object...");
        continue;

      } break;

    case RTL_OP_LABELS:
      abort(); // RTL_OP_LABELS is handled specially above.
      break;

    case RTL_OP_TUPLE:
      VPUSH(a);
      break;

    case RTL_OP_VAR:
      rptr = rtl_reifyTuple(M, M->env, &len);
      if (unlikely(frame >= len)) {
        rtl_triggerFault(M, "out-of-bounds",
                         "var instruction references out-of-bounds frame.");
        continue;
      }

      rptr = rtl_reifyTuple(M, rptr[frame], &len);
      if (unlikely(index >= len)) {
        rtl_triggerFault(M, "arg-count",
                         "var instruction references out-of-bounds argument index; "
                         "function probably expected more arguments.");
        continue;
      }

      VPUSH(rptr[index]);
      break;

    case RTL_OP_SET_VAR:
      rptr = rtl_reifyTuple(M, M->env, &len);
      if (unlikely(frame >= len)) {
        rtl_triggerFault(M, "out-of-bounds",
                         "set-var instruction references out-of-bounds frame.");
        continue;
      }

      rtl_writeTupleElem(M, rptr[frame], index, a);

      VPUSH(a);
      break;

    case RTL_OP_JMP32:
      M->pc += (int32_t)immW;
      break;

    case RTL_OP_CJMP32:
      if (!rtl_isNil(a))
        M->pc += (int32_t)immW;

      break;

    case RTL_OP_CONST32:
      VPUSH(immW);
      break;

    case RTL_OP_CLOSURE32:
      wptr = rtl_allocGC(M, RTL_CLOSURE, &f, 2);

      wptr[0] = rtl_function(immW);
      wptr[1] = M->env;

      VPUSH(f);
      break;

    case RTL_OP_GET_DYN:
      VPUSH(rtl_getVar(M, immW));
      break;

    case RTL_OP_RESTORE_DYN:
      assert(M->dStackLen > 0);

      rtl_setVar(M, immW, M->dStack[--M->dStackLen]);
      break;

    case RTL_OP_SAVE_DYN:
      if (unlikely(M->dStackLen == M->dStackCap)) {
        M->dStackCap = M->dStackCap == M->dStackCap * 2;
        M->dStack    = realloc(M->dStack, sizeof(rtl_Word)*M->dStackCap);
      }

      M->dStack[M->dStackLen++] = rtl_getVar(M, immW);
      rtl_setVar(M, immW, a);

      VPUSH(a);
      break;

    case RTL_OP_SET_DYN:
      rtl_setVar(M, immW, a);
      VPUSH(a);
      break;

    case RTL_OP_UNDEF_VAR:
      c = RTL_MAP;
      c = rtl_recordSet(M, c, "type",
                        rtl_internSelector(NULL, "undefined-var"));

      c = rtl_recordSet(M, c, "message",
                        rtl_string(M, "Tried to reference an undefined global variable."));

      c = rtl_recordSet(M, c, "name", immW);

      __rtl_triggerFault(M, c);
      continue;


    case RTL_OP_UNDEF_CALL:
    case RTL_OP_UNDEF_TAIL:
      c = RTL_MAP;
      c = rtl_recordSet(M, c, "type",
                        rtl_internSelector(NULL, "undefined-function"));

      c = rtl_recordSet(M, c, "message",
                        rtl_string(M, "Tried to call an undefined function."));

      c = rtl_recordSet(M, c, "name", f);

      __rtl_triggerFault(M, c);
      continue;

    }
  }

 cleanup:
  rtl_popWorkingSet(M);
}

rtl_Word rtl_listToTuple(rtl_Machine *M, rtl_Word list)
{
  size_t   len;
  rtl_Word *ptr;
  rtl_Word tuple;
  size_t   i;

  RTL_PUSH_WORKING_SET(M, &list);

  assert(rtl_isCons(list) || rtl_isNil(list));

  len = rtl_listLength(M, list);
  ptr = rtl_allocTuple(M, &tuple, len);

  for (i = 0; list != RTL_NIL; list = rtl_cdr(M, list), i++) {
    ptr[i] = rtl_car(M, list);
  }

  rtl_popWorkingSet(M);

  return tuple;
}

rtl_Word rtl_applyList(rtl_Machine *M, rtl_Word fn, rtl_Word argList)
{
  rtl_Word *ptr;
  rtl_Word args = RTL_NIL,
           env  = RTL_NIL,
           out  = RTL_NIL;

  RTL_PUSH_WORKING_SET(M, &args, &env, &argList, &out);

  args = rtl_listToTuple(M, argList);

  ptr    = rtl_allocTuple(M, &env, 1);
  ptr[0] = args;

  M->env = env;

  rtl_popWorkingSet(M);

  return rtl_call(M, fn);
}

uint32_t rtl_newFuncID(rtl_CodeBase *cb, rtl_Word name)
{
  rtl_Function *func;

  if (cb->fnsCap == cb->fnsLen) {
    cb->fnsCap = cb->fnsCap == 0 ? 32 : 2*cb->fnsCap;
    cb->fns    = realloc(cb->fns, sizeof(rtl_Function *)*cb->fnsCap);
  }

  // Use calloc to get zeroed memory
  func = calloc(1, sizeof(rtl_Function));

  func->name    = name;
  func->version = 0;

  cb->fns[cb->fnsLen] = func;

  return cb->fnsLen++;
}

void rtl_newFuncVersion(rtl_CodeBase *cb, uint32_t fnID)
{
  rtl_Function *oldFn, *newFn;

  oldFn = cb->fns[fnID];
  newFn = calloc(1, sizeof(rtl_Function));

  newFn->name    = oldFn->name;
  newFn->version = oldFn->version + 1;

  free(oldFn);

  cb->fns[fnID] = newFn;
}

rtl_Function *rtl_getFuncByID(rtl_CodeBase *cb, uint32_t id)
{
  assert(id < cb->fnsLen);
  return cb->fns[id];
}

void rtl_emitByteToFunc(rtl_CodeBase *cb, uint32_t fnID, uint8_t b)
{
  rtl_Function *func;

  assert(fnID < cb->fnsLen);
  func = cb->fns[fnID];

  assert(!func->isBuiltin);

  if (unlikely(func->as.lisp.cap == func->as.lisp.len)) {
    func->as.lisp.cap = !func->as.lisp.cap ? 32 : 4*func->as.lisp.cap/3;
    func              = realloc(func, sizeof(rtl_Function) + func->as.lisp.cap);
    cb->fns[fnID]     = func;
  }

  func->as.lisp.code[func->as.lisp.len++] = b;
}

void rtl_emitShortToFunc(rtl_CodeBase *cb, uint32_t fnID, uint16_t u16)
{

  rtl_emitByteToFunc(cb, fnID, (u16 >> 0) & 0xFF);
  rtl_emitByteToFunc(cb, fnID, (u16 >> 8) & 0xFF);
}

void rtl_emitWordToFunc(rtl_CodeBase *cb, uint32_t fnID, rtl_Word w)
{
  rtl_emitByteToFunc(cb, fnID, (w >>  0) & 0xFF);
  rtl_emitByteToFunc(cb, fnID, (w >>  8) & 0xFF);
  rtl_emitByteToFunc(cb, fnID, (w >> 16) & 0xFF);
  rtl_emitByteToFunc(cb, fnID, (w >> 24) & 0xFF);
}

uint32_t rtl_nextFuncOffs(rtl_CodeBase *cb, uint32_t fnID)
{
  assert(fnID < cb->fnsLen);
  assert(!cb->fns[fnID]->isBuiltin);
  return cb->fns[fnID]->as.lisp.len;
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
  for (i = 0; i < M->wsStackLen; i++) printf(" ");
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
  for (i = 1; i < M->wsStackLen; i++) printf(" ");
  printf("< POP  '%s'\n", fName);
#endif

  assert(M->wsStackLen > 0);

  M->wsStackLen--;
}

void rtl_setVar(rtl_Machine *M, rtl_Word key, rtl_Word val)
{
  M->dynamic = rtl_mapInsert(M, M->dynamic, key, val);
}

rtl_Word rtl_getVar(rtl_Machine *M, rtl_Word key)
{
  rtl_Word const sentinel = rtl_internSelector("std", "var-lookup-sentinel");

  rtl_Word w;

  w = rtl_mapLookup(M, M->dynamic, key, sentinel);

  if (unlikely(w == sentinel)) {
    RTL_PUSH_WORKING_SET(M, &w);

    w = RTL_MAP;
    w = rtl_mapInsert(M, w, rtl_internSelector(NULL, "type"),
                      rtl_internSelector(NULL, "undefined-dynamic-var"));
    w = rtl_mapInsert(M, w, rtl_internSelector(NULL, "message"),
                      rtl_string(M, "Tried to read from an undefined "
                                    "dynamic variable."));
    w = rtl_mapInsert(M, w, rtl_internSelector(NULL, "name"),
                      key);

    __rtl_triggerFault(M, w);

    w = RTL_NIL;

    rtl_popWorkingSet(M);
  }

  return w;
}

bool rtl_isString(rtl_Machine *M, rtl_Word w)
{
  rtl_Word const *rptr;
  size_t len, i;

  if (rtl_isTuple(w)) {
    rptr = rtl_reifyTuple(M, w, &len);

    for (i = 0; i < len; i++) {
      if (!rtl_isChar(rptr[i])) return false;
    }

    return true;
  } else {
    return false;
  }
}

void rtl_reifyString(rtl_Machine *M, rtl_Word str, char *buf, size_t cap)
{
  char *end, *prev, *next;
  rtl_Word const *rptr;
  size_t len, i;

  end  = buf + cap - 1;
  next = buf;

  assert(rtl_isString(M, str));

  rptr = rtl_reifyTuple(M, str, &len);
  for (i = 0; i < len && next < end; i++) {
    prev = next;
    next = utf8catcodepoint(next, rtl_charValue(rptr[i]), end - next);

    if (!next) {
      *prev = '\0';
      return;
    }
  }

  *end = 0;
}

uint32_t rtl_stringSize(rtl_Machine *M, rtl_Word str)
{
  rtl_Word const *rptr;
  size_t len, i;

  uint32_t size;

  size = 0;

  rptr = rtl_reifyTuple(M, str, &len);

  for (i = 0; i < len; i++) {
    size += utf8codepointsize(rtl_charValue(rptr[i]));
  }

  return size;
}

rtl_Word rtl_string(rtl_Machine *M, char const *cstr)
{
  size_t len, i;

  rtl_Word str;
  rtl_Word *wptr;

  utf8_int32_t ch;

  len  = utf8len(cstr);
  wptr = rtl_allocTuple(M, &str, len);

  for (i = 0; i < len; i++) {
    cstr = (char const *)utf8codepoint((void const *)cstr, &ch);
    wptr[i] = rtl_char(ch);
  }

  return str;
}

void __rtl_triggerFault(rtl_Machine *M, rtl_Word data)
{
  rtl_Word lispHandler;

  if (data == RTL_NIL) {
    rtl_triggerFault(M, "nil-fault", "Nil was passed as fault data!");
  } else {
    lispHandler = rtl_getVar(M, rtl_intern("std", "*error-handler*"));

    rtl_callWithArgs(M, lispHandler, data);
  }
}

void rtl_triggerFault(rtl_Machine *M, char const *type, char const *message)
{
  rtl_Word data = RTL_NIL,
           key  = RTL_NIL,
           val  = RTL_NIL;

  RTL_PUSH_WORKING_SET(M, &data, &key, &val);

  data = RTL_MAP;

  key  = rtl_internSelector(NULL, "type");
  val  = rtl_internSelector(NULL, type);
  data = rtl_mapInsert(M, data, key, val);

  key  = rtl_internSelector(NULL, "message");
  val  = rtl_string(M, message);
  data = rtl_mapInsert(M, data, key, val);

  rtl_popWorkingSet(M);

  __rtl_triggerFault(M, data);
}

rtl_Word rtl_tupleConcat(rtl_Machine *M, rtl_Word a, rtl_Word b)
{
  rtl_Word const *aptr, *bptr;
  size_t aLen, bLen;
  rtl_Word *wptr;
  rtl_Word out = RTL_NIL;

  RTL_PUSH_WORKING_SET(M, &a, &b, &out);

  rtl_reifyTuple(M, a, &aLen);
  rtl_reifyTuple(M, b, &bLen);

  wptr = rtl_allocTuple(M, &out, aLen + bLen);
  aptr = rtl_reifyTuple(M, a, &aLen);
  bptr = rtl_reifyTuple(M, b, &bLen);

  memcpy(wptr,        aptr, sizeof(rtl_Word)*aLen);
  memcpy(wptr + aLen, bptr, sizeof(rtl_Word)*bLen);

  rtl_popWorkingSet(M);

  return out;
}

rtl_Word rtl_tupleSlice(rtl_Machine *M,
                        rtl_Word    tuple,
                        uint32_t    beg,
                        uint32_t    end)
{
  size_t    inLen,
            outLen;

  rtl_Word  out = RTL_NIL;

  rtl_Word const *rptr;
  rtl_Word       *wptr;

  RTL_PUSH_WORKING_SET(M, &tuple, &out);

  rtl_reifyTuple(M, tuple, &inLen);

  // TODO: Trigger faults here ...
  if (unlikely(inLen < end)) {
    rtl_triggerFault(M, "bad-slice",
                     "Trying to slice past the end of a tuple.");
    return RTL_NIL;
  }
    
  if (unlikely(end < beg)) {
    rtl_triggerFault(M, "bad-slice",
                     "Trying to slice with end index < beginning index.");
    return RTL_NIL;
  }

  outLen = end - beg;

  wptr = rtl_allocTuple(M, &out, outLen);
  rptr = rtl_reifyTuple(M, tuple, &inLen);

  memcpy(wptr, rptr + beg, sizeof(rtl_Word)*outLen);

  rtl_popWorkingSet(M);

  return out;
}


rtl_Word rtl_tuplePushLast(rtl_Machine *M, rtl_Word in, rtl_Word w)
{
  rtl_Word const *rptr;
  rtl_Word       *wptr;

  size_t   len;
  rtl_Word out = RTL_NIL;

  RTL_PUSH_WORKING_SET(M, &in, &out, &w);

  if (in != RTL_NIL) {
    rptr = rtl_reifyTuple(M, in, &len);
  } else {
    rptr = NULL;
    len  = 0;
  }

  wptr = rtl_allocTuple(M, &out, len + 1);
  wptr[len] = w;

  if (in != RTL_NIL) rptr = rtl_reifyTuple(M, in, &len);
  memcpy(wptr, rptr, sizeof(rtl_Word)*len);

  rtl_popWorkingSet(M);

  return out;
}

rtl_Word rtl_tuplePushFirst(rtl_Machine *M, rtl_Word in, rtl_Word w)
{
  rtl_Word const *rptr;
  rtl_Word       *wptr;

  size_t   len;
  rtl_Word out = RTL_NIL;

  RTL_PUSH_WORKING_SET(M, &in, &out, &w);

  if (in != RTL_NIL) {
    rptr = rtl_reifyTuple(M, in, &len);
  } else {
    rptr = NULL;
    len  = 0;
  }

  wptr = rtl_allocTuple(M, &out, len + 1);
  wptr[0] = w;

  if (in != RTL_NIL) rptr = rtl_reifyTuple(M, in, &len);
  memcpy(wptr + 1, rptr, sizeof(rtl_Word)*len);

  rtl_popWorkingSet(M);

  return out;
}

rtl_Word rtl_tuple(rtl_Machine *M, rtl_Word *elems, size_t elemsLen)
{
  rtl_Word *workingSet[elemsLen + 1];
  rtl_Word *wptr;
  rtl_Word tuple;

  size_t i;

  workingSet[elemsLen] = NULL;
  for (i = 0; i < elemsLen; i++)
    workingSet[i] = elems + i;

  rtl_pushWorkingSet(M, workingSet);

  wptr = rtl_allocTuple(M, &tuple, elemsLen);
  memcpy(wptr, elems, sizeof(rtl_Word)*elemsLen);

  rtl_popWorkingSet(M);

  return tuple;
}

rtl_Word __rtl_callWithArgs(rtl_Machine *M,
                            rtl_Word    callable,
                            rtl_Word    *args,
                            size_t      argsLen)
{
  rtl_Word argsTuple = RTL_NIL,
           tmpTuple  = RTL_NIL,
           envTuple  = RTL_NIL;

  rtl_Word const *rptr;

  RTL_PUSH_WORKING_SET(M, &argsTuple, &envTuple);


  argsTuple = rtl_tuple(M, args, argsLen);

  switch (rtl_typeOf(callable)) {
  case RTL_CLOSURE:
    rptr = __rtl_reifyPtr(M, callable);

    callable = rptr[0];
    envTuple = rptr[1];

    envTuple = rtl_tuplePushLast(M, envTuple, argsTuple);
    break;

  case RTL_FUNCTION:
    tmpTuple = argsTuple;
    envTuple = rtl_tuple(M, &tmpTuple, 1);
    break;

  default: {
      char msg[512];

      snprintf(msg, 512, "Can't rtl_callWithArgs object of type %s.",
               rtl_typeNameOf(callable));
      rtl_triggerFault(M, "uncallable",
                       msg);
      return RTL_NIL;
    }
  }

  M->env = envTuple;

  rtl_popWorkingSet(M);

  return rtl_call(M, callable);
}

static
void addToBackSet(rtl_Machine *M, rtl_Word w)
{
  if (unlikely(M->backSetLen == M->backSetCap)) {
    M->backSetCap = 2*M->backSetCap;
    M->backSet    = realloc(M->backSet, M->backSetCap*sizeof(rtl_Word));
  }

  M->backSet[M->backSetLen++] = w;
}

void rtl_writeCar(rtl_Machine *M, rtl_Word cons, rtl_Word val)
{
  rtl_Word *ptr;

  uint32_t consGen, consOffs;

  if (!rtl_isCons(cons)) {
    rtl_triggerFault(M, "expected-cons",
                     "First argument of rtl_writeCar must be a cons.");
    return;
  }

  ptr = __rtl_reifyPtr(M, cons);
  ptr[0] = val;

  consGen  = __rtl_ptrGen(cons);
  consOffs = __rtl_ptrOffs(cons);

  if (rtl_isPtr(val) && consGen > __rtl_ptrGen(val)) {
    // val is younger than cons, add a pointer to the car to the backset.
    addToBackSet(M, mkPtr(RTL_HEADER, consGen, consOffs));
  }
}

void rtl_writeCdr(rtl_Machine *M, rtl_Word cons, rtl_Word val)
{
  rtl_Word *ptr;

  uint32_t consGen, consOffs;

  if (!rtl_isCons(cons)) {
    rtl_triggerFault(M, "expected-cons",
                     "First argument of rtl_writeCdr must be a cons.");
    return;
  }

  ptr = __rtl_reifyPtr(M, cons);
  ptr[1] = val;

  consGen  = __rtl_ptrGen(cons);
  consOffs = __rtl_ptrOffs(cons);

  if (rtl_isPtr(val) && consGen > __rtl_ptrGen(val)) {
    // val is younger than cons, add a pointer to the car to the backset.
    addToBackSet(M, mkPtr(RTL_HEADER, consGen, consOffs + 1));
  }
}

void rtl_writeTupleElem(rtl_Machine *M,
                        rtl_Word    tuple,
                        uint32_t    idx,
                        rtl_Word    val)
{
  rtl_Word *ptr;
  uint32_t tplGen, tplOffs;
  size_t len;

  if (!rtl_isTuple(tuple)) {
    rtl_triggerFault(M, "expected-tuple",
                     "First argument of rtl_writeTupleElem must be a tuple.");
    return;
  }

  ptr = __rtl_reifyPtr(M, tuple);
  len = rtl_int28Value(ptr[0]);

  if (len <= idx) {
    rtl_triggerFault(M, "bad-write-index",
                     "Trying to write to a tuple at an out-of-bounds index!");
    return;
  }

  ptr[1 + idx] = val;

  tplGen  = __rtl_ptrGen(tuple);
  tplOffs = __rtl_ptrOffs(tuple);

  if (rtl_isPtr(val) && tplGen > __rtl_ptrGen(val)) {
    // val is younger than cons, add a pointer to the car to the backset.
    addToBackSet(M, mkPtr(RTL_HEADER, tplGen, tplOffs + 1 + idx));
  }
}
