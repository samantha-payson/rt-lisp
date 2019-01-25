#ifndef _RTL_RT_LISP_H_
#define _RTL_RT_LISP_H_

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include <assert.h>
#include <stdio.h>

#define _RTL_INSIDE_RT_LISP_H_

#include "rtl/BitMap.h"

// `rtl_Word' is the fundamental type operated on by the runtime LISP machine.
// It is used to represent data internally. All heap addresses are expressed in
// terms of `rtl_Word's.
//
// Because an `rtl_Word' might encode a heap address, they cannot be shared
// between seperate rtl_Machine's.
//
// The low 4 bits of an `rtl_Word' encode its type, and the remaining 28 bits
// contain data.
typedef uint32_t rtl_Word;

typedef enum rtl_WordType {
  RTL_NIL      = 0,
  RTL_SYMBOL   = 1,
  RTL_SELECTOR = 2,
  RTL_INT28    = 3,
  RTL_FIX14    = 4,
  RTL_TUPLE    = 5,
  RTL_STRING   = 6,
  RTL_RECORD   = 7,
  RTL_CONS     = 8,

  // This is a special word type which only exists on the heap. Its upper 28
  // bits are a an unsigned integer indicating the number of words occupied by a
  // string or tuple immediately after it on the heap.
  RTL_LENGTH = 9,

  // This word type is used to encode addresses in the code segment, rather than
  // the heap segment.
  RTL_ADDR = 10,

  RTL_BUILTIN = 11,
  RTL_CLOSURE = 12,

  RTL_UNRESOLVED_SYMBOL = 13,

  // 14 is unused still ..

  RTL_TOP = 15,

  RTL_MAX = 16,
} rtl_WordType;

char const *rtl_typeName(rtl_WordType type);

static inline
uint32_t rtl_typeOf(rtl_Word w)
{
  return w & 0xF;
}

static inline
char const *rtl_typeNameOf(rtl_Word w)
{
  return rtl_typeName(rtl_typeOf(w));
}

typedef struct rtl_Generation {
  // The number of this generation.
  int nbr;

  // The index of the next word to be allocated from this generation.
  size_t fillPtr;

  // This is where the fill pointer was at the beginning of this collection
  // cycle. This is used by the moveWord helper in rt-lisp.c, it has no other
  // purpose.
  size_t preMoveFillPtr;

  // The total number of words this generation can contribute.
  size_t capacity;

  // A BitMap where we can mark the reachable blocks during collection.
  rtl_BitMap *marks;

  // The actual word data managed by this generation.
  rtl_Word words[];
} rtl_Generation;

#define RTL_MAX_GENERATIONS 16

typedef struct rtl_Heap {
  // Each generation is twice the size of the previous generation.
  rtl_Generation *gen[RTL_MAX_GENERATIONS];
} rtl_Heap;

typedef enum rtl_Error {
  RTL_OK,
  RTL_ERR_INVALID_OPERATION,
  RTL_ERR_OUT_OF_MEMORY,
  RTL_ERR_STACK_UNDERFLOW,
  RTL_ERR_EXPECTED_TUPLE,
  RTL_ERR_EXPECTED_CONS,
  RTL_ERR_EXPECTED_INT28,
  RTL_ERR_EXPECTED_FIX14,
} rtl_Error;

typedef struct rtl_RetAddr {
  uint8_t  *pc;
  rtl_Word env;
} rtl_RetAddr;

typedef struct rtl_Page {
  // The number of bytes in this page which are in use.
  uint32_t len;

  // The number of bytes in this page, including unused space at the end.
  uint32_t cap;

  // Every time that a new page is installed for a given pageID, the version is
  // incremented. This allows us to invalidate rtl_CallSites which refer to old
  // versions of the page.
  uint32_t version;

  // The actual bytecode.
  uint8_t code[];
} rtl_Page;

typedef rtl_Word **rtl_WorkingSet;

typedef struct rtl_Machine {
  rtl_Heap heap;

  rtl_Word env;

  rtl_Word *vStack;
  size_t   vStackLen;
  size_t   vStackCap;

  uint8_t *pc;

  rtl_RetAddr *rStack;
  size_t      rStackLen;
  size_t      rStackCap;

  rtl_WorkingSet *wsStack;
  size_t         wsStackLen;
  size_t         wsStackCap;

  rtl_Page **pages;
  size_t   pagesLen;
  size_t   pagesCap;

  rtl_Error error;
} rtl_Machine;

// Initialize the machine M.
void rtl_initMachine(rtl_Machine *M);

static inline
size_t rtl_push(rtl_Machine *M, rtl_Word w)
{
  size_t pos;

  pos = M->vStackLen;

  M->vStack[M->vStackLen++] = w;

  return pos;
}

static inline
rtl_Word rtl_pop(rtl_Machine *M)
{
  return M->vStack[--M->vStackLen];
}

static inline
rtl_Word rtl_peek(rtl_Machine *M, int n)
{
  return M->vStack[M->vStackLen - (1 + n)];
}

static inline
void rtl_popK(rtl_Machine *M, int k)
{
  M->vStackLen -= k;
}

// Replace the pageID'th page with a new empty page and increment the
// version. This will free the old version of the page.
void rtl_newPageVersion(rtl_Machine *M, uint16_t pageID);

// Add a byte to the end of the pageID'th page.
//
// Returns the address of that byte.
rtl_Word rtl_emitByteToPage(rtl_Machine *M, uint16_t pageID, uint8_t b);

// Add an unsigned 16-bit short to the end of the pageID'th page, in
// little-endian encoding. This is the format expected by instructions with a
// 16-bit argument.
//
// Returns the address of that short.
rtl_Word rtl_emitShortToPage(rtl_Machine *M, uint16_t pageID, uint16_t u16);

// Add a word to the end of the pageID'th page, in little-endian encoding. This
// is the format expected by instructions with a word argument.
//
// Returns the address of that word.
rtl_Word rtl_emitWordToPage(rtl_Machine *M, uint16_t pageID, rtl_Word w);

rtl_Word rtl_nextAddrInPage(rtl_Machine *M, uint16_t pageID);

// Create a new empty page and return its ID.
uint16_t rtl_newPageID(rtl_Machine *M);

void rtl_pushWorkingSet(rtl_Machine *M, rtl_WorkingSet ws);

void rtl_popWorkingSet(rtl_Machine *M);

#define RTL_PUSH_WORKING_SET(M, PTRS...)		\
  rtl_Word *___rtl_workingSet___[] = { PTRS, NULL };	\
  rtl_pushWorkingSet(M, ___rtl_workingSet___);		\
  // End of multi-line macro

// Return any pending error from M, or RTL_OK if there is no error. This
// function does not clear the error.
static inline
rtl_Error rtl_peekError(rtl_Machine *M) { return M->error; }

// Return any pending error from M, or RTL_OK if there is no error, then clear
// the error.
static inline
rtl_Error rtl_getError(rtl_Machine *M) {
  rtl_Error err;

  err      = M->error;
  M->error = RTL_OK;

  return err;
}

// Return a human-readable string describing err.
char const *rtl_errString(rtl_Error err);

// Returns a pointer to a newly allocated block of nbr words. Writes a word of
// type t with this pointer to w. t must be one of:
//
//   - RTL_TUPLE
//   - RTL_STRING
//   - RTL_RECORD
//   - RTL_CONS
//
// Errors:
//   RTL_ERR_INVALID_OPERATION:  if t is not one of the types listed above.
//
//   RTL_ERR_OUT_OF_MEMORY:      if the allocator can't allocate a block of nbr
//                               words.
//
rtl_Word *rtl_allocGC(rtl_Machine *M, rtl_WordType t, rtl_Word *w, size_t nbr);

rtl_Word *rtl_allocTuple(rtl_Machine *M, rtl_Word *w, size_t len);

void rtl_testGarbageCollector(size_t count);

// Return true if w is one of the pointer types:
//
//   - RTL_TUPLE
//   - RTL_STRING
//   - RTL_RECORD
//   - RTL_CONS
//
static inline
int rtl_isPtr(rtl_Word w) {
  switch (rtl_typeOf(w)) {
  case RTL_TUPLE:
  case RTL_STRING:
  case RTL_RECORD:
  case RTL_CONS:
    return 1;

  default:
    return 0;
  }
}

#include "rtl/nil.h"
#include "rtl/symbol.h"
#include "rtl/int28.h"
#include "rtl/fix14.h"
#include "rtl/tuple.h"
// #include "rtl/string.h"
// #include "rtl/record.h"
#include "rtl/cons.h"
#include "rtl/addr.h"
// #include "rtl/top.h"

#include "rtl/rto.h"
#include "rtl/instructions.h"
#include "rtl/intrinsic.h"
#include "rtl/debug.h"
#include "rtl/compiler.h"

#undef _RTL_INSIDE_RT_LISP_H_

rtl_Error rtl_run(rtl_Machine *M, rtl_Word addr);

rtl_Error rtl_runSnippet(rtl_Machine *M, uint8_t *code, uint16_t len);

rtl_Word rtl_resolveSymbol(rtl_Compiler        *C,
			   rtl_NameSpace const *ns,
			   uint32_t            unresID);

rtl_Word rtl_read(rtl_Compiler *C, FILE *f);

#endif // rt-lisp.h
