#ifndef _RTL_RT_LISP_H_
#define _RTL_RT_LISP_H_

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include <assert.h>

#define _RTL_INSIDE_RT_LISP_H_

#include "rtl/BitMap.h"

// `rtl_Word' is the fundamental type operated on by the runtime LISP machine.
// It is used to represent both code and data internally. All addresses are
// expressed in terms of `rtl_Word's.
//
// Because an `rtl_Word' might encode a heap address, they cannot be shared
// between seperate machines.
//
// The low 4 bits of an `rtl_Word' encode its type, and the next 28 bits contain
// data.
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
  RTL_TOP      = 9,

  // This is a special word type which only exists on the heap. Its upper 28
  // bits are a an unsigned integer indicating the number of words occupied by a
  // string or tuple immediately after it on the heap.
  RTL_LENGTH = 10,

  // This word type is used to encode addresses in the code segment, rather than
  // the heap segment.
  RTL_ADDR = 11,

  RTL_BUILTIN = 12,

  RTL_CLOSURE = 13,

  // 14, 15 aren't yet in use

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
  rtl_Word *pc;
  rtl_Word envFrame;
} rtl_RetAddr;

typedef struct rtl_CodePage {
  uint32_t len;
  rtl_Word code[];
} rtl_CodePage;

typedef struct rtl_Machine {
  rtl_Heap heap;

  rtl_Word envFrame;

  rtl_Word *vStack;
  size_t   vStackLen;
  size_t   vStackCap;

  uint8_t *pc;

  rtl_RetAddr *rStack;
  size_t      rStackLen;
  size_t      rStackCap;

  // All code addresses are relative to this pointer.
  uint8_t *code;

  // This is the end of the code. 
  uint8_t *codeEnd;

  rtl_Error error;
} rtl_Machine;

// Initialize the machine M.
void rtl_initMachine(rtl_Machine *M);

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
// #include "rtl/top.h"

#include "rtl/rto.h"

#include "rtl/instructions.h"

#include "rtl/debug.h"

#undef _RTL_INSIDE_RT_LISP_H_

rtl_Error rtl_runSnippet(rtl_Machine *M, uint8_t *code);

#endif // rt-lisp.h
