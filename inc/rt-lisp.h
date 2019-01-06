#ifndef _RTL_RT_LISP_H_
#define _RTL_RT_LISP_H_

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include <assert.h>

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
  RTL_ADDR   = 11,

  // 12 .. 15 aren't yet in use

  RTL_MAX = 16,
} rtl_WordType;

static inline
uint32_t rtl_typeOf(rtl_Word w)
{
  return w & 0xF;
}

typedef enum rtl_Error {
  RTL_OK,
  RTL_ERR_STACK_UNDERFLOW,
} rtl_Error;

#define _RTL_INSIDE_RT_LISP_H_

#include "rtl/nil.h"
#include "rtl/symbol.h"
// #include "rtl/int28.h"
// #include "rtl/fix14.h"
// #include "rtl/tuple.h"
// #include "rtl/string.h"
// #include "rtl/record.h"
// #include "rtl/cons.h"
// #include "rtl/top.h"

#include "rtl/BitMap.h"

#undef _RTL_INSIDE_RT_LISP_H_

typedef struct rtl_Generation {
  size_t     fillPtr;

  rtl_BitMap blkFirst,
             blkLast;

  rtl_Word   words[];
} rtl_Generation;

#define RTL_NBR_GENERATIONS 16

typedef struct rtl_Heap {
  // Each generation is twice the size of the previous generation.
  rtl_Generation *gen[RTL_NBR_GENERATIONS];

  size_t nbrGen;
} rtl_Heap;

typedef struct rtl_Machine {
  rtl_Heap *heap;

  // This is a pointer to the current environment frame, it points to a tuple on
  // the heap.
  rtl_Word *envFrame;

  rtl_Word reg[32];

  rtl_Word *pc;

  rtl_Word **rStack;
  size_t   rStackLen;
  size_t   rStackCap;

  rtl_Error error;
} rtl_Machine;

void rtl_initMachine(rtl_Machine *machine);

rtl_Error rtl_getError(rtl_Machine *machine);

#endif // rt-lisp.h
