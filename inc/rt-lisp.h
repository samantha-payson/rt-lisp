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

#ifndef _RTL_RT_LISP_H_

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

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
  RTL_CHAR     = 5,
  RTL_TUPLE    = 6,
  RTL_MAP      = 7,
  RTL_CONS     = 8,

  // This is a special word type which only exists on the heap. Its upper 28
  // bits depend on the type of word pointing at it:
  //
  //    RTL_TUPLE:  an unsigned 28-bit integer encoding the number of elements in
  //                the tuple.
  //
  //    RTL_NATIVE: an unsigned 28-bit integer encoding the number of bytes.
  //
  //    RTL_MAP: a 28-bit bitmap, indicating the number of key/value pairs in
  //            this level of the HAMT.
  RTL_HEADER = 9,

  RTL_FUNCTION = 10,
  RTL_CLOSURE  = 11,

  // Used to represent arbitrary binary data, useful for encoding native types.
  RTL_NATIVE = 12,

  RTL_UNRESOLVED_SYMBOL = 13,

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

static inline
bool rtl_isHeader(rtl_Word w) { return rtl_typeOf(w) == RTL_HEADER; }

static inline
uint32_t rtl_headerValue(rtl_Word w) { return w >> 4; }

static inline
rtl_Word rtl_header(uint32_t val28) { return (val28 << 4) | RTL_HEADER; }

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

#define RTL_MAX_GENERATIONS 18

typedef struct rtl_Heap {
  // Each generation is twice the size of the previous generation.
  rtl_Generation *gen[RTL_MAX_GENERATIONS];
} rtl_Heap;

typedef struct rtl_RetAddr {
  uint8_t  *pc;
  rtl_Word env;

  rtl_Word fn; // RTL_FUNCTION which was ultimately called.
} rtl_RetAddr;

typedef struct rtl_Machine rtl_Machine;

typedef rtl_Word (*rtl_BuiltinFn)(rtl_Machine    *M,
                                  rtl_Word const *args,
                                  size_t         argsLen);

typedef struct rtl_Function {
  // The name of this function
  rtl_Word name;

  // True if this is a builtin function.
  bool isBuiltin;

  // Every time that a new function is installed for a given fnID, the version
  // is incremented. This allows us to invalidate rtl_CallSites which refer to
  // old versions of the function.
  uint32_t version;

  union {
    struct {
      // The builtin function, if isBuiltin is true (otherwise undefined).
      rtl_BuiltinFn cFn;
    } builtin;

    struct {
      // The number of bytes in this function which are in use.
      uint32_t len;

      // The number of bytes in this function, including unused space at the end.
      uint32_t cap;

      // The actual bytecode.
      uint8_t code[];
    } lisp;
  } as;
} rtl_Function;

#define RTL_CODE_BASE_FN_HASH_SIZE 61

// TODO: Make this able to represent 
typedef struct rtl_FnDef {
  rtl_Word name;
  rtl_Word fn, macro;

  struct rtl_FnDef *next;
} rtl_FnDef;

typedef struct rtl_CodeBase {
  rtl_Function **fns;
  size_t       fnsLen;
  size_t       fnsCap;

  rtl_FnDef *fnsByName[RTL_CODE_BASE_FN_HASH_SIZE];
} rtl_CodeBase;

void rtl_initCodeBase(rtl_CodeBase *codeBase);

typedef rtl_Word **rtl_WorkingSet;

typedef struct rtl_UnwindHandler {
  // A unary function that should be called when this handler is resolved.
  //
  // If this is called as a result of an exception, the exception object will
  // be passed as the functions argument. Otherwise nil will be passed.
  //
  // If the function fails to handle an exception, it should return nil. It may
  // return any other value if it succeeds.
  //
  // As a special case, this value will be nil if this unwind point represents
  // a return to the C stack.
  rtl_Word  fn;

  // The top of the return stack when unwinding to this point.
  size_t    rStackIdx;

  // This is where execution should resume after successfully handling an
  // exception.
  uint8_t   *resumePC;
} rtl_UnwindHandler;

typedef struct rtl_Exception {
  rtl_Word    data;

  size_t      stackLen;

  // This is the stack at the point where the exception was thrown. 
  //
  // NOTE: The env field of these rtl_RetAddr's is NOT updated when garbage
  //       collection occurs and hence it is UNDEFINED BEHAVIOUR to access them.
  rtl_RetAddr stack[];
} rtl_Exception;

struct rtl_Machine {
  rtl_Heap heap;

  rtl_Word env;

  rtl_Word dynamic;

  uint8_t *pc;

  // Saved dynamic variables.
  //
  // Used by the save-dyn and restore-dyn instructions.
  rtl_Word *dStack;
  size_t   dStackLen;
  size_t   dStackCap;

  // Value stack
  //
  // The main stack used for data manipulation.
  rtl_Word *vStack;
  size_t   vStackLen;
  size_t   vStackCap;

  // Return stack
  rtl_RetAddr *rStack;
  size_t      rStackLen;
  size_t      rStackCap;

  // Working set stack
  //
  // Keeps track of words stored on the C stack, so the
  // GC can update them properly on collection.
  rtl_WorkingSet *wsStack;
  size_t         wsStackLen;
  size_t         wsStackCap;

  // Unwind Handler stack
  rtl_UnwindHandler *uwStack;
  size_t            uwStackLen;
  size_t            uwStackCap;

  // Pointers to heap words which may point at younger generations.
  //
  // This is what's called a "rememberedSet" in the GC literature sometimes.
  rtl_Word *backSet;
  size_t    backSetLen;
  size_t    backSetCap;

  // The exception currently being handled by this machine. This will be NULL if
  // there is no pending exception.
  rtl_Exception *exception;

  // True if the machine has currently yielded. This will cause an error if it
  // is started up from a non-rtl_resume entrypoint.
  bool yield;

  rtl_CodeBase *codeBase;
};

#define RTL_LIKELY(x)       __builtin_expect(!!(x),1)
#define RTL_UNLIKELY(x)     __builtin_expect((x),0)

#define RTL_UNWIND(M) if (RTL_UNLIKELY((M)->exception != NULL))

#ifndef NDEBUG
# define RTL_ASSERT_NO_UNWIND(M) ({               \
      RTL_UNWIND (M) { abort(); }                 \
      ((void) 0);                                 \
    })                                            \
// end of multi-line macro
#else
# define RTL_ASSERT_NO_UNWIND(M) ((void)0)
#endif

// Initialize the machine M.
void rtl_initMachine(rtl_Machine *M, rtl_CodeBase *codeBase);

void rtl_resetMachine(rtl_Machine *M);

void rtl_throw(rtl_Machine *M, rtl_Word data);

static inline
size_t rtl_push(rtl_Machine *M, rtl_Word w)
{
  size_t pos;

  pos = M->vStackLen;

  if (RTL_UNLIKELY(M->vStackLen == M->vStackCap)) {
    M->vStackCap = 2*M->vStackCap;
    M->vStack = realloc(M->vStack, sizeof(rtl_Word)*M->vStackCap);
  }

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

// Replace the fnID'th function with a new empty page and increment the
// version. This will free the old version of the page.
void rtl_newFuncVersion(rtl_CodeBase *cb, uint32_t fnID);

// Add a byte to the end of the fnID'th function.
void rtl_emitByteToFunc(rtl_CodeBase *cb, uint32_t fnID, uint8_t b);

// Add an unsigned 16-bit short to the end of the fnID'th function, in
// little-endian encoding. This is the format expected by instructions with a
// 16-bit argument.
void rtl_emitShortToFunc(rtl_CodeBase *cb, uint32_t fnID, uint16_t u16);

// Add a word to the end of the fnID'th function, in little-endian
// encoding. This is the format expected by instructions with a word argument.
void rtl_emitWordToFunc(rtl_CodeBase *cb, uint32_t fnID, rtl_Word w);

// Return the offset of the next byte to be emitted to this page.
uint32_t rtl_nextFuncOffs(rtl_CodeBase *cb, uint32_t fnID);

// Create a new empty function and return its ID.
uint32_t rtl_newFuncID(rtl_CodeBase *cb, rtl_Word name);

void  __rtl_pushWorkingSet(rtl_Machine *M, rtl_WorkingSet ws, char const *fName);
#define rtl_pushWorkingSet(M, WS) __rtl_pushWorkingSet(M, WS, __func__)

void  __rtl_popWorkingSet(rtl_Machine *M, char const *fName);
#define rtl_popWorkingSet(M) __rtl_popWorkingSet(M, __func__)



#define RTL_PUSH_WORKING_SET(M, PTRS...)                \
  rtl_Word *___rtl_workingSet___[] = { PTRS, NULL };    \
  rtl_pushWorkingSet(M, ___rtl_workingSet___);          \
  // End of multi-line macro

// Returns a pointer to a newly allocated block of nbr words. Writes a word of
// type t with this pointer to w. t must be one of:
//
//   - RTL_TUPLE
//   - RTL_MAP
//   - RTL_CONS
//   - RTL_NATIVE
//   - RTL_CLOSURE
//
rtl_Word *rtl_allocGC(rtl_Machine *M, rtl_WordType t, rtl_Word *w, size_t nbr);

void rtl_testGarbageCollector(size_t count);

// Return true if w is one of the pointer types:
//
//   - RTL_TUPLE
//   - RTL_MAP
//   - RTL_NATIVE
//   - RTL_CONS
//   - RTL_CLOSURE
//
static inline
int rtl_isPtr(rtl_Word w) {
  switch (rtl_typeOf(w)) {
  case RTL_TUPLE:
  case RTL_MAP:
  case RTL_NATIVE:
  case RTL_CONS:
  case RTL_CLOSURE:
    return 1;

  default:
    return 0;
  }
}

static inline
bool rtl_isZeroValue(rtl_Word w) {
  return (w >> 4) == 0;
}

static inline
bool rtl_isClosure(rtl_Word w) {
  return rtl_typeOf(w) == w;
}


// Throw an exception which just consists of a message.
void rtl_throwMsg(rtl_Machine *M, char const *type, char const *message);

// Throw an exception for an incorrectly typed argument.
void rtl_throwWrongType(rtl_Machine *M, rtl_WordType type, rtl_Word obj);

static inline
void rtl_xAssertType(rtl_Machine *M, rtl_WordType type, rtl_Word w)
{
  if (RTL_UNLIKELY(rtl_typeOf(w) != type)) {
    rtl_throwWrongType(M, type, w);
  }
}

#include "rtl/nil.h"
#include "rtl/symbol.h"
#include "rtl/selector.h"
#include "rtl/int28.h"
#include "rtl/fix14.h"
#include "rtl/tuple.h"
#include "rtl/char.h"
#include "rtl/map.h"
#include "rtl/cons.h"
#include "rtl/native.h"
#include "rtl/function.h"
#include "rtl/top.h"

#include "rtl/rto.h"
#include "rtl/instructions.h"
#include "rtl/intrinsic.h"
#include "rtl/debug.h"
#include "rtl/compiler.h"

#undef _RTL_INSIDE_RT_LISP_H_

// Returns word referring to the function.
rtl_Word rtl_registerBuiltin(rtl_Compiler  *C,
                             rtl_Word      name,
                             rtl_BuiltinFn cFn);

rtl_Word rtl_xCall(rtl_Machine *M, rtl_Word addr);

void rtl_xRun(rtl_Machine *M);

rtl_Word __rtl_xCallWithArgs(rtl_Machine *M,
                             rtl_Word    callable,
                             rtl_Word    *args,
                             size_t      argsLen);

#define rtl_xCallWithArgs(M, CALLABLE, ARGS...) ({                             \
      rtl_Word ___callArgs___[]  = { ARGS };                                   \
      size_t   ___callArgsLen___ = (sizeof ___callArgs___) / sizeof(rtl_Word); \
                                                                               \
      __rtl_xCallWithArgs(M, CALLABLE, ___callArgs___, ___callArgsLen___);     \
    })                                                                         \
  // End of multi-line macro


rtl_Word rtl_xApplyList(rtl_Machine *M, rtl_Word addr, rtl_Word argList);

rtl_Word rtl_xListToTuple(rtl_Machine *M, rtl_Word list);

static inline
bool rtl_clearException(rtl_Machine *M)
{
  bool wasExn = M->exception != NULL;

  if (wasExn) {
    M->vStackLen = 0;
    M->dStackLen = 0;
    M->rStackLen = 0;

    M->env = RTL_TUPLE;

    free(M->exception);
    M->exception = NULL;
  }

  return wasExn;
}

rtl_Word rtl_xRead(rtl_Compiler *C, FILE *f);

void rtl_setVar(rtl_Machine *M, rtl_Word key, rtl_Word value);

rtl_Word rtl_getVar(rtl_Machine *M, rtl_Word key);

void rtl_io_installBuiltins(rtl_Compiler *C);

void rtl_xRepl(rtl_Compiler *C);

void rtl_xLoad(rtl_Compiler *C, rtl_NameSpace const *ns, char const *path);

bool rtl_isString(rtl_Machine *M, rtl_Word w);

void rtl_xReifyString(rtl_Machine *M, rtl_Word str, char *buf, size_t cap);

char const *rtl_xReifyStringAlloc(rtl_Machine *M, rtl_Word str);

uint32_t rtl_xStringSize(rtl_Machine *M, rtl_Word str);

rtl_Word rtl_string(rtl_Machine *M, char const *cstr);

int rtl_cmp(rtl_Machine *M, rtl_Word a, rtl_Word b);

// Overwrite the car of a cons cell on the heap.
void rtl_xWriteCar(rtl_Machine *M, rtl_Word cons, rtl_Word val);
void rtl_xWriteCdr(rtl_Machine *M, rtl_Word cons, rtl_Word val);
void rtl_xWriteTupleElem(rtl_Machine *M,
                         rtl_Word    tuple,
                         uint32_t    idx,
                         rtl_Word    val);

#endif // rt-lisp.h
