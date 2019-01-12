#ifndef _RTL_INSIDE_RT_LISP_H_
# error "rtl/instructions.h should only be included indirectly via rt-lisp.h"
#endif

typedef enum rtl_Opcode {
  // No-op
  RTL_OP_NOP,

  // Constants
  RTL_OP_CONST,
  RTL_OP_CONST_NIL,
  RTL_OP_CONST_TOP,

  // Basic list operations
  RTL_OP_CONS,
  RTL_OP_CAR,
  RTL_OP_CDR,

  // Stack Operationsx
  RTL_OP_POP,
  RTL_OP_SWAP,
  RTL_OP_DUP,

  // Type Predicates
  RTL_OP_IS_INT28,
  RTL_OP_IS_FIX14,
  RTL_OP_IS_SYMBOL,
  RTL_OP_IS_SELECTOR,
  RTL_OP_IS_NIL,
  RTL_OP_IS_CONS,
  RTL_OP_IS_TUPLE,
  RTL_OP_IS_TOP,

  // Logical operations
  RTL_OP_NOT,

  // Control flow operations
  RTL_OP_JMP,
  RTL_OP_CJMP,
  RTL_OP_CALL,
  RTL_OP_RETURN,

  // Int28 Arithmetic
  RTL_OP_IADD,
  RTL_OP_ISUB,
  RTL_OP_IMUL,
  RTL_OP_IDIV,
  RTL_OP_IMOD,

  // Fix14 Arithmetic
  RTL_OP_FADD,
  RTL_OP_FSUB,
  RTL_OP_FMUL,
  RTL_OP_FDIV,
} rtl_Opcode;
