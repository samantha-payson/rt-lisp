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

  // Basic tuple operations
  RTL_OP_TUPLE,
  RTL_OP_GET,
  RTL_OP_LEN,
  RTL_OP_PUSH_FIRST,
  RTL_OP_PUSH_LAST,
  RTL_OP_CONCAT,
  RTL_OP_SLICE,

  // Basic map operations
  RTL_OP_MAP,
  RTL_OP_INSERT,
  RTL_OP_LOOKUP,

  // Dynamic variable operations
  RTL_OP_DYN_GET,
  RTL_OP_DYN_SET,
  RTL_OP_DYN_SAVE,
  RTL_OP_DYN_RESTORE,

  // Stack Operations
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
  RTL_OP_IS_MAP,
  RTL_OP_IS_CHAR,
  RTL_OP_IS_TUPLE,
  RTL_OP_IS_TOP,

  // Logical operations
  RTL_OP_NOT,

  // Symbol operations
  RTL_OP_GENSYM,

  // Control flow operations
  RTL_OP_JMP8,
  RTL_OP_JMP16,
  RTL_OP_JMP32,
  RTL_OP_CJMP8,
  RTL_OP_CJMP16,
  RTL_OP_CJMP32,
  RTL_OP_CALL,
  RTL_OP_TAIL,
  RTL_OP_STATIC_CALL,
  RTL_OP_STATIC_TAIL,
  RTL_OP_APPLY_LIST,
  RTL_OP_APPLY_TUPLE,
  RTL_OP_RETURN,
  RTL_OP_REST,

  // This opcode is placed wherever there is a call site (or load site) that
  // hasn't been resolved yet. It will print an error and exit the interpreter
  // if executed.
  RTL_OP_UNDEFINED_TAIL,
  RTL_OP_UNDEFINED_CALL,
  RTL_OP_UNDEFINED_VAR,

  // Load a variable onto the stack.
  RTL_OP_VAR,

  // Create a closure in the current environment
  RTL_OP_CLOSURE,
  RTL_OP_LABELS,
  RTL_OP_END_LABELS,

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

  // Comparisons
  RTL_OP_LT,
  RTL_OP_LEQ,
  RTL_OP_GT,
  RTL_OP_GEQ,
  RTL_OP_EQ,
  RTL_OP_NEQ,
  RTL_OP_ISO,

  // Mutation
  RTL_OP_SET_VAR,
  RTL_OP_SET_CAR,
  RTL_OP_SET_CDR,
  RTL_OP_SET_ELEM,

} rtl_Opcode;
