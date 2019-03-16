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

// The top 4 bytes of each opcode express that opcode's encoding. Each constant
// has the form
//
//      RTL_OP_ENC_{stack-fmt}_{instruction-fmt}
//
// where {stack-fmt} may be one of:
//
//    NULLARY - this instruction expects 0 arguments on the stack.
//
//    UNARY   - this instruction expects 1 argument  on the stack.
//
//    BINARY  - this instruction expects 2 arguments on the stack.
//
//    TERNARY - this instruction expects 3 arguments on the stack.
//
//    DYNAMIC - the number of stack arguments to this function is given by
//              the instruction's 2-byte field, from the SHORT or STATIC
//              instruction-fmt (see below).
//
//
// and {instruction-fmt} may be one of:
//
//    NOARG
//    NOARG_A
//    NOARG_B - this instruction is encoded as only an opcode.
//
//    BYTE    - this instruction is encoded as an opcode followed by a 1-byte
//              immediate value.
//
//    SHORT   - this instruction is encoded as an opcode followed by a 2-byte
//              immediate value.
//
//    VAR     - this instruction is encoded as an opcode followed by two 2-byte
//              immediate values.
//
//    WORD    - this instruction is encoded as an opcode followed by a 4-byte
//              rtl_Word value.
//
//    STATIC  - this instruction is encoded as an opcode followed by a 4-byte
//              rtl_Word, and then a 2-byte immediate value.
typedef enum rtl_OpEncoding {
  RTL_OP_ENC_NULLARY_NOARG  = 0,
  RTL_OP_ENC_UNARY_NOARG    = 1,
  RTL_OP_ENC_BINARY_NOARG_A = 2,
  RTL_OP_ENC_BINARY_NOARG_B = 3,
  RTL_OP_ENC_TERNARY_NOARG  = 4,

  RTL_OP_ENC_NULLARY_BYTE   = 5,
  RTL_OP_ENC_UNARY_BYTE     = 6,

  RTL_OP_ENC_NULLARY_SHORT  = 7,
  RTL_OP_ENC_UNARY_SHORT    = 8,
  RTL_OP_ENC_DYNAMIC_SHORT  = 9,
  RTL_OP_ENC_FUNCTION_SHORT = 10,

  RTL_OP_ENC_NULLARY_VAR    = 11,
  RTL_OP_ENC_UNARY_VAR      = 12,

  RTL_OP_ENC_NULLARY_WORD   = 13,
  RTL_OP_ENC_UNARY_WORD     = 14,

  RTL_OP_ENC_STATIC         = 15,
} rtl_OpEncoding;

#define RTL_MAP_OPCODES(M)                             \
    M(NOP,         "nop",         NULLARY_NOARG,    0) \
    M(MAP,         "map",         NULLARY_NOARG,    1) \
    M(NIL,         "nil",         NULLARY_NOARG,    2) \
    M(TOP,         "top",         NULLARY_NOARG,    3) \
    M(GENSYM,      "gensym",      NULLARY_NOARG,    4) \
    M(RET,         "ret",         NULLARY_NOARG,    5) \
    M(END_LABELS,  "end-labels",  NULLARY_NOARG,    6) \
                                                       \
    M(CAR,         "car",         UNARY_NOARG,      0) \
    M(CDR,         "cdr",         UNARY_NOARG,      1) \
    M(POP,         "pop",         UNARY_NOARG,      2) \
    M(DUP,         "dup",         UNARY_NOARG,      3) \
    M(INT28P,      "int28?",      UNARY_NOARG,      4) \
    M(FIX14P,      "fix14?",      UNARY_NOARG,      5) \
    M(SYMBOLP,     "symbol?",     UNARY_NOARG,      6) \
    M(SELECTORP,   "selector?",   UNARY_NOARG,      7) \
    M(MAPP,        "map?",        UNARY_NOARG,      8) \
    M(CHARP,       "char?",       UNARY_NOARG,      9) \
    M(NILP,        "nil?",        UNARY_NOARG,     10) \
    M(TOPP,        "top?",        UNARY_NOARG,     11) \
    M(CONSP,       "cons?",       UNARY_NOARG,     12) \
    M(TUPLEP,      "tuple?",      UNARY_NOARG,     13) \
    M(NOT,         "not",         UNARY_NOARG,     14) \
    M(LEN,         "len",         UNARY_NOARG,     15) \
                                                       \
    M(IADD,        "iadd",        BINARY_NOARG_A,   0) \
    M(ISUB,        "isub",        BINARY_NOARG_A,   1) \
    M(IMUL,        "imul",        BINARY_NOARG_A,   2) \
    M(IDIV,        "idiv",        BINARY_NOARG_A,   3) \
    M(IMOD,        "imod",        BINARY_NOARG_A,   4) \
    M(LT,          "lt",          BINARY_NOARG_A,   5) \
    M(LEQ,         "leq",         BINARY_NOARG_A,   6) \
    M(GT,          "gt",          BINARY_NOARG_A,   7) \
    M(GEQ,         "geq",         BINARY_NOARG_A,   8) \
    M(EQ,          "eq",          BINARY_NOARG_A,   9) \
    M(NEQ,         "neq",         BINARY_NOARG_A,  10) \
    M(ISO,         "iso",         BINARY_NOARG_A,  11) \
    M(FADD,        "fadd",        BINARY_NOARG_A,  12) \
    M(FSUB,        "fsub",        BINARY_NOARG_A,  13) \
    M(FMUL,        "fmul",        BINARY_NOARG_A,  14) \
    M(FDIV,        "fdiv",        BINARY_NOARG_B,  15) \
                                                       \
    M(SET_CAR,     "set-car",     BINARY_NOARG_B,   0) \
    M(SET_CDR,     "set-cdr",     BINARY_NOARG_B,   1) \
    M(CONS,        "cons",        BINARY_NOARG_B,   2) \
    M(SWAP,        "swap",        BINARY_NOARG_B,   3) \
    M(PUSH_FIRST,  "push-first",  BINARY_NOARG_B,   4) \
    M(PUSH_LAST,   "push-last",   BINARY_NOARG_B,   5) \
    M(CONCAT,      "concat",      BINARY_NOARG_B,   6) \
    M(APPLY_LIST,  "apply-list",  BINARY_NOARG_B,   7) \
    M(APPLY_TUPLE, "apply-tuple", BINARY_NOARG_B,   8) \
    M(GET,         "get",         BINARY_NOARG_B,   9) \
                                                       \
    M(LOOKUP,      "lookup",      TERNARY_NOARG,    0) \
    M(INSERT,      "insert",      TERNARY_NOARG,    1) \
    M(SET_ELEM,    "set-elem",    TERNARY_NOARG,    2) \
    M(SLICE,       "slice",       TERNARY_NOARG,    3) \
                                                       \
                                                       \
    M(JMP8,        "jmp8",        NULLARY_BYTE,     0) \
    M(SYMBOL8,     "symbol8",     NULLARY_BYTE,     2) \
    M(SELECTOR8,   "selector8",   NULLARY_BYTE,     3) \
    M(INT8,        "int8",        NULLARY_BYTE,     4) \
    M(FIX8,        "fix8",        NULLARY_BYTE,     5) \
    M(CHAR8,       "char8",       NULLARY_BYTE,     6) \
    M(FN8,         "fn8",         NULLARY_BYTE,     7) \
    M(UNRES8,      "unres8",      NULLARY_BYTE,     8) \
    M(CLOSURE8,    "closure8",    NULLARY_BYTE,     9) \
                                                       \
    M(CJMP8,       "cjmp8",       UNARY_BYTE,       0) \
                                                       \
                                                       \
    M(JMP16,       "jmp16",       NULLARY_SHORT,    0) \
    M(SYMBOL16,    "symbol16",    NULLARY_SHORT,    1) \
    M(SELECTOR16,  "selector16",  NULLARY_SHORT,    2) \
    M(INT16,       "int16",       NULLARY_SHORT,    3) \
    M(FIX16,       "fix16",       NULLARY_SHORT,    4) \
    M(CHAR16,      "char16",      NULLARY_SHORT,    5) \
    M(FN16,        "fn16",        NULLARY_SHORT,    6) \
    M(UNRES16,     "unres16",     NULLARY_SHORT,    7) \
    M(CLOSURE16,   "closure16",   NULLARY_SHORT,    8) \
    M(REST,        "rest",        NULLARY_SHORT,    9) \
                                                       \
    M(CJMP16,      "cjmp16",      UNARY_SHORT,      0) \
                                                       \
    M(CALL,        "call",        FUNCTION_SHORT,   0) \
    M(TAIL,        "tail",        FUNCTION_SHORT,   1) \
                                                       \
    M(LABELS,      "labels",      DYNAMIC_SHORT,    0) \
    M(TUPLE,       "tuple",       DYNAMIC_SHORT,    1) \
                                                       \
                                                       \
    M(VAR,         "var",         NULLARY_VAR,      0) \
                                                       \
    M(SET_VAR,     "set-var",     UNARY_VAR,        0) \
                                                       \
                                                       \
    M(JMP32,       "jmp32",       NULLARY_WORD,     0) \
    M(CJMP32,      "cjmp32",      NULLARY_WORD,     1) \
    M(CONST32,     "const32",     NULLARY_WORD,     3) \
    M(CLOSURE32,   "closure32",   NULLARY_WORD,     4) \
    M(GET_DYN,     "get-dyn",     NULLARY_WORD,     5) \
    M(RESTORE_DYN, "restore-dyn", NULLARY_WORD,     6) \
    M(UNDEF_VAR,   "undef-var",   NULLARY_WORD,     7) \
                                                       \
    M(SAVE_DYN,    "save-dyn",    UNARY_WORD,       0) \
    M(SET_DYN,     "set-dyn",     UNARY_WORD,       1) \
                                                       \
                                                       \
    M(UNDEF_CALL,  "undef-call",  STATIC,           0) \
    M(UNDEF_TAIL,  "undef-tail",  STATIC,           1) \
    M(STATIC_CALL, "static-call", STATIC,           2) \
    M(STATIC_TAIL, "static-tail", STATIC,           3) \
  // End of multi-line macro


#define RTL_GEN_OPCODE_ENUM(ENUM, STRING, ENCODING, INDEX) \
    RTL_OP_##ENUM = ((RTL_OP_ENC_##ENCODING << 4) | INDEX), \
  // End of multi-line macro

typedef enum rtl_Opcode {
  RTL_MAP_OPCODES(RTL_GEN_OPCODE_ENUM)
} rtl_Opcode;

#undef RTL_GEN_OPCODE_ENUM