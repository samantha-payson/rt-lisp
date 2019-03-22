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

#include <stdio.h>

static
void formatChar(utf8_int32_t ch)
{
  uint8_t utf8[5];
  switch (ch) {
  case '\\':
    printf("\\\\");
    break;

  case '\n':
    printf("\\n");
    break;

  case '\r':
    printf("\\r");
    break;

  case '\t':
    printf("\\t");
    break;

  case '\'':
    printf("\\'");
    break;

  case '\"':
    printf("\\\"");
    break;

  default:
    *(uint8_t *)utf8catcodepoint(utf8, ch, 4) = '\0';

    printf("%s", (char *)utf8);
    break;
  }
}

// Format an expression, but don't follow any pointers. Basically just print
// atoms, and addresses for non-atoms.
void rtl_formatExprShallow(rtl_Word w)
{
  char const *pkgName;

  switch (rtl_typeOf(w)) {
  case RTL_NIL:
    printf("nil");
    break;

  case RTL_SYMBOL:
    if (rtl_isGensym(w)) {
      printf("#:G%04X", (unsigned int)rtl_symbolID(w) & 0x7FFFFFF);
    } else {
      printf("%s:%s", rtl_symbolPackageName(w), rtl_symbolName(w));
    }
    break;

  case RTL_UNRESOLVED_SYMBOL:
    printf("unres:%s", rtl_unresolvedSymbolName(w));
    break;

  case RTL_SELECTOR:
    pkgName = rtl_selectorPackageName(w);
    if (pkgName[0] != '\0') {
      printf(".%s:%s", pkgName, rtl_selectorName(w));
    } else {
      printf(".%s", rtl_selectorName(w));
    } break;

  case RTL_INT28:
    printf("%d", (int)rtl_int28Value(w));
    break;

  case RTL_FIX14:
    printf("%f", rtl_fix14Value(w));
    break;

  case RTL_TUPLE:
    if (rtl_isZeroValue(w)) {
      printf("[]");
    } else {
      printf("Tuple#%Xg%d", __rtl_ptrOffs(w), __rtl_ptrGen(w));
    } break;

  case RTL_CHAR:
    printf("'");
    formatChar(rtl_charValue(w));
    printf("'");
    break;

  case RTL_MAP:
    if (rtl_isZeroValue(w)) {
      printf("{}");
    } else {
      printf("Map#%Xg%d", __rtl_ptrOffs(w), __rtl_ptrGen(w));
    } break;

  case RTL_CONS:
    printf("Cons#%Xg%d", __rtl_ptrOffs(w), __rtl_ptrGen(w));
    break;

  case RTL_FUNCTION:
    printf("Fn#%u", (unsigned int)(w >> 4));
    break;

  case RTL_CLOSURE:
    printf("<closure>");
    break;

  case RTL_TOP:
    printf("T");
    break;

  case RTL_HEADER:
    printf("Header#%X", rtl_headerValue(w));
    break;

  default:
    printf("<Unhandled type '%s'>", rtl_typeNameOf(w));
    break;
  }
}

void __rtl_formatMap(rtl_Machine *M, rtl_Word map, int indent)
{
  rtl_Word const *rptr, *entry;
  uint32_t mask;
  size_t   len,
           i;

  if (rtl_isZeroValue(map)) return;

  rptr = __rtl_reifyPtr(M, map);
  mask = rtl_headerValue(rptr[0]);

  len = __builtin_popcount(mask);

  for (i = 0; i < len; i++) {
    entry = rptr + 1 + 2*i;

    if (rtl_isHeader(entry[0])) {
      __rtl_formatMap(M, entry[1], indent);
    } else {
      rtl_formatExprIndented(M, entry[0], indent + 1);
      printf(" ");
      rtl_formatExprIndented(M, entry[1], indent + 1);
      printf(", ");
    }
  }
}

void __rtl_debugFormatMap(rtl_Machine *M, rtl_Word map, int indent)
{
  rtl_Word const *rptr, *entry;
  uint32_t mask;
  size_t   len,
           i;

  if (rtl_isZeroValue(map)) return;

  rptr = __rtl_reifyPtr(M, map);
  mask = rtl_headerValue(rptr[0]);

  len = __builtin_popcount(mask);

  printf("%04Xg%d#%d{ ", __rtl_ptrOffs(map), (int)__rtl_ptrGen(map), (int)len);

  for (i = 0; i < len; i++) {
    entry = rptr + 1 + 2*i;

    if (rtl_isHeader(entry[0])) {
      __rtl_debugFormatMap(M, entry[1], indent);
    } else {
      rtl_formatExprIndented(M, entry[0], indent + 1);
      printf(" ");
      rtl_formatExprIndented(M, entry[1], indent + 1);
      printf(", ");
    }
  }
  printf("} ");
}


void rtl_formatExprIndented(rtl_Machine *M, rtl_Word w, int indent)
{
  rtl_Word const *ptr;

  size_t         len,
                 i;

  switch (rtl_typeOf(w)) {
  case RTL_TUPLE:
    ptr = rtl_reifyTuple(M, w, &len);
    if (rtl_isString(M, w)) {
      printf("\"");
      for (i = 0; i < len; i++) {
        formatChar(rtl_charValue(ptr[i]));
      }
      printf("\"");
    } else {
      printf("[ ");
      for (i = 0; i < len; i++) {
        rtl_formatExprIndented(M, ptr[i], indent + 1);
        printf(" ");
      }
      printf("]");
    }
    break;

  case RTL_CONS:
    printf("(");
    ptr = rtl_reifyCons(M, w);
    rtl_formatExprIndented(M, ptr[0], indent + 1);

    while (rtl_isCons(ptr[1])) {
      ptr = rtl_reifyCons(M, ptr[1]);
      printf(" ");
      rtl_formatExprIndented(M, ptr[0], indent + 1);
    }
    if (!rtl_isNil(ptr[1])) {
      printf(" . ");
      rtl_formatExprIndented(M, ptr[1], indent + 1);
    }
    printf(")");
    break;

  case RTL_MAP:
    if (rtl_isEmptyMap(w)) {
      printf("{}");
    } else {
      printf("{ ");
      __rtl_formatMap(M, w, indent);
      printf("}");
    }
    break;

  default:
    rtl_formatExprShallow(w);
    break;
  }
}

void rtl_formatExpr(rtl_Machine *M, rtl_Word w)
{
  rtl_formatExprIndented(M, w, 0);
}

#define RTL_EMIT_INSTRUCTION_NAME(ENUM, NAME, _ENC, _IDX) \
  [RTL_OP_ ## ENUM] = NAME,

static const char *instructionNames[] = {
  RTL_MAP_OPCODES(RTL_EMIT_INSTRUCTION_NAME)
};

#undef RTL_EMIT_INSTRUCTION_NAME

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

uint8_t *rtl_disasm(rtl_CodeBase *codeBase, uint8_t *pc)
{
  rtl_Word        immW;

  uint16_t        frame,
                  idx,
                  imm16;

  uint8_t         imm8;

  rtl_OpEncoding  enc;

  rtl_Opcode      opcode;

  rtl_Function    *func;

  char nameBuf[1024];

  opcode = *pc++;
  enc = (rtl_OpEncoding)(opcode >> 4);

  printf("   %-14s", instructionNames[opcode]);

  switch (enc) {
  case RTL_OP_ENC_NULLARY_NOARG:
  case RTL_OP_ENC_UNARY_NOARG:
  case RTL_OP_ENC_BINARY_NOARG_A:
  case RTL_OP_ENC_BINARY_NOARG_B:
  case RTL_OP_ENC_TERNARY_NOARG:
    printf("\n");
    break;

  case RTL_OP_ENC_NULLARY_BYTE:
  case RTL_OP_ENC_UNARY_BYTE:
    imm8 = *pc++;

    switch (opcode) {
    case RTL_OP_JMP8:
    case RTL_OP_CJMP8:
      printf("  %+d\n", (int)(int8_t)imm8);
      break;

    case RTL_OP_SYMBOL8:
      printf("  ");
      rtl_formatExprShallow(rtl_symbol(imm8));
      printf("\n");
      break;

    case RTL_OP_SELECTOR8:
      printf("  ");
      rtl_formatExprShallow(rtl_selector(imm8));
      printf("\n");
      break;

    case RTL_OP_INT8:
      printf("  ");
      rtl_formatExprShallow(rtl_int28(imm8));
      printf("\n");
      break;

    case RTL_OP_FIX8:
      printf("  ");
      rtl_formatExprShallow(rtl_fix14(imm8));
      printf("\n");
      break;

    case RTL_OP_CHAR8:
      printf("  ");
      rtl_formatExprShallow(rtl_char(imm8));
      printf("\n");
      break;

    case RTL_OP_UNRES8:
      printf("  ");
      rtl_formatExprShallow(rtl_unresolvedSymbol(imm8));
      printf("\n");
      break;

    case RTL_OP_CLOSURE8:
    case RTL_OP_FN8:
      func = rtl_reifyFunction(codeBase, rtl_function(imm8));
      snprintf(nameBuf, 1024, "%s:%s",
               rtl_symbolPackageName(func->name),
               rtl_symbolName(func->name));

      printf("  %-32s  ; Function Nbr %d\n", nameBuf, (int)imm8);
      break;

    default:
      abort(); // unreachable ... ?

    } break;

  case RTL_OP_ENC_NULLARY_SHORT:
  case RTL_OP_ENC_UNARY_SHORT:
  case RTL_OP_ENC_DYNAMIC_SHORT:
  case RTL_OP_ENC_FUNCTION_SHORT:
    pc = readShort(pc, &imm16);

    switch (opcode) {
    case RTL_OP_JMP16:
    case RTL_OP_CJMP16:
      printf("  %+d\n", (int)(int16_t)imm16);
      break;

    case RTL_OP_SYMBOL16:
      printf("  ");
      rtl_formatExprShallow(rtl_symbol(imm16));
      printf("\n");
      break;

    case RTL_OP_SELECTOR16:
      printf("  ");
      rtl_formatExprShallow(rtl_selector(imm16));
      printf("\n");
      break;

    case RTL_OP_INT16:
      printf("  ");
      rtl_formatExprShallow(rtl_int28(imm16));
      printf("\n");
      break;

    case RTL_OP_FIX16:
      printf("  ");
      rtl_formatExprShallow(rtl_fix14(imm16));
      printf("\n");
      break;

    case RTL_OP_CHAR16:
      printf("  ");
      rtl_formatExprShallow(rtl_char(imm16));
      printf("\n");
      break;

    case RTL_OP_UNRES16:
      printf("  ");
      rtl_formatExprShallow(rtl_unresolvedSymbol(imm16));
      printf("\n");
      break;

    case RTL_OP_CLOSURE16:
    case RTL_OP_FN16:
      func = rtl_reifyFunction(codeBase, rtl_function(imm16));
      snprintf(nameBuf, 1024, "%s:%s",
               rtl_symbolPackageName(func->name),
               rtl_symbolName(func->name));

      printf("  %-32s  ; Function Nbr %d\n", nameBuf, (int)imm16);
      break;

    default:
      printf("  %d\n", (int)imm16);
      break;

    } break;

  case RTL_OP_ENC_NULLARY_VAR:
  case RTL_OP_ENC_UNARY_VAR:
    pc = readShort(pc, &frame);
    pc = readShort(pc, &idx);

    printf("  %-3d %-3d\n", (int)frame, (int)idx);
    break;

  case RTL_OP_ENC_NULLARY_WORD:
  case RTL_OP_ENC_UNARY_WORD:
    pc = readWord(pc, &immW);

    switch (opcode) {
    case RTL_OP_JMP32:
    case RTL_OP_CJMP32:
      printf("  %+d\n", (int)(int32_t)immW);
      break;


    case RTL_OP_CONST32:
      printf("  ");
      rtl_formatExprShallow(immW);
      printf("\n");
      break;

    case RTL_OP_CLOSURE32:
      func = rtl_reifyFunction(codeBase, rtl_function(immW));
      snprintf(nameBuf, 1024, "%s:%s",
               rtl_symbolPackageName(func->name),
               rtl_symbolName(func->name));

      printf("  %-32s  ; Function Nbr %d\n", nameBuf, (int)immW);
      break;

    case RTL_OP_GET_DYN:
    case RTL_OP_RESTORE_DYN:
    case RTL_OP_UNDEF_VAR:
    case RTL_OP_SAVE_DYN:
    case RTL_OP_SET_DYN:
      printf("  %s:%s\n",
             rtl_symbolPackageName(immW),
             rtl_symbolName(immW));
      break;

    default:
      abort(); // unreachable ... ?

    } break;

  case RTL_OP_ENC_STATIC:
    pc = readWord(pc, &immW);
    pc = readShort(pc, &imm16);

    switch (opcode) {
    case RTL_OP_STATIC_CALL:
    case RTL_OP_STATIC_TAIL:
      func = rtl_reifyFunction(codeBase, immW);
      snprintf(nameBuf, 1024, "%s:%s",
               rtl_symbolPackageName(func->name),
               rtl_symbolName(func->name));

      printf("  %-24s %-4d  ; Function Nbr %d\n", nameBuf,
             (int)imm16,
             (int)rtl_functionID(immW));
      break;

    case RTL_OP_UNDEF_CALL:
    case RTL_OP_UNDEF_TAIL:
      printf("  %s:%s\n",
             rtl_symbolPackageName(immW),
             rtl_symbolName(immW));
      break;

    default:
      abort(); // unreachable ... ?
    } 
    break;
  }

  return pc;
}

void rtl_disasmFn(rtl_Machine *M, rtl_Word fn)
{
  uint8_t  *start,
           *code,
           *end;

  rtl_Word const *rptr;

  rtl_FnDef *fnDef;
  rtl_Function *func;

  switch (rtl_typeOf(fn)) {
  case RTL_SYMBOL:
    fnDef = rtl_lookupFn(M->codeBase, fn);
    if (!fnDef) {
      printf("  no such function: %s:%s\n",
             rtl_symbolPackageName(fn),
             rtl_symbolName(fn));
    } else {
      rtl_disasmFn(M, fnDef->fn);
    } return;

  case RTL_CLOSURE:
    rptr = __rtl_reifyPtr(M, fn);
    rtl_disasmFn(M, rptr[0]);
    return;

  case RTL_FUNCTION:
    break;

  default:
    printf("  can't disassemble type: %s\n", rtl_typeNameOf(fn));
    return;
  }

  func = rtl_reifyFunction(M->codeBase, fn);

  if (func->isBuiltin) {
    printf("\n ---- Builtin Function %s:%s ----\n---------\n\n",
           rtl_symbolPackageName(func->name),
           rtl_symbolName(func->name));
    return;
  }

  code  = func->as.lisp.code;
  end   = code + func->as.lisp.len;
  start = code;

  printf("\n ---- Disassembly of Function %s:%s ----\n\n",
         rtl_symbolPackageName(func->name),
         rtl_symbolName(func->name));

  while (code < end) {
    printf("    @%04u",
           (unsigned int)((uintptr_t)(code - start) & 0xFFFF));

    code = rtl_disasm(M->codeBase, code);
  }

  printf("\n ----------------------------------\n\n");
}

void rtl_disasmMacro(rtl_Machine *M, rtl_Word name)
{
  rtl_FnDef *fnDef;

  if (!rtl_isSymbol(name)) {
    printf("  error: Can't disassemble %s as macro\n",
           rtl_typeNameOf(name));
    return;
  }

  fnDef = rtl_lookupFn(M->codeBase, name);
  if (!fnDef) {
    printf("  no such macro: %s:%s\n",
           rtl_symbolPackageName(name),
           rtl_symbolName(name));
  } else {
    rtl_disasmFn(M, fnDef->macro);
  } return;
}

bool rtl_debugCheckForCycles(rtl_Machine *M)
{
  size_t i, j;
  rtl_Generation *gen;
  rtl_Word w;
  uint32_t offs;
  int      genNbr;

  for (i = 0; i < RTL_MAX_GENERATIONS; i++) {
    gen = M->heap.gen[i];
    if (!gen) break;

    for (j = 0; j < gen->fillPtr; j++) {
      w = gen->words[j];

      if (rtl_isCons(w)) {
        offs   = __rtl_ptrOffs(w);
        genNbr = __rtl_ptrGen(w);

        if (genNbr < gen->nbr ||
            (genNbr == gen->nbr && offs >= j))
        {
          printf("\n    CYCLE: %04Xg%d: ", (unsigned int)j, (int)i);
          rtl_formatExprShallow(w);
          printf("\n\n");
          return true;
        }
      }
    }
  }

  return false;
}

void __rtl_debugCheckAlloc(rtl_Machine *M, rtl_Word w) {
  rtl_Word const *rptr;
  size_t i, len;

  uint32_t offs, gen;
  uint32_t elemOffs, elemGen;

  offs = __rtl_ptrOffs(w);
  gen  = __rtl_ptrGen(w);

  switch (rtl_typeOf(w)) {
  case RTL_TUPLE:
    rptr = rtl_reifyTuple(M, w, &len);

    for (i = 0; i < len; i++) {
      elemOffs = __rtl_ptrOffs(rptr[i]);
      elemGen  = __rtl_ptrGen(rptr[i]);

      if (elemGen < gen || (elemGen == gen && elemOffs >= offs)) {
        printf("   @ Index %d!\n", (int)i);
        asm("int3");
      }
    } break;

  case RTL_CONS:
    rptr = __rtl_reifyPtr(M, w);
    for (i = 0; i < 2; i++) {
      elemOffs = __rtl_ptrOffs(rptr[i]);
      elemGen  = __rtl_ptrGen(rptr[i]);

      if (elemGen < gen || (elemGen == gen && elemOffs >= offs)) {
        printf("   @ Index %d!\n", (int)i);
        asm("int3");
      }
    } break;

  default:
    break;
  }
}

void rtl_dumpHeap(rtl_Machine *M)
{
  size_t i, j;
  rtl_Generation *gen;

  for (i = 0; i < RTL_MAX_GENERATIONS; i++) {
    gen = M->heap.gen[i];
    if (!gen) break;

    printf("\n"
           "  -------- Gen %2d --------\n", gen->nbr);

    for (j = 0; j < gen->fillPtr; j++) {
      printf("   %04Xg%d: ", (unsigned int)j, gen->nbr);
      rtl_formatExprShallow(gen->words[j]);
      printf("\n");
    }

    printf("  ------------------------\n");
  }
}

static
uint8_t *nextInstruction(uint8_t *pc)
{
  switch (*pc >> 4) {
  case RTL_OP_ENC_NULLARY_NOARG:
  case RTL_OP_ENC_UNARY_NOARG:
  case RTL_OP_ENC_BINARY_NOARG_A:
  case RTL_OP_ENC_BINARY_NOARG_B:
  case RTL_OP_ENC_TERNARY_NOARG:
    return pc + 1;

  case RTL_OP_ENC_NULLARY_BYTE:
  case RTL_OP_ENC_UNARY_BYTE:
    return pc + 2;
    break;

  case RTL_OP_ENC_NULLARY_SHORT:
  case RTL_OP_ENC_UNARY_SHORT:
  case RTL_OP_ENC_DYNAMIC_SHORT:
  case RTL_OP_ENC_FUNCTION_SHORT:
    return pc + 3;

  case RTL_OP_ENC_NULLARY_VAR:
  case RTL_OP_ENC_UNARY_VAR:
  case RTL_OP_ENC_NULLARY_WORD:
  case RTL_OP_ENC_UNARY_WORD:
    return pc + 5;

  case RTL_OP_ENC_STATIC:
    return pc + 7;

  default:
    abort(); // unreachable ... ?
  }
}

static
uint8_t *prevInstruction(uint8_t *start, uint8_t *after)
{
  uint8_t *prev, *next;

  prev = start;

  if (prev >= after) {
    return prev;
  }

  for (next = nextInstruction(prev); next < after; next = nextInstruction(prev))
  {
    prev = next;
  }

  return prev;
}

static
void __rtl_printStackTrace(rtl_Machine *M, rtl_RetAddr *stack, size_t stackLen)
{
  size_t         i;
  rtl_Function  *func;
  uint32_t      fnID;
  uint8_t       *pc;
  uint8_t       *prev;

  char fnName[1024];

  printf("Stack Trace (most recent at bottom):\n\n");
  for (i = 0; i < stackLen; i++) {
    func = rtl_reifyFunction(M->codeBase, stack[i].fn);
    fnID = rtl_functionID(stack[i].fn);

    snprintf(fnName, 1024, "%s:%s",
             rtl_symbolPackageName(func->name),
             rtl_symbolName(func->name));


    printf("    %4zu: %-32s #%04u\n",
           stackLen - 1 - i,
           fnName,
           (unsigned int)fnID);

    if (i + 1 < M->rStackLen) {
      pc = stack[i + 1].pc;
    } else {
      pc = M->pc;
    }

    if (!func->isBuiltin) {
      prev = prevInstruction(func->as.lisp.code, pc);

      printf("            @%04X", (unsigned int)(prev - func->as.lisp.code));
      rtl_disasm(M->codeBase, prev);

    } else {
      printf("            .. builtin ..\n");
    }

    printf("\n");
  }
}

void rtl_printStackTrace(rtl_Machine *M)
{
  __rtl_printStackTrace(M, M->rStack, M->rStackLen);
}

void rtl_printException(rtl_Machine *M, rtl_Exception *exn)
{
  rtl_Word const dotType = rtl_internSelector(NULL, "type");
  rtl_Word const dotMsg  = rtl_internSelector(NULL, "message");

  rtl_Word type, msg;

  char *msgBuf;
  uint32_t msgLen;

  type = rtl_mapLookup(M, exn->data, dotType, RTL_NIL);
  msg  = rtl_mapLookup(M, exn->data, dotMsg,  RTL_NIL);

  msgLen = rtl_stringSize(M, msg);
  msgBuf = malloc(msgLen + 1);

  rtl_reifyString(M, msg, msgBuf, msgLen + 1);

  printf("\nException ");
  rtl_formatExprShallow(type);
  printf(":\n\n    %s\n\n", msgBuf);

  __rtl_printStackTrace(M, exn->stack, exn->stackLen);

  free(msgBuf);
}