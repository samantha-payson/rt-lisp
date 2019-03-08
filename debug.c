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

  case RTL_UNRESOLVED_SELECTOR:
    printf("<unres-sel>");
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
    printf("Tuple#%Xg%d", __rtl_ptrOffs(w), __rtl_ptrGen(w));
    break;

  case RTL_CHAR:
    printf("'");
    formatChar(rtl_charValue(w));
    printf("'");
    break;

  case RTL_MAP:
    printf("Map#%Xg%d", __rtl_ptrOffs(w), __rtl_ptrGen(w));
    break;

  case RTL_CONS:
    printf("Cons#%Xg%d", __rtl_ptrOffs(w), __rtl_ptrGen(w));
    break;

  case RTL_FUNCTION:
    printf("Function#%u", (unsigned int)(w >> 4));
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

uint8_t *rtl_disasm(rtl_CodeBase *codeBase, uint8_t *bc)
{
  rtl_Word     literal;

  uint16_t     frame,
               idx,
               size;

  rtl_Function *func;

  switch (*bc) {
  case RTL_OP_NOP:
    printf("   nop\n");
    return bc + 1;

  case RTL_OP_CONST:
    literal = (rtl_Word)bc[1] << 0
            | (rtl_Word)bc[2] << 8
            | (rtl_Word)bc[3] << 16
            | (rtl_Word)bc[4] << 24 ;

    printf("   const       ");
    rtl_formatExprShallow(literal);

    if (rtl_isFunction(literal)) {
      func = rtl_reifyFunction(codeBase, literal);
      printf("         ;; %s:%s",
             rtl_symbolPackageName(func->name),
             rtl_symbolName(func->name));
    }
    printf("\n");

    return bc + 5;

  case RTL_OP_CONST_NIL:
    printf("   nil\n");
    return bc + 1;

  case RTL_OP_CONST_TOP:
    printf("   top\n");
    return bc + 1;

  case RTL_OP_GENSYM:
    printf("   gensym\n");
    return bc + 1;

  case RTL_OP_CONS:
    printf("   cons\n");
    return bc + 1;

  case RTL_OP_CAR:
    printf("   car\n");
    return bc + 1;

  case RTL_OP_CDR:
    printf("   cdr\n");
    return bc + 1;

  case RTL_OP_POP:
    printf("   pop\n");
    return bc + 1;

  case RTL_OP_SWAP:
    printf("   swap\n");
    return bc + 1;

  case RTL_OP_DUP:
    printf("   dup\n");
    return bc + 1;

  case RTL_OP_IS_INT28:
    printf("   int28?\n");
    return bc + 1;

  case RTL_OP_IS_FIX14:
    printf("   fix14?\n");
    return bc + 1;

  case RTL_OP_IS_SYMBOL:
    printf("   symbol?\n");
    return bc + 1;

  case RTL_OP_IS_SELECTOR:
    printf("   selector?\n");
    return bc + 1;

  case RTL_OP_IS_MAP:
    printf("   selector?\n");
    return bc + 1;

  case RTL_OP_IS_CHAR:
    printf("   char?\n");
    return bc + 1;

    // `nil?' and `not' are actually the same function.
  case RTL_OP_IS_NIL:
    printf("   nil?\n");
    return bc + 1;

    // `nil?' and `not' are actually the same function.
  case RTL_OP_IS_TOP:
    printf("   top?\n");
    return bc + 1;

  case RTL_OP_NOT:
    printf("   not\n");
    return bc + 1;

  case RTL_OP_IS_CONS:
    printf("   cons?\n");
    return bc + 1;

  case RTL_OP_IS_TUPLE:
    printf("   tuple?\n");
    return bc + 1;

  case RTL_OP_CJMP8:
    printf("   cjmp8       %+-3d\n", (int8_t)bc[1]);
    return bc + 2;

  case RTL_OP_CJMP16:
    literal = (uint32_t)bc[1] << 0
            | (uint32_t)bc[2] << 8 ;

    printf("   cjmp16      %+-3d\n", (int16_t)literal);
    return bc + 3;

  case RTL_OP_CJMP32:
    literal = (uint32_t)bc[1] << 0
            | (uint32_t)bc[2] << 8
            | (uint32_t)bc[3] << 16
            | (uint32_t)bc[4] << 24 ;

    printf("   cjmp32      %+-3d\n", (int32_t)literal);
    return bc + 5;

  case RTL_OP_JMP8:
    printf("   jmp8        %+-3d\n", (int8_t)bc[1]);
    return bc + 2;

  case RTL_OP_JMP16:
    literal = (uint32_t)bc[1] << 0
            | (uint32_t)bc[2] << 8 ;

    printf("   jmp16       %+-3d\n", (int16_t)literal);
    return bc + 3;

  case RTL_OP_JMP32:
    literal = (uint32_t)bc[1] << 0
            | (uint32_t)bc[2] << 8
            | (uint32_t)bc[3] << 16
            | (uint32_t)bc[4] << 24 ;

    printf("   jmp32       %+-3d\n", (int32_t)literal);
    return bc + 5;

  case RTL_OP_CALL:
    size = (uint16_t)bc[1] << 0
         | (uint16_t)bc[2] << 8;

    printf("   call        %d\n", (int)size);
    return bc + 3;

  case RTL_OP_TAIL:
    size = (uint16_t)bc[1] << 0
         | (uint16_t)bc[2] << 8;

    printf("   tail        %d\n", (int)size);
    return bc + 3;

  case RTL_OP_STATIC_CALL:
    literal = (rtl_Word)bc[1] << 0
            | (rtl_Word)bc[2] << 8
            | (rtl_Word)bc[3] << 16
            | (rtl_Word)bc[4] << 24 ;

    size = (uint16_t)bc[5] << 0
         | (uint16_t)bc[6] << 8;

    func = rtl_reifyFunction(codeBase, literal);

    printf("   static-call %s:%s %d\n",
           rtl_symbolPackageName(func->name),
           rtl_symbolName(func->name),
           (int)size);
    return bc + 7;

  case RTL_OP_STATIC_TAIL:
    literal = (rtl_Word)bc[1] << 0
            | (rtl_Word)bc[2] << 8
            | (rtl_Word)bc[3] << 16
            | (rtl_Word)bc[4] << 24 ;

    size = (uint16_t)bc[5] << 0
         | (uint16_t)bc[6] << 8;

    func = rtl_reifyFunction(codeBase, literal);

    printf("   static-tail %s:%s %d\n",
           rtl_symbolPackageName(func->name),
           rtl_symbolName(func->name),
           (int)size);
    return bc + 7;

  case RTL_OP_APPLY_LIST:
    printf("   apply-list\n");
    return bc + 1;

  case RTL_OP_APPLY_TUPLE:
    printf("   apply-tuple\n");
    return bc + 1;

  case RTL_OP_UNDEFINED_CALL:
    literal = (rtl_Word)bc[1] << 0
            | (rtl_Word)bc[2] << 8
            | (rtl_Word)bc[3] << 16
            | (rtl_Word)bc[4] << 24 ;

    printf("   undef-call  %s:%s\n",
           rtl_symbolPackageName(literal),
           rtl_symbolName(literal));
    return bc + 7;

  case RTL_OP_UNDEFINED_TAIL:
    literal = (rtl_Word)bc[1] << 0
            | (rtl_Word)bc[2] << 8
            | (rtl_Word)bc[3] << 16
            | (rtl_Word)bc[4] << 24 ;

    printf("   undef-tail  %s:%s\n",
           rtl_symbolPackageName(literal),
           rtl_symbolName(literal));
    return bc + 7;

  case RTL_OP_UNDEFINED_VAR:
    literal = (rtl_Word)bc[1] << 0
            | (rtl_Word)bc[2] << 8
            | (rtl_Word)bc[3] << 16
            | (rtl_Word)bc[4] << 24 ;

    printf("   undef-var   %s:%s\n",
           rtl_symbolPackageName(literal),
           rtl_symbolName(literal));
    return bc + 5;

  case RTL_OP_RETURN:
    printf("   return\n");
    return bc + 1;

  case RTL_OP_REST:
    size = (uint16_t)bc[1] << 0
         | (uint16_t)bc[2] << 8;

    printf("   rest        %d\n", (int)size);

    return bc + 3;

  case RTL_OP_VAR:
    frame = (uint16_t)bc[1] << 0
          | (uint16_t)bc[2] << 8;

    idx = (uint16_t)bc[3] << 0
        | (uint16_t)bc[4] << 8;

    printf("   var         %d %d\n", (int)frame, (int)idx);
    return bc + 5;

  case RTL_OP_CLOSURE:
    literal = (rtl_Word)bc[1] << 0
            | (rtl_Word)bc[2] << 8
            | (rtl_Word)bc[3] << 16
            | (rtl_Word)bc[4] << 24 ;

    printf("   closure     Function#%d",
           (int)rtl_functionID(literal));

    func = rtl_reifyFunction(codeBase, literal);
    printf("         ;; %s:%s",
           rtl_symbolPackageName(func->name),
           rtl_symbolName(func->name));
    printf("\n");
    return bc + 5;

  case RTL_OP_LABELS:
    size = (uint16_t)bc[1] << 0
         | (uint16_t)bc[2] << 8;

    printf("   labels      %d\n", (int)size);
    return bc + 3;

  case RTL_OP_END_LABELS:
    printf("   end-labels\n");
    return bc + 1;

  case RTL_OP_TUPLE:
    size = (uint16_t)bc[1] << 0
         | (uint16_t)bc[2] << 8;

    printf("   tuple       %d\n", (int)size);
    return bc + 3;

  case RTL_OP_GET:
    printf("   get\n");
    return bc + 1;

  case RTL_OP_LEN:
    printf("   len\n");
    return bc + 1;

  case RTL_OP_PUSH_FIRST:
    printf("   push-first\n");
    return bc + 1;

  case RTL_OP_PUSH_LAST:
    printf("   push-last\n");
    return bc + 1;

  case RTL_OP_CONCAT:
    printf("   concat\n");
    return bc + 1;

  case RTL_OP_SLICE:
    printf("   slice\n");
    return bc + 1;

  case RTL_OP_DYN_GET:
    literal = (rtl_Word)bc[1] << 0
            | (rtl_Word)bc[2] << 8
            | (rtl_Word)bc[3] << 16
            | (rtl_Word)bc[4] << 24 ;

    printf("   dyn-get     %s:%s\n",
           rtl_symbolPackageName(literal),
           rtl_symbolName(literal));
    return bc + 5;

  case RTL_OP_DYN_SET:
    literal = (rtl_Word)bc[1] << 0
            | (rtl_Word)bc[2] << 8
            | (rtl_Word)bc[3] << 16
            | (rtl_Word)bc[4] << 24 ;

    printf("   dyn-set     %s:%s\n",
           rtl_symbolPackageName(literal),
           rtl_symbolName(literal));
    return bc + 5;

  case RTL_OP_DYN_SAVE:
    literal = (rtl_Word)bc[1] << 0
            | (rtl_Word)bc[2] << 8
            | (rtl_Word)bc[3] << 16
            | (rtl_Word)bc[4] << 24 ;

    printf("   dyn-save    %s:%s\n",
           rtl_symbolPackageName(literal),
           rtl_symbolName(literal));
    return bc + 5;

  case RTL_OP_DYN_RESTORE:
    literal = (rtl_Word)bc[1] << 0
            | (rtl_Word)bc[2] << 8
            | (rtl_Word)bc[3] << 16
            | (rtl_Word)bc[4] << 24 ;

    printf("   dyn-restore %s:%s\n",
           rtl_symbolPackageName(literal),
           rtl_symbolName(literal));
    return bc + 5;

  case RTL_OP_MAP:
    printf("   map\n");
    return bc + 1;

  case RTL_OP_INSERT:
    printf("   insert\n");
    return bc + 1;

  case RTL_OP_LOOKUP:
    printf("   lookup\n");
    return bc + 1;

  case RTL_OP_IADD:
    printf("   iadd\n");
    return bc + 1;

  case RTL_OP_ISUB:
    printf("   isub\n");
    return bc + 1;

  case RTL_OP_IMUL:
    printf("   imul\n");
    return bc + 1;

  case RTL_OP_IDIV:
    printf("   idiv\n");
    return bc + 1;

  case RTL_OP_IMOD:
    printf("   imod\n");
    return bc + 1;

  case RTL_OP_LT:
    printf("   lt\n");
    return bc + 1;

  case RTL_OP_LEQ:
    printf("   leq\n");
    return bc + 1;

  case RTL_OP_GT:
    printf("   gt\n");
    return bc + 1;

  case RTL_OP_GEQ:
    printf("   geq\n");
    return bc + 1;

  case RTL_OP_EQ:
    printf("   eq\n");
    return bc + 1;

  case RTL_OP_NEQ:
    printf("   neq\n");
    return bc + 1;

  case RTL_OP_ISO:
    printf("   iso // unimplemented!\n");
    return bc + 1;

  case RTL_OP_FADD:
    printf("   fadd\n");
    return bc + 1;

  case RTL_OP_FSUB:
    printf("   fsub\n");
    return bc + 1;

  case RTL_OP_FMUL:
    printf("   fmul\n");
    return bc + 1;

  case RTL_OP_FDIV:
    printf("   fdiv\n");
    return bc + 1;

  case RTL_OP_SET_VAR:
    frame = (uint16_t)bc[1] << 0
          | (uint16_t)bc[2] << 8;

    idx = (uint16_t)bc[3] << 0
        | (uint16_t)bc[4] << 8;

    printf("   set-var     %d %d\n", (int)frame, (int)idx);
    return bc + 5;

  case RTL_OP_SET_CAR:
    printf("   set-car\n");
    return bc + 1;

  case RTL_OP_SET_CDR:
    printf("   set-cdr\n");
    return bc + 1;

  case RTL_OP_SET_ELEM:
    printf("   set-elem\n");
    return bc + 1;

  default:
    printf("  ??? ; opcode %d\n", (int)*bc);
    abort();
  }
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
    printf("    @%04X",
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
uint8_t *nextInstruction(uint8_t *bc)
{
  switch ((rtl_Opcode)*bc) {
  case RTL_OP_NOP:
  case RTL_OP_MAP:
  case RTL_OP_INSERT:
  case RTL_OP_LOOKUP:
  case RTL_OP_IADD:
  case RTL_OP_ISUB:
  case RTL_OP_IMUL:
  case RTL_OP_IDIV:
  case RTL_OP_IMOD:
  case RTL_OP_LT:
  case RTL_OP_LEQ:
  case RTL_OP_GT:
  case RTL_OP_GEQ:
  case RTL_OP_EQ:
  case RTL_OP_NEQ:
  case RTL_OP_ISO:
  case RTL_OP_FADD:
  case RTL_OP_FSUB:
  case RTL_OP_FMUL:
  case RTL_OP_FDIV:
  case RTL_OP_SET_CAR:
  case RTL_OP_SET_CDR:
  case RTL_OP_SET_ELEM:
  case RTL_OP_CONST_NIL:
  case RTL_OP_CONST_TOP:
  case RTL_OP_GENSYM:
  case RTL_OP_CONS:
  case RTL_OP_CAR:
  case RTL_OP_CDR:
  case RTL_OP_POP:
  case RTL_OP_SWAP:
  case RTL_OP_DUP:
  case RTL_OP_IS_INT28:
  case RTL_OP_IS_FIX14:
  case RTL_OP_IS_SYMBOL:
  case RTL_OP_IS_SELECTOR:
  case RTL_OP_IS_MAP:
  case RTL_OP_IS_CHAR:
  case RTL_OP_IS_NIL:
  case RTL_OP_IS_TOP:
  case RTL_OP_NOT:
  case RTL_OP_IS_CONS:
  case RTL_OP_IS_TUPLE:
  case RTL_OP_APPLY_LIST:
  case RTL_OP_APPLY_TUPLE:
  case RTL_OP_RETURN:
  case RTL_OP_END_LABELS:
  case RTL_OP_GET:
  case RTL_OP_LEN:
  case RTL_OP_PUSH_FIRST:
  case RTL_OP_PUSH_LAST:
  case RTL_OP_CONCAT:
  case RTL_OP_SLICE:
    return bc + 1;

  case RTL_OP_CJMP8:
  case RTL_OP_JMP8:
    return bc + 2;

  case RTL_OP_JMP16:
  case RTL_OP_CJMP16:
  case RTL_OP_CALL:
  case RTL_OP_TAIL:
  case RTL_OP_REST:
  case RTL_OP_LABELS:
  case RTL_OP_TUPLE:
    return bc + 3;

  case RTL_OP_CJMP32:
  case RTL_OP_JMP32:
  case RTL_OP_UNDEFINED_VAR:
  case RTL_OP_VAR:
  case RTL_OP_CLOSURE:
  case RTL_OP_DYN_GET:
  case RTL_OP_DYN_SET:
  case RTL_OP_DYN_SAVE:
  case RTL_OP_DYN_RESTORE:
  case RTL_OP_SET_VAR:
  case RTL_OP_CONST:
    return bc + 5;

  case RTL_OP_STATIC_CALL:
  case RTL_OP_STATIC_TAIL:
  case RTL_OP_UNDEFINED_CALL:
  case RTL_OP_UNDEFINED_TAIL:
    return bc + 7;

  default:
     printf("Missing case in nextInstruction!\n");
     abort();
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

void rtl_stackTrace(rtl_Machine *M)
{
  size_t         i;
  rtl_Function  *func;
  uint32_t      fnID;
  uint8_t       *pc;
  uint8_t       *prev;

  char fnName[1024];

  printf("Stack Trace (most recent at bottom):\n");
  for (i = 0; i < M->rStackLen; i++) {
    func = rtl_reifyFunction(M->codeBase, M->rStack[i].fn);
    fnID = rtl_functionID(M->rStack[i].fn);

    snprintf(fnName, 1024, "%s:%s",
             rtl_symbolPackageName(func->name),
             rtl_symbolName(func->name));


    printf("    %4zu: %-32s #%04u\n",
           M->rStackLen - 1 - i,
           fnName,
           (unsigned int)fnID);

    if (i + 1 < M->rStackLen) {
      pc = M->rStack[i + 1].pc;
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
