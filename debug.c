#include "rt-lisp.h"

#include <stdio.h>


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
    printf("%s:%s", rtl_symbolPackageName(w), rtl_symbolName(w));
    break;

  case RTL_UNRESOLVED_SYMBOL:
    printf("<unres-sym>");
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

  case RTL_STRING:
    printf("String#%Xg%d", __rtl_ptrOffs(w), __rtl_ptrGen(w));
    break;

  case RTL_MAP:
    printf("Map#%Xg%d", __rtl_ptrOffs(w), __rtl_ptrGen(w));
    break;

  case RTL_CONS:
    printf("Cons#%Xg%d", __rtl_ptrOffs(w), __rtl_ptrGen(w));
    break;

  case RTL_ADDR:
    printf("Addr#%X", w >> 4);
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

void __rtl_formatMap(rtl_Machine *M, rtl_Word map, int indent, uint32_t mask)
{
  rtl_Word const *rptr, *entry;
  size_t   len,
           i;

  if (rtl_isEmptyMap(map)) return;

  rptr = __rtl_reifyPtr(M, map);
  len  = __builtin_popcount(mask);

  for (i = 0; i < len; i++) {
    entry = rptr + 2*i;

    if (rtl_isHeader(entry[0])) {
      __rtl_formatMap(M, entry[1], indent, rtl_headerValue(entry[0]));
    } else {
      rtl_formatExprIndented(M, entry[0], indent + 1);
      printf(" ");
      rtl_formatExprIndented(M, entry[1], indent + 1);
      printf(", ");
    }
  }
}

void rtl_formatExprIndented(rtl_Machine *M, rtl_Word w, int indent)
{
  rtl_Word const *ptr;

  size_t         len,
                 i;

  switch (rtl_typeOf(w)) {
  case RTL_TUPLE:
    ptr = rtl_reifyTuple(M, w, &len);

    printf("[ ");
    for (i = 0; i < len; i++) {
      rtl_formatExprIndented(M, ptr[i], indent + 1);
      printf(" ");
    }
    printf("]");
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
      __rtl_formatMap(M, w, indent, 1);
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

uint8_t *rtl_disasm(uint8_t *bc)
{
  rtl_Word literal;
  uint16_t frame, idx, size;

  printf("%X: ", (unsigned int)(((uintptr_t)bc) & 0xFFFF));

  switch (*bc) {
  case RTL_OP_NOP:
    printf("   nop\n");
    return bc + 1;

  case RTL_OP_CONST:
    literal = (rtl_Word)bc[1] << 0
            | (rtl_Word)bc[2] << 8
            | (rtl_Word)bc[3] << 16
            | (rtl_Word)bc[4] << 24 ;

    printf("   const     ");
    rtl_formatExprShallow(literal);
    printf("\n");

    return bc + 5;

  case RTL_OP_CONST_NIL:
    printf("   nil\n");
    return bc + 1;

  case RTL_OP_CONST_TOP:
    printf("   top\n");
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

    // `nil?' and `not' are actually the same function.
  case RTL_OP_IS_NIL:
    printf("   nil?\n");
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

  case RTL_OP_CJMP:
    literal = (rtl_Word)bc[1] << 0
            | (rtl_Word)bc[2] << 8
            | (rtl_Word)bc[3] << 16
            | (rtl_Word)bc[4] << 24 ;

    printf("   cjmp      %d#%X\n",
	   (int)rtl_addrPage(literal),
	   (unsigned int)rtl_addrOffs(literal));
    return bc + 5;

    // Intentionally fallthrough into JMP ...

  case RTL_OP_JMP:
    literal = (rtl_Word)bc[1] << 0
            | (rtl_Word)bc[2] << 8
            | (rtl_Word)bc[3] << 16
            | (rtl_Word)bc[4] << 24 ;

    printf("   jmp       %d#%X\n",
	   (int)rtl_addrPage(literal),
	   (unsigned int)rtl_addrOffs(literal));
    return bc + 5;

  case RTL_OP_CALL:
    size = (uint16_t)bc[1] << 0
         | (uint16_t)bc[2] << 8;

    printf("   call      %d\n", (int)size);
    return bc + 3;

  case RTL_OP_STATIC_CALL:
    literal = (rtl_Word)bc[1] << 0
            | (rtl_Word)bc[2] << 8
            | (rtl_Word)bc[3] << 16
            | (rtl_Word)bc[4] << 24 ;

    size = (uint16_t)bc[5] << 0
         | (uint16_t)bc[6] << 8;

    printf("   static-call %d#%X %d\n",
	   (int)rtl_addrPage(literal),
	   (unsigned int)rtl_addrOffs(literal),
	   (int)size);
    return bc + 7;

  case RTL_OP_APPLY_LIST:
    printf("  apply-list\n");
    return bc + 1;

  case RTL_OP_APPLY_TUPLE:
    printf("  apply-tuple\n");
    return bc + 1;

  case RTL_OP_UNDEFINED_FUNCTION:
    printf("   undefined-fn\n");
    return bc + 7;

  case RTL_OP_UNDEFINED_VAR:
    printf("   undefined-var");
    return bc + 5;

  case RTL_OP_RETURN:
    printf("   return\n");
    return bc + 1;

  case RTL_OP_REST:
    size = (uint16_t)bc[1] << 0
         | (uint16_t)bc[2] << 8;

    printf("   rest %d\n", (int)size);

    return bc + 3;

  case RTL_OP_VAR:
    frame = (uint16_t)bc[1] << 0
          | (uint16_t)bc[2] << 8;

    idx = (uint16_t)bc[3] << 0
        | (uint16_t)bc[4] << 8;

    printf("   var       %d %d\n", (int)frame, (int)idx);
    return bc + 5;

  case RTL_OP_CLOSURE:
    literal = (rtl_Word)bc[1] << 0
            | (rtl_Word)bc[2] << 8
            | (rtl_Word)bc[3] << 16
            | (rtl_Word)bc[4] << 24 ;

    printf("   closure   %d#%X\n",
	   (int)rtl_addrPage(literal),
	   (unsigned int)rtl_addrOffs(literal));
    return bc + 5;

  case RTL_OP_TUPLE:
    size = (uint16_t)bc[1] << 0
         | (uint16_t)bc[2] << 8;

    printf("   tuple %d\n", (int)size);
    return bc + 3;

  case RTL_OP_GET:
    printf("   get\n");
    return bc + 1;

  case RTL_OP_LEN:
    printf("   len\n");
    return bc + 1;

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

  default:
    printf("  ??? ; opcode %d\n", (int)*bc);
    abort();
  }
}

void rtl_disasmPage(rtl_Machine *M, uint16_t pageID)
{
  uint8_t *code, *end;

  assert(pageID < M->pagesLen);

  code = M->pages[pageID]->code;
  end  = code + M->pages[pageID]->len;

  printf("\n ---- Disassembly of Page #% 3d ----\n\n", (int)pageID);

  while (code < end) {
    code = rtl_disasm(code);
  }

  printf("\n ----------------------------------\n\n");
}

char const *rtl_errString(rtl_Error err)
{
  switch (err) {
  case RTL_OK:
    return "OK -- No Error";

  case RTL_ERR_INVALID_OPERATION:
    return "Invalid Operation";

  case RTL_ERR_OUT_OF_MEMORY:
    return "Out of Memory";

  case RTL_ERR_STACK_UNDERFLOW:
    return "Stack Underflow";

  case RTL_ERR_EXPECTED_TUPLE:
    return "Expected Tuple";

  case RTL_ERR_EXPECTED_CONS:
    return "Expected Cons";

  case RTL_ERR_EXPECTED_INT28:
    return "Expected Int28";

  case RTL_ERR_EXPECTED_FIX14:
    return "Expected Fix14";

  default:
    return "[Uknown Error]";
  }
}
