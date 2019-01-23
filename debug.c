#include "rt-lisp.h"

#include <stdio.h>


// Format an expression, but don't follow any pointers. Basically just print
// atoms, and addresses for non-atoms.
void rtl_formatExprShallow(rtl_Word w)
{
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

  case RTL_SELECTOR:
    printf("<selector>");
    break;

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

  case RTL_RECORD:
    printf("Record#%Xg%d", __rtl_ptrOffs(w), __rtl_ptrGen(w));
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
  }
}

void rtl_formatExpr(rtl_Machine *M, rtl_Word w)
{
  rtl_Word const *ptr;
  size_t         len,
                 i;

  switch (rtl_typeOf(w)) {
  case RTL_TUPLE:
    ptr = rtl_reifyTuple(M, w, &len);

    printf("[");
    if (len != 0) {
      rtl_formatExpr(M, ptr[0]);
    }
    for (i = 1; i < len; i++) {
      printf(" ");
      rtl_formatExpr(M, ptr[i]);
    }
    printf("]");
    break;

  case RTL_CONS:
    printf("(");
    ptr = rtl_reifyCons(M, w);
    rtl_formatExpr(M, ptr[0]);

    while (rtl_isCons(ptr[1])) {
      ptr = rtl_reifyCons(M, ptr[1]);
      printf(" ");
      rtl_formatExpr(M, ptr[0]);
    }
    if (!rtl_isNil(ptr[1])) {
      printf(" . ");
      rtl_formatExpr(M, ptr[1]);
    }
    printf(")");
    break;

  default:
    rtl_formatExprShallow(w);
    break;
  }
}

uint8_t *rtl_disasm(uint8_t *bc)
{
  rtl_Word literal;
  uint16_t frame, idx, size;

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

    printf("   cjmp      #%X\n", literal);
    return bc + 5;

    // Intentionally fallthrough into JMP ...

  case RTL_OP_JMP:
    literal = (rtl_Word)bc[1] << 0
            | (rtl_Word)bc[2] << 8
            | (rtl_Word)bc[3] << 16
            | (rtl_Word)bc[4] << 24 ;

    printf("   jmp       #%X\n", literal);
    return bc + 5;

  case RTL_OP_CALL:
    size = (uint16_t)bc[1] << 0
         | (uint16_t)bc[2] << 8;

    printf("   call      %d\n", (int)size);
    return bc + 3;

  case RTL_OP_RETURN:
    printf("   return\n");
    return bc + 1;

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

    printf("   closure %d#%X\n",
	   (int)rtl_addrPage(literal),
	   (unsigned int)rtl_addrOffs(literal));
    return bc + 5;

  case RTL_OP_EXPLODE:
    printf("   explode\n");
    return bc + 1;

  case RTL_OP_TUPLE:
    printf("   tuple\n");
    return bc + 1;

  case RTL_OP_GET:
    printf("   get\n");
    return bc + 1;

  case RTL_OP_LEN:
    printf("   len\n");
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
    break;
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
