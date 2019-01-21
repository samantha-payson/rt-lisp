#include <stdio.h>
#include <stdlib.h>

#include "rt-lisp.h"

// This macro is just a helper, designed to be expanded in the middle of a uint8_t[] literal.
#define I28(N)					\
  (RTL_INT28 | ((N & 0xF) << 4)),		\
    ((N & 0xFF0) << 4),				\
    ((N & 0xFF000) << 4),			\
    ((N & 0xFF00000) << 4)			\

int main() {
  rtl_Machine   M;
  rtl_Compiler  C;
  rtl_Word      w, a, b;
  uint16_t      pageID;
  rtl_NameSpace ns;

  rtl_initMachine(&M);
  rtl_initCompiler(&C, &M);

  RTL_PUSH_WORKING_SET(&M, &w, &a, &b);

  ns = rtl_nsInPkg(NULL, rtl_internPackage(&C, "intrinsic"));

  w = rtl_read(&C, stdin);
  w = rtl_macroExpand(&C, &ns, w);

  printf("\n Input source was: ");
  rtl_formatExpr(&M, w);
  printf("\n");

  pageID = rtl_newPageID(&M);

  rtl_compileExpr(&C, pageID, w);

  if (C.error.type) {
    printf("Error compiling expression!\n");
    return 1;
  }

  rtl_emitByteToPage(&M, pageID, RTL_OP_RETURN);

  printf("\n Running code on VM:\n");

  if (rtl_run(&M, rtl_addr(pageID, 0))) {
    printf("Error running snippet!\n");
    return 1;
  }

  w = M.vStack[0];

  printf("\n Result was a '%s': ", rtl_typeNameOf(w));
  rtl_formatExpr(&M, w);
  printf("\n");

  return 0;
}
