#include <stdio.h>
#include <stdlib.h>

#include "rt-lisp.h"

// This macro is just a helper, designed to be expanded in the middle of a
// uint8_t[] literal.
#define I28(N)					\
  (RTL_INT28 | ((N & 0xF) << 4)),		\
    ((N & 0xFF0) << 4),				\
    ((N & 0xFF000) << 4),			\
    ((N & 0xFF00000) << 4)			\

int main() {
  rtl_Machine   M;
  rtl_Compiler  C;
  rtl_Word      w, a, b;
  uint16_t      replPageID;
  rtl_NameSpace ns;

  rtl_initMachine(&M);
  rtl_initCompiler(&C, &M);

  RTL_PUSH_WORKING_SET(&M, &w, &a, &b);

  ns = rtl_nsInPackage(NULL, rtl_internPackage(&C, "intrinsic"));

  replPageID = rtl_newPageID(&M);

  while (!feof(stdin)) {
    w = rtl_read(&C, stdin);

    rtl_compile(&C, &ns, replPageID, w);

    if (C.error.type) {
      printf("Error compiling expression!\n");
      return 1;
    }

    rtl_emitByteToPage(&M, replPageID, RTL_OP_RETURN);

    rtl_disasmPage(&M, replPageID);

    printf("\n Running code on VM:\n");

    w = rtl_run(&M, rtl_addr(replPageID, 0));

    if (rtl_peekError(&M) != RTL_OK) {
      printf("Error running snippet: '%s'\n",
	     rtl_errString(rtl_getError(&M)));
    }

    printf("\n Result was a '%s': ", rtl_typeNameOf(w));
    rtl_formatExpr(&M, w);
    printf("\n");

    rtl_newPageVersion(&M, replPageID);
  }

  rtl_popWorkingSet(&M);

  return 0;
}
