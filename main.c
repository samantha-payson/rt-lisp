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
  rtl_Machine M;
  rtl_Word    w;

  uint8_t code[] = {
    RTL_OP_CONST, I28(1),
    RTL_OP_CONST, I28(2),
    RTL_OP_CONST, I28(3),
    RTL_OP_CONST, I28(4),
    RTL_OP_CONST_NIL,
    RTL_OP_CONS,
    RTL_OP_CONS,
    RTL_OP_CONS,
    RTL_OP_CONS,
    RTL_OP_RETURN,
  };

  rtl_initMachine(&M);

  if (rtl_runSnippet(&M, code)) {
    printf("Error running snippet!\n");
  }

  w = M.vStack[0];

  printf("Result was a '%s': ", rtl_typeNameOf(w));
  rtl_formatExpr(&M, w);
  printf("\n");

  return 0;
}
