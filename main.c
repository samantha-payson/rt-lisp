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

rtl_Word builtinHello(rtl_Machine *M, rtl_Word const *args, size_t argsLen)
{
  assert(argsLen == 0);

  printf("hello, world!\n");

  return RTL_NIL;
}

int main() {
  rtl_CodeBase  codeBase;
  rtl_Machine   M;
  rtl_Compiler  C;
  rtl_Word      w = RTL_NIL,
                a = RTL_NIL,
                b = RTL_NIL;
  uint32_t      replFnID;
  rtl_NameSpace ns;

  rtl_initCodeBase(&codeBase);
  rtl_initMachine(&M, &codeBase);
  rtl_initCompiler(&C, &M);

  rtl_io_installBuiltins(&C);

  RTL_PUSH_WORKING_SET(&M, &w, &a, &b);

  ns = rtl_nsInPackage(NULL, rtl_internPackage(&C, "intrinsic"));

  replFnID = rtl_newFuncID(M.codeBase, rtl_intern("repl", "code-page"));

  while (!feof(stdin)) {
    w = rtl_read(&C, stdin);

    rtl_compile(&C, &ns, replFnID, w);

    if (C.error.type) {
      printf("Error compiling expression!\n");
      return 1;
    }

    rtl_emitByteToFunc(M.codeBase, replFnID, RTL_OP_RETURN);

    rtl_disasmFn(M.codeBase, rtl_function(replFnID));

    printf("\n Running code on VM:\n");

    w = rtl_call(&M, rtl_function(replFnID));

    if (rtl_peekError(&M) != RTL_OK) {
      printf("Error running snippet: '%s'\n",
	     rtl_errString(rtl_getError(&M)));
    }

    printf("\n Result was a '%s': ", rtl_typeNameOf(w));
    rtl_formatExpr(&M, w);
    printf("\n");

    rtl_newFuncVersion(M.codeBase, replFnID);
  }

  rtl_popWorkingSet(&M);

  return 0;
}
