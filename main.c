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
#include <signal.h>

#include "rt-lisp.h"

rtl_Machine M;

void ctrlC(int sig)
{
  rtl_throwMsg(&M, "interrupt",
               "An interrupt was signalled by the user.");

  signal(SIGINT, ctrlC);
}

int main() {
  rtl_CodeBase  codeBase;

  rtl_Compiler  C;

  rtl_initCodeBase(&codeBase);
  rtl_initMachine(&M, &codeBase);
  rtl_initCompiler(&C, &M);

  signal(SIGINT, ctrlC);

  rtl_io_installBuiltins(&C);

  rtl_repl(&C);

  return 0;
}
