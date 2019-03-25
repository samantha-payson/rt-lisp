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
#include <string.h>

#include "rt-lisp.h"

typedef struct rtl_repl_Compiler {
  char const          *tag;
  rtl_Compiler        *C;
  rtl_NameSpace const *ns;
} rtl_repl_Compiler;

void rtl_xLoad(rtl_Compiler *C, rtl_NameSpace const *ns, char const *path)
{
  rtl_Word w = RTL_NIL;
  FILE     *file;
  uint32_t scratchFnID;

  RTL_PUSH_WORKING_SET(C->M, &w);

  file = fopen(path, "r");
  if (!file) {
    w = RTL_MAP;
    w = rtl_xMapInsert(C->M, w, rtl_internSelector(NULL, "type"),
                       rtl_internSelector(NULL, "fopen-failed"));
    RTL_ASSERT_NO_UNWIND(C->M);

    w = rtl_xMapInsert(C->M, w, rtl_internSelector(NULL, "message"),
                       rtl_string(C->M, "Failed to open file"));
    RTL_ASSERT_NO_UNWIND(C->M);

    w = rtl_xMapInsert(C->M, w, rtl_internSelector(NULL, "path"),
                       rtl_string(C->M, path));
    RTL_ASSERT_NO_UNWIND(C->M);

    rtl_throw(C->M, w);

  } else {
    scratchFnID = rtl_newFuncID(C->M->codeBase, rtl_intern("repl", "scratch"));

    while (!feof(file)) {
      w = rtl_xRead(C, file);
      RTL_UNWIND (C->M) goto cleanup;

      rtl_xCompile(C, ns, scratchFnID, w);
      RTL_UNWIND (C->M) goto cleanup;

      rtl_emitByteToFunc(C->M->codeBase, scratchFnID, RTL_OP_RET);

      C->M->env = RTL_TUPLE;
      
      rtl_xCall(C->M, rtl_function(scratchFnID));
      RTL_UNWIND (C->M) goto cleanup;

      rtl_newFuncVersion(C->M->codeBase, scratchFnID);
    }

 cleanup:
    fclose(file);
  }

  rtl_popWorkingSet(C->M);
}

static
rtl_Word rtl_repl_load(rtl_Machine *M, rtl_Word const *args, size_t argsLen)
{
  rtl_Word          handle = RTL_NIL;
  rtl_repl_Compiler wrapper;
  char              *path;
  size_t            pathLen;

  if (argsLen != 1) {
    printf("\n  usage: (load <path>)\n\n");
    return RTL_NIL;
  }

  RTL_PUSH_WORKING_SET(M, &handle);

  handle = rtl_getVar(M, rtl_intern("std", "*compiler*"));

  rtl_xReifyNative(M, handle, &wrapper, sizeof(rtl_repl_Compiler));
  RTL_UNWIND (M) goto cleanup;

  assert(!strcmp(wrapper.tag, "rtl_repl_Compiler"));

  pathLen = rtl_xStringSize(M, args[0]);
  RTL_UNWIND (M) goto cleanup;
  path    = malloc(pathLen + 1);

  rtl_xReifyString(M, args[0], path, pathLen + 1);
  RTL_UNWIND (M) {
    free(path);
    goto cleanup;
  }

  rtl_xLoad(wrapper.C, wrapper.ns, path);

 cleanup:
  rtl_popWorkingSet(M);

  return rtl_internSelector(NULL, "ok");
}

static
rtl_Word rtl_repl_macroExpand(rtl_Machine *M, rtl_Word const *args, size_t argsLen)
{
  rtl_Word          handle = RTL_NIL, result = RTL_NIL;
  rtl_repl_Compiler wrapper;

  if (argsLen != 1) {
    printf("\n  usage: (macroexpand <expr>)\n\n");
    return RTL_NIL;
  }

  RTL_PUSH_WORKING_SET(M, &handle, &result);

  handle = rtl_getVar(M, rtl_intern("std", "*compiler*"));
  rtl_xReifyNative(M, handle, &wrapper, sizeof(rtl_repl_Compiler));
  RTL_UNWIND (M) goto cleanup;

  assert(!strcmp(wrapper.tag, "rtl_repl_Compiler"));

  result = rtl_xMacroExpand(wrapper.C, wrapper.ns, args[0]);

 cleanup:
  rtl_popWorkingSet(M);

  return result;
}

static
rtl_Word rtl_repl_disassemble(rtl_Machine *M, rtl_Word const *args, size_t argsLen)
{
  rtl_Word fn;

  if (argsLen != 1) {
    printf("\n  usage: (disassemble <fn>)\n\n");
    return RTL_NIL;
  }

  if (rtl_isInt28(args[0])) {
    fn = rtl_function(rtl_int28Value(args[0]));
  } else {
    fn = args[0];
  }

  rtl_disasmFn(M, fn);

  return rtl_internSelector(NULL, "ok");
}

static
rtl_Word rtl_repl_disassembleMacro(rtl_Machine *M, rtl_Word const *args, size_t argsLen)
{
  if (argsLen != 1) {
    printf("\n  usage: (disassemble-macro <name>)\n\n");
    return RTL_NIL;
  }

  rtl_disasmMacro(M, args[0]);

  return rtl_internSelector(NULL, "ok");
}

void rtl_xRepl(rtl_Compiler *C)
{
  rtl_repl_Compiler wrapper;
  rtl_Word          handle = RTL_NIL, w = RTL_NIL;
  rtl_NameSpace     replNS, useNS, intrinsicNS;
  uint32_t          replFnID;

  RTL_PUSH_WORKING_SET(C->M, &handle, &w);

  // First step: Load the prelude
  intrinsicNS = rtl_nsInPackage(NULL, rtl_internPackage(C, "intrinsic"));
  rtl_xLoad(C, &intrinsicNS, "lisp/bootstrap.lisp");
  RTL_UNWIND (C->M) goto cleanup;

  rtl_xLoad(C, &intrinsicNS, "lisp/prelude.lisp");
  RTL_UNWIND (C->M) goto cleanup;

  replNS = rtl_nsInPackage(NULL, rtl_internPackage(C, "repl"));
  useNS  = rtl_nsUsePackage(&replNS, rtl_internPackage(C, "std"));

  wrapper.tag = "rtl_repl_Compiler";
  wrapper.C   = C;
  wrapper.ns  = &useNS;

  handle = rtl_native(C->M, &wrapper, sizeof(rtl_repl_Compiler));

  rtl_setVar(C->M, rtl_intern("std", "*compiler*"), handle);

  replFnID = rtl_newFuncID(C->M->codeBase, rtl_intern("repl", "code-page"));

  rtl_registerBuiltin(C, rtl_intern("std", "load"), rtl_repl_load);
  rtl_xExport(C, rtl_intern("std", "load"));
  RTL_ASSERT_NO_UNWIND(C->M);

  rtl_registerBuiltin(C, rtl_intern("std", "disassemble"), rtl_repl_disassemble);
  rtl_xExport(C, rtl_intern("std", "disassemble"));
  RTL_ASSERT_NO_UNWIND(C->M);

  rtl_registerBuiltin(C, rtl_intern("std", "disassemble-macro"),
                      rtl_repl_disassembleMacro);
  rtl_xExport(C, rtl_intern("std", "disassemble-macro"));
  RTL_ASSERT_NO_UNWIND(C->M);

  rtl_registerBuiltin(C, rtl_intern("std", "macroexpand"), rtl_repl_macroExpand);
  rtl_xExport(C, rtl_intern("std", "macroexpand"));
  RTL_ASSERT_NO_UNWIND(C->M);

  while (!feof(stdin)) {
    rtl_newFuncVersion(C->M->codeBase, replFnID);

    printf("\n[ \x1B[1mCRTL\x1B[0m ] ");
    fflush(stdout);
    w = rtl_xRead(C, stdin);
    RTL_UNWIND (C->M) {
      rtl_printException(C->M, C->M->exception);
      rtl_clearException(C->M);
      printf("\n  \x1B[1mERROR!\x1B[0m\n");

      continue;
    }

    rtl_xCompile(C, &useNS, replFnID, w);
    RTL_UNWIND (C->M) {
      rtl_printException(C->M, C->M->exception);
      rtl_clearException(C->M);
      printf("\n  \x1B[1mERROR!\x1B[0m\n");

      continue;
    }

    rtl_emitByteToFunc(C->M->codeBase, replFnID, RTL_OP_RET);

    C->M->env = RTL_TUPLE;

    w = rtl_xCall(C->M, rtl_function(replFnID));

    RTL_UNWIND (C->M) {
      rtl_printException(C->M, C->M->exception);
      rtl_clearException(C->M);
      printf("\n  \x1B[1mERROR!\x1B[0m\n");

      continue;
    }

    printf("\n=> ");
    rtl_formatExpr(C->M, w);
    printf("\n");
  }

 cleanup:
  rtl_popWorkingSet(C->M);
}
