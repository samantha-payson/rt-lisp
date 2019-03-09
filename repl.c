#include <stdio.h>
#include <string.h>

#include "rt-lisp.h"

typedef struct rtl_repl_Compiler {
  char const          *tag;
  rtl_Compiler        *C;
  rtl_NameSpace const *ns;
} rtl_repl_Compiler;

void rtl_load(rtl_Compiler *C, rtl_NameSpace const *ns, char const *path)
{
  rtl_Word w = RTL_NIL;
  FILE     *file;
  uint32_t scratchFnID;

  RTL_PUSH_WORKING_SET(C->M, &w);

  file = fopen(path, "r");
  if (!file) {
    w = RTL_MAP;
    w = rtl_mapInsert(C->M, w, rtl_internSelector(NULL, "type"),
                      rtl_internSelector(NULL, "fopen-failed"));
    w = rtl_mapInsert(C->M, w, rtl_internSelector(NULL, "message"),
                      rtl_string(C->M, "Failed to open file"));
    w = rtl_mapInsert(C->M, w, rtl_internSelector(NULL, "path"),
                      rtl_string(C->M, path));

    __rtl_triggerFault(C->M, w);

  } else {
    scratchFnID = rtl_newFuncID(C->M->codeBase, rtl_intern("repl", "scratch"));

    while (!feof(file)) {
      w = rtl_read(C, file);

      rtl_compile(C, ns, scratchFnID, w);

      rtl_emitByteToFunc(C->M->codeBase, scratchFnID, RTL_OP_RETURN);

      if (!rtl_clearFault(C->M)) {
        C->M->env = RTL_TUPLE;
        w = rtl_call(C->M, rtl_function(scratchFnID));

        rtl_clearFault(C->M);
      }

      rtl_newFuncVersion(C->M->codeBase, scratchFnID);
    }

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
  rtl_reifyNative(M, handle, &wrapper, sizeof(rtl_repl_Compiler));
  assert(!strcmp(wrapper.tag, "rtl_repl_Compiler"));

  pathLen = rtl_stringSize(M, args[0]);
  path    = malloc(pathLen + 1);

  rtl_reifyString(M, args[0], path, pathLen + 1);

  rtl_load(wrapper.C, wrapper.ns, path);

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
  rtl_reifyNative(M, handle, &wrapper, sizeof(rtl_repl_Compiler));
  assert(!strcmp(wrapper.tag, "rtl_repl_Compiler"));

  result = rtl_macroExpand(wrapper.C, wrapper.ns, args[0]);

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

void rtl_repl(rtl_Compiler *C)
{
  rtl_repl_Compiler wrapper;
  rtl_Word          handle = RTL_NIL, w = RTL_NIL;
  rtl_NameSpace     replNS, useNS, intrinsicNS;
  uint32_t          replFnID;

  RTL_PUSH_WORKING_SET(C->M, &handle, &w);

  // First step: Load the prelude
  intrinsicNS = rtl_nsInPackage(NULL, rtl_internPackage(C, "intrinsic"));
  rtl_load(C, &intrinsicNS, "lisp/bootstrap.lisp");
  rtl_load(C, &intrinsicNS, "lisp/prelude.lisp");

  replNS = rtl_nsInPackage(NULL, rtl_internPackage(C, "repl"));
  useNS  = rtl_nsUsePackage(&replNS, rtl_internPackage(C, "std"));

  wrapper.tag = "rtl_repl_Compiler";
  wrapper.C   = C;
  wrapper.ns  = &useNS;

  handle = rtl_native(C->M, &wrapper, sizeof(rtl_repl_Compiler));

  rtl_setVar(C->M, rtl_intern("std", "*compiler*"), handle);

  replFnID = rtl_newFuncID(C->M->codeBase, rtl_intern("repl", "code-page"));

  rtl_registerBuiltin(C, rtl_intern("std", "load"), rtl_repl_load);
  rtl_export(C, rtl_intern("std", "load"));

  rtl_registerBuiltin(C, rtl_intern("std", "disassemble"), rtl_repl_disassemble);
  rtl_export(C, rtl_intern("std", "disassemble"));

  rtl_registerBuiltin(C, rtl_intern("std", "disassemble-macro"),
                      rtl_repl_disassembleMacro);
  rtl_export(C, rtl_intern("std", "disassemble-macro"));

  rtl_registerBuiltin(C, rtl_intern("std", "macroexpand"), rtl_repl_macroExpand);
  rtl_export(C, rtl_intern("std", "macroexpand"));

  while (!feof(stdin)) {
    printf("\n[ \x1B[1mCRTL\x1B[0m ] ");
    fflush(stdout);
    w = rtl_read(C, stdin);

    rtl_compile(C, &useNS, replFnID, w);

    rtl_emitByteToFunc(C->M->codeBase, replFnID, RTL_OP_RETURN);

    if (!rtl_clearFault(C->M)) {
      C->M->env = RTL_TUPLE;

      w = rtl_call(C->M, rtl_function(replFnID));

      if (!rtl_clearFault(C->M)) {
        printf("\n=> ");
        rtl_formatExpr(C->M, w);
        printf("\n");
      } else {
        printf("\n  \x1B[1mERROR!\x1B[0m\n");
      }

      rtl_newFuncVersion(C->M->codeBase, replFnID);
    }
  }

  rtl_popWorkingSet(C->M);
}
