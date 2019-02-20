#include <stdio.h>
#include <string.h>

#include "rt-lisp.h"

typedef struct rtl_repl_Compiler {
  char const          *tag;
  rtl_Compiler        *C;
  rtl_NameSpace const *ns;
} rtl_repl_Compiler;

static
rtl_Word rtl_repl_load(rtl_Machine *M, rtl_Word const *args, size_t argsLen)
{
  rtl_Word          handle = RTL_NIL;
  rtl_repl_Compiler wrapper;
  rtl_Compiler      *C;
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

  C = wrapper.C;

  pathLen = rtl_stringLength(M, args[0]);
  path    = malloc(pathLen + 1);

  rtl_reifyString(M, args[0], path, pathLen + 1, &pathLen);

  rtl_load(C, wrapper.ns, path);

  rtl_popWorkingSet(M);

  return rtl_internSelector(NULL, "OK");
}

void rtl_load(rtl_Compiler *C, rtl_NameSpace const *ns, char const *path)
{
  rtl_Word w = RTL_NIL;
  FILE     *file;
  uint32_t scratchFnID;

  RTL_PUSH_WORKING_SET(C->M, &w);

  file = fopen(path, "r");
  if (!file) {
    printf("  error: load: fopen: Can't open '%s'\n", path);
  }

  scratchFnID = rtl_newFuncID(C->M->codeBase, rtl_intern("repl", "scratch"));

  while (!feof(file)) {
    w = rtl_read(C, file);

    rtl_compile(C, ns, scratchFnID, w);

    if (C->error.type) {
      printf("Error compiling expression!\n");
      asm("int3");
    }

    rtl_emitByteToFunc(C->M->codeBase, scratchFnID, RTL_OP_RETURN);

    w = rtl_call(C->M, rtl_function(scratchFnID));

    if (rtl_peekError(C->M) != RTL_OK) {
      printf("Error running snippet: '%s'\n",
	     rtl_errString(rtl_getError(C->M)));
      asm("int3");
    }

    rtl_newFuncVersion(C->M->codeBase, scratchFnID);
  }

  fclose(file);

  rtl_popWorkingSet(C->M);
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
  rtl_load(C, &intrinsicNS, "prelude.lisp");

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

  while (!feof(stdin)) {
    w = rtl_read(C, stdin);

    rtl_compile(C, &useNS, replFnID, w);

    if (C->error.type) {
      printf("Error compiling expression!\n");
      asm("int3");
    }

    rtl_emitByteToFunc(C->M->codeBase, replFnID, RTL_OP_RETURN);

    rtl_disasmFn(C->M->codeBase, rtl_function(replFnID));

    w = rtl_call(C->M, rtl_function(replFnID));

    if (rtl_peekError(C->M) != RTL_OK) {
      printf("Error running snippet: '%s'\n",
	     rtl_errString(rtl_getError(C->M)));
    }

    printf("\n Result was a '%s': ", rtl_typeNameOf(w));
    rtl_formatExpr(C->M, w);
    printf("\n");

    rtl_newFuncVersion(C->M->codeBase, replFnID);
  }

  rtl_popWorkingSet(C->M);
}

