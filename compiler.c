#include "rt-lisp.h"

#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#define likely(x)       __builtin_expect(!!(x),1)
#define unlikely(x)     __builtin_expect((x),0)

// Return the CallSiteArray for a given function name, creating it if none exists.
static
rtl_CallSiteArray *getCallSiteArray(rtl_Compiler *C, rtl_Word name)
{
  size_t            idx;
  rtl_CallSiteArray *csa;

  idx = name % RTL_COMPILER_CALL_SITE_HASH_SIZE;

  for (csa = C->callSitesByName[idx]; csa; csa = csa->next) {
    if (name == csa->fnName) {
      return csa;
    }
  }

  // No such CSA... time to make one.
  csa = malloc(sizeof(rtl_CallSiteArray));
  csa->fnName   = name;
  csa->sites    = NULL;
  csa->sitesLen = 0;
  csa->sitesCap = 0;

  csa->next = C->callSitesByName[idx];
  C->callSitesByName[idx] = csa;

  return csa;
}

void rtl_registerCallSite(rtl_Compiler *C, rtl_Word name, rtl_Word siteAddr)
{
  rtl_CallSiteArray *csa;
  uint16_t          pageID;

  csa = getCallSiteArray(C, name);

  if (csa->sitesLen == csa->sitesCap) {
    csa->sitesCap = !csa->sitesCap ? 16 : 2*csa->sitesCap;
    csa->sites    = realloc(csa->sites, csa->sitesCap*sizeof(rtl_CallSite));
  }

  pageID = rtl_addrPage(siteAddr);

  csa->sites[csa->sitesLen++] = (rtl_CallSite) {
    .version = C->M->pages[pageID]->version,
    .pageID  = pageID,
    .offs    = rtl_addrOffs(siteAddr),
  };
}

void rtl_resolveCallSites(rtl_Compiler *C, rtl_Word name, rtl_Word fn)
{
  rtl_CallSiteArray *csa;
  size_t            r, w;
  rtl_CallSite      *site;
  rtl_Page          *page;
  uint8_t           *code;

  csa = getCallSiteArray(C, name);
  for (r = w = 0; r < csa->sitesLen; r++) {
    site = csa->sites + r;
    page = C->M->pages[site->pageID];
    code = page->code + site->offs;

    if (site->version == page->version) {
      csa->sites[w++] = csa->sites[r];

      code[0] = RTL_OP_CALL;
      code[1] = (fn >>  0) & 0xFF;
      code[2] = (fn >>  8) & 0xFF;
      code[3] = (fn >> 16) & 0xFF;
      code[4] = (fn >> 24) & 0xFF;
    }
  }

  csa->sitesLen = w;
}

rtl_Package *rtl_internPackage(rtl_Compiler *C, char const *pkgName)
{
  uint32_t    pkgID;
  uint32_t    idx;
  rtl_Package *pkg;

  pkgID = rtl_internPackageID(pkgName);
  idx   = pkgID % RTL_COMPILER_PKG_HASH_SIZE;

  for (pkg = C->pkgByID[idx]; pkg != NULL; pkg = pkg->next) {
    if (pkg->id == pkgID) {
      return pkg;
    }
  }

  pkg = malloc(sizeof(rtl_Package));

  pkg->name       = strdup(pkgName);
  pkg->id         = pkgID;
  pkg->exports    = NULL;
  pkg->exportsCap = 0;
  pkg->exportsLen = 0;

  pkg->next       = C->pkgByID[idx];
  C->pkgByID[idx] = pkg;

  return pkg;
}

// Intern a symbol, with a given package and name. 
rtl_Word rtl_intern(char const *pkg, char const *name)
{
  uint32_t    pkgID;
  uint32_t    id;

  pkgID = rtl_internPackageID(pkg);
  id    = rtl_internSymbolID(pkgID, name);

  return (id << 4) | RTL_SYMBOL;
}

static struct symCache_t {
  struct {
    rtl_Word cons,
             car,
             cdr,
             var,
             call,
             namedCall,
             lambda,
             iadd,
             isub,
             imul,
             idiv,
             imod;
  } intrinsic;
} symCache;

static bool symCacheWasInit;

static inline
void ensureSymCache() {
  if (unlikely(!symCacheWasInit)) {
    symCache = (struct symCache_t) {
      .intrinsic = {
	.cons      = rtl_intern("intrinsic", "cons"),
	.car       = rtl_intern("intrinsic", "car"),
	.cdr       = rtl_intern("intrinsic", "cdr"),
	.var       = rtl_intern("intrinsic", "var"),
	.call      = rtl_intern("intrinsic", "call"),
	.namedCall = rtl_intern("intrinsic", "named-call"),
	.lambda    = rtl_intern("intrinsic", "lambda"),
	.iadd      = rtl_intern("intrinsic", "iadd"),
	.isub      = rtl_intern("intrinsic", "isub"),
	.imul      = rtl_intern("intrinsic", "imul"),
	.idiv      = rtl_intern("intrinsic", "idiv"),
	.imod      = rtl_intern("intrinsic", "imod"),
      },
    };
  }
}

void rtl_initCompiler(rtl_Compiler *C, rtl_Machine *M) {
  C->error = (rtl_CompilerError) {
    .type = RTL_COMPILER_OK,
  };

  C->M = M;

  memset(C->callSitesByName, 0, sizeof C->callSitesByName);
  memset(C->pkgByID, 0, sizeof C->pkgByID);
}

// In the future, this will check if w is the name of a macro... right now it
// just returns false 100% of the time.
bool rtl_isMacroName(rtl_Compiler *C, rtl_Word w) {
  return false;
}

rtl_Word rtl_macroExpand(rtl_Compiler *C, rtl_NameSpace const *ns, rtl_Word in)
{
  rtl_Word head = RTL_NIL,
    arg  = RTL_NIL,
    tail = RTL_NIL,
    out  = RTL_NIL;

  RTL_PUSH_WORKING_SET(C->M, &in, &head, &arg, &tail, &out);

  switch (rtl_typeOf(in)) {
  case RTL_UNRESOLVED_SYMBOL:
    out = rtl_resolveSymbol(C, ns, rtl_symbolID(in));
    break;

  case RTL_CONS:
    head = rtl_macroExpand(C, ns, rtl_car(C->M, in));

    if (rtl_isMacroName(C, head)) {
      // TODO: Implement macros
      out = RTL_NIL;
      break;
    }

    out = rtl_cons(C->M, head, RTL_NIL);

    for (tail = rtl_cdr(C->M, in); rtl_isCons(tail); tail = rtl_cdr(C->M, tail))
      {
	arg = rtl_macroExpand(C, ns, rtl_car(C->M, tail));
	out = rtl_cons(C->M, arg, out);
      }

    out = rtl_reverseListImproper(C->M, out, rtl_macroExpand(C, ns, tail));
    break;

  default:
    out = in;
    break;
  }

  rtl_popWorkingSet(C->M);

  return out;
}

rtl_Intrinsic *rtl_exprToIntrinsic(rtl_Compiler *C, rtl_Word sxp)
{
  rtl_Word head, tail;
  size_t len;
  rtl_Intrinsic **buf;
  size_t        bufLen;
  size_t        bufCap;

  rtl_Word *argNames;
  size_t   argNamesLen;
  size_t   argNamesCap;

  ensureSymCache();

  buf    = NULL;
  bufCap = bufLen = 0;

  argNames    = NULL;
  argNamesCap = argNamesLen = 0;

  if (rtl_isCons(sxp)) {
    head = rtl_car(C->M, sxp);
    len  = rtl_listLength(C->M, sxp);

    if (head == symCache.intrinsic.cons) {
      assert(len == 3);
      return rtl_mkConsIntrinsic(rtl_exprToIntrinsic(C, rtl_cadr(C->M, sxp)),
				 rtl_exprToIntrinsic(C, rtl_caddr(C->M, sxp)));

    } else if (head == symCache.intrinsic.car) {
      assert(len == 2);
      return rtl_mkCarIntrinsic(rtl_exprToIntrinsic(C, rtl_cadr(C->M, sxp)));

    } else if (head == symCache.intrinsic.cdr) {
      assert(len == 2);
      return rtl_mkCdrIntrinsic(rtl_exprToIntrinsic(C, rtl_cadr(C->M, sxp)));

    } else if (head == symCache.intrinsic.lambda) {
      assert(len >= 2);
      for (tail = rtl_cadr(C->M, sxp);
	   tail != RTL_NIL;
	   tail = rtl_cdr(C->M, tail))
	{
	  assert(rtl_isSymbol(rtl_car(C->M, tail)));

	  if (argNamesCap == argNamesLen) {
	    argNamesCap = !argNamesCap ? 4 : argNamesCap*2;
	    argNames    = realloc(argNames, sizeof(rtl_Word)*argNamesCap);
	  }

	  argNames[argNamesLen++] = rtl_car(C->M, tail);
	}

      for (tail = rtl_cddr(C->M, sxp);
	   tail != RTL_NIL;
	   tail = rtl_cdr(C->M, tail))
	{
	  if (bufCap == bufLen) {
	    bufCap = !bufCap ? 4 : 2*bufCap;
	    buf    = realloc(buf, sizeof(rtl_Intrinsic *)*bufCap);
	  }

	  buf[bufLen++] = rtl_exprToIntrinsic(C, rtl_car(C->M, tail));
	}

      return rtl_mkLambdaIntrinsic(argNames, argNamesLen, buf, bufLen);

    } else if (head == symCache.intrinsic.iadd) {
      assert(len == 3);
      return rtl_mkIAddIntrinsic(rtl_exprToIntrinsic(C, rtl_cadr(C->M, sxp)),
				 rtl_exprToIntrinsic(C, rtl_caddr(C->M, sxp)));
    } else if (head == symCache.intrinsic.isub) {
      assert(len == 3);
      return rtl_mkISubIntrinsic(rtl_exprToIntrinsic(C, rtl_cadr(C->M, sxp)),
				 rtl_exprToIntrinsic(C, rtl_caddr(C->M, sxp)));

    } else if (head == symCache.intrinsic.imul) {
      assert(len == 3);
      return rtl_mkIMulIntrinsic(rtl_exprToIntrinsic(C, rtl_cadr(C->M, sxp)),
				 rtl_exprToIntrinsic(C, rtl_caddr(C->M, sxp)));
      
    } else if (head == symCache.intrinsic.idiv) {
      assert(len == 3);
      return rtl_mkIDivIntrinsic(rtl_exprToIntrinsic(C, rtl_cadr(C->M, sxp)),
				 rtl_exprToIntrinsic(C, rtl_caddr(C->M, sxp)));
      
    } else if (head == symCache.intrinsic.imod) {
      assert(len == 3);
      return rtl_mkIModIntrinsic(rtl_exprToIntrinsic(C, rtl_cadr(C->M, sxp)),
				 rtl_exprToIntrinsic(C, rtl_caddr(C->M, sxp)));
      
    } else {
      assert(len >= 1);

      for (tail = rtl_cdr(C->M, sxp);
	   tail != RTL_NIL;
	   tail = rtl_cdr(C->M, tail))
      {
	if (bufCap == bufLen) {
	  bufCap = !bufCap ? 4 : 2*bufCap;
	  buf    = realloc(buf, sizeof(rtl_Intrinsic *)*bufCap);
	}

	buf[bufLen++] = rtl_exprToIntrinsic(C, rtl_car(C->M, tail));
      }

      return rtl_mkCallIntrinsic(rtl_exprToIntrinsic(C, rtl_car(C->M, sxp)),
				 buf,
				 bufLen);
  }

  } else if (rtl_isSymbol(sxp)) {
    return rtl_mkVarIntrinsic(sxp);
    
  } else if (rtl_isInt28(sxp)) {
    return rtl_mkConstantIntrinsic(sxp);

  } else if (rtl_isNil(sxp)) {
    return rtl_mkConstantIntrinsic(RTL_NIL);

  }
  printf("   !!! Unhandled intrinsic in rtl_exprToIntrinsic !!!\n");
  abort();
}

typedef struct Environment {
  struct Environment const *super;
  uint16_t frame;

  uint16_t len;
  rtl_Word *names;
} Environment;

// Returns true if there is a variable by this name in env, and writes its
// location into frame and idx.
//
// Either or both of frame/idx may be NULL, in which case the NULL pointers are
// not written to.
//
// Returns false if there is no variable by this name in scope.
static
bool lookupVar(Environment const *env,
	       rtl_Word name,
	       uint16_t *frame,
	       uint16_t *idx)
{
  uint16_t i;

  for (i = 0; i < env->len; i++) {
    if (env->names[i] == name) {
      if (frame) *frame = env->frame;
      if (idx)   *idx   = i;

      return true;
    }
  }

  if (env->super != NULL) {
    return lookupVar(env->super, name, frame, idx);
  } else {
    return false;
  }
}

static
rtl_Intrinsic *__impl_transformIntrinsic(Environment const *env, rtl_Intrinsic *x)
{
  rtl_Word      nameTmp;
  rtl_Intrinsic **argsTmp;
  size_t        argsLenTmp;
  Environment   newEnv;

  size_t i;

  switch (x->type) {
  case RTL_INTRINSIC_CONS:
    x->as.cons.car = __impl_transformIntrinsic(env, x->as.cons.car);
    x->as.cons.cdr = __impl_transformIntrinsic(env, x->as.cons.cdr);
    break;

  case RTL_INTRINSIC_CAR:
    x->as.car.arg = __impl_transformIntrinsic(env, x->as.car.arg);
    break;

  case RTL_INTRINSIC_CDR:
    x->as.cdr.arg = __impl_transformIntrinsic(env, x->as.cdr.arg);
    break;

  case RTL_INTRINSIC_VAR:
    if (!env ||
	!lookupVar(env, x->as.var.name, &x->as.var.frame, &x->as.var.idx))
    {
      printf("\n   error: No variable named '%s:%s' in scope!\n\n",
	     rtl_symbolPackageName(x->as.var.name),
	     rtl_symbolName(x->as.var.name));
      abort();
    }

    // If we didn't abort, then the correct frame/idx values were written by
    // lookupVar.
    break;

  case RTL_INTRINSIC_CALL:
    // Start by transforming all of the args
    for (i = 0; i < x->as.call.argsLen; i++) {
      x->as.call.args[i] = __impl_transformIntrinsic(env, x->as.call.args[i]);
    }

    // Then check if this is a named-call (i.e. a call of a global function
    // rather than a function resulting from the evaluation of an expression).
    if (x->as.call.fn->type == RTL_INTRINSIC_VAR &&
	(!env || !lookupVar(env, x->as.call.fn->as.var.name, NULL, NULL)))
    {
      nameTmp    = x->as.call.fn->as.var.name;
      argsTmp    = x->as.call.args;
      argsLenTmp = x->as.call.argsLen;

      // Release the VAR node, it's no longer needed.
      free(x->as.call.fn);

      *x = (rtl_Intrinsic) {
	.type = RTL_INTRINSIC_NAMED_CALL,
	.as = {
	  .namedCall = {
	    .name    = nameTmp,
	    .args    = argsTmp,
	    .argsLen = argsLenTmp,
	  },
	},
      };
    } else {
      x->as.call.fn = __impl_transformIntrinsic(env, x->as.call.fn);
    } break;

  case RTL_INTRINSIC_LAMBDA:
    newEnv.super = env;
    newEnv.frame = env ? env->frame + 1 : 0;
    newEnv.len   = x->as.lambda.argNamesLen;
    newEnv.names = x->as.lambda.argNames;

    for (i = 0; i < x->as.lambda.bodyLen; i++) {
      x->as.lambda.body[i] = __impl_transformIntrinsic(&newEnv,
						    x->as.lambda.body[i]);
    }
    break;

  case RTL_INTRINSIC_IADD:
    x->as.iadd.leftArg  = __impl_transformIntrinsic(env, x->as.iadd.leftArg);
    x->as.iadd.rightArg = __impl_transformIntrinsic(env, x->as.iadd.rightArg);
    break;

  case RTL_INTRINSIC_ISUB:
    x->as.isub.leftArg  = __impl_transformIntrinsic(env, x->as.isub.leftArg);
    x->as.isub.rightArg = __impl_transformIntrinsic(env, x->as.isub.rightArg);
    break;

  case RTL_INTRINSIC_IMUL:
    x->as.imul.leftArg  = __impl_transformIntrinsic(env, x->as.imul.leftArg);
    x->as.imul.rightArg = __impl_transformIntrinsic(env, x->as.imul.rightArg);
    break;

  case RTL_INTRINSIC_IDIV:
    x->as.idiv.leftArg  = __impl_transformIntrinsic(env, x->as.idiv.leftArg);
    x->as.idiv.rightArg = __impl_transformIntrinsic(env, x->as.idiv.rightArg);
    break;

  case RTL_INTRINSIC_IMOD:
    x->as.imod.leftArg  = __impl_transformIntrinsic(env, x->as.imod.leftArg);
    x->as.imod.rightArg = __impl_transformIntrinsic(env, x->as.imod.rightArg);
    break;

  case RTL_INTRINSIC_CONSTANT:
    break;
  }

  return x;
}

rtl_Intrinsic *rtl_transformIntrinsic(rtl_Intrinsic *x) {
  return __impl_transformIntrinsic(NULL, x);
}

void rtl_emitIntrinsicCode(rtl_Compiler *C,
			   uint16_t pageID,
			   rtl_Intrinsic const *x)
{
  size_t   i;
  uint16_t newPageID;

  switch (x->type) {
  case RTL_INTRINSIC_CONS:
    rtl_emitIntrinsicCode(C, pageID, x->as.cons.car);
    rtl_emitIntrinsicCode(C, pageID, x->as.cons.cdr);
    rtl_emitByteToPage(C->M, pageID, RTL_OP_CONS);
    break;

  case RTL_INTRINSIC_CAR:
    rtl_emitIntrinsicCode(C, pageID, x->as.car.arg);
    rtl_emitByteToPage(C->M, pageID, RTL_OP_CAR);
    break;

  case RTL_INTRINSIC_CDR:
    rtl_emitIntrinsicCode(C, pageID, x->as.cdr.arg);
    rtl_emitByteToPage(C->M, pageID, RTL_OP_CDR);
    break;

  case RTL_INTRINSIC_VAR:
    rtl_emitByteToPage(C->M, pageID, RTL_OP_VAR);
    rtl_emitShortToPage(C->M, pageID, x->as.var.frame);
    rtl_emitShortToPage(C->M, pageID, x->as.var.idx);
    break;

  case RTL_INTRINSIC_CALL:
    rtl_emitIntrinsicCode(C, pageID, x->as.call.fn);
    for (i = 0; i < x->as.call.argsLen; i++) {
      rtl_emitIntrinsicCode(C, pageID, x->as.call.args[i]);
    }

    rtl_emitByteToPage(C->M, pageID, RTL_OP_CALL);
    rtl_emitShortToPage(C->M, pageID, x->as.call.argsLen);
    break;

  case RTL_INTRINSIC_LAMBDA:
    // TODO: Prevent recompilation of the same function from creating the same
    // lambda over and over with a new page each time...

    newPageID = rtl_newPageID(C->M);

    rtl_emitByteToPage(C->M, pageID, RTL_OP_CLOSURE);
    rtl_emitWordToPage(C->M, pageID, rtl_addr(newPageID, 0));

    for (i = 0; i < x->as.lambda.bodyLen; i++) {
      rtl_emitIntrinsicCode(C, newPageID, x->as.lambda.body[i]);

      // Ignore the result of all but the last expression.
      if (i + 1 < x->as.lambda.bodyLen)
	rtl_emitByteToPage(C->M, newPageID, RTL_OP_POP);
    }

    rtl_emitByteToPage(C->M, newPageID, RTL_OP_RETURN);

    break;

  case RTL_INTRINSIC_CONSTANT:
    if (x->as.constant == RTL_NIL) {
      rtl_emitByteToPage(C->M, pageID, RTL_OP_CONST_NIL);

    } else if (x->as.constant == RTL_TOP) {
      rtl_emitByteToPage(C->M, pageID, RTL_OP_CONST_TOP);

    } else {
      rtl_emitByteToPage(C->M, pageID, RTL_OP_CONST);
      rtl_emitWordToPage(C->M, pageID, x->as.constant);

    } break;
  }
}
