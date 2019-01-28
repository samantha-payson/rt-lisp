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

      code[0] = RTL_OP_STATIC_CALL;
      code[1] = (fn >>  0) & 0xFF;
      code[2] = (fn >>  8) & 0xFF;
      code[3] = (fn >> 16) & 0xFF;
      code[4] = (fn >> 24) & 0xFF;
    }
  }

  csa->sitesLen = w;
}

void rtl_defineFn(rtl_Compiler *C, rtl_Word name, rtl_Word addr, bool isMacro)
{
  rtl_FnDef *def;
  size_t    idx;

  idx = rtl_symbolID(name) % RTL_COMPILER_FN_HASH_SIZE;

  if (!isMacro) {
    rtl_resolveCallSites(C, name, addr);
  }

  printf("warning: defining '%s:%s' as a %s\n",
	 rtl_symbolPackageName(name),
	 rtl_symbolName(name),
	 isMacro ? "macro" : "function");
  for (def = C->fnsByName[idx]; def != NULL; def = def->next) {
    if (def->name == name) {
      def->addr    = addr;

      if (isMacro && !def->isMacro) {
	printf("warning: redefining '%s:%s' as a macro (was a function)\n",
	       rtl_symbolPackageName(name),
	       rtl_symbolName(name));
      } else if (!isMacro && def->isMacro) {
	printf("warning: redefining '%s:%s' as a function (was a macro)\n",
	       rtl_symbolPackageName(name),
	       rtl_symbolName(name));
      }
      def->isMacro = isMacro;

      return;
    }
  }

  // If we made it here, there is no existing entry by this name... create one.

  def = malloc(sizeof(rtl_FnDef));

  def->name    = name;
  def->addr    = addr;
  def->isMacro = isMacro;

  def->next         = C->fnsByName[idx];
  C->fnsByName[idx] = def;
}

rtl_FnDef *rtl_lookupFn(rtl_Compiler *C, rtl_Word name)
{
  rtl_FnDef *def;
  size_t    idx;

  if (!rtl_isSymbol(name)) {
    return NULL;
  }

  idx = rtl_symbolID(name) % RTL_COMPILER_FN_HASH_SIZE;

  for (def = C->fnsByName[idx]; def != NULL; def = def->next)
  {
    if (def->name == name) {
      return def;
    }
  }

  return NULL;
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

  return rtl_symbol(id);
}

static struct symCache_t {
  struct {
    rtl_Word cons,
             car,
             cdr,
             var,
             call,
             namedCall,
             progn,
             lambda,
             defun, defmacro,
             quote,
             inPackage, usePackage, aliasPackage, alias, export,
             iadd, isub, imul, idiv, imod,
             lt, leq, gt, geq, eq, neq, iso, niso,
             _if;
  } intrinsic;
} symCache;

static bool symCacheWasInit;

static inline
void ensureSymCache(rtl_Compiler *C) {
  if (unlikely(!symCacheWasInit)) {
    symCache = (struct symCache_t) {
      .intrinsic = {
	.cons         = rtl_intern("intrinsic", "cons"),
	.car          = rtl_intern("intrinsic", "car"),
	.cdr          = rtl_intern("intrinsic", "cdr"),
	.var          = rtl_intern("intrinsic", "var"),
	.call         = rtl_intern("intrinsic", "call"),
	.namedCall    = rtl_intern("intrinsic", "named-call"),
	.progn        = rtl_intern("intrinsic", "progn"),
	.lambda       = rtl_intern("intrinsic", "lambda"),
	.defun        = rtl_intern("intrinsic", "defun"),
	.defmacro     = rtl_intern("intrinsic", "defmacro"),
	.quote        = rtl_intern("intrinsic", "quote"),
	.inPackage    = rtl_intern("intrinsic", "in-package"),
	.usePackage   = rtl_intern("intrinsic", "use-package"),
	.aliasPackage = rtl_intern("intrinsic", "alias-package"),
	.alias        = rtl_intern("intrinsic", "alias"),
	.export       = rtl_intern("intrinsic", "export"),
	.iadd         = rtl_intern("intrinsic", "iadd"),
	.isub         = rtl_intern("intrinsic", "isub"),
	.imul         = rtl_intern("intrinsic", "imul"),
	.idiv         = rtl_intern("intrinsic", "idiv"),
	.imod         = rtl_intern("intrinsic", "imod"),
	.lt           = rtl_intern("intrinsic", "lt"),
	.leq          = rtl_intern("intrinsic", "leq"),
	.gt           = rtl_intern("intrinsic", "gt"),
	.geq          = rtl_intern("intrinsic", "geq"),
	.eq           = rtl_intern("intrinsic", "eq"),
	.neq          = rtl_intern("intrinsic", "neq"),
	.iso          = rtl_intern("intrinsic", "iso"),
	.niso         = rtl_intern("intrinsic", "niso"),
	._if          = rtl_intern("intrinsic", "if"),
      },
    };
  }

  rtl_export(C, symCache.intrinsic.cons);
  rtl_export(C, symCache.intrinsic.car);
  rtl_export(C, symCache.intrinsic.cdr);
  rtl_export(C, symCache.intrinsic.car);
  rtl_export(C, symCache.intrinsic.call);
  rtl_export(C, symCache.intrinsic.namedCall);
  rtl_export(C, symCache.intrinsic.progn);
  rtl_export(C, symCache.intrinsic.lambda);
  rtl_export(C, symCache.intrinsic.defun);
  rtl_export(C, symCache.intrinsic.defmacro);
  rtl_export(C, symCache.intrinsic.quote);
  rtl_export(C, symCache.intrinsic.inPackage);
  rtl_export(C, symCache.intrinsic.usePackage);
  rtl_export(C, symCache.intrinsic.aliasPackage);
  rtl_export(C, symCache.intrinsic.alias);
  rtl_export(C, symCache.intrinsic.export);
  rtl_export(C, symCache.intrinsic.iadd);
  rtl_export(C, symCache.intrinsic.isub);
  rtl_export(C, symCache.intrinsic.imul);
  rtl_export(C, symCache.intrinsic.idiv);
  rtl_export(C, symCache.intrinsic.imod);
  rtl_export(C, symCache.intrinsic.lt);
  rtl_export(C, symCache.intrinsic.leq);
  rtl_export(C, symCache.intrinsic.gt);
  rtl_export(C, symCache.intrinsic.geq);
  rtl_export(C, symCache.intrinsic.eq);
  rtl_export(C, symCache.intrinsic.neq);
  rtl_export(C, symCache.intrinsic.iso);
  rtl_export(C, symCache.intrinsic.niso);
}

void rtl_initCompiler(rtl_Compiler *C, rtl_Machine *M) {
  C->error = (rtl_CompilerError) {
    .type = RTL_COMPILER_OK,
  };

  C->M = M;

  memset(C->callSitesByName, 0, sizeof C->callSitesByName);
  memset(C->fnsByName,       0, sizeof C->fnsByName);
  memset(C->pkgByID,         0, sizeof C->pkgByID);
}

// In the future, this will check if w is the name of a macro... right now it
// just returns false 100% of the time.
bool rtl_isMacroName(rtl_Compiler *C, rtl_Word w) {
  return false;
}

rtl_Word rtl_macroExpand(rtl_Compiler *C, rtl_NameSpace const *ns, rtl_Word in)
{
  rtl_Word head  = RTL_NIL,
           arg   = RTL_NIL,
           name  = RTL_NIL,
           alias = RTL_NIL,
           tail  = RTL_NIL,
           out   = RTL_NIL;

  rtl_FnDef *fnDef;

  rtl_NameSpace newNS;

  uint32_t    pkgID;
  rtl_Package *pkg;

  ensureSymCache(C);

  RTL_PUSH_WORKING_SET(C->M, &in, &head, &arg, &name, &tail, &out);

  switch (rtl_typeOf(in)) {
  case RTL_UNRESOLVED_SYMBOL:
    out = rtl_resolveSymbol(C, ns, rtl_symbolID(in));
    break;

  case RTL_CONS:
    head = rtl_macroExpand(C, ns, rtl_car(C->M, in));

    if (head == symCache.intrinsic.inPackage) {
      name  = rtl_macroExpand(C, ns, rtl_cadr(C->M, in));
      newNS = rtl_nsInPackage(ns, rtl_internPackage(C, rtl_symbolName(name)));
      out   = rtl_macroExpand(C, &newNS,
			      rtl_cons(C->M,
				       rtl_intern("intrinsic", "progn"),
				       rtl_cddr(C->M, in)));
      break;

    } else if (head == symCache.intrinsic.usePackage) {
      name  = rtl_macroExpand(C, ns, rtl_cadr(C->M, in));
      newNS = rtl_nsUsePackage(ns, rtl_internPackage(C, rtl_symbolName(name)));
      out   = rtl_macroExpand(C, &newNS,
			      rtl_cons(C->M,
				       rtl_intern("intrinsic", "progn"),
				       rtl_cddr(C->M, in)));
      break;

    } else if (head == symCache.intrinsic.aliasPackage) {
      name  = rtl_macroExpand(C, ns, rtl_car(C->M, rtl_cadr(C->M, in)));
      alias = rtl_macroExpand(C, ns, rtl_cadr(C->M, rtl_cadr(C->M, in)));
      newNS = rtl_nsAliasPackage(ns, rtl_internPackage(C, rtl_symbolName(name)),
				 rtl_symbolName(alias));
      out   = rtl_macroExpand(C, &newNS,
			      rtl_cons(C->M,
				       rtl_intern("intrinsic", "progn"),
				       rtl_cddr(C->M, in)));
      break;

    } else if (head == symCache.intrinsic.alias) {
      break;

    }

    fnDef = rtl_lookupFn(C, head);
    if (fnDef != NULL && fnDef->isMacro) {
      out = rtl_macroExpand(C, ns, rtl_applyList(C->M,
						 fnDef->addr,
						 rtl_cdr(C->M, in)));
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
  rtl_Word head, tail, name, _else;
  size_t len;
  rtl_Intrinsic **buf;
  size_t        bufLen;
  size_t        bufCap;

  rtl_Word *argNames;
  size_t   argNamesLen;
  size_t   argNamesCap;

  bool hasRestArg;

  ensureSymCache(C);

  buf    = NULL;
  bufCap = bufLen = 0;

  argNames    = NULL;
  argNamesCap = argNamesLen = 0;

  hasRestArg = false;

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

    } else if (head == symCache.intrinsic.progn) {
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

      return rtl_mkPrognIntrinsic(buf, bufLen);

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

    } else if (head == symCache.intrinsic.defun ||
	       head == symCache.intrinsic.defmacro) {
      assert(len >= 3);

      name = rtl_cadr(C->M, sxp);

      for (tail =  rtl_caddr(C->M, sxp);
	   rtl_isCons(tail);
	   tail =  rtl_cdr(C->M, tail))
      {
	assert(rtl_isSymbol(rtl_car(C->M, tail)));

	if (argNamesCap == argNamesLen) {
	  argNamesCap = !argNamesCap ? 4 : argNamesCap*2;
	  argNames    = realloc(argNames, sizeof(rtl_Word)*argNamesCap);
	}

	argNames[argNamesLen++] = rtl_car(C->M, tail);
      }

      if (!rtl_isNil(tail)) {
	assert(rtl_isSymbol(tail));

	if (argNamesCap == argNamesLen) {
	  argNamesCap = !argNamesCap ? 4 : argNamesCap*2;
	  argNames    = realloc(argNames, sizeof(rtl_Word)*argNamesCap);
	}

	argNames[argNamesLen++] = tail;
	hasRestArg = true;
      }

      for (tail = rtl_cdddr(C->M, sxp);
	   tail != RTL_NIL;
	   tail = rtl_cdr(C->M, tail))
      {
	if (bufCap == bufLen) {
	  bufCap = !bufCap ? 4 : 2*bufCap;
	  buf    = realloc(buf, sizeof(rtl_Intrinsic *)*bufCap);
	}

	buf[bufLen++] = rtl_exprToIntrinsic(C, rtl_car(C->M, tail));
      }

      if (head == symCache.intrinsic.defun) {
	return rtl_mkDefunIntrinsic(name,
				    argNames,
				    argNamesLen,
				    hasRestArg,
				    buf,
				    bufLen);
      } else {
	return rtl_mkDefmacroIntrinsic(name,
				       argNames,
				       argNamesLen,
				       hasRestArg,
				       buf,
				       bufLen);
      }
    } else if (head == symCache.intrinsic.quote) {
      assert(len == 2);
      return rtl_mkQuoteIntrinsic(rtl_cadr(C->M, sxp));

    } else if (head == symCache.intrinsic.iadd) {
      assert(len == 3);
      return rtl_mkBinopIntrinsic(RTL_INTRINSIC_IADD,
				  rtl_exprToIntrinsic(C, rtl_cadr(C->M, sxp)),
				  rtl_exprToIntrinsic(C, rtl_caddr(C->M, sxp)));

    } else if (head == symCache.intrinsic.isub) {
      assert(len == 3);
      return rtl_mkBinopIntrinsic(RTL_INTRINSIC_ISUB,
				  rtl_exprToIntrinsic(C, rtl_cadr(C->M, sxp)),
				  rtl_exprToIntrinsic(C, rtl_caddr(C->M, sxp)));

    } else if (head == symCache.intrinsic.imul) {
      assert(len == 3);
      return rtl_mkBinopIntrinsic(RTL_INTRINSIC_IMUL,
				  rtl_exprToIntrinsic(C, rtl_cadr(C->M, sxp)),
				  rtl_exprToIntrinsic(C, rtl_caddr(C->M, sxp)));
      
    } else if (head == symCache.intrinsic.idiv) {
      assert(len == 3);
      return rtl_mkBinopIntrinsic(RTL_INTRINSIC_IDIV,
				  rtl_exprToIntrinsic(C, rtl_cadr(C->M, sxp)),
				  rtl_exprToIntrinsic(C, rtl_caddr(C->M, sxp)));
      
    } else if (head == symCache.intrinsic.imod) {
      assert(len == 3);
      return rtl_mkBinopIntrinsic(RTL_INTRINSIC_IMOD,
				  rtl_exprToIntrinsic(C, rtl_cadr(C->M, sxp)),
				  rtl_exprToIntrinsic(C, rtl_caddr(C->M, sxp)));
      
    } else if (head == symCache.intrinsic.lt) {
      assert(len == 3);
      return rtl_mkBinopIntrinsic(RTL_INTRINSIC_LT,
				  rtl_exprToIntrinsic(C, rtl_cadr(C->M, sxp)),
				  rtl_exprToIntrinsic(C, rtl_caddr(C->M, sxp)));
      
    } else if (head == symCache.intrinsic.leq) {
      assert(len == 3);
      return rtl_mkBinopIntrinsic(RTL_INTRINSIC_LEQ,
				  rtl_exprToIntrinsic(C, rtl_cadr(C->M, sxp)),
				  rtl_exprToIntrinsic(C, rtl_caddr(C->M, sxp)));
      
    } else if (head == symCache.intrinsic.gt) {
      assert(len == 3);
      return rtl_mkBinopIntrinsic(RTL_INTRINSIC_GT,
				  rtl_exprToIntrinsic(C, rtl_cadr(C->M, sxp)),
				  rtl_exprToIntrinsic(C, rtl_caddr(C->M, sxp)));
      
    } else if (head == symCache.intrinsic.geq) {
      assert(len == 3);
      return rtl_mkBinopIntrinsic(RTL_INTRINSIC_GEQ,
				  rtl_exprToIntrinsic(C, rtl_cadr(C->M, sxp)),
				  rtl_exprToIntrinsic(C, rtl_caddr(C->M, sxp)));
      
    } else if (head == symCache.intrinsic.eq) {
      assert(len == 3);
      return rtl_mkBinopIntrinsic(RTL_INTRINSIC_EQ,
				  rtl_exprToIntrinsic(C, rtl_cadr(C->M, sxp)),
				  rtl_exprToIntrinsic(C, rtl_caddr(C->M, sxp)));
      
    } else if (head == symCache.intrinsic.neq) {
      assert(len == 3);
      return rtl_mkBinopIntrinsic(RTL_INTRINSIC_NEQ,
				  rtl_exprToIntrinsic(C, rtl_cadr(C->M, sxp)),
				  rtl_exprToIntrinsic(C, rtl_caddr(C->M, sxp)));
      
    } else if (head == symCache.intrinsic.iso) {
      assert(len == 3);
      return rtl_mkBinopIntrinsic(RTL_INTRINSIC_ISO,
				  rtl_exprToIntrinsic(C, rtl_cadr(C->M, sxp)),
				  rtl_exprToIntrinsic(C, rtl_caddr(C->M, sxp)));
      
    } else if (head == symCache.intrinsic._if) {
      assert(len == 3 || len == 4);

      return rtl_mkIfIntrinsic(rtl_exprToIntrinsic(C, rtl_cadr(C->M, sxp)),
			       rtl_exprToIntrinsic(C, rtl_caddr(C->M, sxp)),
			       rtl_exprToIntrinsic(C, rtl_car(C->M, rtl_cdddr(C->M, sxp))));

    } else if (head == symCache.intrinsic.lt) {
      

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

  } else if (rtl_isTop(sxp)) {
    return rtl_mkConstantIntrinsic(RTL_TOP);

  }
  printf("   !!! Unhandled intrinsic in rtl_exprToIntrinsic !!!\n   expr:");
  rtl_formatExpr(C->M, sxp);
  printf("\n");
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

  case RTL_INTRINSIC_PROGN:
    for (i = 0; i < x->as.progn.formsLen; i++) {
      x->as.progn.forms[i] = __impl_transformIntrinsic(env, x->as.progn.forms[i]);
    }

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

  case RTL_INTRINSIC_DEFUN:
    newEnv.super = env;
    newEnv.frame = env ? env->frame + 1 : 0;
    newEnv.len   = x->as.defun.argNamesLen;
    newEnv.names = x->as.defun.argNames;

    for (i = 0; i < x->as.defun.bodyLen; i++) {
      x->as.defun.body[i] = __impl_transformIntrinsic(&newEnv,
						      x->as.defun.body[i]);
    }
    break;

  case RTL_INTRINSIC_DEFMACRO:
    newEnv.super = env;
    newEnv.frame = env ? env->frame + 1 : 0;
    newEnv.len   = x->as.defmacro.argNamesLen;
    newEnv.names = x->as.defmacro.argNames;

    for (i = 0; i < x->as.defmacro.bodyLen; i++) {
      x->as.defmacro.body[i] = __impl_transformIntrinsic(&newEnv,
							 x->as.defmacro.body[i]);
    }
    break;

  case RTL_INTRINSIC_IADD:
  case RTL_INTRINSIC_ISUB:
  case RTL_INTRINSIC_IMUL:
  case RTL_INTRINSIC_IDIV:
  case RTL_INTRINSIC_IMOD:
  case RTL_INTRINSIC_LT:
  case RTL_INTRINSIC_LEQ:
  case RTL_INTRINSIC_GT:
  case RTL_INTRINSIC_GEQ:
  case RTL_INTRINSIC_EQ:
  case RTL_INTRINSIC_NEQ:
  case RTL_INTRINSIC_ISO:
    x->as.binop.leftArg  = __impl_transformIntrinsic(env, x->as.binop.leftArg);
    x->as.binop.rightArg = __impl_transformIntrinsic(env, x->as.binop.rightArg);
    break;

  case RTL_INTRINSIC_IF:
    x->as._if.test  = __impl_transformIntrinsic(env, x->as._if.test);
    x->as._if.then  = __impl_transformIntrinsic(env, x->as._if.then);
    x->as._if._else = __impl_transformIntrinsic(env, x->as._if._else);
    break;

  case RTL_INTRINSIC_QUOTE:
  case RTL_INTRINSIC_CONSTANT:
    break;
  }

  return x;
}

rtl_Intrinsic *rtl_transformIntrinsic(rtl_Intrinsic *x) {
  return __impl_transformIntrinsic(NULL, x);
}

static
void writeWordAtAddr(rtl_Machine *M, rtl_Word addr, rtl_Word w)
{
  uint8_t *ptr;
  ptr = rtl_resolveAddr(M, addr);

  ptr[0] = (w >>  0) & 0xFF;
  ptr[1] = (w >>  8) & 0xFF;
  ptr[2] = (w >> 16) & 0xFF;
  ptr[3] = (w >> 24) & 0xFF;
}

static
void emitQuoteCode(rtl_Compiler *C, uint16_t pageID, rtl_Word expr)
{
  switch (rtl_typeOf(expr)) {
  case RTL_NIL:
    rtl_emitByteToPage(C->M, pageID, RTL_OP_CONST_NIL);
    break;

  // TODO: Should this exist at all?
  case RTL_UNRESOLVED_SYMBOL:

  case RTL_SYMBOL:
  case RTL_SELECTOR:
  case RTL_INT28:
  case RTL_FIX14:
    rtl_emitByteToPage(C->M, pageID, RTL_OP_CONST);
    rtl_emitWordToPage(C->M, pageID, expr);
    break;


  case RTL_TOP:
    rtl_emitByteToPage(C->M, pageID, RTL_OP_CONST_TOP);
    break;

  case RTL_CONS:
    emitQuoteCode(C, pageID, rtl_car(C->M, expr));
    emitQuoteCode(C, pageID, rtl_cdr(C->M, expr));
    rtl_emitByteToPage(C->M, pageID, RTL_OP_CONS);
    break;

  default:
    printf("Can't quote object of type '%s' ... yet?\n",
	   rtl_typeNameOf(expr));
    break;
  }
}

void rtl_emitIntrinsicCode(rtl_Compiler *C,
			   uint16_t pageID,
			   rtl_Intrinsic const *x)
{
  size_t    i;
  uint16_t  newPageID;
  rtl_Word  addr0, addr1;
  rtl_FnDef *fnDef;
  uint8_t   *ptr;

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

  case RTL_INTRINSIC_NAMED_CALL:
    for (i = 0; i < x->as.namedCall.argsLen; i++) {
      rtl_emitIntrinsicCode(C, pageID, x->as.namedCall.args[i]);
    }

    fnDef = rtl_lookupFn(C, x->as.namedCall.name);
    printf("fnDef: %p\n", fnDef);
    if (fnDef != NULL && !fnDef->isMacro) {
      addr0 = rtl_emitByteToPage(C->M, pageID, RTL_OP_STATIC_CALL);

      rtl_emitWordToPage(C->M, pageID, fnDef->addr);
      rtl_emitShortToPage(C->M, pageID, x->as.namedCall.argsLen);
    } else {
      addr0 = rtl_emitByteToPage(C->M, pageID, RTL_OP_UNDEFINED_FUNCTION);

      rtl_emitWordToPage(C->M, pageID, x->as.namedCall.name);
      rtl_emitShortToPage(C->M, pageID, x->as.namedCall.argsLen);
    }

    rtl_registerCallSite(C, x->as.namedCall.name, addr0);
    break;

  case RTL_INTRINSIC_PROGN:
    if (x->as.progn.formsLen == 0) {
      rtl_emitByteToPage(C->M, pageID, RTL_OP_CONST_NIL);
    } else {
      for (i = 0; i < x->as.progn.formsLen; i++) {
	rtl_emitIntrinsicCode(C, pageID, x->as.progn.forms[i]);

	// Ignore the result of all but the last expression.
	if (i + 1 < x->as.progn.formsLen)
	  rtl_emitByteToPage(C->M, newPageID, RTL_OP_POP);
      }
    } break;

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

  case RTL_INTRINSIC_DEFUN:
    newPageID = rtl_newPageID(C->M);

    if (x->as.defun.hasRestArg) {
      rtl_emitByteToPage(C->M, newPageID, RTL_OP_REST);
      rtl_emitShortToPage(C->M, newPageID, x->as.defun.argNamesLen - 1);
    }

    // TODO: We're not using the argnames at all.. we should probably check
    // arity when resolving call sites.

    for (i = 0; i < x->as.defun.bodyLen; i++) {
      rtl_emitIntrinsicCode(C, newPageID, x->as.defun.body[i]);

      // Ignore the result of all but the last expression.
      if (i + 1 < x->as.defun.bodyLen)
	rtl_emitByteToPage(C->M, newPageID, RTL_OP_POP);
    }

    rtl_emitByteToPage(C->M, newPageID, RTL_OP_RETURN);

    rtl_defineFn(C, x->as.defun.name, rtl_addr(newPageID, 0), false);
    break;

  case RTL_INTRINSIC_DEFMACRO:
    newPageID = rtl_newPageID(C->M);

    if (x->as.defmacro.hasRestArg) {
      rtl_emitByteToPage(C->M, newPageID, RTL_OP_REST);
      rtl_emitShortToPage(C->M, newPageID, x->as.defmacro.argNamesLen - 1);
    }

    // TODO: We're not using the argnames at all.. we should probably check
    // arity when resolving call sites.

    for (i = 0; i < x->as.defun.bodyLen; i++) {
      rtl_emitIntrinsicCode(C, newPageID, x->as.defun.body[i]);

      // Ignore the result of all but the last expression.
      if (i + 1 < x->as.defun.bodyLen)
	rtl_emitByteToPage(C->M, newPageID, RTL_OP_POP);
    }

    rtl_emitByteToPage(C->M, newPageID, RTL_OP_RETURN);

    rtl_defineFn(C, x->as.defun.name, rtl_addr(newPageID, 0), true);
    break;

  case RTL_INTRINSIC_QUOTE:
    emitQuoteCode(C, pageID, x->as.quote);
    break;

  case RTL_INTRINSIC_IADD:
  case RTL_INTRINSIC_ISUB:
  case RTL_INTRINSIC_IMUL:
  case RTL_INTRINSIC_IDIV:
  case RTL_INTRINSIC_IMOD:
  case RTL_INTRINSIC_LT:
  case RTL_INTRINSIC_LEQ:
  case RTL_INTRINSIC_GT:
  case RTL_INTRINSIC_GEQ:
  case RTL_INTRINSIC_EQ:
  case RTL_INTRINSIC_NEQ:
  case RTL_INTRINSIC_ISO:
    rtl_emitIntrinsicCode(C, pageID, x->as.binop.leftArg);
    rtl_emitIntrinsicCode(C, pageID, x->as.binop.rightArg);

    switch (x->type) {
    case RTL_INTRINSIC_IADD:
      rtl_emitByteToPage(C->M, pageID, RTL_OP_IADD);
      break;

    case RTL_INTRINSIC_ISUB:
      rtl_emitByteToPage(C->M, pageID, RTL_OP_ISUB);
      break;

    case RTL_INTRINSIC_IMUL:
      rtl_emitByteToPage(C->M, pageID, RTL_OP_IMUL);
      break;

    case RTL_INTRINSIC_IDIV:
      rtl_emitByteToPage(C->M, pageID, RTL_OP_IDIV);
      break;

    case RTL_INTRINSIC_IMOD:
      rtl_emitByteToPage(C->M, pageID, RTL_OP_IMOD);
      break;

    case RTL_INTRINSIC_LT:
      rtl_emitByteToPage(C->M, pageID, RTL_OP_LT);
      break;

    case RTL_INTRINSIC_LEQ:
      rtl_emitByteToPage(C->M, pageID, RTL_OP_LEQ);
      break;

    case RTL_INTRINSIC_GT:
      rtl_emitByteToPage(C->M, pageID, RTL_OP_GT);
      break;

    case RTL_INTRINSIC_GEQ:
      rtl_emitByteToPage(C->M, pageID, RTL_OP_GEQ);
      break;

    case RTL_INTRINSIC_EQ:
      rtl_emitByteToPage(C->M, pageID, RTL_OP_EQ);
      break;

    case RTL_INTRINSIC_NEQ:
      rtl_emitByteToPage(C->M, pageID, RTL_OP_NEQ);
      break;

    case RTL_INTRINSIC_ISO:
      rtl_emitByteToPage(C->M, pageID, RTL_OP_ISO);
      break;

    } break;

  case RTL_INTRINSIC_IF:
    rtl_emitIntrinsicCode(C, pageID, x->as._if.test);
    rtl_emitByteToPage(C->M, pageID, RTL_OP_NOT);
    rtl_emitByteToPage(C->M, pageID, RTL_OP_CJMP);
    addr0 = rtl_emitWordToPage(C->M, pageID, 0);

    rtl_emitIntrinsicCode(C, pageID, x->as._if.then);

    rtl_emitByteToPage(C->M, pageID, RTL_OP_JMP);
    addr1 = rtl_emitWordToPage(C->M, pageID, 0);

    writeWordAtAddr(C->M, addr0, rtl_nextAddrInPage(C->M, pageID));

    rtl_emitIntrinsicCode(C, pageID, x->as._if._else);

    writeWordAtAddr(C->M, addr1, rtl_nextAddrInPage(C->M, pageID));
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
