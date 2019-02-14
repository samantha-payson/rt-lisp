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

void rtl_registerCallSite(rtl_Compiler *C,
			  rtl_Word     name,
			  rtl_Word     fn,
			  uint32_t     offs)
{
  rtl_CallSiteArray *csa;
  uint16_t          fnID;

  csa = getCallSiteArray(C, name);

  if (csa->sitesLen == csa->sitesCap) {
    csa->sitesCap = !csa->sitesCap ? 16 : 2*csa->sitesCap;
    csa->sites    = realloc(csa->sites, csa->sitesCap*sizeof(rtl_CallSite));
  }

  fnID = rtl_functionID(fn);

  csa->sites[csa->sitesLen++] = (rtl_CallSite) {
    .version = C->M->codeBase->fns[fnID]->version,
    .fnID    = fnID,
    .offs    = offs,
  };
}

void rtl_resolveCallSites(rtl_Compiler *C, rtl_Word name, rtl_Word fn)
{
  rtl_CallSiteArray *csa;
  size_t            r, w;
  rtl_CallSite      *site;
  rtl_Function      *func;
  uint8_t           *code;

  csa = getCallSiteArray(C, name);
  for (r = w = 0; r < csa->sitesLen; r++) {
    site = csa->sites + r;
    func = C->M->codeBase->fns[site->fnID];

    // Can't resolve a call site within a builtin...
    assert(!func->isBuiltin);

    if (site->version == func->version) {
      code = func->as.lisp.code + site->offs;
      csa->sites[w++] = csa->sites[r];

      switch (code[0]) {
      case RTL_OP_UNDEFINED_CALL:
      case RTL_OP_STATIC_CALL:
	code[0] = RTL_OP_STATIC_CALL;
	code[1] = (fn >>  0) & 0xFF;
	code[2] = (fn >>  8) & 0xFF;
	code[3] = (fn >> 16) & 0xFF;
	code[4] = (fn >> 24) & 0xFF;
	break;

      case RTL_OP_UNDEFINED_TAIL:
      case RTL_OP_STATIC_TAIL:
	code[0] = RTL_OP_STATIC_TAIL;
	code[1] = (fn >>  0) & 0xFF;
	code[2] = (fn >>  8) & 0xFF;
	code[3] = (fn >> 16) & 0xFF;
	code[4] = (fn >> 24) & 0xFF;
	break;

      case RTL_OP_UNDEFINED_VAR:
      case RTL_OP_CONST:
	code[0] = RTL_OP_CONST;
	code[1] = (fn >>  0) & 0xFF;
	code[2] = (fn >>  8) & 0xFF;
	code[3] = (fn >> 16) & 0xFF;
	code[4] = (fn >> 24) & 0xFF;
	break;

      default:
	printf("   !!! Invalid call site !!!\n");
	abort();
      }
    }
  }

  csa->sitesLen = w;
}

void rtl_defineFn(rtl_Compiler *C, rtl_Word name, rtl_Word fn, bool isMacro)
{
  rtl_FnDef    *def;
  size_t       idx;
  rtl_CodeBase *codeBase = C->M->codeBase;

  idx = rtl_symbolID(name) % RTL_CODE_BASE_FN_HASH_SIZE;

  if (!isMacro) {
    rtl_resolveCallSites(C, name, fn);
  }

  for (def = codeBase->fnsByName[idx]; def != NULL; def = def->next) {
    if (def->name == name) {
      def->fn = fn;

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
  def->fn      = fn;
  def->isMacro = isMacro;

  def->next                = codeBase->fnsByName[idx];
  codeBase->fnsByName[idx] = def;
}

rtl_FnDef *rtl_lookupFn(rtl_Compiler *C, rtl_Word name)
{
  rtl_FnDef *def;
  size_t    idx;

  if (!rtl_isSymbol(name)) {
    return NULL;
  }

  idx = rtl_symbolID(name) % RTL_CODE_BASE_FN_HASH_SIZE;

  for (def = C->M->codeBase->fnsByName[idx]; def != NULL; def = def->next)
  {
    if (def->name == name) {
      return def;
    }
  }

  return NULL;
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

rtl_Word rtl_internSelector(char const *pkg, char const *name)
{
  uint32_t pkgID, id;

  pkgID = rtl_internPackageID(pkg ? pkg : "");
  id    = rtl_internSymbolID(pkgID, name);

  return rtl_selector(id);
}

#define MAP_INTRINSICS(M)			\
  M(cons,         "cons")			\
  M(car,          "car")			\
  M(cdr,          "cdr")			\
  M(tuple,        "tuple")			\
  M(len,          "len")			\
  M(get,          "get")			\
  M(map,          "map")			\
  M(insert,       "insert")			\
  M(lookup,       "lookup")			\
  M(var,          "var")			\
  M(gensym,       "gensym")			\
  M(call,         "call")			\
  M(namedCall,    "named-call")			\
  M(applyList,    "apply-list")			\
  M(applyTuple,   "apply-tuple")		\
  M(progn,        "progn")			\
  M(lambda,       "lambda")			\
  M(labels,       "labels")			\
  M(defun,        "defun")			\
  M(defmacro,     "defmacro")			\
  M(load,         "load")			\
  M(quote,        "quote")			\
  M(inPackage,    "in-package")			\
  M(usePackage,   "use-package")		\
  M(aliasPackage, "alias-package")		\
  M(alias,        "alias")			\
  M(export,       "export")			\
  M(iadd,         "iadd")			\
  M(isub,         "isub")			\
  M(imul,         "imul")			\
  M(idiv,         "idiv")			\
  M(imod,         "imod")			\
  M(lt,           "lt")				\
  M(leq,          "leq")			\
  M(gt,           "gt")				\
  M(geq,          "geq")			\
  M(eq,           "eq")				\
  M(neq,          "neq")			\
  M(iso,          "iso")			\
  M(nilp,         "nil?")			\
  M(symbolp,      "symbol?")			\
  M(selectorp,    "selector?")			\
  M(int28p,       "int28?")			\
  M(fix14p,       "fix14?")			\
  M(tuplep,       "tuple?")			\
  M(stringp,      "string?")			\
  M(mapp,         "map?")			\
  M(consp,        "cons?")			\
  M(functionp,    "function?")			\
  M(closurep,     "closure?")			\
  M(unresolvedp,  "unresolved?")		\
  M(topp,         "top?")			\
  M(_if,          "if")				\
  // End of multi-line macro

#define DECLARE_INTRINSIC_WORD(CNAME, LISPNAME)	CNAME,

static struct symCache_t {
  struct {
    rtl_Word MAP_INTRINSICS(DECLARE_INTRINSIC_WORD)
      ___ignore___; // Need an identifier here so the trailing comma isn't a
                    // problem...
  } intrinsic;
} symCache;

static bool symCacheWasInit;

#define CACHE_INTRINSIC_SYM(CNAME, LISPNAME)	\
  .CNAME = rtl_intern("intrinsic", LISPNAME),	\
  // End of multi-line macro

#define EXPORT_INTRINSIC(CNAME, LISPNAME)	\
  rtl_export(C, symCache.intrinsic.CNAME);	\
  // End of multi-line macro

static inline
void ensureSymCache(rtl_Compiler *C) {
  if (unlikely(!symCacheWasInit)) {
    symCacheWasInit = true;

    symCache = (struct symCache_t) {
      .intrinsic = {
	MAP_INTRINSICS(CACHE_INTRINSIC_SYM)
      },
    };
  }

  MAP_INTRINSICS(EXPORT_INTRINSIC)
}

void rtl_initCompiler(rtl_Compiler *C, rtl_Machine *M) {
  C->error = (rtl_CompilerError) {
    .type = RTL_COMPILER_OK,
  };

  C->M = M;

  memset(C->callSitesByName, 0, sizeof C->callSitesByName);
  memset(C->pkgByID,         0, sizeof C->pkgByID);
}

rtl_Word rtl_resolveAll(rtl_Compiler *C,
			rtl_NameSpace const *ns,
			rtl_Word sxp);

rtl_Word rtl_resolveMap(rtl_Compiler *C,
			rtl_NameSpace const *ns,
			uint32_t mask,
			rtl_Word map)
{
  rtl_Word const *rptr, *entry;
  size_t   len,
           i;

  rtl_Word *wptr, *newEntry;

  rtl_Word newMap = RTL_NIL;

  RTL_PUSH_WORKING_SET(C->M, &map, &newMap);

  len  = __builtin_popcount(mask);

  wptr = rtl_allocGC(C->M, RTL_MAP, &newMap, 2*len);
  rptr = __rtl_reifyPtr(C->M, map);

  memcpy(wptr, rptr, sizeof(rtl_Word)*2*len);

  for (i = 0; i < len; i++) {
    entry    = rptr + 2*i;
    newEntry = wptr + 2*i;

    if (rtl_isHeader(entry[0])) {
      newEntry[1] = rtl_resolveMap(C, ns, rtl_headerValue(entry[0]), entry[1]);
      newEntry[0] = rtl_resolveAll(C, ns, entry[0]);
    } else {
      newEntry[0] = rtl_resolveAll(C, ns, entry[0]);
      newEntry[1] = rtl_resolveAll(C, ns, entry[1]);
    }
  }

  return newMap;
}

rtl_Word rtl_resolveAll(rtl_Compiler *C,
			rtl_NameSpace const *ns,
			rtl_Word in)
{
  rtl_Word out = RTL_NIL;

  rtl_Word const *rptr;
  rtl_Word *wptr;
  size_t i, len;

  RTL_PUSH_WORKING_SET(C->M, &in, &out);

  switch (rtl_typeOf(in)) {
  case RTL_UNRESOLVED_SYMBOL:
    out = rtl_resolveSymbol(C, ns, rtl_symbolID(in));
    break;

  case RTL_UNRESOLVED_SELECTOR:
    out = rtl_resolveSelector(C, ns, rtl_selectorID(in));
    break;

  case RTL_MAP:
    if (rtl_isEmptyMap(in)) {
      out = in;
    } else {
      out = rtl_resolveMap(C, ns, 1, in);
    } break;

  case RTL_TUPLE:
    rptr = rtl_reifyTuple(C->M, in, &len);
    wptr = rtl_allocTuple(C->M, &out, len);
    rptr = rtl_reifyTuple(C->M, in, &len);
    for (i = 0; i < len; i++) {
      wptr[i] = rtl_resolveAll(C, ns, rptr[i]);
    } break;
    
  case RTL_CONS:
    out = rtl_cons(C->M, rtl_resolveAll(C, ns, rtl_car(C->M, in)),
		   rtl_resolveAll(C, ns, rtl_cdr(C->M, in)));
    break;

  default:
    out = in;
    break;
  }

  rtl_popWorkingSet(C->M);

  return out;
}

rtl_Word macroExpandMap(rtl_Compiler        *C,
			rtl_NameSpace const *ns,
			rtl_Word            map,
			uint32_t            mask)
{
  rtl_Word const *rptr, *entry;
  size_t   len,
           i;

  rtl_Word *wptr, *newEntry;

  rtl_Word newMap = RTL_NIL;

  RTL_PUSH_WORKING_SET(C->M, &map, &newMap);

  len  = __builtin_popcount(mask);

  wptr = rtl_allocGC(C->M, RTL_MAP, &newMap, 2*len);
  rptr = __rtl_reifyPtr(C->M, map);

  memset(wptr, 0, sizeof(rtl_Word)*2*len);

  for (i = 0; i < len; i++) {
    entry    = rptr + 2*i;
    newEntry = wptr + 2*i;

    if (rtl_isHeader(entry[0])) {
      newEntry[1] = macroExpandMap(C, ns, entry[1], rtl_headerValue(entry[0]));
      newEntry[0] = entry[0];
    } else {
      newEntry[0] = rtl_macroExpand(C, ns, entry[0]);
      newEntry[1] = rtl_macroExpand(C, ns, entry[1]);
    }
  }

  return newMap;
}

// Expects everything after the 'lambda' symbol of a lambda expression.
//
// e.g. ((arg0 arg1 arg2) body)
rtl_Word rtl_macroExpandLambda(rtl_Compiler *C, rtl_NameSpace const *ns, rtl_Word in)
{
  rtl_Word arg, out, tail;

  arg = out = tail = RTL_NIL;

  RTL_PUSH_WORKING_SET(C->M, &in, &arg, &out, &tail);

  arg = rtl_resolveAll(C, ns, rtl_car(C->M, in));
  out = rtl_cons(C->M, arg, RTL_NIL);

  for (tail = rtl_cdr(C->M, in); tail != RTL_NIL; tail = rtl_cdr(C->M, tail)) {
    arg = rtl_macroExpand(C, ns, rtl_car(C->M, tail));
    out = rtl_cons(C->M, arg, out);
  }

  out = rtl_reverseList(C->M, out);

  rtl_popWorkingSet(C->M);

  return out;
}

rtl_Word rtl_macroExpand(rtl_Compiler *C, rtl_NameSpace const *ns, rtl_Word in)
{
  rtl_Word head   = RTL_NIL,
           arg    = RTL_NIL,
           clause = RTL_NIL,
           name   = RTL_NIL,
           alias  = RTL_NIL,
           tail   = RTL_NIL,
           out    = RTL_NIL;

  rtl_Word const *rptr;
  rtl_Word       *wptr;

  rtl_FnDef *fnDef;

  size_t len, i;

  rtl_NameSpace newNS;

  ensureSymCache(C);

  RTL_PUSH_WORKING_SET(C->M, &in, &head, &arg, &clause, &name, &alias, &tail, &out);

  switch (rtl_typeOf(in)) {
  case RTL_UNRESOLVED_SYMBOL:
    out = rtl_resolveSymbol(C, ns, rtl_symbolID(in));
    break;

  case RTL_UNRESOLVED_SELECTOR:
    out = rtl_resolveSelector(C, ns, rtl_selectorID(in));
    break;

  case RTL_TUPLE:
    rptr = rtl_reifyTuple(C->M, in, &len);
    wptr = rtl_allocTuple(C->M, &out, len);
    rptr = rtl_reifyTuple(C->M, in, &len);
    for (i = 0; i < len; i++) {
      wptr[i] = rtl_macroExpand(C, ns, rptr[i]);
    } break;

  case RTL_MAP:
    if (rtl_isEmptyMap(in)) {
      out = in;
    } else {
      out = macroExpandMap(C, ns, in, 1);
    } break;

  case RTL_CONS:
    head = rtl_macroExpand(C, ns, rtl_car(C->M, in));

    if (head == symCache.intrinsic.inPackage) {
      name  = rtl_macroExpand(C, ns, rtl_cadr(C->M, in));
      newNS = rtl_nsInPackage(ns, rtl_internPackage(C, rtl_symbolName(name)));
      out   = rtl_macroExpand(C, &newNS,
			      rtl_cons(C->M,
				       rtl_intern("intrinsic", "progn"),
				       rtl_cddr(C->M, in)));

    } else if (head == symCache.intrinsic.usePackage) {
      name  = rtl_macroExpand(C, ns, rtl_cadr(C->M, in));
      newNS = rtl_nsUsePackage(ns, rtl_internPackage(C, rtl_symbolName(name)));
      out   = rtl_macroExpand(C, &newNS,
			      rtl_cons(C->M,
				       rtl_intern("intrinsic", "progn"),
				       rtl_cddr(C->M, in)));

    } else if (head == symCache.intrinsic.aliasPackage) {
      name  = rtl_macroExpand(C, ns, rtl_car(C->M, rtl_cadr(C->M, in)));
      alias = rtl_macroExpand(C, ns, rtl_cadr(C->M, rtl_cadr(C->M, in)));
      newNS = rtl_nsAliasPackage(ns, rtl_internPackage(C, rtl_symbolName(name)),
				 rtl_symbolName(alias));
      out   = rtl_macroExpand(C, &newNS,
			      rtl_cons(C->M,
				       rtl_intern("intrinsic", "progn"),
				       rtl_cddr(C->M, in)));

    } else if (head == symCache.intrinsic.alias) {
      out = in;

    } else if (head == symCache.intrinsic.quote) {
      out = rtl_resolveAll(C, ns, in);

    } else if (head == symCache.intrinsic.lambda) {
      out = rtl_cons(C->M, head,
		     rtl_macroExpandLambda(C, ns, rtl_cdr(C->M, in)));

    } else if (head == symCache.intrinsic.labels) {
      arg = RTL_NIL;

      for (clause = rtl_cadr(C->M, in); clause != RTL_NIL; clause = rtl_cdr(C->M, clause)) {
	name = rtl_resolveSymbol(C, ns, rtl_symbolID(rtl_caar(C->M, clause)));
	arg  = rtl_cons(C->M, rtl_cons(C->M, name,
				       rtl_macroExpandLambda(C, ns, rtl_cdar(C->M, clause))),
			arg);
      }

      out = rtl_cons(C->M, arg,
		     rtl_cons(C->M, head,
			      RTL_NIL));

      for (tail = rtl_cddr(C->M, in); tail != RTL_NIL; tail = rtl_cdr(C->M, tail)) {
	arg = rtl_macroExpand(C, ns, rtl_car(C->M, tail));
	out = rtl_cons(C->M, arg, out);
      }

      out = rtl_reverseList(C->M, out);

    } else if (head == symCache.intrinsic.defun) {
      tail = rtl_macroExpandLambda(C, ns, rtl_cddr(C->M, in));
      tail = rtl_cons(C->M, rtl_resolveSymbol(C, ns, rtl_symbolID(rtl_cadr(C->M, in))),
		      tail);
      out = rtl_cons(C->M, head, tail);

    } else if (head == symCache.intrinsic.defmacro) {
      tail = rtl_macroExpandLambda(C, ns, rtl_cddr(C->M, in));
      tail = rtl_cons(C->M, rtl_resolveSymbol(C, ns, rtl_symbolID(rtl_cadr(C->M, in))),
		      tail);
      out = rtl_cons(C->M, head, tail);

    } else {
      fnDef = rtl_lookupFn(C, head);

      if (fnDef != NULL && fnDef->isMacro) {
	out = rtl_macroExpand(C, ns,
			      rtl_applyList(C->M,
					    fnDef->fn,
					    rtl_cdr(C->M, in)));

     } else {
	out = rtl_cons(C->M, head, RTL_NIL);

	for (tail = rtl_cdr(C->M, in); rtl_isCons(tail); tail = rtl_cdr(C->M, tail))
	{
	  arg = rtl_macroExpand(C, ns, rtl_car(C->M, tail));
	  out = rtl_cons(C->M, arg, out);
	}

	out = rtl_reverseListImproper(C->M, out, rtl_macroExpand(C, ns, tail));
      }

    } break;

  default:
    out = in;
    break;
  }

  rtl_popWorkingSet(C->M);

  return out;
}

static
size_t annotateCodeSize(rtl_Machine *M, rtl_Intrinsic *x);

static
void rtl_tailCallPass(rtl_Intrinsic *x);

void rtl_compile(rtl_Compiler *C,
		 rtl_NameSpace const *ns,
		 uint32_t fnID,
		 rtl_Word in)
{
  rtl_Word head  = RTL_NIL,
           arg   = RTL_NIL,
           name  = RTL_NIL,
           alias = RTL_NIL,
           tail  = RTL_NIL,
           out   = RTL_NIL;

  rtl_NameSpace newNS;

  rtl_Intrinsic *ir;

  rtl_CodeBase *codeBase = C->M->codeBase;

  ensureSymCache(C);

  RTL_PUSH_WORKING_SET(C->M, &in, &head, &arg, &name, &alias, &tail, &out);

  switch (rtl_typeOf(in)) {
  case RTL_CONS:
    head = rtl_macroExpand(C, ns, rtl_car(C->M, in));

    if (head == symCache.intrinsic.inPackage) {
      name  = rtl_macroExpand(C, ns, rtl_cadr(C->M, in));
      newNS = rtl_nsInPackage(ns, rtl_internPackage(C, rtl_symbolName(name)));
      rtl_compile(C, &newNS, fnID,
		  rtl_cons(C->M, rtl_intern("intrinsic", "progn"),
			   rtl_cddr(C->M, in)));

      rtl_popWorkingSet(C->M);
      return;

    } else if (head == symCache.intrinsic.usePackage) {
      name  = rtl_macroExpand(C, ns, rtl_cadr(C->M, in));
      newNS = rtl_nsUsePackage(ns, rtl_internPackage(C, rtl_symbolName(name)));
      rtl_compile(C, &newNS, fnID,
		  rtl_cons(C->M, rtl_intern("intrinsic", "progn"),
			   rtl_cddr(C->M, in)));

      rtl_popWorkingSet(C->M);
      return;

    } else if (head == symCache.intrinsic.aliasPackage) {
      name  = rtl_macroExpand(C, ns, rtl_car(C->M, rtl_cadr(C->M, in)));
      alias = rtl_macroExpand(C, ns, rtl_cadr(C->M, rtl_cadr(C->M, in)));
      newNS = rtl_nsAliasPackage(ns, rtl_internPackage(C, rtl_symbolName(name)),
				 rtl_symbolName(alias));
      rtl_compile(C, &newNS, fnID,
		  rtl_cons(C->M, rtl_intern("intrinsic", "progn"),
			   rtl_cddr(C->M, in)));

      rtl_popWorkingSet(C->M);
      return;

    } else if (head == symCache.intrinsic.alias) {
      printf("\n   !!! alias not yet supported !!!\n\n");
      abort();

    } else if (head == symCache.intrinsic.progn) {
      for (tail = rtl_cdr(C->M, in);
	   tail != RTL_NIL;
	   tail = rtl_cdr(C->M, tail))
      {
	rtl_compile(C, ns, fnID, rtl_car(C->M, tail));

	if (rtl_cdr(C->M, tail) != RTL_NIL) {
	  rtl_emitByteToFunc(codeBase, fnID, RTL_OP_POP);
	}
      }

      rtl_popWorkingSet(C->M);
      return;

    }

  default:
    out = rtl_macroExpand(C, ns, in);
    break;
  }

  printf("Generating intrinsics for: ");
  rtl_formatExpr(C->M, out);
  printf("\n");

  ir = rtl_exprToIntrinsic(C, out);
  ir = rtl_transformIntrinsic(ir);

  rtl_tailCallPass(ir);
  annotateCodeSize(C->M, ir);

  rtl_emitIntrinsicCode(C, fnID, ir);

  rtl_popWorkingSet(C->M);
}

rtl_Intrinsic *mapToIntrinsic(rtl_Compiler  *C,
			      rtl_Intrinsic *soFar,
			      rtl_Word      map,
			      uint32_t      mask)
{
  rtl_Word const *rptr,
                 *entry;

  size_t         len,
                 i;

  rptr = __rtl_reifyPtr(C->M, map);
  len  = __builtin_popcount(mask);

  for (i = 0; i < len; i++) {
    entry = rptr + 2*i;

    if (rtl_isHeader(entry[0])) {
      soFar = mapToIntrinsic(C, soFar, entry[1], rtl_headerValue(entry[0]));
    } else {
      soFar = rtl_mkInsertIntrinsic(soFar,
				    rtl_exprToIntrinsic(C, entry[0]),
				    rtl_exprToIntrinsic(C, entry[1]));
    }
  }

  return soFar;
}

rtl_Intrinsic *rtl_exprToIntrinsic(rtl_Compiler *C, rtl_Word sxp)
{
  rtl_Word head, clause, tail, name;
  rtl_Word const *rptr;
  size_t len, i;
  rtl_Intrinsic **buf;
  size_t        bufLen;
  size_t        bufCap;

  rtl_Intrinsic **labels;
  rtl_Word      *labelsNames;
  size_t        labelsLen;
  size_t        labelsCap;

  rtl_Word *argNames;
  size_t   argNamesLen;
  size_t   argNamesCap;

  bool hasRestArg;

  ensureSymCache(C);

  buf    = NULL;
  bufCap = bufLen = 0;

  labels      = NULL;
  labelsNames = NULL;
  labelsCap   = labelsLen = 0;

  argNames    = NULL;
  argNamesCap = argNamesLen = 0;

  hasRestArg = false;

  switch (rtl_typeOf(sxp)) {
  case RTL_TUPLE:
    rptr = rtl_reifyTuple(C->M, sxp, &len);
    for (i = 0; i < len; i++) {
      if (bufCap == bufLen) {
	bufCap = !bufCap ? 4 : 2*bufCap;
	buf    = realloc(buf, sizeof(rtl_Intrinsic *)*bufCap);
      }

      buf[bufLen++] = rtl_exprToIntrinsic(C, rptr[i]);
    }

    return rtl_mkTupleIntrinsic(buf, bufLen);

  case RTL_MAP:
    if (rtl_isEmptyMap(sxp)) {
      return rtl_mkConstantIntrinsic(RTL_MAP);
    } else {
      return mapToIntrinsic(C, rtl_mkConstantIntrinsic(RTL_MAP), sxp, 1);
    } abort(); // unreachable ..

  case RTL_CONS:
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

    } else if (head == symCache.intrinsic.tuple) {
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

      return rtl_mkTupleIntrinsic(buf, bufLen);

    } else if (head == symCache.intrinsic.len) {
      return rtl_mkLenIntrinsic(rtl_exprToIntrinsic(C, rtl_cadr(C->M, sxp)));

    } else if (head == symCache.intrinsic.get) {
      return rtl_mkGetIntrinsic(rtl_exprToIntrinsic(C, rtl_cadr(C->M, sxp)),
				rtl_exprToIntrinsic(C, rtl_caddr(C->M, sxp)));

    } else if (head == symCache.intrinsic.map) {
      return rtl_mkConstantIntrinsic(RTL_MAP);

    } else if (head == symCache.intrinsic.insert) {
      return rtl_mkInsertIntrinsic(rtl_exprToIntrinsic(C, rtl_cadr(C->M, sxp)),
				   rtl_exprToIntrinsic(C, rtl_caddr(C->M, sxp)),
				   rtl_exprToIntrinsic(C, rtl_car(C->M, rtl_cdddr(C->M, sxp))));

    } else if (head == symCache.intrinsic.lookup) {
      return rtl_mkLookupIntrinsic(rtl_exprToIntrinsic(C, rtl_cadr(C->M, sxp)),
				   rtl_exprToIntrinsic(C, rtl_caddr(C->M, sxp)));

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

    } else if (head == symCache.intrinsic.labels) {
      assert(len >= 2);

      for (clause = rtl_cadr(C->M, sxp);
	   clause != RTL_NIL;
	   clause = rtl_cdr(C->M, clause))
      {
	for (tail = rtl_cadar(C->M, clause);
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

	for (tail = rtl_cddar(C->M, clause);
	     tail != RTL_NIL;
	     tail = rtl_cdr(C->M, tail))
	{
	  if (bufCap == bufLen) {
	    bufCap = !bufCap ? 4 : 2*bufCap;
	    buf    = realloc(buf, sizeof(rtl_Intrinsic *)*bufCap);
	  }

	  buf[bufLen++] = rtl_exprToIntrinsic(C, rtl_car(C->M, tail));
	}

	if (labelsCap == labelsLen) {
	  labelsCap   = !labelsCap ? 4 : 2*labelsCap;
	  labels      = realloc(labels, sizeof(rtl_Intrinsic *)*labelsCap);
	  labelsNames = realloc(labelsNames, sizeof(rtl_Word)*labelsCap);
	}

	labelsNames[labelsLen] = rtl_caar(C->M, clause);
	labels[labelsLen++]    = rtl_mkLambdaIntrinsic(argNames, argNamesLen,
						       buf,      bufLen);

	argNames    = NULL;
	argNamesCap = argNamesLen = 0;

	buf    = NULL;
	bufCap = bufLen = 0;
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

      return rtl_mkLabelsIntrinsic(labelsNames, labels, labelsLen, buf, bufLen);

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
    } else if (head == symCache.intrinsic.export) {
      assert(len == 2);
      return rtl_mkExportIntrinsic(rtl_cadr(C->M, sxp));

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

    } else if (head == symCache.intrinsic.nilp) {
      assert(len == 2);
      return rtl_mkTypePredIntrinsic(RTL_NIL,
				     rtl_exprToIntrinsic(C, rtl_cadr(C->M, sxp)));

    } else if (head == symCache.intrinsic.symbolp) {
      assert(len == 2);
      return rtl_mkTypePredIntrinsic(RTL_SYMBOL,
				     rtl_exprToIntrinsic(C, rtl_cadr(C->M, sxp)));

    } else if (head == symCache.intrinsic.selectorp) {
      assert(len == 2);
      return rtl_mkTypePredIntrinsic(RTL_SELECTOR,
				     rtl_exprToIntrinsic(C, rtl_cadr(C->M, sxp)));

    } else if (head == symCache.intrinsic.int28p) {
      assert(len == 2);
      return rtl_mkTypePredIntrinsic(RTL_INT28,
				     rtl_exprToIntrinsic(C, rtl_cadr(C->M, sxp)));

    } else if (head == symCache.intrinsic.fix14p) {
      assert(len == 2);
      return rtl_mkTypePredIntrinsic(RTL_FIX14,
				     rtl_exprToIntrinsic(C, rtl_cadr(C->M, sxp)));

    } else if (head == symCache.intrinsic.tuplep) {
      assert(len == 2);
      return rtl_mkTypePredIntrinsic(RTL_TUPLE,
				     rtl_exprToIntrinsic(C, rtl_cadr(C->M, sxp)));

    } else if (head == symCache.intrinsic.stringp) {
      assert(len == 2);
      return rtl_mkTypePredIntrinsic(RTL_STRING,
				     rtl_exprToIntrinsic(C, rtl_cadr(C->M, sxp)));

    } else if (head == symCache.intrinsic.mapp) {
      assert(len == 2);
      return rtl_mkTypePredIntrinsic(RTL_MAP,
				     rtl_exprToIntrinsic(C, rtl_cadr(C->M, sxp)));

    } else if (head == symCache.intrinsic.consp) {
      assert(len == 2);
      return rtl_mkTypePredIntrinsic(RTL_CONS,
				     rtl_exprToIntrinsic(C, rtl_cadr(C->M, sxp)));

    } else if (head == symCache.intrinsic.functionp) {
      assert(len == 2);
      return rtl_mkTypePredIntrinsic(RTL_FUNCTION,
				     rtl_exprToIntrinsic(C, rtl_cadr(C->M, sxp)));

    } else if (head == symCache.intrinsic.closurep) {
      assert(len == 2);
      return rtl_mkTypePredIntrinsic(RTL_CLOSURE,
				     rtl_exprToIntrinsic(C, rtl_cadr(C->M, sxp)));

    } else if (head == symCache.intrinsic.unresolvedp) {
      assert(len == 2);
      return rtl_mkTypePredIntrinsic(RTL_UNRESOLVED_SYMBOL,
				     rtl_exprToIntrinsic(C, rtl_cadr(C->M, sxp)));

    } else if (head == symCache.intrinsic.topp) {
      assert(len == 2);
      return rtl_mkTypePredIntrinsic(RTL_TOP,
				     rtl_exprToIntrinsic(C, rtl_cadr(C->M, sxp)));

    } else if (head == symCache.intrinsic._if) {
      assert(len == 3 || len == 4);

      return rtl_mkIfIntrinsic(rtl_exprToIntrinsic(C, rtl_cadr(C->M, sxp)),
			       rtl_exprToIntrinsic(C, rtl_caddr(C->M, sxp)),
			       rtl_exprToIntrinsic(C, rtl_car(C->M, rtl_cdddr(C->M, sxp))));

    } else if (head == symCache.intrinsic.applyList) {
      assert(len == 3);

      return rtl_mkApplyListIntrinsic(rtl_exprToIntrinsic(C, rtl_cadr(C->M, sxp)),
				      rtl_exprToIntrinsic(C, rtl_caddr(C->M, sxp)));

    } else if (head == symCache.intrinsic.applyTuple) {
      assert(len == 3);

      return rtl_mkApplyTupleIntrinsic(rtl_exprToIntrinsic(C, rtl_cadr(C->M, sxp)),
				       rtl_exprToIntrinsic(C, rtl_caddr(C->M, sxp)));

    } else if (head == symCache.intrinsic.gensym) {
      assert(len == 1);

      return rtl_mkGensymIntrinsic();
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
    } break;

  case RTL_SYMBOL:
    return rtl_mkVarIntrinsic(sxp);

  case RTL_STRING:
    return rtl_mkStringIntrinsic(C->M, sxp);

  case RTL_SELECTOR:
  case RTL_INT28:
  case RTL_NIL:
  case RTL_TOP:
    return rtl_mkConstantIntrinsic(sxp);
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
  size_t        argsLenTmp,
                i;
  Environment   newEnv;

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

  case RTL_INTRINSIC_TUPLE:
    for (i = 0; i < x->as.tuple.elemsLen; i++) {
      x->as.tuple.elems[i]  = __impl_transformIntrinsic(env, x->as.tuple.elems[i]);
    } break;

  case RTL_INTRINSIC_LEN:
    x->as.len.tuple = __impl_transformIntrinsic(env, x->as.len.tuple);
    break;

  case RTL_INTRINSIC_INSERT:
    x->as.insert.map = __impl_transformIntrinsic(env, x->as.insert.map);
    x->as.insert.key = __impl_transformIntrinsic(env, x->as.insert.key);
    x->as.insert.val = __impl_transformIntrinsic(env, x->as.insert.val);
    break;

  case RTL_INTRINSIC_LOOKUP:
    x->as.lookup.map = __impl_transformIntrinsic(env, x->as.lookup.map);
    x->as.lookup.key = __impl_transformIntrinsic(env, x->as.lookup.key);
    break;

  case RTL_INTRINSIC_GET:
    x->as.get.tuple = __impl_transformIntrinsic(env, x->as.get.tuple);
    x->as.get.index = __impl_transformIntrinsic(env, x->as.get.index);
    break;

  case RTL_INTRINSIC_VAR:
    if ((!env ||
	 !lookupVar(env, x->as.var.name, &x->as.var.frame, &x->as.var.idx))) {
      x->as.var.global = true;
    }

    x->codeSize = 5;
    break;

  case RTL_INTRINSIC_NAMED_TAIL:
  case RTL_INTRINSIC_TAIL:
  case RTL_INTRINSIC_NAMED_CALL:
    abort(); // This should never happen

  case RTL_INTRINSIC_CALL:
    x->codeSize = 0;

    // Start by transforming all of the args
    for (i = 0; i < x->as.call.argsLen; i++) {
      x->as.call.args[i]  = __impl_transformIntrinsic(env, x->as.call.args[i]);
      x->codeSize        += x->as.call.args[i]->codeSize;
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


  case RTL_INTRINSIC_APPLY_LIST:
    x->as.applyList.fn  = __impl_transformIntrinsic(env, x->as.applyList.fn);
    x->as.applyList.arg = __impl_transformIntrinsic(env, x->as.applyList.arg);
    break;

  case RTL_INTRINSIC_APPLY_TUPLE:
    x->as.applyTuple.fn  = __impl_transformIntrinsic(env, x->as.applyTuple.fn);
    x->as.applyTuple.arg = __impl_transformIntrinsic(env, x->as.applyTuple.arg);
    break;

  case RTL_INTRINSIC_PROGN:
    for (i = 0; i < x->as.progn.formsLen; i++) {
      x->as.progn.forms[i] = __impl_transformIntrinsic(env, x->as.progn.forms[i]);
    }
    break;

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

  case RTL_INTRINSIC_LABELS:
    newEnv.super = env;
    newEnv.frame = env ? env->frame + 1 : 0;
    newEnv.len   = x->as.labels.labelsLen;
    newEnv.names = x->as.labels.labelsNames;

    for (i = 0; i < x->as.labels.labelsLen; i++) {
      x->as.labels.labelsFns[i] = __impl_transformIntrinsic(&newEnv,
							    x->as.labels.labelsFns[i]);
    }

    for (i = 0; i < x->as.labels.bodyLen; i++) {
      x->as.labels.body[i] = __impl_transformIntrinsic(&newEnv,
						       x->as.labels.body[i]);
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

  case RTL_INTRINSIC_TYPE_PRED:
    x->as.typePred.arg = __impl_transformIntrinsic(env, x->as.typePred.arg);
    break;

  case RTL_INTRINSIC_IF:
    x->as._if.test  = __impl_transformIntrinsic(env, x->as._if.test);
    x->as._if.then  = __impl_transformIntrinsic(env, x->as._if.then);
    x->as._if._else = __impl_transformIntrinsic(env, x->as._if._else);
    break;

  case RTL_INTRINSIC_EXPORT:
  case RTL_INTRINSIC_QUOTE:
  case RTL_INTRINSIC_STRING:
  case RTL_INTRINSIC_CONSTANT:
  case RTL_INTRINSIC_GENSYM:
    break;
  }

  return x;
}

rtl_Intrinsic *rtl_transformIntrinsic(rtl_Intrinsic *x) {
  return __impl_transformIntrinsic(NULL, x);
}

static
void rtl_markTailCalls(rtl_Intrinsic *x)
{
  switch (x->type) {
  case RTL_INTRINSIC_NAMED_CALL:
    x->type = RTL_INTRINSIC_NAMED_TAIL;
    break;

  case RTL_INTRINSIC_CALL:
    x->type = RTL_INTRINSIC_TAIL;
    break;

  case RTL_INTRINSIC_APPLY_LIST:
  case RTL_INTRINSIC_APPLY_TUPLE:
    // TODO: These can be turned into tail calls too...
    break;

  case RTL_INTRINSIC_PROGN:
    // The last position of a non-empty progn is in tail position.
    if (x->as.progn.formsLen > 0) {
      rtl_markTailCalls(x->as.progn.forms[x->as.progn.formsLen - 1]);
    } break;

  case RTL_INTRINSIC_IF:
    rtl_markTailCalls(x->as._if.then);
    rtl_markTailCalls(x->as._if._else);
    break;

  default:
    break;
  }
}

static
void rtl_tailCallPass(rtl_Intrinsic *x)
{
  size_t i;

  switch (x->type) {
  case RTL_INTRINSIC_CONS:
    rtl_tailCallPass(x->as.cons.car);
    rtl_tailCallPass(x->as.cons.cdr);
    break;

  case RTL_INTRINSIC_CAR:
    rtl_tailCallPass(x->as.car.arg);
    break;

  case RTL_INTRINSIC_CDR:
    rtl_tailCallPass(x->as.cdr.arg);
    break;

  case RTL_INTRINSIC_TUPLE:
    for (i = 0; i < x->as.tuple.elemsLen; i++) {
      rtl_tailCallPass(x->as.tuple.elems[i]);
    } break;

  case RTL_INTRINSIC_LEN:
    rtl_tailCallPass(x->as.len.tuple);
    break;

  case RTL_INTRINSIC_INSERT:
    rtl_tailCallPass(x->as.insert.map);
    rtl_tailCallPass(x->as.insert.key);
    rtl_tailCallPass(x->as.insert.val);
    break;

  case RTL_INTRINSIC_LOOKUP:
    rtl_tailCallPass(x->as.lookup.map);
    rtl_tailCallPass(x->as.lookup.key);
    break;

  case RTL_INTRINSIC_GET:
    rtl_tailCallPass(x->as.get.tuple);
    rtl_tailCallPass(x->as.get.index);
    break;

  case RTL_INTRINSIC_VAR:
    break;

  case RTL_INTRINSIC_NAMED_CALL:
    for (i = 0; i < x->as.namedCall.argsLen; i++) {
      rtl_tailCallPass(x->as.namedCall.args[i]);
    } break;

  case RTL_INTRINSIC_NAMED_TAIL:
    for (i = 0; i < x->as.namedTail.argsLen; i++) {
      rtl_tailCallPass(x->as.namedTail.args[i]);
    } break;


  case RTL_INTRINSIC_TAIL:
    rtl_tailCallPass(x->as.tail.fn);

    for (i = 0; i < x->as.tail.argsLen; i++) {
      rtl_tailCallPass(x->as.tail.args[i]);
    } break;

  case RTL_INTRINSIC_CALL:
    rtl_tailCallPass(x->as.call.fn);

    for (i = 0; i < x->as.call.argsLen; i++) {
      rtl_tailCallPass(x->as.call.args[i]);
    } break;

  case RTL_INTRINSIC_APPLY_LIST:
    rtl_tailCallPass(x->as.applyList.fn);
    rtl_tailCallPass(x->as.applyList.arg);
    break;

  case RTL_INTRINSIC_APPLY_TUPLE:
    rtl_tailCallPass(x->as.applyTuple.fn);
    rtl_tailCallPass(x->as.applyTuple.arg);
    break;

  case RTL_INTRINSIC_PROGN:
    for (i = 0; i < x->as.progn.formsLen; i++) {
      rtl_tailCallPass(x->as.progn.forms[i]);
    } break;

  case RTL_INTRINSIC_LAMBDA:
    for (i = 0; i < x->as.lambda.bodyLen; i++) {
      rtl_tailCallPass(x->as.lambda.body[i]);
    }

    if (x->as.lambda.bodyLen > 0) {
      rtl_markTailCalls(x->as.lambda.body[x->as.lambda.bodyLen - 1]);
    } break;

  case RTL_INTRINSIC_LABELS:
    for (i = 0; i < x->as.labels.labelsLen; i++) {
      rtl_tailCallPass(x->as.labels.labelsFns[i]);
    }

    for (i = 0; i < x->as.labels.bodyLen; i++) {
     rtl_tailCallPass(x->as.labels.body[i]);
    } break;

  case RTL_INTRINSIC_DEFUN:
    for (i = 0; i < x->as.defun.bodyLen; i++) {
      rtl_tailCallPass(x->as.defun.body[i]);
    }

    if (x->as.defun.bodyLen > 0) {
      rtl_markTailCalls(x->as.defun.body[x->as.defun.bodyLen - 1]);
    } break;

  case RTL_INTRINSIC_DEFMACRO:
    for (i = 0; i < x->as.defmacro.bodyLen; i++) {
      rtl_tailCallPass(x->as.defmacro.body[i]);
    }

    if (x->as.defmacro.bodyLen > 0) {
      rtl_markTailCalls(x->as.defmacro.body[x->as.defmacro.bodyLen - 1]);
    } break;

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
    rtl_tailCallPass(x->as.binop.leftArg);
    rtl_tailCallPass(x->as.binop.rightArg);
    break;

  case RTL_INTRINSIC_TYPE_PRED:
    rtl_tailCallPass(x->as.typePred.arg);
    break;

  case RTL_INTRINSIC_IF:
    rtl_tailCallPass(x->as._if.test);
    rtl_tailCallPass(x->as._if.then);
    rtl_tailCallPass(x->as._if._else);
    break;

  case RTL_INTRINSIC_EXPORT:
  case RTL_INTRINSIC_QUOTE:
  case RTL_INTRINSIC_STRING:
  case RTL_INTRINSIC_CONSTANT:
  case RTL_INTRINSIC_GENSYM:
    break;
  }
}

static
size_t quoteCodeSize(rtl_Machine *M, rtl_Word x);

static
size_t quoteMapCodeSize(rtl_Machine *M, rtl_Word x, uint32_t mask)
{
  rtl_Word const *rptr, *entry;
  size_t   len,
           i,
           codeSize;

  rptr     = __rtl_reifyPtr(M, x);
  len      = __builtin_popcount(mask);
  codeSize = 0;

  for (i = 0; i < len; i++) {
    entry = rptr + 2*i;

    if (rtl_isHeader(entry[0])) {
      codeSize += quoteMapCodeSize(M, entry[1], rtl_headerValue(entry[0]));
    } else {
      codeSize += quoteCodeSize(M, entry[0])
	        + quoteCodeSize(M, entry[1])
		+ 1;
    }
  }

  return codeSize;
}

static
size_t quoteCodeSize(rtl_Machine *M, rtl_Word x)
{
  rtl_Word const *rptr;
  size_t i, len, codeSize;

  switch (rtl_typeOf(x)) {
  case RTL_NIL:
  case RTL_TOP:
    return 1;

  case RTL_SYMBOL:
  case RTL_SELECTOR:
  case RTL_INT28:
  case RTL_FIX14:
    return 5;

  case RTL_CONS:
    return quoteCodeSize(M, rtl_car(M, x))
         + quoteCodeSize(M, rtl_cdr(M, x))
         + 1;

  case RTL_MAP:
    if (rtl_isEmptyMap(x)) {
      return 1;
    }

    return 1 + quoteMapCodeSize(M, x, 1);

  case RTL_TUPLE:
    codeSize = 0;
    rptr = rtl_reifyTuple(M, x, &len);

    for (i = 0; i < len; i++) {
      codeSize += quoteCodeSize(M, rptr[i]);
    }

    return codeSize + 3;

  default:
    printf("Can't quote object of type '%s' ... yet?\n",
	   rtl_typeNameOf(x));
    abort();
  }
}

static
size_t annotateCodeSize(rtl_Machine *M, rtl_Intrinsic *x)
{
  size_t i,
         thenSize,
         elseSize,
         thenJmpBytes,
         elseJmpBytes;

  switch (x->type) {
  case RTL_INTRINSIC_CONS:
    return x->codeSize = annotateCodeSize(M, x->as.cons.car)
                       + annotateCodeSize(M, x->as.cons.cdr)
                       + 1;

  case RTL_INTRINSIC_CAR:
    return x->codeSize = annotateCodeSize(M, x->as.car.arg)
                       + 1;

  case RTL_INTRINSIC_CDR:
    return x->codeSize = annotateCodeSize(M, x->as.cdr.arg)
                       + 1;

  case RTL_INTRINSIC_TUPLE:
    x->codeSize = 3;
    for (i = 0; i < x->as.tuple.elemsLen; i++) {
      x->codeSize += annotateCodeSize(M, x->as.tuple.elems[i]);
    }

    return x->codeSize;

  case RTL_INTRINSIC_LEN:
    return x->codeSize = annotateCodeSize(M, x->as.len.tuple)
                       + 1;

  case RTL_INTRINSIC_INSERT:
    return x->codeSize = annotateCodeSize(M, x->as.insert.map)
                       + annotateCodeSize(M, x->as.insert.key)
                       + annotateCodeSize(M, x->as.insert.val)
                       + 1;

  case RTL_INTRINSIC_LOOKUP:
    return x->codeSize = annotateCodeSize(M, x->as.lookup.map)
                       + annotateCodeSize(M, x->as.lookup.key)
                       + 1;

  case RTL_INTRINSIC_GET:
    return x->codeSize = annotateCodeSize(M, x->as.get.tuple)
                       + annotateCodeSize(M, x->as.get.index)
                       + 1;

  case RTL_INTRINSIC_VAR:
    // Instruction is one of:
    //
    //    undef-var <u32>
    //    const     <u32>
    //    var       <u16> <u16>
    //
    // All cases are 5 bytes
    return x->codeSize = 5;

  case RTL_INTRINSIC_NAMED_CALL:
    x->codeSize = 7;

    for (i = 0; i < x->as.namedCall.argsLen; i++) {
      x->codeSize += annotateCodeSize(M, x->as.namedCall.args[i]);
    }
    return x->codeSize;

  case RTL_INTRINSIC_NAMED_TAIL:
    x->codeSize = 7;

    for (i = 0; i < x->as.namedTail.argsLen; i++) {
      x->codeSize += annotateCodeSize(M, x->as.namedTail.args[i]);
    }
    return x->codeSize;

  case RTL_INTRINSIC_CALL:
    x->codeSize = 3 + annotateCodeSize(M, x->as.call.fn);

    for (i = 0; i < x->as.call.argsLen; i++) {
      x->codeSize += annotateCodeSize(M, x->as.call.args[i]);
    }
    return x->codeSize;

  case RTL_INTRINSIC_TAIL:
    x->codeSize = 3 + annotateCodeSize(M, x->as.tail.fn);

    for (i = 0; i < x->as.tail.argsLen; i++) {
      x->codeSize += annotateCodeSize(M, x->as.tail.args[i]);
    }
    return x->codeSize;

  case RTL_INTRINSIC_APPLY_LIST:
    return x->codeSize = annotateCodeSize(M, x->as.applyList.fn)
                       + annotateCodeSize(M, x->as.applyList.arg)
                       + 1;

  case RTL_INTRINSIC_APPLY_TUPLE:
    return x->codeSize = annotateCodeSize(M, x->as.applyTuple.fn)
                       + annotateCodeSize(M, x->as.applyTuple.arg)
                       + 1;

  case RTL_INTRINSIC_PROGN:
    if (x->as.progn.formsLen == 0) {
      return x->codeSize = 1;
    }

    x->codeSize = 0;
    for (i = 0; i < x->as.progn.formsLen; i++) {
      x->codeSize += annotateCodeSize(M, x->as.progn.forms[i]);
      if (i + 1 < x->as.progn.formsLen) {
	x->codeSize++;
      }
    }

    return x->codeSize;

  case RTL_INTRINSIC_LAMBDA:
    for (i = 0; i < x->as.lambda.bodyLen; i++) {
      annotateCodeSize(M, x->as.lambda.body[i]);
    }

    return x->codeSize = 5;

  case RTL_INTRINSIC_LABELS:
    for (i = 0; i < x->as.labels.labelsLen; i++) {
      annotateCodeSize(M, x->as.labels.labelsFns[i]);
    }

    return x->codeSize = 3 + 5*x->as.labels.labelsLen;

  case RTL_INTRINSIC_DEFUN:
    for (i = 0; i < x->as.defun.bodyLen; i++) {
      annotateCodeSize(M, x->as.defun.body[i]);
    }
    return x->codeSize = 5;

  case RTL_INTRINSIC_DEFMACRO:
    for (i = 0; i < x->as.defmacro.bodyLen; i++) {
      annotateCodeSize(M, x->as.defmacro.body[i]);
    }
    return x->codeSize = 5;

  case RTL_INTRINSIC_EXPORT:
    return x->codeSize = 1;

  case RTL_INTRINSIC_QUOTE:
    return x->codeSize = quoteCodeSize(M, x->as.quote);

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
    return x->codeSize = annotateCodeSize(M, x->as.binop.leftArg)
                       + annotateCodeSize(M, x->as.binop.rightArg)
                       + 1;

  case RTL_INTRINSIC_TYPE_PRED:
    return x->codeSize = annotateCodeSize(M, x->as.typePred.arg)
                       + 1;

  case RTL_INTRINSIC_IF:
    thenSize = annotateCodeSize(M, x->as._if.then);
    elseSize = annotateCodeSize(M, x->as._if._else);

    if (thenSize + 5 < (1 << 7)) {
      thenJmpBytes = 1;
    } else if (thenSize + 5 < (1 << 15)) {
      thenJmpBytes = 2;
    } else {
      thenJmpBytes = 4;
    }

    if (elseSize < (1 << 7)) {
      elseJmpBytes = 1;
    } else if (elseSize < (1 << 15)) {
      elseJmpBytes = 2;
    } else {
      elseJmpBytes = 4;
    }

    return x->codeSize = annotateCodeSize(M, x->as._if.test)
                       + 1
                       + thenSize
                       + thenJmpBytes + 1
                       + elseSize
                       + elseJmpBytes + 1;

  case RTL_INTRINSIC_STRING:
    return x->codeSize = 5 + strlen(x->as.string.str) + 1;

  case RTL_INTRINSIC_GENSYM:
    return x->codeSize = 1;

  case RTL_INTRINSIC_CONSTANT:
    switch (x->as.constant) {
    case RTL_NIL:
    case RTL_TOP:
    case RTL_MAP:
      return x->codeSize = 1;

    default:
      return x->codeSize = 5;
    }

  default:
    abort(); // unreachable
  }
}

static
void emitQuoteCode(rtl_Compiler *C, uint16_t fnID, rtl_Word expr);

static
void emitMapQuoteCode(rtl_Compiler        *C,
		      uint16_t            fnID,
		      rtl_Word            map,
		      uint32_t            mask)
{
  rtl_Word const *rptr, *entry;
  size_t   len,
           i;

  rptr = __rtl_reifyPtr(C->M, map);
  len  = __builtin_popcount(mask);

  for (i = 0; i < len; i++) {
    entry = rptr + 2*i;

    if (rtl_isHeader(entry[0])) {
      emitMapQuoteCode(C, fnID, entry[1], rtl_headerValue(entry[0]));
    } else {
      emitQuoteCode(C, fnID, entry[0]);
      emitQuoteCode(C, fnID, entry[1]);
      rtl_emitByteToFunc(C->M->codeBase, fnID, RTL_OP_INSERT);
    }
  }
}

static
void emitQuoteCode(rtl_Compiler *C, uint16_t fnID, rtl_Word expr)
{
  rtl_Word const *rptr;
  size_t i, len;

  rtl_CodeBase *codeBase;

  codeBase = C->M->codeBase;

  switch (rtl_typeOf(expr)) {
  case RTL_NIL:
    rtl_emitByteToFunc(codeBase, fnID, RTL_OP_CONST_NIL);
    break;

  case RTL_SYMBOL:
  case RTL_SELECTOR:
  case RTL_INT28:
  case RTL_FIX14:
    rtl_emitByteToFunc(codeBase, fnID, RTL_OP_CONST);
    rtl_emitWordToFunc(codeBase, fnID, expr);
    break;

  case RTL_TOP:
    rtl_emitByteToFunc(codeBase, fnID, RTL_OP_CONST_TOP);
    break;

  case RTL_CONS:
    emitQuoteCode(C, fnID, rtl_car(C->M, expr));
    emitQuoteCode(C, fnID, rtl_cdr(C->M, expr));
    rtl_emitByteToFunc(codeBase, fnID, RTL_OP_CONS);
    break;

  case RTL_MAP:
    rtl_emitByteToFunc(codeBase, fnID, RTL_OP_MAP);
    if (!rtl_isEmptyMap(expr)) {
      emitMapQuoteCode(C, fnID, expr, 1);
    } break;

  case RTL_TUPLE:
    rptr = rtl_reifyTuple(C->M, expr, &len);
    for (i = 0; i < len; i++) {
      emitQuoteCode(C, fnID, rptr[i]);
    }
    rtl_emitByteToFunc(codeBase, fnID, RTL_OP_TUPLE);
    rtl_emitShortToFunc(codeBase, fnID, len);
    break;

  default:
    printf("Can't quote object of type '%s' ... yet?\n",
	   rtl_typeNameOf(expr));
    break;
  }
}

void rtl_emitIntrinsicCode(rtl_Compiler *C,
			   uint32_t fnID,
			   rtl_Intrinsic const *x)
{
  size_t    i, j;
  uint16_t  newFnID;
  uint32_t  offs;
  rtl_FnDef *fnDef;

  rtl_Intrinsic const *y;

  size_t thenJmpBytes,
         elseJmpBytes;

  rtl_CodeBase *codeBase;

  codeBase = C->M->codeBase;

  switch (x->type) {
  case RTL_INTRINSIC_CONS:
    rtl_emitIntrinsicCode(C, fnID, x->as.cons.car);
    rtl_emitIntrinsicCode(C, fnID, x->as.cons.cdr);
    rtl_emitByteToFunc(codeBase, fnID, RTL_OP_CONS);
    break;

  case RTL_INTRINSIC_CAR:
    rtl_emitIntrinsicCode(C, fnID, x->as.car.arg);
    rtl_emitByteToFunc(codeBase, fnID, RTL_OP_CAR);
    break;

  case RTL_INTRINSIC_CDR:
    rtl_emitIntrinsicCode(C, fnID, x->as.cdr.arg);
    rtl_emitByteToFunc(codeBase, fnID, RTL_OP_CDR);
    break;

  case RTL_INTRINSIC_TUPLE:
    assert(x->as.tuple.elemsLen < (1 << 16));

    for (i = 0; i < x->as.tuple.elemsLen; i++) {
      rtl_emitIntrinsicCode(C, fnID, x->as.tuple.elems[i]);
    }

    rtl_emitByteToFunc(codeBase, fnID, RTL_OP_TUPLE);
    rtl_emitShortToFunc(codeBase, fnID, x->as.tuple.elemsLen);
    break;

  case RTL_INTRINSIC_LEN:
    rtl_emitIntrinsicCode(C, fnID, x->as.len.tuple);
    rtl_emitByteToFunc(codeBase, fnID, RTL_OP_LEN);
    break;

  case RTL_INTRINSIC_GET:
    rtl_emitIntrinsicCode(C, fnID, x->as.get.tuple);
    rtl_emitIntrinsicCode(C, fnID, x->as.get.index);
    rtl_emitByteToFunc(codeBase, fnID, RTL_OP_GET);
    break;

  case RTL_INTRINSIC_INSERT:
    rtl_emitIntrinsicCode(C, fnID, x->as.insert.map);
    rtl_emitIntrinsicCode(C, fnID, x->as.insert.key);
    rtl_emitIntrinsicCode(C, fnID, x->as.insert.val);
    rtl_emitByteToFunc(codeBase, fnID, RTL_OP_INSERT);
    break;

  case RTL_INTRINSIC_LOOKUP:
    rtl_emitIntrinsicCode(C, fnID, x->as.lookup.map);
    rtl_emitIntrinsicCode(C, fnID, x->as.lookup.key);
    rtl_emitByteToFunc(codeBase, fnID, RTL_OP_LOOKUP);
    break;

  case RTL_INTRINSIC_VAR:
    if (x->as.var.global) {
      fnDef = rtl_lookupFn(C, x->as.var.name);

      if (fnDef != NULL && !fnDef->isMacro) {
	offs = rtl_nextFuncOffs(codeBase, fnID);
	rtl_emitByteToFunc(codeBase, fnID, RTL_OP_CONST);

	rtl_emitWordToFunc(codeBase, fnID, fnDef->fn);
      } else {
	offs = rtl_nextFuncOffs(codeBase, fnID);
	rtl_emitByteToFunc(codeBase, fnID, RTL_OP_UNDEFINED_VAR);

	rtl_emitWordToFunc(codeBase, fnID, x->as.var.name);
      }

      rtl_registerCallSite(C, x->as.var.name, rtl_function(fnID), offs);

    } else {
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_VAR);
      rtl_emitShortToFunc(codeBase, fnID, x->as.var.frame);
      rtl_emitShortToFunc(codeBase, fnID, x->as.var.idx);
    }
    break;

  case RTL_INTRINSIC_CALL:
  case RTL_INTRINSIC_TAIL:
    rtl_emitIntrinsicCode(C, fnID, x->as.call.fn);
    for (i = 0; i < x->as.call.argsLen; i++) {
      rtl_emitIntrinsicCode(C, fnID, x->as.call.args[i]);
    }

    switch (x->type) {
    case RTL_INTRINSIC_CALL:
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_CALL);
      break;

    case RTL_INTRINSIC_TAIL:
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_TAIL);
      break;

    default:
      abort();
    }

    rtl_emitShortToFunc(codeBase, fnID, x->as.call.argsLen);
    break;

  case RTL_INTRINSIC_NAMED_CALL:
  case RTL_INTRINSIC_NAMED_TAIL:
    for (i = 0; i < x->as.namedCall.argsLen; i++) {
      rtl_emitIntrinsicCode(C, fnID, x->as.namedCall.args[i]);
    }

    fnDef = rtl_lookupFn(C, x->as.namedCall.name);
    if (fnDef != NULL && !fnDef->isMacro) {
      offs = rtl_nextFuncOffs(codeBase, fnID);

      switch (x->type) {
      case RTL_INTRINSIC_NAMED_CALL:
	rtl_emitByteToFunc(codeBase, fnID, RTL_OP_STATIC_CALL);
	break;

      case RTL_INTRINSIC_NAMED_TAIL:
	rtl_emitByteToFunc(codeBase, fnID, RTL_OP_STATIC_TAIL);
	break;

      default:
	abort();
      }

      rtl_emitWordToFunc(codeBase, fnID, fnDef->fn);
      rtl_emitShortToFunc(codeBase, fnID, x->as.namedCall.argsLen);
    } else {
      offs = rtl_nextFuncOffs(codeBase, fnID);

      switch (x->type) {
      case RTL_INTRINSIC_NAMED_CALL:
	rtl_emitByteToFunc(codeBase, fnID, RTL_OP_UNDEFINED_CALL);
	break;

      case RTL_INTRINSIC_NAMED_TAIL:
	rtl_emitByteToFunc(codeBase, fnID, RTL_OP_UNDEFINED_TAIL);
	break;

      default:
	abort();
      }

      rtl_emitWordToFunc(codeBase, fnID, x->as.namedCall.name);
      rtl_emitShortToFunc(codeBase, fnID, x->as.namedCall.argsLen);
    }

    rtl_registerCallSite(C, x->as.namedCall.name, rtl_function(fnID), offs);
    break;

  case RTL_INTRINSIC_APPLY_LIST:
    rtl_emitIntrinsicCode(C, fnID, x->as.applyList.fn);
    rtl_emitIntrinsicCode(C, fnID, x->as.applyList.arg);
    rtl_emitByteToFunc(codeBase, fnID, RTL_OP_APPLY_LIST);
    break;

  case RTL_INTRINSIC_APPLY_TUPLE:
    rtl_emitIntrinsicCode(C, fnID, x->as.applyList.fn);
    rtl_emitIntrinsicCode(C, fnID, x->as.applyList.arg);
    rtl_emitByteToFunc(codeBase, fnID, RTL_OP_APPLY_TUPLE);
    break;

  case RTL_INTRINSIC_PROGN:
    if (x->as.progn.formsLen == 0) {
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_CONST_NIL);
    } else {
      for (i = 0; i < x->as.progn.formsLen; i++) {
	rtl_emitIntrinsicCode(C, fnID, x->as.progn.forms[i]);

	// Ignore the result of all but the last expression.
	if (i + 1 < x->as.progn.formsLen)
	  rtl_emitByteToFunc(codeBase, fnID, RTL_OP_POP);
      }
    } break;

  case RTL_INTRINSIC_LAMBDA:
    // TODO: Prevent recompilation of the same function from creating the same
    // lambda over and over with a new page each time...

    newFnID = rtl_newFuncID(codeBase, rtl_intern("__lambda__", "body"));

    rtl_emitByteToFunc(codeBase, fnID, RTL_OP_CLOSURE);
    rtl_emitWordToFunc(codeBase, fnID, rtl_function(newFnID));

    for (i = 0; i < x->as.lambda.bodyLen; i++) {
      rtl_emitIntrinsicCode(C, newFnID, x->as.lambda.body[i]);

      // Ignore the result of all but the last expression.
      if (i + 1 < x->as.lambda.bodyLen)
	rtl_emitByteToFunc(codeBase, newFnID, RTL_OP_POP);
    }

    rtl_emitByteToFunc(codeBase, newFnID, RTL_OP_RETURN);

    printf("  _____\n");
    printf(" | Compiled %d-ary lambda to function %d |\n",
	   (int)x->as.lambda.argNamesLen,
	   (int)newFnID);
    printf("       ------------");
    rtl_disasmFn(codeBase, rtl_function(newFnID));

    break;

  case RTL_INTRINSIC_LABELS:
    for (i = 0; i < x->as.labels.labelsLen; i++) {
      newFnID = rtl_newFuncID(codeBase, x->as.labels.labelsNames[i]);

      y = x->as.labels.labelsFns[i];

      assert(y->type == RTL_INTRINSIC_LAMBDA);

      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_CONST);
      rtl_emitWordToFunc(codeBase, fnID, rtl_function(newFnID));

      for (j = 0; j < y->as.lambda.bodyLen; j++) {
	rtl_emitIntrinsicCode(C, newFnID, y->as.lambda.body[j]);

	// Ignore the result of all but the last expression.
	if (j + 1 < y->as.lambda.bodyLen)
	  rtl_emitByteToFunc(codeBase, newFnID, RTL_OP_POP);
      }

      rtl_emitByteToFunc(codeBase, newFnID, RTL_OP_RETURN);

      printf("  _____\n");
      printf(" | Compiled labels '%s' |\n",
	     rtl_symbolName(x->as.labels.labelsNames[i]));
      printf("       ------------");
      rtl_disasmFn(codeBase, rtl_function(newFnID));
    }

    rtl_emitByteToFunc(codeBase, fnID, RTL_OP_LABELS);
    rtl_emitShortToFunc(codeBase, fnID, x->as.labels.labelsLen);

    if (x->as.labels.bodyLen > 0) {
      for (i = 0; i < x->as.labels.bodyLen; i++) {
	rtl_emitIntrinsicCode(C, fnID, x->as.labels.body[i]);

	// Ignore the result of all but the last expression.
	if (i + 1 < x->as.labels.bodyLen)
	  rtl_emitByteToFunc(codeBase, fnID, RTL_OP_POP);
      }
    } else {
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_CONST_NIL);
    }
    rtl_emitByteToFunc(codeBase, fnID, RTL_OP_END_LABELS);

    break;

  case RTL_INTRINSIC_DEFUN:
    newFnID = rtl_newFuncID(codeBase, x->as.defun.name);

    if (x->as.defun.hasRestArg) {
      rtl_emitByteToFunc(codeBase, newFnID, RTL_OP_REST);
      rtl_emitShortToFunc(codeBase, newFnID, x->as.defun.argNamesLen - 1);
    }

    // TODO: We're not using the argnames at all.. we should probably check
    // arity when resolving call sites.

    for (i = 0; i < x->as.defun.bodyLen; i++) {
      rtl_emitIntrinsicCode(C, newFnID, x->as.defun.body[i]);

      // Ignore the result of all but the last expression.
      if (i + 1 < x->as.defun.bodyLen)
	rtl_emitByteToFunc(codeBase, newFnID, RTL_OP_POP);
    }

    rtl_emitByteToFunc(codeBase, newFnID, RTL_OP_RETURN);

    rtl_defineFn(C, x->as.defun.name, rtl_function(newFnID), false);

    printf("  _____\n");
    printf(" | Compiled function '%s' to page %d |\n",
	   rtl_symbolName(x->as.defun.name),
	   (int)newFnID);
    printf("       ------------");
    rtl_disasmFn(codeBase, rtl_function(newFnID));

    rtl_emitByteToFunc(codeBase, fnID, RTL_OP_CONST);
    rtl_emitWordToFunc(codeBase, fnID, x->as.defun.name);
    break;

  case RTL_INTRINSIC_DEFMACRO:
    newFnID = rtl_newFuncID(codeBase, x->as.defmacro.name);

    if (x->as.defmacro.hasRestArg) {
      rtl_emitByteToFunc(codeBase, newFnID, RTL_OP_REST);
      rtl_emitShortToFunc(codeBase, newFnID, x->as.defmacro.argNamesLen - 1);
    }

    // TODO: We're not using the argnames at all.. we should probably check
    // arity when resolving call sites.

    for (i = 0; i < x->as.defmacro.bodyLen; i++) {
      rtl_emitIntrinsicCode(C, newFnID, x->as.defmacro.body[i]);

      // Ignore the result of all but the last expression.

      if (i + 1 < x->as.defmacro.bodyLen)
	rtl_emitByteToFunc(codeBase, newFnID, RTL_OP_POP);
    }

    rtl_emitByteToFunc(codeBase, newFnID, RTL_OP_RETURN);

    rtl_defineFn(C, x->as.defmacro.name, rtl_function(newFnID), true);

    printf("  _____\n");
    printf(" | Compiled macro '%s' to page %d |\n",
	   rtl_symbolName(x->as.defmacro.name),
	   (int)newFnID);
    printf("       ------------");
    rtl_disasmFn(codeBase, rtl_function(newFnID));


    rtl_emitByteToFunc(codeBase, fnID, RTL_OP_CONST);
    rtl_emitWordToFunc(codeBase, fnID, x->as.defmacro.name);
    break;

  case RTL_INTRINSIC_EXPORT:
    rtl_export(C, x->as.export);
    rtl_emitByteToFunc(codeBase, fnID, RTL_OP_CONST_TOP);
    break;

  case RTL_INTRINSIC_QUOTE:
    emitQuoteCode(C, fnID, x->as.quote);
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
    rtl_emitIntrinsicCode(C, fnID, x->as.binop.leftArg);
    rtl_emitIntrinsicCode(C, fnID, x->as.binop.rightArg);

    switch (x->type) {
    case RTL_INTRINSIC_IADD:
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_IADD);
      break;

    case RTL_INTRINSIC_ISUB:
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_ISUB);
      break;

    case RTL_INTRINSIC_IMUL:
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_IMUL);
      break;

    case RTL_INTRINSIC_IDIV:
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_IDIV);
      break;

    case RTL_INTRINSIC_IMOD:
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_IMOD);
      break;

    case RTL_INTRINSIC_LT:
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_LT);
      break;

    case RTL_INTRINSIC_LEQ:
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_LEQ);
      break;

    case RTL_INTRINSIC_GT:
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_GT);
      break;

    case RTL_INTRINSIC_GEQ:
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_GEQ);
      break;

    case RTL_INTRINSIC_EQ:
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_EQ);
      break;

    case RTL_INTRINSIC_NEQ:
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_NEQ);
      break;

    case RTL_INTRINSIC_ISO:
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_ISO);
      break;

    default:
      abort(); // Unreachable

    } break;

  case RTL_INTRINSIC_TYPE_PRED:
    rtl_emitIntrinsicCode(C, fnID, x->as.typePred.arg);

    switch (x->as.typePred.type) {
    case RTL_INT28:
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_IS_INT28);
      break;

    case RTL_FIX14:
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_IS_FIX14);
      break;

    case RTL_SYMBOL:
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_IS_SYMBOL);
      break;

    case RTL_SELECTOR:
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_IS_SELECTOR);
      break;

    case RTL_NIL:
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_IS_NIL);
      break;

    case RTL_CONS:
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_IS_CONS);
      break;

    case RTL_TUPLE:
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_IS_TUPLE);
      break;

    case RTL_TOP:
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_IS_TOP);
      break;

    default:
      abort(); // Type not handled yet...
    } break;

  case RTL_INTRINSIC_IF:
    if (x->as._if.then->codeSize + 5 < (1 << 7)) {
      thenJmpBytes = 1;
    } else if (x->as._if.then->codeSize + 5 < (1 << 15)) {
      thenJmpBytes = 2;
    } else {
      thenJmpBytes = 4;
    }

    if (x->as._if._else->codeSize + 5 < (1 << 7)) {
      elseJmpBytes = 1;
    } else if (x->as._if._else->codeSize + 5 < (1 << 15)) {
      elseJmpBytes = 2;
    } else {
      elseJmpBytes = 4;
    }

    rtl_emitIntrinsicCode(C, fnID, x->as._if.test);
    rtl_emitByteToFunc(codeBase, fnID, RTL_OP_NOT);
    switch (thenJmpBytes) {
    case 1:
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_CJMP8);
      rtl_emitByteToFunc(codeBase, fnID, x->as._if.then->codeSize + 1 + elseJmpBytes);
      break;

    case 2:
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_CJMP16);
      rtl_emitShortToFunc(codeBase, fnID, x->as._if.then->codeSize + 1 + elseJmpBytes);
      break;

    case 4:
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_CJMP32);
      rtl_emitWordToFunc(codeBase, fnID, x->as._if.then->codeSize + 1 + elseJmpBytes);
      break;
    }

    rtl_emitIntrinsicCode(C, fnID, x->as._if.then);

    switch (elseJmpBytes) {
    case 1:
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_JMP8);
      rtl_emitByteToFunc(codeBase, fnID, x->as._if._else->codeSize);
      break;

    case 2:
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_JMP16);
      rtl_emitByteToFunc(codeBase, fnID, x->as._if._else->codeSize);
      break;

    case 4:
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_JMP32);
      rtl_emitByteToFunc(codeBase, fnID, x->as._if._else->codeSize);
      break;
    }

    rtl_emitIntrinsicCode(C, fnID, x->as._if._else);

    break;

  case RTL_INTRINSIC_STRING:
    rtl_emitByteToFunc(codeBase, fnID, RTL_OP_STRING);
    rtl_emitWordToFunc(codeBase, fnID, x->as.string.strLen);
    rtl_emitStringToFunc(codeBase, fnID, x->as.string.str);
    break;

  case RTL_INTRINSIC_GENSYM:
    rtl_emitByteToFunc(codeBase, fnID, RTL_OP_GENSYM);
    break;

  case RTL_INTRINSIC_CONSTANT:
    if (x->as.constant == RTL_NIL) {
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_CONST_NIL);

    } else if (x->as.constant == RTL_TOP) {
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_CONST_TOP);

    } else if (x->as.constant == RTL_MAP) {
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_MAP);

    } else {
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_CONST);
      rtl_emitWordToFunc(codeBase, fnID, x->as.constant);

    } break;
  }
}
