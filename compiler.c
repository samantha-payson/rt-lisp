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

      switch (code[0]) {
      case RTL_OP_UNDEFINED_FUNCTION:
      case RTL_OP_STATIC_CALL:
	code[0] = RTL_OP_STATIC_CALL;
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

// Intern a symbol, with a given package and name. 
rtl_Word rtl_intern(char const *pkg, char const *name)
{
  uint32_t    pkgID;
  uint32_t    id;

  pkgID = rtl_internPackageID(pkg);
  id    = rtl_internSymbolID(pkgID, name);

  return rtl_symbol(id);
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
  M(call,         "call")			\
  M(namedCall,    "named-call")			\
  M(applyList,    "apply-list")			\
  M(applyTuple,   "apply-tuple")		\
  M(progn,        "progn")			\
  M(lambda,       "lambda")			\
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
  M(addrp,        "addr?")			\
  M(builtinp,     "builtin?")			\
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
  memset(C->fnsByName,       0, sizeof C->fnsByName);
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

  rptr = __rtl_reifyPtr(C->M, map);
  len  = __builtin_popcount(mask);

  wptr = rtl_allocGC(C->M, RTL_MAP, &newMap, 2*len);

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

  rptr = __rtl_reifyPtr(C->M, map);
  len  = __builtin_popcount(mask);

  wptr = rtl_allocGC(C->M, RTL_MAP, &newMap, 2*len);

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

rtl_Word rtl_macroExpand(rtl_Compiler *C, rtl_NameSpace const *ns, rtl_Word in)
{
  rtl_Word head  = RTL_NIL,
           arg   = RTL_NIL,
           name  = RTL_NIL,
           alias = RTL_NIL,
           tail  = RTL_NIL,
           out   = RTL_NIL;

  rtl_Word const *rptr;
  rtl_Word       *wptr;

  rtl_FnDef *fnDef;

  size_t len, i;

  rtl_NameSpace newNS;

  ensureSymCache(C);

  RTL_PUSH_WORKING_SET(C->M, &in, &head, &arg, &name, &tail, &out);

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

    } else {
      fnDef = rtl_lookupFn(C, head);

      if (fnDef != NULL && fnDef->isMacro) {
	printf("'%s:%s' is a '%s' which names a macro!\n",
	       rtl_symbolPackageName(head),
	       rtl_symbolName(head),
	       rtl_typeNameOf(head));

	out = rtl_macroExpand(C, ns, rtl_applyList(C->M,
						   fnDef->addr,
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

void rtl_compile(rtl_Compiler *C,
		 rtl_NameSpace const *ns,
		 uint16_t pageID,
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

  ensureSymCache(C);

  RTL_PUSH_WORKING_SET(C->M, &in, &head, &arg, &name, &tail, &out);

  switch (rtl_typeOf(in)) {
  case RTL_CONS:
    head = rtl_macroExpand(C, ns, rtl_car(C->M, in));

    if (head == symCache.intrinsic.inPackage) {
      name  = rtl_macroExpand(C, ns, rtl_cadr(C->M, in));
      newNS = rtl_nsInPackage(ns, rtl_internPackage(C, rtl_symbolName(name)));
      rtl_compile(C, &newNS, pageID,
		  rtl_cons(C->M, rtl_intern("intrinsic", "progn"),
			   rtl_cddr(C->M, in)));

      rtl_popWorkingSet(C->M);
      return;

    } else if (head == symCache.intrinsic.usePackage) {
      name  = rtl_macroExpand(C, ns, rtl_cadr(C->M, in));
      newNS = rtl_nsUsePackage(ns, rtl_internPackage(C, rtl_symbolName(name)));
      rtl_compile(C, &newNS, pageID,
		  rtl_cons(C->M, rtl_intern("intrinsic", "progn"),
			   rtl_cddr(C->M, in)));

      rtl_popWorkingSet(C->M);
      return;

    } else if (head == symCache.intrinsic.aliasPackage) {
      name  = rtl_macroExpand(C, ns, rtl_car(C->M, rtl_cadr(C->M, in)));
      alias = rtl_macroExpand(C, ns, rtl_cadr(C->M, rtl_cadr(C->M, in)));
      newNS = rtl_nsAliasPackage(ns, rtl_internPackage(C, rtl_symbolName(name)),
				 rtl_symbolName(alias));
      rtl_compile(C, &newNS, pageID,
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
	rtl_compile(C, ns, pageID, rtl_car(C->M, tail));

	if (rtl_cdr(C->M, tail) != RTL_NIL) {
	  rtl_emitByteToPage(C->M, pageID, RTL_OP_POP);
	}
      }

      rtl_popWorkingSet(C->M);
      return;

    }

  default:
    out = rtl_macroExpand(C, ns, in);
    break;
  }

  ir = rtl_exprToIntrinsic(C, out);
  ir = rtl_transformIntrinsic(ir);
  rtl_emitIntrinsicCode(C, pageID, ir);

  rtl_popWorkingSet(C->M);
}

rtl_Intrinsic *mapToIntrinsic(rtl_Compiler  *C,
			      rtl_Intrinsic *soFar,
			      rtl_Word      map,
			      uint32_t      mask)
{
  rtl_Word const *rptr, *entry;
  size_t   len,
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
  rtl_Word head, tail, name;
  rtl_Word const *rptr;
  size_t len, i;
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

  printf("Generating intrinsics for: ");
  rtl_formatExpr(C->M, sxp);
  printf("\n");

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

    } else if (head == symCache.intrinsic.addrp) {
      assert(len == 2);
      return rtl_mkTypePredIntrinsic(RTL_ADDR,
				     rtl_exprToIntrinsic(C, rtl_cadr(C->M, sxp)));

    } else if (head == symCache.intrinsic.builtinp) {
      assert(len == 2);
      return rtl_mkTypePredIntrinsic(RTL_BUILTIN,
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

  case RTL_INTRINSIC_TUPLE:
    for (i = 0; i < x->as.tuple.elemsLen; i++) {
      x->as.tuple.elems[i] = __impl_transformIntrinsic(env, x->as.tuple.elems[i]);
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
    } // If lookupVar returned true, then frame and idx were set.
    break;

  case RTL_INTRINSIC_NAMED_CALL:
    abort(); // This should never happen

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
void emitQuoteCode(rtl_Compiler *C, uint16_t pageID, rtl_Word expr);

static
void emitMapQuoteCode(rtl_Compiler        *C,
		      uint16_t            pageID,
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
      emitMapQuoteCode(C, pageID, entry[1], rtl_headerValue(entry[0]));
    } else {
      emitQuoteCode(C, pageID, entry[0]);
      emitQuoteCode(C, pageID, entry[1]);
      rtl_emitByteToPage(C->M, pageID, RTL_OP_INSERT);
    }
  }
}

static
void emitQuoteCode(rtl_Compiler *C, uint16_t pageID, rtl_Word expr)
{
  rtl_Word const *rptr;
  size_t i, len;

  switch (rtl_typeOf(expr)) {
  case RTL_NIL:
    rtl_emitByteToPage(C->M, pageID, RTL_OP_CONST_NIL);
    break;

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

  case RTL_MAP:
    rtl_emitByteToPage(C->M, pageID, RTL_OP_MAP);
    if (!rtl_isEmptyMap(expr)) {
      emitMapQuoteCode(C, pageID, expr, 1);
    } break;

  case RTL_TUPLE:
    rptr = rtl_reifyTuple(C->M, expr, &len);
    for (i = 0; i < len; i++) {
      emitQuoteCode(C, pageID, rptr[i]);
    }
    rtl_emitByteToPage(C->M, pageID, RTL_OP_TUPLE);
    rtl_emitShortToPage(C->M, pageID, len);
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

  case RTL_INTRINSIC_TUPLE:
    assert(x->as.tuple.elemsLen < (1 << 16));

    for (i = 0; i < x->as.tuple.elemsLen; i++) {
      rtl_emitIntrinsicCode(C, pageID, x->as.tuple.elems[i]);
    }

    rtl_emitByteToPage(C->M, pageID, RTL_OP_TUPLE);
    rtl_emitShortToPage(C->M, pageID, x->as.tuple.elemsLen);
    break;

  case RTL_INTRINSIC_LEN:
    rtl_emitIntrinsicCode(C, pageID, x->as.len.tuple);
    rtl_emitByteToPage(C->M, pageID, RTL_OP_LEN);
    break;

  case RTL_INTRINSIC_GET:
    rtl_emitIntrinsicCode(C, pageID, x->as.get.tuple);
    rtl_emitIntrinsicCode(C, pageID, x->as.get.index);
    rtl_emitByteToPage(C->M, pageID, RTL_OP_GET);
    break;

  case RTL_INTRINSIC_INSERT:
    rtl_emitIntrinsicCode(C, pageID, x->as.insert.map);
    rtl_emitIntrinsicCode(C, pageID, x->as.insert.key);
    rtl_emitIntrinsicCode(C, pageID, x->as.insert.val);
    rtl_emitByteToPage(C->M, pageID, RTL_OP_INSERT);
    break;

  case RTL_INTRINSIC_LOOKUP:
    rtl_emitIntrinsicCode(C, pageID, x->as.lookup.map);
    rtl_emitIntrinsicCode(C, pageID, x->as.lookup.key);
    rtl_emitByteToPage(C->M, pageID, RTL_OP_LOOKUP);
    break;

  case RTL_INTRINSIC_VAR:
    if (x->as.var.global) {
      fnDef = rtl_lookupFn(C, x->as.var.name);

      if (fnDef != NULL && !fnDef->isMacro) {
	addr0 = rtl_emitByteToPage(C->M, pageID, RTL_OP_CONST);

	rtl_emitWordToPage(C->M, pageID, fnDef->addr);
      } else {
	addr0 = rtl_emitByteToPage(C->M, pageID, RTL_OP_UNDEFINED_VAR);

	rtl_emitWordToPage(C->M, pageID, x->as.var.name);
      }

      rtl_registerCallSite(C, x->as.var.name, addr0);

    } else {
      rtl_emitByteToPage(C->M, pageID, RTL_OP_VAR);
      rtl_emitShortToPage(C->M, pageID, x->as.var.frame);
      rtl_emitShortToPage(C->M, pageID, x->as.var.idx);
    }
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

  case RTL_INTRINSIC_APPLY_LIST:
    rtl_emitIntrinsicCode(C, pageID, x->as.applyList.fn);
    rtl_emitIntrinsicCode(C, pageID, x->as.applyList.arg);
    rtl_emitByteToPage(C->M, pageID, RTL_OP_APPLY_LIST);
    break;

  case RTL_INTRINSIC_APPLY_TUPLE:
    rtl_emitIntrinsicCode(C, pageID, x->as.applyList.fn);
    rtl_emitIntrinsicCode(C, pageID, x->as.applyList.arg);
    rtl_emitByteToPage(C->M, pageID, RTL_OP_APPLY_TUPLE);
    break;

  case RTL_INTRINSIC_PROGN:
    if (x->as.progn.formsLen == 0) {
      rtl_emitByteToPage(C->M, pageID, RTL_OP_CONST_NIL);
    } else {
      for (i = 0; i < x->as.progn.formsLen; i++) {
	rtl_emitIntrinsicCode(C, pageID, x->as.progn.forms[i]);

	// Ignore the result of all but the last expression.
	if (i + 1 < x->as.progn.formsLen)
	  rtl_emitByteToPage(C->M, pageID, RTL_OP_POP);
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

    printf("  _____\n");
    printf(" | Compiled function '%s' to page %d |\n",
	   rtl_symbolName(x->as.defun.name),
	   (int)newPageID);
    printf("       ------------");
    rtl_disasmPage(C->M, newPageID);

    rtl_emitByteToPage(C->M, pageID, RTL_OP_CONST);
    rtl_emitWordToPage(C->M, pageID, x->as.defun.name);
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

    rtl_emitByteToPage(C->M, pageID, RTL_OP_CONST);
    rtl_emitWordToPage(C->M, pageID, x->as.defun.name);
    break;

  case RTL_INTRINSIC_EXPORT:
    rtl_export(C, x->as.export);
    rtl_emitByteToPage(C->M, pageID, RTL_OP_CONST_TOP);
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

    default:
      abort(); // Unreachable

    } break;

  case RTL_INTRINSIC_TYPE_PRED:
    rtl_emitIntrinsicCode(C, pageID, x->as.typePred.arg);

    switch (x->as.typePred.type) {
    case RTL_INT28:
      rtl_emitByteToPage(C->M, pageID, RTL_OP_IS_INT28);
      break;

    case RTL_FIX14:
      rtl_emitByteToPage(C->M, pageID, RTL_OP_IS_FIX14);
      break;

    case RTL_SYMBOL:
      rtl_emitByteToPage(C->M, pageID, RTL_OP_IS_SYMBOL);
      break;

    case RTL_SELECTOR:
      rtl_emitByteToPage(C->M, pageID, RTL_OP_IS_SELECTOR);
      break;

    case RTL_NIL:
      rtl_emitByteToPage(C->M, pageID, RTL_OP_IS_NIL);
      break;

    case RTL_CONS:
      rtl_emitByteToPage(C->M, pageID, RTL_OP_IS_CONS);
      break;

    case RTL_TUPLE:
      rtl_emitByteToPage(C->M, pageID, RTL_OP_IS_TUPLE);
      break;

    case RTL_TOP:
      rtl_emitByteToPage(C->M, pageID, RTL_OP_IS_TOP);
      break;

    default:
      abort(); // Type not handled yet...
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

    } else if (x->as.constant == RTL_MAP) {
      rtl_emitByteToPage(C->M, pageID, RTL_OP_MAP);

    } else {
      rtl_emitByteToPage(C->M, pageID, RTL_OP_CONST);
      rtl_emitWordToPage(C->M, pageID, x->as.constant);

    } break;
  }
}
