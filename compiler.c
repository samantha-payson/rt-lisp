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
      case RTL_OP_UNDEF_CALL:
      case RTL_OP_STATIC_CALL:
        code[0] = RTL_OP_STATIC_CALL;
        code[1] = (fn >>  0) & 0xFF;
        code[2] = (fn >>  8) & 0xFF;
        code[3] = (fn >> 16) & 0xFF;
        code[4] = (fn >> 24) & 0xFF;
        break;

      case RTL_OP_UNDEF_TAIL:
      case RTL_OP_STATIC_TAIL:
        code[0] = RTL_OP_STATIC_TAIL;
        code[1] = (fn >>  0) & 0xFF;
        code[2] = (fn >>  8) & 0xFF;
        code[3] = (fn >> 16) & 0xFF;
        code[4] = (fn >> 24) & 0xFF;
        break;

      case RTL_OP_UNDEF_VAR:
      case RTL_OP_CONST32:
        code[0] = RTL_OP_CONST32;
        code[1] = (fn >>  0) & 0xFF;
        code[2] = (fn >>  8) & 0xFF;
        code[3] = (fn >> 16) & 0xFF;
        code[4] = (fn >> 24) & 0xFF;
        break;

      default:
        printf("   !!! Invalid call site !!!\n");
        __builtin_unreachable();
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
      if (isMacro) {
        def->macro = fn;
      } else {
        def->fn = fn;
      }
      return;
    }
  }

  // If we made it here, there is no existing entry by this name... create one.

  def = malloc(sizeof(rtl_FnDef));

  def->name    = name;
  if (isMacro) {
    def->macro = fn;
    def->fn    = RTL_NIL;
  } else {
    def->fn    = fn;
    def->macro = RTL_NIL;
  }

  def->next                = codeBase->fnsByName[idx];
  codeBase->fnsByName[idx] = def;
}

rtl_FnDef *rtl_lookupFn(rtl_CodeBase *codeBase, rtl_Word name)
{
  rtl_FnDef *def;
  size_t    idx;

  if (!rtl_isSymbol(name)) {
    return NULL;
  }

  idx = rtl_symbolID(name) % RTL_CODE_BASE_FN_HASH_SIZE;

  for (def = codeBase->fnsByName[idx]; def != NULL; def = def->next)
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

#define MAP_INTRINSICS(M)                       \
  M(cons,         "cons")                       \
  M(car,          "car")                        \
  M(cdr,          "cdr")                        \
  M(tuple,        "tuple")                      \
  M(len,          "len")                        \
  M(get,          "get")                        \
  M(pushFirst,    "push-first")                 \
  M(pushLast,     "push-last")                  \
  M(concat,       "concat")                     \
  M(slice,        "slice")                      \
  M(map,          "map")                        \
  M(insert,       "insert")                     \
  M(lookup,       "lookup")                     \
  M(dynGet,       "dyn-get")                    \
  M(dynSet,       "dyn-set")                    \
  M(bind,         "bind")                       \
  M(var,          "var")                        \
  M(gensym,       "gensym")                     \
  M(call,         "call")                       \
  M(yield,        "yield")                      \
  M(namedCall,    "named-call")                 \
  M(applyList,    "apply-list")                 \
  M(applyTuple,   "apply-tuple")                \
  M(progn,        "progn")                      \
  M(protect,      "protect")                    \
  M(lambda,       "lambda")                     \
  M(labels,       "labels")                     \
  M(defun,        "defun")                      \
  M(defmacro,     "defmacro")                   \
  M(load,         "load")                       \
  M(quote,        "quote")                      \
  M(inPackage,    "in-package")                 \
  M(usePackage,   "use-package")                \
  M(aliasPackage, "alias-package")              \
  M(alias,        "alias")                      \
  M(export,       "export")                     \
  M(iadd,         "iadd")                       \
  M(isub,         "isub")                       \
  M(imul,         "imul")                       \
  M(idiv,         "idiv")                       \
  M(imod,         "imod")                       \
  M(lt,           "lt")                         \
  M(leq,          "leq")                        \
  M(gt,           "gt")                         \
  M(geq,          "geq")                        \
  M(eq,           "eq")                         \
  M(neq,          "neq")                        \
  M(iso,          "iso")                        \
  M(nilp,         "nil?")                       \
  M(symbolp,      "symbol?")                    \
  M(selectorp,    "selector?")                  \
  M(int28p,       "int28?")                     \
  M(fix14p,       "fix14?")                     \
  M(tuplep,       "tuple?")                     \
  M(charp,        "char?")                      \
  M(mapp,         "map?")                       \
  M(consp,        "cons?")                      \
  M(functionp,    "function?")                  \
  M(closurep,     "closure?")                   \
  M(unresolvedp,  "unresolved?")                \
  M(topp,         "top?")                       \
  M(_if,          "if")                         \
  M(setVar,       "set-var")                    \
  M(setCar,       "set-car")                    \
  M(setCdr,       "set-cdr")                    \
  M(setElem,      "set-elem")                   \
  // End of multi-line macro

#define DECLARE_INTRINSIC_WORD(CNAME, LISPNAME) CNAME,

static struct symCache_t {
  struct {
    rtl_Word MAP_INTRINSICS(DECLARE_INTRINSIC_WORD)
      ___ignore___; // Need an identifier here so the trailing comma isn't a
                    // problem...
  } intrinsic;
} symCache;

#undef DECLARE_INTRINSIC_WORD

static bool symCacheWasInit;

#define CACHE_INTRINSIC_SYM(CNAME, LISPNAME)    \
  .CNAME = rtl_intern("intrinsic", LISPNAME),   \
  // End of multi-line macro

#define EXPORT_INTRINSIC(CNAME, LISPNAME)     \
  rtl_xExport(C, symCache.intrinsic.CNAME);   \
  RTL_ASSERT_NO_UNWIND(C->M);                 \
  // End of multi-line macro

static inline
void ensureSymCache(rtl_Compiler *C) {
  if (unlikely(!symCacheWasInit)) {
    symCacheWasInit = true;

    rtl_internPackage(C, "intrinsic");

    symCache = (struct symCache_t) {
      .intrinsic = {
        MAP_INTRINSICS(CACHE_INTRINSIC_SYM)
      },
    };
  }

  MAP_INTRINSICS(EXPORT_INTRINSIC)
}

#undef CACHE_INTRINSIC_SYM

#undef EXPORT_INTRINSIC

void rtl_initCompiler(rtl_Compiler *C, rtl_Machine *M) {
  C->M = M;

  memset(C->callSitesByName, 0, sizeof C->callSitesByName);
  memset(C->pkgByID,         0, sizeof C->pkgByID);
}

rtl_Word rtl_xResolveAll(rtl_Compiler *C,
                         rtl_NameSpace const *ns,
                         rtl_Word sxp);

typedef struct mapAccum {
  rtl_Compiler         *C;
  rtl_NameSpace const  *ns;
  rtl_Word             map;
} mapAccum;

static
void __resolveMapEntry(rtl_Machine *M,
                      void         *vaccum,
                      rtl_Word     key,
                      rtl_Word     val)
{
  mapAccum *acc = (mapAccum *)vaccum;

  RTL_PUSH_WORKING_SET(M, &key, &val);

  key = rtl_xResolveAll(acc->C, acc->ns, key);
  RTL_UNWIND (M) goto cleanup;

  val = rtl_xResolveAll(acc->C, acc->ns, val);
  RTL_UNWIND (M) goto cleanup;

  acc->map = rtl_xMapInsert(M, acc->map, key, val);

 cleanup:
  rtl_popWorkingSet(M);
}

rtl_Word rtl_xResolveMap(rtl_Compiler *C,
                        rtl_NameSpace const *ns,
                        uint32_t mask,
                        rtl_Word map)
{
  mapAccum acc = {
    .C   = C,
    .ns  = ns,
    .map = RTL_MAP,
  };

  RTL_PUSH_WORKING_SET(C->M, &map, &acc.map);

  rtl_xVisitMap(C->M, &acc, __resolveMapEntry, map);

  rtl_popWorkingSet(C->M);

  return acc.map;
}

rtl_Word rtl_xResolveAll(rtl_Compiler *C,
                         rtl_NameSpace const *ns,
                         rtl_Word in)
{
  rtl_Word a = RTL_NIL, b = RTL_NIL, out = RTL_NIL;

  rtl_Word const *rptr;
  rtl_Word *wptr;
  size_t i, len;

  RTL_PUSH_WORKING_SET(C->M, &in, &a, &b, &out);

  switch (rtl_typeOf(in)) {
  case RTL_UNRESOLVED_SYMBOL:
    out = rtl_xResolveSymbol(C, ns, in);
    break;

  case RTL_MAP:
    out = rtl_xResolveMap(C, ns, 1, in);
    break;

  case RTL_TUPLE:
    rtl_xReifyTuple(C->M, in, &len);
    RTL_ASSERT_NO_UNWIND(C->M);

    wptr = rtl_allocTuple(C->M, &out, len);
    rptr = rtl_xReifyTuple(C->M, in, &len);
    RTL_ASSERT_NO_UNWIND(C->M);

    for (i = 0; i < len; i++) {
      wptr[i] = rtl_xResolveAll(C, ns, rptr[i]);
      rptr = rtl_xReifyTuple(C->M, in, &len);
      RTL_UNWIND (C->M) break;
    } break;
    
  case RTL_CONS:
    a = rtl_xCar(C->M, in);
    RTL_ASSERT_NO_UNWIND(C->M);

    a = rtl_xResolveAll(C, ns, a);
    RTL_UNWIND (C->M) break;

    b = rtl_xCdr(C->M, in);
    RTL_ASSERT_NO_UNWIND(C->M);

    b = rtl_xResolveAll(C, ns, b);
    RTL_UNWIND (C->M) break;

    out = rtl_cons(C->M, a, b);
    break;

  default:
    out = in;
    break;
  }

  rtl_popWorkingSet(C->M);

  return out;
}

static
void __macroExpandEntry(rtl_Machine *M,
                        void        *vaccum,
                        rtl_Word    key,
                        rtl_Word    val)
{
  mapAccum *acc = (mapAccum *)vaccum;

  RTL_PUSH_WORKING_SET(M, &key, &val);

  key = rtl_xMacroExpand(acc->C, acc->ns, key);
  RTL_UNWIND (M) goto cleanup;

  val = rtl_xMacroExpand(acc->C, acc->ns, val);
  RTL_UNWIND (M) goto cleanup;

  acc->map = rtl_xMapInsert(M, acc->map, key, val);

cleanup:
  rtl_popWorkingSet(M);
}

static
rtl_Word macroExpandMap(rtl_Compiler        *C,
                        rtl_NameSpace const *ns,
                        rtl_Word            map)
{
  mapAccum acc = {
    .C = C,
    .ns = ns,
    .map = RTL_MAP,
  };

  RTL_PUSH_WORKING_SET(C->M, &map, &acc.map);

  rtl_xVisitMap(C->M, &acc, __macroExpandEntry, map);

  rtl_popWorkingSet(C->M);

  return acc.map;
}

// Expects everything after the 'lambda' symbol of a lambda expression.
//
// e.g. ((arg0 arg1 arg2) body)
rtl_Word rtl_xMacroExpandLambda(rtl_Compiler *C, rtl_NameSpace const *ns, rtl_Word in)
{
  rtl_Word arg, out, tail;

  arg = out = tail = RTL_NIL;

  RTL_PUSH_WORKING_SET(C->M, &in, &arg, &out, &tail);

  arg = rtl_xCar(C->M, in);
  RTL_UNWIND (C->M) goto cleanup;

  arg = rtl_xResolveAll(C, ns, arg);
  RTL_UNWIND (C->M) goto cleanup;

  out = rtl_cons(C->M, arg, RTL_NIL);

  for (tail = rtl_xCdr(C->M, in); tail != RTL_NIL; tail = rtl_xCdr(C->M, tail)) {
    RTL_UNWIND (C->M) goto cleanup;

    arg = rtl_xCar(C->M, tail);
    RTL_UNWIND (C->M) goto cleanup;

    arg = rtl_xMacroExpand(C, ns, arg);
    RTL_UNWIND (C->M) goto cleanup;

    out = rtl_cons(C->M, arg, out);
  }

  out = rtl_xReverseList(C->M, out);

cleanup:
  rtl_popWorkingSet(C->M);

  return out;
}

rtl_Word rtl_xMacroExpand(rtl_Compiler *C, rtl_NameSpace const *ns, rtl_Word in)
{
  rtl_Word head   = RTL_NIL,
           arg    = RTL_NIL,
           clause = RTL_NIL,
           name   = RTL_NIL,
           alias  = RTL_NIL,
           tail   = RTL_NIL,
           tmp    = RTL_NIL,
           out    = RTL_NIL;

  rtl_Word const *rptr;
  rtl_Word       *wptr;

  rtl_FnDef *fnDef;

  size_t len, i;

  rtl_NameSpace newNS;

  ensureSymCache(C);

  RTL_PUSH_WORKING_SET(C->M, &in, &head, &arg, &clause, &name, &alias, &tail, &tmp, &out);

  switch (rtl_typeOf(in)) {
  case RTL_UNRESOLVED_SYMBOL:
    out = rtl_xResolveSymbol(C, ns, in);
    break;

  case RTL_TUPLE:
    rtl_xReifyTuple(C->M, in, &len);
    RTL_ASSERT_NO_UNWIND(C->M);

    wptr = rtl_allocTuple(C->M, &out, len);
    rptr = rtl_xReifyTuple(C->M, in, &len);
    RTL_ASSERT_NO_UNWIND(C->M);
    
    for (i = 0; i < len; i++) {
      tmp     = rtl_xMacroExpand(C, ns, rptr[i]);
      rptr    = rtl_xReifyTuple(C->M, in, &len);
      wptr    = (rtl_Word *)rtl_xReifyTuple(C->M, out, &len);
      wptr[i] = tmp;
      RTL_UNWIND (C->M) break;
    } break;

  case RTL_MAP:
    out = macroExpandMap(C, ns, in);
    break;

  case RTL_CONS:
    head = rtl_xCar(C->M, in);
    RTL_ASSERT_NO_UNWIND(C->M);

    head = rtl_xMacroExpand(C, ns, head);
    RTL_UNWIND (C->M) break;

    if (head == symCache.intrinsic.inPackage) {
      name = rtl_xCadr(C->M, in);
      RTL_UNWIND (C->M) break;

      name = rtl_xMacroExpand(C, ns, name);
      RTL_UNWIND (C->M) break;

      tail = rtl_xCddr(C->M, in);
      RTL_UNWIND (C->M) break;

      newNS = rtl_nsInPackage(ns, rtl_internPackage(C, rtl_symbolName(name)));
      out   = rtl_xMacroExpand(C, &newNS,
                               rtl_cons(C->M,
                                        rtl_intern("intrinsic", "progn"),
                                        tail));

    } else if (head == symCache.intrinsic.usePackage) {
      name  = rtl_xCadr(C->M, in); 
      RTL_UNWIND (C->M) break;

      name  = rtl_xMacroExpand(C, ns, name);
      RTL_UNWIND (C->M) break;

      tail = rtl_xCddr(C->M, in);
      RTL_UNWIND (C->M) break;

      newNS = rtl_nsUsePackage(ns, rtl_internPackage(C, rtl_symbolName(name)));
      out   = rtl_xMacroExpand(C, &newNS,
                               rtl_cons(C->M,
                                        rtl_intern("intrinsic", "progn"),
                                        tail));

    } else if (head == symCache.intrinsic.aliasPackage) {
      name = rtl_xCaadr(C->M, in);
      RTL_UNWIND (C->M) break;

      name = rtl_xMacroExpand(C, ns, name);
      RTL_UNWIND (C->M) break;

      alias = rtl_xCadr(C->M, rtl_xCadr(C->M, in));
      RTL_UNWIND (C->M) break;

      alias = rtl_xMacroExpand(C, ns, alias);
      newNS = rtl_nsAliasPackage(ns, rtl_internPackage(C, rtl_symbolName(name)),
                                 rtl_symbolName(alias));
      tail = rtl_xCddr(C->M, in);
      RTL_UNWIND (C->M) break;

      out = rtl_xMacroExpand(C, &newNS,
                             rtl_cons(C->M,
                                      rtl_intern("intrinsic", "progn"),
                                      tail));

    } else if (head == symCache.intrinsic.alias) {
      out = in;

    } else if (head == symCache.intrinsic.quote) {
      out = rtl_xResolveAll(C, ns, in);

    } else if (head == symCache.intrinsic.lambda) {
      tail = rtl_xCdr(C->M, in);
      RTL_UNWIND (C->M) break;

      out = rtl_cons(C->M, head,
                     rtl_xMacroExpandLambda(C, ns, tail));

    } else if (head == symCache.intrinsic.labels) {
      arg = RTL_NIL;

      for (clause = rtl_xCadr(C->M, in);
           clause != RTL_NIL;
           clause = rtl_xCdr(C->M, clause))
      {
        name = rtl_xCaar(C->M, clause);
        RTL_UNWIND (C->M) break;

        name = rtl_xResolveSymbol(C, ns, name);

        tail = rtl_xCdar(C->M, clause);
        RTL_UNWIND (C->M) break;

        tail = rtl_xMacroExpandLambda(C, ns, tail);
        RTL_UNWIND (C->M) break;

        arg = rtl_cons(C->M, rtl_cons(C->M, name, tail),
                        arg);
      }

      RTL_UNWIND (C->M) break;

      out = rtl_cons(C->M, arg,
                     rtl_cons(C->M, head, RTL_NIL));

      for (tail = rtl_xCddr(C->M, in); tail != RTL_NIL; tail = rtl_xCdr(C->M, tail)) {
        RTL_UNWIND (C->M) break;

        arg = rtl_xCar(C->M, tail);
        RTL_UNWIND (C->M) break;

        arg = rtl_xMacroExpand(C, ns, arg);
        RTL_UNWIND (C->M) break;

        out = rtl_cons(C->M, arg, out);
      }

      RTL_UNWIND (C->M) break;

      out = rtl_xReverseList(C->M, out);

    } else if (head == symCache.intrinsic.defun || head == symCache.intrinsic.defmacro) {
      tail = rtl_xCddr(C->M, in);
      RTL_UNWIND (C->M) break;

      tail = rtl_xMacroExpandLambda(C, ns, tail);
      RTL_UNWIND (C->M) break;

      name = rtl_xCadr(C->M, in);
      RTL_UNWIND (C->M) break;

      name = rtl_xResolveSymbol(C, ns, name);
      RTL_UNWIND (C->M) break;

      tail = rtl_cons(C->M, name, tail);
      out = rtl_cons(C->M, head, tail);

    } else if (head == symCache.intrinsic.dynGet) {
      out = rtl_xResolveAll(C, ns, in);

    } else if (head == symCache.intrinsic.dynSet) {
      tail = rtl_xCaddr(C->M, in);
      RTL_UNWIND (C->M) break;

      tail = rtl_xMacroExpand(C, ns, tail);
      RTL_UNWIND (C->M) break;

      tail = rtl_cons(C->M, tail, RTL_NIL);

      name = rtl_xCadr(C->M, in);
      RTL_UNWIND (C->M) break;

      name = rtl_xResolveSymbol(C, ns, name);
      RTL_UNWIND (C->M) break;

      tail = rtl_cons(C->M, name, tail);

      out = rtl_cons(C->M, head, tail);

    } else if (head == symCache.intrinsic.bind) {
      arg = rtl_xCar(C->M, rtl_xCdadr(C->M, in));
      RTL_UNWIND (C->M) break;

      arg = rtl_xMacroExpand(C, ns, arg);
      RTL_UNWIND (C->M) break;

      arg = rtl_cons(C->M, arg, RTL_NIL);

      name = rtl_xCaadr(C->M, in);
      RTL_UNWIND (C->M) break;

      name = rtl_xResolveSymbol(C, ns, name);
      RTL_UNWIND (C->M) break;

      clause = rtl_cons(C->M, name, clause);

      out = RTL_NIL;

      for (tail = rtl_xCddr(C->M, in); tail != RTL_NIL; tail = rtl_xCdr(C->M, tail)) {
        RTL_UNWIND (C->M) break;

        arg = rtl_xCar(C->M, tail);
        RTL_UNWIND (C->M) break;

        arg = rtl_xMacroExpand(C, ns, arg);
        RTL_UNWIND (C->M) break;

        out = rtl_cons(C->M, arg, out);
      }

      RTL_UNWIND (C->M) break;

      out = rtl_xReverseList(C->M, out);
      RTL_UNWIND (C->M) break;
      
      out = rtl_cons(C->M, head,
                     rtl_cons(C->M, clause,
                              out));

    } else {
      fnDef = rtl_lookupFn(C->M->codeBase, head);

      if (fnDef != NULL && fnDef->macro != RTL_NIL) {
        out = rtl_xCdr(C->M, in);
        RTL_UNWIND (C->M) break;

        out = rtl_xApplyList(C->M, fnDef->macro, out);
        RTL_UNWIND (C->M) break;

        out = rtl_xMacroExpand(C, ns, out);

     } else {
        out = rtl_cons(C->M, head, RTL_NIL);

        for (tail = rtl_xCdr(C->M, in); rtl_isCons(tail); tail = rtl_xCdr(C->M, tail))
        {
          RTL_UNWIND (C->M) break;

          arg = rtl_xCar(C->M, tail);
          RTL_UNWIND (C->M) break;

          arg = rtl_xMacroExpand(C, ns, arg);
          RTL_UNWIND (C->M) break;

          out = rtl_cons(C->M, arg, out);
        }

        RTL_UNWIND (C->M) break;

        tail = rtl_xMacroExpand(C, ns, tail);
        RTL_UNWIND (C->M) break;

        out = rtl_xReverseListImproper(C->M, out, tail);
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

void rtl_xCompile(rtl_Compiler *C,
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

  rtl_FnDef *fnDef;

  rtl_Intrinsic *ir;

  rtl_CodeBase *codeBase = C->M->codeBase;

  ensureSymCache(C);

  RTL_PUSH_WORKING_SET(C->M, &in, &head, &arg, &name, &alias, &tail, &out);

  switch (rtl_typeOf(in)) {
  case RTL_CONS:
    head = rtl_xCar(C->M, in);
    RTL_UNWIND (C->M) goto cleanup;

    head = rtl_xMacroExpand(C, ns, head);
    RTL_UNWIND (C->M) goto cleanup;

    if (head == symCache.intrinsic.inPackage) {
      name = rtl_xCadr(C->M, in);
      RTL_UNWIND (C->M) goto cleanup;

      name = rtl_xMacroExpand(C, ns, name);
      RTL_UNWIND (C->M) goto cleanup;

      newNS = rtl_nsInPackage(ns, rtl_internPackage(C, rtl_symbolName(name)));

      tail = rtl_xCddr(C->M, in);
      RTL_UNWIND (C->M) goto cleanup;

      rtl_xCompile(C, &newNS, fnID,
                   rtl_cons(C->M, rtl_intern("intrinsic", "progn"), tail));

      goto cleanup;

    } else if (head == symCache.intrinsic.usePackage) {
      name = rtl_xCadr(C->M, in);
      RTL_UNWIND (C->M) goto cleanup;

      name = rtl_xMacroExpand(C, ns, name);
      RTL_UNWIND (C->M) goto cleanup;

      newNS = rtl_nsUsePackage(ns, rtl_internPackage(C, rtl_symbolName(name)));

      tail = rtl_xCddr(C->M, in);
      RTL_UNWIND (C->M) goto cleanup;

      rtl_xCompile(C, &newNS, fnID,
                   rtl_cons(C->M, rtl_intern("intrinsic", "progn"), tail));

      goto cleanup;

    } else if (head == symCache.intrinsic.aliasPackage) {
      name = rtl_xCaadr(C->M, in);
      RTL_UNWIND (C->M) goto cleanup;

      name  = rtl_xMacroExpand(C, ns, name);
      RTL_UNWIND (C->M) goto cleanup;

      alias = rtl_xCadr(C->M, rtl_xCadr(C->M, in));
      RTL_UNWIND (C->M) goto cleanup;

      alias = rtl_xMacroExpand(C, ns, alias);
      RTL_UNWIND (C->M) goto cleanup;

      newNS = rtl_nsAliasPackage(ns, rtl_internPackage(C, rtl_symbolName(name)),
                                 rtl_symbolName(alias));

      tail = rtl_xCddr(C->M, in);
      RTL_UNWIND (C->M) goto cleanup;

      rtl_xCompile(C, &newNS, fnID,
                   rtl_cons(C->M, rtl_intern("intrinsic", "progn"), tail));

      goto cleanup;

    } else if (head == symCache.intrinsic.alias) {
      printf("\n   !!! alias not yet supported !!!\n\n");
      abort();

    } else if (head == symCache.intrinsic.progn) {
      if (rtl_xCdr(C->M, in) == RTL_NIL) {
        RTL_UNWIND (C->M) goto cleanup;

        rtl_emitByteToFunc(codeBase, fnID, RTL_OP_NIL);
      } else {
        for (tail = rtl_xCdr(C->M, in);
             tail != RTL_NIL;
             tail = rtl_xCdr(C->M, tail))
        {
          RTL_UNWIND (C->M) goto cleanup;

          arg = rtl_xCar(C->M, tail);
          RTL_UNWIND (C->M) goto cleanup;

          rtl_xCompile(C, ns, fnID, arg);
          RTL_UNWIND (C->M) goto cleanup;

          if (rtl_xCdr(C->M, tail) != RTL_NIL) {
            RTL_UNWIND (C->M) goto cleanup;

            rtl_emitByteToFunc(codeBase, fnID, RTL_OP_POP);
          }
        }
      }

      goto cleanup;
    }

    fnDef = rtl_lookupFn(C->M->codeBase, head);

    if (fnDef != NULL && fnDef->macro != RTL_NIL) {
      arg = rtl_xCdr(C->M, in);
      RTL_UNWIND (C->M) goto cleanup;

      arg = rtl_xApplyList(C->M, fnDef->macro, arg);
      RTL_UNWIND (C->M) goto cleanup;

      rtl_xCompile(C, ns, fnID, arg);

      goto cleanup;
    }

    // Fallthrough ...

  default:
    out = rtl_xMacroExpand(C, ns, in);
    RTL_UNWIND (C->M) goto cleanup;

    break;
  }

  ir = rtl_xExprToIntrinsic(C, out);
  RTL_UNWIND (C->M) goto cleanup;

  ir = rtl_transformIntrinsic(C, ir);

  rtl_tailCallPass(ir);
  annotateCodeSize(C->M, ir);

  rtl_emitIntrinsicCode(C, fnID, ir);

cleanup:
  rtl_popWorkingSet(C->M);
}

typedef struct intrAccum {
  rtl_Compiler  *C;
  rtl_Intrinsic *intr;
} intrAccum;

static
void __entryToIntrinsic(rtl_Machine *M,
                        void        *vaccum,
                        rtl_Word    key,
                        rtl_Word    val)
{
  intrAccum *acc = (intrAccum *)vaccum;

  rtl_Intrinsic *iKey, *iVal;

  iKey = rtl_xExprToIntrinsic(acc->C, key);
  RTL_UNWIND (M) return;

  iVal = rtl_xExprToIntrinsic(acc->C, val);
  RTL_UNWIND (M) return;

  acc->intr = rtl_mkInsertIntrinsic(acc->intr, iKey, iVal);
}

static
rtl_Intrinsic *xMapToIntrinsic(rtl_Compiler  *C,
                               rtl_Word      map)
{
  intrAccum acc = {
    .C    = C,
    .intr = rtl_mkConstantIntrinsic(RTL_MAP),
  };

  rtl_xVisitMap(C->M, &acc, __entryToIntrinsic, map);

  return acc.intr;
}

rtl_Intrinsic *rtl_xExprToIntrinsic(rtl_Compiler *C, rtl_Word sxp)
{
  rtl_Word head, clause, tail, name, arg;
  rtl_Word const *rptr;
  size_t len, i;
  rtl_Intrinsic **buf;
  size_t        bufLen;
  size_t        bufCap;

  rtl_Intrinsic **labels;
  rtl_Word      *labelsNames;
  size_t        labelsLen;
  size_t        labelsCap;

  rtl_Intrinsic *a, *b, *c;

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
    rptr = rtl_xReifyTuple(C->M, sxp, &len);
    RTL_ASSERT_NO_UNWIND(C->M);

    for (i = 0; i < len; i++) {
      if (bufCap == bufLen) {
        bufCap = !bufCap ? 4 : 2*bufCap;
        buf    = realloc(buf, sizeof(rtl_Intrinsic *)*bufCap);
      }

      buf[bufLen++] = rtl_xExprToIntrinsic(C, rptr[i]);
      RTL_UNWIND (C->M) break;
    }

    return rtl_mkTupleIntrinsic(buf, bufLen);

  case RTL_MAP:
    return xMapToIntrinsic(C, sxp);

  case RTL_CONS:
    head = rtl_xCar(C->M, sxp);
    RTL_UNWIND (C->M) return NULL;

    len  = rtl_xListLength(C->M, sxp);
    RTL_UNWIND (C->M) return NULL;

    if (head == symCache.intrinsic.cons) {
      assert(len == 3);
      arg = rtl_xCadr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      a = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      arg = rtl_xCaddr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      b = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      return rtl_mkConsIntrinsic(a, b);

    } else if (head == symCache.intrinsic.car) {
      assert(len == 2);
      arg = rtl_xCadr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      a = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      return rtl_mkCarIntrinsic(a);

    } else if (head == symCache.intrinsic.cdr) {
      assert(len == 2);
      arg = rtl_xCadr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      a = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      return rtl_mkCdrIntrinsic(a);

    } else if (head == symCache.intrinsic.tuple) {
      for (tail = rtl_xCdr(C->M, sxp);
           tail != RTL_NIL;
           tail = rtl_xCdr(C->M, tail))
      {
        RTL_UNWIND (C->M) return NULL;

        if (bufCap == bufLen) {
          bufCap = !bufCap ? 4 : 2*bufCap;
          buf    = realloc(buf, sizeof(rtl_Intrinsic *)*bufCap);
        }

        arg = rtl_xCar(C->M, tail);
        RTL_UNWIND (C->M) return NULL;

        buf[bufLen++] = rtl_xExprToIntrinsic(C, arg);
        RTL_UNWIND (C->M) return NULL;
      }

      return rtl_mkTupleIntrinsic(buf, bufLen);

    } else if (head == symCache.intrinsic.len) {
      arg = rtl_xCadr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      a = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      return rtl_mkLenIntrinsic(a);

    } else if (head == symCache.intrinsic.get) {
      arg = rtl_xCadr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      a = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      arg = rtl_xCaddr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      b = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      return rtl_mkGetIntrinsic(a, b);

    } else if (head == symCache.intrinsic.pushFirst) {
      arg = rtl_xCadr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      a = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      arg = rtl_xCaddr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      b = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      return rtl_mkBinopIntrinsic(RTL_INTRINSIC_PUSH_FIRST, a, b);

    } else if (head == symCache.intrinsic.pushLast) {
      arg = rtl_xCadr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      a = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      arg = rtl_xCaddr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      b = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      return rtl_mkBinopIntrinsic(RTL_INTRINSIC_PUSH_LAST, a, b);

    } else if (head == symCache.intrinsic.concat) {
      arg = rtl_xCadr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      a = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      arg = rtl_xCaddr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      b = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      return rtl_mkBinopIntrinsic(RTL_INTRINSIC_CONCAT, a, b);

    } else if (head == symCache.intrinsic.slice) {
      arg = rtl_xCadr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      a = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      arg = rtl_xCaddr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      b = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      arg = rtl_xCar(C->M, rtl_xCdddr(C->M, sxp));
      RTL_UNWIND (C->M) return NULL;

      c = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      return rtl_mkSliceIntrinsic(a, b, c);

    } else if (head == symCache.intrinsic.map) {
      return rtl_mkConstantIntrinsic(RTL_MAP);

    } else if (head == symCache.intrinsic.insert) {
      arg = rtl_xCadr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      a = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      arg = rtl_xCaddr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      b = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      arg = rtl_xCar(C->M, rtl_xCdddr(C->M, sxp));
      RTL_UNWIND (C->M) return NULL;

      c = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      return rtl_mkInsertIntrinsic(a, b, c);

    } else if (head == symCache.intrinsic.lookup) {
      arg = rtl_xCadr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      a = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      arg = rtl_xCaddr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      b = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      arg = rtl_xCar(C->M, rtl_xCdddr(C->M, sxp));
      RTL_UNWIND (C->M) return NULL;

      c = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      return rtl_mkLookupIntrinsic(a, b, c);

    } else if (head == symCache.intrinsic.dynGet) {
      arg = rtl_xCadr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      return rtl_mkDynGetIntrinsic(arg);

    } else if (head == symCache.intrinsic.dynSet) {
      name = rtl_xCadr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      arg = rtl_xCaddr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      a = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      return rtl_mkDynSetIntrinsic(name, a);

    } else if (head == symCache.intrinsic.bind) {
      for (tail = rtl_xCddr(C->M, sxp);
           tail != RTL_NIL;
           tail = rtl_xCdr(C->M, tail))
      {
        RTL_UNWIND (C->M) return NULL;

        if (bufCap == bufLen) {
          bufCap = !bufCap ? 4 : 2*bufCap;
          buf    = realloc(buf, sizeof(rtl_Intrinsic *)*bufCap);
        }

        arg = rtl_xCar(C->M, tail);
        RTL_UNWIND (C->M) return NULL;

        buf[bufLen++] = rtl_xExprToIntrinsic(C, arg);
        RTL_UNWIND (C->M) return NULL;
      }

      name = rtl_xCaadr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      arg = rtl_xCar(C->M, rtl_xCdadr(C->M, sxp));
      RTL_UNWIND (C->M) return NULL;

      a = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      return rtl_mkBindIntrinsic(name, a, buf, bufLen);

    } else if (head == symCache.intrinsic.progn) {
      for (tail = rtl_xCdr(C->M, sxp);
           tail != RTL_NIL;
           tail = rtl_xCdr(C->M, tail))
      {
        RTL_UNWIND (C->M) return NULL;

        if (bufCap == bufLen) {
          bufCap = !bufCap ? 4 : 2*bufCap;
          buf    = realloc(buf, sizeof(rtl_Intrinsic *)*bufCap);
        }

        buf[bufLen++] = rtl_xExprToIntrinsic(C, rtl_xCar(C->M, tail));
        RTL_UNWIND (C->M) return NULL;
      }

      return rtl_mkPrognIntrinsic(buf, bufLen);

    } else if (head == symCache.intrinsic.protect) {
      arg = rtl_xCadr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      a = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      arg = rtl_xCaddr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      b = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      return rtl_mkProtectIntrinsic(a, b);

    } else if (head == symCache.intrinsic.lambda) {
      assert(len >= 2);

      for (tail = rtl_xCadr(C->M, sxp);
           tail != RTL_NIL;
           tail = rtl_xCdr(C->M, tail))
      {
        RTL_UNWIND (C->M) return NULL;
        assert(rtl_isSymbol(rtl_xCar(C->M, tail)));

        if (argNamesCap == argNamesLen) {
          argNamesCap = !argNamesCap ? 4 : argNamesCap*2;
          argNames    = realloc(argNames, sizeof(rtl_Word)*argNamesCap);
        }

        argNames[argNamesLen++] = rtl_xCar(C->M, tail);
        RTL_UNWIND (C->M) return NULL;
      }

      for (tail = rtl_xCddr(C->M, sxp);
           tail != RTL_NIL;
           tail = rtl_xCdr(C->M, tail))
      {
        RTL_UNWIND (C->M) return NULL;

        if (bufCap == bufLen) {
          bufCap = !bufCap ? 4 : 2*bufCap;
          buf    = realloc(buf, sizeof(rtl_Intrinsic *)*bufCap);
        }

        arg = rtl_xCar(C->M, tail);
        RTL_UNWIND (C->M) return NULL;

        buf[bufLen++] = rtl_xExprToIntrinsic(C, arg);
        RTL_UNWIND (C->M) return NULL;
      }

      return rtl_mkLambdaIntrinsic(argNames, argNamesLen, buf, bufLen);

    } else if (head == symCache.intrinsic.labels) {
      assert(len >= 2);

      for (clause = rtl_xCadr(C->M, sxp);
           clause != RTL_NIL;
           clause = rtl_xCdr(C->M, clause))
      {
        RTL_UNWIND (C->M) return NULL;
        for (tail = rtl_xCadar(C->M, clause);
             tail != RTL_NIL;
             tail = rtl_xCdr(C->M, tail))
        {
          RTL_UNWIND (C->M) return NULL;
          assert(rtl_isSymbol(rtl_xCar(C->M, tail)));

          if (argNamesCap == argNamesLen) {
            argNamesCap = !argNamesCap ? 4 : argNamesCap*2;
            argNames    = realloc(argNames, sizeof(rtl_Word)*argNamesCap);
          }

          argNames[argNamesLen++] = rtl_xCar(C->M, tail);
          RTL_UNWIND (C->M) return NULL;
        }

        for (tail = rtl_xCddar(C->M, clause);
             tail != RTL_NIL;
             tail = rtl_xCdr(C->M, tail))
        {
          RTL_UNWIND (C->M) return NULL;

          if (bufCap == bufLen) {
            bufCap = !bufCap ? 4 : 2*bufCap;
            buf    = realloc(buf, sizeof(rtl_Intrinsic *)*bufCap);
          }

          arg = rtl_xCar(C->M, tail);
          RTL_UNWIND (C->M) return NULL;

          buf[bufLen++] = rtl_xExprToIntrinsic(C, arg);
          RTL_UNWIND (C->M) return NULL;
        }

        if (labelsCap == labelsLen) {
          labelsCap   = !labelsCap ? 4 : 2*labelsCap;
          labels      = realloc(labels, sizeof(rtl_Intrinsic *)*labelsCap);
          labelsNames = realloc(labelsNames, sizeof(rtl_Word)*labelsCap);
        }

        labelsNames[labelsLen] = rtl_xCaar(C->M, clause);
        RTL_UNWIND (C->M) return NULL;

        labels[labelsLen++]    = rtl_mkLambdaIntrinsic(argNames, argNamesLen,
                                                       buf,      bufLen);

        argNames    = NULL;
        argNamesCap = argNamesLen = 0;

        buf    = NULL;
        bufCap = bufLen = 0;
      }

      for (tail = rtl_xCddr(C->M, sxp);
           tail != RTL_NIL;
           tail = rtl_xCdr(C->M, tail))
      {
        RTL_UNWIND (C->M) return NULL;
        if (bufCap == bufLen) {
          bufCap = !bufCap ? 4 : 2*bufCap;
          buf    = realloc(buf, sizeof(rtl_Intrinsic *)*bufCap);
        }

        arg = rtl_xCar(C->M, tail);
        RTL_UNWIND (C->M) return NULL;

        buf[bufLen++] = rtl_xExprToIntrinsic(C, arg);
        RTL_UNWIND (C->M) return NULL;

      }

      return rtl_mkLabelsIntrinsic(labelsNames, labels, labelsLen, buf, bufLen);

    } else if (head == symCache.intrinsic.defun ||
               head == symCache.intrinsic.defmacro) {
      assert(len >= 3);

      name = rtl_xCadr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      for (tail = rtl_xCaddr(C->M, sxp);
           rtl_isCons(tail);
           tail = rtl_xCdr(C->M, tail))
      {
        RTL_UNWIND (C->M) return NULL;
        assert(rtl_isSymbol(rtl_xCar(C->M, tail)));

        if (argNamesCap == argNamesLen) {
          argNamesCap = !argNamesCap ? 4 : argNamesCap*2;
          argNames    = realloc(argNames, sizeof(rtl_Word)*argNamesCap);
        }

        argNames[argNamesLen++] = rtl_xCar(C->M, tail);
        RTL_UNWIND (C->M) return NULL;
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

      for (tail = rtl_xCdddr(C->M, sxp);
           tail != RTL_NIL;
           tail = rtl_xCdr(C->M, tail))
      {
        RTL_UNWIND (C->M) return NULL;

        if (bufCap == bufLen) {
          bufCap = !bufCap ? 4 : 2*bufCap;
          buf    = realloc(buf, sizeof(rtl_Intrinsic *)*bufCap);
        }

        arg = rtl_xCar(C->M, tail);
        RTL_UNWIND (C->M) return NULL;

        buf[bufLen++] = rtl_xExprToIntrinsic(C, arg);
        RTL_UNWIND (C->M) return NULL;
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
      arg = rtl_xCadr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      return rtl_mkExportIntrinsic(arg);

    } else if (head == symCache.intrinsic.quote) {
      assert(len == 2);
      arg = rtl_xCadr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      return rtl_mkQuoteIntrinsic(arg);

    } else if (head == symCache.intrinsic.iadd) {
      assert(len == 3);
      arg = rtl_xCadr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      a = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      arg = rtl_xCaddr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      b = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      return rtl_mkBinopIntrinsic(RTL_INTRINSIC_IADD, a, b);

    } else if (head == symCache.intrinsic.isub) {
      assert(len == 3);
      arg = rtl_xCadr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      a = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      arg = rtl_xCaddr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      b = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      return rtl_mkBinopIntrinsic(RTL_INTRINSIC_ISUB, a, b);

    } else if (head == symCache.intrinsic.imul) {
      assert(len == 3);
      arg = rtl_xCadr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      a = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      arg = rtl_xCaddr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      b = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      return rtl_mkBinopIntrinsic(RTL_INTRINSIC_IMUL, a, b);
      
    } else if (head == symCache.intrinsic.idiv) {
      assert(len == 3);
      arg = rtl_xCadr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      a = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      arg = rtl_xCaddr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      b = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      return rtl_mkBinopIntrinsic(RTL_INTRINSIC_IDIV, a, b);

    } else if (head == symCache.intrinsic.imod) {
      assert(len == 3);
      arg = rtl_xCadr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      a = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      arg = rtl_xCaddr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      b = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      return rtl_mkBinopIntrinsic(RTL_INTRINSIC_IMOD, a, b);
      
    } else if (head == symCache.intrinsic.lt) {
      assert(len == 3);
      arg = rtl_xCadr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      a = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      arg = rtl_xCaddr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      b = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      return rtl_mkBinopIntrinsic(RTL_INTRINSIC_LT, a, b);
      
    } else if (head == symCache.intrinsic.leq) {
      assert(len == 3);
      arg = rtl_xCadr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      a = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      arg = rtl_xCaddr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      b = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      return rtl_mkBinopIntrinsic(RTL_INTRINSIC_LEQ, a, b);
      
    } else if (head == symCache.intrinsic.gt) {
      assert(len == 3);
      arg = rtl_xCadr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      a = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      arg = rtl_xCaddr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      b = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      return rtl_mkBinopIntrinsic(RTL_INTRINSIC_GT, a, b);
      
    } else if (head == symCache.intrinsic.geq) {
      assert(len == 3);
      arg = rtl_xCadr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      a = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      arg = rtl_xCaddr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      b = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      return rtl_mkBinopIntrinsic(RTL_INTRINSIC_GEQ, a, b);
      
    } else if (head == symCache.intrinsic.eq) {
      assert(len == 3);
      arg = rtl_xCadr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      a = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      arg = rtl_xCaddr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      b = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      return rtl_mkBinopIntrinsic(RTL_INTRINSIC_EQ, a, b);
      
    } else if (head == symCache.intrinsic.neq) {
      assert(len == 3);
      arg = rtl_xCadr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      a = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      arg = rtl_xCaddr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      b = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      return rtl_mkBinopIntrinsic(RTL_INTRINSIC_NEQ, a, b);
      
    } else if (head == symCache.intrinsic.iso) {
      assert(len == 3);
      arg = rtl_xCadr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      a = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      arg = rtl_xCaddr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      b = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      return rtl_mkBinopIntrinsic(RTL_INTRINSIC_ISO, a, b);

    } else if (head == symCache.intrinsic.nilp) {
      assert(len == 2);
      arg = rtl_xCadr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      a = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      return rtl_mkTypePredIntrinsic(RTL_NIL, a);

    } else if (head == symCache.intrinsic.symbolp) {
      assert(len == 2);
      
      arg = rtl_xCadr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      a = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      return rtl_mkTypePredIntrinsic(RTL_SYMBOL, a);

    } else if (head == symCache.intrinsic.selectorp) {
      assert(len == 2);
      
      arg = rtl_xCadr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      a = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      return rtl_mkTypePredIntrinsic(RTL_SELECTOR, a);

    } else if (head == symCache.intrinsic.int28p) {
      assert(len == 2);
      
      arg = rtl_xCadr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      a = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      return rtl_mkTypePredIntrinsic(RTL_INT28, a);

    } else if (head == symCache.intrinsic.fix14p) {
      assert(len == 2);
      
      arg = rtl_xCadr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      a = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      return rtl_mkTypePredIntrinsic(RTL_FIX14, a);

    } else if (head == symCache.intrinsic.tuplep) {
      assert(len == 2);
      
      arg = rtl_xCadr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      a = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      return rtl_mkTypePredIntrinsic(RTL_TUPLE, a);

    } else if (head == symCache.intrinsic.charp) {
      assert(len == 2);
      
      arg = rtl_xCadr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      a = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      return rtl_mkTypePredIntrinsic(RTL_CHAR, a);

    } else if (head == symCache.intrinsic.mapp) {
      assert(len == 2);
      
      arg = rtl_xCadr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      a = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      return rtl_mkTypePredIntrinsic(RTL_MAP, a);

    } else if (head == symCache.intrinsic.consp) {
      assert(len == 2);
      
      arg = rtl_xCadr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      a = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      return rtl_mkTypePredIntrinsic(RTL_CONS, a);

    } else if (head == symCache.intrinsic.functionp) {
      assert(len == 2);
      
      arg = rtl_xCadr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      a = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      return rtl_mkTypePredIntrinsic(RTL_FUNCTION, a);

    } else if (head == symCache.intrinsic.closurep) {
      assert(len == 2);
      
      arg = rtl_xCadr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      a = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      return rtl_mkTypePredIntrinsic(RTL_CLOSURE, a);

    } else if (head == symCache.intrinsic.unresolvedp) {
      assert(len == 2);
      
      arg = rtl_xCadr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      a = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      return rtl_mkTypePredIntrinsic(RTL_UNRESOLVED_SYMBOL, a);

    } else if (head == symCache.intrinsic.topp) {
      assert(len == 2);
      
      arg = rtl_xCadr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      a = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      return rtl_mkTypePredIntrinsic(RTL_TOP, a);

    } else if (head == symCache.intrinsic._if) {
      assert(len == 3 || len == 4);
      
      arg = rtl_xCadr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      a = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      arg = rtl_xCaddr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      b = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      arg = rtl_xCar(C->M, rtl_xCdddr(C->M, sxp));
      RTL_UNWIND (C->M) return NULL;

      c = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      return rtl_mkIfIntrinsic(a, b, c);

    } else if (head == symCache.intrinsic.applyList) {
      assert(len == 3);
      
      arg = rtl_xCadr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      a = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      arg = rtl_xCaddr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      b = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      return rtl_mkApplyListIntrinsic(a, b);

    } else if (head == symCache.intrinsic.applyTuple) {
      assert(len == 3);
      
      arg = rtl_xCadr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      a = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      arg = rtl_xCaddr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      b = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      return rtl_mkApplyTupleIntrinsic(a, b);

    } else if (head == symCache.intrinsic.setVar) {
      name = rtl_xCadr(C->M, sxp);

      arg = rtl_xCaddr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      a = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      return rtl_mkSetVarIntrinsic(name, a);

    } else if (head == symCache.intrinsic.setCar) {
      arg = rtl_xCadr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      a = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      arg = rtl_xCaddr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      b = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      return rtl_mkSetCarIntrinsic(a, b);

    } else if (head == symCache.intrinsic.setCdr) {
      arg = rtl_xCadr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      a = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      arg = rtl_xCaddr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      b = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      return rtl_mkSetCdrIntrinsic(a, b);

    } else if (head == symCache.intrinsic.setElem) {
      arg = rtl_xCadr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      a = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      arg = rtl_xCaddr(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      b = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      arg = rtl_xCar(C->M, rtl_xCdddr(C->M, sxp));
      RTL_UNWIND (C->M) return NULL;

      c = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      return rtl_mkSetElemIntrinsic(a, b, c);

    } else if (head == symCache.intrinsic.yield) {
      return rtl_mkYieldIntrinsic();

    } else if (head == symCache.intrinsic.gensym) {
      assert(len == 1);

      return rtl_mkGensymIntrinsic();
    } else {
      assert(len >= 1);

      for (tail = rtl_xCdr(C->M, sxp);
           tail != RTL_NIL;
           tail = rtl_xCdr(C->M, tail))
      {
        RTL_UNWIND (C->M) return NULL;

        if (bufCap == bufLen) {
          bufCap = !bufCap ? 4 : 2*bufCap;
          buf    = realloc(buf, sizeof(rtl_Intrinsic *)*bufCap);
        }

        arg = rtl_xCar(C->M, tail);
        RTL_UNWIND (C->M) return NULL;

        buf[bufLen++] = rtl_xExprToIntrinsic(C, arg);
        RTL_UNWIND (C->M) return NULL;
      }

      arg = rtl_xCar(C->M, sxp);
      RTL_UNWIND (C->M) return NULL;

      a = rtl_xExprToIntrinsic(C, arg);
      RTL_UNWIND (C->M) return NULL;

      return rtl_mkCallIntrinsic(a, buf, bufLen);
    } break;

  case RTL_SYMBOL:
    return rtl_mkVarIntrinsic(sxp);

  case RTL_CHAR:
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
rtl_Intrinsic *__impl_transformIntrinsic(rtl_Compiler *C,
                                         Environment const *env,
                                         rtl_Intrinsic *x)
{
  rtl_Word      nameTmp;
  rtl_Intrinsic **argsTmp;
  size_t        argsLenTmp,
                i;
  Environment   newEnv;

  switch (x->type) {
  case RTL_INTRINSIC_CONS:
    x->as.cons.car = __impl_transformIntrinsic(C, env, x->as.cons.car);
    x->as.cons.cdr = __impl_transformIntrinsic(C, env, x->as.cons.cdr);
    break;

  case RTL_INTRINSIC_CAR:
    x->as.car.arg = __impl_transformIntrinsic(C, env, x->as.car.arg);
    break;

  case RTL_INTRINSIC_CDR:
    x->as.cdr.arg = __impl_transformIntrinsic(C, env, x->as.cdr.arg);
    break;

  case RTL_INTRINSIC_TUPLE:
    for (i = 0; i < x->as.tuple.elemsLen; i++) {
      x->as.tuple.elems[i]  = __impl_transformIntrinsic(C, env, x->as.tuple.elems[i]);
    } break;

  case RTL_INTRINSIC_LEN:
    x->as.len.tuple = __impl_transformIntrinsic(C, env, x->as.len.tuple);
    break;

  case RTL_INTRINSIC_GET:
    x->as.get.tuple = __impl_transformIntrinsic(C, env, x->as.get.tuple);
    x->as.get.index = __impl_transformIntrinsic(C, env, x->as.get.index);
    break;

  case RTL_INTRINSIC_SLICE:
    x->as.slice.tuple = __impl_transformIntrinsic(C, env, x->as.slice.tuple);
    x->as.slice.beg   = __impl_transformIntrinsic(C, env, x->as.slice.beg);
    x->as.slice.end   = __impl_transformIntrinsic(C, env, x->as.slice.end);
    break;

  case RTL_INTRINSIC_INSERT:
    x->as.insert.map = __impl_transformIntrinsic(C, env, x->as.insert.map);
    x->as.insert.key = __impl_transformIntrinsic(C, env, x->as.insert.key);
    x->as.insert.val = __impl_transformIntrinsic(C, env, x->as.insert.val);
    break;

  case RTL_INTRINSIC_LOOKUP:
    x->as.lookup.map = __impl_transformIntrinsic(C, env, x->as.lookup.map);
    x->as.lookup.key = __impl_transformIntrinsic(C, env, x->as.lookup.key);
    break;

  case RTL_INTRINSIC_DYN_GET:
    break;

  case RTL_INTRINSIC_DYN_SET:
    x->as.dynSet.value = __impl_transformIntrinsic(C, env, x->as.dynSet.value);
    break;

  case RTL_INTRINSIC_BIND:
    x->as.bind.value = __impl_transformIntrinsic(C, env, x->as.bind.value);

    for (i = 0; i < x->as.bind.bodyLen; i++) {
      x->as.bind.body[i] = __impl_transformIntrinsic(C, env, x->as.bind.body[i]);
    } break;

  case RTL_INTRINSIC_VAR:
    if ((!env ||
         !lookupVar(env, x->as.var.name, &x->as.var.frame, &x->as.var.idx))) {
      x->as.var.global = true;
    }
    break;

  case RTL_INTRINSIC_NAMED_TAIL:
  case RTL_INTRINSIC_TAIL:
  case RTL_INTRINSIC_NAMED_CALL:
    abort(); // This should never happen

  case RTL_INTRINSIC_CALL:
    // Start by transforming all of the args
    for (i = 0; i < x->as.call.argsLen; i++) {
      x->as.call.args[i]  = __impl_transformIntrinsic(C, env, x->as.call.args[i]);
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
      x->as.call.fn = __impl_transformIntrinsic(C, env, x->as.call.fn);
    } break;


  case RTL_INTRINSIC_APPLY_LIST:
    x->as.applyList.fn  = __impl_transformIntrinsic(C, env, x->as.applyList.fn);
    x->as.applyList.arg = __impl_transformIntrinsic(C, env, x->as.applyList.arg);
    break;

  case RTL_INTRINSIC_APPLY_TUPLE:
    x->as.applyTuple.fn  = __impl_transformIntrinsic(C, env, x->as.applyTuple.fn);
    x->as.applyTuple.arg = __impl_transformIntrinsic(C, env, x->as.applyTuple.arg);
    break;

  case RTL_INTRINSIC_PROGN:
    for (i = 0; i < x->as.progn.formsLen; i++) {
      x->as.progn.forms[i] = __impl_transformIntrinsic(C, env, x->as.progn.forms[i]);
    }
    break;

  case RTL_INTRINSIC_PROTECT:
    x->as.protect.handler = __impl_transformIntrinsic(C, env, x->as.protect.handler);
    x->as.protect.expr    = __impl_transformIntrinsic(C, env, x->as.protect.expr);
    break;

  case RTL_INTRINSIC_LAMBDA:
    newEnv.super = env;
    newEnv.frame = env ? env->frame + 1 : 0;
    newEnv.len   = x->as.lambda.argNamesLen;
    newEnv.names = x->as.lambda.argNames;

    for (i = 0; i < x->as.lambda.bodyLen; i++) {
      x->as.lambda.body[i] = __impl_transformIntrinsic(C, &newEnv,
                                                       x->as.lambda.body[i]);
    }

    x->as.lambda.fnID = rtl_newFuncID(C->M->codeBase, rtl_intern("lambda", "body"));
    break;

  case RTL_INTRINSIC_LABELS:
    newEnv.super = env;
    newEnv.frame = env ? env->frame + 1 : 0;
    newEnv.len   = x->as.labels.labelsLen;
    newEnv.names = x->as.labels.labelsNames;

    for (i = 0; i < x->as.labels.labelsLen; i++) {
      x->as.labels.labelsFns[i] = __impl_transformIntrinsic(C, &newEnv,
                                                            x->as.labels.labelsFns[i]);
    }

    for (i = 0; i < x->as.labels.bodyLen; i++) {
      x->as.labels.body[i] = __impl_transformIntrinsic(C, &newEnv,
                                                       x->as.labels.body[i]);
    }
    break;

  case RTL_INTRINSIC_DEFUN:
    newEnv.super = env;
    newEnv.frame = env ? env->frame + 1 : 0;
    newEnv.len   = x->as.defun.argNamesLen;
    newEnv.names = x->as.defun.argNames;

    for (i = 0; i < x->as.defun.bodyLen; i++) {
      x->as.defun.body[i] = __impl_transformIntrinsic(C, &newEnv,
                                                      x->as.defun.body[i]);
    }
    break;

  case RTL_INTRINSIC_DEFMACRO:
    newEnv.super = env;
    newEnv.frame = env ? env->frame + 1 : 0;
    newEnv.len   = x->as.defmacro.argNamesLen;
    newEnv.names = x->as.defmacro.argNames;

    for (i = 0; i < x->as.defmacro.bodyLen; i++) {
      x->as.defmacro.body[i] = __impl_transformIntrinsic(C, &newEnv,
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
  case RTL_INTRINSIC_PUSH_FIRST:
  case RTL_INTRINSIC_PUSH_LAST:
  case RTL_INTRINSIC_CONCAT:
    x->as.binop.leftArg  = __impl_transformIntrinsic(C, env, x->as.binop.leftArg);
    x->as.binop.rightArg = __impl_transformIntrinsic(C, env, x->as.binop.rightArg);
    break;

  case RTL_INTRINSIC_TYPE_PRED:
    x->as.typePred.arg = __impl_transformIntrinsic(C, env, x->as.typePred.arg);
    break;

  case RTL_INTRINSIC_IF:
    x->as._if.test  = __impl_transformIntrinsic(C, env, x->as._if.test);
    x->as._if.then  = __impl_transformIntrinsic(C, env, x->as._if.then);
    x->as._if._else = __impl_transformIntrinsic(C, env, x->as._if._else);
    break;

  case RTL_INTRINSIC_EXPORT:
  case RTL_INTRINSIC_QUOTE:
  case RTL_INTRINSIC_CONSTANT:
  case RTL_INTRINSIC_GENSYM:
  case RTL_INTRINSIC_YIELD:
    break;

  case RTL_INTRINSIC_SET_VAR:
    x->as.setVar.value = __impl_transformIntrinsic(C, env, x->as.setVar.value);

    if ((!env || !lookupVar(env, x->as.setVar.name,
                            &x->as.setVar.frame,
                            &x->as.setVar.idx)))
    {
      printf("compile error: can't set undefined var %s:%s!\n",
             rtl_symbolPackageName(x->as.setVar.name),
             rtl_symbolName(x->as.setVar.name));
      abort();
    } break;

  case RTL_INTRINSIC_SET_CAR:
    x->as.setCar.cons  = __impl_transformIntrinsic(C, env, x->as.setCar.cons);
    x->as.setCar.value = __impl_transformIntrinsic(C, env, x->as.setCar.value);
    break;

  case RTL_INTRINSIC_SET_CDR:
    x->as.setCdr.cons  = __impl_transformIntrinsic(C, env, x->as.setCdr.cons);
    x->as.setCdr.value = __impl_transformIntrinsic(C, env, x->as.setCdr.value);
    break;

  case RTL_INTRINSIC_SET_ELEM:
    x->as.setElem.tuple = __impl_transformIntrinsic(C, env, x->as.setElem.tuple);
    x->as.setElem.index = __impl_transformIntrinsic(C, env, x->as.setElem.index);
    x->as.setElem.value = __impl_transformIntrinsic(C, env, x->as.setElem.value);
    break;

  }

  return x;
}

rtl_Intrinsic *rtl_transformIntrinsic(rtl_Compiler *C, rtl_Intrinsic *x) {
  return __impl_transformIntrinsic(C, NULL, x);
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

  case RTL_INTRINSIC_SLICE:
    rtl_tailCallPass(x->as.slice.tuple);
    rtl_tailCallPass(x->as.slice.beg);
    rtl_tailCallPass(x->as.slice.end);
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
  case RTL_INTRINSIC_DYN_GET:
    break;

  case RTL_INTRINSIC_DYN_SET:
    rtl_tailCallPass(x->as.dynSet.value);
    break;

  case RTL_INTRINSIC_BIND:
    rtl_tailCallPass(x->as.bind.value);
    for (i = 0; i < x->as.bind.bodyLen; i++) {
      rtl_tailCallPass(x->as.bind.body[i]);
    } break;

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

  case RTL_INTRINSIC_PROTECT:
    rtl_tailCallPass(x->as.protect.handler);
    rtl_tailCallPass(x->as.protect.expr);
    break;

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
  case RTL_INTRINSIC_PUSH_FIRST:
  case RTL_INTRINSIC_PUSH_LAST:
  case RTL_INTRINSIC_CONCAT:
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

  case RTL_INTRINSIC_SET_VAR:
    rtl_tailCallPass(x->as.setVar.value);
    break;

  case RTL_INTRINSIC_SET_CAR:
    rtl_tailCallPass(x->as.setCar.cons);
    rtl_tailCallPass(x->as.setCar.value);
    break;

  case RTL_INTRINSIC_SET_CDR:
    rtl_tailCallPass(x->as.setCdr.cons);
    rtl_tailCallPass(x->as.setCdr.value);
    break;

  case RTL_INTRINSIC_SET_ELEM:
    rtl_tailCallPass(x->as.setElem.tuple);
    rtl_tailCallPass(x->as.setElem.index);
    rtl_tailCallPass(x->as.setElem.value);
    break;

  case RTL_INTRINSIC_EXPORT:
  case RTL_INTRINSIC_QUOTE:
  case RTL_INTRINSIC_CONSTANT:
  case RTL_INTRINSIC_GENSYM:
  case RTL_INTRINSIC_YIELD:
    break;
  }
}

static
size_t quoteCodeSize(rtl_Machine *M, rtl_Word x);

static
void __quoteEntryCodeSize(rtl_Machine *M,
                          void        *vaccum,
                          rtl_Word    key,
                          rtl_Word    val)
{
  size_t *acc = (size_t *)vaccum;

  *acc += quoteCodeSize(M, key)
        + quoteCodeSize(M, val)
        + 1;
}

static
size_t quoteMapCodeSize(rtl_Machine *M, rtl_Word map)
{
  size_t codeSize = 1;

  rtl_xVisitMap(M, &codeSize, __quoteEntryCodeSize, map);
  RTL_ASSERT_NO_UNWIND(M);

  return codeSize;
}

static
size_t quoteCodeSize(rtl_Machine *M, rtl_Word x)
{
  rtl_Word const *rptr;
  size_t i, len, codeSize;

  rtl_Word a, b;

  switch (rtl_typeOf(x)) {
  case RTL_NIL:
  case RTL_TOP:
    return 1;

  case RTL_SYMBOL:
  case RTL_SELECTOR:
  case RTL_INT28:
  case RTL_FIX14:
  case RTL_CHAR:
    // Determine whether we need a 32-, 16-, or 8-bit opcode to represent this value.
    if (x > 0xFFFFF) {
      return 5;
    } else if (x > 0xFFF) {
      return 3;
    } else {
      return 2;
    }

  case RTL_CONS:
    a = rtl_xCar(M, x);
    RTL_ASSERT_NO_UNWIND(M);

    b = rtl_xCdr(M, x);
    RTL_ASSERT_NO_UNWIND(M);

    return quoteCodeSize(M, a)
         + quoteCodeSize(M, b)
         + 1;

  case RTL_MAP:
    return quoteMapCodeSize(M, x);

  case RTL_TUPLE:
    codeSize = 0;
    rptr = rtl_xReifyTuple(M, x, &len);
    RTL_ASSERT_NO_UNWIND(M);

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
         thenSize,    thenJmpBytes,
         elseSize,    elseJmpBytes,
         protectSize, protectJmpBytes;

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

  case RTL_INTRINSIC_GET:
    return x->codeSize = annotateCodeSize(M, x->as.get.tuple)
                       + annotateCodeSize(M, x->as.get.index)
                       + 1;

  case RTL_INTRINSIC_SLICE:
    return x->codeSize = annotateCodeSize(M, x->as.slice.tuple)
                       + annotateCodeSize(M, x->as.slice.beg)
                       + annotateCodeSize(M, x->as.slice.end)
                       + 1;

  case RTL_INTRINSIC_DYN_GET:
    return x->codeSize = 5;

  case RTL_INTRINSIC_DYN_SET:
    return x->codeSize = annotateCodeSize(M, x->as.dynSet.value)
                       + 5;

  case RTL_INTRINSIC_BIND:
    x->codeSize = 10 + annotateCodeSize(M, x->as.bind.value);

    for (i = 0; i < x->as.bind.bodyLen; i++) {
      x->codeSize += annotateCodeSize(M, x->as.bind.body[i]);
      if (i + 1 < x->as.bind.bodyLen) {
        x->codeSize++;
      }
    }

    return x->codeSize;

  case RTL_INTRINSIC_INSERT:
    return x->codeSize = annotateCodeSize(M, x->as.insert.map)
                       + annotateCodeSize(M, x->as.insert.key)
                       + annotateCodeSize(M, x->as.insert.val)
                       + 1;

  case RTL_INTRINSIC_LOOKUP:
    return x->codeSize = annotateCodeSize(M, x->as.lookup.map)
                       + annotateCodeSize(M, x->as.lookup.key)
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

  case RTL_INTRINSIC_PROTECT:
    protectSize = annotateCodeSize(M, x->as.protect.expr);

    if (protectSize + 2< 0xFF) {
      protectJmpBytes = 2;
    } if (protectSize + 2 < 0xFFFF) {
      protectJmpBytes = 3;
    } else {
      protectJmpBytes = 5;
    }

    return x->codeSize = annotateCodeSize(M, x->as.protect.handler)
                       + protectJmpBytes + protectSize
                       + 2;

  case RTL_INTRINSIC_LAMBDA:
    for (i = 0; i < x->as.lambda.bodyLen; i++) {
      annotateCodeSize(M, x->as.lambda.body[i]);
    }

    // Determine whether to use closure32, closure16, or closure8.
    if (x->as.lambda.fnID > 0xFFFF) {
      return x->codeSize = 5;
    } else if (x->as.lambda.fnID > 0xFF) {
      return x->codeSize = 3;
    } else {
      return x->codeSize = 2;
    }

  case RTL_INTRINSIC_LABELS:
    x->codeSize = 3; // labels <u16>

    for (i = 0; i < x->as.labels.labelsLen; i++) {
      annotateCodeSize(M, x->as.labels.labelsFns[i]);

      // Determine whether to use const32, fn16, or fn8.
      if (x->as.labels.labelsFns[i]->as.lambda.fnID > 0xFFFF) {
        x->codeSize += 5;
      } else if (x->as.labels.labelsFns[i]->as.lambda.fnID > 0xFF) {
        x->codeSize += 3;
      } else {
        x->codeSize += 2;
      }
    }

    for (i = 0; i < x->as.labels.bodyLen; i++) {
      x->codeSize += annotateCodeSize(M, x->as.labels.body[i]);
    }

    x->codeSize += 1; // end-labels

    return x->codeSize;

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
  case RTL_INTRINSIC_PUSH_FIRST:
  case RTL_INTRINSIC_PUSH_LAST:
  case RTL_INTRINSIC_CONCAT:
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

  case RTL_INTRINSIC_GENSYM:
    return x->codeSize = 1;

  case RTL_INTRINSIC_CONSTANT:
    switch (x->as.constant) {
    case RTL_NIL:
    case RTL_TOP:
    case RTL_MAP:
      return x->codeSize = 1;

    default:
      if (x->as.constant > 0xFFFFF) {
        return x->codeSize = 5;
      } else if (x->as.constant > 0xFFF) {
        return x->codeSize = 3;
      } else {
        return x->codeSize = 2;
      }
    }

  case RTL_INTRINSIC_SET_VAR:
    return x->codeSize = annotateCodeSize(M, x->as.setVar.value)
                       + 5;

  case RTL_INTRINSIC_SET_CAR:
    return x->codeSize = annotateCodeSize(M, x->as.setCar.cons)
                       + annotateCodeSize(M, x->as.setCar.value)
                       + 1;

  case RTL_INTRINSIC_SET_CDR:
    return x->codeSize = annotateCodeSize(M, x->as.setCdr.cons)
                       + annotateCodeSize(M, x->as.setCdr.value)
                       + 1;

  case RTL_INTRINSIC_SET_ELEM:
    return x->codeSize = annotateCodeSize(M, x->as.setElem.tuple)
                       + annotateCodeSize(M, x->as.setElem.index)
                       + annotateCodeSize(M, x->as.setElem.value)
                       + 1;

  case RTL_INTRINSIC_YIELD:
    return x->codeSize = 2;

  default:
    abort(); // unreachable
  }
}

static
void emitQuoteCode(rtl_Compiler *C, uint16_t fnID, rtl_Word expr);

typedef struct qCodeAccum {
  rtl_Compiler *C;
  uint32_t     fnID;
} qCodeAccum;

static
void __emitEntryQuoteCode(rtl_Machine *M,
                          void        *vaccum,
                          rtl_Word    key,
                          rtl_Word    val)
{
  qCodeAccum *acc = (qCodeAccum *)vaccum;

  emitQuoteCode(acc->C, acc->fnID, key);
  emitQuoteCode(acc->C, acc->fnID, val);
  rtl_emitByteToFunc(acc->C->M->codeBase, acc->fnID, RTL_OP_INSERT);
}

static
void emitMapQuoteCode(rtl_Compiler        *C,
                      uint32_t            fnID,
                      rtl_Word            map)
{
  qCodeAccum acc = {
    .C    = C,
    .fnID = fnID,
  };

  rtl_emitByteToFunc(C->M->codeBase, fnID, RTL_OP_MAP);

  rtl_xVisitMap(C->M, &acc, __emitEntryQuoteCode, map);
  RTL_ASSERT_NO_UNWIND(C->M);
}

void rtl_emitAtomConst(rtl_CodeBase *codeBase, uint32_t fnID, rtl_Word expr)
{
  switch (rtl_typeOf(expr)) {
  case RTL_NIL:
    rtl_emitByteToFunc(codeBase, fnID, RTL_OP_NIL);
    break;

  case RTL_SYMBOL:
    if (expr > 0xFFFFF) {
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_CONST32);
      rtl_emitWordToFunc(codeBase, fnID, expr);
    } else if (expr > 0xFFF) {
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_SYMBOL16);
      rtl_emitShortToFunc(codeBase, fnID, expr >> 4);
    } else {
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_SYMBOL8);
      rtl_emitByteToFunc(codeBase, fnID, expr >> 4);
    } break;

  case RTL_SELECTOR:
    if (expr > 0xFFFFF) {
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_CONST32);
      rtl_emitWordToFunc(codeBase, fnID, expr);
    } else if (expr > 0xFFF) {
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_SELECTOR16);
      rtl_emitShortToFunc(codeBase, fnID, expr >> 4);
    } else {
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_SELECTOR8);
      rtl_emitByteToFunc(codeBase, fnID, expr >> 4);
    } break;

  case RTL_INT28:
    if (expr > 0xFFFFF) {
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_CONST32);
      rtl_emitWordToFunc(codeBase, fnID, expr);
    } else if (expr > 0xFFF) {
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_INT16);
      rtl_emitShortToFunc(codeBase, fnID, expr >> 4);
    } else {
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_INT8);
      rtl_emitByteToFunc(codeBase, fnID, expr >> 4);
    } break;

  case RTL_CHAR:
    if (expr > 0xFFFFF) {
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_CONST32);
      rtl_emitWordToFunc(codeBase, fnID, expr);
    } else if (expr > 0xFFF) {
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_CHAR16);
      rtl_emitShortToFunc(codeBase, fnID, expr >> 4);
    } else {
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_CHAR8);
      rtl_emitByteToFunc(codeBase, fnID, expr >> 4);
    } break;

  case RTL_FIX14:
    if (expr > 0xFFFFF) {
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_CONST32);
      rtl_emitWordToFunc(codeBase, fnID, expr);
    } else if (expr > 0xFFF) {
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_FIX16);
      rtl_emitShortToFunc(codeBase, fnID, expr >> 4);
    } else {
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_FIX8);
      rtl_emitByteToFunc(codeBase, fnID, expr >> 4);
    } break;

  case RTL_TOP:
    rtl_emitByteToFunc(codeBase, fnID, RTL_OP_TOP);
    break;

  }
}

static
void emitQuoteCode(rtl_Compiler *C, uint16_t fnID, rtl_Word expr)
{
  rtl_Word const *rptr;
  size_t i, len;

  rtl_Word a, b;

  rtl_CodeBase *codeBase;

  codeBase = C->M->codeBase;

  switch (rtl_typeOf(expr)) {
  case RTL_NIL:
  case RTL_SYMBOL:
  case RTL_SELECTOR:
  case RTL_INT28:
  case RTL_CHAR:
  case RTL_FIX14:
  case RTL_TOP:
    rtl_emitAtomConst(codeBase, fnID, expr);
    break;

  case RTL_CONS:
    a = rtl_xCar(C->M, expr);
    RTL_ASSERT_NO_UNWIND(C->M);

    b = rtl_xCdr(C->M, expr);
    RTL_ASSERT_NO_UNWIND(C->M);

    emitQuoteCode(C, fnID, a);
    emitQuoteCode(C, fnID, b);
    rtl_emitByteToFunc(codeBase, fnID, RTL_OP_CONS);
    break;

  case RTL_MAP:
    emitMapQuoteCode(C, fnID, expr);
    break;

  case RTL_TUPLE:
    rptr = rtl_xReifyTuple(C->M, expr, &len);
    RTL_ASSERT_NO_UNWIND(C->M);

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

  case RTL_INTRINSIC_SLICE:
    rtl_emitIntrinsicCode(C, fnID, x->as.slice.tuple);
    rtl_emitIntrinsicCode(C, fnID, x->as.slice.beg);
    rtl_emitIntrinsicCode(C, fnID, x->as.slice.end);
    rtl_emitByteToFunc(codeBase, fnID, RTL_OP_SLICE);
    break;

  case RTL_INTRINSIC_GET:
    rtl_emitIntrinsicCode(C, fnID, x->as.get.tuple);
    rtl_emitIntrinsicCode(C, fnID, x->as.get.index);
    rtl_emitByteToFunc(codeBase, fnID, RTL_OP_GET);
    break;

  case RTL_INTRINSIC_DYN_GET:
    rtl_emitByteToFunc(codeBase, fnID, RTL_OP_GET_DYN);
    rtl_emitWordToFunc(codeBase, fnID, x->as.dynGet.name);
    break;

  case RTL_INTRINSIC_DYN_SET:
    rtl_emitIntrinsicCode(C, fnID, x->as.dynSet.value);
    rtl_emitByteToFunc(codeBase, fnID, RTL_OP_SET_DYN);
    rtl_emitWordToFunc(codeBase, fnID, x->as.dynSet.name);
    break;

  case RTL_INTRINSIC_BIND:
    rtl_emitIntrinsicCode(C, fnID, x->as.bind.value);

    rtl_emitByteToFunc(codeBase, fnID, RTL_OP_SAVE_DYN);
    rtl_emitWordToFunc(codeBase, fnID, x->as.bind.name);

    for (i = 0; i < x->as.bind.bodyLen; i++) {
      rtl_emitIntrinsicCode(C, fnID, x->as.bind.body[i]);
      if (i + 1 < x->as.bind.bodyLen) {
        rtl_emitByteToFunc(codeBase, fnID, RTL_OP_POP);
      }
    }

    rtl_emitByteToFunc(codeBase, fnID, RTL_OP_RESTORE_DYN);
    rtl_emitWordToFunc(codeBase, fnID, x->as.bind.name);
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
    rtl_emitIntrinsicCode(C, fnID, x->as.lookup.def);
    rtl_emitByteToFunc(codeBase, fnID, RTL_OP_LOOKUP);
    break;

  case RTL_INTRINSIC_VAR:
    if (x->as.var.global) {
      fnDef = rtl_lookupFn(codeBase, x->as.var.name);

      if (fnDef != NULL && fnDef->fn != RTL_NIL) {
        offs = rtl_nextFuncOffs(codeBase, fnID);
        rtl_emitByteToFunc(codeBase, fnID, RTL_OP_CONST32);

        rtl_emitWordToFunc(codeBase, fnID, fnDef->fn);
      } else {
        offs = rtl_nextFuncOffs(codeBase, fnID);
        rtl_emitByteToFunc(codeBase, fnID, RTL_OP_UNDEF_VAR);

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

    fnDef = rtl_lookupFn(codeBase, x->as.namedCall.name);
    if (fnDef != NULL && fnDef->fn != RTL_NIL) {
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
        rtl_emitByteToFunc(codeBase, fnID, RTL_OP_UNDEF_CALL);
        break;

      case RTL_INTRINSIC_NAMED_TAIL:
        rtl_emitByteToFunc(codeBase, fnID, RTL_OP_UNDEF_TAIL);
        break;

      default:
        abort();
      }

      rtl_emitWordToFunc(codeBase, fnID, x->as.namedCall.name);
      rtl_emitShortToFunc(codeBase, fnID, x->as.namedCall.argsLen);
    }

    rtl_registerCallSite(C, x->as.namedCall.name, rtl_function(fnID), offs);
    break;

  case RTL_INTRINSIC_YIELD:
    // rtl_emitByteToFunc(codeBase, fnID, RTL_OP_YIELD);
    rtl_emitByteToFunc(codeBase, fnID, RTL_OP_NIL); // TODO: implement yield again..
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
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_NIL);
    } else {
      for (i = 0; i < x->as.progn.formsLen; i++) {
        rtl_emitIntrinsicCode(C, fnID, x->as.progn.forms[i]);

        // Ignore the result of all but the last expression.
        if (i + 1 < x->as.progn.formsLen)
          rtl_emitByteToFunc(codeBase, fnID, RTL_OP_POP);
      }
    } break;

  case RTL_INTRINSIC_PROTECT:
    rtl_emitIntrinsicCode(C, fnID, x->as.protect.handler);

    if (x->as.protect.expr->codeSize + 2 < 0xFF) {
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_PROTECT8);
      rtl_emitByteToFunc(codeBase, fnID, x->as.protect.expr->codeSize + 2);

    } else if (x->as.protect.expr->codeSize + 2 < 0xFFFF) {
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_PROTECT16);
      rtl_emitShortToFunc(codeBase, fnID, x->as.protect.expr->codeSize + 2);

    } else {
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_PROTECT32);
      rtl_emitWordToFunc(codeBase, fnID, x->as.protect.expr->codeSize + 2);
    }

    rtl_emitIntrinsicCode(C, fnID, x->as.protect.expr);
    rtl_emitByteToFunc(codeBase, fnID, RTL_OP_END_PROTECT);
    rtl_emitByteToFunc(codeBase, fnID, RTL_OP_POP);
    break;

  case RTL_INTRINSIC_LAMBDA:
    if (x->as.lambda.fnID > 0xFFFF) {
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_CLOSURE32);
      rtl_emitWordToFunc(codeBase, fnID, x->as.lambda.fnID);
    } else if (x->as.lambda.fnID > 0xFF) {
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_CLOSURE16);
      rtl_emitShortToFunc(codeBase, fnID, x->as.lambda.fnID);
    } else {
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_CLOSURE8);
      rtl_emitByteToFunc(codeBase, fnID, x->as.lambda.fnID);
    }

    for (i = 0; i < x->as.lambda.bodyLen; i++) {
      rtl_emitIntrinsicCode(C, x->as.lambda.fnID, x->as.lambda.body[i]);

      // Ignore the result of all but the last expression.
      if (i + 1 < x->as.lambda.bodyLen)
        rtl_emitByteToFunc(codeBase, x->as.lambda.fnID, RTL_OP_POP);
    }

    rtl_emitByteToFunc(codeBase, x->as.lambda.fnID, RTL_OP_RET);
    break;

  case RTL_INTRINSIC_LABELS:
    for (i = 0; i < x->as.labels.labelsLen; i++) {
      y = x->as.labels.labelsFns[i];

      rtl_setFunctionName(codeBase,
                          rtl_function(y->as.lambda.fnID),
                          x->as.labels.labelsNames[i]);

      if (y->as.lambda.fnID > 0xFFFF) {
        rtl_emitByteToFunc(codeBase, fnID, RTL_OP_CONST32);
        rtl_emitWordToFunc(codeBase, fnID, rtl_function(y->as.lambda.fnID));
      } else if (y->as.lambda.fnID > 0xFF) {
        rtl_emitByteToFunc(codeBase, fnID, RTL_OP_FN16);
        rtl_emitShortToFunc(codeBase, fnID, y->as.lambda.fnID);
      } else {
        rtl_emitByteToFunc(codeBase, fnID, RTL_OP_FN8);
        rtl_emitByteToFunc(codeBase, fnID, y->as.lambda.fnID);
      }

      for (j = 0; j < y->as.lambda.bodyLen; j++) {
        rtl_emitIntrinsicCode(C, y->as.lambda.fnID, y->as.lambda.body[j]);

        // Ignore the result of all but the last expression.
        if (j + 1 < y->as.lambda.bodyLen)
          rtl_emitByteToFunc(codeBase, y->as.lambda.fnID, RTL_OP_POP);
      }

      rtl_emitByteToFunc(codeBase, y->as.lambda.fnID, RTL_OP_RET);
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
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_NIL);
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

    rtl_emitByteToFunc(codeBase, newFnID, RTL_OP_RET);

    rtl_defineFn(C, x->as.defun.name, rtl_function(newFnID), false);

    printf("Compiled function '%s:%s'\n",
           rtl_symbolPackageName(x->as.defun.name),
           rtl_symbolName(x->as.defun.name));

    rtl_emitByteToFunc(codeBase, fnID, RTL_OP_CONST32);
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

    rtl_emitByteToFunc(codeBase, newFnID, RTL_OP_RET);

    rtl_defineFn(C, x->as.defmacro.name, rtl_function(newFnID), true);

    printf("Compiled macro    '%s:%s'\n",
           rtl_symbolPackageName(x->as.defmacro.name),
           rtl_symbolName(x->as.defmacro.name));


    rtl_emitByteToFunc(codeBase, fnID, RTL_OP_CONST32);
    rtl_emitWordToFunc(codeBase, fnID, x->as.defmacro.name);
    break;

  case RTL_INTRINSIC_EXPORT:
    rtl_xExport(C, x->as.export);
    rtl_emitByteToFunc(codeBase, fnID, RTL_OP_TOP);
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
  case RTL_INTRINSIC_PUSH_FIRST:
  case RTL_INTRINSIC_PUSH_LAST:
  case RTL_INTRINSIC_CONCAT:
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

    case RTL_INTRINSIC_PUSH_FIRST:
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_PUSH_FIRST);
      break;

    case RTL_INTRINSIC_PUSH_LAST:
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_PUSH_LAST);
      break;

    case RTL_INTRINSIC_CONCAT:
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_CONCAT);
      break;

    default:
      abort(); // Unreachable

    } break;

  case RTL_INTRINSIC_TYPE_PRED:
    rtl_emitIntrinsicCode(C, fnID, x->as.typePred.arg);

    switch (x->as.typePred.type) {
    case RTL_INT28:
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_INT28P);
      break;

    case RTL_FIX14:
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_FIX14P);
      break;

    case RTL_SYMBOL:
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_SYMBOLP);
      break;

    case RTL_SELECTOR:
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_SELECTORP);
      break;

    case RTL_NIL:
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_NILP);
      break;

    case RTL_CONS:
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_CONSP);
      break;

    case RTL_MAP:
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_MAPP);
      break;

    case RTL_CHAR:
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_CHARP);
      break;

    case RTL_TUPLE:
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_TUPLEP);
      break;

    case RTL_TOP:
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_TOPP);
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

    if (x->as._if._else->codeSize < (1 << 7)) {
      elseJmpBytes = 1;
    } else if (x->as._if._else->codeSize < (1 << 15)) {
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
      rtl_emitShortToFunc(codeBase, fnID, x->as._if._else->codeSize);
      break;

    case 4:
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_JMP32);
      rtl_emitWordToFunc(codeBase, fnID, x->as._if._else->codeSize);
      break;
    }

    rtl_emitIntrinsicCode(C, fnID, x->as._if._else);

    break;

  case RTL_INTRINSIC_GENSYM:
    rtl_emitByteToFunc(codeBase, fnID, RTL_OP_GENSYM);
    break;

  case RTL_INTRINSIC_CONSTANT:
    if (x->as.constant == RTL_MAP) {
      rtl_emitByteToFunc(codeBase, fnID, RTL_OP_MAP);

    } else {
      rtl_emitAtomConst(codeBase, fnID, x->as.constant);

    } break;

  case RTL_INTRINSIC_SET_VAR:
    rtl_emitIntrinsicCode(C, fnID, x->as.setVar.value);
    rtl_emitByteToFunc(codeBase, fnID, RTL_OP_SET_VAR);
    rtl_emitShortToFunc(codeBase, fnID, x->as.setVar.frame);
    rtl_emitShortToFunc(codeBase, fnID, x->as.setVar.idx);
    break;

  case RTL_INTRINSIC_SET_CAR:
    rtl_emitIntrinsicCode(C, fnID, x->as.setCar.cons);
    rtl_emitIntrinsicCode(C, fnID, x->as.setCar.value);
    rtl_emitByteToFunc(codeBase, fnID, RTL_OP_SET_CAR);
    break;

  case RTL_INTRINSIC_SET_CDR:
    rtl_emitIntrinsicCode(C, fnID, x->as.setCdr.cons);
    rtl_emitIntrinsicCode(C, fnID, x->as.setCdr.value);
    rtl_emitByteToFunc(codeBase, fnID, RTL_OP_SET_CDR);
    break;

  case RTL_INTRINSIC_SET_ELEM:
    rtl_emitIntrinsicCode(C, fnID, x->as.setElem.tuple);
    rtl_emitIntrinsicCode(C, fnID, x->as.setElem.index);
    rtl_emitIntrinsicCode(C, fnID, x->as.setElem.value);
    rtl_emitByteToFunc(codeBase, fnID, RTL_OP_SET_ELEM);
    break;
  }
}
