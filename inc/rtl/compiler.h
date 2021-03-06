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

#ifndef _RTL_INSIDE_RT_LISP_H_
# error "rtl/compiler.h should only be included indirectly via rt-lisp.h"
#endif

#define RTL_COMPILER_PKG_HASH_SIZE 17
#define RTL_COMPILER_CALL_SITE_HASH_SIZE 127

typedef struct rtl_CallSite {
  uint32_t version;
  uint16_t fnID;
  uint16_t offs;
} rtl_CallSite;

typedef struct rtl_CallSiteArray {
  rtl_Word fnName;

  rtl_CallSite *sites;
  size_t       sitesLen;
  size_t       sitesCap;

  struct rtl_CallSiteArray *next;
} rtl_CallSiteArray;

typedef struct rtl_Compiler {

  rtl_Machine *M;

  // A hash table which maps function names to arrays of call sites for this
  // compiler instance.
  rtl_CallSiteArray *callSitesByName[RTL_COMPILER_CALL_SITE_HASH_SIZE];

  // A hash table which maps package IDs to packages for this compiler instance.
  rtl_Package *pkgByID[RTL_COMPILER_PKG_HASH_SIZE];

} rtl_Compiler;

// Let the compiler know about a location where a particular function is called,
// so it can be updated when the function changes.
void rtl_registerCallSite(rtl_Compiler *C,
                          rtl_Word     name,
                          rtl_Word     fn,
                          uint32_t     offs);

// Tell the compiler the address (or other indicator)of a function, so it can
// update all of its call sites.
//
// fn may be either an Addr, a Builtin, or a Closure.
void rtl_resolveCallSites(rtl_Compiler *C, rtl_Word name, rtl_Word fn);

void rtl_defineFn(rtl_Compiler *C, rtl_Word name, rtl_Word addr, bool isMacro);

rtl_FnDef *rtl_lookupFn(rtl_CodeBase *cb, rtl_Word name);

// Declare a package as existing. If the package already exists, this function
// does nothing.
rtl_Package *rtl_internPackage(rtl_Compiler *C, char const *pkg);

// Resolve a symbol in the given namespace.
rtl_Word rtl_xResolveSymbol(rtl_Compiler        *C,
                            rtl_NameSpace const *ns,
                            rtl_Word            sym);

// Compile the (already macro-expanded) S-Expr and emit the resulting to the
// fnID'th function of C->M->codeBase.
void rtl_compileExpr(rtl_Compiler *C, uint32_t fnID, rtl_Word sxp);

void rtl_xCompile(rtl_Compiler *C,
                 rtl_NameSpace const *ns,
                 uint32_t fnID,
                 rtl_Word in);

void rtl_initCompiler(rtl_Compiler *C, rtl_Machine *M);

rtl_Word rtl_xMacroExpand(rtl_Compiler *C, rtl_NameSpace const *ns, rtl_Word in);

rtl_Intrinsic *rtl_xExprToIntrinsic(rtl_Compiler *C, rtl_Word sxp);

rtl_Intrinsic *rtl_transformIntrinsic(rtl_Compiler *C, rtl_Intrinsic *x);

void rtl_emitIntrinsicCode(rtl_Compiler *C,
                           uint32_t fnID,
                           rtl_Intrinsic const *x);

// Export a symbol within its package.
void rtl_xExport(rtl_Compiler *C, rtl_Word symbol);
