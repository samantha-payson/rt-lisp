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

typedef enum rtl_CompilerErrorType {
  RTL_COMPILER_OK,
  RTL_COMPILER_SYMBOL_NOT_EXPORTED,
  RTL_COMPILER_NO_SUCH_PACKAGE,
} rtl_CompilerErrorType;

typedef struct rtl_CompilerError {
  rtl_CompilerErrorType type;

  union {
    struct {
      char const  *name;
      rtl_Package *pkg;
    } symbolNotExported;

    struct {
      char const *name;
    } noSuchPackage;
  } as;
} rtl_CompilerError;

static inline
rtl_CompilerError __rtl_errSymbolNotExported(char const  *name,
                                             rtl_Package *pkg)
{
  return (rtl_CompilerError) {
    .type = RTL_COMPILER_SYMBOL_NOT_EXPORTED,
    .as = {
      .symbolNotExported = {
        .name = name,
        .pkg  = pkg,
      },
    },
  };
}

static inline
rtl_CompilerError __rtl_errNoSuchPackage(char const  *name)
{
  return (rtl_CompilerError) {
    .type = RTL_COMPILER_NO_SUCH_PACKAGE,
    .as = {
      .noSuchPackage = {
        .name = name,
      },
    },
  };
}

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
  rtl_CompilerError error;

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

// Intern a symbol, with a given package and name. 
rtl_Word rtl_intern(char const *pkg, char const *name);

// Package may be NULL or "" for selectors w/ no package.
rtl_Word rtl_internSelector(char const *pkg, char const *name);

// Resolve a symbol in the given namespace.
rtl_Word rtl_resolveSymbol(rtl_Compiler        *C,
                           rtl_NameSpace const *ns,
                           uint32_t            unresID);

// Resolve a symbol in the given namespace.
rtl_Word rtl_resolveSelector(rtl_Compiler        *C,
                             rtl_NameSpace const *ns,
                             uint32_t            unresID);

// Compile the (already macro-expanded) S-Expr and emit the resulting to the
// fnID'th function of C->M->codeBase.
void rtl_compileExpr(rtl_Compiler *C, uint32_t fnID, rtl_Word sxp);

void rtl_compile(rtl_Compiler *C,
                 rtl_NameSpace const *ns,
                 uint32_t fnID,
                 rtl_Word in);

void rtl_initCompiler(rtl_Compiler *C, rtl_Machine *M);

rtl_Word rtl_macroExpand(rtl_Compiler *C, rtl_NameSpace const *ns, rtl_Word in);

rtl_Intrinsic *rtl_exprToIntrinsic(rtl_Compiler *C, rtl_Word sxp);

rtl_Intrinsic *rtl_transformIntrinsic(rtl_Intrinsic *x);

void rtl_emitIntrinsicCode(rtl_Compiler *C,
                           uint32_t fnID,
                           rtl_Intrinsic const *x);

// Export a symbol within its package.
void rtl_export(rtl_Compiler *C, rtl_Word symbol);
