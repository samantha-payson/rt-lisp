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
  uint16_t pageID;
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
// so it can be updated when the page changes.
void rtl_registerCallSite(rtl_Compiler *C, rtl_Word name, rtl_Word siteAddr);

// Tell the compiler the address (or other indicator)of a function, so it can
// update all of its call sites.
//
// fn may be either an Addr, a Builtin, or a Closure.
void rtl_resolveCallSites(rtl_Compiler *C, rtl_Word name, rtl_Word fn);

// Declare a package as existing. If the package already exists, this function
// does nothing.
rtl_Package *rtl_internPackage(rtl_Compiler *C, char const *pkg);

// Intern a symbol, with a given package and name. 
rtl_Word rtl_intern(char const *pkg, char const *name);

// Resolve a symbol in the given namespace.
rtl_Word rtl_resolveSymbol(rtl_Compiler        *C,
			   rtl_NameSpace const *ns,
			   uint32_t            unresID);

// Compile the (already macro-expanded) S-Expr and emit the resulting to the
// pageID'th code page of C->M.
void rtl_compileExpr(rtl_Compiler *C, uint16_t pageID, rtl_Word sxp);

void rtl_initCompiler(rtl_Compiler *C, rtl_Machine *M);

rtl_Word rtl_macroExpand(rtl_Compiler *C, rtl_NameSpace const *ns, rtl_Word in);
