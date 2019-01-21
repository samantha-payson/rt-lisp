#ifndef _RTL_INSIDE_RT_LISP_H_
# error "rtl/symbol.h should only be included indirectly via rt-lisp.h"
#endif

static inline
int rtl_isSymbol(rtl_Word w) { return rtl_typeOf(w) == RTL_SYMBOL; }

char const *rtl_symbolName(rtl_Word w);

char const *rtl_symbolPackageName(rtl_Word w);

static inline
uint32_t rtl_symbolID(rtl_Word w)
{
  return w >> 4;
}

static inline
rtl_Word rtl_symbol(uint32_t id)
{
  return (id << 4) | RTL_SYMBOL;
}

static inline
rtl_Word rtl_unresolvedSymbol(uint32_t id)
{
  return (id << 4) | RTL_UNRESOLVED_SYMBOL;
}

// These two functions are part of the symbol resolution process which takes
// place during macro expansion. They return symbol IDs:

// Intern a new unresolved symbol. For a symbol which doesn't have a package
// specifier, pkg should be NULL.
uint32_t rtl_internUnresolvedID(char const *pkg, char const *name);


// Intern a package. This expects the package's name in the global namespace.
uint32_t rtl_internPackageID(char const *name);

// Intern a symbol.
uint32_t rtl_internSymbolID(uint32_t pkgID, char const *name);

// This is the C-side name resolution code. It's not super efficient, but later
// on we can replace it with better lisp code.

typedef enum rtl_NSType {
  RTL_NS_IN_PKG,
  RTL_NS_USE_PKG,
  RTL_NS_PKG_ALIAS,
  RTL_NS_ALIAS,
} rtl_NSType;

typedef struct rtl_PkgExport {
  char const *name;
  rtl_Word   symbol;
} rtl_PkgExport;

typedef struct rtl_Package {
  char const *name;
  uint32_t   id;

  rtl_PkgExport *exports;
  size_t        exportsCap,
                exportsLen;

  // Points to the next package in the rtl_Compiler's package hash table.
  struct rtl_Package *next;
} rtl_Package;

typedef struct rtl_NameSpace {
  rtl_NSType type;

  struct rtl_NameSpace const *super;

  rtl_Package *currentPkg;

  union {
    struct {
      rtl_Package *pkg;
      char const  *aliasName;
    } pkgAlias;

    struct {
      rtl_Word   symbol;
      char const *aliasName;
    } alias;
  } as;
} rtl_NameSpace;

// Extend super to be in the package pkg.
rtl_NameSpace rtl_nsInPkg(rtl_NameSpace const *super, rtl_Package *pkg);

// Extend super to have alias refer to pkg.
rtl_NameSpace rtl_nsPkgAlias(rtl_NameSpace const *super,
			     rtl_Package *pkg,
			     char const *alias);

// Extend super to have alias refer to symbol.
rtl_NameSpace rtl_nsAlias(rtl_NameSpace const *super,
			  rtl_Word symbol,
			  char const *alias);
