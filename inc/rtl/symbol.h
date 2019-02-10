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
# error "rtl/symbol.h should only be included indirectly via rt-lisp.h"
#endif

static inline
int rtl_isSymbol(rtl_Word w) { return rtl_typeOf(w) == RTL_SYMBOL; }

char const *rtl_symbolName(rtl_Word w);

char const *rtl_symbolPackageName(rtl_Word w);

char const *rtl_unresolvedSymbolName(rtl_Word w);

static inline
uint32_t rtl_symbolID(rtl_Word w)
{
  return w >> 4;
}

// Gensyms have the highest bit set.
static inline
bool rtl_isGensym(rtl_Word w)
{
  return rtl_isSymbol(w) && (w & (1 << 31));
}

rtl_Word rtl_gensym();

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
  RTL_NS_IN_PACKAGE,
  RTL_NS_USE_PACKAGE,
  RTL_NS_ALIAS_PACKAGE,
  RTL_NS_ALIAS,
} rtl_NSType;

typedef struct rtl_PkgSymbolExport {
  char const *name;
  rtl_Word   symbol;
} rtl_PkgSymbolExport;

typedef struct rtl_PkgSelectorExport {
  char const *name;
  rtl_Word   selector;
} rtl_PkgSelectorExport;

typedef struct rtl_Package {
  char const *name;
  uint32_t   id;

  rtl_PkgSymbolExport *symbolExports;
  size_t              symbolExportsCap,
                      symbolExportsLen;

  rtl_PkgSelectorExport *selectorExports;
  size_t                selectorExportsCap,
                        selectorExportsLen;

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
    } aliasPackage;

    struct {
      rtl_Package *pkg;
    } usePackage;

    struct {
      rtl_Word   symbol;
      char const *aliasName;
    } alias;
  } as;
} rtl_NameSpace;

// Extend super to be in the package pkg.
static inline
rtl_NameSpace rtl_nsInPackage(rtl_NameSpace const *super, rtl_Package *pkg)
{
  return (rtl_NameSpace) {
    .type       = RTL_NS_IN_PACKAGE,
    .super      = super,
    .currentPkg = pkg,
  };
}

// Bring all symbols exported by pkg into scope.
static inline
rtl_NameSpace rtl_nsUsePackage(rtl_NameSpace const *super, rtl_Package *pkg)
{
  return (rtl_NameSpace) {
    .type       = RTL_NS_USE_PACKAGE,
    .super      = super,
    .currentPkg = super->currentPkg,
    .as    = {
      .usePackage = {
	.pkg = pkg,
      },
    },
  };
}

// Extend super to have alias refer to pkg.
static inline
rtl_NameSpace rtl_nsAliasPackage(rtl_NameSpace const *super,
				 rtl_Package *pkg,
				 char const *alias)
{
  return (rtl_NameSpace) {
    .type       = RTL_NS_ALIAS_PACKAGE,
    .super      = super,
    .currentPkg = super->currentPkg,
    .as    = {
      .aliasPackage = {
	.pkg       = pkg,
	.aliasName = alias,
      },
    },
  };
}

// Extend super to have alias refer to symbol.
static inline
rtl_NameSpace rtl_nsAlias(rtl_NameSpace const *super,
			  rtl_Word symbol,
			  char const *alias)
{
  return (rtl_NameSpace) {
    .type       = RTL_NS_ALIAS,
    .super      = super,
    .currentPkg = super->currentPkg,
    .as    = {
      .alias = {
	.symbol    = symbol,
	.aliasName = alias,
      },
    },
  };
}
