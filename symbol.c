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

#include <assert.h>
#include <string.h>
#include <stdlib.h>

#include <stdio.h>

#ifdef RTL_LINUX
# include <libexplain/open.h>
# include <libexplain/fdopen.h>
# include <libexplain/flock.h>
#else
# include <errno.h>

# define explain_open(...)   strerror(errno)
# define explain_fdopen(...) strerror(errno)
# define explain_flock(...)  strerror(errno)
#endif

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <fcntl.h>


typedef struct UnresolvedSymbol {
  uint32_t   id;
  char const *name;
  char const *pkg;

  // Next element in the hash table linked list.
  struct UnresolvedSymbol *next;
} UnresolvedSymbol;

typedef struct Package {
  uint32_t   id;
  char const *name;

  // Next element in the hash table linked list.
  struct Package *next;
} Package;

typedef struct Symbol {
  uint32_t      id;
  char const    *name;
  Package const *pkg;

  // Next element in the hash table linked list.
  struct Symbol *next;
} Symbol;

static
uint32_t hashStr(char const *str)
{
  uint32_t h, g;
  char     c;

  h = 0;
  c = *str;
  while (c) {
    h = (h << 5) ^ c;
    g = h & 0xFC000000;
    if (g) {
      // Recycle the top 6 bits back into the lower part of the array.
      h ^= (g >> 25);
    }
    c = *(++str);
  }

  return h;
}

// For now, we'll use global symbol tables. So symbols are consistent across
// seperate machines running in the same process.

// Choose a nice prime number for the size of the hash tables.
#define HASH_SIZE 317

static UnresolvedSymbol *unresTable[HASH_SIZE];
static UnresolvedSymbol **unresByID;
static uint32_t         unresByIDCap;
static uint32_t         unresNextID;

static Symbol   *symTable[HASH_SIZE];
static Symbol   **symByID;
static uint32_t symByIDCap;
static uint32_t symNextID;

static Package  *pkgTable[HASH_SIZE];
static Package  **pkgByID;
static uint32_t pkgByIDCap;
static uint32_t pkgNextID;

uint32_t rtl_internUnresolvedID(char const *pkg, char const *name)
{
  uint32_t hash, idx;
  UnresolvedSymbol *usym;

  assert(name);

  hash = hashStr(name) ^ (pkg ? hashStr(pkg) : 0);
  idx  = hash % HASH_SIZE;

  for (usym = unresTable[idx]; usym; usym = usym->next)
  {
    if (((!pkg && !usym->pkg) || (pkg && usym->pkg && !strcmp(usym->pkg, pkg)))
        && !strcmp(name, usym->name))
    {
      return usym->id;
    }
  }

  // asm("int3");

  // There is no such unresolved symbol at this point -- make a new one.
  usym = malloc(sizeof(UnresolvedSymbol));
  usym->id   = unresNextID;
  usym->name = strdup(name);
  usym->pkg  = pkg ? strdup(pkg) : NULL;

  // Add this new UnresolvedSymbol to its list in the hash table.
  usym->next = unresTable[idx];
  unresTable[idx] = usym;

  // Make sure there is enough capacity for unresByID ..
  if (unresByIDCap <= unresNextID) {
    unresByIDCap = unresByIDCap == 0 ? 32 : 2*unresByIDCap;
    unresByID = realloc(unresByID, sizeof(UnresolvedSymbol *)*unresByIDCap);
  }
  // .. then add this to the array.
  unresByID[unresNextID++] = usym;

  return usym->id;
}

uint32_t rtl_internPackageID(char const *name)
{
  uint32_t hash, idx;
  Package  *pkg;

  assert(name);

  hash = hashStr(name);
  idx  = hash % HASH_SIZE;

  for (pkg = pkgTable[idx]; pkg; pkg = pkg->next)
  {
    if (!strcmp(name, pkg->name))
    {
      return pkg->id;
    }
  }

  // There is no such package at this point -- make a new one.
  pkg = malloc(sizeof(Package));
  pkg->id   = pkgNextID;
  pkg->name = strdup(name);

  // Add this new Package to its list in the hash table.
  pkg->next = pkgTable[idx];
  pkgTable[idx] = pkg;

  // Make sure there is enough capacity for pkgByID ..
  if (pkgByIDCap <= pkgNextID) {
    pkgByIDCap = pkgByIDCap == 0 ? 32 : 2*pkgByIDCap;
    pkgByID = realloc(pkgByID, sizeof(Package *)*pkgByIDCap);
  }

  // .. then add this to the array.
  pkgByID[pkgNextID++] = pkg;

  return pkg->id;
}

// A file descriptor for the on-disk cache of symbols.
static int cacheFD = 0;
static FILE *cache = NULL;

static
void ensureCache() {
  if (cache == NULL) {
    cacheFD = open(".rtl-symbol-cache", O_CREAT | O_RDWR | O_APPEND, 0644);
    if (cacheFD < 0) {
      fprintf(stderr, "\n  crtl: error: %s\n\n",
              explain_open(".rtl-symbol-cache",
                           O_CREAT | O_RDWR | O_APPEND,
                           0644));
      abort();
    }

    cache = fdopen(cacheFD, "a+");
    if (cache == NULL) {
      fprintf(stderr, "\n  crtl: error: %s\n\n",
              explain_fdopen(cacheFD, "a+"));
      abort();
    }
  }
}

// I'm not 100% sure that this will work as intended, but the idea is to
// synchronize the numeric IDs of symbols across multiple (possibly
// simultaneous) process invokations.
static
uint32_t __newSymbolID(Package const *pkg, char const *name) {
  Symbol *sym = NULL;

  uint32_t cPkgID,
           hash,
           idx;

  Package const *cPkg;
  Symbol        *cSym;

  char *colon,
       *newline;

  char const *cPkgName;
  char const *cSymName;

  char   *lineBuf = NULL;
  size_t len      = 0;

  ensureCache();

  // First we lock the on-disk cache, to make sure this operation is atomic WRT
  // the disk.
  if (flock(cacheFD, LOCK_EX) < 0) {
    fprintf(stderr, "\n  crtl: error: %s\n",
            explain_flock(cacheFD, LOCK_EX));
    abort();
  }

  while ((len = getline(&lineBuf, &len, cache)) != -1) {
    colon = strchr(lineBuf, ':');
    assert(colon);

    newline = strchr(lineBuf, '\n'); 
    assert(newline);

    *colon   = '\0';
    *newline = '\0';

    cPkgName = lineBuf;
    cSymName = strdup(colon + 1);

    hash = hashStr(cPkgName) ^ hashStr(cSymName);
    idx  = hash % HASH_SIZE;

    cPkgID = rtl_internPackageID(cPkgName);
    assert(cPkgID < pkgNextID);

    cPkg = pkgByID[cPkgID];

    cSym       = malloc(sizeof(Symbol));
    cSym->id   = symNextID;
    cSym->name = strdup(cSymName);
    cSym->pkg  = cPkg;

    // Make sure that we haven't overflowed into the realm of gensyms.
    assert(cSym->id < (1 << 27));

    // Add this new Symbol to its list in the hash table.
    cSym->next    = symTable[idx];
    symTable[idx] = cSym;

    // Make sure there is enough capacity for symByID ..
    if (symByIDCap <= symNextID) {
      symByIDCap = symByIDCap == 0 ? 32 : 2*symByIDCap;
      symByID = realloc(symByID, sizeof(Symbol *)*symByIDCap);
    }

    // .. then add this to the array.
    symByID[symNextID++] = cSym;

    if (cPkg == pkg && !strcmp(name, cSymName)) {
      // This is the symbol we're looking for.
      sym = cSym;
    }
  }

  if (sym == NULL) {
    fprintf(cache, "%s:%s\n", pkg->name, name);

    sym       = malloc(sizeof(Symbol));
    sym->id   = symNextID;
    sym->name = strdup(name);
    sym->pkg  = pkg;

    hash = hashStr(pkg->name) ^ hashStr(name);
    idx  = hash % HASH_SIZE;

    sym->next     = symTable[idx];
    symTable[idx] = sym;

    // Make sure there is enough capacity for symByID ..
    if (symByIDCap <= symNextID) {
      symByIDCap = symByIDCap == 0 ? 32 : 2*symByIDCap;
      symByID = realloc(symByID, sizeof(Symbol *)*symByIDCap);
    }

    // .. then add this to the array.
    symByID[symNextID++] = sym;
  }

  // We're done with the cache for now, unlock it.
  if (flock(cacheFD, LOCK_UN) < 0) {
    fprintf(stderr, "\n  crtl: error: %s\n",
            explain_flock(cacheFD, LOCK_UN));
    abort();
  }

  return sym->id;
}

uint32_t rtl_internSymbolID(uint32_t pkgID, char const *name)
{
  uint32_t      hash, idx;
  Symbol        *sym;
  Package const *pkg;

  assert(pkgID < pkgNextID);
  pkg = pkgByID[pkgID];

  assert(name);

  hash = hashStr(pkg->name) ^ hashStr(name);
  idx  = hash % HASH_SIZE;

  for (sym = symTable[idx]; sym; sym = sym->next)
  {
    if (!strcmp(name, sym->name) && sym->pkg == pkg)
    {
      return sym->id;
    }
  }

  return __newSymbolID(pkg, name);
}

static uint32_t gensymNextID;

rtl_Word rtl_gensym()
{
  uint32_t id = gensymNextID++ | (1 << 27);

  return rtl_symbol(id);
}

static
uint32_t resolvePkgID(rtl_NameSpace const *ns, UnresolvedSymbol const *usym)
{
  if (usym->pkg) {
    // Search the NS hierarchy for any alias that matches usym->pkg
    for (; NULL != ns; ns = ns->super) {
      switch (ns->type) {
      case RTL_NS_ALIAS_PACKAGE:
        if (!strcmp(usym->pkg, ns->as.aliasPackage.aliasName)) {
          return ns->as.aliasPackage.pkg->id;
        } else {
          continue;
        }

      default:
        continue;
      }
    }

    // If we don't find one, then just intern the package name as-is.
    return rtl_internPackageID(usym->pkg);

  } else {
    return ns->currentPkg->id;
  }
}

static
rtl_Package *rtl_xResolvePackage(rtl_Compiler *C, uint32_t id)
{
  rtl_Package *rtlPkg;
  uint32_t    idx;

  assert(id < pkgNextID);

  idx = id % RTL_COMPILER_PKG_HASH_SIZE;

  for (rtlPkg = C->pkgByID[idx]; NULL != rtlPkg; rtlPkg = rtlPkg->next) {
    if (rtlPkg->id == id) {
      return rtlPkg;
    }
  }

  rtl_throwMsg(C->M, "unknown-package",
               "Can't resolve package with a given ID (likely compiler bug).");

  return NULL;
}

rtl_Word rtl_xResolveSymbol(rtl_Compiler        *C,
                            rtl_NameSpace const *ns,
                            rtl_Word            sym)
{
  UnresolvedSymbol const *usym;
  rtl_NameSpace const    *seek;

  uint32_t    pkgID;
  rtl_Package *pkg;
  size_t      i;

  uint32_t unresID;

  size_t const errBufLen = 1024;

  char errBuf[errBufLen];

  if (rtl_isSymbol(sym)) {
    return sym;
  }

  if (rtl_typeOf(sym) != RTL_UNRESOLVED_SYMBOL) {
    // Throw RTL_SYMBOL for a clearer error message...
    rtl_throwWrongType(C->M, RTL_SYMBOL, sym);
    return RTL_NIL;
  }

  unresID = rtl_symbolID(sym);

  assert(unresID < unresNextID);

  usym  = unresByID[unresID];

  // First check to see if this is an unqualified alias
  if (NULL == usym->pkg) {
    for (seek = ns;
         NULL != seek && seek->type != RTL_NS_IN_PACKAGE;
         seek = seek->super)
    {
      switch (seek->type) {
      case RTL_NS_USE_PACKAGE:
        pkg = seek->as.usePackage.pkg;
        for (i = 0; i < pkg->symbolExportsLen; i++) {
          if (!strcmp(pkg->symbolExports[i].name, usym->name)) {
            return pkg->symbolExports[i].symbol;
          }
        }
        continue;

      case RTL_NS_ALIAS:
        if (!strcmp(usym->name, seek->as.alias.aliasName)) {
          // This symbol refers to an alias, our work here is done.
          return seek->as.alias.symbol;
        }
        continue;

      default:
        continue;
      }
    }
  }

  // Ok, it wasn't a simple alias, now we need to resolve this symbol within a
  // package.
  pkgID = resolvePkgID(ns, usym);
  pkg   = rtl_xResolvePackage(C, pkgID);
  RTL_UNWIND (C->M) return RTL_NIL;

  if (pkg != ns->currentPkg) {
    // pkg is not the current package, this means that the symbol was qualified
    // and the qualifier refers to a different package.
    //
    // Check to see if that package exports a symbol with the name we want ..
    for (i = 0; i < pkg->symbolExportsLen; i++) {
      if (!strcmp(pkg->symbolExports[i].name, usym->name)) {
        // .. then return it if we find one ..
        return pkg->symbolExports[i].symbol;
      }
    }

    // .. or throw an exception if there is no exported symbol with that name.
    snprintf(errBuf, errBufLen,
             "Symbol referred to as %s:%s not exported from package %s!\n",
             usym->pkg, usym->name, pkg->name);

    rtl_throwMsg(C->M, "not-exported", errBuf);
    return RTL_NIL;
  } else {
    // pkg is the current package, just intern the symbol.
    return rtl_symbol(rtl_internSymbolID(pkgID, usym->name));
  }
}

rtl_Package *rtl_internPackage(rtl_Compiler *C, char const *name)
{ 
  Package     *pkg;
  rtl_Package *rtlPkg;
  uint32_t    idx, pkgID;

  pkgID = rtl_internPackageID(name);

  pkg = pkgByID[pkgID];

  idx = pkg->id % RTL_COMPILER_PKG_HASH_SIZE;

  for (rtlPkg = C->pkgByID[idx]; NULL != rtlPkg; rtlPkg = rtlPkg->next) {
    if (rtlPkg) {
      return rtlPkg;
    }
  }

  rtlPkg = malloc(sizeof(rtl_Package));

  rtlPkg->name       = strdup(name);
  rtlPkg->id         = pkg->id;

  rtlPkg->symbolExports    = NULL;
  rtlPkg->symbolExportsLen = 0;
  rtlPkg->symbolExportsCap = 0;

  rtlPkg->next    = C->pkgByID[idx];
  C->pkgByID[idx] = rtlPkg;

  return rtlPkg;
}

void rtl_xExport(rtl_Compiler *C, rtl_Word w)
{
  uint32_t    id;
  size_t      i;
  Symbol      *sym;
  rtl_Package *pkg;

  if (RTL_UNLIKELY(!rtl_isSymbol(w))) {
    rtl_throwWrongType(C->M, RTL_SYMBOL, w);
    return;
  }

  id = rtl_symbolID(w);
  assert(id < symNextID);

  sym = symByID[id];
  pkg = rtl_xResolvePackage(C, sym->pkg->id);
  RTL_UNWIND (C->M) return;

  // Check if this symbol has already been exported ..
  for (i = 0; i < pkg->symbolExportsLen; i++) {
    if (pkg->symbolExports[i].symbol == w) {
      // .. if so, just return.
      return;
    }
  }

  if (pkg->symbolExportsCap == pkg->symbolExportsLen) {
    pkg->symbolExportsCap = pkg->symbolExportsCap == 0 ? 32 : 2*pkg->symbolExportsCap;
    pkg->symbolExports    = realloc(pkg->symbolExports,
                                    sizeof(rtl_PkgSymbolExport)*pkg->symbolExportsCap);
  }

  pkg->symbolExports[pkg->symbolExportsLen++] = (rtl_PkgSymbolExport) {
    .name   = sym->name,
    .symbol = w,
  };
}

char const *rtl_symbolName(rtl_Word w)
{
  uint32_t     id;
  Symbol const *sym;

  if (rtl_isGensym(w)) {
    return "G";
  }

  id = rtl_symbolID(w);
  assert(id < symNextID);

  sym = symByID[id];

  return sym->name;
}

char const *rtl_unresolvedSymbolName(rtl_Word w)
{
  uint32_t               id;
  UnresolvedSymbol const *usym;

  id = rtl_symbolID(w);
  assert(id < unresNextID);

  usym = unresByID[id];

  return usym->name;
}

char const *rtl_symbolPackageName(rtl_Word w) {
  uint32_t      id;
  Symbol const  *sym;

  if (rtl_isGensym(w)) {
    return "__gensym__";
  }

  id = rtl_symbolID(w);

  assert(id < symNextID);
  sym = symByID[id];

  return sym->pkg->name;
}
