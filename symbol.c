#include "rt-lisp.h"

#include <assert.h>
#include <string.h>
#include <stdlib.h>

// DEBUG
#include <stdio.h>

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
    if (g = h & 0xFC000000) {
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

  // There is no such symbol at this point -- make a new one.
  sym = malloc(sizeof(Symbol));
  sym->id   = symNextID;
  sym->name = strdup(name);
  sym->pkg  = pkg;

  // Add this new Symbol to its list in the hash table.
  sym->next = symTable[idx];
  symTable[idx] = sym;

  // Make sure there is enough capacity for symByID ..
  if (symByIDCap <= symNextID) {
    symByIDCap = symByIDCap == 0 ? 32 : 2*symByIDCap;
    symByID = realloc(symByID, sizeof(Symbol *)*symByIDCap);
  }

  // .. then add this to the array.
  symByID[symNextID++] = sym;

  return sym->id;
}

static
uint32_t resolvePkgID(rtl_NameSpace const *ns, UnresolvedSymbol const *usym)
{
  if (usym->pkg) {
    // Search the NS hierarchy for any alias that matches usym->pkg
    for (ns = ns; NULL != ns; ns = ns->super) {
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
rtl_Package *rtl_resolvePackage(rtl_Compiler *C, uint32_t id)
{
  Package     *pkg;
  rtl_Package *rtlPkg;
  uint32_t    idx;

  assert(id < pkgNextID);
  pkg = pkgByID[id];

  idx = id % RTL_COMPILER_PKG_HASH_SIZE;

  for (rtlPkg = C->pkgByID[idx]; NULL != rtlPkg; rtlPkg = rtlPkg->next) {
    if (rtlPkg->id == id) {
      return rtlPkg;
    }
  }

  // We didn't find a package with the given ID, error!
  C->error = __rtl_errNoSuchPackage(pkg->name);

  return NULL;
}

rtl_Word rtl_resolveSymbol(rtl_Compiler        *C,
			   rtl_NameSpace const *ns,
			   uint32_t            unresID)
{
  UnresolvedSymbol const *usym;
  rtl_NameSpace const    *seek;

  uint32_t    pkgID;
  rtl_Package *pkg;
  size_t      i;

  assert(unresID < unresNextID);

  usym  = unresByID[unresID];

  // First check to see if this is an unqualified alias
  if (NULL == usym->pkg) {
    for (seek = ns; NULL != seek; seek = seek->super) {
      switch (seek->type) {
      case RTL_NS_USE_PACKAGE:
	pkg = seek->as.usePackage.pkg;
	for (i = 0; i < pkg->exportsLen; i++) {
	  if (!strcmp(pkg->exports[i].name, usym->name)) {
	    return pkg->exports[i].symbol;
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
  pkg   = rtl_resolvePackage(C, pkgID);

  if (NULL == pkg) // Check for errors resolving package...
    return RTL_NIL;

  if (pkg != ns->currentPkg) {
    // pkg is not the current package, this means that the symbol was qualified
    // and the qualifier refers to a different package.
    //
    // Check to see if that package exports a symbol with the name we want ..
    for (i = 0; i < pkg->exportsLen; i++) {
      if (!strcmp(pkg->exports[i].name, usym->name)) {
	// .. then return it if we find one ..
	return pkg->exports[i].symbol;
      }
    }

    // .. or return an error if there is no exported symbol with that name.
    C->error = __rtl_errSymbolNotExported(usym->name, pkg);
    abort();
    return RTL_NIL;
  } else {
    // pkg is the current package, just intern the symbol.
    return rtl_symbol(rtl_internSymbolID(pkgID, usym->name));
  }
}

rtl_Package *rtl_newPackage(rtl_Compiler *C, char const *name)
{ 
  Package     *pkg;
  rtl_Package *rtlPkg;
  uint32_t    idx;

  pkg = pkgByID[rtl_internPackageID(name)];

  idx = pkg->id % RTL_COMPILER_PKG_HASH_SIZE;

  for (rtlPkg = C->pkgByID[idx]; NULL != rtlPkg; rtlPkg = rtlPkg->next) {
    if (rtlPkg) {
      return rtlPkg;
    }
  }

  rtlPkg = malloc(sizeof(rtl_Package));

  rtlPkg->name       = strdup(name);
  rtlPkg->id         = pkg->id;
  rtlPkg->exports    = NULL;
  rtlPkg->exportsLen = 0;
  rtlPkg->exportsCap = 0;

  return rtlPkg;
}

void rtl_export(rtl_Compiler *C, rtl_Word w)
{
  uint32_t    id;
  size_t      i;
  Symbol      *sym;
  rtl_Package *pkg;

  assert(rtl_isSymbol(w));

  id = rtl_symbolID(w);
  assert(id < symNextID);

  sym = symByID[id];
  pkg = rtl_resolvePackage(C, sym->pkg->id);

  // Check if this symbol has already been exported ..
  for (i = 0; i < pkg->exportsLen; i++) {
    if (pkg->exports[i].symbol == w)
      // .. if so, just return.
      return;
  }

  if (pkg->exportsCap == pkg->exportsLen) {
    pkg->exportsCap = pkg->exportsCap == 0 ? 32 : 2*pkg->exportsCap;
    pkg->exports    = realloc(pkg->exports,
			      sizeof(rtl_PkgExport)*pkg->exportsCap);
  }

  pkg->exports[pkg->exportsLen++] = (rtl_PkgExport) {
    .name   = sym->name,
    .symbol = w,
  };
}

char const *rtl_symbolName(rtl_Word w)
{
  uint32_t     id;
  Symbol const *sym;

  id = rtl_symbolID(w);
  assert(id < symNextID);

  sym = symByID[id];

  return sym->name;
}

char const *rtl_symbolPackageName(rtl_Word w) {
  uint32_t      id;
  Symbol const  *sym;

  id = rtl_symbolID(w);

  assert(id < symNextID);
  sym = symByID[id];

  return sym->pkg->name;
}
