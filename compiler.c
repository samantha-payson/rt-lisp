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

      code[0] = RTL_OP_CALL;
      code[1] = (fn >>  0) & 0xFF;
      code[2] = (fn >>  8) & 0xFF;
      code[3] = (fn >> 16) & 0xFF;
      code[4] = (fn >> 24) & 0xFF;
    }
  }

  csa->sitesLen = w;
}

rtl_Package *rtl_internPackage(rtl_Compiler *C, char const *pkgName)
{
  uint32_t    pkgID;
  uint32_t    idx;
  rtl_Package *pkg;

  pkgID = rtl_internPackageID(pkgName);
  idx   = pkgID % RTL_COMPILER_PKG_HASH_SIZE;

  for (pkg = C->pkgByID[idx]; pkg != NULL; pkg = pkg->next) {
    if (pkg->id == pkgID) {
      return pkg;
    }
  }

  pkg = malloc(sizeof(rtl_Package));

  pkg->name       = strdup(pkgName);
  pkg->id         = pkgID;
  pkg->exports    = NULL;
  pkg->exportsCap = 0;
  pkg->exportsLen = 0;

  pkg->next       = C->pkgByID[idx];
  C->pkgByID[idx] = pkg;
}

// Intern a symbol, with a given package and name. 
rtl_Word rtl_intern(char const *pkg, char const *name)
{
  uint32_t    pkgID;
  uint32_t    id;

  pkgID = rtl_internPackageID(pkg);
  id    = rtl_internSymbolID(pkgID, name);

  return (id << 4) | RTL_SYMBOL;
}

static struct symCache_t {
  struct {
    rtl_Word cons;
  } intrinsic;
} symCache;

static bool symCacheWasInit;

static
void initSymCache() {
  symCache = (struct symCache_t) {
    .intrinsic = {
      .cons = rtl_intern("intrinsic", "cons"),
    },
  };
}

void rtl_compileExpr(rtl_Compiler *C, uint16_t pageID, rtl_Word sxp)
{
  if (unlikely(!symCacheWasInit)) {
    initSymCache();
  }

  if (rtl_isCons(sxp)) {
    if (rtl_car(C->M, sxp) == symCache.intrinsic.cons) {
      rtl_compileExpr(C, pageID, rtl_cadr(C->M, sxp));
      rtl_compileExpr(C, pageID, rtl_caddr(C->M, sxp));

      rtl_emitByteToPage(C->M, pageID, RTL_OP_CONS);

      // TODO: Check that there are only 2 arguments...
    }

  } else if (rtl_isInt28(sxp)) {
    rtl_emitByteToPage(C->M, pageID, RTL_OP_CONST);
    rtl_emitWordToPage(C->M, pageID, sxp);

  } else if (rtl_isNil(sxp)) {
    rtl_emitByteToPage(C->M, pageID, RTL_OP_CONST_NIL);

  }
}

void rtl_initCompiler(rtl_Compiler *C, rtl_Machine *M) {
  C->error = (rtl_CompilerError) {
    .type = RTL_COMPILER_OK,
  };

  C->M = M;

  memset(C->callSitesByName, 0, sizeof(C->callSitesByName));
  memset(C->pkgByID, 0, sizeof(C->pkgByID));
}
