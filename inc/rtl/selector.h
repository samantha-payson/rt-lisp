#ifndef _RTL_INSIDE_RT_LISP_H_
# error "rtl/selector.h should only be included indirectly via rt-lisp.h"
#endif

static inline
int rtl_isSelector(rtl_Word w) { return rtl_typeOf(w) == RTL_SELECTOR; }

static inline
rtl_Word rtl_selector(uint32_t id) { return (id << 4) | RTL_SELECTOR; }

static inline
rtl_Word rtl_unresolvedSelector(uint32_t id) {
  return (id << 4) | RTL_UNRESOLVED_SELECTOR;
}

static inline
uint32_t rtl_selectorID(rtl_Word w) { return (w >> 4); }

static inline
char const *rtl_selectorName(rtl_Word w) { return rtl_symbolName(w); }

static inline
char const *rtl_selectorPackageName(rtl_Word w) {
  return rtl_symbolPackageName(w);
}
