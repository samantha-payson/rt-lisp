#ifndef _RTL_INSIDE_RT_LISP_H_
# error "rtl/symbol.h should only be included indirectly via rt-lisp.h"
#endif

static inline
int rtl_isSymbol(rtl_Word w) { return rtl_typeOf(w) == RTL_SYMBOL; }

char const *rtl_symbolName(rtl_Word w);
uint32_t rtl_symbolID(rtl_Word w);

rtl_Word rtl_intern(char const *str);

