#ifndef _RTL_INSIDE_RT_LISP_H_
# error "rtl/top.h should only be included indirectly via rt-lisp.h"
#endif

static inline
int rtl_isTop(rtl_Word w) { return rtl_typeOf(w) == RTL_TOP; }
