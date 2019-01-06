#ifndef _RTL_INSIDE_RT_LISP_H_
# error "rtl/nil.h should only be included indirectly via rt-lisp.h"
#endif

static inline
int rtl_isNil(rtl_Word w) { return rtl_typeOf(w) == RTL_NIL; }
