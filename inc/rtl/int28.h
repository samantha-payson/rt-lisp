#ifndef _RTL_INSIDE_RT_LISP_H_
# error "rtl/int28.h should only be included indirectly via rt-lisp.h"
#endif

static inline
int rtl_isInt28(rtl_Word w) { return rtl_typeOf(w) == RTL_INT28; }

static inline
int32_t rtl_int28Value(rtl_Word w) { return (int32_t)w >> 4; }

static inline
rtl_Word rtl_int28(int32_t n) { return ((rtl_Word)n << 4) | RTL_INT28; }
