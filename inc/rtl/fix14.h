#ifndef _RTL_INSIDE_RT_LISP_H_
# error "rtl/fix14.h should only be included indirectly via rt-lisp.h"
#endif

static inline
int rtl_isFix14(rtl_Word w) { return rtl_typeOf(w) == RTL_FIX14; }

static inline
float rtl_fix14Value(rtl_Word w) {
  return (float)rtl_int28Value(w) / (float)(1 << 14);
}

static inline
rtl_Word rtl_fix14(float f) {
  return ((rtl_Word)(f * (float)(1 << 14))) << 4 | RTL_FIX14;
}
