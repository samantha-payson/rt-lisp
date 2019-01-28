#ifndef _RTL_INSIDE_RT_LISP_H_
# error "rtl/tuple.h should only be included indirectly via rt-lisp.h"
#endif

static inline
int rtl_isTuple(rtl_Word w) { return rtl_typeOf(w) == RTL_TUPLE; }

rtl_Word *rtl_allocTuple(rtl_Machine *M, rtl_Word *w, size_t len);

// Return a pointer to the fields of tpl. Write the length of that tuple to
// *len, which MUST point to a valid size_t.
rtl_Word const *rtl_reifyTuple(rtl_Machine *M, rtl_Word tpl, size_t *len);
