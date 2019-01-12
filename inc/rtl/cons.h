#ifndef _RTL_INSIDE_RT_LISP_H_
# error "rtl/cons.h should only be included indirectly via rt-lisp.h"
#endif

static inline
int rtl_isCons(rtl_Word w) { return rtl_typeOf(w) == RTL_CONS; }

// Return a pointer to two consecutive rtl_Words -- [0] = CAR and [1] = CDR
// respectively.
rtl_Word const *rtl_reifyCons(rtl_Machine *M, rtl_Word cons);

// NOTE: rtl_cons allocates GC memory, so CAR and CDR must be *EITHER* reachable
// from M, or non-ptr types.
//
// TODO: Once it's a bit clearer what usage looks like, we need to find an easy
// way for C users to specify a working set of reachable words.
rtl_Word rtl_cons(rtl_Machine *M, rtl_Word car, rtl_Word cdr);

