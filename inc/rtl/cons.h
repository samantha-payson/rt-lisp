#ifndef _RTL_INSIDE_RT_LISP_H_
# error "rtl/cons.h should only be included indirectly via rt-lisp.h"
#endif

static inline
int rtl_isCons(rtl_Word w) { return rtl_typeOf(w) == RTL_CONS; }

// Return a pointer to two consecutive rtl_Words -- [0] = CAR and [1] = CDR
// respectively.
rtl_Word const *rtl_reifyCons(rtl_Machine *M, rtl_Word cons);

// NOTE: rtl_cons allocates GC memory, so if either of CAR or CDR is a pointer
// type, then the memory they point to MUST be reachable from M.
//
// TODO: Once it's a bit clearer what usage looks like, we need to find an easy
// way for C users to specify a working set of reachable words.
rtl_Word rtl_cons(rtl_Machine *M, rtl_Word car, rtl_Word cdr);

rtl_Word rtl_car(rtl_Machine *M, rtl_Word cons);
rtl_Word rtl_cdr(rtl_Machine *M, rtl_Word cons);

rtl_Word rtl_reverseListImproper(rtl_Machine *M, rtl_Word ls, rtl_Word last);

static inline
rtl_Word rtl_reverseList(rtl_Machine *M, rtl_Word ls) {
  return rtl_reverseListImproper(M, ls, RTL_NIL);
}

size_t rtl_listLength(rtl_Machine *M, rtl_Word ls);

static inline
rtl_Word rtl_caar(rtl_Machine *M, rtl_Word cons) {
  return rtl_car(M, rtl_car(M, cons));
}

static inline
rtl_Word rtl_cadr(rtl_Machine *M, rtl_Word cons) {
  return rtl_car(M, rtl_cdr(M, cons));
}

static inline
rtl_Word rtl_cdar(rtl_Machine *M, rtl_Word cons) {
  return rtl_cdr(M, rtl_car(M, cons));
}

static inline
rtl_Word rtl_cddr(rtl_Machine *M, rtl_Word cons) {
  return rtl_cdr(M, rtl_cdr(M, cons));
}

static inline
rtl_Word rtl_caaar(rtl_Machine *M, rtl_Word cons) {
  return rtl_car(M, rtl_caar(M, cons));
}

static inline
rtl_Word rtl_caadr(rtl_Machine *M, rtl_Word cons) {
  return rtl_car(M, rtl_cadr(M, cons));
}

static inline
rtl_Word rtl_cadar(rtl_Machine *M, rtl_Word cons) {
  return rtl_car(M, rtl_cdar(M, cons));
}

static inline
rtl_Word rtl_caddr(rtl_Machine *M, rtl_Word cons) {
  return rtl_car(M, rtl_cddr(M, cons));
}

static inline
rtl_Word rtl_cdaar(rtl_Machine *M, rtl_Word cons) {
  return rtl_cdr(M, rtl_caar(M, cons));
}

static inline
rtl_Word rtl_cdadr(rtl_Machine *M, rtl_Word cons) {
  return rtl_cdr(M, rtl_cadr(M, cons));
}

static inline
rtl_Word rtl_cddar(rtl_Machine *M, rtl_Word cons) {
  return rtl_cdr(M, rtl_cdar(M, cons));
}

static inline
rtl_Word rtl_cdddr(rtl_Machine *M, rtl_Word cons) {
  return rtl_cdr(M, rtl_cddr(M, cons));
}
