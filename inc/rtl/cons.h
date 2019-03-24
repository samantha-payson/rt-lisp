// This file is part of RT Lisp.
//
// RT Lisp is free software: you can redistribute it and/or modify it under the
// terms of the GNU Lesser General Public License as published by the Free
// Software Foundation, either version 3 of the License, or (at your option) any
// later version.
//
// RT Lisp is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
// A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more
// details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with RT Lisp.  If not, see <https://www.gnu.org/licenses/>.

#ifndef _RTL_INSIDE_RT_LISP_H_
# error "rtl/cons.h should only be included indirectly via rt-lisp.h"
#endif

static inline
int rtl_isCons(rtl_Word w) { return rtl_typeOf(w) == RTL_CONS; }

// Return a pointer to two consecutive rtl_Words -- [0] = CAR and [1] = CDR
// respectively.
rtl_Word const *rtl_xReifyCons(rtl_Machine *M, rtl_Word cons);

rtl_Word rtl_cons(rtl_Machine *M, rtl_Word car, rtl_Word cdr);

// xCar and xCdr can be chained without checking for exceptions in-between,
// since they will ALWAYS return RTL_NIL when they throw an exception.
//
// In other words, rtl_xCar(M, rtl_xCar(M, rtl_xCdr(M, x))) will gracefully
// propagate an exception thrown by the cdr.
//
rtl_Word rtl_xCar(rtl_Machine *M, rtl_Word cons);
rtl_Word rtl_xCdr(rtl_Machine *M, rtl_Word cons);

rtl_Word rtl_xReverseListImproper(rtl_Machine *M, rtl_Word ls, rtl_Word last);

static inline
rtl_Word rtl_xReverseList(rtl_Machine *M, rtl_Word ls) {
  return rtl_xReverseListImproper(M, ls, RTL_NIL);
}

size_t rtl_xListLength(rtl_Machine *M, rtl_Word ls);

static inline
rtl_Word rtl_xCaar(rtl_Machine *M, rtl_Word cons) {
  return rtl_xCar(M, rtl_xCar(M, cons));
}

static inline
rtl_Word rtl_xCadr(rtl_Machine *M, rtl_Word cons) {
  return rtl_xCar(M, rtl_xCdr(M, cons));
}

static inline
rtl_Word rtl_xCdar(rtl_Machine *M, rtl_Word cons) {
  return rtl_xCdr(M, rtl_xCar(M, cons));
}

static inline
rtl_Word rtl_xCddr(rtl_Machine *M, rtl_Word cons) {
  return rtl_xCdr(M, rtl_xCdr(M, cons));
}

static inline
rtl_Word rtl_xCaaar(rtl_Machine *M, rtl_Word cons) {
  return rtl_xCar(M, rtl_xCaar(M, cons));
}

static inline
rtl_Word rtl_xCaadr(rtl_Machine *M, rtl_Word cons) {
  return rtl_xCar(M, rtl_xCadr(M, cons));
}

static inline
rtl_Word rtl_xCadar(rtl_Machine *M, rtl_Word cons) {
  return rtl_xCar(M, rtl_xCdar(M, cons));
}

static inline
rtl_Word rtl_xCaddr(rtl_Machine *M, rtl_Word cons) {
  return rtl_xCar(M, rtl_xCddr(M, cons));
}

static inline
rtl_Word rtl_xCdaar(rtl_Machine *M, rtl_Word cons) {
  return rtl_xCdr(M, rtl_xCaar(M, cons));
}

static inline
rtl_Word rtl_xCdadr(rtl_Machine *M, rtl_Word cons) {
  return rtl_xCdr(M, rtl_xCadr(M, cons));
}

static inline
rtl_Word rtl_xCddar(rtl_Machine *M, rtl_Word cons) {
  return rtl_xCdr(M, rtl_xCdar(M, cons));
}

static inline
rtl_Word rtl_xCdddr(rtl_Machine *M, rtl_Word cons) {
  return rtl_xCdr(M, rtl_xCddr(M, cons));
}
