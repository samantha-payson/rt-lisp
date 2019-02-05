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
