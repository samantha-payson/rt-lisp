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
# error "rtl/int28.h should only be included indirectly via rt-lisp.h"
#endif

static inline
int rtl_isInt28(rtl_Word w) { return rtl_typeOf(w) == RTL_INT28; }

static inline
void rtl_xAssertInt28(rtl_Machine *M, rtl_Word w)
{
  rtl_xAssertType(M, RTL_INT28, w);
}

static inline
int32_t rtl_int28Value(rtl_Word w) { return (int32_t)w >> 4; }

static inline
rtl_Word rtl_int28(int32_t n) { return ((rtl_Word)n << 4) | RTL_INT28; }
