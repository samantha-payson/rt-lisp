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
# error "rtl/string.h should only be included indirectly via rt-lisp.h"
#endif

static inline
bool rtl_isString(rtl_Word w) { return rtl_typeOf(w) == RTL_TUPLE; }

rtl_Word rtl_string(rtl_Machine *M, char const *cstr);

rtl_Word rtl_stringWithLen(rtl_Machine *M, char const *cstr, size_t len);

char const *rtl_reifyString(rtl_Machine *M, rtl_Word str, size_t *len);
