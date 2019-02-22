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
# error "rtl/char.h should only be included indirectly via char.h"
#endif

#include "utf8/utf8.h"

static inline
bool rtl_isChar(rtl_Word w) { return rtl_typeOf(w) == RTL_CHAR; }

static inline
rtl_Word rtl_char(utf8_int32_t c) { return (c << 4) | RTL_CHAR; }

static inline
utf8_int32_t rtl_charValue(rtl_Word w) { return (utf8_int32_t)w >> 4; }
