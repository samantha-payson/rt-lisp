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
# error "rtl/native.h should only be included indirectly via rt-lisp.h"
#endif

static inline
bool rtl_isNative(rtl_Word w) { return rtl_typeOf(w) == RTL_NATIVE; }

rtl_Word rtl_native(rtl_Machine *M, void const *data, uint32_t size);

void rtl_xReifyNative(rtl_Machine *M, rtl_Word w, void *out, uint32_t size);

uint32_t rtl_sizeOfNative(rtl_Machine *M, rtl_Word native);
