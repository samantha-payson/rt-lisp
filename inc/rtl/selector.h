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
# error "rtl/selector.h should only be included indirectly via rt-lisp.h"
#endif

static inline
int rtl_isSelector(rtl_Word w) { return rtl_typeOf(w) == RTL_SELECTOR; }

static inline
rtl_Word rtl_selector(uint32_t id) { return (id << 4) | RTL_SELECTOR; }

static inline
rtl_Word rtl_unresolvedSelector(uint32_t id) {
  return (id << 4) | RTL_UNRESOLVED_SELECTOR;
}

static inline
uint32_t rtl_selectorID(rtl_Word w) { return (w >> 4); }

static inline
char const *rtl_selectorName(rtl_Word w) { return rtl_symbolName(w); }

static inline
char const *rtl_selectorPackageName(rtl_Word w) {
  return rtl_symbolPackageName(w);
}
