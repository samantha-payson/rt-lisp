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
# error "rtl/map.h should only be included indirectly via rt-lisp.h"
#endif

static inline
int rtl_isMap(rtl_Word w) { return rtl_typeOf(w) == RTL_MAP; }


static inline
rtl_Word rtl_emptyMap() { return RTL_MAP; }

static inline
bool rtl_isEmptyMap(rtl_Word w) { return w == RTL_MAP; }

rtl_Word rtl_mapInsert(rtl_Machine *M,
                       rtl_Word    map,
                       rtl_Word    key,
                       rtl_Word    val);

rtl_Word rtl_mapLookup(rtl_Machine *M,
                       rtl_Word    map,
                       rtl_Word    key,
                       rtl_Word    def);

rtl_Word rtl_mapDelete(rtl_Machine *M, rtl_Word map, rtl_Word key);

typedef void (*rtl_MapVisitorFn)(rtl_Machine *M,
                                 void        *accum,
                                 rtl_Word    key,
                                 rtl_Word    val);

void rtl_visitMap(rtl_Machine      *M,
                  void             *accum,
                  rtl_MapVisitorFn fn,
                  rtl_Word         map);

// Shorthand method for setting a selector-keyed field in a map.
static inline
rtl_Word rtl_recordSet(rtl_Machine *M,
                       rtl_Word map,
                       char const *selector,
                       rtl_Word data)
{
  return rtl_mapInsert(M, map,
                       rtl_internSelector(NULL, selector),
                       data);
}