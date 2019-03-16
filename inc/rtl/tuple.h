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
# error "rtl/tuple.h should only be included indirectly via rt-lisp.h"
#endif

static inline
int rtl_isTuple(rtl_Word w) { return rtl_typeOf(w) == RTL_TUPLE; }

rtl_Word *rtl_allocTuple(rtl_Machine *M, rtl_Word *w, size_t len);

// Return a pointer to the fields of tpl. Write the length of that tuple to
// *len, which MUST point to a valid size_t.
rtl_Word const *rtl_reifyTuple(rtl_Machine *M, rtl_Word tpl, size_t *len);

// Allocate a tuple with all the same elements, but with an extra position
// containing w at the end.
rtl_Word rtl_tuplePushLast(rtl_Machine *M, rtl_Word tuple, rtl_Word w);

// Allocate a tuple with all the same elements, but with an extra position
// containing w at the beginning.
rtl_Word rtl_tuplePushFirst(rtl_Machine *M, rtl_Word tuple, rtl_Word w);

// Allocate a tuple with the elements of a at the beginning, and the elements of
// b at the end.
rtl_Word rtl_tupleConcat(rtl_Machine *M, rtl_Word a, rtl_Word b);

// Allocate a tuple which contains the elements of the input tuple in the index
// range [beg, end).
rtl_Word rtl_tupleSlice(rtl_Machine *M,
                        rtl_Word    tuple,
                        uint32_t    beg,
                        uint32_t    end);

// Allocate a new tuple and fill it with elemsLen words pointed to by elems.
rtl_Word rtl_tuple(rtl_Machine *M, rtl_Word *elems, size_t elemsLen);

static inline
size_t rtl_tupleLen(rtl_Machine *M, rtl_Word tuple)
{
  size_t len;

  rtl_reifyTuple(M, tuple, &len);

  return len;
}

static inline
rtl_Word rtl_tuplePopLast(rtl_Machine *M, rtl_Word tuple)
{
  return rtl_tupleSlice(M, tuple, 0, rtl_tupleLen(M, tuple) - 1);
}

static inline
rtl_Word rtl_tuplePopFirst(rtl_Machine *M, rtl_Word tuple)
{
  return rtl_tupleSlice(M, tuple, 1, rtl_tupleLen(M, tuple));
}
