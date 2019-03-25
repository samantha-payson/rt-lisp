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

static inline
void rtl_xAssertTuple(rtl_Machine *M, rtl_Word w)
{
  rtl_xAssertType(M, RTL_TUPLE, w);
}

rtl_Word *rtl_allocTuple(rtl_Machine *M, rtl_Word *w, size_t len);

// Return a pointer to the fields of tpl. Write the length of that tuple to
// *len, which MUST point to a valid size_t.
rtl_Word const *rtl_xReifyTuple(rtl_Machine *M, rtl_Word tpl, size_t *len);

// Allocate a tuple with all the same elements, but with an extra position
// containing w at the end.
rtl_Word rtl_xTuplePushLast(rtl_Machine *M, rtl_Word tuple, rtl_Word w);

// Allocate a tuple with all the same elements, but with an extra position
// containing w at the beginning.
rtl_Word rtl_xTuplePushFirst(rtl_Machine *M, rtl_Word tuple, rtl_Word w);

// Allocate a tuple with the elements of a at the beginning, and the elements of
// b at the end.
rtl_Word rtl_xTupleConcat(rtl_Machine *M, rtl_Word a, rtl_Word b);

// Allocate a tuple which contains the elements of the input tuple in the index
// range [beg, end).
rtl_Word rtl_xTupleSlice(rtl_Machine *M,
                         rtl_Word    tuple,
                         uint32_t    beg,
                         uint32_t    end);

// Allocate a new tuple and fill it with elemsLen words pointed to by elems.
rtl_Word rtl_tuple(rtl_Machine *M, rtl_Word *elems, size_t elemsLen);

static inline
size_t rtl_xTupleLen(rtl_Machine *M, rtl_Word tuple)
{
  size_t len;

  rtl_xReifyTuple(M, tuple, &len);
  RTL_UNWIND (M) return 0;

  return len;
}

static inline
rtl_Word rtl_xTuplePopLast(rtl_Machine *M, rtl_Word tuple)
{
  size_t len;

  len = rtl_xTupleLen(M, tuple);
  RTL_UNWIND (M) return RTL_TUPLE;

  return rtl_xTupleSlice(M, tuple, 0, len - 1);
}

static inline
rtl_Word rtl_xTuplePopFirst(rtl_Machine *M, rtl_Word tuple)
{
  size_t len;

  len = rtl_xTupleLen(M, tuple);
  RTL_UNWIND (M) return RTL_TUPLE;

  return rtl_xTupleSlice(M, tuple, 1, len);
}

