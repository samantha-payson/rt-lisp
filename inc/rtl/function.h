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
# error "rtl/function.h should only be included indirectly via rt-lisp.h"
#endif


static inline
bool rtl_isFunction(rtl_Word w) { return rtl_typeOf(w) == RTL_FUNCTION; }

static inline
uint32_t rtl_functionID(rtl_Word w) { return (w >> 4) & 0xFFFFFFF; }

static inline
rtl_Function *rtl_reifyFunction(rtl_CodeBase *cb, rtl_Word w)
{
  uint32_t fnID;

  assert(rtl_isFunction(w));
  fnID = rtl_functionID(w);

  assert(fnID < cb->fnsLen);
  return cb->fns[fnID];
}

static inline
rtl_Word rtl_function(uint32_t fnID)
{
  return (fnID << 4) | RTL_FUNCTION;
}

rtl_Word rtl_callBuiltin(rtl_Machine    *M,
			 rtl_Word       builtin,
			 rtl_Word const *args,
			 size_t         argsLen);
