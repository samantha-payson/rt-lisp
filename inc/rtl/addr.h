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
# error "rtl/addr.h should only be included indirectly via rt-lisp.h"
#endif

static inline
int rtl_isAddr(rtl_Word w) { return rtl_typeOf(w) == RTL_ADDR; }

static inline
uint16_t rtl_addrPage(rtl_Word w)
{
  return (w >> 20) & 0xFFF;
}

static inline
uint16_t rtl_addrOffs(rtl_Word w)
{
  return (w >> 4) & 0xFFFF;
}

static inline
uint8_t *rtl_resolveAddr(rtl_Machine *M, rtl_Word w)
{
  uint16_t pageID,
           offs;

  assert(rtl_isAddr(w));

  pageID = rtl_addrPage(w);
  offs   = rtl_addrOffs(w);

  assert(pageID < M->pagesLen);
  assert(offs   < M->pages[pageID]->len);

  return M->pages[pageID]->code + offs;
}

static inline
rtl_Word rtl_addr(uint16_t page, uint16_t offs)
{
  return ((rtl_Word)page << 20) | ((rtl_Word)offs << 4) | RTL_ADDR;
}
