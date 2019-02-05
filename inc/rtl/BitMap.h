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
# error "rtl/BitMap.h should only be included indirectly via rt-lisp.h"
#endif

// Implemented based on the paper
//
//     "Fast, Small, Simple Rank/Select on Bitmaps"
//
// by Gonzalo Novarro and Eliana Providel.
typedef struct rtl_BitMap {
  uint32_t nbrBits, nbrBlocks, nbrOnes;

  uint32_t *sampleRank, *sampleSelect;

  uint32_t blocks[];
} rtl_BitMap;


// Allocate a new bitmap of size nbrBits in contiguous memory.
rtl_BitMap *rtl_newBitMap(size_t nbrBits);

static inline
bool rtl_bmpGetBit(rtl_BitMap const *bmp, uint32_t idx)
{
  assert(idx < bmp->nbrBits);

  uint32_t bit = 1 << (idx & 0x1F);

  return bmp->blocks[idx >> 5] & bit;
}

// Set the value of a bit and return the old value.
static inline
bool rtl_bmpSetBit(rtl_BitMap *bmp, uint32_t idx, bool value)
{
  assert(idx < bmp->nbrBits);

  uint32_t block = bmp->blocks[idx >> 5];
  uint32_t bit   = 1 << (idx & 0x1F);

  bool oldValue = 0 != (block & bit);

  if (value) {
    block |= bit;
  } else {
    block &= ~bit;
  }
  bmp->blocks[idx >> 5] = block;

  assert(rtl_bmpGetBit(bmp, idx) == value);

  return oldValue;
}

// Populates the rank/select tables, according to the current state of the bitmap.
void rtl_bmpTabulate(rtl_BitMap *bmp);

void rtl_bmpClearAll(rtl_BitMap *bmp);

uint32_t rtl_bmpSelect(rtl_BitMap *B, uint32_t k);

uint32_t rtl_bmpRank(rtl_BitMap *B, uint32_t k);

uint32_t rtl_scanForKth(uint32_t x, uint32_t k);

void rtl_testBitMap();

