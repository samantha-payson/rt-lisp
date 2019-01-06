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
void rtl_bmpSetBit(rtl_BitMap *bmp, uint32_t idx, bool value)
{
  assert(idx < bmp->nbrBits);

  if (value) {
    bmp->blocks[idx >> 5] |= (1 << (idx & 31));
  } else {
    bmp->blocks[idx >> 5] &= ~(1 << (idx & 31));
  }
}

static inline
bool rtl_bmpGetBit(rtl_BitMap const *bmp, uint32_t idx)
{
  assert(idx < bmp->nbrBits);

  return bmp->blocks[idx >> 5] & (1 << (idx & 31));
}

// Populates the rank/select tables, according to the current state of the bitmap.
void rtl_bmpTabulate(rtl_BitMap *bmp);

void rtl_bmpClearAll(rtl_BitMap *bmp);

uint32_t rtl_bmpSelect(rtl_BitMap *B, uint32_t k);

uint32_t rtl_bmpRank(rtl_BitMap *B, uint32_t k);

uint32_t rtl_scanForKth(uint32_t x, uint32_t k);

void rtl_testBitMap();

