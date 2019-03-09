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

#include "rt-lisp.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define RANK_STRIDE   128
#define SELECT_STRIDE 512


rtl_BitMap *rtl_newBitMap(size_t nbrBits)
{
  size_t nbrBlocks,
         nbrRank,
         nbrSelect;
  rtl_BitMap *bmp;

  nbrBlocks  = (nbrBits + 31) / 32;

  nbrSelect = (nbrBits + SELECT_STRIDE - 1) / SELECT_STRIDE;
  nbrRank   = (nbrBits + RANK_STRIDE   - 1) / RANK_STRIDE;

  bmp = malloc(sizeof(rtl_BitMap)
               + nbrSelect*sizeof(uint32_t)
               + nbrRank*sizeof(uint32_t)
               + nbrBlocks*sizeof(uint32_t));

  bmp->nbrBits   = nbrBits;
  bmp->nbrBlocks = nbrBlocks;

  // This will be filled out when rtl_bmpTabulate is called.
  bmp->nbrOnes = 0;

  bmp->sampleRank   = bmp->blocks     + nbrBlocks;
  bmp->sampleSelect = bmp->sampleRank + nbrRank;

  rtl_bmpClearAll(bmp);

  return bmp;
}

static
uint32_t mask32(uint32_t n)
{
  return (1 << n) - 1;
}

// Given x, a u32 with at least K 1-bits, return the index of the kᵗʰ bit. The
// index of the lowest order bit is 0, highest order bit is 31.
//
// The return value of this function is undefined if there are less than k bits
// in x.
uint32_t rtl_scanForKth(uint32_t x, uint32_t k)
{
  uint32_t lo, mid, hi;

  lo = 0, hi = 31;

  while (lo < hi) {
    mid = ((lo + hi + 1) >> 1);

    if (__builtin_popcount(mask32(mid) & x) <= k) {
      lo = mid;
    } else {
      hi = mid - 1;
    }
  }

  return lo;
}

static
uint32_t debugNbrOnes(rtl_BitMap *bmp) {
  uint32_t i, count;

  for (count = 0, i = 0; i < bmp->nbrBits; i++) {
    if (rtl_bmpGetBit(bmp, i)) {
      count++;
    }
  }

  return count;
}

void rtl_bmpTabulate(rtl_BitMap *bmp)
{
  uint32_t i, rankIdx, selectIdx, blockIdx;

  uint32_t nbrRankSamples,
           accumulatedRank;

  uint32_t word, popcnt;

  uint32_t selectSample,
           nextSelect,
           selectCorrection;

  accumulatedRank = 0;
  selectIdx       = 0;
  nbrRankSamples  = (bmp->nbrBits + RANK_STRIDE - 1) / RANK_STRIDE;
  nextSelect      = 1;

  for (rankIdx = 0; rankIdx < nbrRankSamples; rankIdx++) {
    bmp->sampleRank[rankIdx] = accumulatedRank;

    for (i = 0, blockIdx = rankIdx*(RANK_STRIDE >> 5) + i;
         i < RANK_STRIDE >> 5 && blockIdx < bmp->nbrBlocks;
         i++, blockIdx = rankIdx*(RANK_STRIDE >> 5) + i)
    {
      word   = bmp->blocks[blockIdx];
      popcnt = __builtin_popcount(word);

      if (accumulatedRank + popcnt > nextSelect) {
        // Since a selectSample just points at the 32-bit block CONTAINING the
        // nᵗʰ bit, this correction gives the difference between the rank at the
        // beginning of that block and the rank that we're searching for.
        //
        // This value is guaranteed to be less than 32.
        selectCorrection = nextSelect - accumulatedRank;

        // Store the block index in the top 27 bits (or 11 bits, or 3 bits,
        // depending on bmp->sampleBits), and the correction in the bottom 5.
        selectSample = (blockIdx << 5) | selectCorrection;

        bmp->sampleSelect[selectIdx] = selectSample;

        // Update nextSelect and selectIdx ; now that this sample has been
        // computed we'll be looking for the next one.
        nextSelect = (++selectIdx) * SELECT_STRIDE + 1;
      }

      accumulatedRank += popcnt;
    }
  }

  bmp->nbrOnes = accumulatedRank;

#ifdef RTL_BITMAP_SANITY_CHECKS
  assert(bmp->nbrOnes == debugNbrOnes(bmp));
#endif
}

void rtl_bmpClearAll(rtl_BitMap *bmp)
{
  size_t nbrRank,
         nbrSelect;

  nbrSelect = (bmp->nbrBits + SELECT_STRIDE - 1) / SELECT_STRIDE;
  nbrRank   = (bmp->nbrBits + RANK_STRIDE - 1) / RANK_STRIDE;

  memset(bmp->blocks,
         0,
         nbrRank*sizeof(uint32_t)
         + nbrSelect*sizeof(uint32_t)
         + bmp->nbrBlocks*sizeof(uint32_t));
  }

void rtl_bmpRelease(rtl_BitMap *bmp)
{
  free(bmp);
}


// A very slow but extremely straightforward implementation of RANK, for testing
// purposes.
static
uint32_t controlRank(rtl_BitMap *B, uint32_t k)
{
  uint32_t rank, select;

  for (rank = select = 0; select < k; select++)
    if (rtl_bmpGetBit(B, select))
      rank++;

  return rank;
}


// A very slow but extremely straightforward implementation of SELECT, for testing
// purposes.
static
uint32_t controlSelect(rtl_BitMap *B, uint32_t k)
{
  uint32_t rank, select;

  rank = 0;
  select = -1;

  do {
    select++;
    if (rtl_bmpGetBit(B, select)) rank++;
  } while (rank <= k);
  
  /* for (rank = select = 0; rank < k; select++) */
  /*   if (select > 0 && rtl_bmpGetBit(B, select)) */
  /*     rank++; */

  return select;
}

// Return the number of bits that are set before index k in B.
uint32_t rtl_bmpRank(rtl_BitMap *B, uint32_t k)
{
  uint32_t rankIdx,
           rank,
           i,
           mask;

  // If it turns out we want to be able to query past the end of the bitmap, we
  // could add some logic for that instead of this assertion.
  assert(k < B->nbrBits);

  rankIdx = k / RANK_STRIDE;

  rank = B->sampleRank[rankIdx];

  for (i = rankIdx*(RANK_STRIDE >> 5); i < k >> 5; i++) {
    rank += __builtin_popcount(B->blocks[i]);
  }

  mask = (1 << (k & 0x1F)) - 1;
  if (mask) { // Don't execute this if mask == 0, then B->blocks[i] might be
              // past the end of the array!
    rank += __builtin_popcount(mask & B->blocks[i]);
  }

#ifdef RTL_BITMAP_SANITY_CHECKS
  assert(rank == controlRank(B, k));
#endif

  return rank;
}

// Return the index of the kᵗʰ bit set in B.
uint32_t rtl_bmpSelect(rtl_BitMap *B, uint32_t k)
{
  uint32_t selectIdx,
           rankIdx,
           sample,
           popcnt,
           scanRank,
           rank,
           i,
           select;

  // If it turns out we want to be able to query past the end of the bitmap, we
  // could add some logic for that instead of this assertion.
  assert(k < B->nbrOnes);

  selectIdx = (k ? k - 1 : k) / SELECT_STRIDE;

  sample  = B->sampleSelect[selectIdx];
  rankIdx = (sample ? sample - 1 : sample) / RANK_STRIDE;
  rank = B->sampleRank[rankIdx];

  for (scanRank = B->sampleRank[rankIdx + 1];
       scanRank < k && rankIdx + 1 < (B->nbrBlocks / (RANK_STRIDE >> 5));
       scanRank = B->sampleRank[++rankIdx + 1])
  {
    rank = scanRank;
  }

  for (i = rankIdx *(RANK_STRIDE >> 5) ;; i++) {
    popcnt = __builtin_popcount(B->blocks[i]);

    if (popcnt + rank > k) {
      break;
    } else {
      rank += popcnt;
    }
  }

  select = i*32 + rtl_scanForKth(B->blocks[i], k - rank);
  if (select > B->nbrBits) {
    asm("int3");
  }

#ifdef RTL_BITMAP_SANITY_CHECKS
  assert(select == controlSelect(B, k));
#endif

  return select;
}


static
  void bmpTestRankSelect(rtl_BitMap *bmp, uint32_t size)
{
  uint32_t i;

  for (i = 0; i < size; i++) {
    uint32_t have = rtl_bmpRank(bmp, i);
    uint32_t want = controlRank(bmp, i);

    if (have != want)
      printf("rank(%d): got %d, expected %d.\n", i, have, want);
  }

  for (i = 0; i < bmp->nbrOnes; i++) {
    uint32_t have = rtl_bmpSelect(bmp, i);
    uint32_t want = controlSelect(bmp, i);

    if (have != want)
      printf("select(%d): got %d, expected %d.\n", i, have, want);
  }
}

void rtl_testBitMap(uint32_t size)
{
  rtl_BitMap *bmp;
  uint32_t i;

  bmp = rtl_newBitMap(size);

  // Test 1: all bits are set.
  for (i = 0; i < size; i++) {
    rtl_bmpSetBit(bmp, i, 1);
  }

  rtl_bmpTabulate(bmp);
  bmpTestRankSelect(bmp, size);
  rtl_bmpClearAll(bmp);

  printf("Passed Test #1!\n");

  // Test 2: every other bit is set.
  for (i = 0; i < size; i++) {
    rtl_bmpSetBit(bmp, i, i & 1);
  }

  rtl_bmpTabulate(bmp);
  bmpTestRankSelect(bmp, size);
  rtl_bmpClearAll(bmp);

  printf("Passed Test #2!\n");

  // Test 3: a random 10% of bits are set.
  for (i = 0; i < size; i++) {
    rtl_bmpSetBit(bmp, i, random() % 100 < 10);
  }

  rtl_bmpTabulate(bmp);
  bmpTestRankSelect(bmp, size);
  rtl_bmpClearAll(bmp);

  printf("Passed Test #3!\n");

  // Test 4: a random 25% of bits are set.
  for (i = 0; i < size; i++) {
    rtl_bmpSetBit(bmp, i, random() % 100 < 25);
  }

  rtl_bmpTabulate(bmp);
  bmpTestRankSelect(bmp, size);
  rtl_bmpClearAll(bmp);

  printf("Passed Test #4!\n");

  // Test 5: a random 50% of bits are set.
  for (i = 0; i < size; i++) {
    rtl_bmpSetBit(bmp, i, random() % 100 < 50);
  }

  rtl_bmpTabulate(bmp);
  bmpTestRankSelect(bmp, size);
  rtl_bmpClearAll(bmp);

  printf("Passed Test #5!\n");

  // Test 6: a random 90% of bits are set.
  for (i = 0; i < size; i++) {
    rtl_bmpSetBit(bmp, i, random() % 100 < 90);
  }

  rtl_bmpTabulate(bmp);
  bmpTestRankSelect(bmp, size);
  rtl_bmpClearAll(bmp);

  printf("Passed Test #6!\n");


  rtl_bmpRelease(bmp);
  
}

