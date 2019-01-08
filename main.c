#include <stdio.h>
#include <stdlib.h>

#include "rt-lisp.h"

void testScanK(uint32_t x, uint32_t k) {
  printf("bit %d of %#x: %d\n", k, x, (int)rtl_scanForKth(x, k));
}

int main() {
  rtl_testGarbageCollector(1 << 20);

  return 0;
}
