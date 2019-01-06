#include <stdio.h>
#include <stdlib.h>

#include "rt-lisp.h"

int main() {
  srand(109234);
  rtl_testBitMap(1 << 18);

  return 0;
}
