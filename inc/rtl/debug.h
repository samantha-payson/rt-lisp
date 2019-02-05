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
# error "rtl/debug.h should only be included indirectly via rt-lisp.h"
#endif

// Format an expression, following pointers and expanding nested data.
void rtl_formatExpr(rtl_Machine *M, rtl_Word w);

// Format an expression, indented as if it's nested n levels deep.
void rtl_formatExprIndented(rtl_Machine *M, rtl_Word w, int n);

// Format an expression, but don't follow any pointers. Basically just print
// atoms, and addresses for non-atoms.
void rtl_formatExprShallow(rtl_Word w);

// Useful for debugging the map implementation. formatExpr or formatExprIndented
// should be used outside of map code.
void __rtl_debugFormatMap(rtl_Machine *M, rtl_Word map, int indent, uint32_t mask);

// Print the next instruction in bytecode, and return a pointer to the beginning
// of the next instruction.
uint8_t *rtl_disasm(uint8_t *bytecode);

// Return the number of words in generation genNbr. This will always return a
// power of 2.
static inline
size_t __rtl_genCapacity(int genNbr)
{
  return 1 << ((27 + genNbr) - RTL_MAX_GENERATIONS);
}

// Return the generation number of a pointer type.
static inline
int __rtl_ptrGen(rtl_Word ptr) {
  return RTL_MAX_GENERATIONS - __builtin_clz(ptr);
}

// Return the offset pointed at by ptr within its generation.
static inline
uint32_t __rtl_ptrOffs(rtl_Word ptr) {
  return ~__rtl_genCapacity(__rtl_ptrGen(ptr)) & (ptr >> 4);
}

rtl_Word *__rtl_reifyPtr(rtl_Machine *M, rtl_Word ptr);

void rtl_disasmPage(rtl_Machine *M, uint16_t pageID);
