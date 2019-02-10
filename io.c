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

typedef struct rtl_io_File {
  uint32_t tag;
  FILE     *f;
} rtl_io_File;

#define MULTICHAR(x, y, z, w) \
  (((uint32_t)x << 24) |      \
   ((uint32_t)x << 16) |      \
   ((uint32_t)x <<  8) |      \
   ((uint32_t)x <<  0))	      \
  // End of multi-line macro

static
rtl_Word rtl_io_open(rtl_Machine    *M,
		     rtl_Word const *args,
		     size_t         argsLen)
{
  rtl_io_File rif;
  rif.tag = MULTICHAR('F', 'I', 'L', 'E');

  const rtl_Word dotRead  = rtl_internSelector(NULL, "read");
  const rtl_Word dotWrite = rtl_internSelector(NULL, "write");

  size_t pathLen;
  char   *path;

  assert(argsLen == 2);
  assert(rtl_isString(args[0]));
  assert(rtl_isSelector(args[1]));

  pathLen = rtl_stringLength(M, args[0]);
  path    = malloc(pathLen + 1);

  rtl_reifyString(M, args[0], path, pathLen + 1, NULL);

  if (args[1] == dotRead) {
    rif.f = fopen(path, "r");
    if (!rif.f) {
      fprintf(stderr, "  error: can't open \"%s\"\n", path);
      abort();
    }

    return rtl_native(M, &rif, sizeof(rtl_io_File));
  } else if (args[1] == dotWrite) {
    rif.f = fopen(path, "w");
    if (!rif.f) {
      fprintf(stderr, "  error: can't open \"%s\"\n", path);
      abort();
    }

    return rtl_native(M, &rif, sizeof(rtl_io_File));
  } else {
    fprintf(stderr, "\n   usage: (io:open <path> .read)\n"
	              "          (io:open <path> .write)\n\n");
    abort();
  }
}

static
rtl_Word rtl_io_readChar(rtl_Machine    *M,
			 rtl_Word const *args,
			 size_t         argsLen)
{
  rtl_io_File rif;
  int ch;

  assert(argsLen == 1);
  assert(rtl_isNative(args[0]));

  rtl_reifyNative(M, args[0], &rif, sizeof(rtl_io_File));
  assert(rif.tag == MULTICHAR('F', 'I', 'L', 'E'));

  ch = fgetc(rif.f);

  if (ch == EOF) {
    return rtl_internSelector("io", "EOF");
  }

  return rtl_int28(ch);
}

static
rtl_Word rtl_io_close(rtl_Machine    *M,
		      rtl_Word const *args,
		      size_t         argsLen)
{
  rtl_io_File rif;

  assert(argsLen == 1);
  assert(rtl_isNative(args[0]));

  rtl_reifyNative(M, args[0], &rif, sizeof(rtl_io_File));
  assert(rif.tag == MULTICHAR('F', 'I', 'L', 'E'));

  fclose(rif.f);

  return RTL_NIL;
}

void rtl_io_installBuiltins(rtl_Compiler *C)
{
  rtl_internPackage(C, "io");

  rtl_export(C, rtl_internSelector("io", "EOF"));

  rtl_registerBuiltin(C, rtl_intern("io", "open"), rtl_io_open);
  rtl_export(C, rtl_intern("io", "open"));

  rtl_registerBuiltin(C, rtl_intern("io", "read-char"), rtl_io_readChar);
  rtl_export(C, rtl_intern("io", "read-char"));

  rtl_registerBuiltin(C, rtl_intern("io", "close"), rtl_io_close);
  rtl_export(C, rtl_intern("io", "close"));
}
