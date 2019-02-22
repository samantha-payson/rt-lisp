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

  pathLen = rtl_stringSize(M, args[0]);
  path    = malloc(pathLen + 1);

  rtl_reifyString(M, args[0], path, pathLen + 1);

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
uint8_t utf8ByteCount(uint8_t firstByte) {
  static const uint8_t utf8_lengths[] = {
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 2, 2, 3, 3, 4, 0
  };

  return utf8_lengths[firstByte >> 3];
}

static
rtl_Word rtl_io_readChar(rtl_Machine    *M,
			 rtl_Word const *args,
			 size_t         argsLen)
{
  rtl_io_File rif;
  utf8_int32_t ch;
  uint8_t byteCount, utf8[4];

  assert(argsLen == 1);
  assert(rtl_isNative(args[0]));

  rtl_reifyNative(M, args[0], &rif, sizeof(rtl_io_File));
  assert(rif.tag == MULTICHAR('F', 'I', 'L', 'E'));

  ch = fgetc(rif.f);
  if (ch == EOF) return rtl_internSelector("io", "EOF");

  utf8[0] = ch;

  byteCount = utf8ByteCount(ch);

  switch (byteCount) {
  case 0:
    return rtl_char(0xFFFD); // Replacement char

  case 4:
    ch = fgetc(rif.f);
    if (ch == EOF) return rtl_internSelector("io", "EOF");

    utf8[3] = ch;
    
    // fallthrough ..

  case 3:
    ch = fgetc(rif.f);
    if (ch == EOF) return rtl_internSelector("io", "EOF");

    utf8[2] = ch;

    // fallthrough ..

  case 2:
    ch = fgetc(rif.f);
    if (ch == EOF) return rtl_internSelector("io", "EOF");

    utf8[1] = ch;

    utf8codepoint(utf8, &ch);

    // fallthrough ..
    
  case 1:
    return rtl_char(ch);

  default:
    abort(); // unreachable
  }
}

static
rtl_Word rtl_io_writeChar(rtl_Machine    *M,
			  rtl_Word const *args,
			  size_t         argsLen)
{
  rtl_io_File  rif;
  uint8_t      utf8[4];
  int32_t      count;
  utf8_int32_t ch;

  assert(argsLen == 2);
  assert(rtl_isNative(args[0]) && rtl_isChar(args[1]));

  ch = rtl_charValue(args[1]);

  rtl_reifyNative(M, args[0], &rif, sizeof(rtl_io_File));
  assert(rif.tag == MULTICHAR('F', 'I', 'L', 'E'));

  utf8catcodepoint(utf8, ch, 4);

  count = fwrite(utf8, 1, utf8codepointsize(ch), rif.f);
  
  if (count < utf8codepointsize(ch)) {
    return rtl_internSelector("io", "EOF");
  }

  return rtl_int28(count);
}

static
rtl_Word rtl_io_writeString(rtl_Machine    *M,
			    rtl_Word const *args,
			    size_t         argsLen)
{
  rtl_io_File  rif;
  char         *utf8;
  uint32_t     size;

  int32_t      count;

  assert(argsLen == 2);
  assert(rtl_isNative(args[0]) && rtl_isString(args[1]));

  rtl_reifyNative(M, args[0], &rif, sizeof(rtl_io_File));
  assert(rif.tag == MULTICHAR('F', 'I', 'L', 'E'));

  size = rtl_stringSize(M, args[1]);
  utf8 = malloc(size + 1);

  rtl_reifyString(M, args[1], utf8, size + 1);

  count = fwrite(utf8, 1, size, rif.f);

  free(utf8);

  if (count < size) {
    return rtl_internSelector("io", "EOF");
  }

  return rtl_int28(count);
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

  rtl_registerBuiltin(C, rtl_intern("io", "write-char"), rtl_io_writeChar);
  rtl_export(C, rtl_intern("io", "write-char"));

  rtl_registerBuiltin(C, rtl_intern("io", "write-string"), rtl_io_writeString);
  rtl_export(C, rtl_intern("io", "write-string"));

  rtl_registerBuiltin(C, rtl_intern("io", "close"), rtl_io_close);
  rtl_export(C, rtl_intern("io", "close"));
}
