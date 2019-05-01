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
#include <dirent.h>
#include <errno.h>

#ifdef RTL_LINUX
# include <libexplain/closedir.h>
# include <libexplain/readdir.h>
# include <libexplain/opendir.h>
# include <libexplain/fopen.h>
# include <libexplain/fclose.h>
# include <libexplain/fgetc.h>
# include <libexplain/fwrite.h>
#else
# define explain_closedir(_d)           strerror(errno)
# define explain_readdir(_d)            strerror(errno)
# define explain_opendir(_p)            strerror(errno)
# define explain_fopen(_p, _m)          strerror(errno)
# define explain_fclose(_f)             strerror(errno)
# define explain_fgetc(_f)              strerror(errno)
# define explain_fwrite(_p, _s, _n, _f) strerror(errno)
#endif

typedef struct rtl_io_File {
  uint32_t tag;
  FILE     *f;
} rtl_io_File;

#define MULTICHAR(x, y, z, w) \
  (((uint32_t)x << 24) |      \
   ((uint32_t)y << 16) |      \
   ((uint32_t)z <<  8) |      \
   ((uint32_t)w <<  0))       \
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

  if (argsLen != 2) {
    rtl_throwMsg(M, "arg-count",
                 "usage: (io:open <path> .read) or "
                 "(io:open <path> .write)");
    return RTL_NIL;
  }

  pathLen = rtl_xStringSize(M, args[0]);
  RTL_UNWIND (M) return RTL_NIL;
  path    = malloc(pathLen + 1);

  rtl_xReifyString(M, args[0], path, pathLen + 1);
  RTL_UNWIND (M) {
    free(path);
    return RTL_NIL;
  }

  // TODO: proper error handling here.
  if (args[1] == dotRead) {
    rif.f = fopen(path, "r");
    if (!rif.f) {
      rtl_throwMsg(M, "native-error",
                   explain_fopen(path, "r"));
      return RTL_NIL;
    }

    return rtl_native(M, &rif, sizeof(rtl_io_File));
  } else if (args[1] == dotWrite) {
    rif.f = fopen(path, "w");
    if (!rif.f) {
      rtl_throwMsg(M, "native-error",
                   explain_fopen(path, "w"));
      return RTL_NIL;
    }

    return rtl_native(M, &rif, sizeof(rtl_io_File));
  } else {
    rtl_throwMsg(M, "usage-error",
                 "usage: (io:open <path> .read) or "
                 "(io:open <path> .write)");
    return RTL_NIL;
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

  if (argsLen != 1) {
    rtl_throwMsg(M, "arg-count",
                 "io:read-char expects a single argument.");
    return RTL_NIL;
  }

  rtl_xReifyNative(M, args[0], &rif, sizeof(rtl_io_File));
  RTL_UNWIND (M) return RTL_NIL;

  if (rif.tag != MULTICHAR('F', 'I', 'L', 'E')) {
    rtl_throwMsg(M, "native-type",
                 "native argument to io:read-char is not a file.");
    return RTL_NIL;
  }

  ch = fgetc(rif.f);
  if (ferror(rif.f)) goto fgetc_error;
  if (ch == EOF) return rtl_internSelector("io", "EOF");

  utf8[0] = ch;

  byteCount = utf8ByteCount(ch);

  switch (byteCount) {
  case 0:
    return rtl_char(0xFFFD); // Replacement char

  case 4:
    ch = fgetc(rif.f);
    if (ferror(rif.f)) goto fgetc_error;
    if (ch == EOF) return rtl_internSelector("io", "EOF");

    utf8[3] = ch;
    
    // fallthrough ..

  case 3:
    ch = fgetc(rif.f);
    if (ferror(rif.f)) goto fgetc_error;
    if (ch == EOF) return rtl_internSelector("io", "EOF");

    utf8[2] = ch;

    // fallthrough ..

  case 2:
    ch = fgetc(rif.f);
    if (ferror(rif.f)) goto fgetc_error;
    if (ch == EOF) return rtl_internSelector("io", "EOF");

    utf8[1] = ch;

    utf8codepoint(utf8, &ch);

    // fallthrough ..
    
  case 1:
    return rtl_char(ch);

  default:
    abort(); // unreachable
  }

 fgetc_error:
  rtl_throwMsg(M, "native-error",
               explain_fgetc(rif.f));
  return RTL_NIL;
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

  if (argsLen != 2) {
    rtl_throwMsg(M, "arg-count",
                 "io:write-char expects two arguments.");
    return RTL_NIL;
  }

  rtl_xAssertChar(M, args[1]);
  RTL_UNWIND (M) return RTL_NIL;

  ch = rtl_charValue(args[1]);

  rtl_xReifyNative(M, args[0], &rif, sizeof(rtl_io_File));
  RTL_UNWIND (M) return RTL_NIL;

  if (rif.tag != MULTICHAR('F', 'I', 'L', 'E')) {
    rtl_throwMsg(M, "native-type",
                 "native argument to io:write-char is not a file.");
    return RTL_NIL;
  }

  utf8catcodepoint(utf8, ch, 4);

  count = fwrite(utf8, 1, utf8codepointsize(ch), rif.f);
  
  if (count < utf8codepointsize(ch)) {
    if (ferror(rif.f)) {
      rtl_throwMsg(M, "native-error",
                   explain_fwrite(utf8, 1, utf8codepointsize(ch), rif.f));
      return RTL_NIL;
    }

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

  if (argsLen != 2) {
    rtl_throwMsg(M, "arg-count",
                 "io:write-string expects 2 arguments.");
  }

  rtl_xReifyNative(M, args[0], &rif, sizeof(rtl_io_File));
  RTL_UNWIND (M) return RTL_NIL;

  if (rif.tag != MULTICHAR('F', 'I', 'L', 'E')) {
    rtl_throwMsg(M, "native-type",
                 "native argument to io:read-char is not a file.");
    return RTL_NIL;
  }

  size = rtl_xStringSize(M, args[1]);
  RTL_UNWIND (M) return RTL_NIL;

  utf8 = malloc(size + 1);

  rtl_xReifyString(M, args[1], utf8, size + 1);
  RTL_UNWIND (M) {
    free(utf8);
    return RTL_NIL;
  }

  count = fwrite(utf8, 1, size, rif.f);

  free(utf8);

  if (count < size) {
    if (ferror(rif.f)) {
      rtl_throwMsg(M, "native-error",
                   explain_fwrite(utf8, 1, size, rif.f));
      return RTL_NIL;
    }

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

  rtl_xReifyNative(M, args[0], &rif, sizeof(rtl_io_File));
  RTL_UNWIND (M) return RTL_NIL;

  assert(rif.tag == MULTICHAR('F', 'I', 'L', 'E'));

  if (fclose(rif.f)) {
    rtl_throwMsg(M, "native-error",
                 explain_fclose(rif.f));
  }

  return RTL_NIL;
}

typedef struct rtl_io_Dir {
  uint32_t tag;
  DIR      *d;
} rtl_io_Dir;

static
rtl_Word rtl_io_openDir(rtl_Machine    *M,
                        rtl_Word const *args,
                        size_t         argsLen)
{
  rtl_io_Dir rid;

  char   *path;
  size_t pathLen;

  if (argsLen != 1) {
    rtl_throwMsg(M, "arg-count",
                 "io:open-dir expects 1 argument, a path name.");
    return RTL_NIL;
  }

  if (!rtl_isString(M, args[0])) {
    rtl_throwMsg(M, "expected-string",
                 "Passed non-string as path to io:open-dir.");
    return RTL_NIL;
  }

  pathLen = rtl_xStringSize(M, args[0]);
  RTL_UNWIND (M) return RTL_NIL;

  path    = malloc(pathLen + 1);

  rtl_xReifyString(M, args[0], path, pathLen + 1);
  RTL_UNWIND (M) {
    free(path);
    return RTL_NIL;
  }

  rid.tag = MULTICHAR('D', 'I', 'R', 0);
  rid.d   = opendir(path);

  if (rid.d == NULL) {
    rtl_throwMsg(M, "native-error",
                 explain_opendir(path));
    free(path);

    return RTL_NIL;
  }

  free(path);

  return rtl_native(M, &rid, sizeof(rtl_io_Dir));
}

static
rtl_Word rtl_io_readDir(rtl_Machine    *M,
                        rtl_Word const *args,
                        size_t         argsLen)
{
  rtl_io_Dir    rid;
  struct dirent *ent;

  rtl_Word out = RTL_MAP;

  if (argsLen != 1) {
    rtl_throwMsg(M, "arg-count",
                 "io:read-dir expects 1 argument, a DIR object.");

    return RTL_NIL;
  }

  rtl_xReifyNative(M, args[0], &rid, sizeof(rtl_io_Dir));
  RTL_UNWIND (M) return RTL_NIL;

  if (rid.tag != MULTICHAR('D', 'I', 'R', 0)) {
    rtl_throwMsg(M, "expected-dir",
                 "io:close-dir expects its first argument to be a "
                 "DIR object.");
    return RTL_NIL;
  }

  errno = 0;

  ent = readdir(rid.d);
  if (ent == NULL) {
    if (errno != 0) {
      rtl_throwMsg(M, "native-error",
                   explain_readdir(rid.d));
    }

    return RTL_NIL;
  }

  RTL_PUSH_WORKING_SET(M, &out);

  out = rtl_xRecordSet(M, out, "name", rtl_string(M, ent->d_name));
  RTL_ASSERT_NO_UNWIND(M);

  switch (ent->d_type) {
  case DT_BLK:
    out = rtl_xRecordSet(M, out, "type",
                         rtl_internSelector("io", "block"));
    break;

  case DT_CHR:
    out = rtl_xRecordSet(M, out, "type",
                        rtl_internSelector("io", "char"));
    break;

  case DT_DIR:
    out = rtl_xRecordSet(M, out, "type",
                        rtl_internSelector("io", "directory"));
    break;

  case DT_FIFO:
    out = rtl_xRecordSet(M, out, "type",
                        rtl_internSelector("io", "fifo"));
    break;

  case DT_LNK:
    out = rtl_xRecordSet(M, out, "type",
                        rtl_internSelector("io", "sym-link"));
    break;

  case DT_REG:
    out = rtl_xRecordSet(M, out, "type",
                        rtl_internSelector("io", "regular"));
    break;

  case DT_SOCK:
    out = rtl_xRecordSet(M, out, "type",
                        rtl_internSelector("io", "socket"));
    break;

  case DT_UNKNOWN:
  default:
    out = rtl_xRecordSet(M, out, "type",
                        rtl_internSelector("io", "unknown"));
    break;
  }
  
  RTL_ASSERT_NO_UNWIND(M);

  rtl_popWorkingSet(M);

  return out;
}

static
rtl_Word rtl_io_closeDir(rtl_Machine    *M,
                         rtl_Word const *args,
                         size_t         argsLen)
{
  rtl_io_Dir    rid;

  if (argsLen != 1) {
    rtl_throwMsg(M, "arg-count",
                 "io:close-dir expects 1 argument, a DIR object.");

    return RTL_NIL;
  }

  if (!rtl_isNative(args[0])) {
    rtl_throwMsg(M, "expected-native",
                 "io:close-dir expects its first argument to be a "
                 "native object.");

    return RTL_NIL;
  }

  rtl_xReifyNative(M, args[0], &rid, sizeof(rtl_io_Dir));
  RTL_UNWIND (M) return RTL_NIL;

  if (rid.tag != MULTICHAR('D', 'I', 'R', 0)) {
    rtl_throwMsg(M, "expected-dir",
                 "io:close-dir expects its first argument to be a "
                 "DIR object.");
    return RTL_NIL;
  }

  if (closedir(rid.d)) {
    rtl_throwMsg(M, "native-error",
                 explain_closedir(rid.d));
    return RTL_NIL;
  }

  return rtl_internSelector(NULL, "ok");
}

void rtl_io_installBuiltins(rtl_Compiler *C)
{
  rtl_internPackage(C, "io");

  rtl_registerBuiltin(C, rtl_intern("io", "open"), rtl_io_open);
  rtl_xExport(C, rtl_intern("io", "open"));
  RTL_ASSERT_NO_UNWIND(C->M);

  rtl_registerBuiltin(C, rtl_intern("io", "read-char"), rtl_io_readChar);
  rtl_xExport(C, rtl_intern("io", "read-char"));
  RTL_ASSERT_NO_UNWIND(C->M);

  rtl_registerBuiltin(C, rtl_intern("io", "write-char"), rtl_io_writeChar);
  rtl_xExport(C, rtl_intern("io", "write-char"));
  RTL_ASSERT_NO_UNWIND(C->M);

  rtl_registerBuiltin(C, rtl_intern("io", "write-string"), rtl_io_writeString);
  rtl_xExport(C, rtl_intern("io", "write-string"));
  RTL_ASSERT_NO_UNWIND(C->M);

  rtl_registerBuiltin(C, rtl_intern("io", "close"), rtl_io_close);
  rtl_xExport(C, rtl_intern("io", "close"));
  RTL_ASSERT_NO_UNWIND(C->M);

  rtl_registerBuiltin(C, rtl_intern("io", "open-dir"), rtl_io_openDir);
  rtl_xExport(C, rtl_intern("io", "open-dir"));
  RTL_ASSERT_NO_UNWIND(C->M);

  rtl_registerBuiltin(C, rtl_intern("io", "read-dir"), rtl_io_readDir);
  rtl_xExport(C, rtl_intern("io", "read-dir"));
  RTL_ASSERT_NO_UNWIND(C->M);

  rtl_registerBuiltin(C, rtl_intern("io", "close-dir"), rtl_io_closeDir);
  rtl_xExport(C, rtl_intern("io", "close-dir"));
  RTL_ASSERT_NO_UNWIND(C->M);
}
