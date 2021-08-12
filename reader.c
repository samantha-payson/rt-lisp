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

#ifdef RTL_LIBEXPLAIN
# include <libexplain/fgetc.h>
#else
# include <errno.h>

# define explain_fgetc(...) strerror(errno)
#endif

#include <ctype.h>
#include <stdlib.h>
#include <string.h>

static
void eatWhitespace(rtl_Compiler *C, FILE *f)
{
  int ch;

  while (isspace(ch = fgetc(f)) || ch == ',' || ch == ';') {
    if (ch == ';') {
      while (fgetc(f) != '\n' && !feof(f)) {
        if (ferror(f)) {
          rtl_throwMsg(C->M, "native-error", explain_fgetc(f));
          return;
        }
      }
    }
  }

  if (ferror(f)) {
    rtl_throwMsg(C->M, "native-error", explain_fgetc(f));
    return;
  }

  ungetc(ch, f);
}

rtl_Word rtl_xReadAtom(rtl_Compiler *C, FILE *f, int first)
{
  char buf[512];
  char *ptr;
  int ch, next;

  static const char nonAtomChars[] = ")}],;'";

  int   i28;
  float f14;

  buf[0] = first;
  next   = 1;

  ch = fgetc(f);
  if (ferror(f)) {
    rtl_throwMsg(C->M, "native-error", explain_fgetc(f));
    return RTL_NIL;
  }

  while (isgraph(ch) && !strchr(nonAtomChars, ch)) {
    if (next == 511) {
      // This atom is too long, complain and abort ...
      buf[32] = '\0';
      printf("\n  crtl: error: reader: atom must be less than 512 "
                                      "characters... (compiler bug)\n\n"
             "     begins with %s...\n\n",
             buf);
      abort();
    }

    buf[next++] = ch;

    ch = fgetc(f);
    if (ferror(f)) {
      rtl_throwMsg(C->M, "native-error", explain_fgetc(f));
      return RTL_NIL;
    }
  }

  // Make sure the string is null terminated
  buf[next] = '\0';


  // Last char we read wasn't part of the atom.
  ungetc(ch, f);

  i28 = strtol(buf, &ptr, 0);
  if (ptr == buf + next)
    return rtl_int28(i28);

  f14 = strtof(buf, &ptr);
  if (ptr == buf + next)
    return rtl_fix14(f14);

  if (!strcmp(buf, "nil")) {
    return RTL_NIL;
  }

  if (!strcmp(buf, "T")) {
    return RTL_TOP;
  }

  ptr = strchr(buf, ':');
  if (ptr == NULL) {
    if (buf[0] == '.') {
      return rtl_internSelector(NULL, buf + 1);

    } else {
      return rtl_unresolvedSymbol(rtl_internUnresolvedID(NULL, buf));

    }

  } else {
    *ptr = '\0';
    if (buf[0] == '.') {
      return rtl_internSelector(buf + 1, ptr + 1);

    } else {
      return rtl_unresolvedSymbol(rtl_internUnresolvedID(buf, ptr + 1));

    }
  }
}

int rtl_xReadDelim(rtl_Compiler *C, FILE *f, int delim)
{
  int ch, n;
  rtl_Word w;

  eatWhitespace(C, f);
  ch = fgetc(f);
  if (ferror(f)) {
    rtl_throwMsg(C->M, "native-error", explain_fgetc(f));
    return 0;
  }

  for (n = 0; ch != delim && ch != EOF; n++) {
    // Put ch back on the stream so that rtl_read can work with it.
    ungetc(ch, f);
    w = rtl_xRead(C, f);
    RTL_UNWIND (C->M) return 0;
    
    rtl_push(C->M, w);

    eatWhitespace(C, f);
    ch = fgetc(f);
    if (ferror(f)) {
      rtl_throwMsg(C->M, "native-error", explain_fgetc(f));
      return 0;
    }
  }

  if (ch == EOF) {
    rtl_throwMsg(C->M, "unexpected-eof",
                 "Premature EOF while reading a delimited expression.");
  }

  return n;
}

static
rtl_Word xReadList(rtl_Compiler *C, FILE *f)
{
  int ch;
  rtl_Word w = RTL_NIL, car = RTL_NIL, cdr = RTL_NIL;

  RTL_PUSH_WORKING_SET(C->M, &w, &car, &cdr);

  eatWhitespace(C, f);
  ch = fgetc(f);
  if (ferror(f)) {
    rtl_throwMsg(C->M, "native-error", explain_fgetc(f));
    return RTL_NIL;
  }

  switch (ch) {
  case ')':
    w = RTL_NIL;
    break;

  case '.':
    // TODO: make this code selector-friendly
    ch = fgetc(f);
    if (ferror(f)) {
      rtl_throwMsg(C->M, "native-error", explain_fgetc(f));
      return RTL_NIL;
    }
  
    if (isspace(ch)) {
      w = rtl_xRead(C, f);
      RTL_UNWIND (C->M) goto cleanup;

      eatWhitespace(C, f);
      ch = fgetc(f);
      if (ch != ')') {
        rtl_throwMsg(C->M, "bad-delim",
                     "Dotted list does not end with ')'");
        goto cleanup;
      }

    } else {
      ungetc(ch, f);
      car = rtl_xReadAtom(C, f, '.');
      RTL_UNWIND (C->M) goto cleanup;

      cdr = xReadList(C, f);
      RTL_UNWIND (C->M) goto cleanup;

      w = rtl_cons(C->M, car, cdr);

    } break;

  default:
    if (ch == EOF) {
      rtl_throwMsg(C->M, "unexpected-eof",
                   "Premature EOF while reading a list expression.");
      goto cleanup;
    }


    ungetc(ch, f);
    car = rtl_xRead(C, f);
    RTL_UNWIND (C->M) goto cleanup;

    cdr = xReadList(C, f);
    RTL_UNWIND (C->M) goto cleanup;

    w = rtl_cons(C->M, car, cdr);
    break;
  }

 cleanup:
  rtl_popWorkingSet(C->M);

  return w;
}

static
rtl_Word xReadChar(rtl_Compiler *C, FILE *f)
{
  int ch;
  rtl_Word w;
  char errBuf[1024];

  ch = fgetc(f);
  if (ferror(f)) {
    rtl_throwMsg(C->M, "native-error", explain_fgetc(f));
    return RTL_NIL;
  }

  if (ch == EOF) {
    rtl_throwMsg(C->M, "unexpected-eof",
                 "Premature EOF while reading character constant.");
    return RTL_NIL;
  }

  if (ch == '\\') {
    switch ((ch = fgetc(f))) {
    case EOF:
      if (ferror(f)) {
        rtl_throwMsg(C->M, "native-error", explain_fgetc(f));
        return RTL_NIL;
      }

      rtl_throwMsg(C->M, "unexpected-eof",
                   "Premature EOF while reading character constant.");

      return RTL_NIL;

    case '\\':
      w = rtl_char('\\');
      break;

    case '0':
      w = rtl_char('\0');
      break;

    case 'n':
      w = rtl_char('\n');
      break;

    case 'r':
      w = rtl_char('\r');
      break;

    case 't':
      w = rtl_char('\t');
      break;

    case '\'':
      w = rtl_char('\'');
      break;

    case '\"':
      w = rtl_char('\"');
      break;

    default:
      snprintf(errBuf, 1024,
               "Bad escape '\\%c' (%02X) in char constant.\n",
               ch, (unsigned)ch);
      rtl_throwMsg(C->M, "malformed", errBuf);
      return RTL_NIL;
    }
  } else {
    w = rtl_char(ch);
  }

  if ((ch = fgetc(f)) != '\'') {
    if (ferror(f)) {
      rtl_throwMsg(C->M, "native-error", explain_fgetc(f));
      return RTL_NIL;
    } else if (ch == EOF) {
      rtl_throwMsg(C->M, "unexpected-eof",
                   "Premature EOF while reading character constant.");
      return RTL_NIL;
    } else {
      snprintf(errBuf, 1024,
               "Character constant %02X followed by %c (expected '\\'')\n",
               (unsigned)rtl_int28Value(w), ch);
      rtl_throwMsg(C->M, "malformed", errBuf);
      return RTL_NIL;
    }
  }

  return w;
}

static
rtl_Word xReadString(rtl_Compiler *C, FILE *f)
{
  int ch;

  char buf[2048];
  int  idx;

  char errBuf[1024];

  for (ch = fgetc(f), idx = 0;
       ch != '"' && ch != EOF && idx < 2048;
       ch = fgetc(f), idx++)
  {
    if (ferror(f)) {
      rtl_throwMsg(C->M, "native-error", explain_fgetc(f));
      return RTL_NIL;
    }

    if (ch == '\\') {
      switch ((ch = fgetc(f))) {
      case '\\':
        ch = '\\';
        break;

      case '0':
        ch = '\0';
        break;

      case 'n':
        ch = '\n';
        break;

      case 'r':
        ch = '\r';
        break;

      case 't':
        ch = '\t';
        break;

      case '"':
        ch = '"';
        break;

      default:
        if (ferror(f)) {
          rtl_throwMsg(C->M, "native-error", explain_fgetc(f));
          return RTL_NIL;
        }

        snprintf(errBuf, 1024,
                 "Bad escape '\\%c' (%02X) in string constant.\n",
                 ch, (unsigned)ch);
        rtl_throwMsg(C->M, "malformed", errBuf);
        return RTL_NIL;
      }
    }

    buf[idx] = ch;
  }

  if (idx == 2048) {
    buf[32] = '\0';
    printf("\n  crtl: error: reader: String longer than 2048 characters\n\n"
           "      beginning with \"%s... (this is a compiler limitation)\n\n",
           buf);
    abort();
  }

  buf[idx] = '\0';

  return rtl_string(C->M, buf);
}

rtl_Word rtl_xRead(rtl_Compiler *C, FILE *f)
{
  int      ch,
           n,
           i;

  rtl_Word w = RTL_NIL,
           k = RTL_NIL,
           v = RTL_NIL,
           *ptr;

  eatWhitespace(C, f);

  ch = fgetc(f);  
  if (ferror(f)) {
    rtl_throwMsg(C->M, "native-error", explain_fgetc(f));
    return RTL_NIL;
  }

  RTL_PUSH_WORKING_SET(C->M, &w, &k, &v);

  switch (ch) {
  case EOF:
    w = rtl_cons(C->M, rtl_internSelector("io", "EOF"), RTL_NIL);
    w = rtl_cons(C->M, rtl_intern("intrinsic", "quote"), w);
    break;

  case ')':
  case '}':
  case ']': {
    char errBuf[1024];
    snprintf(errBuf, 1024,
             "Expression may not begin with '%c'\n", ch);
    rtl_throwMsg(C->M, "malformed", errBuf);
  } break;

  case '(':
    w = xReadList(C, f);
    break;

  case '{':

    n = rtl_xReadDelim(C, f, '}');
    RTL_UNWIND (C->M) break;

    if ((n & 1) != 0) {
      rtl_throwMsg(C->M, "malformed",
                   "Odd number of expressions in a map literal!");
      break;
    }

    w = rtl_emptyMap();

    for (i = 0; i*2 < n; i++) {
      k = C->M->vStack[C->M->vStackLen - n + i*2];
      v = C->M->vStack[C->M->vStackLen - n + i*2 + 1];
      w = rtl_xMapInsert(C->M, w, k, v);
      RTL_ASSERT_NO_UNWIND(C->M);
    }

    C->M->vStackLen -= n;

    break;

  case '[':
    n   = rtl_xReadDelim(C, f, ']');
    RTL_UNWIND (C->M) break;

    ptr = rtl_allocTuple(C->M, &w, n);
    memcpy(ptr, C->M->vStack + C->M->vStackLen - n, sizeof(rtl_Word)*n);
    C->M->vStackLen -= n;
    break;

  case '\'':
    w = xReadChar(C, f);
    break;

  case '"':
    w = xReadString(C, f);
    break;

  case '`':
    w = rtl_xRead(C, f);
    RTL_UNWIND (C->M) break;

    w = rtl_cons(C->M, w, RTL_NIL);
    w = rtl_cons(C->M, rtl_intern("std", "semiquote"), w);
    break;

  case '~':
    w = rtl_xRead(C, f);
    RTL_UNWIND (C->M) break;

    w = rtl_cons(C->M, w, RTL_NIL);
    w = rtl_cons(C->M, rtl_intern("std", "escape"), w);
    break;

  case '@':
    w = rtl_xRead(C, f);
    RTL_UNWIND (C->M) break;

    w = rtl_cons(C->M, w, RTL_NIL);
    w = rtl_cons(C->M, rtl_intern("std", "splice"), w);
    break;

  default:
    if (isgraph(ch)) {
      w = rtl_xReadAtom(C, f, ch);
    } else {
      char errBuf[1024];

      snprintf(errBuf, 1024, 
               "Got bad character %#X '%c'.\n",
               (unsigned int)ch, (char)ch);

      rtl_throwMsg(C->M, "malformed", errBuf);
    }
  }

  rtl_popWorkingSet(C->M);

  return w;
}
