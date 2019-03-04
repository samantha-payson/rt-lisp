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

#include <ctype.h>
#include <stdlib.h>
#include <string.h>

static
void eatWhitespace(FILE *f)
{
  int ch;

  while (isspace(ch = fgetc(f)) || ch == ',' || ch == ';')
    if (ch == ';')
      while (fgetc(f) != '\n' && !feof(f))
	;;

  ungetc(ch, f);
}

rtl_Word rtl_readAtom(rtl_Compiler *C, FILE *f, int first)
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

  while (isgraph(ch) && !strchr(nonAtomChars, ch)) {
    if (next == 511) {
      // This atom is too long, complain and abort ...
      buf[next] = '\0';
      printf("An atom must be less than 512 characters...\n     got \"%s\"\n",
	     buf);
      abort();
    }

    buf[next++] = ch;

    ch = fgetc(f);
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
      return rtl_unresolvedSelector(rtl_internUnresolvedID(NULL, buf + 1));

    } else {
      return rtl_unresolvedSymbol(rtl_internUnresolvedID(NULL, buf));

    }

  } else {
    *ptr = '\0';
    if (buf[0] == '.') {
      return rtl_unresolvedSelector(rtl_internUnresolvedID(buf + 1, ptr + 1));

    } else {
      return rtl_unresolvedSymbol(rtl_internUnresolvedID(buf, ptr + 1));

    }
  }
}

int rtl_readDelim(rtl_Compiler *C, FILE *f, int delim)
{
  int ch, n;

  eatWhitespace(f);
  ch = fgetc(f);
  for (n = 0; ch != delim && ch != EOF; n++) {
    // Put ch back on the stream so that rtl_read can work with it.
    ungetc(ch, f);
    
    rtl_push(C->M, rtl_read(C, f));

    eatWhitespace(f);
    ch = fgetc(f);
  }

  if (ch == EOF) {
    printf("\n   !!! EOF WHILE READING DELIMITED EXPRESSION  { .delim '%c' }\n\n",
	   (char)delim);
    abort();
  }

  return n;
}

static
rtl_Word readList(rtl_Compiler *C, FILE *f)
{
  int ch;
  rtl_Word w = RTL_NIL, car = RTL_NIL, cdr = RTL_NIL;

  RTL_PUSH_WORKING_SET(C->M, &w, &car, &cdr);

  eatWhitespace(f);
  ch = fgetc(f);

  switch (ch) {
  case ')':
    w = RTL_NIL;
    break;

  case '.':
    // TODO: make this code selector-friendly
    ch = fgetc(f);
    if (isspace(ch)) {
      w = rtl_read(C, f);
      eatWhitespace(f);
      ch = fgetc(f);
      assert(ch == ')');

    } else {
      ungetc(ch, f);
      car = rtl_readAtom(C, f, '.');
      cdr = readList(C, f);
      w = rtl_cons(C->M, car, cdr);

    } break;

  default:
    if (ch == EOF) {
      printf("\n   !!! EOF WHILE READING LIST EXPRESSION  { .delim ')' }\n\n");
      abort();
    }

    ungetc(ch, f);
    car = rtl_read(C, f);
    cdr = readList(C, f);
    w = rtl_cons(C->M, car, cdr);
    break;
  }

  rtl_popWorkingSet(C->M);

  return w;
}

static
rtl_Word readChar(FILE *f)
{
  int ch;
  rtl_Word w;

  ch = fgetc(f);
  if (ch == EOF) {
    printf("  error: EOF during char constant.\n");
    abort();
  }

  if (ch == '\\') {
    switch ((ch = fgetc(f))) {
    case EOF:
      printf("  error: EOF during char constant.\n");
      abort();

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

    default:
      printf(" error: bad escape '%c' (%02X) in char constant.\n", ch, (unsigned)ch);
      abort();
    }
  } else {
    w = rtl_char(ch);
  }

  if ((ch = fgetc(f)) != '\'') {
    if (ch == EOF) {
      printf("  error: EOF during char constant.\n");
    } else {
      printf("Malformed character constant %02X followed by %c (expected '\\'')\n",
	     (unsigned)rtl_int28Value(w), ch);
    }

    abort();
  }

  return w;
}

static
rtl_Word readString(rtl_Compiler *C, FILE *f)
{
  int ch;

  char buf[2048];
  int  idx;

  for (ch = fgetc(f), idx = 0;
       ch != '"' && ch != EOF && idx < 2048;
       ch = fgetc(f), idx++)
  {
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

      default:
	printf(" !!! BAD ESCAPE '%c' (%02X) IN STRING !!!", ch, (unsigned)ch);
	break;
      }
    }

    buf[idx] = ch;
  }

  if (idx == 2048) {
    buf[32] = '\0';
    printf(" READ ERROR: String longer than 2048 characters,"
	   " beginning with \"%s...\n", buf);
    abort();
  }

  buf[idx] = '\0';

  return rtl_string(C->M, buf);
}

rtl_Word rtl_read(rtl_Compiler *C, FILE *f)
{
  int      ch,
           n,
           i;

  rtl_Word w = RTL_NIL,
           k = RTL_NIL,
           v = RTL_NIL,
           *ptr;

  RTL_PUSH_WORKING_SET(C->M, &w);

  eatWhitespace(f);

  ch = fgetc(f);
  switch (ch) {
  case EOF:
    w = rtl_cons(C->M, rtl_internSelector("io", "EOF"), RTL_NIL);
    w = rtl_cons(C->M, rtl_intern("intrinsic", "quote"), w);
    break;

  case ')':
  case '}':
  case ']':
    printf("\n   !!! RTL EXPRESSION MAY NOT BEGIN WITH '%c' !!!\n\n", (char)ch);
    abort();

  case '(':
    w = readList(C, f);
    break;

  case '{': {
    RTL_PUSH_WORKING_SET(C->M, &w, &k, &v);

    n = rtl_readDelim(C, f, '}');

    assert((n & 1) == 0); // Must be even numbered!

    w = rtl_emptyMap();

    for (i = 0; i*2 < n; i++) {
      k = C->M->vStack[C->M->vStackLen - n + i*2];
      v = C->M->vStack[C->M->vStackLen - n + i*2 + 1];
      w = rtl_mapInsert(C->M, w, k, v);
    }

    C->M->vStackLen -= n;

    rtl_popWorkingSet(C->M);



  } break;

  case '[':
    n   = rtl_readDelim(C, f, ']');
    ptr = rtl_allocTuple(C->M, &w, n);
    memcpy(ptr, C->M->vStack + C->M->vStackLen - n, sizeof(rtl_Word)*n);
    C->M->vStackLen -= n;
    break;

  case '\'':
    w = readChar(f);
    break;

  case '"':
    w = readString(C, f);
    break;

  case '`':
    w = rtl_cons(C->M, rtl_read(C, f), RTL_NIL);
    w = rtl_cons(C->M, rtl_intern("std", "semiquote"), w);
    break;

  case '~':
    w = rtl_cons(C->M, rtl_read(C, f), RTL_NIL);
    w = rtl_cons(C->M, rtl_intern("std", "escape"), w);
    break;

  case '@':
    w = rtl_cons(C->M, rtl_read(C, f), RTL_NIL);
    w = rtl_cons(C->M, rtl_intern("std", "splice"), w);
    break;

  default:
    if (isgraph(ch)) {
      w = rtl_readAtom(C, f, ch);
    } else {
      printf("Got bad character %X '%c'.\n", (unsigned int)ch, (char)ch);
      abort();
    }
  }

  rtl_popWorkingSet(C->M);

  return w;
}
