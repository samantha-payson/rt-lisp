#include "rt-lisp.h"

#include <ctype.h>
#include <stdlib.h>
#include <string.h>

// This function has the same effect as the RTL_OP_CONS instruction.
static
void consStackTop(rtl_Machine *M) {
  rtl_Word w;

  assert(M->vStackLen >= 2);


  w = rtl_cons(M, rtl_peek(M, 1), rtl_peek(M, 0));

  rtl_popK(M, 2);
  rtl_push(M, w);
}

static
void eatWhitespace(FILE *f)
{
  int ch;

  while (isspace(ch = fgetc(f)))
    ;;

  ungetc(ch, f);
}

rtl_Word rtl_readAtom(rtl_Compiler *C, FILE *f, int first)
{
  char buf[512];
  char *ptr;
  int ch, next;

  int   i28;
  float f14;

  buf[0] = first;
  next   = 1;

  ch = fgetc(f);

  while (isgraph(ch) && ch != ')' && ch != '}' && ch != ']') {
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
    return rtl_unresolvedSymbol(rtl_internUnresolvedID(NULL, buf));
  } else {
    *ptr = '\0';
    return rtl_unresolvedSymbol(rtl_internUnresolvedID(buf, ptr + 1));
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

rtl_Word rtl_read(rtl_Compiler *C, FILE *f)
{
  int ch, n, i;

  eatWhitespace(f);

  ch = fgetc(f);
  switch (ch) {
  case ')':
  case '}':
  case ']':
    printf("\n   !!! RTL EXPRESSION MAY NOT BEGIN WITH '%c' !!!\n\n", (char)ch);
    abort();

  case '(':
    n = rtl_readDelim(C, f, ')');
    rtl_push(C->M, RTL_NIL);

    for (i = 0; i < n; i++) {
      consStackTop(C->M);
    }

    return rtl_pop(C->M);

  case '{':
    printf("\n   !!! RTL CAN'T READ RECORDS YET !!!\n\n");
    abort();

  case '[':
    printf("\n   !!! RTL CAN'T READ TUPLES YET !!!\n\n");
    abort();

  case '"':
    printf("\n   !!! RTL CAN'T READ STRINGS YET !!!\n\n");
    abort();

  default:
    if (isgraph(ch)) {
      rtl_readAtom(C, f, ch);
    }
  }
}
