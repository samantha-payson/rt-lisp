CC     = gcc
CFLAGS = -Iinc -g -O0 -march=native

LD      = gcc
LDFLAGS =

OBJS = \
  rt-lisp.o \
  BitMap.o \
  debug.o \
  symbol.o \
  compiler.o \
  reader.o \
  main.o

HDRS = \
  inc/rt-lisp.h \
  inc/rtl/BitMap.h \
  inc/rtl/int28.h \
  inc/rtl/nil.h \
  inc/rtl/cons.h \
  inc/rtl/symbol.h \
  inc/rtl/fix14.h \
  inc/rtl/tuple.h \
  inc/rtl/rto.h \
  inc/rtl/instructions.h \
  inc/rtl/compiler.h \
  inc/rtl/debug.h


crtl: $(OBJS)
	@ echo "  LD    $@"
	@ $(LD) $(LDFLAGS) $^ -o $@

%.o: %.c $(HDRS)
	@ echo "  CC    $@"
	@ $(CC) $(CFLAGS) -c $< -o $@
