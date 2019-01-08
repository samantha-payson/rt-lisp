CC     = gcc
CFLAGS = -Iinc -g -O3 -march=native

LD      = gcc
LDFLAGS =

OBJS = \
  rt-lisp.o \
  BitMap.o \
  main.o

HDRS = \
  inc/rt-lisp.h \
  inc/rtl/BitMap.h \
  inc/rtl/int28.h \
  inc/rtl/nil.h \
  inc/rtl/cons.h \
  inc/rtl/symbol.h \
  inc/rtl/fix14.h \
  inc/rtl/tuple.h


rtl: $(OBJS)
	@ echo "  LD    $@"
	@ $(LD) $(LDFLAGS) $^ -o $@

%.o: %.c $(HDRS)
	@ echo "  CC    $@"
	@ $(CC) $(CFLAGS) -c $< -o $@
