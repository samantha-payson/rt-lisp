CC     = gcc
CFLAGS = -Iinc -g -O0 -march=native -D RTL_TRACE_WORKING_SETS

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
  inc/rtl/intrinsic.h \
  inc/rtl/compiler.h \
  inc/rtl/int28.h \
  inc/rtl/map.h \
  inc/rtl/top.h \
  inc/rtl/selector.h \
  inc/rtl/nil.h \
  inc/rtl/cons.h \
  inc/rtl/rto.h \
  inc/rtl/symbol.h \
  inc/rtl/instructions.h \
  inc/rtl/fix14.h \
  inc/rtl/tuple.h \
  inc/rtl/debug.h \
  inc/rtl/addr.h

crtl: $(OBJS)
	@ echo "  LD    $@"
	@ $(LD) $(LDFLAGS) $^ -o $@

%.o: %.c $(HDRS)
	@ echo "  CC    $@"
	@ $(CC) $(CFLAGS) -c $< -o $@


clean:
	@ rm -f $(OBJS) crtl
