CC     = gcc
CFLAGS = -Wall -Werror -Iinc -g -O3 -march=native \
  -Wno-error=unused-function \
  -Wno-error=unused-variable \
  -D NDEBUG \
#  -D RTL_TRACE_WORKING_SETS \
#  -D RTL_BITMAP_SANITY_CHECKS \

LD      = gcc
LDFLAGS =

OBJS = \
  rt-lisp.o \
  BitMap.o \
  debug.o \
  symbol.o \
  compiler.o \
  io.o \
  reader.o \
  repl.o \
  main.o


HDRS = \
  inc/rt-lisp.h \
  inc/rtl/BitMap.h \
  inc/rtl/intrinsic.h \
  inc/rtl/compiler.h \
  inc/rtl/function.h \
  inc/rtl/int28.h \
  inc/rtl/map.h \
  inc/rtl/top.h \
  inc/rtl/selector.h \
  inc/rtl/nil.h \
  inc/rtl/string.h \
  inc/rtl/cons.h \
  inc/rtl/rto.h \
  inc/rtl/native.h \
  inc/rtl/symbol.h \
  inc/rtl/instructions.h \
  inc/rtl/fix14.h \
  inc/rtl/tuple.h \
  inc/rtl/debug.h


crtl: $(OBJS)
	@ echo "  LD    $@"
	@ $(LD) $(LDFLAGS) $^ -o $@

%.o: %.c $(HDRS)
	@ echo "  CC    $@"
	@ $(CC) $(CFLAGS) -c $< -o $@


clean:
	@ rm -f $(OBJS) crtl
