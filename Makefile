CC     = gcc
CFLAGS = -Iinc -g -O0 -march=native

LD      = gcc
LDFLAGS =

OBJS = \
  rt-lisp.o \
  BitMap.o \
  main.o

rtl: $(OBJS)
	@ echo "  LD    $@"
	@ $(LD) $(LDFLAGS) $^ -o $@

%.o: %.c
	@ echo "  CC    $@"
	@ $(CC) $(CFLAGS) -c $< -o $@
