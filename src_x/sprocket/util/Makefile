# Makefile for ASF Converter
include ../Make_rules

OBJS = util.o

all: build_only clean

build_only: $(OBJS)
	$(AR) rv libUtil.a $(OBJS)
	$(RANLIB) libUtil.a
	mv libUtil.a $(LIBDIR)

clean:
	rm -f *.o
