# Makefile for		: remap
# Module Author		: Orion Lawlor

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libgsl.a \
	-lm

OBJS = CLA.o \
       Matrix2D.o \
       mapping.o \
       fetcher.o \
       quadratic.o \
       mapSamp.o \
       warpMap.o \
       writer.o 

all: remap clean

remap: $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o remap remap.c $(OBJS) $(LIBS) $(LDFLAGS)
	mv remap$(BIN_POSTFIX) $(BINDIR)
	cp remap.1 ../../man/cat1

clean:
	rm -f *.o
