# Makefile for		: remap
# Module Author		: Orion Lawlor

include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/asf_meta.a \
        $(GSL_LIBS) \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(PROJ_LIBS) \
	-lm

OBJS = CLA.o \
	Matrix2D.o \
	mapping.o \
	fetcher.o \
	poly.o \
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
