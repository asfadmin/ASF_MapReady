# Makefile for program image_stats
# by Rudi Gens, ASF, 2004
# Based on image_layers

DEBUG_BUILD = 2

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libgsl.a \
	$(LIBDIR)/asf.a \
	-lm

OBJS  = least_squares.o \
	matrix.o \
	meta_util.o \
	plot.o \
	utilities.o

all: image_stats clean

image_stats: $(OBJS) image_stats.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o image_stats image_stats.c $(OBJS) $(LIBS)
	cp image_stats$(BIN_POSTFIX) $(BINDIR)
#	cp image_layers.1 ../../man/cat1/

clean:
	rm -f core *.o
