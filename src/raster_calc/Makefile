# Makefile for program raster_calc
# by Orion Lawlor, ASF, 1997
# by Patrick Denny, 2/2004

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libgsl.a \
	-lm
	
OBJS  = tokenizer.o \
	expression.o \
	raster_calc.o

all: raster_calc clean

raster_calc: $(OBJS)
	$(CC) $(CFLAGS) -o raster_calc $(OBJS) $(LIBS) $(LDFLAGS)
	mv raster_calc$(BIN_POSTFIX) $(BINDIR)
	cp raster_calc.1 ../../man/cat1/

clean:
	rm -f core *.o
