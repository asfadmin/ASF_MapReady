# Makefile for		: water_mask
# Module Author		: Dave Koster

include ../../make_support/system_rules

CFLAGS = $(FLAGS) -DDECODE_CEOS

LIBS  = $(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf_las.a \
	$(LIBDIR)/asf_geolib.a \
	$(LIBDIR)/asf.a \
	-lm

OBJS  = water_mask.o \
	swath_bound.o

all: water_mask clean

water_mask: $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o water_mask $(OBJS) $(LIBS)
	mv water_mask $(BINDIR)
	cp water_mask.1 ../../man/cat1

clean:
	rm -f core *.o

