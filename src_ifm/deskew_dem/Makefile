# Makefile for deskew_dem
# by Orion Lawlor 1997; Patrick Denny 2004

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libgsl.a \
	$(LIBDIR)/asf.a \
	-lm

OBJS  = calc_ranges.o \
	dem_interp_col.o \
	dem_sr2gr.o \
	deskew_dem.o \
	geo_compensate.o \
	radio_compensate.o

all: deskew_dem clean

deskew_dem: $(OBJS)
	$(CC) $(CFLAGS) -o deskew_dem $(OBJS) $(LIBS) $(LDFLAGS)
	mv deskew_dem$(BIN_POSTFIX) $(BINDIR)
	cp deskew_dem.1 ../../man/cat1

clean:
	rm -f core *.o
