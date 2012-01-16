# Makefile for libasf_sar library

include ../../make_support/system_rules

CFLAGS := -Wall $(CFLAGS) $(GSL_CFLAGS) $(GLIB_CFLAGS)

OBJS = \
	gr2sr.o \
	sr2gr.o \
	reskew_dem.o \
	deskew_dem.o \
	deskew.o \
	create_dem_grid.o \
	poly.o \
	fit_poly.o \
	fetcher.o \
	remap_poly.o \
	mfd.o \
	to_sr.o \
	refine_offset.o \
	interp_dem_holes.o \
	find_band.o \
	classify.o \
	polarimetry.o \
	farcorr.o \
	calibrate.o \
	c2p.o

LIBS = \
	$(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_sar.a \
	$(LIBDIR)/libasf_vector.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/libshp.a \
	$(LDFLAGS) \
	$(GSL_LIBS) \
	-lm

all: build_only
	cp libasf_sar.a $(LIBDIR)
	cp asf_sar.h poly.h $(ASF_INCLUDE_DIR)
	mkdir -p $(SHAREDIR)/classifications
	cp cloude8.cla cloude16.cla $(SHAREDIR)/classifications
	mkdir -p $(SHAREDIR)/look_up_tables
	cp cloude8.lut cloude16.lut $(SHAREDIR)/look_up_tables
	cp jasc_palettes/* $(SHAREDIR)/look_up_tables
	-cp ea_boundary.txt $(SHAREDIR)/classifications

build_only: $(OBJS)
	rm -f libasf_sar.a
	ar r libasf_sar.a $(OBJS)
	$(RANLIB) libasf_sar.a

$(OBJS): Makefile $(wildcard *.h) $(wildcard ../../include/*.h)

clean:
	rm -rf $(OBJS) libasf_sar.a *~
