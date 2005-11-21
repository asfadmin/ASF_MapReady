# Makefile for          :  asf_check_geolocation
# Module Author         :  Rudi Gens
# Combined the functionality of some established InSAR procedure into one tool

include ../../make_support/system_rules

OBJS  = asf_check_geolocation.o \
	create_dem_grid.o \
	fit_plane.o \
	fit_poly.o \
	reskew_dem.o

LIBS = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/libasf_proj.a \
	$(PROJ_LIBS) \
	$(GSL_LIBS) \
	$(LIBDIR)/asf_fft.a \
	$(LIBDIR)/libasf_reporting.a

CFLAGS += $(GSL_CFLAGS) $(PROJ_CFLAGS) $(GLIB_CFLAGS)

all: asf_check_geolocation
	-rm *.o

asf_check_geolocation: $(OBJS) $(LIBS)
	${CC} ${CFLAGS} -o asf_check_geolocation $(OBJS) $(LIBS) -lm
	mv asf_check_geolocation$(BIN_POSTFIX) $(BINDIR)

clean:
	-rm -f core *.o





