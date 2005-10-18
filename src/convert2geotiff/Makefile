# Makefile for:  convert2geotiff
# Module Author:  Sherman Watts (used tae code as referenced), 09/2001
#                 Patrick Denny, 02/2004

include ../../make_support/system_rules


LIBS =	$(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/libproj.a \
	$(LIBDIR)/libasf_reporting.a \
	$(GSL_LIBS) \
	$(GEOTIFF_LIBS) \
	$(TIFF_LIBS) \
	-lm

OBJS =	get_tiepoints.o \
	c_decdeg.o \
	c_getdatum.o \
	upcase.o \
	geo_tiffp.o \
        get_utmzone.o \
	get_utmzonegen.o \
	get_linear_units.o \
	get_angular_units.o \
	get_stateplanezone.o \
        set_major_minor.o \
        get_sphere.o \
	geokeyset.o \
	utmkeyset.o \
	spcskeyset.o \
	alberskeyset.o \
	lamcckeyset.o \
	mercatkeyset.o \
	pskeyset.o \
	polyckeyset.o \
	equidckeyset.o \
	tmkeyset.o \
	stereokeyset.o \
	lamazkeyset.o \
	azmeqdkeyset.o \
	gnomonkeyset.o \
	orthokeyset.o \
	gvnspkeyset.o \
	snsoidkeyset.o \
	eqrectkeyset.o \
	millerkeyset.o \
	vgrintkeyset.o \
	homkeyset.o \
	robinkeyset.o \
	somkeyset.o \
	alaskakeyset.o \
	goodkeyset.o \
	mollkeyset.o \
	imollkeyset.o \
	hammerkeyset.o \
	wagivkeyset.o \
	wagviikeyset.o \
	obeqakeyset.o \
	get_geocode.o \
	convert2geotiff_IO.o \
	convert2geotiff.o 

CFLAGS += -D_GNU_SOURCE

all: convert2geotiff clean

convert2geotiff: $(OBJS)
	$(CC) $(CFLAGS) -o convert2geotiff $(OBJS) $(LIBS)
	cp convert2geotiff.1 ../../man/man1
	mv convert2geotiff$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f core *.o
