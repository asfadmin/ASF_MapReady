# Makefile for asf_export
# Author Britton Kerin, June 2004
#    mod Patrick Denny, June 2004

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_reporting.a \
	$(LIBDIR)/libgeotiff.a \
	$(LIBDIR)/libtiff.a \
	$(LIBDIR)/libjpeg.a \
	$(LIBDIR)/libgsl.a \
	-lm

OBJS  = asf_export.o \
	ceos_util.o \
	export_as_ceos.o \
	export_as_envi.o \
	export_as_esri.o \
	export_as_geotiff.o \
	export_as_jpeg.o \
	export_as_ppm.o \
	util.o

all: asf_export

asf_export: $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o asf_export $(OBJS) $(LIBS) $(LDFLAGS)
	mv asf_export$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f *.o core.* asf_export$(BIN_POSTFIX)
