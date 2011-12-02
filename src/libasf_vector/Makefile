# Makefile for libasf_vector library

include ../../make_support/system_rules

CFLAGS += \
	$(TIFF_CFLAGS) \
	$(GEOTIFF_CFLAGS) \
	$(JPEG_CFLAGS) \
	$(XML_CFLAGS) \
	$(PROJ_CFLAGS) \
	$(GLIB_CFLAGS) \
	-D_REENTRANT \
	$(GSL_CFLAGS) \
	$(CFLAGS)

OBJS = \
	convert2vector.o \
	convert2vector.t.o \
	utils.o \
	header.o \
	geotiff.o \
	polygon.o \
	point.o \
	auig.o \
	rgps.o \
	multimatch.o \
	meta.o \
	terrasar.o \
	kml.o \
	shape.o \
	generic_csv.o \
	ursa.o \
	datapool.o \
	high_altitude_photography.o \
	smap.o \
	custom.o \
	utils.o \
	config.o

LIBS = \
	$(LIBDIR)/libasf_vector.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libshp.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(LIBDIR)/libasf_import.a \
	$(TIFF_LIBS) \
	$(GEOTIFF_LIBS) \
	$(HDF5_LIBS) \
	$(JPEG_LIBS) \
	$(XML_LIBS) \
	$(PNG_LIBS) \
	$(PROJ_LIBS) \
	$(GSL_LIBS) \
	$(GLIB_LIBS) \
	$(LDFAGS) \
	-lm -lz

all: build_only
	mv libasf_vector.a $(LIBDIR)
	cp asf_vector.h $(ASF_INCLUDE_DIR)
	cp ursa.h $(ASF_INCLUDE_DIR)
	mkdir -p $(SHAREDIR)
	cp header.lst $(SHAREDIR)
	cp convert2vector.config $(SHAREDIR)

build_only: $(OBJS)
	rm -f libasf_vector.a
	ar r libasf_vector.a $(OBJS)
	$(RANLIB) libasf_vector.a


# Rebuild everything if makefile or any local of ASF header or library
# has changed.
$(OBJS): Makefile $(wildcard *.h) $(wildcard ../../include/*.h)

clean:
	rm -rf $(OBJS) libasf_vector.a core* *~
