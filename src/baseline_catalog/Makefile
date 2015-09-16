CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
CFLAGS += $(HDF5_CFLAGS)
# Makefile for libasf_baseline

include ../../make_support/system_rules

CFLAGS := \
	$(GEOTIFF_CFLAGS) \
	$(TIFF_CFLAGS) \
	$(JPEG_CFLAGS) \
	$(XML_CFLAGS) \
	$(PROJ_CFLAGS) \
	$(GLIB_CFLAGS) \
	$(CFLAGS)

LIBS =  \
	$(LIBDIR)/libasf_vector.a \
	$(LIBDIR)/libasf_import.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/asf_meta.a \
	$(SHAPELIB_LIBS) \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(GEOTIFF_LIBS) \
	$(HDF5_LIBS) \
	$(TIFF_LIBS) \
	$(JPEG_LIBS) \
	$(XML_LIBS) \
	$(PROJ_LIBS) \
	$(GSL_LIBS) \
	$(GLIB_LIBS) \
	$(XML_LIBS) \
	$(LDFLAGS) \
	-lz -lm

OBJLIB = 	\
	baseline2vector.o \
	parse_funcs.o \
	read_srf.o \
	setup.o \
	determine_baseline.o \
	generate_products.o \
	baseline_catalog.o

all: lib exe clean

lib: $(OBJLIB)
	rm -f libasf_baseline.a
	ar r libasf_baseline.a $(OBJLIB)
	$(RANLIB) libasf_baseline.a
	mv libasf_baseline.a $(LIBDIR)
	cp asf_baseline.h $(ASF_INCLUDE_DIR)

exe: baseline_catalog filter_srfs

baseline_catalog: baseline_catalog_main.c lib
	$(CC) $(CFLAGS) -o baseline_catalog baseline_catalog_main.c $(LIBDIR)/libasf_baseline.a $(LIBS)
	rm *.o
	mv baseline_catalog$(BIN_POSTFIX) $(BINDIR)

filter_srfs: filter_srfs.c
	$(CC) $(CFLAGS) -o filter_srfs filter_srfs.c $(LIBS)
	mv filter_srfs$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f *.o *~ core*
