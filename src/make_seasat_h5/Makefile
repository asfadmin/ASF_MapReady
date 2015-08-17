CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
CFLAGS += $(HDF5_CFLAGS)
# Makefile for make_seasat_h5
# Author Rudi Gens

include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/libasf_export.a \
	$(LIBDIR)/libasf_vector.a \
	$(LIBDIR)/libasf_import.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/asf_meta.a \
	$(SHAPELIB_LIBS) \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(GLIB_LIBS) \
	$(GSL_LIBS) \
	$(GEOTIFF_LIBS) \
	$(TIFF_LIBS) \
	$(JPEG_LIBS) \
	$(XML_LIBS) \
	$(PROJ_LIBS) \
	$(HDF5_LIBS) \
	$(GLIB_LIBS) \
	$(ZLIB_LIBS) \
	-lm

CFLAGS += \
	$(PROJ_CFLAGS) \
	$(GLIB_CFLAGS) \
	$(GSL_CFLAGS) \
	$(GEOTIFF_CFLAGS) \
	$(TIFF_CFLAGS) \
	$(JPEG_CFLAGS) \
	$(XML_CFLAGS) \
	$(SHAPELIB_CFLAGS)

OBJS_M2X = make_seasat_h5.o

all: make_seasat_h5 clean
	cp iso_meta_ext.xml $(SHAREDIR)

make_seasat_h5: $(OBJS_M2X)
	$(CC) $(CFLAGS) $(LFLAGS) -o make_seasat_h5 $(OBJS_M2X) $(LIBS)
	mv make_seasat_h5$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f $(OBJS_M2X)

