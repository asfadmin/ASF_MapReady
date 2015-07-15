CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
CFLAGS += $(HDF5_CFLAGS)
# Makefile for: brs2jpg
# Wrapper for converting ALOS Palsar browse image to JPEG format

include ../../make_support/system_rules

CFLAGS += \
	$(GEOTIFF_CFLAGS) \
	$(TIFF_CFLAGS) \
	$(JPEG_CFLAGS) \
	$(PNG_CFLAGS) \
	$(PROJ_CFLAGS) \
	$(GLIB_CFLAGS) \
	$(GSL_CFLAGS)

LIBS  = \
	$(LIBDIR)/libasf_export.a \
	$(LIBDIR)/libasf_vector.a \
	$(LIBDIR)/libasf_import.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/asf_meta.a \
	$(SHAPELIB_LIBS) \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(GEOTIFF_LIBS) \
	$(TIFF_LIBS) \
	$(JPEG_LIBS) \
	$(PNG_LIBS) \
	$(PROJ_LIBS) \
	$(XML_LIBS) \
	$(GLIB_LIBS) \
	$(NETCDF_LIBS) \
	$(HDF5_LIBS) \
	$(GSL_LIBS) \
	$(ZLIB_LIBS) \
	-lm

all: brs2jpg

brs2jpg: brs2jpg.c
	$(CC) $(CFLAGS) -o brs2jpg brs2jpg.c $(LIBS)
	mv  brs2jpg$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f *.o core
