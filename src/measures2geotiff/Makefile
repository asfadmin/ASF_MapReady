CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
CFLAGS += $(HDF5_CFLAGS)
# Makefile for measures2geotiff
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
	$(GEOTIFF_LIBS) \
	$(TIFF_LIBS) \
	$(JPEG_LIBS) \
	$(PNG_LIBS) \
	$(GSL_LIBS) \
	$(XML_LIBS) \
	$(PROJ_LIBS) \
	$(GLIB_LIBS) \
	$(NETCDF_LIBS) \
	$(HDF5_LIBS) \
	$(HDFEOS5_LIBS) \
	$(ZLIB_LIBS) \
	-lm

CFLAGS += \
	$(GEOTIFF_CFLAGS) \
	$(TIFF_CFLAGS) \
	$(JPEG_CFLAGS) \
	$(PNG_CFLAGS) \
	$(GLIB_CFLAGS) \
	$(GSL_CFLAGS) \
	$(PROJ_CFLAGS) \
	$(XML_CFLAGS)

OBJS_M2X = measures2geotiff.o

all: measures2geotiff clean

measures2geotiff: $(OBJS_M2X)
	$(CC) $(CFLAGS) $(LFLAGS) -o measures2geotiff $(OBJS_M2X) $(LIBS)
	mv measures2geotiff$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f $(OBJS_M2X)

