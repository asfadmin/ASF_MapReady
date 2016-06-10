# Makefile for asf2geobrowse

include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/libasf_export.a \
	$(LIBDIR)/libasf_geocode.a \
	$(LIBDIR)/libasf_vector.a \
	$(LIBDIR)/libasf_import.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/asf_meta.a \
	$(SHAPELIB_LIBS) \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(GSL_LIBS) \
	$(PROJ_LIBS) \
	$(NETCDF_LIBS) \
	$(HDF5_LIBS) \
	$(GEOTIFF_LIBS) \
	$(TIFF_LIBS) \
	$(JPEG_LIBS) \
	$(PNG_LIBS) \
	$(GLIB_LIBS) \
	$(ZLIB_LIBS) \
	$(XML_LIBS) \
	-lm

CFLAGS += \
	$(GSL_CLAGS) \
	$(GEOTIFF_CFLAGS) \
	$(TIFF_CFLAGS) \
	$(JPEG_CFLAGS) \
	$(PROJ_CFLAGS) \
	$(GLIB_CFLAGS) \
	$(SHAPELIB_CFLAGS) \
	$(HDF5_CFLAGS) \
	$(GEOTIFF_CFLAGS) \
	$(HDF5_CFLAGS) \
	$(XML_CFLAGS)

OBJS  = asf2geobrowse.o

all: asf2geobrowse clean

asf2geobrowse: $(OBJS)
	$(CC) $(CFLAGS) -o asf2geobrowse $(OBJS) $(LIBS) $(LDFLAGS)
	mv asf2geobrowse$(BIN_POSTFIX) $(BINDIR)
clean:
	rm -f core* $(OBJS) *~
