CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
CFLAGS += $(HDF5_CFLAGS)
# Makefile for diffvector

include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/libasf_vector.a \
	$(LIBDIR)/libasf_import.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/asf_meta.a \
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
	$(GDAL_LIBS) \
	$(GLIB_LIBS) \
	$(ZLIB_LIBS) \
	$(XML_LIBS) \
	-lm -ldl

CFLAGS += \
	$(GSL_CLAGS) \
	$(GEOTIFF_CFLAGS) \
	$(TIFF_CFLAGS) \
	$(JPEG_CFLAGS) \
	$(PROJ_CFLAGS) \
	$(GLIB_CFLAGS) \
	$(SHAPELIB_CFLAGS)

OBJS  = diffvector.o

all: diffvector clean

diffvector: $(OBJS)
	$(CC) $(CFLAGS) -o diffvector $(OBJS) $(LIBS) $(LDFLAGS)
	mv diffvector$(BIN_POSTFIX) $(BINDIR)
clean:
	rm -f core* $(OBJS) *~
