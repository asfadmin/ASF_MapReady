CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
CFLAGS += $(HDF5_CFLAGS)
# Makefile for asf_kml_overlay

DEST_TARGET = asf_kml_overlay

include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/libasf_convert.a \
	$(LIBDIR)/libasf_import.a \
	$(LIBDIR)/libasf_export.a \
	$(LIBDIR)/libasf_terrcorr.a \
	$(LIBDIR)/libasf_geocode.a \
	$(LIBDIR)/libasf_ardop.a \
	$(LIBDIR)/libasf_sar.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/libasf_vector.a \
	$(SHAPELIB_LIBS) \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf_fft.a \
	$(GSL_LIBS) \
	$(PROJ_LIBS) \
	$(XML_LIBS) \
	$(GLIB_LIBS) \
	$(TIFF_LIBS) \
	$(JPEG_LIBS) \
	$(PNG_LIBS) \
	$(GEOTIFF_LIBS) \
	$(NETCDF_LIBS) \
	$(HDF5_LIBS) \
	$(ZLIB_LIBS)

CFLAGS += -Wall $(W_ERROR) $(SHAPELIB_CFLAGS) $(XML_CFLAGS)

OBJS  = asf_kml_overlay.o

all:    asf_kml_overlay clean

asf_kml_overlay: $(OBJS)
	$(CC) $(CFLAGS) -o asf_kml_overlay $(OBJS) $(LIBS) $(LDFLAGS)
	mv asf_kml_overlay$(BIN_POSTFIX) $(BINDIR)/$(DEST_TARGET)$(BIN_POSTFIX)

clean:
	rm -f core $(OBJS) *.o *~
