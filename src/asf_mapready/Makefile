CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
CFLAGS += $(HDF5_CFLAGS)
# Makefile for asf_mapready - now just a wrapper to call library
# function in libasf_convert

DEST_TARGET = asf_mapready

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
	$(ZLIB_LIBS) \
	-lm

CFLAGS += -Wall $(W_ERROR)

OBJS  = asf_mapready.o

all:    asf_mapready clean

asf_mapready: $(OBJS)
	$(CC) $(CFLAGS) -o asf_mapready $(OBJS) $(LIBS) $(LDFLAGS)
	mv asf_mapready$(BIN_POSTFIX) $(BINDIR)/$(DEST_TARGET)$(BIN_POSTFIX)

clean:
	rm -f core $(OBJS) *.o *~
