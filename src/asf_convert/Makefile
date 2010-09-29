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
	$(LIBDIR)/libshp.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf_fft.a \
	$(GSL_LIBS) \
	$(PROJ_LIBS) \
	$(GLIB_LIBS) \
	$(TIFF_LIBS) \
	$(JPEG_LIBS) \
	$(PNG_LIBS) \
	$(GEOTIFF_LIBS) \
	$(XML_LIBS) \
	$(NETCDF_LIBS) \
	$(HDF5_LIBS) \
	$(HDFEOS5_LIBS) \
	$(ZLIB_LIBS)

OBJS  = asf_convert.o

all:    asf_convert clean

asf_convert: $(OBJS)
	$(CC) $(CFLAGS) -o asf_convert $(OBJS) $(LIBS) $(LDFLAGS)
	mv asf_convert$(BIN_POSTFIX) $(BINDIR)/$(DEST_TARGET)$(BIN_POSTFIX)

clean:
	rm -f core $(OBJS) *.o *~
