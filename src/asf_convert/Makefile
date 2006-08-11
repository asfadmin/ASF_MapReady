# Makefile for asf_convert - now just a wrapper to call library
# function in libasf_convert

include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/libasf_convert.a \
	$(LIBDIR)/libasf_import.a \
	$(LIBDIR)/libasf_export.a \
	$(LIBDIR)/libasf_geocode.a \
	$(LIBDIR)/libasf_terrcorr.a \
	$(LIBDIR)/libasf_ardop.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/libasf_sar.a \
	$(LIBDIR)/libasf_vector.a \
        $(LIBDIR)/libshp.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf_odl.a \
	$(LIBDIR)/asf.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf_fft.a \
	$(GSL_LIBS) \
	$(PROJ_LIBS) \
	$(JPEG_LIBS) \
	$(GLIB_LIBS) \
	$(TIFF_LIBS) \
	$(GEOTIFF_LIBS)

OBJS  = asf_convert.o

all:    asf_convert clean

asf_convert: $(OBJS)
	$(CC) $(CFLAGS) -o asf_convert $(OBJS) $(LIBS) $(LDFLAGS)
	mv asf_convert$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f core $(OBJS) *.o *~
