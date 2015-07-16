include ../../make_support/system_rules

CFLAGS += -Wall $(W_ERROR) $(GLIBS_CFLAGS)

LIBS  = \
	$(LIBDIR)/libasf_terrcorr.a \
	$(LIBDIR)/libasf_vector.a \
	$(LIBDIR)/libasf_import.a \
	$(LIBDIR)/libasf_export.a \
	$(LIBDIR)/libasf_geocode.a \
	$(LIBDIR)/libasf_ardop.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/libasf_sar.a \
	$(SHAPELIB_LIBS) \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf_fft.a \
	$(GSL_LIBS) \
	$(PROJ_LIBS) \
	$(XML_LIBS) \
	$(GLIB_LIBS) \
	$(GEOTIFF_LIBS) \
	$(HDF5_LIBS) \
	$(TIFF_LIBS) \
	$(JPEG_LIBS) \
	$(PNG_LIBS) \
	$(ZLIB_LIBS) \
	-lm

CFLAGS += \
	$(TIFF_CFLAGS) \
	$(GEOTIFF_CFLAGS) \
	$(HDF5_CFLAGS) \
	$(GSL_CFLAGS) \
	$(PROJ_CFLAGS) \
	$(GLIB_CFLAGS)

OBJS  = asf_terrcorr.o

all: tc
	-rm *.o

tc: $(OBJS)
	$(CC) $(CFLAGS) -o asf_terrcorr $(OBJS) $(LIBS) $(LDFLAGS)
	mv asf_terrcorr$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f core $(OBJS) *.o *tctmp*

