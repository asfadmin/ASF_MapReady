CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
include ../../make_support/system_rules

CFLAGS += $(GLIBS_CFLAGS)

LIBS  = \
	$(LIBDIR)/libasf_geocode.a \
	$(LIBDIR)/libasf_terrcorr.a \
	$(LIBDIR)/libasf_sar.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/libasf_import.a \
	$(SHAPELIB_LIBS) \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf_fft.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(LIBDIR)/libasf_vector.a \
	$(PROJ_LIBS) \
	$(TIFF_LIBS) \
	$(GEOTIFF_LIBS) \
	$(XML_LIBS) \
	$(GSL_LIBS) \
	$(GLIB_LIBS) \
	$(JPEG_LIBS) \
	-lm

CFLAGS += $(GSL_CFLAGS) $(PROJ_CFLAGS) $(GLIB_CFLAGS)

OBJS  = dem_trim.o

dem_trim: $(OBJS)
	$(CC) $(CFLAGS) -o dem_trim $(OBJS) $(LIBS) $(LDFLAGS)
	mv dem_trim$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f core $(OBJS) *.o

