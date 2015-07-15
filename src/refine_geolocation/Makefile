CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
CFLAGS += $(HDF5_CFLAGS)
include ../../make_support/system_rules

TARGET = refine_geolocation

CFLAGS += \
	$(GLIBS_CFLAGS) \
	$(TIFF_CFLAGS)

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

CFLAGS += $(GSL_CFLAGS) $(PROJ_CFLAGS) $(GLIB_CFLAGS)

OBJS  = $(TARGET).o

all: prog
	-rm *.o

prog: $(OBJS)
	$(CC) $(CFLAGS) -o $(TARGET) $(OBJS) $(LIBS) $(LDFLAGS)
	mv $(TARGET)$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f core $(OBJS) *.o *tctmp*
