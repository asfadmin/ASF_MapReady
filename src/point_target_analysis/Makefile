CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
CFLAGS += $(HDF5_CFLAGS)
# Makefile for point_target_analysis
# Module Author: Rudi Gens

include ../../make_support/system_rules

CFLAGS += \
	$(GSL_CFLAGS) \
	$(GEOTIFF_CFLAGS) \
	$(TIFF_CFLAGS) \
	$(JPEG_CFLAGS) \
	$(PROJ_CFLAGS) \
	$(GLIB_CFLAGS)

LIBS  = \
	$(LIBDIR)/libasf_sar.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/asf_fft.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(GSL_LIBS) \
	$(PROJ_LIBS) \
	$(GEOTIFF_LIBS) \
	$(HDF5_LIBS) \
	$(TIFF_LIBS) \
	$(JPEG_LIBS) \
	$(GLIB_LIBS) \
	$(FFT_LIBS) \
	$(XML_LIBS) \
	-lm

OBJS  = point_target_analysis.o

all: pta clean

pta: $(OBJS)
	$(CC) $(CFLAGS) $(LDFLAGS) -o point_target_analysis $(OBJS) $(LIBS)
	mv point_target_analysis$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f core *.o
