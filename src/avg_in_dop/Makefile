# Makefile for avg_in_dop
# Author: Patrick Denny
# Version 1.1

include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/libasf_insar.a \
	$(LIBDIR)/libasf_ardop.a \
	$(LIBDIR)/libasf_sar.a \
	$(LIBDIR)/libasf_export.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/asf_fft.a \
	$(LIBDIR)/asf_meta.a \
	$(GSL_LIBS) \
	$(LIBDIR)/asf.a \
	$(LIBDIR)/libasf_proj.a \
	$(PROJ_LIBS) \
	$(FFT_LIBS) \
	$(PNG_LIBS) \
	$(JPEG_LIBS) \
	$(TIFF_LIBS) \
	$(GEOTIFF_LIBS) \
	$(NETCDF_LIBS) \
	$(HDF5_LIBS) \
	$(GLIB_LIBS) \
	$(XML_LIBS) \
	-lz

OBJS  = avg_in_dop.o

all: avg_in_dop clean

avg_in_dop: $(OBJS)
	$(CC) $(CFLAGS) -o avg_in_dop $(OBJS) $(LIBS) $(LDFLAGS)
	mv avg_in_dop$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f core $(OBJS)
