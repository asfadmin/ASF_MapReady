# Makefile for          :  asf_check_geolocation
# Module Author         :  Rudi Gens
# Combined the functionality of some established InSAR procedure into one tool

include ../../make_support/system_rules

CFLAGS += \
	$(GSL_CFLAGS) \
	$(XML_CFLAGS) \
	$(PROJ_CFLAGS) \
	$(GLIB_CFLAGS) \
	$(TIFF_CFLAGS) \
	-Wall

OBJS  = asf_check_geolocation.o

LIBS = \
	$(LIBDIR)/libasf_terrcorr.a \
	$(LIBDIR)/libasf_vector.a \
	$(LIBDIR)/libasf_import.a \
	$(LIBDIR)/libasf_geocode.a \
	$(LIBDIR)/libasf_sar.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(SHAPELIB_LIBS) \
	$(XML_LIBS) \
	$(PROJ_LIBS) \
	$(GSL_LIBS) \
	$(GLIB_LIBS) \
	$(HDF5_LIBS) \
	$(GEOTIFF_LIBS) \
	$(TIFF_LIBS) \
	$(JPEG_LIBS) \
	$(XML_LIBS) \
	$(LIBDIR)/asf_fft.a \
	-lz

all: asf_check_geolocation
	-rm *.o

asf_check_geolocation: $(OBJS)
	$(CC) $(CFLAGS) -o asf_check_geolocation $(OBJS) $(LIBS) -lm
	mv asf_check_geolocation$(BIN_POSTFIX) $(BINDIR)

clean:
	-rm -f core *.o

