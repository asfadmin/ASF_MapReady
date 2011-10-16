# Makefile for asf_test

include ../../make_support/system_rules

CFLAGS += \
	$(GSL_CFLAGS) \
	$(PNG_CFLAGS) \
	$(GEOTIFF_CFLAGS) \
	$(TIFF_CFLAGS) \
	$(JPEG_CFLAGS) \
	$(PROJ_CFLAGS) \
	$(GLIB_CFLAGS) \
	$(SHAPELIB_CFLAGS)

LIBS =  \
	$(LIBDIR)/libcunit.a \
	$(LIBDIR)/libasf_convert.a \
	$(LIBDIR)/libasf_import.a \
	$(LIBDIR)/libasf_export.a \
	$(LIBDIR)/libasf_geocode.a \
	$(LIBDIR)/libasf_terrcorr.a \
	$(LIBDIR)/libasf_sar.a \
	$(LIBDIR)/libasf_ardop.a \
	$(LIBDIR)/libasf_vector.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/asf_fft.a \
	$(LIBDIR)/libasf_metadata.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf.a \
	$(GSL_LIBS) \
	$(PNG_LIBS) \
	$(GEOTIFF_LIBS) \
	$(TIFF_LIBS) \
	$(JPEG_LIBS) \
	$(XML_LIBS) \
	$(PROJ_LIBS) \
	$(GLIB_LIBS) \
	$(NETCDF_LIBS) \
	$(HDF5_LIBS) \
	$(SHAPELIB_LIBS) \
	-lm -lz

OBJS = 	\
	asf_test.o \
	config.o \
	uavsar_test.o \
	rsat1_test.o \
	alos_browse_test.o \
	alos_leader_test.o \
	alos_calibration_test.o

all: asf_test clean

asf_test: $(OBJS)
	$(CC) $(CFLAGS) $(LDFLAGS) -o asf_test $(OBJS) $(LIBS)
	rm *.o
	mv asf_test$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f *.o *~ core*
