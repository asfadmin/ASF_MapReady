CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
# Makefile for asf_calibrate

include ../../make_support/system_rules

CFLAGS += \
	$(GSL_CFLAGS) \
	$(PROJ_CFLAGS) \
	$(GLIB_CFLAGS) \
	$(GEOTIFF_CFLAGS) \
	$(HDF5_CFLAGS)

LIBS =  \
	$(LIBDIR)/libasf_sar.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf_fft.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(GSL_LIBS) \
	$(PROJ_LIBS) \
	$(XML_LIBS) \
	$(GLIB_LIBS) \
	$(ZLIB_LIBS) \
	-lm

OBJS = 	\
	asf_calibrate.o \

all: asf_calibrate clean

asf_calibrate: $(OBJS)
	$(CC) $(CFLAGS) $(LDFLAGS) -o asf_calibrate $(OBJS) $(LIBS)
	rm *.o
	mv asf_calibrate$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f *.o *~ core*
