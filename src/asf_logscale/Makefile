CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
CFLAGS += $(HDF5_CFLAGS)
# Makefile for asf_logscale

include ../../make_support/system_rules

CFLAGS += \
	$(GSL_CFLAGS) \
	$(PROJ_CFLAGS) \
	$(GLIB_CFLAGS) \

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
	asf_logscale.o \

all: asf_logscale clean

asf_logscale: $(OBJS)
	$(CC) $(CFLAGS) $(LDFLAGS) -o asf_logscale $(OBJS) $(LIBS)
	rm *.o
	mv asf_logscale$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f *.o *~ core*
