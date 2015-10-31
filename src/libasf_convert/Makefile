CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(XML_CFLAGS)
# Makefile for libasf_convert library

include ../../make_support/system_rules

OBJS  = asf_convert.o \
	config.o \
	functions.o \
	kml_overlay.o

CFLAGS += -Wall $(W_ERROR) $(GLIB_CFLAGS) $(GSL_CFLAGS) $(PROJ_CFLAGS) $(JPEG_CFLAGS) $(SHAPELIB_CFLAGS)

all: build_only
	mv libasf_convert.a $(LIBDIR)
	cp asf_convert.h $(ASF_INCLUDE_DIR)
	mkdir -p $(SHAREDIR)/asf_mapready
	cp batch.config $(SHAREDIR)/asf_mapready
	cp asf_mapready.defaults $(SHAREDIR)/asf_mapready
	cp mapready_settings.cfg $(SHAREDIR)

build_only: $(OBJS)
	rm -f libasf_convert.a
	ar r libasf_convert.a $(OBJS)
	$(RANLIB) libasf_convert.a

$(OBJS): Makefile $(wildcard *.h) $(wildcard ../../include/*h)

clean:
	rm -f core $(OBJS) *.o *~
