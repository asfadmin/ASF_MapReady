# Makefile for libasf_vector library

include ../../make_support/system_rules

CFLAGS := $(JPEG_CFLAGS) $(PROJ_CFLAGS) $(GLIB_CFLAGS) $(CFLAGS)

OBJS = \
	kml.o \
	convert_kml.o \
	shape.o \
	convert_shape.o

LIBS = \
	$(LIBDIR)/libasf_vector.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libshp.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(JPEG_LIBS) \
	$(PNG_LIBS) \
	$(PROJ_LIBS) \
	$(GSL_LIBS) \
	$(GLIB_LIBS) \
	$(LDFAGS) \
	-lm

all: build_only
	mv libasf_vector.a $(LIBDIR)
	cp asf_vector.h $(ASF_INCLUDE_DIR)

build_only: $(OBJS)
	rm -f libasf_vector.a
	ar r libasf_vector.a $(OBJS)
	$(RANLIB) libasf_vector.a


# Rebuild everything if makefile or any local of ASF header or library
# has changed.
$(OBJS): Makefile $(wildcard *.h) $(wildcard ../../include/*.h)

clean:
	rm -rf $(OBJS) libasf_vector.a core* *~
