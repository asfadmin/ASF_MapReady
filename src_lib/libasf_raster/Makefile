# Makefile for libasf_raster library

include ../../make_support/system_rules

OBJS = interpolate.o \
       kernel.o \
       float_image.o \
       scaling.o \
       stats.o

CFLAGS += $(shell pkg-config --cflags-only-I glib-2.0)

LIBS = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libgsl.a \
	-lm

all: build_only
	cp libasf_raster.a $(LIBDIR)
	cp asf_raster.h float_image.h $(ASF_INCLUDE_DIR)

build_only: $(OBJS)
	rm -f libasf_raster.a
	ar r libasf_raster.a $(OBJS)
	$(RANLIB) libasf_raster.a

# Test program useful for verifying that the float_image class works
# as expected.
brighten_float_image: brighten_float_image.o
	$(CC) $^ \
              $(LIBDIR)/libasf_meta.a \
               $(LIBDIR)/libasf_raster.a \
              $(LIBDIR)/libgsl.a \
              `pkg-config --libs glib-2.0` \
              $(LIBDIR)/libgslcblas.a \
              -lm \
              -o $@

# Test program useful for checking the speed of float_image pixel
# lookups.
brighten_in_memory: brighten_in_memory.o
	$(CC) $^ \
              $(LIBDIR)/libasf_meta.a \
              $(LIBDIR)/libasf_raster.a \
              $(LIBDIR)/libgsl.a \
              `pkg-config --libs glib-2.0` \
              $(LIBDIR)/libgslcblas.a \
              -lm \
              -o $@

# Rebuild everything if makefile or any local of ASF header or library
# has changed.
$(OBJS): Makefile $(wildcard *.h) $(wildcard ../../include/*.h) \
	 $(LIBS)

clean:
	rm -rf $(OBJS) rm brighten_float_image.o brighten_float_image \
               brighten_in_memory.o brighten_in_memory libasf_raster.a

test: interpolate.t.c all
	$(CC) $(CFLAGS) interpolate.t.c $(LIBDIR)/libasf_raster.a $(LIBS) -o interpolate.t
