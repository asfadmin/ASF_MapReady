# Makefile for libasf_raster library

include ../../make_support/system_rules

CFLAGS := $(GLIB_CFLAGS) $(CFLAGS)

OBJS = interpolate.o \
       kernel.o \
       float_image.o \
       uint8_image.o \
       scaling.o \
       stats.o \
       trim.o \
       fftMatch.o

LIBS = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(GSL_LIBS) \
	$(GLIB_LIBS) \
	$(LDFAGS) \
	-lm

all: build_only
	cp libasf_raster.a $(LIBDIR)
	cp asf_raster.h float_image.h uint8_image.h $(ASF_INCLUDE_DIR)

build_only: $(OBJS)
	rm -f libasf_raster.a
	ar r libasf_raster.a $(OBJS)
	$(RANLIB) libasf_raster.a


# This class is the source file corresponding to this target has lots
# of tricky stuff, so I get some extra help from compiler warnings.
float_image.o: CFLAGS := $(CFLAGS) -Werror -Wall -W -Wmissing-prototypes \
                         -Wstrict-prototypes -Wshadow -Wpointer-arith \
                         -Wcast-qual -Wcast-align -Wwrite-strings \
                         -Wnested-externs -fno-common

# Test program useful for verifying that the float_image class works
# as expected.
brighten_float_image: brighten_float_image.o
	$(CC) -Wall -g3 $^ \
              $(LIBDIR)/libasf_meta.a \
               $(LIBDIR)/libasf_raster.a \
              $(LIBDIR)/libgsl.a \
              `pkg-config --libs glib-2.0` \
              $(LIBDIR)/libgslcblas.a \
	      $(LIBDIR)/libjpeg.a \
              -lm \
              -o $@

# Test program useful for checking the speed of float_image pixel
# lookups.
brighten_in_memory: brighten_in_memory.o
	$(CC) -Wall -g3 $^ \
              $(LIBDIR)/libasf_meta.a \
              $(LIBDIR)/libasf_raster.a \
              $(LIBDIR)/libgsl.a \
              `pkg-config --libs glib-2.0` \
              $(LIBDIR)/libgslcblas.a \
	      $(LIBDIR)/libjpeg.a \
              -lm \
              -o $@

# FIXME: remove the stupid PKG_CONFIG_PATH environment var setting
# once it is sorted out how to have pkg-config know where to find the
# .pc file that the glib module should be installing.
test_float_image_statistics: test_float_image_statistics.o
	$(CC) -Wall -g $^ \
              $(LIBDIR)/libasf_meta.a \
              $(LIBDIR)/libasf_raster.a \
              $(LIBDIR)/libgsl.a \
              `PKG_CONFIG_PATH=/usr/lib/pkgconfig pkg-config --libs glib-2.0` \
              $(LIBDIR)/libgslcblas.a \
	      $(LIBDIR)/libjpeg.a \
              -lm \
              -o $@
	./$@

# Rebuild everything if makefile or any local of ASF header or library
# has changed.
$(OBJS): Makefile $(wildcard *.h) $(wildcard ../../include/*.h)
#	 $(LIBS)

clean:
	rm -rf $(OBJS) rm brighten_float_image.o brighten_float_image \
               brighten_in_memory.o brighten_in_memory libasf_raster.a \
               test_float_image_statistics

test: interpolate.t.c all
	$(CC) $(CFLAGS) interpolate.t.c $(LIBDIR)/libasf_raster.a $(LIBS) -o interpolate.t
