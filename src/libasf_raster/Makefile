# Makefile for libasf_raster library

include ../../make_support/system_rules

CFLAGS := $(JPEG_CFLAGS) $(GLIB_CFLAGS) $(CFLAGS)

OBJS = \
	interpolate.o \
	kernel.o \
	float_image.o \
	banded_float_image.o \
	uint8_image.o \
	scaling.o \
	bands.o \
	stats.o \
	trim.o \
	fftMatch.o \
	shaded_relief.o \
	resample.o \
	tile.o \
	look_up_table.o

LIBS :=	\
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/asf.a \
	$(GSL_LIBS) \
	$(JPEG_LIBS) \
	$(PROJ_LIBS) \
	$(GLIB_LIBS) \
	-lm

all: build_only
	cp libasf_raster.a $(LIBDIR)
	cp asf_raster.h float_image.h banded_float_image.h uint8_image.h $(ASF_INCLUDE_DIR)
	rm -rf $(SHAREDIR)/look_up_tables
	cp -R look_up_tables/ $(SHAREDIR)

build_only: $(OBJS)
	rm -f libasf_raster.a
	ar r libasf_raster.a $(OBJS)
	$(RANLIB) libasf_raster.a

# Test program useful for verifying that the float_image class works
# as expected.
brighten_float_image: brighten_float_image.o
	$(CC) -Wall -g3 $^ $(LIBS) -o $@
	./$@

# Test program useful for checking the speed of float_image pixel
# lookups.
brighten_in_memory: brighten_in_memory.o
	$(CC) -Wall -g3 $^ $(LIBS) -o $@
	./$@

# Test program useful for testing banded_float_image
test_bfi: test_bfi.o
	$(CC) -Wall -g3 $^ $(LIBS) -o $@
	./$@
	rm ./$@

# FIXME: remove the stupid PKG_CONFIG_PATH environment var setting
# once it is sorted out how to have pkg-config know where to find the
# .pc file that the glib module should be installing.
test_float_image_statistics: test_float_image_statistics.o
	$(CC) -Wall -g $^ $(LIBS) -o $@
	./$@

# Rebuild everything if makefile or any local of ASF header or library
# has changed.
$(OBJS): Makefile $(wildcard *.h) $(wildcard ../../include/*.h)

clean:
	rm -rf $(OBJS) \
		brighten_float_image.o brighten_float_image \
		brighten_in_memory.o brighten_in_memory \
		test_float_image_statistics \
		libasf_raster.a

test: interpolate.t.c all
	$(CC) $(CFLAGS) interpolate.t.c $(LIBS) -o interpolate.t
