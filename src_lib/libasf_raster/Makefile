# Makefile for libasf_raster library

include ../../make_support/system_rules

OBJS = interpolate.o \
       kernel.o \
       scaling.o \
       stats.o

CFLAGS += $(shell pkg-config --cflags-only-I glib-2.0)

LIBS = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libgsl.a \
	-lm

all: build_only
	cp libasf_raster.a $(LIBDIR)
	cp asf_raster.h $(ASF_INCLUDE_DIR)

build_only: $(OBJS)
	rm -f libasf_raster.a
	ar r libasf_raster.a $(OBJS)
	$(RANLIB) libasf_raster.a

# Rebuild everything if makefile or any local of ASF header or library
# has changed.
$(OBJS): Makefile $(wildcard *.h) $(wildcard ../../include/*.h) \
	 $(LIBS)

clean:
	rm -rf $(OBJS) libasf_raster.a

test: interpolate.t.c all
	$(CC) $(CFLAGS) interpolate.t.c $(LIBDIR)/libasf_raster.a $(LIBS) -o interpolate.t
