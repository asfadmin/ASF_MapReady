# Makefile for libasf_vector library

include ../../make_support/system_rules

CFLAGS := $(JPEG_CFLAGS) $(PROJ_CFLAGS) $(GLIB_CFLAGS) $(CFLAGS)

OBJS = \
	write_shape.o \
	read_shape.o \
	mask.o \
	kml.o \
	meta2shape.o

LIBS = \
	$(LIBDIR)/libasf_vector.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf.a \
	$(LIBDIR)/libshp.a \
	$(LIBDIR)/libasf_proj.a \
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
	rm -rf $(OBJS) libasf_vector.a

test: test_shape.t.c all
	$(CC) $(CFLAGS) test_shape.t.c $(LIBS) -o test_shape.t
