CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
CFLAGS += $(HDF5_CFLAGS)
# Makefile for libasf_proj library

include ../../make_support/system_rules

CFLAGS += $(W_ERROR) \
	$(GLIB_CFLAGS) \
	$(TIFF_CFLAGS) \
	$(GEOTIFF_CFLAGS)

# libs for the test program
LIBS =	$(PROJ_LIBS) \
	$(LIBDIR)/asf.a \
	$(GSL_LIBS) \
	$(TIFF_LIBS) \
	$(GEOTIFF_LIBS) \
	-lm

OBJS = project.o spheroid_axes_lengths.o datum_spheroid.o geotiff_support.o

all: build_only
	cp libasf_proj.a $(LIBDIR)
	cp libasf_proj.h geotiff_support.h $(ASF_INCLUDE_DIR)
	cp spheroids.h $(ASF_INCLUDE_DIR)
	rm *.o

build_only: $(OBJS)
	rm -f libasf_proj.a
	ar r libasf_proj.a $(OBJS)
	$(RANLIB) libasf_proj.a

clean:
	rm -rf $(OBJS) libasf_proj.a project.t.o project.t test *.t.o

test: project.t.c  nad27.t.c $(OBJS)
	$(CC) $(CFLAGS) $(LIBDIR)/libasf_proj.a *.t.c $(LIBDIR)/libcunit.a $(LIBS) $(LIBDIR)/asf_meta.a $(XML_LIBS) -o test
	./test

