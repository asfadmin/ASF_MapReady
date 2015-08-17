CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
# Makefile for		: remap
# Module Author		: Orion Lawlor

include ../../make_support/system_rules

OBJS = CLA.o \
	Matrix2D.o \
	mapping.o \
	fetcher.o \
	poly.o \
	quadratic.o \
	mapSamp.o \
	warpMap.o \
	writer.o \
	remap.o

CFLAGS += -Wall $(W_ERROR) $(GEOTIFF_CFLAGS) $(HDF5_CFLAGS)

all: lib clean

lib: $(OBJS)
	ar r libasf_remap.a $(OBJS)
	$(RANLIB) libasf_remap.a
	mv libasf_remap.a $(LIBDIR)
	cp asf_remap.h $(ASF_INCLUDE_DIR)

clean:
	rm -f *.o
