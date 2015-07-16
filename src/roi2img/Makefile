CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
CFLAGS += $(HDF5_CFLAGS)
# Makefile for meta2ddr/ddr2meta
# Author Patrick Denny

include ../../make_support/system_rules

INCLUDES = -I../../include/libsgp4

LIBS  = \
	$(LIBDIR)/libasf_sar.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/asf_meta.a \
	$(GSL_LIBS) \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(PROJ_LIBS) \
	$(XML_LIBS) \
	$(LIBDIR)/libfftw3f.a \
	$(LIBDIR)/libsgp4.a \
	-lm -lz

OBJS = roi2img.o \
	tle_code.o \
	fix_vectors.o \
	runtest.o

all: roi2img

runtest.o: runtest.cpp
	c++ -c runtest.cpp -I../../include/libsgp4

roi2img: $(OBJS)
	c++ $(CFLAGS) $(LFLAGS) -o roi2img $(OBJS) $(LIBS) $(LDFLAGS) $(INCLUDES)
	mv roi2img$(BIN_POSTFIX) $(BINDIR)

convert_vectors: convert_vectors.o 
	cc $(CFLAGS) $(LFLAGS) -o convert_vectors convert_vectors.o $(LIBS) $(LDFLAGS) $(INCLUDES)

make_vector_files: make_vector_files.o 
	cc $(CFLAGS) $(LFLAGS) -o make_vector_files make_vector_files.o $(LIBS) $(LDFLAGS) $(INCLUDES)

clean:
	rm -f $(OBJS)
