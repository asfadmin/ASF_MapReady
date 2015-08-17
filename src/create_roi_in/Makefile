CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
CFLAGS += $(HDF5_CFLAGS)

include ../../make_support/system_rules

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

OBJS = create_roi_in.o \
	dop.o \
	spectra.o \
	fix_vectors.o \
	get_peg.o \
	tle_code.o \
	runtest.o \
	esa_nodes.o

all: create_roi_in create_base_images reprocess_frame

runtest.o: runtest.cpp
	c++ -c runtest.cpp -I../../include/libsgp4
	
create_roi_in: $(OBJS)
	c++ $(CFLAGS) $(LFLAGS) -o create_roi_in $(OBJS) $(LIBS) $(LDFLAGS) $(INCLUDES)
	mv create_roi_in $(BINDIR)

create_base_images: esa_nodes.o create_base_images.o
	cc $(CFLAGS) $(LFLAGS) -o create_base_images esa_nodes.o create_base_images.o $(LIBS) $(LDFLAGS) $(INCLUDES)
	mv create_base_images $(BINDIR)

reprocess_frame: reprocess_frame.o
	cc $(CFLAGS) $(LFLAGS) -o reprocess_frame reprocess_frame.o $(LIBS) $(LDFLAGS) $(INCLUDES)
	mv reprocess_frame $(BINDIR)

clean:
	rm -f *.o
