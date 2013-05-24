

include ../../make_support/system_rules

INCLUDES = -I/home/talogan/ASF_MapReady/include -Ilibsgp4

LIBS  = \
	$(LIBDIR)/libasf_sar.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/asf_meta.a \
	$(GSL_LIBS) \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(PROJ_LIBS) \
	$(XML_LIBS) \
	libfftw3f.a \
	libsgp4.a \
	-lm

OBJS = create_roi_in.o \
	dop.o \
	spectra.o \
	fix_vectors.o \
	get_peg.o \
	tle_code.o \
	runtest.o \
	get_line_for_frame.o

all: create_roi_in

runtest.o: runtest.cpp
	c++ -c runtest.cpp -Ilibsgp4
	
create_roi_in: $(OBJS)
	c++ $(CFLAGS) $(LFLAGS) -o create_roi_in $(OBJS) $(LIBS) $(LDFLAGS) $(INCLUDES)
	mv create_roi_in $(BINDIR)

create_base_images: get_line_for_frame.o create_base_images.o
	cc $(CFLAGS) $(LFLAGS) -o create_base_images get_line_for_frame.o create_base_images.o $(LIBS) $(LDFLAGS) $(INCLUDES)
	mv create_base_images $(BINDIR)
	

clean:
	rm -f $(OBJS)
