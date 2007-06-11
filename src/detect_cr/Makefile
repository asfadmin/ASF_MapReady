# Makefile for detect_cr
# Module Author: Rudi Gens (sliced and diced from coregister_fine,
#	                    coregister_coarse and fftMatch) 

include ../../make_support/system_rules

CFLAGS += $(GSL_CFLAGS) $(PROJ_CFLAGS) $(GLIB_CFLAGS) $(JPEG_CFLAGS)

LIBS  = \
	$(LIBDIR)/libasf_geocode.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(LIBDIR)/libasf_insar.a \
	$(LIBDIR)/asf_fft.a \
	$(GSL_LIBS) \
	$(PROJ_LIBS) \
	$(GLIB_LIBS) \
	$(JPEG_LIBS)

OBJS  = detect_cr.o \
	utilities.o

all: detect_cr clean

detect_cr: $(OBJS)
	$(CC) $(CFLAGS) -o detect_cr $(OBJS) $(LIBS) $(LDFLAGS)
	mv detect_cr$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f core *.o
