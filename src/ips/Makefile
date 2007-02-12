#	ips
#	by Rudi Gens, ASF, 2001

include ../../make_support/system_rules

CFLAGS += \
	$(GSL_CLAGS) \
	$(GEOTIFF_CFLAGS) \
	$(TIFF_CFLAGS) \
	$(JPEG_CFLAGS) \
	$(PROJ_CFLAGS) \
	$(GLIB_CFLAGS) \
	$(SHAPELIB_CFLAGS)

LIBS = 	\
	$(LIBDIR)/libasf_insar.a \
	$(LIBDIR)/libasf_import.a \
	$(LIBDIR)/libasf_export.a \
	$(LIBDIR)/libasf_geocode.a \
	$(LIBDIR)/libasf_terrcorr.a \
	$(LIBDIR)/libasf_vector.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/libasf_sar.a \
	$(LIBDIR)/asf_fft.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf.a \
	$(GSL_LIBS) \
	$(GEOTIFF_LIBS) \
	$(TIFF_LIBS) \
	$(JPEG_LIBS) \
	$(PROJ_LIBS) \
	$(GLIB_LIBS) \
	$(SHAPELIB_LIBS) \
	-lm -lz

OBJLIB	= config.o \
	functions.o \
	check_refinement.o \
	ips.o

all: ips clean

ips: ips_main.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o ips ips_main.c $(LIBS)
	mv ips $(BINDIR)
#	cp ips.1 ../../man/cat1

clean:
	rm -f core* *.o
