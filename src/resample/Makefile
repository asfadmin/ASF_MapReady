CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
CFLAGS += $(HDF5_CFLAGS)
include ../../make_support/system_rules

LIBS = $(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
        $(LIBDIR)/asf.a \
	$(PROJ_LIBS) \
	$(GSL_LIBS) \
	$(XML_LIBS) \
	$(GLIB_LIBS) \
	$(ZLIB_LIBS) \
	-lm

CFLAGS += $(GSL_CFLAGS) $(PROJ_CFLAGS) $(GLIB_CFLAGS)

OBJS = resample.o help.o

all: resample
	-rm *.o

resample:  $(OBJS)
	$(CC) $(CFLAGS) -o resample $(OBJS) $(LIBS) $(LDFLAGS)
	mv resample$(BIN_POSTFIX) $(BINDIR)
	cp resample.1 ../../man/man1/resample.1

clean:
	rm -f core* resample.o resample
