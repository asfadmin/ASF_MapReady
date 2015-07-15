CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
CFLAGS += $(HDF5_CFLAGS)
# Makefile for		: trim_zeros

include ../../make_support/system_rules

LIBS =  $(LIBDIR)/libasf_raster.a \
        $(LIBDIR)/asf.a \
        $(LIBDIR)/asf_meta.a \
        $(LIBDIR)/libasf_proj.a \
        $(LIBDIR)/asf.a \
        $(PROJ_LIBS) \
        $(GSL_LIBS) \
        $(XML_LIBS) \
	-lm
CFLAGS += $(PROJ_CFLAGS) $(GSL_CFLAGS) $(GLIB_CFLAGS)
all: trim_zeros

trim_zeros: trim_zeros.c
	$(CC) $(CFLAGS) $(LFLAGS) -o trim_zeros trim_zeros.c $(LIBS) $(LDFLAGS)
	mv trim_zeros$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f *.o core* *~

