CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
CFLAGS += $(HDF5_CFLAGS)
# Makefile for		: trim
# Module Author		: Orion Lawlor

include ../../make_support/system_rules

LIBS =  $(LIBDIR)/libasf_raster.a \
        $(LIBDIR)/asf.a \
        $(LIBDIR)/asf_meta.a \
        $(LIBDIR)/libasf_proj.a \
        $(LIBDIR)/asf.a \
        $(PROJ_LIBS) \
        $(GSL_LIBS) \
	$(XML_LIBS) \
	$(GLIB_LIBS) \
	$(ZLIB_LIBS) \
	-lm

CFLAGS += $(PROJ_CFLAGS) $(GSL_CFLAGS) $(GLIB_CFLAGS)
all: trim

trim: trim.c
	$(CC) $(CFLAGS) $(LFLAGS) -o trim trim.c help.c $(LIBS) $(LDFLAGS)
	mv trim$(BIN_POSTFIX) $(BINDIR)
	cp *.1 ../../man/cat1

clean:
	rm -f *.o core* *~

