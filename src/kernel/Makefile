CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
CFLAGS += $(HDF5_CFLAGS)
include ../../make_support/system_rules

PROG = kernel

LIBS =  $(LIBDIR)/libasf_raster.a \
        $(LIBDIR)/asf.a \
        $(LIBDIR)/asf_meta.a \
        $(LIBDIR)/libasf_proj.a \
        $(LIBDIR)/asf.a \
        $(PROJ_LIBS) \
        $(GSL_LIBS) \
	$(GLIB_LIBS) \
        $(XML_LIBS) \
	-lm

CFLAGS += $(PROJ_CFLAGS) $(GSL_CFLAGS) $(GLIB_CFLAGS)
all: $(PROG)

$(PROG): $(PROG).c
	$(CC) $(CFLAGS) $(LFLAGS) -o $(PROG) $(PROG).c $(LIBS) $(LDFLAGS)
	mv $(PROG)$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f *.o core* *~

