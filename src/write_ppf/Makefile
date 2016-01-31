# Makefile for write_ppf

include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/libasf_import.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/asf.a \
	$(PROJ_LIBS) \
	$(GSL_LIBS) \
	$(XML_LIBS) \
	$(GLIB_LIBS) \
	$(ZLIB_LIBS) \
	-lm

CFLAGS += \
	$(HDF5_CFLAGS) \
	$(GEOTIFF_CFLAGS) \
	$(HDF5_CFLAGS) \
	$(XML_CFLAGS)

OBJS  = write_ppf.o

all: write_ppf clean

write_ppf: $(OBJS)
	$(CC) $(CFLAGS) -o write_ppf $(OBJS) $(LIBS) $(LDFLAGS)
	mv write_ppf$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f core *.o
