CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
# Makefile for: asf_gamma_ingest
# Wrapper for ingesting gamma data

include ../../make_support/system_rules

CFLAGS += $(GEOTIFF_CFLAGS) $(HDF5_CFLAGS)

LIBS  = \
	$(LIBDIR)/libasf_import.a \
	$(LIBDIR)/libasf_sar.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(LIBDIR)/libasf_raster.a \
	$(PROJ_LIBS) \
	$(GSL_LIBS) \
	$(XML_LIBS) \
	$(GLIB_LIBS) \
	$(ZLIB_LIBS) \
	-lm

all: asf_gamma_import

asf_gamma_import: asf_gamma_import.c
	$(CC) $(CFLAGS) -o asf_gamma_import asf_gamma_import.c $(LIBS)
	mv  asf_gamma_import$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f *.o core
