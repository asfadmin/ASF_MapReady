# Makefile for: asf_polsarpro_ingest
# Wrapper for ingesting PolSAR Pro data

include ../../make_support/system_rules

CFLAGS := \
	$(GEOTIFF_CFLAGS) \
	$(TIFF_CFLAGS) \
	$(JPEG_CFLAGS) \
	$(GLIB_CFLAGS) \
	$(HDF5_CFLAGS) \
	$(GEOTIFF_CFLAGS) \
	$(HDF5_CFLAGS) \
	$(XML_CFLAGS) \
	$(CFLAGS)

LIBS  = \
	$(LIBDIR)/libasf_import.a \
	$(LIBDIR)/libasf_sar.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(GEOTIFF_LIBS) \
	$(TIFF_LIBS) \
	$(JPEG_LIBS) \
	$(PROJ_LIBS) \
	$(GSL_LIBS) \
	$(XML_LIBS) \
	$(GLIB_LIBS) \
	$(ZLIB_LIBS) \
	-lm

all: asf_polsarpro_import

asf_polsarpro_import: asf_polsarpro_import.c
	$(CC) $(CFLAGS) -o asf_polsarpro_import asf_polsarpro_import.c $(LIBS)
	mv  asf_polsarpro_import$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f *.o core
