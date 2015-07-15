CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
CFLAGS += $(HDF5_CFLAGS)
# Makefile for		: fit_warp
# Module Author		: Orion Lawlor

include ../../make_support/system_rules

CFLAGS += $(GLIB_CFLAGS)

LIBS  = \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/asf_meta.a \
	$(GSL_LIBS) \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(PROJ_LIBS) \
	$(XML_LIBS) \
	$(GLIB_LIBS) \
	-lm -lz

fit_warp: fit_warp.c
	$(CC) $(CFLAGS) -o fit_warp fit_warp.c $(LIBS) $(LDFLAGS)
	mv fit_warp$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f *.o
