# Makefile for rgps_subset

include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/libasf_import.a \
	$(LIBDIR)/libasf_export.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/libasf_vector.a \
	$(LIBDIR)/libshp.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf.a \
	$(LIBDIR)/libasf_proj.a \
	$(GSL_LIBS) \
	$(PROJ_LIBS) \
	$(GEOTIFF_LIBS) \
	$(TIFF_LIBS) \
	$(JPEG_LIBS) \
	$(GLIB_LIBS) \
	-lm

CFLAGS += \
	$(GSL_CLAGS) \
	$(GEOTIFF_CFLAGS) \
	$(TIFF_CFLAGS) \
	$(JPEG_CFLAGS) \
	$(PROJ_CFLAGS) \
	$(GLIB_CFLAGS) \
	$(SHAPELIB_CFLAGS)

OBJS  = rgps_subset.o

all: rgps_subset clean

rgps_subset: $(OBJS)
	$(CC) $(CFLAGS) -o rgps_subset $(OBJS) $(LIBS) $(LDFLAGS)
	mv rgps_subset$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f core* $(OBJS) *~
