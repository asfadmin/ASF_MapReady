# Makefile for multimatch2vector

include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/libasf_import.a \
	$(LIBDIR)/libasf_export.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/libasf_vector.a \
	$(LIBDIR)/libshp.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf.a \
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

OBJS  = multimatch2vector.o

all: multimatch2vector clean

multimatch2vector: $(OBJS)
	$(CC) $(CFLAGS) -o multimatch2vector $(OBJS) $(LIBS) $(LDFLAGS)
	mv multimatch2vector$(BIN_POSTFIX) $(BINDIR)
#	cp *.png $(SHAREDIR)
clean:
	rm -f core* $(OBJS) *~
