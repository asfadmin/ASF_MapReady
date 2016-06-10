# Makefile for write_hdf5_xml
# Author Rudi Gens

include ../../make_support/system_rules

LIBS  = \
  $(LIBDIR)/libasf_import.a \
  $(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(GEOTIFF_LIBS) \
	$(TIFF_LIBS) \
	$(JPEG_LIBS) \
	$(GSL_LIBS) \
	$(XML_LIBS) \
	$(PROJ_LIBS) \
	$(GLIB_LIBS) \
	$(ZLIB_LIBS) \
	-lm

CFLAGS += $(GSL_CFLAGS) \
 	$(GLIB_CFLAGS) \
 	$(PROJ_CFLAGS) \
 	$(TIFF_CFLAGS) \
 	$(GEOTIFF_CFLAGS) \
 	$(JPEG_CFLAGS) \
	$(HDF5_CFLAGS) \
	$(GEOTIFF_CFLAGS) \
	$(HDF5_CFLAGS) \
        $(XML_CFLAGS)

OBJS_M2X = write_hdf5_xml.o

all: write_hdf5_xml clean

write_hdf5_xml: $(OBJS_M2X)
	$(CC) $(CFLAGS) $(LFLAGS) -o write_hdf5_xml $(OBJS_M2X) $(LIBS)
	mv write_hdf5_xml$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f $(OBJS_M2X)

