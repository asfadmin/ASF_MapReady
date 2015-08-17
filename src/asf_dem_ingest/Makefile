CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
CFLAGS += $(HDF5_CFLAGS)
# Makefile for asf_dem_ingest

DEST_TARGET = asf_dem_ingest

include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/libasf_geocode.a \
	$(LIBDIR)/libasf_vector.a \
	$(LIBDIR)/libasf_import.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/libgdal.a \
	$(GSL_LIBS) \
	$(GLIB_LIBS) \
	$(PROJ_LIBS) \
        $(TIFF_LIBS) \
        $(JPEG_LIBS) \
        $(PNG_LIBS) \
        $(GEOTIFF_LIBS) \
	$(XML_LIBS) \
	$(NETCDF_LIBS) \
	$(HDF5_LIBS) \
	$(ZLIB_LIBS) \
	-ldl

OBJS  = asf_dem_ingest.o

all:    asf_dem_ingest clean

asf_dem_ingest: $(OBJS)
	g++ $(CFLAGS) -o asf_dem_ingest $(OBJS) $(LIBS) $(LDFLAGS)
	mv asf_dem_ingest$(BIN_POSTFIX) $(BINDIR)/$(DEST_TARGET)$(BIN_POSTFIX)

clean:
	rm -f core $(OBJS) *.o *~
