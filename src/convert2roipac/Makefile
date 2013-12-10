# Makefile for convert2roipac

include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/libasf_import.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/libgdal.a \
	$(GSL_LIBS) \
	$(TIFF_LIBS) \
	$(GEOTIFF_LIBS) \
	$(HDF5_LIBS) \
	$(NETCDF_LIBS) \
	$(PROJ_LIBS) \
	$(GLIB_LIBS) \
	$(JPEG_LIBS) \
	$(XML_LIBS) \
	$(ZLIB_LIBS) \
	-lm

CFLAGS += \
	$(GEOTIFF_CFLAGS) \
	$(TIFF_CFLAGS)

OBJS  = convert2roipac.o

all: convert2roipac clean

convert2roipac: $(OBJS)
	g++ $(CFLAGS) -o convert2roipac $(OBJS) $(LIBS) $(LDFLAGS)
	mv convert2roipac$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f *.o
