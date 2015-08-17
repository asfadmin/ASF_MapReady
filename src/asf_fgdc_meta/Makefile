CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
CFLAGS += $(HDF5_CFLAGS)
# Makefile for asf_fgdc_meta

DEST_TARGET = asf_fgdc_meta

include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/libasf_vector.a \
	$(LIBDIR)/libasf_import.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/libgdal.a \
	$(GSL_LIBS) \
	$(XML_LIBS) \
	$(GLIB_LIBS) \
	$(PROJ_LIBS) \
        $(TIFF_LIBS) \
        $(JPEG_LIBS) \
        $(PNG_LIBS) \
        $(GEOTIFF_LIBS) \
	$(NETCDF_LIBS) \
	$(HDF5_LIBS) \
	$(HDFEOS5_LIBS)

ifeq ($(SYS),win32)

LIBS +=	$(ZLIB_LIBS)

else

LIBS +=	-ldl

endif

OBJS  = asf_fgdc_meta.o

all:    asf_fgdc_meta clean

asf_fgdc_meta: $(OBJS)
	g++ $(CFLAGS) -o asf_fgdc_meta $(OBJS) $(LIBS) $(LDFLAGS)
	mv asf_fgdc_meta$(BIN_POSTFIX) $(BINDIR)/$(DEST_TARGET)$(BIN_POSTFIX)

clean:
	rm -f core $(OBJS) *.o *~
