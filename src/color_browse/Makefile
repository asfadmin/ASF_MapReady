CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
CFLAGS += $(HDF5_CFLAGS)
# Makefile for fill_holes

TARGET = color_browse

include ../../make_support/system_rules

OBJS  = $(TARGET).o

LIBS = \
  $(LIBDIR)/libasf_export.a \
  $(LIBDIR)/libasf_vector.a \
  $(LIBDIR)/libasf_import.a \
	$(LIBDIR)/libasf_sar.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/asf_meta.a \
	$(SHAPELIB_LIBS) \
	$(LIBDIR)/libasf_proj.a \
  $(LIBDIR)/asf.a \
  $(GEOTIFF_LIBS) \
  $(NETCDF_LIBS) \
  $(HDF5_LIBS) \
	$(GLIB_LIBS) \
	$(GSL_LIBS) \
	$(PROJ_LIBS) \
	$(XML_LIBS) \
	$(TIFF_LIBS) \
	$(JPEG_LIBS) \
	$(PNG_LIBS) \
	$(ZLIB_LIBS) \
	-lm

CFLAGS += \
  $(GEOTIFF_CFLAGS) \
  $(TIFF_CFLAGS) \
  $(JPEG_CFLAGS) \
  $(PNG_CFLAGS) \
  $(PROJ_CFLAGS) \
	$(GLIB_CFLAGS) \
	$(GSL_CFLAGS) \
	$(XML_CFLAGS)

all: $(TARGET) clean

$(TARGET): $(OBJS)
	$(CC) $(CFLAGS) $(LDFLAGS) -o $(TARGET) $(OBJS) $(LIBS)
	mv $(TARGET)$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f *.o core*
