CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
CFLAGS += $(HDF5_CFLAGS)
include ../../make_support/system_rules

TARGET = create_thumbs

SRCS = \
	$(TARGET).c \
	help.c

INCLUDES = \
	$(TARGET)_help.h

LIBS = \
	$(LIBDIR)/libasf_import.a \
	$(LIBDIR)/libasf_export.a \
	$(LIBDIR)/libasf_vector.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/libasf_ardop.a \
	$(LIBDIR)/libasf_sar.a \
	$(LIBDIR)/asf_fft.a \
	$(LIBDIR)/asf_meta.a \
	$(SHAPELIB_LIBS) \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(PNG_LIBS) \
	$(PROJ_LIBS) \
	$(GSL_LIBS) \
	$(GLIB_LIBS) \
	$(GTK_LIBS) \
	$(TIFF_LIBS) \
	$(GEOTIFF_LIBS) \
	$(JPEG_LIBS) \
	$(XML_LIBS) \
	$(NETCDF_LIBS) \
	$(HDF5_LIBS) \
	$(HDFEOS5_LIBS) \
	$(ZLIB_LIBS)

CFLAGS += \
	$(GSL_CFLAGS) \
	$(PNG_CFLAGS) \
	$(PROJ_CFLAGS) \
	$(GLIB_CFLAGS) \
	$(GTK_CFLAGS) \
	$(TIFF_CFLAGS) \
	$(GEOTIFF_CFLAGS) \
	-D_XOPEN_SOURCE=600

OBJS = $(patsubst %.c, %.o, $(SRCS))

$(TARGET): $(SRCS) $(INCLUDES)
	$(CC) $(CFLAGS) -o $(TARGET) $(SRCS) $(LIBS) $(LDFLAGS)
	rm -f *.o
	mv $(TARGET)$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f core* *.o $(TARGET)

