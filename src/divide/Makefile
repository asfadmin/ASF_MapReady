CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
CFLAGS += $(HDF5_CFLAGS)
# Makefile for fill_holes

TARGET = divide

include ../../make_support/system_rules

OBJS  = $(TARGET).o

LIBS = \
	$(LIBDIR)/libasf_sar.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
  $(LIBDIR)/asf.a \
	$(GLIB_LIBS) \
	$(GSL_LIBS) \
	$(PROJ_LIBS) \
	$(XML_LIBS) \
	$(TIFF_LIBS) \
	$(JPEG_LIBS)

CFLAGS += \
	$(GLIB_CFLAGS) \
	$(TIFF_CFLAGS)

all: $(TARGET) clean

$(TARGET): $(OBJS)
	$(CC) $(CFLAGS) $(LDFLAGS) -o $(TARGET) $(OBJS) $(LIBS)
	mv $(TARGET)$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f *.o core*
