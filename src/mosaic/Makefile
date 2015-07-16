CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
CFLAGS += $(HDF5_CFLAGS)
include ../../make_support/system_rules

TARGET = mosaic

CFLAGS += $(W_ERROR) \
	$(GLIBS_CFLAGS) \
	$(TIFF_CFLAGS)

LIBS  = \
	$(LIBDIR)/libasf_geocode.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(PROJ_LIBS) \
	$(GSL_LIBS) \
	$(XML_LIBS) \
	$(GLIB_LIBS) \
	$(TIFF_LIBS) \
	$(JPEG_LIBS) \
	$(ZLIB_LIBS) \
	-lm

CFLAGS += $(GSL_CFLAGS) $(PROJ_CFLAGS) $(GLIB_CFLAGS)

OBJS  = $(TARGET).o

all: $(TARGET) combine layer_stack
	-rm *.o

$(TARGET): $(OBJS)
	$(CC) $(CFLAGS) -o $(TARGET) $(OBJS) $(LIBS) $(LDFLAGS)
	mv $(TARGET)$(BIN_POSTFIX) $(BINDIR)

combine: combine.o
	$(CC) $(CFLAGS) -o combine combine.o $(LIBS) $(LDFLAGS)
	mv combine$(BIN_POSTFIX) $(BINDIR)

layer_stack: layer_stack.o
	$(CC) $(CFLAGS) -o layer_stack layer_stack.o $(LIBS) $(LDFLAGS)
	mv layer_stack$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f core *.o

