CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
PROGRAM := extract_rgb_palette

include ../../make_support/system_rules

SOURCES := extract_rgb_palette.c help.c
HEADERS := 

CFLAGS += \
	$(GEOTIFF_CFLAGS) \
	$(TIFF_CFLAGS) \
	$(JPEG_CFLAGS) \
	$(PROJ_CFLAGS) \
	$(GLIB_CFLAGS) \
	$(GSL_CFLAGS)

LIBS  = \
	$(LIBDIR)/libasf_import.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf.a \
	$(GEOTIFF_LIBS) \
	$(TIFF_LIBS) \
	$(JPEG_LIBS) \
	$(PROJ_LIBS) \
	$(GLIB_LIBS) \
	$(GSL_LIBS) \
	-lm \
	-lz

LDLIBS := $(LIBS)

OBJS := $(patsubst %.c, %.o, $(SOURCES))

all: $(PROGRAM)

$(PROGRAM): $(OBJS) $(HEADERS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
	mv $(PROGRAM)$(BIN_POSTFIX) $(BINDIR)

.PHONY: clean
clean:
	rm -f $(PROGRAM) $(OBJS) core.* core *~

