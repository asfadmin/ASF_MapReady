CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
CFLAGS += $(HDF5_CFLAGS)
include ../../make_support/system_rules

TARGET = plan
LIBNAME = libasf_$(TARGET).a
CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(SHAPELIB_CFLAGS)

LIBS  = \
	$(LIBDIR)/libasf_sar.a \
	$(LIBDIR)/libasf_vector.a \
	$(LIBDIR)/libasf_import.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/libasf_sgpsdp.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf_fft.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(SHAPELIB_LIBS) \
	$(PROJ_LIBS) \
	$(GSL_LIBS) \
	$(XML_LIBS) \
	$(GLIB_LIBS) \
	$(GEOTIFF_LIBS) \
	$(HDF5_LIBS) \
	$(TIFF_LIBS) \
	$(JPEG_LIBS) \
	$(ZLIB_LIBS) \
	-lm

CFLAGS += \
	$(GSL_CFLAGS) \
	$(PROJ_CFLAGS) \
	$(GLIB_CFLAGS) \
	$(TIFF_CFLAGS) \
	$(XML_CFLAGS) \
	$(GEOTIFF_CFLAGS)

OBJS  = $(TARGET).o \
	polygon.o \
	beam_mode_table.o \
	kml.o \
	pass.o \
	overlap.o \
	tle.o

all: lib exe clean

lib: $(OBJS)
	ar r $(LIBNAME) $(OBJS)
	$(RANLIB) $(LIBNAME)
	mv $(LIBNAME) $(LIBDIR)
	cp beam_modes.txt $(SHAREDIR)
	cp polygon.h plan.h $(ASF_INCLUDE_DIR)
	cp plan_internal.h $(ASF_INCLUDE_DIR)
	cp tle $(SHAREDIR)

exe: $(TARGET)_main.o
	$(CC) $(CFLAGS) -o $(TARGET) $(TARGET)_main.c $(LIBDIR)/$(LIBNAME) $(LIBS) $(LDFLAGS)
	mv $(TARGET)$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f core $(OBJS) *.o
