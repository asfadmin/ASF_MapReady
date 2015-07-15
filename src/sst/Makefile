CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
CFLAGS += $(HDF5_CFLAGS)
# Makefile for libasf_simulation

include ../../make_support/system_rules

CFLAGS += \
	$(GSL_CFLAGS) \
	$(JPEG_CFLAGS) \
	$(PROJ_CFLAGS) \
	$(GLIB_CFLAGS) \
	$(PNG_CFLAGS) \
	$(TIFF_CFLAGS) \
	$(GEOTIFF_CFLAGS)

LIBS =  \
	$(LIBDIR)/libasf_export.a \
	$(LIBDIR)/libasf_terrcorr.a \
	$(LIBDIR)/libasf_geocode.a \
	$(LIBDIR)/libasf_sar.a \
	$(LIBDIR)/libasf_vector.a \
	$(LIBDIR)/libasf_import.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/libasf_sgpsdp.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf_fft.a \
	$(SHAPELIB_LIBS) \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(SHAPELIB_LIBS) \
	$(GEOTIFF_LIBS) \
	$(TIFF_LIBS) \
	$(PNG_LIBS) \
	$(JPEG_LIBS) \
	$(PROJ_LIBS) \
	$(GSL_LIBS) \
	$(GLIB_LIBS) \
	$(LDFLAGS) \
	-lm -lz

OBJLIB = 	\
	tle.o \
	config.o \
	sar_simulation_tool.o

all: lib exe clean

lib: $(OBJLIB)
	rm -f libasf_simulation.a
	ar r libasf_simulation.a $(OBJLIB)
	$(RANLIB) libasf_simulation.a
	mv libasf_simulation.a $(LIBDIR)
	cp asf_simulation.h $(ASF_INCLUDE_DIR)
	cp satellite.config $(SHAREDIR)

exe: sar_simulation_tool

sar_simulation_tool: sar_simulation_tool_main.c lib
	$(CC) $(CFLAGS) -o sst sar_simulation_tool_main.c $(LIBDIR)/libasf_simulation.a $(LIBS)
	rm *.o
	mv sst$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f *.o *~ core*
