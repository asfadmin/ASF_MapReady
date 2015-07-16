CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
CFLAGS += $(HDF5_CFLAGS)
# Makefile for map_grid

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
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(GEOTIFF_LIBS) \
	$(TIFF_LIBS) \
	$(PNG_LIBS) \
	$(JPEG_LIBS) \
	$(PROJ_LIBS) \
	$(GSL_LIBS) \
	$(XML_LIBS) \
	$(GLIB_LIBS) \
	$(LDFLAGS) \
	-lm -lz

OBJS = 	map_grid.o 

all: map_grid clean

map_grid: $(OBJS)
	$(CC) $(CFLAGS) -o map_grid $(OBJS) $(LIBS)
	rm *.o
	mv map_grid$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f *.o *~ core*
