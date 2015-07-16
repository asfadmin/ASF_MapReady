CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
CFLAGS += $(HDF5_CFLAGS)
# Makefile for program raster_calc
# by Orion Lawlor, ASF, 1997
# by Patrick Denny, 2/2004

include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/asf_meta.a \
	$(GSL_LIBS) \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(PROJ_LIBS) \
	$(XML_LIBS) \
	-lm

OBJS  = raster_calc.o

all: raster_calc clean

raster_calc: $(OBJS)
	$(CC) $(CFLAGS) -o raster_calc $(OBJS) $(LIBS) $(LDFLAGS)
	mv raster_calc$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f core *.o
