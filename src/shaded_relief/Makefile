CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
# Makefile for          :  shaded_relief
# Module Author         :  Rudi Gens

include ../../make_support/system_rules

CFLAGS += $(GSL_CFLAGS) $(PROJ_CFLAGS) $(GLIB_CFLAGS) -Wall

OBJS  = shaded_relief.o

LIBS = \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/libasf_sar.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(PROJ_LIBS) \
	$(GSL_LIBS) \
	$(GLIB_LIBS) \
	$(LIBDIR)/asf_fft.a \
	$(XML_LIBS)

all: shaded_relief
	-rm *.o

shaded_relief: $(OBJS)
	$(CC) $(CFLAGS) -o shaded_relief $(OBJS) $(LIBS) -lm
	mv shaded_relief$(BIN_POSTFIX) $(BINDIR)

clean:
	-rm -f core *.o

