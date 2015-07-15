CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
CFLAGS += $(HDF5_CFLAGS)
# Makefile for program image_stats
# by Rudi Gens, ASF, 2004
# Based on image_layers

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf_meta.a \
	$(GSL_LIBS) \
	$(LIBDIR)/libasf_proj.a \
	$(PROJ_LIBS) \
	$(LIBDIR)/asf.a \
	$(XML_LIBS) \
	-lm

OBJS  = meta_util.o \
	plot.o \
	utilities.o \
	image_stats.o

all: image_stats clean

image_stats: $(OBJS)
	$(CC) $(CFLAGS) $(LDFLAGS) -o image_stats $(OBJS) $(LIBS)
	mv image_stats$(BIN_POSTFIX) $(BINDIR)
#	cp image_layers.1 ../../man/cat1/

clean:
	rm -f core *.o
