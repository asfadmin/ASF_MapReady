CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
CFLAGS += $(HDF5_CFLAGS)
# Makefile for meta2xml
# Author Rudi Gens

include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(GSL_LIBS) \
	$(XML_LIBS) \
	$(PROJ_LIBS) \
	$(GLIB_LIBS) \
	$(ZLIB_LIBS) \
	-lm

CFLAGS += $(GSL_CFLAGS) $(PROJ_CFLAGS)

OBJS_M2X = meta2xml.o

all: meta2xml clean

meta2xml: $(OBJS_M2X)
	$(CC) $(CFLAGS) $(LFLAGS) -o meta2xml $(OBJS_M2X) $(LIBS)
	mv meta2xml$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f $(OBJS_M2X)

