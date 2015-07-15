CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
CFLAGS += $(HDF5_CFLAGS)
# Makefile for measures2csv
# Author Rudi Gens

include ../../make_support/system_rules

LIBS = \
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

OBJS_M2X = measures2csv.o

all: measures2csv clean

measures2csv: $(OBJS_M2X)
	$(CC) $(CFLAGS) $(LFLAGS) -o measures2csv $(OBJS_M2X) $(LIBS)
	mv measures2csv$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f $(OBJS_M2X)
