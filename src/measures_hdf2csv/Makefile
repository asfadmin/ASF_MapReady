CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
CFLAGS += $(HDF5_CFLAGS)
# Makefile for measures_hdf2csv
# Author Rudi Gens

include ../../make_support/system_rules

LIBS = \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(GSL_LIBS) \
	$(XML_LIBS) \
	$(HDF5_LIBS) \
	$(PROJ_LIBS) \
	$(GLIB_LIBS) \
	$(ZLIB_LIBS) \
	-lm

CFLAGS += $(GSL_CFLAGS) $(PROJ_CFLAGS)

OBJS_M2X = measures_hdf2csv.o

all: measures_hdf2csv clean

measures_hdf2csv: $(OBJS_M2X)
	$(CC) $(CFLAGS) $(LFLAGS) -o measures_hdf2csv $(OBJS_M2X) $(LIBS)
	mv measures_hdf2csv$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f $(OBJS_M2X)
