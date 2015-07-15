CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
CFLAGS += $(HDF5_CFLAGS)
# Makefile for meta_check

include ../../make_support/system_rules

CFLAGS += $(GLIB_CFLAGS)

LIBS  = \
	$(LIBDIR)/asf_meta.a \
	$(GSL_LIBS) \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(PROJ_LIBS) \
	$(GLIB_LIBS) \
	$(XML_LIBS)

OBJS  = meta_compare.o

all: meta_compare clean

meta_check: $(OBJS)
	$(CC) $(CFLAGS) -o meta_check $(OBJS) $(LIBS) $(LDFLAGS)
	mv meta_check$(BIN_POSTFIX) $(BINDIR)
meta_compare: $(OBJS)
	$(CC) $(CFLAGS) -o meta_compare $(OBJS) $(LIBS) $(LDFLAGS)
	mv meta_compare$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f core *.o *~
