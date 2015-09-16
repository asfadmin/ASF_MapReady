CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
CFLAGS += $(HDF5_CFLAGS)
# Makefile for		: coh 
# Module Author		: Rob Fatland, Mike Shindle, & Tom Logan

include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/asf_meta.a \
	$(GSL_LIBS) \
	$(LIBDIR)/libasf_insar.a \
	$(LIBDIR)/libasf_ardop.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(PROJ_LIBS) \
	$(XML_LIBS) \
	-lm
OBJS = coh.o

all: coh
	rm *.o

coh: $(OBJS)
	$(CC) $(CFLAGS) -o coh $(OBJS) $(LIBS) $(LDFLAGS)
	mv coh$(BIN_POSTFIX) $(BINDIR)
	cp coh.1 ../../man/cat1
