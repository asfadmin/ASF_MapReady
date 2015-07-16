CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
CFLAGS += $(HDF5_CFLAGS)
# Makefile for          :  srf2shape
# Module Author         :  Rudi Gens 

include ../../make_support/system_rules

LIBS =  $(LIBDIR)/asf.a $(LIBDIR)/asf_meta.a -lm

OBJS = 	srf2shape.o
BOBJS = base2shape.o

all: srf2shape base2shape

srf2shape: $(OBJS)
	$(CC) -o srf2shape $(OBJS) $(LIBS)
	mv srf2shape$(BIN_POSTFIX) $(BINDIR)
	rm -f *.o

base2shape: $(BOBJS)
	$(CC) -o base2shape $(BOBJS) $(LIBS)
	mv base2shape$(BIN_POSTFIX) $(BINDIR)
	rm -f *.o

