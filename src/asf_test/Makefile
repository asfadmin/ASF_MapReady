# Makefile for asf_test

include ../../make_support/system_rules

LIBS =  \
	$(LIBDIR)/libcunit.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf.a \
	$(TIFF_LIBS) \
	$(GEOTIFF_LIBS) \
	-lm

OBJS = 	\
	asf_test.o \
	config.o \
	uavsar_test.o \
	rsat1_test.o

all: asf_test clean

asf_test: $(OBJS)
	$(CC) $(CFLAGS) -o asf_test $(OBJS) $(LIBS)
	rm *.o
	mv asf_test$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f *.o *~ core*
