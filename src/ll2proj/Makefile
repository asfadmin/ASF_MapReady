# Makefile for          :  ll2proj
# Module Author         :  M. Jessop, mod. by J. Badgley
# Module Version        :  2.0
 
include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/asf_geolib.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(GSL_LIBS) \
	$(PROJ_LIBS) \
	-lm

CFLAGS += \
	$(GSL_CFLAGS) \
	$(PROJ_CFLAGS)

all: ll2proj clean

ll2proj : ll2proj.c
	$(CC) $(CFLAGS) -o ll2proj ll2proj.c $(LIBS) $(LDFLAGS)
	mv ll2proj$(BIN_POSTFIX) $(BINDIR)
	cp ll2proj.man ../../man/cat1/ll2proj.1

clean:
	rm -f *.o *~ core*
