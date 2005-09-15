# Makefile for          :  ll2proj
# Module Author         :  M. Jessop, mod. by J. Badgley
# Module Version        :  2.0
 
include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_geolib.a \
	$(LIBDIR)/asf_meta.a \
	-lm
	
ll2proj : ll2proj.c
	$(CC) $(CFLAGS) -o ll2proj ll2proj.c $(LIBS)
	cp ll2proj$(BIN_POSTFIX) $(BINDIR)
	mv ll2proj$(BIN_POSTFIX) ../bin/
	cp ll2proj.man ../../man/cat1/ll2proj.1
