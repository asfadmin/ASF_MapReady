# Makefile for          :  ll2proj 
# Module Author         :  M. Jessop; mod. by J. Badgley
# Module Version        :  2.0
 
include ../../make_support/system_rules

LIBS = $(LIBDIR)/asf.a -lm

projfit : projfit.c
	$(CC) $(CFLAGS) -o projfit projfit.c $(LIBS) $(LDLIBS)
	mv projfit$(BIN_POSTFIX) $(BINDIR)
	cp projfit.man ../../man/cat1/projfit.1
