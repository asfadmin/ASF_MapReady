# Makefile for          :  ll2proj 
# Module Author         :  M. Jessop; mod. by J. Badgley
# Module Version        :  2.0
 
include ../../make_support/system_rules

LIBS = -lm

#
# Object libraries required to load the program
#
OBJLIB = $(LIBDIR)/asf.a 

projfit : projfit.c
	$(CC) $(CFLAGS) -o projfit projfit.c $(OBJLIB) $(LIBS)
	cp projfit$(BIN_POSTFIX) $(BINDIR)
	mv projfit$(BIN_POSTFIX) ../bin/
	cp projfit.man ../../man/cat1/projfit.1
