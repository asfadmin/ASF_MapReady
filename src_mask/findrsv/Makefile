# Makefile for          :  findrsv
# Module Author         :  M. Jessop
# Module Version        :  1.0
 
include ../../make_support/system_rules

LIBS = -lm


#
# Object libraries required to load the program
#
OBJLIB = $(LIBDIR)/asf_meta.a

findrsv : findrsv.c
	$(CC) $(CFLAGS) -o findrsv findrsv.c $(OBJLIB) $(LIBS)
	cp findrsv$(BIN_POSTFIX) $(BINDIR)
	mv findrsv$(BIN_POSTFIX) ../bin/
	cp findrsv.man ../../man/cat1/findrsv.1
