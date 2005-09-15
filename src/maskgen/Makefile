# Makefile for          :  maskgen.
# Module Author         :  M. Jessop.  Modified by J. Badgley
# Module Version        :  2.0

include ../../make_support/system_rules

LIBS = -lm

#
# Object libraries required to load the program
#
OBJLIB = $(LIBDIR)/asf_meta.a

maskgen : maskgen.c
	$(CC) $(CFLAGS) -o maskgen maskgen.c $(OBJLIB) $(LIBS)
	cp maskgen$(BIN_POSTFIX) $(BINDIR)
	mv maskgen$(BIN_POSTFIX) ../bin/
	cp maskgen.man ../../man/cat1/maskgen.1
