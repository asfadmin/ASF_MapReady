# Makefile for          :  prop2ll 
# Module Author         :  M. Jessop
# Module Version        :  1.0
 
include ../../make_support/system_rules

LIBS = -lm
 
#
# Object libraries required to load the program
#
OBJLIB = \

	$(LIBDIR)/asf.a 

prop2ll : prop2ll.c
	$(CC) $(CFLAGS) -o prop2ll prop2ll.c $(OBJLIB) $(LIBS)
	cp prop2ll$(BIN_POSTFIX) $(BINDIR)
	mv prop2ll$(BIN_POSTFIX) ../bin/
	cp prop2ll.man ../../man/man1/
