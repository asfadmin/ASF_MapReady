# Makefile for          :  prop2ll 
# Module Author         :  M. Jessop
# Module Version        :  1.0
 
include ../../make_support/system_rules

LIBS = $(LIBDIR)/asf.a -lm

prop2ll : prop2ll.c
	$(CC) $(CFLAGS) -o prop2ll prop2ll.c $(LIBS) $(LDFLAGS)
	mv prop2ll$(BIN_POSTFIX) $(BINDIR)
	cp prop2ll.man ../../man/man1/
