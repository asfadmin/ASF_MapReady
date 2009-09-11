# Makefile for          :  automg 
# Module Author         :  M. Jessop
# Module Version        :  1.0
 
include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	-lm
 
automg : automg.c
	$(CC) $(CFLAGS) -o automg automg.c $(LIBS)
	cp automg$(BIN_POSTFIX) $(BINDIR)
	mv automg$(BIN_POSTFIX) ../bin/
