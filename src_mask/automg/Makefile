# Makefile for          :  automg 
# Module Author         :  M. Jessop
# Module Version        :  1.0
 
include ../../make_support/system_rules

LIBS = -lm
 
#
# Object libraries required to load the program
#
OBJLIB = \
#	$(LIBDIR)/asf_meta.a \
#	$(LIBDIR)/asf_las.a \
	$(LIBDIR)/asf.a 

automg : automg.c
	$(CC) $(CFLAGS) -o automg automg.c $(OBJLIB) $(LIBS)
	cp automg$(BIN_POSTFIX) $(BINDIR)
	mv automg$(BIN_POSTFIX) ../bin/
