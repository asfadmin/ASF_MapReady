# Makefile for          :  psconv 
# Module Author         :  Tom Logan   
# Module Version        :  4.0

include ../../make_support/system_rules

LIBS = -lm
 
#
# Object libraries required to load the program
#
OBJLIB = $(LIBDIR)/asf_geolib.a 
 
psconv : psconv.c $(OBJLIB)
	$(CC) -o psconv psconv.c $(OBJLIB) $(LIBS) $(CFLAGS)
	mv psconv$(BIN_POSTFIX) $(BINDIR)
	cp psconv.man ../../man/cat1/psconv.1
