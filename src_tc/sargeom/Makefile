# Makefile for          :  sargeom 
# Module Author         :  Tom Logan   
# Module Version        :  4.0
 
include ../../make_support/system_rules

LIBS = -lm
 
#
# Object libraries required to load the program
#
OBJLIB = \
	$(LIBDIR)/sarmodel.a \
	$(LIBDIR)/tcobj.a \
	$(LIBDIR)/asf_las.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf.a 
 
sargeom : sargeom.c
	$(CC) $(CFLAGS) -o sargeom sargeom.c $(OBJLIB) $(LIBS)
	mv sargeom$(BIN_POSTFIX) $(BINDIR)
	cp sargeom.man ../../man/cat1/sargeom.1
