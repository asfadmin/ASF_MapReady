# Makefile for          :  sargeom 
# Module Author         :  Tom Logan   
# Module Version        :  4.0
 
include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/sarmodel.a \
	$(LIBDIR)/tcobj.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libgsl.a \
	-lm
 
sargeom : sargeom.c
	$(CC) $(CFLAGS) -o sargeom sargeom.c $(LIBS) $(LDFLAGS)
	mv sargeom$(BIN_POSTFIX) $(BINDIR)
	cp sargeom.man ../../man/cat1/sargeom.1
