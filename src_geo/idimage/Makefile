# Makefile for          :  idtable & idimage
# Module Author         :  Mike Shindle  

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a
TOBJS = idtable.o 
IOBJS = idimage.o

all: idtable idimage

idtable: $(TOBJS) $(LIBS)
	$(CC) $(CFLAGS) -o idtable $(TOBJS) $(LIBS) -lm
	rm *.o
	mv idtable$(BIN_POSTFIX) $(BINDIR)
	cp idtable.man ../../man/cat1/idtable.1


idimage: $(IOBJS) $(LIBS)
	$(CC) $(CFLAGS) -o idimage $(IOBJS) $(LIBS) -lm
	rm *.o
	mv idimage$(BIN_POSTFIX) $(BINDIR)
	cp idimage.man ../../man/cat1/idimage.1

