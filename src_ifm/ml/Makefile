# Makefile for		: ml
# Module Author		: Mike Shindle

include ../../make_support/system_rules

LIBS = 	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libifm.a \
	$(LIBDIR)/asf.a \
	-lm  
OBJS = ml.o \
       c2i.o \
       colortable.o

ml: $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o ml $(OBJS) $(LIBS)
	rm *.o
	mv ml$(BIN_POSTFIX) $(BINDIR)
	cp ml.1 ../../man/man1
