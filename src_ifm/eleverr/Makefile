# Makefile for		: eleverr 
# Module Author		: Mike Shindle

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	-lm
OBJS  = eleverr.o 

eleverr: $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o eleverr $(OBJS) $(LIBS)
	rm *.o
	mv eleverr$(BIN_POSTFIX) $(BINDIR)
	cp eleverr.1 ../../man/cat1
