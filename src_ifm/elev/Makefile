# Makefile for		: elev 
# Module Author		: Mike Shindle

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	-lm
OBJS  = elev.o

all: elev
	rm *.o

elev: $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o elev $(OBJS) $(LIBS)
	mv elev$(BIN_POSTFIX) $(BINDIR)
	cp elev.1 ../../man/man1
