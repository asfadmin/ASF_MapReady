# Makefile for		: coh 
# Module Author		: Rob Fatland, Mike Shindle, & Tom Logan

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf_las.a \
	$(LIBDIR)/asf.a \
	$(LIBDIR)/libifm.a \
	-lm
OBJS = coh.o

all: coh
	rm *.o

coh: $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o coh $(OBJS) $(LIBS)
	mv coh$(BIN_POSTFIX) $(BINDIR)
	cp coh.1 ../../man/cat1
