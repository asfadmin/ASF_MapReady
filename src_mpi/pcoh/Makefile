# Makefile for		: pcoh 
# Module Author		: Rob Fatland, Mike Shindle, & Tom Logan

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libifm.a \
	-lmpi -lm
OBJS = pcoh.o

all: pcoh
	rm *.o

pcoh: $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o pcoh $(OBJS) $(LIBS)
	mv pcoh$(BIN_POSTFIX) $(BINDIR)
	cp pcoh.1 ../../man/cat1
