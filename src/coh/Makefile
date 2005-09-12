# Makefile for		: coh 
# Module Author		: Rob Fatland, Mike Shindle, & Tom Logan

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libgsl.a \
	$(LIBDIR)/libifm.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/libproj.a \
	$(LIBDIR)/libasf_reporting.a \
	-lm
OBJS = coh.o

all: coh
	rm *.o

coh: $(OBJS)
	$(CC) $(CFLAGS) -o coh $(OBJS) $(LIBS) $(LDFLAGS)
	mv coh$(BIN_POSTFIX) $(BINDIR)
	cp coh.1 ../../man/cat1
