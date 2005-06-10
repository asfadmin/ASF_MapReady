# Makefile for		: escher 
# Module Author		: Rob Fatland & Mike Shindle

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libgsl.a \
	$(LIBDIR)/libifm.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/libproj.a \
	$(LIBDIR)/libasf_reporting.a \
	-lm
OBJS = cuts.o \
	debug.o \
	integrate.o \
	main.o \
	mask.o \
	util.o 

all: escher
	- rm *.o

escher: $(OBJS)
	$(CC) $(CFLAGS) -o escher $(OBJS) $(LIBS) $(LDFLAGS)
	mv escher$(BIN_POSTFIX) $(BINDIR)
	cp escher.1 ../../man/man1
