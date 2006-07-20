# Makefile for		: escher 
# Module Author		: Rob Fatland & Mike Shindle

include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/asf_meta.a \
	$(GSL_LIBS) \
	$(LIBDIR)/libifm.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(PROJ_LIBS) \
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
