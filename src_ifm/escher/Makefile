# Makefile for		: escher 
# Module Author		: Rob Fatland & Mike Shindle

include ../../make_support/system_rules

LIBS = $(LIBDIR)/asf_las.a $(LIBDIR)/libifm.a $(LIBDIR)/asf.a -lm
OBJS = cuts.o \
	debug.o \
	integrate.o \
	main.o \
	mask.o \
	util.o 

all: escher
	- rm *.o

escher: $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o escher $(OBJS) $(LIBS)
	mv escher$(BIN_POSTFIX) $(BINDIR)
	cp escher.1 ../../man/man1
