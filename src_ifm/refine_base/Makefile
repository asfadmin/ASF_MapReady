# Makefile for		: refine_base
# Module Author		: Rob Fatland & Mike Shindle

include ../../make_support/system_rules

LIBS = 	$(LIBDIR)/libifm.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf_las.a \
	$(LIBDIR)/asf.a -lm

OBJS = 	getphase.o \
	genab.o \
	bp.o \
	test_base.o \
	refine_base.o

all: refine_base

refine_base: $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o refine_base $(OBJS) $(LIBS)
	mv refine_base$(BIN_POSTFIX) $(BINDIR)
	rm *.o
	cp refine_base.1 ../../man/man1
