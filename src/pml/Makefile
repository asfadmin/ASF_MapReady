# Makefile for		: pml
# Module Author		: Mark Ayers

include ../../make_support/system_rules

CFLAGS := $(CFLAGS) -I/usr/local/mpi/include -DUSE_STDARG -DHAVE_STDLIB_H=1 \
	-DHAVE_STRING_H=1 -DHAVE_UNISTD_H=1 -DHAVE_STDARG_H=1 \
	-DUSE_STDARG=1 -DMALLOC_RET_VOID=1

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/libifm.a \
	$(LIBDIR)/asf_meta.a \
	-L/usr/local/mpi/lib \
	-lmpich -lnsl -lthread -lnsl -laio \
	-lm

OBJS = pml.o \
       c2i.o \
       colortable.o

pml: $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o pml $(OBJS) $(LIBS)
	rm *.o
	mv pml$(BIN_POSTFIX) $(BINDIR)
	cp pml.1 ../../man/man1
