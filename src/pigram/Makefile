# Makefile for		: pigram
# Module Author		: Mike Shindle

include ../../make_support/system_rules

CFLAGS := $(CFLAGS) -I/usr/local/mpi/include -DUSE_STDARG -DHAVE_STDLIB_H=1 \
	-DHAVE_STRING_H=1 -DHAVE_UNISTD_H=1 -DHAVE_STDARG_H=1 \
	-DUSE_STDARG=1 -DMALLOC_RET_VOID=1

LIBS = $(LIBDIR)/asf.a \
	-L/usr/local/mpi/lib \
	-lmpich -lnsl -lthread -lnsl -laio \
	-lm

OBJS = pigram.o

pigram: $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o pigram $(OBJS) $(LIBS)
	mv pigram$(BIN_POSTFIX) $(BINDIR)
	cp pigram.1 ../../man/cat1
	rm *.o

