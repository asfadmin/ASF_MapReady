# Makefile for		: pcoh 
# Module Author		: Rob Fatland, Mike Shindle, & Tom Logan

include ../../make_support/system_rules

CFLAGS := $(CFLAGS) -I/usr/local/mpi/include -DUSE_STDARG -DHAVE_STDLIB_H=1 \
	-DHAVE_STRING_H=1 -DHAVE_UNISTD_H=1 -DHAVE_STDARG_H=1 \
	-DUSE_STDARG=1 -DMALLOC_RET_VOID=1

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libifm.a \
	-L/usr/local/mpi/lib \
	-lmpich -lnsl -lthread -lnsl -laio \
	-lm

OBJS = pcoh.o

all: pcoh
	rm *.o

pcoh: $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o pcoh $(OBJS) $(LIBS)
	mv pcoh$(BIN_POSTFIX) $(BINDIR)
	cp pcoh.1 ../../man/cat1
