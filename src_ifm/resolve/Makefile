# Makefile for 		:  resolve
# Module Author 	:  Mike Shindle, Tom Logan, Orion Lawlor

include ../../make_support/system_rules

LIBS = $(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf.a -lm
OBJS = resolve.o \
       find_baseline.o 

all: resolve
	rm *.o

resolve:  $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o resolve $(OBJS) $(LIBS)
	cp resolve.1 ../../man/man1
	mv resolve$(BIN_POSTFIX) $(BINDIR)


