# Makefile for		: pigram
# Module Author		: Mike Shindle

include ../../make_support/system_rules

LIBS = $(LIBDIR)/asf.a -lmpi -lm
OBJS = pigram.o

pigram: $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o pigram $(OBJS) $(LIBS)
	mv pigram$(BIN_POSTFIX) $(BINDIR)
	cp pigram.1 ../../man/cat1
	rm *.o

