# Makefile for		: igram
# Module Author		: Mike Shindle

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	-lm
OBJS = igram.o

igram: $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o igram $(OBJS) $(LIBS)
	mv igram$(BIN_POSTFIX) $(BINDIR)
	cp igram.1 ../../man/man1
	rm *.o

