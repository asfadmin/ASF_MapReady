# Makefile for: 	get_rectypes
# Module Author:	Patrick Denny

include ../../make_support/system_rules

LIBS =	$(LIBDIR)/asf_meta.a $(LIBDIR)/asf.a

get_rectypes: get_rectypes.c
	$(CC) $(CFLAGS) $(LFLAGS) -o get_rectypes get_rectypes.c $(LIBS)
	mv get_rectypes$(BIN_POSTFIX) $(BINDIR)
	cp get_rectypes.1 ../../man/cat1

clean:
	rm -f core
