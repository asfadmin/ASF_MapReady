# Makefile for		: trim
# Module Author		: Orion Lawlor

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	-lm 

trim: trim.c
	$(CC) $(CFLAGS) $(LFLAGS) -o trim trim.c $(LIBS)
	mv trim$(BIN_POSTFIX) $(BINDIR)
	cp *.1 ../../man/cat1
	
