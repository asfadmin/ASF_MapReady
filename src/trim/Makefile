# Makefile for		: trim
# Module Author		: Orion Lawlor

include ../../make_support/system_rules

LIBS = $(LIBDIR)/asf_las.a $(LIBDIR)/asf.a -lm 

trim: trim.c
	$(CC) $(CFLAGS) $(LFLAGS) -o trim trim.c $(LIBS)
	mv trim$(BIN_POSTFIX) $(BINDIR)
	cp *.1 ../../man/cat1
	
