# Makefile for		: fit_line
# Module Author		: Orion Lawlor

include ../../make_support/system_rules

LIBS = $(LIBDIR)/asf.a 

fit_line: fit_line.c
	$(CC) $(CFLAGS) -o fit_line fit_line.c $(LIBS) -lm #-lm needed for solaris
	mv fit_line$(BIN_POSTFIX) $(BINDIR)
	cp fit_line.1 ../../man/cat1
