# Makefile for		: fit_plane
# Module Author		: Orion Lawlor

include ../../make_support/system_rules

LFLAGS = $(LIBDIR)/asf.a
#LIBS = -lm 

fit_plane: fit_plane.c
	$(CC) $(CFLAGS) -o fit_plane fit_plane.c $(LFLAGS) $(LIBS) -lm #-lm needed for solaris
	mv fit_plane$(BIN_POSTFIX) $(BINDIR)
	cp fit_plane.1 ../../man/cat1
