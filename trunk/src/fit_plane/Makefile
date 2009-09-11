# Makefile for		: fit_plane
# Module Author		: Orion Lawlor

include ../../make_support/system_rules

LIBS = $(LIBDIR)/asf.a

fit_plane: fit_plane.c
	$(CC) $(CFLAGS) -o fit_plane fit_plane.c $(LIBS) $(LDFLAGS) -lm
	mv fit_plane$(BIN_POSTFIX) $(BINDIR)
	cp fit_plane.1 ../../man/cat1
