# Makefile for		: fit_warp
# Module Author		: Orion Lawlor

include ../../make_support/system_rules

LFLAGS = $(LIBDIR)/asf_las.a $(LIBDIR)/asf.a
LIBS = -lm 

fit_warp: fit_warp.c
	$(CC) $(CFLAGS) -o fit_warp fit_warp.c $(LFLAGS) $(LIBS)
	mv fit_warp$(BIN_POSTFIX) $(BINDIR)
	cp fit_warp.1 ../../man/cat1
