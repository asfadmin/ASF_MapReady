# Makefile for		: fit_warp
# Module Author		: Orion Lawlor

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	-lm 

fit_warp: fit_warp.c
	$(CC) $(CFLAGS) -o fit_warp fit_warp.c $(LIBS)
	mv fit_warp$(BIN_POSTFIX) $(BINDIR)
	cp fit_warp.1 ../../man/cat1
