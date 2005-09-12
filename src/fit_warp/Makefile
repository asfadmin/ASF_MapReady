# Makefile for		: fit_warp
# Module Author		: Orion Lawlor

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libgsl.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/libproj.a \
	$(LIBDIR)/libasf_reporting.a \
	-lm 

fit_warp: fit_warp.c
	$(CC) $(CFLAGS) -o fit_warp fit_warp.c $(LIBS) $(LDFLAGS)
	mv fit_warp$(BIN_POSTFIX) $(BINDIR)
	cp fit_warp.1 ../../man/cat1
