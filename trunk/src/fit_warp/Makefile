# Makefile for		: fit_warp
# Module Author		: Orion Lawlor

include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/asf_meta.a \
	$(GSL_LIBS) \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(PROJ_LIBS) \
	-lm 

fit_warp: fit_warp.c
	$(CC) $(CFLAGS) -o fit_warp fit_warp.c $(LIBS) $(LDFLAGS)
	mv fit_warp$(BIN_POSTFIX) $(BINDIR)
	cp fit_warp.1 ../../man/cat1
