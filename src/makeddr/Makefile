# Makefile for 		:  makeddr
# Module Author 	:  Orion Lawlor

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(GSL_LIBS) \
	$(LIBDIR)/libasf_proj.a \
	$(PROJ_LIBS) \
	$(LIBDIR)/libasf_reporting.a \
	$(LIBDIR)/asf_geolib.a

makeddr: makeddr.c
	$(CC) $(CFLAGS) -o makeddr makeddr.c $(LIBS) $(LDFLAGS) -lm
	mv makeddr$(BIN_POSTFIX) $(BINDIR)
	cp makeddr.1 ../../man/cat1


