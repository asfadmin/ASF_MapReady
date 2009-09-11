# Makefile for 		:  makeddr
# Module Author 	:  Orion Lawlor

include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/asf_meta.a \
	$(GSL_LIBS) \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(PROJ_LIBS)

makeddr: makeddr.c
	$(CC) $(CFLAGS) -o makeddr makeddr.c $(LIBS) $(LDFLAGS) -lm
	mv makeddr$(BIN_POSTFIX) $(BINDIR)
	cp makeddr.1 ../../man/cat1


