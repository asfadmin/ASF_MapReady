# Makefile for 		:  makeddr
# Module Author 	:  Orion Lawlor

include ../../make_support/system_rules

OBJLIB = $(LIBDIR)/asf_las.a $(LIBDIR)/asf.a $(LIBDIR)/asf_geolib.a

makeddr:  makeddr.c
	$(CC) $(CFLAGS) -o makeddr makeddr.c $(OBJLIB) -lm	# -lm needed for solaris
	mv makeddr$(BIN_POSTFIX) $(BINDIR)
	cp makeddr.1 ../../man/cat1


