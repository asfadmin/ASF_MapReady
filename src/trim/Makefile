# Makefile for		: trim
# Module Author		: Orion Lawlor

include ../../make_support/system_rules

LIBS =  $(LIBDIR)/libasf_raster.a \
        $(LIBDIR)/asf.a \
        $(LIBDIR)/asf_meta.a \
        $(LIBDIR)/libasf_proj.a \
        $(LIBDIR)/libproj.a \
        $(LIBDIR)/libasf_reporting.a \
        $(LIBDIR)/asf.a \
        $(LIBDIR)/libgsl.a \
	-lm

all: trim

trim: trim.c
	$(CC) $(CFLAGS) $(LFLAGS) -o trim trim.c $(LIBS) $(LDFLAGS)
	mv trim$(BIN_POSTFIX) $(BINDIR)
	cp *.1 ../../man/cat1

