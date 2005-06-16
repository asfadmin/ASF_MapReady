# Makefile for        :  demclip
# Module Author       :  Tom Logan 
# Module Version      :  2.0

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libgsl.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/libproj.a \
	$(LIBDIR)/libasf_reporting.a \
	$(LIBDIR)/asf_geolib.a \
	-lm

demclip: demclip.c
	$(CC) $(CFLAGS) -o demclip demclip.c $(LIBS) $(LDFLAGS)
	mv demclip$(BIN_POSTFIX) $(BINDIR) 
	cp demclip.man ../../man/cat1/demclip.1
