# Makefile for        :  demclip
# Module Author       :  Tom Logan 
# Module Version      :  2.0

include ../../make_support/system_rules

LIBS = $(LIBDIR)/asf_geolib.a $(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf_las.a $(LIBDIR)/asf.a -lm

demclip: demclip.c
	$(CC) $(CFLAGS) -o demclip demclip.c $(LIBS)
	mv demclip$(BIN_POSTFIX) $(BINDIR) 
	cp demclip.man ../../man/cat1/demclip.1
