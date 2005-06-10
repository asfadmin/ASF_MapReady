# Makefile for          :  terrcorr 
# Module Author         :  Tom Logan   
# Module Version        :  2.0

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libgsl.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/libproj.a \
	$(LIBDIR)/libasf_reporting.a \
	-lm

terrcorr :  terrcorr.c 
	$(CC) $(CFLAGS) -o terrcorr terrcorr.c $(LIBS) $(LDFLAGS)
	mv terrcorr$(BIN_POSTFIX) $(BINDIR)
	cp terrcorr.man ../../man/cat1/terrcorr.1

