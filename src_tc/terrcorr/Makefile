# Makefile for          :  terrcorr 
# Module Author         :  Tom Logan   
# Module Version        :  2.0

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	-lm

terrcorr :  terrcorr.c 
	$(CC) $(CFLAGS) -o terrcorr terrcorr.c $(LIBS) $(CFLAGS)
	mv terrcorr$(BIN_POSTFIX) $(BINDIR)
	cp terrcorr.man ../../man/cat1/terrcorr.1

