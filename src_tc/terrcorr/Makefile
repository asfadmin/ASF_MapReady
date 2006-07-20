# Makefile for          :  terrcorr 
# Module Author         :  Tom Logan   
# Module Version        :  2.0

include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(GSL_LIBS) \
	$(PROJ_LIBS) \
	-lm

CFLAGS += $(GSL_CFLAGS) $(PROJ_CFLAGS)

terrcorr :  terrcorr.c 
	$(CC) $(CFLAGS) -o terrcorr terrcorr.c $(LIBS) $(LDFLAGS)
	mv terrcorr$(BIN_POSTFIX) $(BINDIR)
	cp terrcorr.man ../../man/cat1/terrcorr.1

