# Makefile for          :  despike 
# Module Author         :  Tom Logan   
# Module Version        :  7.0

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	-lm
 
despike :  despike.c
	$(CC) $(CFLAGS) -o despike despike.c $(LIBS) $(LDFLAGS)
	mv despike$(BIN_POSTFIX) $(BINDIR)
	cp despike.man ../../man/cat1/despike.1


