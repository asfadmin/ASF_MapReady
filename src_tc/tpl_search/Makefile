# Makefile for          :  tpl_search
# Module Author         :  Tom Logan   
# Module Version        :  2.0

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	-lm

tpl_search: tpl_search.c
	$(CC) $(CFLAGS) -o tpl_search tpl_search.c $(LIBS)
	mv tpl_search$(BIN_POSTFIX) $(BINDIR)
	cp tpl_search.man ../../man/cat1/tpl_search.1


