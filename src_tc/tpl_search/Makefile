# Makefile for          :  tpl_search
# Module Author         :  Tom Logan   
# Module Version        :  2.0

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libgsl.a \
	-lm

tpl_search: tpl_search.c
	$(CC) $(CFLAGS) -o tpl_search tpl_search.c $(LIBS) $(LDFLAGS)
	mv tpl_search$(BIN_POSTFIX) $(BINDIR)
	cp tpl_search.man ../../man/cat1/tpl_search.1


