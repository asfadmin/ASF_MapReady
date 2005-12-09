# Makefile for 		:  tpl_mult
# Module Author 	:  Tom Logan   
# Last Modified		:  7/97 

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_reporting.a \
	$(LIBDIR)/asf_fft.a \
        $(LIBDIR)/asf.a \
	-lm

tpl_mult:  tpl_mult.c
	$(CC) $(CFLAGS) -o tpl_mult tpl_mult.c $(OBJS) $(LIBS) $(LDFLAGS)
	mv tpl_mult$(BIN_POSTFIX) $(BINDIR)
	cp tpl_mult.man ../../man/cat1/tpl_mult.1
