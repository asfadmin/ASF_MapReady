# Makefile for rtc_add.c
# Created by Tom Logan for ASF in 8/95 

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libgsl.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/libproj.a \
	$(LIBDIR)/libasf_reporting.a \
	-lm

rtc_add : rtc_add.c
	$(CC) $(CFLAGS) -o rtc_add rtc_add.c $(LIBS) $(LDFLAGS)
	mv rtc_add$(BIN_POSTFIX) $(BINDIR)
	cp rtc_add.man ../../man/cat1/rtc_add.1

