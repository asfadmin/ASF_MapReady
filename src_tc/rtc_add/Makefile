# Makefile for rtc_add.c
# Created by Tom Logan for ASF in 8/95 

include ../../make_support/system_rules

LIBS= \
	$(LIBDIR)/asf_las.a $(LIBDIR)/asf.a -lm

rtc_add : rtc_add.c
	$(CC) $(CFLAGS) -o rtc_add rtc_add.c $(LIBS)
	mv rtc_add$(BIN_POSTFIX) $(BINDIR)
	cp rtc_add.man ../../man/cat1/rtc_add.1

