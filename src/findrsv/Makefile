# Makefile for          :  findrsv
# Module Author         :  M. Jessop
# Module Version        :  1.0
 
include ../../make_support/system_rules

LIBS = \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf.a \
	$(GSL_LIBS) \
	-lm

CFLAGS += $(GSL_CFLAGS)

findrsv : findrsv.c
	$(CC) $(CFLAGS) -o findrsv findrsv.c $(OBJLIB) $(LIBS)
	mv findrsv$(BIN_POSTFIX) $(BINDIR)
	cp findrsv.man ../../man/cat1/findrsv.1
