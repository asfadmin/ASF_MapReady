# Makefile for          :  maskgen.
# Module Author         :  M. Jessop.  Modified by J. Badgley
# Module Version        :  2.0

include ../../make_support/system_rules

LIBS = \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(GSL_LIBS) \
	$(PROJ_LIBS) \
	-lm

CFLAGS += \
	$(GSL_CFLAGS) \
	$(PROJ_CFLAGS)

maskgen : maskgen.c
	$(CC) $(CFLAGS) -o maskgen maskgen.c $(LIBS) $(LDFLAGS)
	mv maskgen$(BIN_POSTFIX) $(BINDIR)
	cp maskgen.man ../../man/cat1/maskgen.1
