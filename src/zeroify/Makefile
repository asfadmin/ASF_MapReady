# Makefile for zeroify

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(GSL_LIBS) \
	$(LIBDIR)/libasf_proj.a \
	$(PROJ_LIBS) \
	$(LIBDIR)/libasf_reporting.a \
	-lm

OBJS = zeroify.o

all:  zeroify
	rm *.o
	cp zeroify.1 ../../man/cat1

zeroify: $(OBJS)
	$(CC) $(CFLAGS) -o zeroify $(OBJS) $(LIBS) $(LDFLAGS)
	mv zeroify$(BIN_POSTFIX) $(BINDIR)
