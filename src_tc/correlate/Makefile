# Makefile for 		:  correlate
# Module Author 	:  Tom Logan & Mike Shindle

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libgsl.a \
	-lm

OBJS  = correlate.o

correlate:  $(OBJS)
	$(CC) $(CFLAGS) -o correlate $(OBJS) $(LIBS) $(LDFLAGS)
	mv correlate$(BIN_POSTFIX) $(BINDIR)
	cp correlate.man ../../man/cat1/correlate.1
