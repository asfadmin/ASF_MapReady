# Makefile for 		:  correlate
# Module Author 	:  Tom Logan & Mike Shindle

include ../../make_support/system_rules

OLIBS = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a 
LIBS = $(OLIBS) -lm

correlate:  correlate.c $(OBJS) $(OLIBS)
	$(CC) $(CFLAGS) -o correlate correlate.c $(OBJS) $(LIBS)
	mv correlate$(BIN_POSTFIX) $(BINDIR)
	cp correlate.man ../../man/cat1/correlate.1
