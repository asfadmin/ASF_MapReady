# Makefile for		: trim_slc
# Module Author		: Orion Lawlor

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(GSL_LIBS) \
	-lm

OBJS=trim_slc.o

trim_slc: $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o trim_slc $(OBJS) $(LIBS) $(LDFLAGS)
	mv trim_slc$(BIN_POSTFIX) $(BINDIR)
	rm *.o
	cp *.1 ../../man/cat1
	
