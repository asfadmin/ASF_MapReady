# Makefile for		: trim_slc
# Module Author		: Orion Lawlor

include ../../make_support/system_rules

LIBS = $(LIBDIR)/asf_meta.a $(LIBDIR)/asf_las.a $(LIBDIR)/asf.a -lm
OBJS=trim_slc.o

trim_slc: $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o trim_slc $(OBJS) $(LIBS)
	mv trim_slc$(BIN_POSTFIX) $(BINDIR)
	rm *.o
	cp *.1 ../../man/cat1
	
