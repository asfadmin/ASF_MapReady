# Makefile for		: fico
# Module Author		: Rob Fatland, Mike Shindle, Tom Logan, Orion Lawlor & Mark Ayers

include ../../make_support/system_rules

LIBS =  $(LIBDIR)/asf_las.a \
	$(LIBDIR)/asf.a \
	$(LIBDIR)/libifm.a \
	$(LIBDIR)/asf_fft.a -lm

OBJS=	fft_corr.o \
	coherence.o \
	fico.o

fico_new: $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o fico $(OBJS) $(LIBS)
	rm *.o
	mv fico $(BIN_POSTFIX) $(BINDIR)
	cp fico.1 ../../man/cat1
	
