# Makefile for		: cpx_filter
# Module Author		: Mark Ayers

include ../../make_support/system_rules

LIBS = $(LIBDIR)/asf_meta.a \
       $(LIBDIR)/asf_las.a \
       $(LIBDIR)/asf.a \
       $(LIBDIR)/asf_fft.a -lm

CPXOBJS = cpx_filter.o \
          cfft1d_risc.o

all:	cpx_filter \
	gen_filt_params
	rm *.o
	cp cpx_autofilter $(BIN_POSTFIX) $(BINDIR)
	-chmod 775 $(BINDIR)/cpx_autofilter
	cp cpx_autofilter.1 ../../man/cat1

cpx_filter: $(CPXOBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o cpx_filter $(CPXOBJS) $(LIBS)
	mv cpx_filter $(BIN_POSTFIX) $(BINDIR)
	cp *.1 ../../man/cat1

gen_filt_params: gen_filt_params.o
	$(CC) $(CFLAGS) $(LFLAGS) -o gen_filt_params gen_filt_params.o $(LIBS)
	mv gen_filt_params $(BIN_POSTFIX) $(BINDIR)
	cp *.1 ../../man/cat1

