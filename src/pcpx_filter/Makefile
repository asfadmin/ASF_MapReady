# Makefile for		: pcpx_filter and gen_filt_params
# Module Author		: Mark Ayers

include ../../make_support/system_rules

LIBS = $(LIBDIR)/asf.a \
       $(LIBDIR)/asf_meta.a \
       $(LIBDIR)/asf_fft.a \
       -lm -lmpi

CPXOBJS = pcpx_filter.o \
          cfft1d_risc.o

all:	pcpx_filter \
	gen_filt_params
	rm *.o
	cp pcpx_autofilter $(BIN_POSTFIX) $(BINDIR)     
	cp pcpx_autofilter.1 ../../man/cat1

pcpx_filter: $(CPXOBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o pcpx_filter $(CPXOBJS) $(LIBS)
	mv pcpx_filter $(BIN_POSTFIX) $(BINDIR)
	cp *.1 ../../man/cat1

gen_filt_params: gen_filt_params.o
	$(CC) $(CFLAGS) $(LFLAGS) -o gen_filt_params gen_filt_params.o $(LIBS)
	mv gen_filt_params $(BIN_POSTFIX) $(BINDIR)
	cp *.1 ../../man/cat1

