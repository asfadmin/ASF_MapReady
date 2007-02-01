# Makefile for		: cpx_filter
# Module Author		: Mark Ayers

include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/asf_fft.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(GSL_LIBS) \
	$(PROJ_LIBS) \
	-lm

CFLAGS += \
	$(GSL_CFLAGS) \
	$(PROJ_CFLAGS)

CPXOBJS = cpx_filter.o \
          cfft1d_risc.o

all:  bins clean

bins: cpx_filter gen_filt_params
	cp cpx_autofilter $(BINDIR)
	-chmod 775 $(BINDIR)/cpx_autofilter
	cp cpx_autofilter.1 ../../man/cat1

cpx_filter: $(CPXOBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o cpx_filter $(CPXOBJS) $(LIBS)
	mv cpx_filter$(BIN_POSTFIX) $(BINDIR)
	cp cpx_filter.1 ../../man/cat1

gen_filt_params: gen_filt_params.o
	$(CC) $(CFLAGS) $(LFLAGS) -o gen_filt_params gen_filt_params.o $(LIBS)
	mv gen_filt_params$(BIN_POSTFIX) $(BINDIR)
	cp gen_filt_params.1 ../../man/cat1

clean:
	rm -f *.o core *~

