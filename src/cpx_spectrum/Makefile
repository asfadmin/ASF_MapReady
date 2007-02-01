# Makefile for		: cpx_spectrum
# Module Author		: Mark Ayers

include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf_fft.a \
	$(LIBDIR)/asf.a \
	$(GSL_LIBS) \
	$(PROJ_LIBS) \
	-lm

CFLAGS += \
	$(GSL_CFLAGS) \
	$(PROJ_CFLAGS)

OBJS  = cpx_spectrum.o \
	cfft1d_risc.o

cpx_spectrum: $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o cpx_spectrum $(OBJS) $(LIBS)
	mv cpx_spectrum $(BIN_POSTFIX) $(BINDIR)
	rm *.o
	cp *.1 ../../man/cat1

