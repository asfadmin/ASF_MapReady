######################################################################
# Makefile for 		:  aisp.c 
# Module Author 	:  Tom Logan
# Module Version	:  1.0
######################################################################

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf_fft.a \
	$(LIBDIR)/asf_odl.a
LIBC = $(LIBS) -lm 

OBJLIB = aisp_libs.o \
	 parse_cla.o \
	 cfft1d_risc.o \
	 estdop.o \
	 fft_dop.o \
	 read_signal.o

SPECLIB = specan.o \
	specan_ml.o \
	specan_patch.o \
	specan_file.o \
#	quicklook.o \
#	my_mpi.o

ASPLIB = patch.o \
	 aisp_setup.o \
	 rciq.o rmpatch.o acpatch.o calibration.o

all: aisp calc_deltas spectra estavedop clean # quicklook

aisp:	$(ASPLIB) $(OBJLIB) aisp.o $(LIBS)
	$(CC) $(CFLAGS) -o aisp aisp.o $(ASPLIB) $(OBJLIB) $(LIBC)
	mv aisp$(BIN_POSTFIX) $(BINDIR)
	cp aisp.man ../../man/cat1/aisp.1 

calc_deltas: calc_deltas.o yaxb.o $(LIBS)
	$(CC) $(CFLAGS) calc_deltas.o -o calc_deltas yaxb.o $(LIBC)
	mv calc_deltas$(BIN_POSTFIX) $(BINDIR)
	cp calc_deltas.man ../../man/cat1/calc_deltas.1

estavedop: estavedop.o $(OBJLIB)
	$(CC) $(CFLAGS) estavedop.o $(OBJLIB) -o estavedop $(LIBC)
	mv estavedop$(BIN_POSTFIX) $(BINDIR)
	cp estavedop.man ../../man/cat1/estavedop.1

#quicklook: $(SPECLIB) $(OBJLIB)
#	$(CC) $(CFLAGS) $(SPECLIB) $(OBJLIB) -o quicklook $(LIBC)
#	mv quicklook$(BIN_POSTFIX) $(BINDIR)
#	cp quicklook.man ../../man/cat1/quicklook.1
#
#quicklook_mpi: $(SPECLIB) $(OBJLIB)
#	$(CC) $(CFLAGS) $(SPECLIB) $(OBJLIB) -o quicklook_mpi $(LIBC) -lmpi
#	mv quicklook_mpi$(BIN_POSTFIX) $(BINDIR)
#	cp quicklook.man ../../man/cat1/quicklook.1

spectra: spectra.o $(OBJLIB)
	$(CC) $(CFLAGS) spectra.o $(OBJLIB) -o spectra $(LIBC)
	mv spectra$(BIN_POSTFIX) $(BINDIR)
	cp spectra.man ../../man/cat1/spectra.1

# Note: signal2amp is a tiny program which generates
# a (very blurry!) image of raw signal data-- it's useful
# for testing the signal-data reading routines.
#  Since it is only useful for development, we don't ship it.

signal2amp: signal2amp.o $(OBJLIB)
	$(CC) $(CFLAGS) signal2amp.o $(OBJLIB) -o signal2amp $(LIBC)
	mv signal2amp$(BIN_POSTFIX) $(BINDIR)

sig_sum: sig_sum.o $(OBJLIB)
	$(CC) $(CFLAGS) sig_sum.o $(OBJLIB) -o sig_sum $(LIBC)
	mv sig_sum$(BIN_POSTFIX) $(BINDIR)

sig_hist: sig_hist.o $(OBJLIB)
	$(CC) $(CFLAGS) sig_hist.o $(OBJLIB) -o sig_hist $(LIBC)
	mv sig_hist$(BIN_POSTFIX) $(BINDIR)

chirp_spectra: chirp_spectra.o $(OBJLIB)
	$(CC) $(CFLAGS) chirp_spectra.o $(OBJLIB) -o chirp_spectra $(LIBC)
	mv chirp_spectra$(BIN_POSTFIX) $(BINDIR)

dop_prf: $(ASPLIB) $(OBJLIB) dop_prf.c $(LIBS)
	$(CC) $(CFLAGS) -o dop_prf dop_prf.c $(ASPLIB) $(OBJLIB) $(LIBC)
	mv dop_prf$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f core *.o
