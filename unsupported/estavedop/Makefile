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

ASPLIB = patch.o \
	 aisp_setup.o \
	 rciq.o rmpatch.o acpatch.o calibration.o

all: estavedop clean

estavedop: estavedop.o $(OBJLIB)
	$(CC) $(CFLAGS) estavedop.o $(OBJLIB) -o estavedop $(LIBC)
	mv estavedop$(BIN_POSTFIX) $(BINDIR)
	cp estavedop.1 ../../man/cat1/estavedop.1

clean:
	rm -f core *.o
