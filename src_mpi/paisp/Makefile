######################################################################
# Makefile for 		:  aisp.c 
# Module Author 	:  Tom Logan
# Module Version	:  1.0
######################################################################

include ../../make_support/system_rules

LIBS =   $(LIBDIR)/asf_fft.a \
         $(LIBDIR)/asf.a \
 	 $(LIBDIR)/asf_meta.a \
	 $(LIBDIR)/asf_odl.a
LIBC =   $(LIBS) -lmpi -lm 

OBJLIB = paisp_libs.o \
	 parse_cla.o \
	 cfft1d_risc.o \
	 estdop.o \
	 fft_dop.o \
	 read_signal.o \
	 paisp_params.o

ASPLIB = patch.o \
	 paisp_setup.o \
	 calibration.o \
	 rciq.o \
	 rmpatch.o

all: paisp pestavedop clean

paisp:	$(ASPLIB) $(OBJLIB) paisp.o acpatch.o $(LIBS)
	$(CC) $(CFLAGS) -o paisp paisp.o acpatch.o $(ASPLIB) $(OBJLIB) $(LIBC) 
	mv paisp$(BIN_POSTFIX) $(BINDIR)
	cp paisp.man ../../man/cat1/paisp.1 

pestavedop: pestavedop.o $(OBJLIB)
	$(CC) $(CFLAGS) pestavedop.o $(OBJLIB) -o pestavedop $(LIBC)
	mv pestavedop$(BIN_POSTFIX) $(BINDIR)
	cp pestavedop.man ../../man/cat1/pestavedop.1

clean:
	-rm *.o

