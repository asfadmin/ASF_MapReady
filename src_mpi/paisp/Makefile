# Makefile for paisp

include ../../make_support/system_rules


CFLAGS := $(CFLAGS) -I/usr/local/mpi/include -DUSE_STDARG -DHAVE_STDLIB_H=1 \
	-DHAVE_STRING_H=1 -DHAVE_UNISTD_H=1 -DHAVE_STDARG_H=1 \
	-DUSE_STDARG=1 -DMALLOC_RET_VOID=1

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_fft.a \
 	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf_odl.a

LIBC  = $(LIBS) -L/usr/local/mpi/lib -lmpich -lnsl -lthread -lnsl -laio -lm 

OBJS  = paisp_libs.o \
	parse_cla.o \
	cfft1d_risc.o \
	estdop.o \
	fft_dop.o \
	read_signal.o \
	paisp_params.o

ASP_OBJS = patch.o \
	paisp_setup.o \
	calibration.o \
	rciq.o \
	rmpatch.o

all: paisp pestavedop clean

paisp:	$(ASP_OBJS) $(OBJS) paisp.o acpatch.o $(LIBS)
	$(CC) $(CFLAGS) -o paisp paisp.o acpatch.o $(ASP_OBJS) $(OBJS) $(LIBC) 
	mv paisp$(BIN_POSTFIX) $(BINDIR)
	cp paisp.man ../../man/cat1/paisp.1 

pestavedop: pestavedop.o $(OBJS)
	$(CC) $(CFLAGS) pestavedop.o $(OBJS) -o pestavedop $(LIBC)
	mv pestavedop$(BIN_POSTFIX) $(BINDIR)
	cp pestavedop.man ../../man/cat1/pestavedop.1

clean:
	-rm *.o

