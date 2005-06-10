######################################################################
# Makefile for 		:  fftMatch.c 
# Module Author 	:  Orion Lawlor
# Module Version	:  1.0
######################################################################

include ../../make_support/system_rules

LIBS =  $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libgsl.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/libproj.a \
	$(LIBDIR)/libasf_reporting.a \
	$(LIBDIR)/asf_fft.a 
LIBC = $(LIBS) -lm 

OBJLIB =  fftMatch.o

all: fftMatch clean

fftMatch: $(OBJLIB) $(LIBS)
	$(CC) $(CFLAGS) -o fftMatch $(OBJLIB) $(LIBC) $(LDFLAGS)
	mv fftMatch$(BIN_POSTFIX) $(BINDIR)
	cp fftMatch.man ../../man/cat1/fftMatch.1

clean:
	rm *.o
