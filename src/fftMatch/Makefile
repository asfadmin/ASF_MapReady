######################################################################
# Makefile for 		:  fftMatch.c 
# Module Author 	:  Orion Lawlor
# Module Version	:  1.0
######################################################################

include ../../make_support/system_rules

LIBS =  $(LIBDIR)/asf_fft.a \
	$(LIBDIR)/asf_las.a \
	$(LIBDIR)/asf.a 
LIBC = $(LIBS) -lm 

OBJLIB =  fftMatch.o

all: fftMatch clean

fftMatch: $(OBJLIB) $(LIBS)
	$(CC) $(CFLAGS) -o fftMatch $(OBJLIB) $(LIBC)
	mv fftMatch$(BIN_POSTFIX) $(BINDIR)
	cp fftMatch.man ../../man/cat1/fftMatch.1

clean:
	rm *.o
