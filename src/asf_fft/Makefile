# Makefile for: fft.a
# Makefile author: Orion Lawlor
# Makefile date: 5/22/98
#

include ../../make_support/system_rules

OBJS =  dxpose.o \
	fft2d.o \
	fftlib.o \
	matlib.o \
	fftext.o 

asf_fft.a:	$(OBJS)
	ar rcv asf_fft.a $(OBJS)
	$(RANLIB) asf_fft.a
	mv asf_fft.a $(LIBDIR)
	echo "ASF FFT Library sucessfully built!"
	rm $(OBJS)

clean:
	-rm -f *.o ../fft.a
