# Makefile for 	      :  libifm.a 
# Module Author       :  Mike Shindle  
# Module Version      :  2.0

include ../../make_support/system_rules

OBJS = alert.o \
       bvector.o \
       c2p.o \
       cabs.o \
       cadd.o \
       cconj.o \
       cmul.o \
       cphase.o \
       cpxmatrix.o \
       cpxvector.o \
       csdiv.o \
       csmul.o \
       czero.o \
       dmatrix.o \
       dvector.o \
       dvmag.o \
       fcpvec.o \
       fft2d.o \
       fileExist.o \
       fileNumLines.o \
       fourn.o \
       get_mean.o \
       get_meand.o \
       imatrix.o \
       ivector.o \
       mat_mult.o \
       matrix.o \
       minv1.o \
       mxv1.o \
       nrerror.o \
       os2d.o \
       oversamp2dCpx.o \
       print.o \
       svdcmp.o \
       vector.o \
       vmag.o \
       zeroPad.o \
       zeroPad2d.o \
       checkDataType.o \
       create_name.o \
       readMatrix.o \
       readVector.o \
       writeAsciiVector.o \
       writeVector.o \
       Exit.o 

IOBJS = imsl_c_smul.o \
	imos2d.o \
        wks_allocate.o 
        
all: libifm.a
	rm *.o

libifm.a: $(OBJS)
	ar rv libifm.a $(OBJS)
	$(RANLIB) libifm.a
	mv libifm.a $(LIBDIR)
