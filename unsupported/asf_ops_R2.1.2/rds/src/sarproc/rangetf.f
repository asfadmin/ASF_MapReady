	subroutine rangetf (a,b,c,nrange,csr,tau,h,tf,avgpow_rtf,
     .powmax_rtf,powloss_rtf,istat)
c/*	subroutine rangetf (a,b,c,nrange,csr,tau,h,tf,avgpow_rtf, --------------
c    .powmax_rtf,powloss_rtf,istat)
c
c	AUTHOR: RICHARD E. CARANDE     DECEMBER 1988.
c
c	NOTE:  THIS PROGAM USES THE VA ARRAY PROCESSOR
c		AND THE GRAPHICAL DISPLAY SOFTWARE
c		LIBRARIES:  -lva -lmp
c
c       modified by quyen dinh nguyen
c       2/26/90: do not use the VA for FFT routine
c
c   This subroutine generates the range transfer function (weighted)
c   given the following inputs:
c
c	INPUT:	a	real*4	coefficient of quadratic phase 
c				for secondary RMC correction (microsec**2)
c		b	real*4	linear phase coefficient of chirp(Hz)
c		c 	real*4	quadratic phase coefficient of chirp (Hz/s)
c		nrange	integer*4 number of points in range FFT (8k=max)
c		csr	real*4	complex sampling rate of data (MHz)
c		tau	real*4  pulse length (micro seconds)
c		h	real*4  cosine squared pedestal
c		istat	integer*4 as input, this variable determines if
c				this program is verbose (=1) or not (=0)
c		
c	OUTPUT:	tf	complex  array corresponding to range transfer
c				function generated from input.  To be
c				used in ASP, must be converted to two's
c				compliment format.
c			avgpow_rtf = real*4 which is the average
c				    power (mag**2) in the vector
c			  	    before normalization.
c			powmax_rtf = real*4 square of the maximum
c				    value found in the t.f. before
c				    normalization,i.e. the square
c			  	    of the normalizing factor.        
c			powloss_rtf = real*4 = power loss due to
c				weighting. units = dB power with 
c				respect to no weighting, h=1.
c			istat integer*4 = return status:
c				= 0 : success
c				= -1 invalid fft lenght
c				= -2 FFT too short for chirp
c				= -3 Trouble allocating VA array processor mem.
c
c    	Error Return to address of last arguement if invalid input
c	or error detected. 
c
c	This subroutine uses the VA array processor.
c*/
#include "libmp.h"
	integer 	gls (SIZEOFGCA)
	integer*4 	nrange
	character*1	ans
	real*4		plot(8192),freq(8192)
	real*4		vec(8192)   !weighting vector
	real*4		a,b,c,bw
	complex 	tf(1),rref(8192),coefftable(8192)
	logical 	verbose
	data  		pi /3.141592654/
101	format (a)
      character*80 sccsid
      data sccsid /'@(#)rangetf.f	1.4 96/04/09 22:51:55\0'/
c   
c   initialize ap
c
	if (istat .eq. 1) then
		verbose = .true.
	else
		verbose = .false.
	end if
c
c   check if nrange valid
c
	loglen = int(log(float(nrange))/log(2.0) + .5)
	if (nrange .ne. 2**loglen.or. nrange .lt. 4 
     .			.or. nrange.gt. 8192) then
		write(6,*)'rangetf:  number of points in range FFT invalid'
		write(6,*)'          fft length input = ',nrange
		istat = -1
		return  
	end if 
c
c   calculate number of points in chirp
c
	npts=  int( csr * tau + .5)	
c
c   make sure at least 50% efficient correlation may be achieved with t.f.
c
	if (npts.gt. nrange/2) then
		write(6,*)'rangetf:  number of points in chirp is over half '
		write(6,*)'          the Range FFT length:'
		write(6,*)'          range fft length=',nrange
		write(6,*)'          points in chirp =',npts
		istat = -2
		return 
	end if
c
c   create chirp in time domain
c
	do i=1,npts
	    t= (i-1)/(csr*1.e6)- (tau*1.e-6)/2.
	    phi=2*pi*(b*t + .5*c*t*t)
	    rref(i) = cmplx(cos(phi),sin(phi))
	end do
	do i=npts+1,nrange
	    rref(i)=cmplx(0.,0.)
	end do
	if (verbose) then
	write(6,*)'rangetf:  Transfer Function Information'
	write(6,*)'- - - - - - - - - - - - - - - - - - - -'
	write(6,*)'number of points in time chirp = ',npts
	write(6,*)'       number of points in FFT = ',nrange
	write(6,*)'  valid points per correlation = ',nrange-npts+1
	write(6,*)'        time duration of chirp = ',tau,' microseconds'
	write(6,*)'Complex Sampling Rate of chirp = ',csr,' Mhz'
	write(6,*)'Bandwidth of Transfer Function = ',abs(c)*tau*1.e-12
     .,' Mhz'
	write(6,*)'        Time Bandwidth Product = ',abs(c)*tau*tau*
     .1.e-12
	if (a.eq.0.) then
		write(6,*)'No secondary Range Correction'
	else
		write(6,*)'Secondary Range correction coef = ',a,'
     .microsec**2'
	end if
	write(6,*)'- - - - - - - - - - - - - - - - - - - -'
	end if ! verbose

	
	call bigfft(rref,coefftable,tf,loglen,1,1)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c   calculate weighting vector and apply
c
	if (h .lt. 1) then  !weight
	bwmhz=abs(c)*tau*1.e-12	! bandwidth in Mhz
	bmhz=b*1.e-6	
	call mkwvec(vec,h,bwmhz,csr,bmhz,nrange)  !vec is re-calculated herE
c
c	multiply tf and weighting function
c
	totpow = 0. !total unweighted power
	do i=1,nrange
	totpow = totpow +(cabs(tf(i)))**2
	tf(i)=tf(i)*cmplx(vec(i),0.)
	end do
	end if  !weight
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  find maximum absolute element and calculate average power
c
	tfmax=0.
	avgpow_rtf = 0.
	do i=1,nrange
	avgpow_rtf= avgpow_rtf + (cabs(tf(i)))**2
	tfmag=cabs(tf(i))
	if (tfmag.gt.tfmax) tfmax=tfmag
	end do
	tfnorm=1./tfmax
c
c   calculate average power, max power bin and power loss due to weighting
c
	if (h .ne. 1) then
		powloss_rtf = 10.*log10(avgpow_rtf/totpow)  !in dB
	else
		powloss_rtf = 0.   !dB
	end if
	powmax_rtf = tfmax**2   
	avgpow_rtf = avgpow_rtf/nrange	
	if (verbose)  then
		write(6,*)'max pow in spectrum found = ',powmax_rtf
		write(6,*)'average power in spec = ',avgpow_rtf
		write(6,*)'power loss due to weighting = ',powloss_rtf,
     .'dB'
	end if !verbose
c
c   multiply by quadratic phase correction for secondary RMC.
c   also normalize by tfmax and take conjugate
c
	if (a.ne.0.0) then

		do i=1,nrange
		f=(i-1)*csr/nrange
		phi=2.*pi*amod(a*f*f,1.0)
		tf(i) = tf(i) * cmplx(cos(phi),sin(phi))
		end do	
	end if !secondary range compression
	do i=1,nrange
	tf(i)=conjg(tf(i))*cmplx(tfnorm,0.)
	end do
	istat = 0 !successful
	if (verbose) then  !plot
c
c   see if want to see plot
c
	write(6,*)'want to see plot of t.f. spectrum? [y]'
	read(5,101)ans
	if (ans .eq. 'n' .or. ans.eq. 'N') return
	do i=1,nrange
	freq(i)=(i-1)*csr/nrange
	plot(i)=(cabs(tf(i)))**2
	end do
	call mpinit (gls)
	call mpdevice(gls, "tek",2,0)
	call mplotsrcx(gls,1,nrange,0,freq,"F",1,1,NULL,NULL)
	call mplotsrcy(gls,1,nrange,0,plot,"F",1,1,NULL,NULL)
	call mptitle(gls,1,-1,-1,"Frequency (MHz)")
	call mptitle(gls,2,-1,-1,"Normalized Power")
	call mptitle(gls,4,-1,-1,"return to continue...")
	call mplot(gls,0,0,0)
	call mpend(gls)
	read(5,101)ans
	end if !verbose
	return
	end
