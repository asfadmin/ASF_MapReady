C Alaska SAR Processor (ASP) %W% %E% %U%
	subroutine mkchirp(data,prf,fd,fdot,npulses,nweight)
c/*	subroutine mkchirp(data,prf,fd,fdot,npulses,nweight) --------
c
c	Author R.E. Carande
c
c   This subroutine makes a complex chirp using the following input parameters:
c
c	Input:	prf = complex sampling rate of chirp(hz)
c		fd = center frequency of chirp (hz)
c		fdot = chirp rate (hz/s)
c		npulses = number of samples in chirp
c		nweight = sinc**2 weighing option 0=off  1 = on
c
c   	Returned:	data is complex data line that is returned
c		 containing the chirp starting at the first sample.
c
c   nweight = 0   flat weighting
c	    = 1   sinc2 (3db-3db) weighting
c
c*/
	complex data(1)
	real weight(16384)
	data pi /3.14159/,x3db /1.38935/
c
c   set weight coeficients
c
	xstep=2.*x3db/npulses
	do 100 i=1,npulses   
	if (nweight .eq. 1) then
          x=(i-1)*xstep -x3db 
   	  if (x .eq. 0) then
		weight(i) = 1.
		goto 100
	  endif                 
	  weight(i)=(sin(x)/x)**2
	else
       	  weight(i)= 1.
	endif
100	continue
	ts=-npulses/prf/2.
	do i=1,npulses
	  t=ts+(i-1)/prf
	  phi=2*pi*(fd*t + .5*fdot*t*t)
	  data(i)= cmplx (weight(i)*cos(phi),weight(i)*sin(phi))
	end do 
	return
	end
