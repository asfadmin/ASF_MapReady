	subroutine snrmeas (ispec,sigbw,csr,nspec,fcnoise,bwnoise,
     .fcsig,bwsig,snr,istat)
c/*	subroutine snrmeas (ispec,sigbw,csr,nspec,fcnoise,bwnoise, -------------
c    .fcsig,bwsig,snr,istat)
c
c   SUBROUTINE SNRMEAS.   AUTHOR R.E.CARANDE    JAN 89
c
c	modified june 19 89 to input spectrum in floating point (complex) form
c       modified december 20 1989 to input "power" spectrum in floating
c                                 point( real format). In addition,we
c                                 take out the plot option.
c
c   This subroutine measures the signal to noise ratio of the input spectrum
c   by averaging power over the specified noise and signal frequency regions.
c   
c   This subroutine assumes input center frequencies are between
c   0 Mhz and csr.  Ok if bandwidth extends beyond the limits when added
c   to the center frequencies
c
c   NOTE:   EVEN THOUGH THIS ROUTINE SPECIFIES FREQUENCY UNITS TO BE
C	    MHz, THIS IS NOT REQUIRED.  ALL THAT IS REQUIRED IS THAT
C	    ALL FREQUENCY UNITS BE THE SAME: MHz or Hz or bins or 
c	    grapefruits or ...
c
c       input:  ispec   Floating point vector containing the  
c                       power spectrum of the data. Data is assumed to 
c                       be power detected (i.e., I^2 + Q^2). Note that
c                       the data can be average of many lines 
c                      
c		sigbw	bandwidth of the signal. (Chirp b.w) (Mhz)(real)
c		csr	sampling rate of the signal (Mhz)(real)
c		nspec	number of float numbers  in ispec.  This is
c			a power of 2, equal to the FFT size used.(integer*4)
c		fcnoise Center frequency of noise location (MHz)(real)
c		bwnoise B.W. of measuring location of noise (MHz)(real)
c		fcsig 	Center frequency of signal location (MHz)(real)
c		bwsig	B.W. of measuring location of signal (MHz)(real)
c		istat  integer*4  = 1 verbose, =0 not verbose
c	
c	output:	snr	Signal to noise ratio as measured using above
c			intervals.  Units are dB power. (REAL*4)
c		istat   = 0 Successful
c			= -1 undefined SNR
c			= -2 No Power found in specified noise bandwidth
c*/
#include "libmp.h"
	real    ispec(nspec)
	real*4 snr,one,plot(8192),freq(8192),xs(4),pn(8192)
	real*4 ps(8192),spec(16384)
	logical verbose
	data one/1.0/
      character*80 sccsid
      data sccsid /'@(#)snrmeas.f	1.4 96/04/09 22:51:58\0'/
	if (istat .eq. 1) then
		verbose = .true.
	else
		verbose = .false.
	end if
c
c   move ispec (real) to spec(real)
c
	do i=1,nspec
	spec(i) = ispec(i)
	end do
c
c   calculate corresponding bin numbers, make sure center bins are odd.
c
	freqstep = csr/nspec ! Mhz/bin
	nbinoise =int( bwnoise/freqstep+.5) ! number of bins noise region
	nbinsig = int(bwsig/freqstep+.5) ! number of bins signal region
	nfcnoise =int(fcnoise/freqstep+.5)+1 ! center bin of noise region
	nfcsig = int(fcsig/freqstep + .5)+1  ! center bin of signal region
	if (verbose) then
		write(6,*)' freqstep,nbinoise,nbinsig,nfcnoise,nfcsig'
		write(6,*)freqstep,nbinoise,nbinsig,nfcnoise,nfcsig
	end if
c
c   calculate noise power
c
	pownoise = 0.0	
	npts2=nspec
	do i = 1,nbinoise
	ibin=mod(nfcnoise - nbinoise/2 + i -1 ,npts2)
	if (ibin .lt. 0) ibin = ibin + npts2
cif (verbose)write(6,*)'noise bin: ',ibin,ispec(ibin),pownoise
	pownoise = pownoise + spec(ibin+1)
	end do 
	if (pownoise .eq. 0.0 ) then 
		istat = -2
		write(6,*)'SNRMEAS: no power detected in noise bandwidth.'
		return
	end if
	pownoise = pownoise/nbinoise  !scale to pow/bin
c
c   calcualate signal power
c
	powsig=0.0	
	do i = 1,nbinsig
	ibin=mod(nfcsig - nbinsig/2 +i - 1,npts2)
	if (ibin.lt.0) ibin = ibin+npts2
cif (verbose)write(6,*)'signal bin: ',ibin,ispec(ibin),powsig
	powsig = powsig + spec(ibin+1)
	end do	
	powsig = powsig/nbinsig   !scale to pow/bin
	if(verbose) write(6,*)'calculating SNR:'
c
c   calculate SNR 
c
        if(pownoise.eq.0.0) pownoise=1.0
	eta = (powsig/pownoise)
	if (verbose)write(6,*)'eta = ',eta
 	snr = (eta - one)
	if (verbose) write(6,*)'SNR before dB conversion ',snr
	if (snr .le. 0.) then
		istat = -1
		write(6,*)'SNR:  Undefined signal to noise ratio'
		return
	end if
	snr = 10.*alog10(snr)
	if (verbose) write(6,*)'SNR in dB power ',snr
	istat=0
	return
	end
