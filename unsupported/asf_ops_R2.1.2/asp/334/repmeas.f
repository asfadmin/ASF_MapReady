C Alaska SAR Processor (ASP) %W% %E% %U%
        subroutine repmeas (replica,tf,npts,gain,bias,dcbias,
     .phase,spikes,nspike3sig,peak,peakoffset,x3db,pslr,xislr,istat)
c/*     subroutine repmeas (replica,tf,npts,gain,bias,dcbias, ------------------
c    .phase,spikes,nspike3sig,peak,peakoffset,x3db,pslr,xislr,istat)
c
c
c       Author   Richard E. Carande      January 1989.
c
c       modified by quyen dinh nguyen
c       2/26/90: do not use the VA array processor for FFT routine
c       5/10/90: different ways to calculate the phase
c       7/31/90: return the phase value of corss-correlation phase
c       8/12/91: add 90 degrees to return phase value (other
c                program requires the phase around 90.0)
c
c	modified by Cynthia Wong
c	3/30/95: Eliminate all masscomp plot routines
c
c   This subroutine makes the following measure on the replica 
c   presented to it:
c
c               1) Absolute I and Q channel Gains
c               2) bias in I and Q channels
c               3) DC bias of replica (frequency offset)
c               4) Phase between I and Q channels
c               5) Location of "spikes" in frequency domain of replica
c               6) Correlation Response with transfer function including:
c			a) peak power
c			b) error in peak position  
c                       c) 3dB width of peak
c                       d) PSLR
c                       e) ISLR
c
c  NOTE: 1) THIS ROUTINE REQUIRES THE VA ARRAY PROCESSOR
c        2) THIS ROUTINE ASSUMES REPLICA IS A POINTER TO A 2K CHARACTER*1
c           VECTOR OF 1024 8I/8Q POINTS, EVEN THOUGHT THE ACTUAL REPLICA
C           IS LESS THAN 1024 IN LENGTH AND THE DATA IS ONLY 6 BIT. 
C
c       INPUT:  replica character*1  Each byte represents a sample (i or q)
c                               of the chirp replica taken from the E-ERS-1
c                               header.  6 replica bits per sample mapped
c                               to 6 lsbs of byte.  Expect 2k bytes
c               tf      integer*2   (2048 x 2 elements)
c                               tf is the transfer function the data
c                               is to be correlated with, and is used by this 
c                               routine to calculate the correlation response.
c                               tf in standard 16I/16Q twos compliment format,
c                               i.e. dimension is tf(4096).
c               npts    integer*4       Number of complex samples in replica
c		istat   integer*4  = 0 nonverbose mode - no plots, otherwise, 
c				verbose mode assumed
c       - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c       RETURN: gain    real*4  dimensioned (2).  Gain of I and Q channels 
c                               respectively.  Simply average of the squares
c                               of the I and Q channel replica samples.
c               bias    real*4  Bias of replica in time domain.  Dimensioned(2)
c                               for I and Q channel respectively.  Units are
c                               in ADC quantization units.
c               dcbias  integer*4  D.C. bias of replica.  D.C. bias defined as 
c                               the frequency that half the power is above, and
c                               half the power is below.  Units are frequency
c                               bins: -1024 to 1023. 
c				dcbias is used to pass in the bias to
c				convert replica to complex numbers. 
c				This number is different for satellites.
c				E-ERS-1 = 32, J-ERS-1 = 28.
c               phase   real*4  Relative phase between I and Q channel in deg.
c				NOTE phase is assumed to be near 90 degrees.
c				Approximation of small deviation is used, so
c				if deviation is large, value returned not 
c				accurate.
c               spikes real*4  array dimensioned 2048.  This array contains
c				the number of standard deviations the particular
c				bin's power is away from the average of the 
c				spectrum's.  This array may be used to determine
c				if a bin has excess power in it (a spike) and 
c				should be zeroed.		
c		nspike3sig int*4 ??? Ask Richard ???
c		peak	real*4	value of power at peak of interpolated response
c		peakoffset real*4	offset of peak from expected position
c				in units of uninterpolated bins
c               x3db    real*4  3dB power width of peak of correlation between
c                               replica and tf.  Units are time bins
c               pslr    real*4  Relative level of first side lobe to main lobe
c                               of correlation.  Units are dB power
c               xislr   real*4  Relative level of power in Main lobe of 
c                               correlation to sidelobes.  Units are dB power
c               istat   interger*4      return status = 0 ok,  < 0 = error.
c*/
        character*1     ans, replica(1)      !pointer to replica
	logical verbose
        integer*2       tf(1)           !pointer to transfer function (4k)
        real*4          gain(2),bias(2),phase,x3db,pslr,xislr,plot(2048)
	real*4  	spikes(1)
        integer*4       npts,dcbias,nspike3sig
        complex         rep(2048),t(2048),repfft(2048),cor(8192),
     .big(8192)
c 8/12/91: modified to increase the look up table according to
c          the bigfft.f routine required.
	real*4		coeftable(8192+1)
        real*4          r_norm,q_norm,xphase_acc
        real*4          pj,sum_iq,sum_i,sum_q,sum_i2,sum_q2
        real*4          Zj,piq,yphase

        data pi /3.14159/       
c
c  see if verbose mode
c
	if (istat.ne.0) then
		verbose = .true.
	else
		verbose = .false.
	end if
	istat=0
c
c   Convert input replica and tf to complex vectors
c
	if(verbose)write(6,*)'converting tf to complex'
        do i=1,2048
        rep(i)=(0.,0.)
        t(i)=cmplx(twos2fp(tf( (i-1)*2 +1)) , twos2fp(tf( i*2 )) )
        end do
c
c offset replica  by 1024 points for clear correlation results
c also subtract out known bias of "16" on raw data replica
c
	if(verbose)write(6,*)'converting replica to complex'
        do i=1,npts
        rep(i+1024)=cmplx(float(ichar(replica( (i-1)*2 +1)) -dcbias),
     . float(ichar(replica(i*2))-dcbias)  )
        end do  
c
c   calculate:   Gain
c                bias
c                phase
c
	if(verbose) write(6,*)'calculating bias,gain and phase'
	bias(1)=0.
	bias(2)=0.
        gain(1)=0.
        gain(2)=0.
	phase = 0.
        n_phase = 0
        xphase_acc = 0.
        sum_iq = 0.0
        sum_i = 0.0
        sum_q = 0.0
        sum_i2 = 0.0
        sum_q2 = 0.0
        do i=1025,1024+npts
          r=real(rep(i))
          q=aimag(rep(i))
          gain(1)=gain(1) + r**2
          gain(2)=gain(2) + q**2
          bias(1)=bias(1) + r
          bias(2)=bias(2) + q
	  rn=r/32.  !normalize to 1
	  qn=q/32.  !normalize to 1
          if(.not.( r.ge.31 .or. r.le.-32 .or. q.ge.31 .or.
     1              q.le.-32 .or.(r.eq.0.and.q.eq.0.)) )then
	     if (rn.eq. 0) then
	     phs = abs(asin(qn))
	     else
	     phs = pi/2. +  sign(1.,q)*(abs(sin(acos(rn))) - abs(qn))/rn 
	     end if
	     phs= 180./pi*phs  !convert to degrees
	     phase = phase + phs 
             n_phase = n_phase+1
             r_norm = r/sqrt(r*r+q*q)
             q_norm = q/sqrt(r*r+q*q)
             xphase_acc  = xphase_acc + asin(q_norm) - acos(r_norm)
             sum_iq = sum_iq + r*q
             sum_i  = sum_i + r
             sum_q  = sum_q + q
             sum_i2 = sum_i2 + r*r
             sum_q2 = sum_q2 + q*q

          end if
c	write(6,*)'I = ',r,'  Q = ',q,'  PHASE =',phs
        end do
        gain(1)=gain(1)/npts
        gain(2)=gain(2)/npts
        bias(1)=bias(1)/npts
        bias(2)=bias(2)/npts
        phase=phase/n_phase
        xphase_acc = xphase_acc/n_phase + 90.0
        pj = (n_phase*sum_iq - sum_i*sum_q)/
     1       sqrt((n_phase*sum_i2 - sum_i*sum_i)*
     1            (n_phase*sum_q2 - sum_q*sum_q))
        Zj = 0.5*log((1.0+pj)/(1-pj))
        piq = tanh(Zj)
        yphase = asin(piq)*180.0/3.14159265 + 90.0

C OUTPUT RESULTS SO FAR.
cif (verbose) then
        print*,' the phase measured by sin(acos(rn))- abs(qn)/rn is',
     1       phase,' deg.'
        print*,' the phase measured by asin(Imag) - acos(Imag) is',
     1      xphase_acc,' deg.'
        print*,' the cross-correlation phase is',yphase,' deg.'
cend if

C LET RETURN THE CROSS-CORRELATION PHASE VALUE
        PHASE = yphase

cif (verbose) then
        write(6,*)'GAINS: ', GAIN
        WRITE(6,*)'BIAS : ',BIAS
        WRITE(6,*)'PHASE: ',PHASE
cend if
c
c   get VA array processor
c
	loglen = int(log(2048.)/log(2.0) + .5)
C
C   TAKE FFT OF REPLICA
c	(use mapbigfftva for simplicity - not speed)
c       (use Quyen's CFFTX routine for simplicity)
C
	call bigfft(rep,coeftable,repfft,loglen,1,1)
c
c   plot fft result:
c
	do i=1,2048
	repfft(i) = repfft(i) / 2048.   !scale by 1/n
	plot(i)=cabs(repfft(i))**2
	end do

C
C   CALCULATE DC BIAS
C
	power=0.
	power2=0.
	if (verbose) write(6,*)'calculating dc bias'
	pmin=1.e11
	do i=1,2048  !sum up power in replica fft
		p=cabs(repfft(i))
		if (p .le. pmin) then
			pmin=p
			imin=i
		endif
		power=p**2 + power
	end do
	if (verbose) then
	write(6,*)'     Total power in spectrum = ',power
	write(6,*)'	minimum bin is bin : ',imin
	write(6,*)'     looking for half power bin'
	end if !verbose
	powavg=power/2048.   !average power per bin
	halfpow = power/2.
	do i=1,2048  !find half power bin
	  	index=i+imin  !start at minumum (outside b.w.)
		if (index.gt.2048) index=index-2048 
		power2=cabs(repfft(index))**2 + power2
		if (power2 .ge. halfpow) goto 500
	end do
	write(6,*)'repmeas:  HALF POWER POINT NEVER FOUND'
	istat=-1
	return
500	dcbias= index - 1   ! no DC bias if dcbias=0
	if (verbose)write(6,*)'      DC BIAS FOUND = ',dcbias
	
C
C   LOOK FOR FREQUENCY SPIKES
C
	if (verbose) write(6,*)'Looking for freq spikes:'
	sum2=0.
	do i=1,2048  ! calculate standard deviation
		sum2=(cabs(repfft(i))**2 - powavg)**2 + sum2
 	end do
	sigma=sqrt(sum2/2048)
	if (verbose)write(6,*)'Average power in replica = ',powavg
	if (verbose)write(6,*)'      standard deviation = ',sigma
	nspike3sig = 0
	do i=1,2048  ! look for spikes
		spikes(i)=cabs(repfft(i))**2/sigma
		if (spikes(i) .gt. 5.) then !count as big spike
			nspike3sig=nspike3sig+1
			if(verbose) write(6,*)'5sigma spike bin =',
     .i,' val= ',spikes(i)
		end if
	end do
C
C   MULTIPLY BY TRANSFER FUNCTION, PAD by 4x AND INVERSE FFT, assume dcbias=0
C
	do i=1,8192  !clear vector big
	big(i) =cmplx(0.0,0.0)
	end do
	do i=1,2048
		index=i
		if (i.gt.1024)index=i+6144
		big(index) = repfft(i) * t(i)  
	end do
	loglen=loglen+2   !interpolate by 4x.
	call bigfft(big,coeftable,cor,loglen,-1,1)
C
C   FIND PEAK AND MEASURE RESPONSE.
C
	cmax=0.
	do i=1,8192	! find peak
	if (cabs(cor(i)) .gt. cmax) then
		imax=i
		cmax=cabs(cor(i))
	endif
	end do
	if (verbose) then
	write(6,*)'max absolute value of correlation = ',cmax
	write(6,*)'maximum in correlation at:', imax
	end if !verbose
	peak=cmax**2
	peakoffset = float(imax - 4096)/4.
c
c   plot 2k near peak
c
	x1=imax-1023
	x2=imax+1024
	do i=1,2048
	x=cabs(cor(i+imax-1024))
	coeftable(i)=x**2  !convenient for getstat routine
	if (x.eq.0.) then
		plot(i) = -500.
	else
		plot(i)=20.*log10(x/cmax)  !dB Power
	end if
	end do

	call getstat(coeftable,1024,x3db,pslr,xislr,1000,2048)
	x3db=x3db/4.
	if (x3db.le.0) then
		write(6,*)'repmeas: Problem measuring peak width'
		istat=-2
	else
		istat=0
	end if
	if (verbose) then
	write(6,*)'POINT TARGET STATISTICS:'
	write(6,*)'3dB ',x3db,'  uninterpolated bins'
	write(6,*)'PSLR',pslr,'  dB'
	write(6,*)'ISLR',xislr,'  dB'
	end if !verbose
	return
        END     
