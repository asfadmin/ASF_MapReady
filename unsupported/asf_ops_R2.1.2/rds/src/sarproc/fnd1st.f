	subroutine fnd1st (r_close,xlambda,prf,csr,fd,fdot,i1st,istat)
c/* subroutine fnd1st (r_close,xlambda,prf,csr,fd,fdot,i1st,istat) -----
c
c	SUBROUTINE FND1ST
C	R.E.CARANDE   JANUARY 1989.
c
c   This subroutine is used to determine the first range bin that will
c   produce a complete path in the range migration memory.
c
c	Input:  r_close	real*4	near range of data, determined by the 
c				delay between transmission and the start
c				of digitization of the radar pulses.
c				Units = meters
c		xlambda real*4	Radar wavelength in meters
c		prf	real*4	Pulse Repitition Frequency (Hz)
c		csr	real*4	Complex Sampling Rate of SAR (MHz)
c				used to determine slant range pixel spacing.
c		fd	real*4	Vector (3 elements) containing the Doppler
c				center frequency of range lines in Hz.
c				The 3 elements are the constant, linear
c				and quadratic coefficients in 
c				slant range units
c		fdot	real*4  Vector (3 elements) containing the Doppler rate
c				terms of range lines in Hz/sec
c				The 3 elements are the constant, linear
c				and quadratic coefficients in 
c				slant range units
c		istat	int*4	As input, indicates number of range bins
c				the Doppler terms fd and fdot are to be
c				used for.  Acceptable input is istat = 1 
c				for one look processing, and istat = 8 for
c				four look processing. (NOTE: THIS VARIABLE
c				IS ALSO A RETURN VARIABLE, THEREFORE IT IS
c				DESTROYED IN THIS SUBROUTINE.)
c		
c	RETURN	i1st 	int*4	The index (range bin) that will produce the
c				first range path that is entirely contained
c				in the data.
c		istat	int*4	Return Status = 0 successful 
c					      < 0 unsuccessful
c
c	ALGORITHM:  The range to a target with known Doppler characteristics
c		may be calculated using the following equation:
c
c		R(f) = R1 - xlambda/(4*fdot) * (f**2 - fd**2)
c
c		where f is the Doppler frequency, R1 is the slant range
c		at the center of the beam, fdot and fd are the Doppler 
c		parameters for the range bin.  The minimum range may be
c		determined by finding the range at the minimum absolute
c		value of frequency (f) in the azimuth bandwidth of the 
c		processor.  The minimum absolute frequency is either:
c			1) = 0, if  abs(fd) <= prf/2, or
c			2) = fd - sign(fd)*PRF/2 if abs(fd) > prf/2
c		The first set of range and Doppler parameters that
c		produce a minimum range that is >= R_close is the first
c		set that will produce a path entirely contained in the data.
c*/
	real*4	fd(3),fdot(3)  	!input Doppler coefficients.
	data c /2.99792458e8/	!speed of light in m/s
      character*80 sccsid
      data sccsid /'@(#)fnd1st.f	1.3 96/04/09 22:51:47\0'/
c
c   save input istat and check validity of value
c
	ibins = istat
	if (ibins .ne. 1 .and. ibins .ne.8 )then ! errror
		write(6,*)'fnd1st: ERROR- INCORRECT UPDATE FREQUENCY.'
		WRITE(6,*)'        ONLY 1 OR 8 VALID... value provided ='
     .,istat
		istat = -2
		return
	end if
c
c   calculate range pixel spacing:
c	
	r_pixel = c/(2.*csr*1e6)
cwrite(6,*)'range pixel size = ',r_pixel
c
c    start range pixel loop
c
	do i=1,100,ibins
c
c   calculate range, fd and fdot for range bin i
c
	r =r_close + (i-1)*r_pixel   ! this is range at beam center.
	fd0 = fd(1) + fd(2)*i + fd(3)*i*i  !fd at range =r
	fdot0 = fdot(1)+fdot(2)*i + fdot(3)*i*i   ! fdot at range = r
cwrite(6,*)'i,r,fd,fdot',i,r,fd0,fdot0
	if (fdot0 .ge. 0.0 ) then  !fdot should be less negative. 
		write(6,*)'fnd1st: ERROR- bad fdot calculated: .ge. 0.'
		write(6,*)'        Check coefficients:',fdot(1),fdot(2),
     .fdot(3)
		istat = -3
	end if 
c 
c   determine the closest range (rcandidate) of this line
c
	if (abs(fd0) .le. prf/2.) then
		fmin = 0.0   !goes through 0 Doppler
	else
		fmin= fd0 - (fd0/abs(fd0)) *prf/2.
	end if
	rcandidate =r -xlambda/(4.*fdot0)*(fmin**2 - fd0**2)
	if (rcandidate .ge. r_close) then  !entire path contained in data
		istat=0
		i1st = i
		return  !successfully
	end if  
c
c  try next range
c
	end do
c
c  if out of loop, no good path found...error:
c
	write(6,*)'fnd1st:  ERROR: NO PATH FITS WITH 100 RANGE CELLS'	
	istat = -1
	return  !unsuccessfully
	end
