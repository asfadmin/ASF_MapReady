	subroutine getloc_C(statevec,range,fd,re,rp,xlambda,
     .iyear,iday,ihour,min,sec,
     .targlat,targlon,targrad,istat)
c/*	subroutine getloc_C(statevec,range,fd,re,rp,xlambda, -------------------
c    .iyear,iday,ihour,min,sec,
c    .targlat,targlon,targrad,istat)
c
c
c   THIS ROUTINE COMBINES CALLS TO THE FOLLOWING: MAKES_CHI,GETLOC AND GHA.
c   C callable version  
c
c   Input:      statevec:  double precision x,y,z,xdot,ydot,zdot of s/c
c		   range:  Known slant range to target (double)
c		      fd:  Known Doppler frequency between s/c and stationary
c     			   target on Earth's (ellipsoidal) surface (double)
c		      re:  equatorial radius (double)(m)
c		      rp:  polar radius (double)(m)
c		 xlambda:  wavelength (double) (m)
c		   iyear:  year of statevec (integer)
c		    iday:  day of statevec (integer)
c		   ihour:  hour of statevec (integer)
c		     min:  minute of statevec (integer)
c		     sec:  second of statevec (float)
c   Output:      
c		 targlat:  geodetic latitude of target (double)
c		 targlon:  geocentric longitude+houranlge of target (double)
c		 targrad:  radius of ellipsoidal earth at target(double)       
c		   istat:  =0 success, = -1 error occured
c
c   UNITS:      Distance dimensions are all meters
c  	        Angle are all radians
c		Frequency are all Hz
c
c
c	NOTE: See getloc.f for complete comments
c
c	NOTE: Restricted by one statement for right looking sars...easily
c             modified.
c   
c   COMMON BLOCKS:   USED to pass to calling routines, not from/to main
c	                earth:   contains the polar (rp) and equatorial (re)
c				 radius of the Earth as given by calling
c				 program.               
c			  sar:   contains wavelenth of radar
c                   transform:   transform matrix as calculated by makes
c*/
	implicit double precision(a-h,o-z)
	double precision statevec(6),s(3,3),gha
	real sec 
	character*4 choice
	character*1 side
	common /earth/ rp_c,re_c
	common /sar/ xlambda_c
	common /transform/ s            
	data pi/3.141592654/
      character*80 sccsid
      data sccsid /'@(#)getloc_C.f	1.3 96/04/09 22:51:49\0'/
c                         
c  check for non-zero values
c
	if (rp.eq.0. .or. re.eq.0. .or. xlambda.eq.0. )
     .then
		write(6,*)'Null input parameter to subroutine getloc'
		write(6,*)'returning from subroutine with null location
     .values'
		targlat=0.             
		targphi=0.
		targrad=0.
		istat =-1
		return
	end if               
	xlambda_c=xlambda
	re_c = re
	rp_c = rp                                
	side='r'       ! looking toward right of flight path      
	choice='usea'  ! use the azimuth angle in the calculation
	targacc = 1.0    ! 1 meter accuracy
	aguess=0.0      !initial azimuth angle guess
c
c   make rotation vector "s"
c
	call makes_chi (statevec,s)
c
c   call getloc
c
	call getloc (statevec,range,fd,aguess,targacc,deltafd,targlat,
     .targphi,targrad,*1000)
c
c   calculate hour angle  in degrees
c
	ha = gha (iyear,iday,ihour,min,sec,istata)
	if (istata .lt. 0) then
		istat=-1
		return
	end if
	targlon = targphi*180./pi - ha
	targlat = datan(dtan(targlat)*(re/rp)**2)
	targlat = targlat * 180./pi
	istat = 0
	return
1000	istat = -1
	return
	end
