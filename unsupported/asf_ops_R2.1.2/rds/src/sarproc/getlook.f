	function getlook (xsc,range,rprec,side,choice,a)
c/*	function getlook (xsc,range,rprec,side,choice,a) -------
c
c   Dec 87
c
c   added double precision aug 88
c
c   Subroutine to calculate the look angle from the s/c to the "touch point"
c   of the range vector with the Earth's surface, where the
c   Earth's surface is defined by an ellipsoid with an equatorial radius (re) 
c   and a polar radius (rp).  These radii are trasfered via the common block
c   labelled /earth/.                                                
c  
c   Input: xsc(3) = double precision x,y,z components of SAR position (m)
c         range  = magnitude (double precision) of slant range vector (m)
c         rprec  = precision to which iterational range matches input range (m)
c         side   = l or L for left looking, r or R for right looking (char*1)
c         choice (character*4) indicates the attitude assumption that is made:
c	       = "side"  indicates orbital side looking SAR.  
c	       = "0dop"  indicates steered to zero doppler.
c	       = "usea"  indicates to use the supplied squint angle a
c	   a = squint angle to use if "usea" is choice   
c
c    Output: function returns last value of gamma that represents the
c            look angle (as defined below) that produces a touch point vector
c            that is within the specifed range of the input value range.
c            Units are in radians.
c
c   Definition of angles:  Begin with SAR in perfect orbital allignment
c               (0 Yaw,Pitch and Roll).  gamma is Roll + SAR look angle.
c		After gamma rotation, a yaw of angle a is performed on 
c		the vector.  Forward yaw (toward vel. vector) is positive a.
c               The angle gamma is returned, the angle a is assumed as 
c               specified in choice.
c
c   Procedure:  The slant range vector "touch" point is calculated by 
c               iteratively varying the look angle gamma until the magnitude
c               of the resulting vector is within specified  limit, rprec,
c               of input arguement range.  The resulting look angle is
c               returned.  Maximum iterations are given by itmax (see data
c               statement. 
c 
c   Error Termination:  All premature termination is flaged by the return
c		of the nonsense value of pi/2 (90 deg) as getlook.  Reasons
c		for error termination are:  No convergence
c					    SAR orbit below Earth Surface
c					    Range Vector does not reach Earth
c					    Range Vector does not hit Earth
c			
c	Common Blocks:  /transform/  contains transform matrix s
c			/earth/ contains Earth constants
c                                         
c*/
	implicit double precision(a-h,o-z)
	double precision xsc(3),s(3,3),calcr
	character*4 choice 
	character*1 side
	common /transform/ s
	common /earth/ rp,re
	data pi/3.141592654/, itmax /1000/
      character*80 sccsid
      data sccsid /'@(#)getlook.f	1.3 96/04/09 22:51:49\0'/
c
c   Determine the squint angle a to assume.
c                       
	if (choice .eq. 'usea' .or. choice .eq. 'USEA') goto 500
	if (choice .eq. 'side' .or. choice .eq. 'SIDE') then
		a=0.
		goto 500
	end if
	if (choice .eq. '0dop' .or. choice .eq. '0DOP') then
c   must calculate the effective squint angle to give fd=0
		write (6,*)'    No Squint Model in function getlook'
		write(6,*) '    at this time.  Exiting function.'
        end if
c  look angle in y'z' plane, az angle a in z''x' plane
c  a > 0 for forward squint
c
c  First guess at a look angle   Simple spherical guess:
c                   
500  	rsc2=(xsc(1)*xsc(1)+xsc(2)*xsc(2)+xsc(3)*xsc(3))
	rsc=dsqrt(rsc2)
	if (range .lt.(rsc-rp)) then ! Range vector does not reach Earth
		write(6,*)'getlook: Range vector does not reach Earth'
		getlook=pi/2
		return 
	endif
	if (rsc .lt. rp) then ! Orbit below Earth surface
		write(6,*)'getlook: Sub-Earth Surface Orbit Error'  
		getlook=pi/2.
		return 
	endif	
	gamma= dacos((-rp*rp+rsc2+range*range)/(2.*range*rsc))  
c  LOOK ANGLE >0 FOR RIGHT LOOKING
	if (side .eq. 'l'.or. side .eq. 'L') gamma=-gamma
c   calcualate boresite unit vector in s/c frame   
	it=0
1000	rtest = calcr(xsc,gamma,a) 
	if (rtest .eq. 0) then ! error condition in function calcr
		write(6,*)'getlook: Intermediate Range vector does 
     .not hit Earth surface.'
	     	getlook=pi/2.
		return 
     	endif   
        deltar = range - rtest 
	if (abs(deltar) .lt. rprec) then  ! good gamma
		getlook = gamma 
c	    	write(6,*)'iterations for gamma = ',it+1,180./pi*getlook
c     .,deltar
                return    !  SUCCESSFUL RETURN.
	else  ! update gamma
		it = it + 1
		if (it .gt. itmax) then
  			getlook=pi/2. 
			write(6,*)'getlook:  No convergence after',
     .itmax,'iterations.'
			return 
		end if   
		sininc=(rsc/rp)*dsin(gamma) !sin of inci. angle (approx)
		taninc= sininc/dsqrt(1-sininc*sininc) ! tan of inci angle
		delgam=(deltar/(range*taninc)) ! update for look angle
c		write(6,*)'getlook: ',it,gamma*180./pi,delgam*180./pi,
c     .deltar
	      	gamma = gamma + delgam   
		goto 1000
	endif
	end
