C Alaska SAR Processor (ASP) %W% %E% %U%
	subroutine getloc(statevec,range,fd,aguess,targacc,deltafd,targlat,
     .targphi,targrad,*)
c/*   subroutine getloc(statevec,range,fd,aguess,targacc,deltafd,targlat, -----
c    .targphi,targrad,*)
c
c   subroutine getloc   R.E. Carande   July 1988
c
c		aug 88 made double precsion
c		dec 88 add return label for errors
c
c   This subroutine iteratively determines the location of the intercept
c   of:    1) an ellipsoidal Earth surface
c          2) a range vector from an orbiting satellite
c      and 3) the known proper Doppler frequency
c                                                                     
c
c   Input:      statevec:  double precision 6 element x,y,z,xdot,ydot,zdot of s/c
c		   range:  Known slant range to target
c		      fd:  Known Doppler frequency between s/c and stationary
c     			   target on Earth's (ellipsoidal) surface
c		  aguess:  First guess for azimuth angle
c		 targacc:  Accuracy to which the iterated fd must predict
c  			   the target location.
c   Output:      deltafd:  difference between the iterated fd and the real
c		 	   fd  ("real fd" - fdguess)
c		 targlat:  geocentric latitude of target
c		 targphi:  geocentric longitude+houranlge of target
c		 targrad:  radius of ellipsoidal earth at target       
c		       *:  return label in error condition
c
c   UNITS:      Distance dimensions are all meters
c  	        Angles are all radians
c		Frequencies are all Hz
c
c
c   This is done by first assuming a satellite attitude, calculating a
c   trial Doppler frequency, comparing with real, computing error in attitude
c   and hence target position, and recalculating (iterating) a and fd.
c
c	NOTE: Since this subroutine may be called several times using the
c    	      same statevector, calculation of the transformation matrix, s,
c	      must be performed before calling this program.  It is passed
c	      via the common block labelled "transform."  For the same reason
c	      this routine returns an angle in inertial coordinates, so the
c	      hour angle must be added after calling this program to calc.
c	      the longitude of the target.
c
c	NOTE: Restricted by one statement for right looking sars...easily
c             modified.
c   
c   COMMON BLOCKS:   relative:   this contains the target statevector and
c				 relative vel. and accel. vectors as calculated
c 				 in the last call to getdop2.            
c	                earth:   contains the polar (rp) and equatorial (re)
c				 radius of the Earth as given by calling
c				 program.               
c		     attitude:   calculated roll,pitch and yaw of antenna
c			  sar:   contains wavelenth of radar
c		     dopguess:   fd and fdot that actually give location
c 				 returned by this program.
c                   transform:   transform matrix as calculated by makes
c                       
c*/
	implicit double precision(a-h,o-z)
	double precision statevec(6),s(3,3),xsc(3)
	character*4 choice
	character*1 side
	common /relative/tr1,tr2,tr3,tv1,tv2,tv3,vv1,vv2,vv3,ra1,ra2,ra3
c   xsc + rvec = rt             
	common /earth/ rp,re
	common /sar/ xlambda
	common /attitude/ roll,pitch,yaw
	common /dopguess/ fdguess,fdotguess 
	common /transform/ s            
	data pi/3.141592654/,itmax/1000/   
c                         
c  check for non-zero values
c
	if (rp.eq.0. .or. re.eq.0. .or. xlambda.eq.0. .or. targacc.eq.0.)
     .then
		write(6,*)'Null input parameter to subroutine getloc'
		write(6,*)'returning from subroutine with null location
     .values'
		targlat=0.             
		targphi=0.
		targrad=0.
		return
	end if                                               
	nit=1.  !Initialize iteration count
c
c   translate positional error into error allowed in aguess
c
	aprec=targacc/range ! angular precesion            
c
c   calculate first guess at Doppler using given azimuth angle aguess
c                                     
	side='r'       ! looking toward right of flight path      
	choice='usea'  ! use the azimuth angle in the calculation
	rprec = targacc/2. ! accuracy on range vector half targ position acc.
10	gamma=getlook(statevec,range,rprec,side,choice,aguess)!iterative routine 
c							   ! to get lookangle
	call getdop2 (xlambda,statevec,gamma,aguess,fdguess,fdotguess)       
c
c   calculate error in fd
c                         
	deltafd=fd-fdguess !sign important 
	vrel=dsqrt(vv1**2+vv2**2+vv3**2)
	deltaa=deltafd*(xlambda/(2*vrel))
c	write(6,*)'Iteration =',nit,' deltafd =',deltafd,' deltaa=',deltaa
c	write(6,*)'     xyz=', tr1,tr2,tr3
	if (abs(deltaa) .gt. aprec) then   ! try another iteration
		nit=nit+1                   
		if (nit .gt. itmax) then
			write(6,*)'getloc: no convergence after',nit,'
     .iterations'
			return 1
		end if
		aguess=aguess+deltaa !next guess for az angle
		goto 10           
 	else  ! error in a less than precision
c		write(6,*)' found target position'
		call cart2sph(tr1,tr2,tr3,targrad,targlat,targphi)
		roll=gamma
		return
	end if
	end
