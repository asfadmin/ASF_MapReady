C Alaska SAR Processor (ASP) %W% %E% %U%
	subroutine newtprop (statevec,t,pstep,propvec)
c/*	subroutine newtprop (statevec,t,pstep,propvec) -----------
c
c   subroutine to propagate the input statevec by simply assuming
c   a circular orbit and using newtons formula to calculate a new
c   statevector, propvec
c
c	INPUT:	STATEVEC:  DOUBLE PRECISION DIMENSION 6 STATEVECTOR
C				DESCRIBING LOCATION AND VELOCITY OF
C				SATELLITE (X,Y,Z,VX,VY,VZ) IN CARTESIAN
C				COORDINATES
C		T	DOUBLE PRECISION TIME TO PROPAGATE INPUT STATEVECTOR
C		PSTEP	STEP TO USE IN PROPAGATION. (SUGGEST = 0.1SEC)
C		
C	OUTPUT:	PROPVEC:	DOUBLE PRECISION PROPAGATED STATEVECTOR
C				UNITS ARE SAME AS INPUT UNITS.
C
C	NOTE ON UNITS TO THIS SUBROUTINE:  Since this subroutine assumes
c		a circular orbit, the units are relative, but must be 
c		consistant.  For instance, if the statevector is in km
c		and km/s, then t and pstep must be in seconds.  The result
c		propvec will then be in km and km/s as well.
c                
c
c   t is the time to propagate statevec (may be pos or neg) in sec.
c
c   avoid modifying the step size by using a local variable. 8/7/91
c                                          
c*/
	implicit double precision (a-h,o-z)
	double precision statevec(6),propvec(6),a(3)
c	data pstep/.1d0/  !.1 sec step size
	tstep = pstep
	do i=1,6
	propvec(i)=statevec(i)
	end do                              
	nstep=int(t/tstep)  ! take tstep sec steps
	tlast= t - tstep*nstep !last step size
	if (nstep .lt. 0) then 
		tstep=-tstep  
		nstep=-nstep
	end if
	do idummy=1,nstep
	h2=(propvec(1)**2 + propvec(2)**2+ propvec(3)**2)
	v2=(propvec(4)**2 + propvec(5)**2+ propvec(6)**2)
	const=v2/h2          
	   do i=1,3             
	   a(i)=-const*propvec(i)
	   propvec(i)=propvec(i)+propvec(i+3)*tstep+.5d0*a(i)*tstep**2
	   propvec(i+3)=propvec(i+3) + a(i)*tstep
	   end do
c	write(6,*)idummy*tstep,'sec',propvec                        
c	write(6,*)idummy*tstep
c	write(6,*)'magnitudes: ',sqrt(propvec(1)**2+propvec(2)**2+
c     .propvec(3)**2),sqrt(propvec(4)**2+propvec(5)**2+propvec(6)**2)
	end do
c
c   propagate last bit (less than tstep)
c
	do i=1,3             
	a(i)=-const*propvec(i)
	propvec(i)=propvec(i)+propvec(i+3)*tlast+.5d0*a(i)*tlast**2
	propvec(i+3)=propvec(i+3) + a(i)*tlast
	end do                   
c	write(6,*)'final statevec: ',propvec             
c	write(6,*)'magnitudes: ',sqrt(propvec(1)**2+propvec(2)**2+
c     .propvec(3)**2),sqrt(propvec(4)**2+propvec(5)**2+propvec(6)**2)
	return
 	end
              
