C Alaska SAR Processor (ASP) %W% %E% %U%
	subroutine simpwrap (f,is,ie_i,nwrap,area)
c/*	subroutine simpwrap (f,is,ie_i,nwrap,area) ----------------
c
c 	Author R.E. Carande
c	
c	
c                               
c   subroutine to integrate from the is element to the ie element of
c   the real vector f according to the simpson weights:
c
c	Area (Simpson) = 1/3*{ f(is)+4f(is+1)+2f(is+2)+4f(is+3)+y(ie) }
c
c   resulting area must be multiplied by step size to get into proper
c   units.     
c                          
c	THIS VERSION, SIMPWRAP, WRAPS AROUND AT NWRAP ELEMENTS TO FIRST.
c   8/12/91: make sure that do not change the value of third arguement
c*/
	real*4 f(1)
	ie = ie_i
c
c   check to see if numpts >=3 and odd.
c
10	numpts=(ie-is+1)
	if (numpts .lt. 3) then
		write(6,*)'simpwrap: lt 3 points.'
		return
	endif
	if (int(numpts/2)*2. .eq. numpts) then !even number of points
c	write(6,*)'simpson: even number of integration points.'
c	write(6,*)'making odd by ignoring last point'
		ie=ie-1
		goto 10
	endif
c
c   begin adding weighted values
c
	nestop=numpts-1
	nostop=numpts-2  
	area=0.  
	ae=0.
	ao=0.
c
c   add even
c
	do i=is+1,is+nestop,2   
	ILOOP=I
198		IF (ILOOP .GT. NWRAP)THEN
			 ILOOP=ILOOP-NWRAP
			 GOTO 198
		ENDIF
		ae=ae + f(ILOOP)
	end do
c
c   add odd
c
	if (numpts.eq.3) goto 200  ! 3rd point is last point
	do i=is+2,is+nostop,2  
	ILOOP=I
199		IF (ILOOP .GT. NWRAP)THEN
			ILOOP=ILOOP-NWRAP    
		        GOTO 199
		ENDIF
		ao=ao+f(iLOOP)
	enddo 
c             
c   weight with simpson weights
c
	IEE=IE
200	IF (IEE .GT. NWRAP) THEN 
		IEE = IEE-NWRAP
		GOTO 200
	END IF
	area=1./3.*( f(is) + 4*ae + 2*ao + f(IEE))
	return
 	end
