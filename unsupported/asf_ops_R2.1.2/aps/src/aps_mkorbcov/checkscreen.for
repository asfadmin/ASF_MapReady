*.
*   CheckScreen - check if a point is in the plotting area
*
*	CheckScreen(x,y,clip) - integer*2 point coords
*	CheckScreenR(xr,yr,clip) - real point coords
*	CheckScreenD(xd,yd,clip) - double precision point coords
*
*	08/03/89 07:58
*..
	subroutine CheckScreen(x,y,clip)
	include 'eosinclude:display.inc'
	integer x,y
	logical clip
      character*100 SccsFileID
     -/'@(#)checkscreen.for	5.1 98/01/08 APS/ASF\0'/

c Functions
	logical twixt


	clip = .not.(twixt(screenx(1),x,screenx(2)) .and.
     .			twixt(screeny(1),y,screeny(2)))

	end


	subroutine CheckScreenR(xr,yr,clip)
	include 'eosinclude:display.inc'
	real xr,yr
	logical clip

c Functions
	logical rTwixt


	clip = .not.(rTwixt(float(screenx(1)),xr,float(screenx(2))) .and.
     .		rTwixt(float(screeny(1)),yr,float(screeny(2))))

	end


	subroutine CheckScreenD(xd,yd,clip)
	include 'eosinclude:display.inc'
	double precision xd,yd
	logical clip

c Functions
	logical dTwixt


	clip = .not.(dTwixt(dble(screenx(1)),xd,dble(screenx(2))) .and.
     .		dTwixt(dble(screeny(1)),yd,dble(screeny(2))))

	end
