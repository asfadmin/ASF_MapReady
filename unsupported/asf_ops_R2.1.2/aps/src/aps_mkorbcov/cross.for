*.
*  Cross(x,y) - draws a cross at screen location x,y
*
*	03/29/89 13:20
*..
	subroutine cross(x,y)
	include 'eosinclude:display.inc'
	include 'eosinclude:constants.inc'
	integer x,y

	integer xy(4),ticksize
      character*100 SccsFileID
     -/'@(#)cross.for	5.1 98/01/08 APS/ASF\0'/


	if(disp.eq.RAM9460 .or. disp.eq.JUPJ .or. disp.eq.PRINTX) then
	    ticksize = 6
	else if(disp.eq.VT100) then
	    ticksize = 3
	else if(disp.eq.VT220 .or. disp.eq.TEK4010
     .		.or. disp.eq.TEK4010SEL .or. disp.eq.TEK4014
     .		.or. disp.eq.TEK4014SEL .or. disp.eq.TEK4105MAC
     .		.or. disp.eq.TEK4208) then
	    ticksize = 25
	else if(disp.eq.VT240) then
	    ticksize = 8
	else if(disp.eq.JUP7) then
	    ticksize = 4
	else if(disp.eq.POSTSCRIPT) then
	    ticksize = 62
	else
	    ticksize = 6
	end if

	xy(1) = x
	xy(2) = y - ticksize * devscaley
	xy(3) = xy(1)
	xy(4) = y + ticksize * devscaley
c..aap	call vector(xy,2)
	call vector(xy,i2two)

	xy(1) = x - ticksize * devscalex
	xy(2) = y
	xy(3) = x + ticksize * devscalex
	xy(4) = xy(2)
c..aap	call vector(xy,2)
	call vector(xy,i2two)

	end
