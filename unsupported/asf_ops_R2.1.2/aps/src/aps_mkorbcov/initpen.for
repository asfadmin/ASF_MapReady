*.
*   InitPen
*
*   History
*	03/18/89
*
*	03/29/89 13:38
*..
	subroutine InitPen

** Includes
	include 'eosinclude:display.inc'
	include 'eosinclude:constants.inc'

 	  character*100 SccsFileID
     -/'@(#)initpen.for	5.1 98/01/08 APS/ASF\0'/
	integer ierr

	if(disp.eq.TEK4105MAC .or. disp.eq.TEK4208) then
	    StyleTable(0,1) = 1
	else if(disp.eq.RAM9460) then
c..aap	    call fgd(255,ierr)
	    call fgd(i255,ierr)
	else if(disp.eq.VT220 .or. disp.eq.TEK4014
     .			.or. disp.eq.TEK4014SEL) then
	    StyleTable(0,1) = 0
	else if(disp.eq.JUPJ) then
c..aap	    call fj_sec(255)
	    call fj_sec(i255)
	else if(disp.eq.JUP7) then
c..aap	    call sec(255)
	    call sec(i255)
	else if(disp.eq.POSTSCRIPT) then

	end if

	end
