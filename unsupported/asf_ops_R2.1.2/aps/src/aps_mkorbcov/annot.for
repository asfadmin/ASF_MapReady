*.
*   Annot - add annotation to graphics display
*
*	05/19/90 Removed message if no annotation available
*	04/17/90
*
*	05/19/90 20:29
*..
	subroutine Annot

	  character*100 SccsFileID
     -/'@(#)annot.for	5.1 98/01/08 APS/ASF\0'/
	include 'eosinclude:display.inc'

	if(disp.eq.RAM9460) then
	    call AnnotRam
	else if(disp.eq.POSTSCRIPT) then
	    call AnnotPS
	else
c	    write(*,'(/,a)') ' No annotation available for this display'
c	    call WaitKey
	end if

	end
