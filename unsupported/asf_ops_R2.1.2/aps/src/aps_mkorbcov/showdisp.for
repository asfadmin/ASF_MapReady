
*.
*   ShowDisp - show display information
*
*   History
*	 3/20/91 Original
*
*   Last modified
*	3/20/91 14:34
*..
	subroutine ShowDisp

	  character*100 SccsFileID
     -/'@(#)showdisp.for	5.1 98/01/08 APS/ASF\0'/
	include 'eosinclude:display.inc'


	if(disp .lt. 1 .or. disp .gt. DEVICES) return

	write(*,'(/,2a)') ' Display is ',DisplayName(disp)

	write(*,'(a,2f8.3)') '   Horizontal, vertical scale factors',
     .				xPageMult,yPageMult
	write(*,'(a,f8.3)') '   Zoom',zoom

	end
