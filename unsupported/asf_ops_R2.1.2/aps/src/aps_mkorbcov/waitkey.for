
*.
*   WaitKey - wait for any key
*
*	07/28/89 13:22
*..
	subroutine WaitKey

	  character*100 SccsFileID
     -/'@(#)waitkey.for	5.1 98/01/08 APS/ASF\0'/
	write(*,'(/,a,$)') ' Press RETURN to continue...'
	read(*,*,err=999,end=999)

999	continue
	end
