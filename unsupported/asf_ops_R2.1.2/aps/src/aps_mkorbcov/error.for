*.
*   ErrorMessage - print error message
*
*	10/14/88 14:17
*..
	subroutine ErrorMessage(text)
	character*(*) text

	  character*100 SccsFileID
     -/'@(#)error.for	5.1 98/01/08 APS/ASF\0'/
	write(*,10) text
10	format(/,' Error from ',a,/)

	end
