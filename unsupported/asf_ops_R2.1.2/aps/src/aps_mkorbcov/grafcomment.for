*.
*   GrafComment - add a comment to graphics output
*
*   Args
*	string		C**	input	comment string
*
*	11/10/87 12:15
*..
	subroutine GrafComment(string)
	character*(*) string

	  character*100 SccsFileID
     -/'@(#)grafcomment.for	5.1 98/01/08 APS/ASF\0'/
	include 'eosinclude:display.inc'

	if(disp.eq.POSTSCRIPT) then

	    write(UnitPS,10) string
10	    format(/,'%----- ',a,/)

	end if

	end
