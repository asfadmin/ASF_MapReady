
*.
*   Strings - useful string routines
*
*	ToUpper(c) - convert c to uppercase
*	ToUpperS(string) - convert string to uppercase
*
*	10/31/88 13:52
*..
c	subroutine ToUpper(c)
c	character c
c
c	if(c.ge.'a' .and. c.le.'z') c = char(ichar(c) - 32)
c
c	end


	subroutine ToUpperS(string)
	character*(*) string

	  character*100 SccsFileID
     -/'@(#)strings.for	5.1 98/01/08 APS/ASF\0'/
	integer slen,i
	character c

	slen = len(string)

	do i=1,slen
	    c = string(i:i)
	    if(c.ge.'a' .and. c.le.'z') string(i:i) = char(ichar(c) - 32)
	end do

	end
