
*.
*   Twixt - check if a number is between two others
*
*   Args
*	x1			I*2	input	1st number
*	x			I*2	input	check number
*	x2			I*2	input	2nd number
*	twixt			logical	return
*
*	1/12/88 8:00
*..
	logical function twixt(x1,x,x2)
	integer x1,x,x2

	  character*100 SccsFileID
     -/'@(#)twixt.for	5.1 98/01/08 APS/ASF\0'/
	if((x.ge.x1 .and. x.le.x2) .or. (x.le.x1 .and. x.ge.x2)) then
	    twixt = .true.
	else
	    twixt = .false.
	end if

	end
