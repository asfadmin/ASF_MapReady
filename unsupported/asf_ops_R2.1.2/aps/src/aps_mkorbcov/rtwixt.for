
	logical function rtwixt(x1,x,x2)
	real x1,x,x2

	  character*100 SccsFileID
     -/'@(#)rtwixt.for	5.1 98/01/08 APS/ASF\0'/
	if((x.ge.x1 .and. x.le.x2) .or. (x.le.x1 .and. x.ge.x2)) then
	    rtwixt = .true.
	else
	    rtwixt = .false.
	end if

	end
