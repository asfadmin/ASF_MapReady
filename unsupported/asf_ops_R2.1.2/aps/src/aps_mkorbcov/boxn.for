*.
*   Boxn - draw an n-sided box
*
*	11/23/88 09:00
*..
	subroutine boxn(x,y,n)
	integer n,x(*),y(*)

	integer i,xy(100)
      character*100 SccsFileID
     -/'@(#)boxn.for	5.1 98/01/08 APS/ASF\0'/

	
	do i=1,n
	    xy(2*i-1) = x(i)
	    xy(2*i) = y(i)
	end do

	xy(2*n+1) = xy(1)
	xy(2*n+2) = xy(2)

	call vector(xy,n+1)

	end


*.
*   Boxni4 - draw an n-sided box
*
*	11/23/88 09:00
*..
	subroutine boxni4(x,y,n)
	integer n,x(*),y(*)

	integer i,xy(100)

	do i=1,n
	    xy(2*i-1) = x(i)
	    xy(2*i) = y(i)
	end do

	xy(2*n+1) = xy(1)
	xy(2*n+2) = xy(2)

	call vector(xy,n+1)

	end
