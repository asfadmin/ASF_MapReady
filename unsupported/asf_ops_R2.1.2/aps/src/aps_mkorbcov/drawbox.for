*.
*   DrawBox - draw a rectangular box
*
*   Args
*	x1,y1		I*2	input	1st corner of box
*	x2,y2		I*2	input	opposite corner of box
*
*	09/08/88 9:45
*..
	subroutine DrawBox(x1,y1,x2,y2)
	include 'eosinclude:constants.inc'
	integer x1,y1,x2,y2

	integer xy(10)

	 character*100 SccsFileID
     -/'@(#)drawbox.for	5.1 98/01/08 APS/ASF\0'/
	xy(1) = x1
	xy(2) = y1
	xy(3) = x2
	xy(4) = y1
	xy(5) = x2
	xy(6) = y2
	xy(7) = x1
	xy(8) = y2
	xy(9) = x1
	xy(10)= y1

c..aap	call vector(xy,5)
	call vector(xy,i2five)

	end
