
*.
*   Line - draw a line
*
*   Args
*	x1,y1		I*2	input	starting location in display coords
*	x2,y2		I*2	input	endpoint location in display coords
*
*	02/23/89 13:54
*..
	subroutine line(x1,y1,x2,y2)
	include 'eosinclude:constants.inc'
	integer x1,y1,x2,y2

	  character*100 SccsFileID
     -/'@(#)line.for	5.1 98/01/08 APS/ASF\0'/
	integer xy(4)

	xy(1) = x1
	xy(2) = y1
	xy(3) = x2
	xy(4) = y2

c..aap	call vector(xy,2)
	call vector(xy,i2two)

	end
