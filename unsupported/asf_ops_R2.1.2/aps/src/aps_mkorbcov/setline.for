
*.
*   SetLine - set line styles
*
*   Args
*	col			integer	input	style index number
*	i1,i2,i3,i4		integer	input	style values for this entry
*	r1,r2			double	input	style values for this entry
*
*	09/08/88 13:30
*..
	subroutine SetLine(col,i1,i2,i3,i4,r1,r2)
	include 'eosinclude:display.inc'
	integer col,i1,i2,i3,i4
	double precision r1,r2

	  character*100 SccsFileID
     -/'@(#)setline.for	5.1 98/01/08 APS/ASF\0'/
	StyleTable(col,1) = i1
	StyleTable(col,2) = i2
	StyleTable(col,3) = i3
	StyleTable(col,4) = i4

	StyleTableR(col,1) = r1
	StyleTableR(col,2) = r2

	end
