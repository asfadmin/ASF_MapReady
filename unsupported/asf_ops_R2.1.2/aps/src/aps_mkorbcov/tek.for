
*.
*   Tek - Tektronix routines
*
*	TekI(i,string,n)  - encode an integer
*	TekXY(x,y,yhi,ylo,xhi,xlo) - encode an x,y pair into 4 bytes
*
*   Args
*	i			integer	input	number to encode
*	string			C**	output	encoded number
*	n			integer	output	number of chars in string
*
*	x,y			I*2	input	x,y pair
*	yhi,ylo,xhi,xlo		L*1	output	encoded pair
*
*	03/17/89 Original
*
*	03/20/89 21:12
*..

	subroutine TekI(i,string,n)
	integer i,n
	character*(*) string

	  character*100 SccsFileID
     -/'@(#)tek.for	5.1 98/01/08 APS/ASF\0'/
	integer ichar0,icharAt
	parameter (ichar0=48,icharAt=64)

	if(i.lt.16) then
	    string(1:1) = char(i + ichar0)
	    n = 1
	else if(i.lt.256) then
	    string(1:1) = char(i/16 + icharAt)
	    string(2:2) = char(mod(i,16) + ichar0)
	    n = 2
	end if

	end


c	subroutine TekXY(x,y,yhi,xtra,ylo,xhi,xlo)
	subroutine TekXY(x,y,yhi,ylo,xhi,xlo)
	integer x,y
	logical*1 yhi,xtra,ylo,xhi,xlo

	integer x4,y4

	x4 = x / 4
	y4 = y / 4

	yhi  = int(y4/32) + 32
c	xtra = 4*(y-4*y4) - 4*x4 + 96
	ylo  = y4 - 32*int(y4/32) + 96
	xhi  = int(x4/32) + 32
	xlo  = x4 - 32*int(x4/32) + 64

	end
