*.
*   FillIt - fill swaths with lines
*
*	10/17/88 09:00
*..
	subroutine FillIt(xy1,xy2,points)
	include 'eosinclude:display.inc'
	integer xy1(*),xy2(*),points

	integer i,iend,cpx,cpy
	integer dx1,dx2,dx3,dx4,toolong

	  character*100 SccsFileID
     -/'@(#)fillit.for	5.1 98/01/08 APS/ASF\0'/

	if(points.lt.2) go to 999

	iend = 2*points - 1
	toolong = abs(swidex/2)

c if we start i=1 then an extra line is drawn
c if we start i=3 then we don't draw the first line
c	do 100 i=1,iend,2
	do 100 i=3,iend,2

cccc Draw a box (vs. a line) cccc
c
c	iend = 2*points - 3
c	do i=1,iend,2
c	    dx1 = xy1(i) - xy1(i+2)
c	    dx2 = xy1(i+2) - xy2(i+2)
c	    dx3 = xy2(i+2) - xy2(i)
c	    dx4 = xy2(i) - xy1(i)
c	    if(abs(dx1).gt.toolong .or. abs(dx2).gt.toolong .or.
c     .		abs(dx3).gt.toolong .or. abs(dx4).gt.toolong) go to 100
c	    call box4(xy1(i),xy1(i+1),xy1(i+2),xy1(i+3),
c     .		      xy2(i+2),xy2(i+3),xy2(i),xy2(i+1))
c
cccc

	    dx1 = xy1(i) - xy2(i)
	    if(abs(dx1).lt.toolong) then
		call line(xy1(i),xy1(i+1),xy2(i),xy2(i+1))
	    end if

cccc Fill cccc
c
c	    if(disp.eq.RAM9460) then
c	        cpx = (xy1(i) + xy1(i+2) + xy2(i) + xy2(i+2))/4
c	        cpy = (xy1(i+1) + xy1(i+3) + xy2(i+1) + xy2(i+3))/4
c	        call cop(cpx,cpy,ierr)
c	        call fill(0,ierr)
c	    end if
c
cccc

100	continue

999	continue
	end
