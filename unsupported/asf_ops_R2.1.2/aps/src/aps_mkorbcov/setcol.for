
*.
*  SetCol - set color table values
*
*	09/08/88 13:35
*..
	subroutine setcol(col,red,green,blue)
	include 'eosinclude:display.inc'
	integer col,red,green,blue

	  character*100 SccsFileID
     -/'@(#)setcol.for	5.1 98/01/08 APS/ASF\0'/
	integer RAMCTSIZ,JUPCTSIZ
	parameter(RAMCTSIZ=1024,JUPCTSIZ=256)
	byte array(4*RAMCTSIZ)
	integer r2,g2,b2
	byte r1,g1,b1

	integer i,ierr

	if(disp.eq.RAM9460) then
	    call ram(0,0,array,2*RAMCTSIZ,ierr)
	    i = 4*col + 1
	    array(i)   = blue
	    array(i+1) = green
	    array(i+2) = red
	    call lam(0,0,array,2*RAMCTSIZ,ierr)
	else if(disp.eq.JUPJ) then
	    r1 = red
	    g1 = green
	    b1 = blue
	    call fj_cltset(col,1,r1,g1,b1)
	    call fj_fflush
	else if(disp.eq.JUP7) then
	    r2 = red
	    g2 = green
	    b2 = blue
	    call sct(col,1,r2,g2,b2)
	else if(disp.eq.TEK4027) then
	    write(*,'(a,i1,3(a,i3))') '+!MIX C',col,' ',
     .			red,',',green,',',blue
	end if

	end
