
*.
*   UseCol - set drawing color or line style
*
*   Args
*	col			integer	input	color or style index
*
*	05/07/90 Added line type for POSTSCRIPT output
*	03/29/89
*
*	05/07/90 14:41
*..
	subroutine UseCol(col)
	include 'eosinclude:display.inc'
	integer col

	  character*100 SccsFileID
     -/'@(#)usecol.for	5.1 98/01/08 APS/ASF\0'/
	integer col2
	integer ierr

	character esc,Tek4014LineType(5)
	integer j,n
	character*3 tekstring
	character*12 PostLineType(5)

	data esc /27/
	data Tek4014LineType /'`','a','b','c','d'/
	data PostLineType
     .		/'[]','[4 20]','[4 27 55 27]','[27 27]','[55 27]'/

	if(disp.eq.RAM9460) then
	    col2 = col
	    call fgd(col2,ierr)
	else if(disp.eq.VT220) then
	    write(*,'(3a,$)') '+',esc5i,esc1
	    j = StyleTable(col,1) + 1
	    write(*,'(3a,$)') '+',esc,Tek4014LineType(j)
	    write(*,'(3a,$)') '+',esc2,esc4i
	else if(disp.eq.TEK4014) then
	    j = StyleTable(col,1) + 1
	    write(*,'(3a,$)') '+',esc,Tek4014LineType(j)
	else if(disp.eq.TEK4014SEL) then
	    write(*,'(2a,$)') '+',esc1
	    j = StyleTable(col,1) + 1
	    write(*,'(3a,$)') '+',esc,Tek4014LineType(j)
	    write(*,'(2a,$)') '+',esc2
	else if(disp.eq.TEK4027) then
	    write(*,'(a,i1,$)') '+!COL C',col
c	    write(*,'(a,i1,$)') '+!LIN ',linetype
	else if(disp.eq.JUPJ) then
	    call fj_sec(col)
	else if(disp.eq.JUP7) then
	    call sec(col2)
	else if(disp.eq.TEK4105MAC .or. disp.eq.TEK4208) then
	    call TekI(StyleTable(col,1),tekstring,n)
	    write(*,'(4a,$)') '+',esc,'ML',tekstring(1:n)
	else if(disp.eq.POSTSCRIPT) then
	    j = StyleTable(col,1) + 1
	    write(UnitPS,10) StyleTableR(col,1),StyleTableR(col,2),
     .				PostLineType(j)
10	    format('stroke ',f5.3,' setgray ',f7.3,' setlinewidth ',
     .			a,' 0 setdash')
	end if

	end
