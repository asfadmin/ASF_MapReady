*.
*   GetColor
*
*   Args
*	col		I	input	color index
*	string		C**	input	prompt string
*.
*.
*   GetCol(col,string) - get pen setup (color, style, width, ...)
*
*	03/29/89 13:24
*..
	subroutine GetColor(col,string)
	include 'eosinclude:display.inc'
	include 'eosinclude:constants.inc'	! aap..11/4/93
	integer col
	character*(*) string

	  character*100 SccsFileID
     -/'@(#)getcolor.for	5.1 98/01/08 APS/ASF\0'/
	integer index

	if(disp.eq.TEK4105MAC .or. disp.eq.TEK4208) then

	    write(*,'(/,3a,/)')
     .		' Choose one of the following colors ',string,': '

	    write(*,'(4(t6,i2,1x,a,t22,i2,1x,a,
     .			t38,i2,1x,a,t54,i2,1x,a,/))')
     .		0,'Black',4,'Blue',8,'Orange',12,'Blue-magenta',
     .		1,'White',5,'Cyan',9,'Green-yellow',13,'Red-magenta',
     .		2,'Red',6,'Magenta',10,'Green-cyan',14,'Dark gray',
     .		3,'Green',7,'Yellow',11,'Blue-cyan',15,'Light gray'

10	    write(*,'(a,$)') ' Color number [1=White]: '
	    read(*,'(i10)',err=10,end=999) index
c	    if(index.eq.0) index = 2
	    if(index.lt.0 .or. index.gt.15) go to 10

    	    col = index
c..aap	    call SetLine(col,index,0,0,0,0.0,0.0)
	    call SetLine(col,index,izero,izero,izero,dzero,dzero)

	else

	    call GetCol(col,string)

	end if

999	continue
	end
