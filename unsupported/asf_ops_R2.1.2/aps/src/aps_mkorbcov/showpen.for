
*.
*   ShowPen - show pen attributes
*
*   Args
*	col		I	input	color index
*
*   History
*	05/07/90 Add line type for POSTSCRIPT output
*	03/18/89 Original
*
*	05/07/90 15:05
*..
	subroutine ShowPen(col)
	include 'eosinclude:display.inc'
	integer col

	  character*100 SccsFileID
     -/'@(#)showpen.for	5.1 98/01/08 APS/ASF\0'/
	integer i1
	double precision r1,r2

	character*10 PenText4014(0:4)
	character*12 PenText4208(0:15)

** Data
	data PenText4014 /'solid','dotted','dot-dash',
     .		'short dash','long dash'/
	data PenText4208 /'Black','White','Red','Green',
     .		'Blue','Cyan','Magenta','Yellow',
     .		'Orange','Green-yellow','Green-cyan','Blue-cyan',
     .		'Blue-magenta','Red-magenta','Dark gray','Light gray'/

	i1 = StyleTable(col,1)
	r1 = StyleTableR(col,1)
	r2 = StyleTableR(col,2)

	if(disp.eq.VT220 .or. disp.eq.Tek4014 .or. disp.eq.Tek4014Sel) then

	    write(*,'(/,a,i3,2a)') ' Pen number ',col,
     .		' is ',PenText4014(i1)

	else if(disp.eq.POSTSCRIPT) then

	    write(*,'(/,a,i3,a,f5.3,a,i3,2a)') ' Pen number ',col,
     .		' is intensity ',r1,' and',int(r2*0.3d0),' dots wide, ',
     .		PenText4014(i1)

	else if(disp.eq.Tek4105MAC .or. disp.eq.Tek4208) then

	    write(*,'(/,a,i3,2a)') ' Pen number ',col,
     .		' is solid ',PenText4208(i1)

	else

	    write(*,'(/,a,i3)')
     .		' No pen information available for pen #',col

	end if

999	continue
	end
