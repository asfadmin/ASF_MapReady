
*.
*  Printronix - routines for drawing on, printing, and clearing the Printronix
*	  "screen"
*
*	PrintronixVector - light up Printronix virtual pixels to draw a vector
*	PrintronixPrint - print virtual screen to Printronix printer
*	PrintronixClear - clear virtual screen
*
*	10/17/88 12:29
*..
	subroutine PrintronixVector(x1,y1,x2,y2)
	include 'eosinclude:display.inc'
	integer x1,y1,x2,y2

	double precision dx,dy,slopex,slopey,xintercept,yintercept
	integer xstart,ystart,xend,yend,xstep,ystep,x,y
	logical StepWithX

	  character*100 SccsFileID
     -/'@(#)printronix.for	5.1 98/01/08 APS/ASF\0'/

	dx = x2 - x1
	dy = y2 - y1

	if(dx.ne.0.0 .or. dy.ne.0) then
	    if(abs(dx).gt.abs(dy)) then
		slopey = dy / dx
		yintercept = ((y1 - slopey*x1) + (y2 - slopey*x2)) / 2.0
		StepWithX = .true.
	    else
		slopex = dx / dy
		xintercept = ((x1 - slopex*y1) + (x2 - slopex*y2)) / 2.0
		StepWithX = .false.
	    end if
	else
	    PxVirtScreen(x1,y1) = .true.		! single point
	    go to 999
	end if

	if(StepWithX) then
	    xstep = dx/abs(dx)
	    do x=x1,x2,xstep
		y = slopey * float(x) + yintercept
		PxVirtScreen(max(0,min(x,779)),max(0,min(y,779))) = .true.
	    end do
	else
	    ystep = dy/abs(dy)
	    do y=y1,y2,ystep
		x = slopex * float(y) + xintercept
		PxVirtScreen(max(0,min(x,779)),max(0,min(y,779))) = .true.
	    end do
	end if

999	continue
	end


	subroutine PrintronixPrint
	include 'eosinclude:display.inc'

	integer columns,DotsPerChar
	integer DotsPerLine
	parameter(columns=132,DotsPerChar=6)
	parameter(DotsPerLine=(columns-2)*DotsPerChar)

	byte DataOut(columns)
	integer ii(DotsPerChar)

	integer ip,ierr,i,j,line

	open(unit=UnitPX,file='Printronix.plt',form='unformatted',
     .		status='new')

	DataOut(1)   =  5	! <ENQ>
	DataOut(132) = 10	! <LF>

	write(*,'(/)')

	DO 10 LINE=0,779
	    IF(MOD(LINE,16).EQ.0) write(*,1111) int(float(line)/7.8)
1111	    format('+Printer file PRINTRONIX.PLT',i4,' % complete')

	    DO 20 I=1,130
		DO 30 J=1,6
		    II(J) = 0
		    IF(PxVirtScreen(I*6-6+J -1 ,line)) II(J) = 1
30		CONTINUE
		DataOut(I+1) =       II(1)
     .				+  2*II(2)
     .				+  4*II(3)
     .				+  8*II(4)
     .				+ 16*II(5)
     .				+ 32*II(6)
     .				+ 64
		IF(DataOut(I+1).EQ.127) DataOut(I+1) = 63	! all dots on
20	    CONTINUE
	    WRITE(UnitPX) DataOut
10	CONTINUE

	WRITE(UnitPX) 12		! <FF>
	close(UnitPX)

	write(*,1111) 100
	write(*,'(/)')

	END


	subroutine PrintronixClear
	include 'eosinclude:display.inc'

	integer x,y

	do x=0,779
	    do y=0,779
		PxVirtScreen(x,y) = .false.
	    end do
	end do

	end
