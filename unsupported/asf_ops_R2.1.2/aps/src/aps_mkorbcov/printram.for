
*.
*   PrintRAM and PrintRAMRot - dump Ramtek screen to Printronix file
*
*	09/06/88 09:40
*..
	subroutine PrintRAM

	include 'eosinclude:display.inc'
	include 'eosinclude:constants.inc'

	BYTE DATA(132)
	INTEGER ARRAY(780),II(6),IDUM(2),IERR
	INTEGER IP,LINE,I,J

	  character*100 SccsFileID
     -/'@(#)printram.for	5.1 98/01/08 APS/ASF\0'/
	write(*,*)

	DATA(1) = 5
	DATA(132) = 10

	open(unit=UnitPx,file='ramtek.plt',form='unformatted',
     .		status='new')

c	write(*,10000)
c10000	format('$I`M GOING TO COPY 780 PIXELS BY 1024 LINES'/
c     1	' SHOULD I DO THE LEFT (DEF) SIDE OR RIGHT? ')
c	read(*,10001) LR
c10001	FORMAT(A1)
c	lr = 'l'
	IP=0
c	IF(LR.EQ.'R'.OR.LR.EQ.'r') IP=499

c	WRITE(UnitPx) "14

	write(*,'(/)')

	DO 10 LINE=0,1023

	    IF(MOD(LINE,16).EQ.0) write(*,1111) int(float(line)/10.23)
1111	    format('+Printer file RAMTEK.PLT',i4,' % complete')

	    call cop(IP,line,ierr)
c..aap	    call ri(array,780,ierr)
	    call ri(array,i780,ierr)

	    DO 20 I=1,130
		DO 30 J=1,6
		    II(J)=0
C		    IF(ARRAY(I*6-6+J).GE.1)II(J)=1	! (dc 7/20/85)
		    IF(ARRAY(I*6-6+J).NE.0)II(J)=1	! (dc 7/20/85)
30		CONTINUE
		DATA(I+1)=II(1)+2*II(2)+4*II(3)+8*II(4)+16*II(5)+32*II(6)+64
		IF(DATA(I+1).EQ.127)DATA(I+1)=63
20	    CONTINUE

	    WRITE(UnitPx) DATA
10	CONTINUE

	close(UnitPx)
	write(*,1111) 100
c	call lib$spawn('PRINT/NOTIFY/DELETE RAMTEK.PLT')

999	continue
	end


	subroutine PrintRAMRot
c	  -- print out ramtek screen after 90 degree rotation
c	  --  so image fits on two consecutive printer pages

	include 'eosinclude:display.inc'
	include 'eosinclude:constants.inc'

	byte data(132)
	integer array(780),ii(6),idum(2),column,firstline,lastline,ierr
	integer i,j
	character tb

      	data(1) = 5
      	data(132) = 10

c	write(*,*)
c10	write(*,'(a,/,a,$)') ' I`m going to copy 1280 pixels by 780 lines',
c     .			' Should I do the top side or bottom [top]? '
c	read(*,'(a)',err=10,end=999) tb
c	if(tb.eq.'B'.or.tb.eq.'b') then
c	    firstline = 1023
c	else
	    firstline = 779
c      	end if
	
c	call RmInit(ierr)
c..aap	call Win(0,0,1279,1023,ierr)
	call Win(izero,izero,i1279,i1023,ierr)
c..aap	call Scn(5,ierr)	! set scan direction to bottom-to-top
	call Scn(ifive,ierr)	! set scan direction to bottom-to-top
	open(unit=UnitPx,file='ramtek.plt',form='unformatted',
     .		status='new',err=999)
c	write(1) 12

	write(*,'(/)')

      	do 100 column=0,1279

	    if(mod(column,16).EQ.0) write(*,1111) int(float(column)/12.79)
1111	    format('+Printer file RAMTEK.PLT',i4,' % complete')

	    call cop(column,firstline,ierr)
c..aap	    call ri(array,780,ierr)
	    call ri(array,i780,ierr)

      	    do 20 i=1,130
      		do 30 j=1,6
      		    ii(j)=0
      		    if(array(i*6-6+j).ne.0) ii(j)=1
30 		continue
      	        data(i+1) = ii(1) + 2*ii(2) + 4*ii(3) + 8*ii(4)
     .				+ 16*ii(5) + 32*ii(6) + 64
      	        if(data(i+1).eq.127) data(i+1)=63
20 	    continue

      	    write(UnitPx) data
100 	continue

	write(*,1111) 100

999	continue
c..aap	call scn(0,ierr)
	call scn(izero,ierr)
      	end
