
*.
*   Vector - draw a vector
*
*	03/17/89 20:04
*..
	subroutine vector(xy,points)
	include 'eosinclude:display.inc'
	integer xy(*)
	integer points

	  character*100 SccsFileID
     -/'@(#)vector.for	5.1 98/01/08 APS/ASF\0'/
	integer ierr
	integer x,y,xy4(4),nword
	logical*1 yhi,xtra,ylo,xhi,xlo
	integer i,istart,iend,j,k
	integer ixtra

* Deranged vars for Jupiter-7
	INTEGER INIT
	INTEGER IIDX,IIDY
	logical*1 BDX,BDY
	logical*1 DMABUF(7000),BUFdXdY(2,3000)
	EQUIVALENCE (DMABUF(3),BUFdXdY)
	EQUIVALENCE (IIDX,BDX),(IIDY,BDY)


	if(disp.eq.RAM9460) then
	    call cop(xy(1),xy(2),ierr)
	    call wv(xy,points,ierr)

	else if(disp.eq.VT100) then
	    iend = 2*(points-1)
	    write(*,'(3a,$)') '+',esc1,escb
	    do i=1,iend,2
		write(*,10) xy(i),xy(i+1),xy(i+2),xy(i+3)
10		format('+',i4,',',i4,'.(',i4,',',i4'.)',$)
	    end do
	    write(*,'(3a,$)') '+',escc,esc2

	else if(disp.eq.VT220) then
	    write(*,'(3a,$)') '+',esc5i,esc1
	    write(*,'(3a,$)') '+',escast,gs
c	    iend = 2*(points-1)
	    iend = 2*points
	    do i=1,iend,2
c		call TekXY(xy(i),xy(i+1),yhi,xtra,ylo,xhi,xlo)
		call TekXY(xy(i),xy(i+1),yhi,ylo,xhi,xlo)
		write(*,'(5a,$)') '+',yhi,ylo,xhi,xlo
	    end do
	    write(*,*)
	    write(*,'(3a,$)') '+',esc2,esc4i

	else if(disp.eq.VT240) then
	    write(*,'(2a$)') '+',regison
	    write(*,'(2a,i4,a,i4,a,$)') '+','p[',xy(1),',',xy(2),']'
	    iend = 2*points
	    do i=3,iend,2
		write(*,'(2a,i4,a,i4,a,$)')
     .				'+','v[',xy(i),',',xy(i+1),']'
	    end do
	    write(*,'(2a,$)') '+',regisoff

	else if(disp.eq.TEK4010) then
c	    write(*,'(3a,$)') '+',escast,gs
	    write(*,'(2a,$)') '+',gs
c	    iend = 2*(points-1)
	    iend = 2*points
	    do i=1,iend,2
c		call TekXY(xy(i),xy(i+1),yhi,xtra,ylo,xhi,xlo)
		call TekXY(xy(i),xy(i+1),yhi,ylo,xhi,xlo)
		write(*,'(5a,$)') '+',yhi,ylo,xhi,xlo
	    end do
	    write(*,'(2a,$)') '+',us
	    write(*,*)

	else if(disp.eq.TEK4010SEL) then
	    write(*,'(3a,$)') '+',esc1,esca
	    write(*,'(2a,$)') '+',gs
c	    iend = 2*(points-1)
	    iend = 2*points
	    do i=1,iend,2
c		call TekXY(xy(i),xy(i+1),yhi,xtra,ylo,xhi,xlo)
		call TekXY(xy(i),xy(i+1),yhi,ylo,xhi,xlo)
		write(*,'(5a,$)') '+',yhi,ylo,xhi,xlo
	    end do
	    write(*,*)
	    write(*,'(3a,$)') '+',esco,esc2

	else if(disp.eq.TEK4014) then
	    write(*,'(2a,$)') '+',gs
c	    iend = 2*(points-1)
	    iend = 2*points
	    do i=1,iend,2
c		call TekXY(xy(i),xy(i+1),yhi,xtra,ylo,xhi,xlo)
		call TekXY(xy(i),xy(i+1),yhi,ylo,xhi,xlo)
		write(*,'(5a,$)') '+',yhi,ylo,xhi,xlo
	    end do
	    write(*,*)
	    write(*,'(2a,$)') '+',us

	else if(disp.eq.TEK4014SEL) then
	    write(*,'(3a,$)') '+',esc1,gs
c	    iend = 2*(points-1)
	    iend = 2*points
	    do i=1,iend,2
c		call TekXY(xy(i),xy(i+1),yhi,xtra,ylo,xhi,xlo)
		call TekXY(xy(i),xy(i+1),yhi,ylo,xhi,xlo)
		write(*,'(5a,$)') '+',yhi,ylo,xhi,xlo
	    end do
	    write(*,*)
	    write(*,'(3a,$)') '+',us,esc2


	else if(disp.eq.TEK4027) then
	    if(points.gt.2) then
		write(*,'(a,$)') '+!VEC '
		iend = 2*(points-1)
		do i=1,iend,2
		    write(*,'(''+'',i4,'','',i4,'','',$)') xy(i),xy(i+1)
		end do
		write(*,'(''+!VEC '',i4,'','',i4,'','',i4,'','',i4)')
     .			xy(2*points-3),xy(2*points-2),
     .			xy(2*points-1),xy(2*points)
	    else
		write(*,'(''+!VEC '',i4,'','',i4,'','',i4,'','',i4)')
     .					xy(1),xy(2),xy(3),xy(4)
	    end if

	else if(disp.eq.TEK4105MAC .or. disp.eq.TEK4208) then
	    write(*,'(3a,$)') '+',gs
c	    iend = 2*(points-1)
	    iend = 2*points
	    do i=1,iend,2
c		call TekXY(xy(i),xy(i+1),yhi,xtra,ylo,xhi,xlo)
		call TekXY(xy(i),xy(i+1),yhi,ylo,xhi,xlo)
		write(*,'(5a,$)') '+',yhi,ylo,xhi,xlo
	    end do
	    write(*,'(2a,$)') '+',us

	else if(disp.eq.PRINTX) then
	    iend = 2*(points-1)
	    do i=1,iend,2
		call PrintronixVector(xy(i),xy(i+1),xy(i+2),xy(i+3))
	    end do

	else if(disp.eq.JUPJ) then

	    iend = 2*(points-1)
	    call fj_mov(xy(1),xy(2))
	    do i=1,iend,2
		call fj_dva(xy(i+2),xy(i+3))
	    end do
	    call fj_fflush

	else if(disp.eq.POSTSCRIPT) then

	    write(UnitPS,100) xy(1),xy(2)
100	    format('newpath ',i5,1x,i5,' mto')

	    if(points.eq.2) then
		write(UnitPS,105) xy(3),xy(4)
105		format(i5,1x,i5,' lto')
	    else
		ixtra = mod(points-1,4)
		iend = 2*(points-ixtra)
		do i=3,iend,8
		    write(UnitPS,110) (xy(i+j),j=0,7)
110		    format(3(i5,1x,i5,' lto '),i5,1x,i5,' lto')
		end do
		if(ixtra.ne.0) then
		    istart = iend + 1
		    iend = 2*points
		    do i=istart,iend,2
			write(UnitPS,120) xy(i),xy(i+1)
120			format(i5,1x,i5,' lto')
		    end do
		end if
	    end if
	    write(UnitPS,'(a)') 'stroke'

	else if(disp.eq.JUP7) then

cccc Works - very slow - draw absolute, one at a time cccc
c
c	    iend = 2*(points - 1)
c	    do i=1,iend,2
c		call mov(xy(i),xy(i+1))
c		call dva(xy(i+2),xy(i+3))
c	    end do
c
cccc - Jupiter-7 buffer version - stolen from [HO.JUP]PLOTVREL.FOR cccc
c	fixed by Frank Varosi 10/21/85, with muliple buffers.

	iend = points - 1
	i=0
	do while ( i .lt. iend )
	    init=i+1
	    j=0
	    do while ( ( j .lt. 120 ) .and. ( i .lt. iend ) )
		i=i+1
		iidx = xy(2*i+1) - xy(2*i-1)
		iidy = xy(2*i+2) - xy(2*i)
		if (abs(iidx).gt.255 .or. abs(iidy).gt.255) then
c		    *** Vector too big - cut into smaller pieces ***
	write(*,*) 'Vector too big -',abs(iidx),abs(iidy)
		    iidx = iidx/4
		    iidy = iidy/4
		    do k=1,4
			j=j+1
			BUFdXdY(1,j) = bdx
			BUFdXdY(2,j) = bdy
		    end do
		else if ( iidx.ne.0 .or. iidy.ne.0 ) then
			j=j+1
			BUFdXdY(1,j) = bdx
			BUFdXdY(2,j) = bdy
		end if
	    end do

D	write(*,*) 'iend=',iend,'  init=',init,'   i=',i,'   j=',j
	    BUFdXdY(1,j+1) = 0
	    BUFdXdY(2,j+1) = 0

		xy4(1)=xy(2*init-1)
		xy4(2)=xy(2*init)
	    CALL MOV(xy4(1),xy4(2))

	    DMABUF(1)=27
	    DMABUF(2)=109
	    DMABUF(2*j+2+3)=27
	    DMABUF(2*j+2+4)=27

	    CALL SCD
	    nword=j+1+2
	    CALL XFR (DMABUF,nword)
	    CALL XDA

	end do
cccc
	end if

999	end
