*.
*   Clear - clear the screen in too many different ways
*
*	Clearg - clear graphics screen
*	Cleart - clear text screen
*	Cleartty - clear text screen
*	NewPage - clear screen
*	ClearTextPlane - clear text
*
*	03/29/89 13:18
*..
	subroutine clearg
	include 'eosinclude:display.inc'
      character*100 SccsFileID
     -/'@(#)clear.for	5.1 98/01/08 APS/ASF\0'/

	integer ierr

	if(disp.eq.RAM9460) then
	    call bers(ierr)
	else if(disp.eq.VT100) then
	    write(*,'(5a,$)') '+',esc1,escm,ff,esc2
	else if(disp.eq.VT220) then
	    write(*,'(6a,$)') '+',esc5i,esc1,escff,esc2,esc4i
	else if(disp.eq.VT240) then
	    write(*,'(4a,$)') '+',regison,'s(e)',regisoff
	else if(disp.eq.JUP7) then
	    call ers
	else if(disp.eq.JUPJ) then
	    call fj_clear
	    call fj_fflush
	else if(disp.eq.TEK4010) then
	    write(*,'(2a,$)') '+',ff
	else if(disp.eq.TEK4010SEL) then
c	    write(*,'(2a,$)') '+',escff
	    write(*,'(5a,$)') '+',esc1,escm,ff,esc2
	else if(disp.eq.TEK4014) then
	    write(*,'(2a,$)') '+',escff
	else if(disp.eq.TEK4014SEL) then
	    write(*,'(4a,$)') '+',esc1,escff,esc2
	else if(disp.eq.TEK4027) then
	    write(*,'(a)') '+!ERA G'
	else if(disp.eq.TEK4105MAC .or. disp.eq.TEK4208) then
	    write(*,'(2a,$)') '+',escff
	else if(disp.eq.PRINTX) then
	    call PrintronixClear
	else if(disp.eq.POSTSCRIPT) then
	    write(UnitPS,'(a)') 'erasepage'
	end if

	end


	subroutine cleart
	include 'eosinclude:display.inc'

	character esc

	esc = char(27)

	if(disp.eq.0) then
	    write(*,'(3a,$)') '+',esc,'[2J'
	else if(.not.GraphicText(disp)) then
	    if(disp.eq.TEK4027) then
		write(*,'(a)') '+!ERA M'
	    else if(disp.eq.TEK4208) then
		write(*,'(3a,$)') '+',esc,'LZ'
	    else if(disp.ne.POSTSCRIPT .and. disp.ne.NONE) then
		write(*,'(3a,$)') '+',esc,'[2J'
		write(*,'(3a,$)') '+',esc,'[H'
	    end if
	end if

	end


	subroutine cleartty
	include 'eosinclude:display.inc'

	character esc

	esc = char(27)

	if(disp.eq.TEK4208) then
	    call cleart
	else if(disp.eq.TEK4027) then
	    write(*,'(a)') '+!ERA M'
	else
	    write(*,'(3a,$)') '+',esc,'[2J'
	    write(*,'(3a,$)') '+',esc,'[H'
	end if

	end


	subroutine NewPage
	include 'eosinclude:display.inc'

	if(disp.eq.0) then
	    call cleart
	else if(GraphicText(disp)) then
	    call clearg
	else
	    call cleart
	end if

	end


	subroutine ClearTextPlane
	include 'eosinclude:display.inc'

	if(.not.TwoMonitors(disp)) call cleart

	end
