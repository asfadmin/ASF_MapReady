*.
*   Hardcopy
*
*   History
*	 6/20/91 - changed POSTSCRIPT to PS-Adobe-3.0
*	 3/06/91 - added 'copypage' option for POSTSCRIPT
*	 4/17/90 - moved annotation to separate routine
*	 1/08/90 - added prompt for NumCopies for POSTSCRIPT
*	 3/17/89
*
*   Last modified
*	6/20/91 13:48
*..
	subroutine HardCopy

	include 'eosinclude:display.inc'
	character YesNo
	character cdate*20,ctime*20
	integer lendate,lentime
	integer NumCopies

	  character*100 SccsFileID
     -/'@(#)hardcopy.for	5.1 98/01/08 APS/ASF\0'/
	if(disp.eq.RAM9460 .or. disp.eq.VT100
     .			.or. disp.eq.VT220 .or. disp.eq.TEK4010SEL) then

	    write(*,*)
10	    write(*,'(a,$)') ' Make a hardcopy [no]? '
	    read(*,'(a)',err=10,end=999) YesNo
	    write(*,*)
	    if(YesNo.eq.'Y' .or. YesNo.eq.'y') then
		if(disp.eq.RAM9460) then
		    if(xPageMult.eq.1.0
     .			    .and.yPageMult.eq.(780.0d0/1024.0d0)) then
			call PrintRAMRot
		    else
			call PrintRAM
		    end if
		else if(disp.eq.VT100 .or. TEK4010SEL) then
		    call PrintVT100
		else if(disp.eq.VT220) then
		    call PrintVT220
		end if
	    end if

	else if(disp.eq.PRINTX) then

	    call PrintronixPrint

	else if(disp.eq.POSTSCRIPT) then

	    write(*,*)
20	    write(*,'(a,$)') ' Print this page as is [yes]? '
	    read(*,'(a)',err=20,end=999) YesNo
	    if(YesNo.ne.'N' .and. YesNo.ne.'n') then

		write(*,*)
30		write(*,'(a,$)') ' Enter number of copies [1]: '
		read(*,'(i10)',err=30,end=999) NumCopies
		if(NumCopies.le.0) NumCopies = 1
		write(UnitPS,'(/,a,i5,a)') '/#copies',NumCopies,' def'

		write(*,*)
40		write(*,'(a,$)') ' Erase current page after printing [yes]? '
		read(*,'(a)',err=40,end=999) YesNo
		if(YesNo.ne.'N' .and. YesNo.ne.'n') then
		    write(UnitPS,'(a)') 'showpage'
		else
		    write(UnitPS,'(a)') 'copypage'
		end if

		write(*,*)
50		write(*,'(a,$)') ' Close the POSTSCRIPT file [yes]: '
		read(*,'(a)',err=50,end=999) YesNo
		if(YesNo.ne.'N' .and. YesNo.ne.'n') then
		    write(UnitPS,'(/,a)') '%%Trailer'
		    call GetSysDateTime(cdate,ctime,lendate,lentime)
		    write(UnitPS,60) PSPageNum,cdate(1:lendate),
     .					ctime(1:lentime)
60		    format('%%Pages:',i5,/,'%----- End at ',a,1x,a)
		    write(UnitPS,'(a)') '%%EOF'
		    close(UnitPS)
		    PSPageNum = 0
		else
		    PSPageNum = PSPageNum + 1
		    write(UnitPS,'(/,a,i5,1x,i5,/)') '%%Page: ',
     .					PSPageNum,PSPageNum
		end if
	    end if

	else if(GraphicText(disp)) then

	    write(*,'(2a,$)') '+',char(7)
	    read(*,*,err=999,end=999)

	end if

999	continue
	end
