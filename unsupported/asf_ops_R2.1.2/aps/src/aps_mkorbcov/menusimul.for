
*.
*   MenuSimul - simul menu for EosMenu
*
*   History
*	03/21/89 Original
*
*	03/23/89 00:25
*..
	subroutine MenuSimul

** Includes
	include 'eosinclude:instruments.inc'
	include 'eosinclude:eossimul.inc'

	  character*100 SccsFileID
     -/'@(#)menusimul.for	5.1 98/01/08 APS/ASF\0'/
** Variables
* Menu
	integer MaxMenu
	parameter (MaxMenu=6)
	character*30 MenuI(MaxMenu)
	character MenuC(MaxMenu)
	integer iMenu
	integer itemToggle,itemInst,itemTime,itemPen
	integer itemSettings,itemHelp
	parameter (itemToggle=1,itemTime=2,itemInst=3,itemPen=4)
	parameter (itemSettings=5,itemHelp=MaxMenu)
	logical done

	integer isimul,index,iplat,iinst,ip1,ip2,slen1,slen2
	character*3 simultext(0:1)
	character YesNo

** Data
	data MenuI /'Turn simul on/off','Set simul time',
     .			'Choose simul instruments',
     .			'Set pen','Show settings','Help'/
	data MenuC /'O','T','I','P','S','?'/

	data simultext /'Off','On'/

** Functions
	integer LogToNum,StringLen

	done = .false.

	ColSimul = 255

	do while(.not.done)

	    call MenuInput('Simultaneous coverage',
     .					MenuI,MenuC,MaxMenu,iMenu)

	    if(iMenu.eq.itemToggle) then

		isimul = LogToNum(ShowSimul)
		write(*,'(/,a,a)') ' Simultaneous coverage display is ',
     .			simultext(isimul)
		write(*,*)
		isimul = 1 - isimul
10		write(*,'(3a,$)') ' Change to ',simultext(isimul),
     .			' [no]? '
		read(*,'(a)',err=10,end=900) YesNo
		if(YesNo.eq.'y' .or. YesNo.eq.'Y') then
		    ShowSimul = .not.ShowSimul
		end if
		if(ShowSimul) then
		    call GetColor(ColSimul,'for simultaneous display')
		end if

	    else if(iMenu.eq.itemInst) then

		SimulOk = .false.

		if(NumPlat.eq.1 .and. NumInstPlat(1).eq.2) then
		    simplat(1) = 1
		    siminst(1) = 1
		    simplat(2) = 1
		    siminst(2) = 2
		else if(NumPlat.eq.2 .and.
     .				NumInstPlat(1).eq.1 .and.
     .				NumInstPlat(2).eq.1) then
		    simplat(1) = 1
		    siminst(1) = 1
		    simplat(2) = 2
		    siminst(2) = 1

		else

		    simplat(1) = 0
		    siminst(1) = 0
		    simplat(2) = 0
		    siminst(2) = 0

		    do index=1,2
			write(*,'(/,a,i1,a,/)')
     .				' For instrument ',index,':'
			call GetPlatNum(iplat)
			if(iplat.ne.0) then
			    call GetInstIndex(iplat,iinst)
			    if(iinst.ne.0) then
				if(tracks(platform(iplat,iinst))
     .						.gt.1) then
				    simplat(index) = iplat
				    siminst(index) = iinst
				end if
			    end if
			end if
		    end do

		end if

		if(simplat(1).ne.0 .and. siminst(1).ne.0
     .		  .and. simplat(2).ne.0 .and. siminst(2).ne.0
     .		  .and. (simplat(1).ne.simplat(2) .or.
     .			 siminst(1).ne.siminst(2))) then
		    ip1 = platform(simplat(1),siminst(1))
		    ip2 = platform(simplat(2),siminst(2))
		    if(tracks(ip1).gt.1 .and. tracks(ip2).gt.1) then
			SimulOk = .true.
		    else
			write(*,'(2(/,a))')
     .	' Selected instruments are not appropriate for simul display.',
     .	' Please use instruments that display more than one track'
			call WaitKey
		    end if
		end if

	    else if(iMenu.eq.itemTime) then

		write(*,*)
20		write(*,'(a,$)') ' Enter simultaneity bias time (min): '
		read(*,'(f20.0)',err=20,end=900) tBias
		if(tBias.lt.0) go to 20
		tBias = tBias * 60.0

	    else if(iMenu.eq.itemPen) then

		call GetColor(ColSimul,'for simultaneous display')

	    else if(iMenu.eq.itemSettings) then

		write(*,'(/,2a,/,a,f7.1,a)')
     .			' Simultaneous coverage display is ',
     .			simultext(LogToNum(ShowSimul)),
     .			' Bias time:',tBias/60.0,' min'

		if(SimulOk) then
		    ip1 = platform(simplat(1),siminst(1))
		    ip2 = platform(simplat(2),siminst(2))
		    slen1 = StringLen(InstName(ip1))
		    slen2 = StringLen(InstName(ip2))
		    write(*,'(2(/,a,i2,a,a))')
     .			' Inst 1 on platform',simplat(1),' is ',
     .			InstName(ip1)(1:slen1),
     .			' Inst 2 on platform',simplat(2),' is ',
     .			InstName(ip2)(1:slen2)
		else
		    write(*,'(/,a)') ' No instruments chosen yet'
		end if

		call WaitKey

	    else if(iMenu.eq.itemHelp) then

	    else if(iMenu.eq.0) then
		done = .true.
	    end if

900	    continue

	end do

	end
