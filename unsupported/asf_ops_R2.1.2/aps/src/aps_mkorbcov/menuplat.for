
*.
*   MenuPlat() - platform menu for EosMenu
*
*   History
*	01/14/91 Added 'Mask?' to selected instrument listing
*	04/20/90 Removed 'Setup inst' and 'Instru info' items
*	07/28/89
*
*   Last modified
*	01/14/91 10:07
*..
	subroutine MenuPlat

** Includes
	include 'eosinclude:instruments.inc'
	include 'eosinclude:eossimul.inc'
	include 'eosinclude:orbit.inc'
	include 'eosinclude:constants.inc'

	  character*100 SccsFileID
     -/'@(#)menuplat.for	5.1 98/01/08 APS/ASF\0'/
** Variables
* Menu
	integer MaxMenu
	parameter(MaxMenu=8)
	character*30 MenuI(MaxMenu)
	character MenuC(MaxMenu)
	integer iMenu
	integer itemNew,itemOrbit,itemInst
	integer itemList,itemRead,itemWrite,itemRemove,itemHelp
	parameter (itemNew=1,itemOrbit=2,itemInst=3)
	parameter (itemList=4,itemRead=5,itemWrite=6)
	parameter (itemRemove=7,itemHelp=MaxMenu)
	logical done

	integer iplat,iinst,ip
	character YesNo
	character*80 FilePlat
	integer imask
	logical usemask
	character maskchar

** Data
	data MenuI /'New platform','Orbit setup','Instruments',
     .			'List platforms',
     .			'Read platform data','Write platform data',
     .			'Remove last platform','Help'/
	data MenuC /'N','O','I','L','R','W','X','?'/

** Functions

	done = .false.

	do while(.not.done)

	    call MenuInput('Platform',MenuI,MenuC,MaxMenu,iMenu)

	    if(iMenu.eq.itemNew) then
		if(NumPlat.lt.MAXPLAT) then
		    iplat = NumPlat + 1
		    write(*,*)
10		    write(*,'(a,i1,a,$)') ' Enter name for platform #',
     .						iplat,': '
		    read(*,'(a)',err=10,end=900) PlatName(iplat)
		    active(iplat) = .true.
		    NumPlat = iplat
		    NumInstPlat(iplat) = 0
		else
		    write(*,'(/,a)')
     .			' Already have maximum number of platforms!'
		    call WaitKey
		end if
	    else if(iMenu.eq.itemOrbit) then
		call MenuOrbit
	    else if(iMenu.eq.itemInst) then
		call MenuInst
	    else if(iMenu.eq.itemList) then
		write(*,*)
		do iplat=1,NumPlat
		    write(*,'(1x,i2,1x,a)') iplat,PlatName(iplat)
		    if(NumInstPlat(iplat).gt.0) then
			write(*,'(a)')
     .'                         Look      Yaw     BeamWid
     .  SwathWid Mask?'
c            1 FooBarSpam      40.123   -90.000    99.057   100.123   *
c      xxxxx01xaaaaaaaaaaxxsfffff.fffsfffff.fffsfffff.fffsfffff.fffxxxa
		    end if
		    do iinst=1,NumInstPlat(iplat)
			ip = platform(iplat,iinst)
			usemask = .false.
			do imask=1,MAXMASK
			    usemask = usemask .or. mask(imask,ip)
			end do
			if(usemask) then
			    maskchar = '+'
			else
			    maskchar = ' '
			end if
			write(*,'(5x,i2,1x,a,2x,4f10.3,3x,a1)')
     .				iinst,InstName(ip),
     .				LookPoint(iplat,iinst),YawPoint(iplat,iinst),
     .				BeamWid(ip),SwathWid(ip),maskchar
		    end do
		end do
		call WaitKey

	    else if(iMenu.eq.itemRead) then
		if(NumPlat.gt.0) then
		    write(*,*)
30		    write(*,'(a,$)')
     .		    ' Really forget the current set of platforms [no]? '
		    read(*,'(a)',err=30,end=900) YesNo
		end if
		if(NumPlat.lt.1 .or.
     .			(NumPlat.ge.1 .and.
     .				(YesNo.eq.'y' .or. YesNo.eq.'Y'))) then
40		    write(*,'(a,$)') ' Enter platform filename [default]: '
		    read(*,'(a)',err=40,end=900) FilePlat
		    DO IPLAT=1,NUMPLAT
			ACTIVE(IPLAT) = .FALSE.
		    END DO
		    SHOWSIMUL = .FALSE.
		    SIMULOK = .FALSE.
		    call ReadPlatFile(FilePlat)
		end if

	    else if(iMenu.eq.itemWrite) then
		write(*,'(/,a)') ' This option not yet available'
c..aap		call WaitAWhile(2.0)
		call WaitAWhile(rtwo)
c		if(NumPlat.gt.0) then
c		    write(*,*)
c50		    write(*,'(a,$)') ' Enter platform filename: '
c		    read(*,'(a)',err=50,end=900) FilePlat
c		    call WritePlat(FilePlat)
c		else
c		    write(*,'(/,a)') ' No platforms defined yet...'
c		    call WaitKey
c		end if

	    else if(iMenu.eq.itemRemove) then
		write(*,*)
		write(*,'(a,i2,a,$)')
     .			' Really remove platform ',NumPlat,' [no]? '
		read(*,'(a)',err=900,end=900) YesNo
		if(YesNo.eq.'y' .or. YesNo.eq.'Y') then
		    a(NumPlat) = 0.0
		    e(NumPlat) = 0.0
		    i(NumPlat) = 0.0
		    node0(NumPlat) = 0.0
		    omega0(NumPlat) = 0.0
		    m0(NumPlat) = 0.0
		    t0(NumPlat) = 0.0
		    if(ShowSimul) then
			if(simplat(1).eq.NumPlat
     .				.or. simplat(2).eq.NumPlat) then
			    ShowSimul = .false.
			    SimulOk = .false.
			end if
		    end if
		    NumInstPlat(NumPlat) = 0
		    active(NumPlat) = 0
		    NumPlat = NumPlat - 1
		    write(*,'(/,a,i2)') ' Removed platform',NumPlat
		    call WaitKey
		end if
	    else if(iMenu.eq.itemHelp) then
		write(*,'(/,a)') ' Help not yet available'
c..aap		call WaitAWhile(2.0)
		call WaitAWhile(rtwo)
	    else if(iMenu.eq.0) then
		done = .true.
	    end if

900	    continue

	end do

999	continue
	end
