
*.
*   MenuFly - fly menu for EosMenu
*
*   History
*	 5/ 7/91 Change time step input to min,sec
*	 6/11/90 Change hours/days inputs to days,hours,minutes,seconds
*	 3/22/89 Original
*
*   Last modified
*	 5/7/91 10:51
*..
	subroutine MenuFly(filename,input,dtmin,dtsec)

** Includes
	include 'eosinclude:instruments.inc'
	include 'eosinclude:orbit.inc'
	include 'eosinclude:cbody.inc'
	include 'eosinclude:constants.inc'
	include 'eosinclude:eossimul.inc'

	  character*100 SccsFileID
     -/'@(#)menufly.for	5.1 98/01/08 APS/ASF\0'/
** Variables
* Menu
	integer MaxMenu
	parameter(MaxMenu=6)
	character*30 MenuI(MaxMenu)
	character MenuC(MaxMenu)
*	integer iMenu
	integer itemTime,itemStep,itemFly,itemEnable,itemShow,itemHelp
	parameter (itemTime=1,itemStep=2,itemFly=3,itemEnable=4)
	parameter (itemShow=5,itemHelp=MaxMenu)
*	logical done

* Platform
	integer iplat
	character*8 ActiveText(0:1)
	character*80 filename 

* Fly
	double precision period,tflight,tdefault
	double precision dtmin,dtsec,dt
	integer iday1,ihour1,imin1,iday2,ihour2,imin2
	double precision rsec1,rsec2,rrev
	character*2 input
	logical first(MAXPLAT)

* Other
	integer slen

** Data
	data MenuI /'Set flight times','Set time step','Fly',
     .			'Enable platform','Show settings','Help'/
	data MenuC /'T','D','F','E','S','?'/

	data ActiveText /'Inactive','Active'/
	data dt /60.0d0/

** Functions
	integer StringLen

*	done = .false.

*	if(NumPlat.le.0) done = .true.

*	do while(.not.done)

*	    call MenuInput('Fly',MenuI,MenuC,MaxMenu,iMenu)

*	    if(iMenu.eq.itemTime) then

*		call GetPlatNum(iplat)
		iplat = 1
		if(iplat.ne.0) then
	
c		    period = twopi * sqrt(a(iplat)**3/mu)
		    if(a(iplat).gt.RBody) then
			call NodePeriod(a(iplat),i(iplat),period)
		    else
			write(*,'(2(/,a))')
     .	' No orbit chosen yet - do not enter times using number',
     .	'   of revolutions'
			period = 0.0
		    end if
	
*		    write(*,*)
*10		    write(*,'(2a,/,a)')
*     .			' Enter tstart - (days,hours,minutes,seconds',
*     .			' or "R" and the number of revs)',
*     .			'   (for example, 1,12,0,0 or R 21): '
		    if(.not.first(iplat)) then
			tdefault = tend(iplat)
			call istodhms(tdefault,iday1,ihour1,imin1,rsec1)
*			write(*,11) iday1,ihour1,imin1,rsec1
*11			format('     [',i4,'/',i2.2,':',
*     .					i2.2,':',f4.1',]: ',$)
		    else
			tdefault = 0.0
			first(iplat) = .false.
		    end if
*		    read(*,'(a)',err=10,end=900) input
*		    if(input(1:1).eq.' ') then
			tstart(iplat) = tdefault
*		    else if(input(1:1).eq.'r' .or. input(1:1).eq.'R') then
*			slen = StringLen(input)
*			read(input(2:slen),'(f20.0)',err=10,end=900) rrev
*			tstart(iplat) = period * (rrev-1.0)
*		    else
*			slen = StringLen(input)
*			read(input(1:slen),'(3i10,f20.0)',err=10,end=900)
*     .				iday1,ihour1,imin1,rsec1
*			call idhmstos(iday1,ihour1,imin1,rsec1,tstart(iplat))
*		    end if
	
c		    if(input.ne.' ' .and. .not.FirstSimul) then
c			*** Did not choose default, so reset simul variables ***
c			call InitSimul(dt,tBias,tracks(ipsim1),tracks(ipsim2))
c		    end if
	
*		    write(*,*)
20		    write(*,'(a)')
*     .' Enter flight duration - (time or "R" and number of revs): '
*		    read(*,'(a)',err=10,end=900) input
		    if(input(1:1).eq.'r' .or. input(1:1).eq.'R') then
			slen = StringLen(input)
			read(input(2:slen),'(f20.0)',err=20,end=900) rrev
			tflight = period * rrev
		    else
			slen = StringLen(input)
			read(input(1:slen),'(3i10,f20.0)',err=20,end=900)
     .				iday2,ihour2,imin2,rsec2
			call idhmstos(iday2,ihour2,imin2,rsec2,tflight)
		    end if
	
		    tend(iplat) = tstart(iplat) + tflight
	
		end if

*	    else if(iMenu.eq.itemStep) then

*		write(*,*)
*30		write(*,'(a,$)') ' Enter time step (min,sec) [1,0]: '
*		read(*,'(2f20.0)',err=30,end=900) dtmin,dtsec
		if(dtmin.eq.0.0 .and. dtsec.eq.0.0) dtmin = 1.0
		dt = 60.0d0 * dtmin + dtsec

*	    else if(iMenu.eq.itemEnable) then

*		call GetPlatNum(iplat)
*		if(iplat.ne.0) then
*		    active(iplat) = .not.active(iplat)
c		    change platform eos-a (#1) to inactive [no]? '
*		end if

*	    else if(iMenu.eq.itemFly) then

*		if(.not.ShowSimul) then
		    call FlyIt(filename,dt)
*		else
*		    call FlyItSimul(dt)
*		end if

*	    else if(iMenu.eq.itemShow) then

*		write(*,*)
*		do iplat=1,NumPlat
*		    slen = StringLen(PlatName(iplat))
*		    call istodhms(tstart(iplat),iday1,ihour1,imin1,rsec1)
*		    call istodhms(tend(iplat),iday2,ihour2,imin2,rsec2)
*		    write(*,50) iplat,PlatName(iplat)(1:slen),
*     .			ActiveText(LogToNum(active(iplat))),
*     .			iday1,ihour1,imin1,rsec1,
*     .			iday2,ihour2,imin2,rsec2
*50		    format(i2,1x,a,/,5x,a,/,5x,'tstart,tend (d,h,m,s):',
*     .				2(i6,'/',i2.2,':',i2.2,':',f4.1))
*		end do
*		write(*,'(/,a,f7.3,a)') ' Time step:',dt/60.0,' min'
*		if(ShowSimul) then
*		    write(*,*)
*		    write(*,*) 'Simultaneous coverage'
*		    if(NumPlat.gt.2) then
*			write(*,'(2(i3,1x,a))')
*     .				simplat(1),PlatName(simplat(1)),
*     .				simplat(2),PlatName(simplat(2))
*		    end if
*		end if
*		call WaitKey

*	    else if(iMenu.eq.itemHelp) then
*		write(*,'(4(/,a))')
*     .' In this menu you may set flight start and stop times,',
*     .'   change the plotting time step, activate a platform',
*     .'   for flight, fly all active platforms or show platform',
*     .'   flight status'
*		call WaitKey

*	    else if(iMenu.eq.0) then
*		done = .true.
*	    end if

900	    continue

*	end do

	end
