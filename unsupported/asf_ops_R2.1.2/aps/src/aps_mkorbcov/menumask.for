
*.
*   MenuMask - ground station mask menu for EosMenu
*
*   History
*	 3/26/91 added MaxElev() display to Settings display
*	 5/ 7/90 changed InitMask to pass a filename
*	 7/26/89
*
*   Last modified
*	3/26/91 10:09
*..
	subroutine MenuMask

** Includes
	include 'eosinclude:files.inc'
	include 'eosinclude:cbody.inc'
	include 'eosinclude:station.inc'
	include 'eosinclude:instruments.inc'
	include 'eosinclude:orbit.inc'
	include 'eosinclude:constants.inc'

	  character*100 SccsFileID
     -/'@(#)menumask.for	5.1 98/01/08 APS/ASF\0'/
** Variables
* Menu
	integer MaxMenu
	parameter(MaxMenu=5)
	character*30 MenuI(MaxMenu)
	character MenuC(MaxMenu)
	integer iMenu
	integer itemChoose,itemDraw,itemPen,itemSettings,itemHelp
	parameter (itemChoose=1,itemDraw=2,itemPen=3,itemSettings=4)
	parameter (itemHelp=MaxMenu)
	logical done

	integer MaskCol,istat
	double precision h

** Data
	data MenuI /'Choose stations','Draw station mask','Pen',
     .			'Settings','Help'/
	data MenuC /'C','D','P','S','?'/
	data MaskCol /-1/

** Save
	save MaskCol

	done = .false.

	do while(.not.done)

	    call MenuInput('Station Masks',MenuI,MenuC,MaxMenu,iMenu)

	    if(iMenu.eq.itemChoose) then
		call InitMask(' ')

	    else if(iMenu.eq.itemDraw) then

*	      For now, use first platform altitude
		if(a(1).gt.0.0) then
		    h = a(1) - RBody
		else
		    write(*,'(/,a,/)') ' No platform orbit selected'
10		    write(*,'(a,$)') '   Enter platform altitude: '
		    read(*,'(f20.0)',err=10,end=900) h
		    if(h.lt.0.0) go to 10
		end if
		if(MaskCol.lt.0) call GetColor(MaskCol,'for station mask')
		call ClearTextPlane
		call DrawMask(h,MaskCol)

	    else if(iMenu.eq.itemPen) then
		call MenuPen(MaskCol,'for station masks')

	    else if(iMenu.eq.itemSettings) then
		if(NumStations .gt. 0) then
		    write(*,'(/,a)') '   #  Min El  Max El  Station'
		    do istat=1,NumStations
			if(StationSelected(istat))
     .			    write(*,'(i4,2f8.2,2x,a)')
     .				istat,MinElev(istat),MaxElev(istat),
     .					StationName(istat)
		    end do
		else
		    write(*,'(/,a)') ' No stations selected'
		end if

		call WaitKey

	    else if(iMenu.eq.itemHelp) then

		write(*,'(/,a)') ' Help not yet available'
c..aap		call WaitAWhile(2.0)
		call WaitAWhile(rtwo)

	    else if(iMenu.eq.0) then
		done = .true.

	    end if

900	    continue

	end do

	end
