
*.
*   MenuEpoch - epoch menu for EosMenu
*
*	04/20/90 changed 'Change epoch' to 'New epoch'
*	03/13/89 original
*
*	04/20/90 17:14
*..
	subroutine MenuEpoch

** Includes
	include 'eosinclude:time.inc'
	include 'eosinclude:constants.inc'

	  character*100 SccsFileID
     -/'@(#)menuepoch.for	5.1 98/01/08 APS/ASF\0'/
** Variables
* Menu
	integer MaxMenu
	parameter(MaxMenu=3)
	character*30 MenuI(MaxMenu)
	character MenuC(MaxMenu)
	integer iMenu
	integer itemChange,itemSettings,itemHelp
	parameter(itemChange=1,itemSettings=2,itemHelp=3)
	logical done

** Data
	data MenuI /'New epoch','Show epoch','Help'/
	data MenuC /'N','S','?'/

	done = .false.

	do while(.not.done)

	    call MenuInput('Epoch',MenuI,MenuC,MaxMenu,iMenu)

	    if(iMenu.eq.itemChange) then
c..aap		call InitEpoch(0,0.0)
		call InitEpoch(izero,dzero)
	    else if(iMenu.eq.itemSettings) then
		write(*,1) BaseYear,BaseDay,BaseHour,BaseMin,BaseSec
1		format(/,' Epoch is ',i4,1x,i3.3,'/',i2.2,':',i2.2,':',f4.1)
		call WaitKey
	    else if(iMenu.eq.itemHelp) then
		write(*,*)
		write(*,'(99(/,a))')
     .' The Sun''s location and platform flight times are referenced',
     .'   to the year, day of year, and Greenwich Mean Time (GMT)',
     .'   entered here'
		call WaitKey
	    else if(iMenu.eq.0) then
		done = .true.
	    end if

	end do

	end
