
*.
*   MenuSpecial - goodies menu for EosMenu
*
*	07/21/89 09:46
*..
	subroutine MenuSpecial

** Includes
	include 'eosinclude:constants.inc'

	  character*100 SccsFileID
     -/'@(#)menuspecial.for	5.1 98/01/08 APS/ASF\0'/
** Variables
* Menu
	integer MaxMenu
	parameter(MaxMenu=5)
	character*30 MenuI(MaxMenu)
	character MenuC(MaxMenu)
	integer iMenu
	integer itemNew,itemErase,itemHardcopy,itemSettings,itemHelp
	parameter(itemNew=1,itemErase=2,itemHardcopy=3,itemSettings=4)
	parameter(itemHelp=MaxMenu)
	logical done

** Data
	data MenuI /'New Display','Erase Display','Hardcopy','Settings',
     .			'Help'/
	data MenuC /'N','E','H','S','?'/

	write(*,'(/,a)') ' Nothing here yet...'
c..aap	call WaitAWhile(2.0)
	call WaitAWhile(rtwo)

c	done = .false.
c
c	do while(.not.done)
c
c	    call MenuInput('Special',MenuI,MenuC,MaxMenu,iMenu)
c
c	end do

	end
