
*.
*   MenuCBody - central body menu for EosMenu
*
*	03/18/89 18:51
*..
	subroutine MenuCBody

** Includes
	include 'eosinclude:cbody.inc'
	include 'eosinclude:map.inc'
	include 'eosinclude:constants.inc'

** Variables
* Menu
	integer MaxMenu
	parameter(MaxMenu=3)
	character*30 MenuI(MaxMenu)
	character MenuC(MaxMenu)
	integer iMenu
	integer itemNew,itemSettings,itemHelp
	parameter(itemNew=1,itemSettings=2,itemHelp=3)
	logical done

	  character*100 SccsFileID
     -/'@(#)menucbody.for	5.1 98/01/08 APS/ASF\0'/
	integer slen

** Data
	data MenuI /'New central body','Show central body','Help'/
	data MenuC /'N','S','?'/

** Functions
	integer StringLen

	done = .false.

	do while(.not.done)

	    call MenuInput('Central Body',MenuI,MenuC,MaxMenu,iMenu)

	    if(iMenu.eq.itemNew) then
c..aap		call InitBody(0)
		call InitBody(izero)
		call SetProj(proj,ProjArg)
	    else if(iMenu.eq.itemSettings) then
		slen = StringLen(NameBody)
		write(*,'(/,2a)') ' Central body is ',NameBody(1:slen)
		call WaitKey
	    else if(iMenu.eq.itemHelp) then
		write(*,'(/,a)')
     .	' In this menu you may change or view the central body'
		call WaitKey
	    else if(iMenu.eq.0) then
		done = .true.
	    end if

	end do

	end
