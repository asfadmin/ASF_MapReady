
*.
*   MenuPen - pen menu for EosMenu
*
*   Args
*	col		I	input	color table index
*
*   History
*	03/18/89 Original
*
*	03/18/89 18:53
*..
	subroutine MenuPen(col,prompt)
	integer col
	character*(*) prompt

	  character*100 SccsFileID
     -/'@(#)menupen.for	5.1 98/01/08 APS/ASF\0'/
** Includes
	include 'eosinclude:display.inc'

** Variables
* Menu
	integer MaxMenu
	parameter(MaxMenu=3)
	character*30 MenuI(MaxMenu)
	character MenuC(MaxMenu)
	integer iMenu
	integer itemNew,itemSettings,itemHelp
	parameter (itemNew=1,itemSettings=2,itemHelp=MaxMenu)
	logical done

	logical err

** Data
	data MenuI /'New pen','Show pen','Help'/
	data MenuC /'N','S','?'/

	done = .false.

	do while(.not.done)

	    call MenuInput('Pen',MenuI,MenuC,MaxMenu,iMenu)

	    if(iMenu.eq.itemNew) then
		call GetColor(col,prompt)
	    else if(iMenu.eq.itemSettings) then
		call ShowPen(col)
		call WaitKey
	    else if(iMenu.eq.itemHelp) then
		write(*,'(/,a)')
     .' In this menu you may change or view the drawing pen settings'
		call WaitKey
	    else if(iMenu.eq.0) then
		done = .true.
	    end if

	end do

	end
