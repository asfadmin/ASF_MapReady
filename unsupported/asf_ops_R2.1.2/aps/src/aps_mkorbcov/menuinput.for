
*.
*   MenuInput - menu display and input
*
*   Args
*	MenuPrompt	C**	input	prompt string
*	MenuI		C**()	input	menu item names
*	MenuC		C()	input	menu item characters
*	NumMenu		I	input	number of menu items
*	iMenu		I	output	menu item selected (0=none)
*
*   Uses
*	ToUpperS - convert characters to uppercase
*
*	10/31/88 14:06
*..
	subroutine MenuInput(MenuPrompt,MenuI,MenuC,NumMenu,iMenu)
	character*(*) MenuPrompt,MenuI(*)
	character MenuC(*)
	integer NumMenu,iMenu

	  character*100 SccsFileID
     -/'@(#)menuinput.for	5.1 98/01/08 APS/ASF\0'/
	integer im
	character MenuChoice,esc

	esc = char(27)

	call NewPage
	write(*,'(1x,a,/)') MenuPrompt

	iMenu = 0

	do 100 im=1,NumMenu
	    write(*,10) MenuC(im),MenuI(im)
10	    format(t5,a,': ',a)
100	continue

	write(*,*)
200	write(*,'(t5,a,$)') 'Select [none]: '
	read(*,'(a)',err=200,end=999) MenuChoice

	if(MenuChoice.ne.' ') then
	    call ToUpperS(MenuChoice)
	    do im=1,NumMenu
		if(MenuChoice.eq.MenuC(im)) then
		    iMenu = im
		    go to 999
		end if
	    end do
	end if

999	continue
	end
