
*.
*   MenuDisplay - display menu for EosMenu
*
*   History
*	 3/20/91 added call to ShowDisp in "Settings"
*	 4/17/90 added Annotate item
*	 7/21/89
*
*   Last modified
*	3/20/91 14:50
*..
	subroutine MenuDisplay

	  character*100 SccsFileID
     -/'@(#)menudisplay.for	5.1 98/01/08 APS/ASF\0'/
** Includes
	include 'eosinclude:display.inc'
	include 'eosinclude:map.inc'
	include 'eosinclude:constants.inc'

** Variables
* Menu
	integer MaxMenu
	parameter(MaxMenu=9)
	character*30 MenuI(MaxMenu)
	character MenuC(MaxMenu)
	integer iMenu
	integer itemNew,itemErase,itemFrame,itemCrop,itemAnnot
	integer itemHardcopy,itemSettings,itemSpecial,itemHelp
	parameter (itemNew=1,itemErase=2,itemFrame=3,itemCrop=4)
	parameter (itemAnnot=5,itemHardcopy=6,itemSettings=7)
	parameter(itemSpecial=8,itemHelp=MaxMenu)
	logical done

** Data
	data MenuI /'New Display','Erase Display','Frame graphics window',
     .			'Crop marks','Annotation','Hardcopy','Settings',
     .			'Special','Help'/
	data MenuC /'N','E','F','C','A','H','S','Z','?'/

	done = .false.

	do while(.not.done)

	    call MenuInput('Display',MenuI,MenuC,MaxMenu,iMenu)

	    if(iMenu .eq. itemNew) then
		call InitDisp
		call SetProj(proj,ProjArg)
	    else if(iMenu .eq. itemErase) then
		call AskErase
	    else if(iMenu .eq. itemFrame) then
		call FrameWindow
	    else if(iMenu .eq. itemCrop) then
		call CropMarks
	    else if(iMenu .eq. itemAnnot) then
		call Annot
	    else if(iMenu .eq. itemHardcopy) then
		call Hardcopy
	    else if(iMenu .eq. itemSettings) then
		call ShowDisp
		call WaitKey
	    else if(iMenu .eq. itemSpecial) then
		write(*,'(/,a)') ' Nothing here yet...'
	    else if(iMenu .eq. itemHelp) then
		write(*,'(/,a)') ' Help not yet available'
c..aap		call WaitAWhile(2.0)
		call WaitAWhile(rtwo)
	    else if(iMenu .eq. 0) then
		done = .true.
	    end if

	end do

	end
