
*.
*   MenuMap - map menu for EosMenu
*
*	04/20/89 08:41
*..
	subroutine MenuMap

** Includes
	include 'eosinclude:files.inc'
	include 'eosinclude:cbody.inc'
	include 'eosinclude:map.inc'
	include 'eosinclude:constants.inc'

	  character*100 SccsFileID
     -/'@(#)menumap.for	5.1 98/01/08 APS/ASF\0'/
** Variables
* Menu
	integer MaxMenu
	parameter(MaxMenu=7)
	character*30 MenuI(MaxMenu)
	character MenuC(MaxMenu)
	integer iMenu
	integer itemNew,itemDraw,itemGrid,itemTargets
	integer itemPen,itemSettings,itemHelp
	parameter (itemNew=1,itemDraw=2,itemGrid=3,itemTargets=4)
	parameter (itemPen=5,itemSettings=6)
	parameter (itemHelp=MaxMenu)
	logical done

	character*80 MapFile
	integer MapCol,GridCol
	double precision GridDLatd,GridDLon
	logical err
	logical DoFrame,DoTicks,DoGrid
	character*3 onofftext(0:1)

** Functions
	integer LogToNum

** Data
	data MenuI /'New Map','Draw Map','Draw Grid','Draw Targets',
     .			'Pen','Settings','Help'/
	data MenuC /'N','D','G','T','P','S','?'/

	data MapCol /255/
	data GridCol /-1/
	data onofftext /'Off','On'/
** Save
	save MapFile,MapCol,GridCol,GridDLatd,GridDLon

	DoFrame = .true.
	DoTicks = .true.

	done = .false.

	do while(.not.done)

	    call MenuInput('Map',MenuI,MenuC,MaxMenu,iMenu)

	    if(iMenu.eq.itemNew) then
		call AskErase
		call GetProj(err)

	    else if(iMenu.eq.itemDraw) then

		call UseCol(MapCol)
		call cleart
		if(DoFrame) call FrameWindow
		if(DoTicks) call MapTicks

		if(CentralBody.eq.EARTH) then
		    MapFile = 'hers.unx'
		else if(CentralBody.eq.VENUS) then
		    MapFile = 'venus.map'
		else
		    MapFile = 'null.map'
		end if

c..aap		call drawmap(DataDir//MapFile,888.8,999.9,err)
		call drawmap(DataDir//MapFile,d888,d999,err)

	    else if(iMenu.eq.itemGrid) then
c		if(GridDLatd.le.0.0 .and. GridDLon.le.0.0) then
		    write(*,*)
20		    write(*,'(a,$)') ' Enter latd,lon grid spacing [15,15]: '
		    read(*,'(2f20.0)',err=20,end=900) GridDLatd,GridDLon
		    if(GridDLatd.le.0.0 .and. GridDLon.le.0.0) then
			GridDLatd = 15.0
			GridDLon = 15.0
		    end if
c		end if
c		if(GridCol.lt.0) then
		    call GetColor(GridCol,'for Grid')
c		end if
		call cleart
		call MapGrid(GridDLatd,GridDLon,GridCol)

	    else if(iMenu.eq.itemTargets) then
		call DrawTargets

	    else if(iMenu.eq.itemPen) then
		call MenuPen(MapCol,'for map')

	    else if(iMenu.eq.itemSettings) then
		call ShowProj
		write(*,'(2a)')   ' Frame is ',
     .			onofftext(LogToNum(DoFrame))
		if(RectProj) write(*,'(2a)')   ' Ticks are ',
     .			onofftext(LogToNum(DoTicks))
		call ShowPen(MapCol)
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
