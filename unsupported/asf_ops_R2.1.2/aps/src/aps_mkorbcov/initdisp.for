
*.
*   InitDisp - initialize plotting variables
*
*   Uses
*	JupInit,Fj_GInit,Mouse_Init,Fj_FFlush - Jupiter J-Station library
*	RmInit - Ramtek library
*	InitPS,ToUpperS - Eos library
*
*	03/29/89 13:24
*..
	subroutine InitDisp

	include 'eosinclude:display.inc'
	include 'eosinclude:constants.inc'	! aap..11/4/93

	character cdevice*5
	integer ierr

	  character*100 SccsFileID
     -/'@(#)initdisp.for	5.1 98/01/08 APS/ASF\0'/
	integer ret,slen
	character esc

* Functions
	integer StringLen,fj_ginit

	esc = char(27)

	screenx(1) = 0
	screeny(1) = 0
	devscalex = 1.0
	devscaley = 1.0

	write(*,*)
20	write(*,'(a,$)')
     .	    ' What display device shall we use [show list]: '
	read(*,'(a)',err=20,end=999) cdevice

	if(cdevice(1:1).eq.' ') then
21	    write(*,'(/,a,/)') ' Choose one of the following:'
	    write(*,'(a)') '      R: Ramtek 9460'
	    write(*,'(a)') '     V1: VT100 (Selanar GR100)'
	    write(*,'(a)') '     V2: VT220 (Selanar SG220)'
	    write(*,'(a)') '    V24: VT240 (and other ReGIS)'
	    write(*,'(a)') '     J7: Jupiter7'
	    write(*,'(a)') '     JJ: Jupiter J-Station'
	    write(*,'(a)') '    T10: Tek 4010'
	    write(*,'(a)') '    S10: Tek 4010 Emulation (VT100 Selanar GR100)'
	    write(*,'(a)') '    T14: Tek 4014'
	    write(*,'(a)') '    S14: Tek 4014 Emulation (Selanar Hirez 100XL)'
	    write(*,'(a)') '    T27: Tek 4027'
	    write(*,'(a)') '   4105: Tek 4105 Emulation (Mac)'
	    write(*,'(a)') '    T42: Tek 420x, 410x'
	    write(*,'(a)') '      P: Printronix line printer'
	    write(*,'(a)') '     PS: PostScript'
	    write(*,'(a)') '      N: None'
	    write(*,'(/,a,$)') '   [None]: '

	    read(*,'(a)',err=21,end=999) cdevice
	end if

	call ToUpperS(cdevice)
	slen = StringLen(cdevice)

	if(cdevice(1:slen).eq.'V1') then
	    disp = VT100
	    screenx(2) = VT100X
	    screeny(2) = VT100Y
	    devscalex = 3.25
*	  Turn on graphics display in case it was toggled off
	    write(*,'(5a,$)') '+',esc1,escb,escdq,esc2
	else if(cdevice(1:slen).eq.'T10') then
	    disp = TEK4010
	    screenx(2) = TEK4010X
	    screeny(1) = TEK4010Y
	    screeny(2) = 0
	else if(cdevice(1:slen).eq.'S10') then
	    disp = TEK4010SEL
	    screenx(2) = TEK4010SELX
	    screeny(1) = TEK4010SELY
	    screeny(2) = 0
	else if(cdevice(1:slen).eq.'V2') then
	    disp = VT220
	    screenx(2) = VT220X
	    screeny(1) = VT220Y
	    screeny(2) = 0
	    devscalex = 0.8
	    call TerminalSet
	else if(cdevice(1:slen).eq.'T14') then
	    disp = TEK4014
	    screenx(2) = TEK4014X
	    screeny(1) = TEK4014Y
	    screeny(2) = 0
	else if(cdevice(1:slen).eq.'S14') then
	    disp = TEK4014SEL
	    screenx(2) = TEK4014SELX
	    screeny(1) = TEK4014SELY
	    screeny(2) = 0
	    call TerminalSet
	else if(cdevice(1:slen).eq.'4027') then
	    disp = TEK4027
	    screenx(2) = TEK4027X
	    screeny(1) = TEK4027Y
	    screeny(2) = 0
*	  Use bottom 12 lines for text, use lines 1-22 (cols 1-80 ?) for graphics
	    write(*,'(a)') '+!MON 12 H K'
	    write(*,'(a)') '+!GRA 1 22 1 80'
	else if(cdevice(1:slen).eq.'V24') then
	    disp = VT240
	    screenx(2) = VT240X
	    screeny(2) = VT240Y
	else if(cdevice(1:slen).eq.'J7') then
	    disp = JUP7
	    screenx(2) = JUP7X
	    screeny(1) = JUP7Y
	    screeny(2) = 0
	    call JupInit
	else if(cdevice(1:slen).eq.'JJ') then
	    disp = JUPJ
	    screenx(2) = JUPJX
	    screeny(1) = JUPJY
	    screeny(2) = 0
	    ret = fj_ginit(0,0,2,1,0,4)
	    if(ret.eq.0) then
		write(*,'(a)') ' Error - couldn''t initialize Jupiter'
		stop
	    end if
c..aap	    call mouse_init(1,15,15,15)
	    call mouse_init(ione,i15,i15,i15)
	    call fj_fflush
	else if(cdevice(1:slen).eq.'4105') then
	    disp = TEK4105MAC
	    screenx(1) = 0
	    screeny(1) = TEK4105MACY
	    screenx(2) = TEK4105MACX
	    screeny(2) = 0
*	  Setups
c	    write(*,'(3a,$)') '+',esc,'%!0'	! set mode (TEK - 2=VT100)
c	    write(*,'(3a,$)') '+',esc,'RE0'	! set border (invisible)
c	    write(*,'(3a,$)') '+',esc,'TM111'	! set color mode (RGB,opaque,)
c	    write(*,'(3a,$)') '+',esc,'KA1'	! enable dialog area
c	    write(*,'(3a,$)') '+',esc,'LV1'	! set dialog area (visible)
c	    write(*,'(3a,$)') '+',esc,'LBA<'	! set dialog area buffer (28)
c	    write(*,'(3a,$)') '+',esc,'LLA8'	! set dialog area lines (24)
	    write(*,'(3a,$)') '+',esc,'MV0'	! set line style (solid)
	    write(*,'(3a,$)') '+',esc,'MM0'	! set marker type (dot)
	else if(cdevice(1:slen).eq.'T42') then
	    disp = TEK4208
	    screenx(1) = 0
	    screeny(1) = TEK4208Y
	    screenx(2) = TEK4208X
	    screeny(2) = 0
*	  Setups
	    write(*,'(3a,$)') '+',esc,'%!0'	! set mode (TEK - 2=VT100)
	    write(*,'(3a,$)') '+',esc,'RE0'	! set border (invisible)
	    write(*,'(3a,$)') '+',esc,'TM111'	! set color mode (RGB,opaque,)
	    write(*,'(3a,$)') '+',esc,'KA1'	! enable dialog area
	    write(*,'(3a,$)') '+',esc,'LV1'	! set dialog area (visible)
	    write(*,'(3a,$)') '+',esc,'LBA<'	! set dialog area buffer (28)
	    write(*,'(3a,$)') '+',esc,'LLA8'	! set dialog area lines (24)
	    write(*,'(3a,$)') '+',esc,'MV0'	! set line style (solid)
	    write(*,'(3a,$)') '+',esc,'MM0'	! set marker type (dot)
	else if(cdevice(1:slen).eq.'P') then
	    disp = PRINTX
	    screenx(2) = PRINTXX
	    screeny(2) = PRINTXY
	    devscalex  = 0.834
	else if(cdevice(1:slen).eq.'PS') then
	    disp = POSTSCRIPT
	    screenx(1) = 0
	    screenx(2) = POSTSCRIPTX
	    screeny(2) = 0
	    screeny(1) = POSTSCRIPTY
	    call InitPS
	else if(cdevice(1:slen).eq.'R') then
	    disp = RAM9460
	    screenx(2) = RAM9460X
	    screeny(2) = RAM9460Y
	    call RmInit(ierr)
c	else if(cdevice(1:slen).eq.'N') then
	else
	    disp = NONE
	    screenx(2) = 1000
	    screeny(2) = 1000
	end if

	call PageSetup

	call InitPen

999	continue
	end
