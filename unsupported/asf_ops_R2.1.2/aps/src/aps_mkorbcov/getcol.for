*.
*   GetCol(col,string) - get pen setup (color, style, width, ...)
*
*	08/03/89 11:54
*..
	subroutine GetCol(col,string)
	include 'eosinclude:display.inc'
	include 'eosinclude:constants.inc'
	integer col
	character*(*) string

	  character*100 SccsFileID
     -/'@(#)getcol.for	5.1 98/01/08 APS/ASF\0'/
	integer r,g,b
	double precision intens
	integer idots
	integer linetype

	integer ios
	logical init

** Common
	common /saveGetCol/ init
** Data
	data init /.true./

	ios = 0

1	continue
	if(init .or. ios.ne.0) call ColHelpText
	init = .false.

	if(disp.eq.RAM9460 .or. disp.eq.JUPJ .or. disp.eq.JUP7) then
10	    write(*,11) string
11	    format(1x,'Enter color # and red,green,blue ',
     .			a,': ',$)
	    read(*,'(4i10)',err=1,iostat=ios,end=999) col,r,g,b
	    if(col.eq.0 .and. r.eq.0 .and. g.eq.0 .and. b.eq.0) then
		col = 255
		r = 255
		g = 255
		b = 255
	    end if

	    if(col.lt.0) then
		write(*,'(/,a)') '    Please use a positive color #'
		go to 10
	    end if

	    r = min(max(0,r),255)
	    g = min(max(0,g),255)
	    b = min(max(0,b),255)

	else if(disp.eq.VT220 .or. disp.eq.TEK4014
     .				.or. disp.eq.TEK4014SEL) then
15	    write(*,16) string
16	    format(1x,'Enter style number and line type ',
     .			a,': ',$)
	    read(*,'(2i10)',err=1,iostat=ios,end=999) col,linetype

	    if(col.lt.0 .or. col.gt.4) then
		write(*,'(/,a)') '    Please use a style in the range (0-4)'
		go to 15
	    end if

	    if(linetype.lt.0 .or. linetype.gt.4) then
		write(*,'(/,a)')
     .			'    Please use a line type in the range (0-4)'
		go to 15
	    end if

c..aap	    call SetLine(col,linetype,0,0,0,0.0,0.0)
	    call SetLine(col,linetype,izero,izero,izero,dzero,dzero)

	else if(disp.eq.TEK4027) then
20	    write(*,21) string
21	    format(1x,'Enter color # and red,green,blue ',
     .			a,': ',$)
	    read(*,'(4i10)',err=1,iostat=ios,end=999) col,r,g,b

	    if(col.lt.0 .or. col.gt.7) then
		write(*,'(/,a)') '    Please use a color in the range 0-7'
		go to 20
	    end if

	    r = min(max(0,r),100)
	    g = min(max(0,g),100)
	    b = min(max(0,b),100)

	else if(disp.eq.POSTSCRIPT) then

25	    write(*,26) string
26	    format(1x,'Enter style #, line intensity, width, line type ',
     .			a,': ',$)
	    read(*,'(i10,f20.0,2i10)',err=1,iostat=ios,end=999)
     .			col,intens,idots,linetype

	    if(col.eq.0 .and. intens.eq.0.0 .and. idots.eq.0
     .			.and. linetype.eq.0) then
		col = 0
		intens = 0.0
		idots = 2
		linetype = 0
	    end if

	    if(col.lt.0) then
		write(*,'(/,a)') '    Please use a positive style #'
		go to 25
	    end if

	    if(intens.lt.0.0 .or. intens.gt.1.0) then
		write(*,'(/,a)')
     .			'    Please use an intensity in the range (0-1)'
		go to 25
	    end if

	    if(idots.lt.0) then
		write(*,'(/,a)') '    Please use a positive number of dots'
		go to 25
	    end if

	    if(linetype.lt.0 .or. linetype.gt.4) then
		write(*,'(/,a)')
     .			'    Please use a line type in the range (0-4)'
		go to 25
	    end if

c..aap	    call SetLine(col,linetype,0,0,0,intens,dble(idots)/0.3)
	    call SetLine(col,linetype,izero,izero,izero,intens,
     .		dble(idots)/0.3)

	else if(disp.eq.TEK4105MAC .or. disp.eq.TEK4208) then
30	    write(*,31) string
31	    format(1x,'Enter style # and line index ',
     .			a,': ',$)
	    read(*,'(2i10)',err=1,iostat=ios,end=999) col,r

	    if(col.eq.0 .and. r.eq.0) then
		col = 0
		r = 1
	    end if
	    if(col.lt.0 .or. col.gt.255) go to 30
	    if(r.lt.0 .or. r.gt.15) go to 30

c..aap	    call SetLine(col,r,0,0,0,0.0,0.0)
	    call SetLine(col,r,izero,izero,izero,dzero,dzero)

	end if

	call SetCol(col,r,g,b)

999	continue
	end


	subroutine ColHelpText

	include 'eosinclude:display.inc'

	if(disp.eq.RAM9460 .or. disp.eq.JUPJ .or. disp.eq.JUP7) then
	    write(*,'(/,a/)') ' *** Color table setup ***'
	    write(*,'(a,a)') ' You have 256 colors to choose ',
     .				'from (0-255)'
	    write(*,'(a,a)') ' Select red, green and blue ',
     .				'intensities in the range 0-255 (0=dark)'
	    write(*,'(a)') ' Default is white, color 255'
	    write(*,*)
	else if(disp.eq.VT220 .or. disp.eq.TEK4014
     .					.or. disp.eq.TEK4014SEL) then
	    write(*,'(/,a,/)') ' *** Line type selection ***'
	    write(*,'(a)') ' You have 5 styles to use (0-4)'
	    write(*,'(a,a)') ' To each style you can assign one of 5',
     .				' line types'
	    write(*,'(a)') '   0: Solid'
	    write(*,'(a)') '   1: Dotted'
	    write(*,'(a)') '   2: Dot-dash'
	    write(*,'(a)') '   3: Short dash'
	    write(*,'(a)') '   4: Long dash'
	    write(*,'(a)') ' Default is style 0, solid lines'
	    write(*,*)
	else if(disp.eq.TEK4027) then
	    write(*,'(/,a,/)') ' *** Color table setup ***'
	    write(*,'(a,a)') ' You have 8 colors to choose ',
     .				'from (0-7)'
	    write(*,'(a,a)') ' Select red, green and blue ',
     .				'intensities in the range 0-100 (0=dark)'
	    write(*,'(a)') ' Default is white, color 0'
	    write(*,*)
	else if(disp.eq.POSTSCRIPT) then
	    write(*,'(/,a/)') ' *** Line type selection ***'
	    write(*,'(a,a)') ' You have 256 styles to choose ',
     .				'from (0-255)'
	    write(*,'(a,a)') ' Select line intensity in the range',
     .				' 0-1 (0=black)'
	    write(*,'(a,a)') ' Select line width in dots',
     .				' (1 dot = 1/300 inch)'
	    write(*,'(a)') ' Select line type'
	    write(*,'(a)') '   0: Solid'
	    write(*,'(a)') '   1: Dotted'
	    write(*,'(a)') '   2: Dot-dash'
	    write(*,'(a)') '   3: Short dash'
	    write(*,'(a)') '   4: Long dash'
	    write(*,'(a)') ' Default is style 0, black, 2 dots, solid'
	    write(*,*)
	else if(disp.eq.TEK4105MAC .or. disp.eq.TEK4208) then
	    write(*,'(/,a/)') ' *** Line Index selection ***'
	    write(*,'(a,a)') ' You have 256 styles to choose ',
     .				'from (0-255)'
	    write(*,'(a,a)') ' Select a line index in the range',
     .				' (0-15)'
	    write(*,'(a)') ' Default is style 0, index 1'
	    write(*,*)
	end if

	end
