
*.
*   PageSetup - set graphics orientation
*
*	10/17/88 12:26
*..
	subroutine PageSetup

	include 'eosinclude:display.inc'

	character YesNo,input

	  character*100 SccsFileID
     -/'@(#)pagesetup.for	5.1 98/01/08 APS/ASF\0'/
	integer PaperSize,LETTER,LEGAL
	parameter(LETTER=1,LEGAL=2)
	integer orient,LANDSCAPE,PORTRAIT
	parameter(LANDSCAPE=1,PORTRAIT=2)

	real marginl,marginr,margint,marginb,minmargin
	real pageshort,pagelong

	if(disp.ne.POSTSCRIPT) then

	    write(*,*)
10	    write(*,'(a,$)')
     .		' Enter horizontal, vertical scale factors [none]: '
	    if(disp.eq.RAM9460) then
		write(*,'(a,$)')
     .			'  (1 = single page, 2 = double page) [none]: '
	    end if

	    read(*,'(2g20.0)',err=10,end=999) xPageMult,yPageMult
	    if(yPageMult.eq.0.) then
		if(xPageMult.eq.0.) then
		    xPageMult = 1.
		    yPageMult = 1.
		else if(xPageMult.eq.1. .and. disp.eq.RAM9460) then
		    xPageMult = .6
		    yPageMult = .6
		else if(xPageMult.eq.2. .and. disp.eq.RAM9460) then
		    xPageMult = 1.
		    yPageMult = 780./1024.
		else
		    go to 10
		end if
	    end if
    
	    if(disp.eq.RAM9460) then
20	    write(*,'(a,$)') ' Scale for printed output [no]? '
		read(*,'(a1)',err=20,end=999) YesNo
		if(YesNo.eq.'Y' .or. YesNo.eq.'y') DevScaleX = 0.834
	    end if
****    
* This assumes that the min screen coord is 0 - should scale the WIDTH by
*   the scale factor and set screen%(2) accordingly
*
	    screenx(1) = screenx(1) * xPageMult
	    screeny(1) = screeny(1) * yPageMult
	    screenx(2) = screenx(2) * xPageMult
	    screeny(2) = screeny(2) * yPageMult
*
****
	else

c PostScript output
c
c   Assumptions
c     Page Width is 8.5 inches
c     Minimum margin is 0.5 inches on all sides
c     Measurements in output file are in 1000ths of an inch
c     Our origin is the lower left corner of the output page plus the minimum
c       margin on both sides

	    write(*,'(/a)') ' ** PostScript Page Setup **'

	    write(UnitPS,'(a,/)') '%----- Page Setup'

c   Paper Size

	    write(*,*)
	    write(*,*) 'Paper Size'
	    write(*,*) '  Letter (8.5 x 11) (T)'
	    write(*,*) '  Legal  (8.5 x 14) (G)'
	    write(*,*)
100	    write(*,'(a,$)') '   Select size [Letter]: '
	    read(*,'(a)',err=100,end=999) input
	    if(input.eq.'G' .or. input.eq.'g') then
		PaperSize = LEGAL
	    else
		PaperSize = LETTER
	    end if

c   Page orientation

	    write(*,*)
	    write(*,*) 'Orientation'
	    write(*,*) '  Landscape (Wide) (L)'
	    write(*,*) '  Portrait  (Tall) (P)'
	    write(*,*)
110	    write(*,'(a,$)') '   Select orientation [Landscape]: '
	    read(*,'(a)',err=110,end=999) input
	    if(input.eq.'P' .or. input.eq.'p') then
		orient = PORTRAIT
	    else
		orient = LANDSCAPE
	    end if

c   Graphics area margins

	    write(*,*)
	    write(*,*) 'Margins'
	    write(*,'(a,a)') '   Enter margins for the graphics portion',
     .		' of the page (left, right, top, bottom)'
	    write(*,*) '  (Default is .5 inch on all sides)'
	    write(*,*)
120	    write(*,'(a,$)') '   Enter margins (inches): '
	    read(*,'(4g20.0)',err=120,end=999)
     .			marginl,marginr,margint,marginb
	    if(marginl.eq.0.0 .and. marginr.eq.0.0 .and.
     .		    margint.eq.0.0 .and. marginb.eq.0.0) then
		marginl = 0.5
		marginr = 0.5
		margint = 0.5
		marginb = 0.5
	    end if

c   Set graphics area boundaries and orientation

	    if(PaperSize.eq.LETTER) then
		pageshort = 8.5
		pagelong = 11.0
	    else if(PaperSize.eq.LEGAL) then
		pageshort = 8.5
		pagelong = 14.0
	    end if

	    minmargin = 0.5

	    screenx(1) = (marginl - minmargin) * 1000
	    screeny(2) = (marginb - minmargin) * 1000
	    if(orient.eq.LANDSCAPE) then
		screenx(2) = (pagelong - minmargin - marginr) * 1000
		screeny(1) = (pageshort - minmargin - margint) * 1000
	    else
		screenx(2) = (pageshort - minmargin - marginr) * 1000
		screeny(1) = (pagelong - minmargin - margint) * 1000
	    end if

	    if(orient.eq.LANDSCAPE) then

		write(UnitPS,200) int((pageshort-minmargin) * 1000.0),
     .		    int(minmargin * 1000.0)
200		format(i5,1x,i5,1x,'translate 90 rotate',/)

		write(UnitPS,'(a)') '% PageOrientation: Landscape'

	    else if(orient.eq.PORTRAIT) then

		write(UnitPS,210) int(minmargin * 1000.0),
     .		    int(minmargin * 1000.0)
210		format(i5,1x,i5,1x,'translate',/)


		write(UnitPS,'(a)') '% PageOrientation: Portrait'
	    end if

	end if
    
	swidex = screenx(2) - screenx(1)
	swidex = swidex + isign(1,swidex)
	swidey = screeny(2) - screeny(1)
	swidey = swidey + isign(1,swidey)

	go to 1000

999	continue
	call exit(0)

1000	continue
	end
