*.
*   AnnotPS - annotate PostScript output
*
*	11/13/90 increased length of string from 80 to 255
*	10/14/88
*..
	subroutine AnnotPS

	include 'eosinclude:constants.inc'	! aap..11/4/93

	character*255 string
	integer iq,length,font,size,x,y,dn
	real sizer,xr,yr

      character*100 SccsFileID
     -/'@(#)annotps.for	5.1 98/01/08 APS/ASF\0'/



* Functions
	integer StringLen

	write(*,*)

	call GrafComment('Annotation')

	do while(.true.)
100	    write(*,'(a,$)') ' Enter annotation text [none]: '
	    read(*,'(a)',err=100,end=999) string
	    if(string(1:10).ne.'          ') then
		iq = StringLen(string)
110		write(*,'(a,a,$)') '   Enter x,y location (inches)',
     .			' and letter height (points): '
		read(*,'(3f20.0)',err=110,end=999) xr,yr,sizer
		x = int(xr * 1000.0)
		y = int(yr * 1000.0)
		size = int(sizer * 1000.0 / 72.0)
c..aap		call DrawText(x,y,string,iq,size,0)
		call DrawText(x,y,string,iq,size,izero)
	    else
		go to 120
	    end if
	end do

120	continue
	write(*,*)

	go to 1000
999	call exit(0)
1000	end
