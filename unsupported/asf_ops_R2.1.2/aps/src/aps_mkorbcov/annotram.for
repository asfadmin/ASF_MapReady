*.
*   AnnotRam - add annotation to Ramtek
*
*	04/17/90
*
*	04/17/90 11:59
*..
	subroutine AnnotRam

	character*80 string
	character lr
	integer*4 iq,length,font,size,x,y,dn
      character*100 SccsFileID
     -/'@(#)annotram.for	5.1 98/01/08 APS/ASF\0'/

* Functions
	integer StringLen

	write(*,*)

	do while(.true.)
100	    write(*,'(a,$)') ' Enter annotation text [none]: '
	    read(*,'(a)',err=100,end=999) string
	    if(string(1:10).ne.'          ') then
		iq = StringLen(string)
110		write(*,'(a,$)') '   Enter x,y location and letter height: '
		read(*,'(3i10)',err=110,end=999) x,y,size
		font = 3
		dn = 255
		call RamText(string,iq,font,size,x,y,dn)
	    else
		go to 999
	    end if
	end do

999	continue
	end
