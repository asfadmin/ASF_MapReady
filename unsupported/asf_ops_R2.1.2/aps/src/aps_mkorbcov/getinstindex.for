*.
*   GetInstIndex - get instrument index given instrument name or index
*
*   Args
*	iplat		I	input	platform number
*	iinst		I	output	instrument index number
*
*	11/15/90 15:54
*..
	subroutine GetInstIndex(iplat,iinst)
	integer iplat,iinst

** Includes
	include 'eosinclude:instruments.inc'

	  character*100 SccsFileID
     -/'@(#)getinstindex.for	5.1 98/01/08 APS/ASF\0'/
** Variables

	character*10 ainst
	integer jinst,ip,slen

** Functions
	integer StringLen

	iinst = 0

	if(NumInstPlat(iplat).eq.1) then
	    iinst = 1
	else
	    write(*,*)
10	    write(*,'(a,$)')
     .		' Enter instrument # (or ? for list): '
	    read(*,'(a)',err=10,end=999) ainst
	    if(ainst(1:1).eq.'?') then
		write(*,*)
		do jinst=1,NumInstPlat(iplat)
		    ip = platform(iplat,jinst)
		    slen = StringLen(InstName(ip))
		    write(*,'(1x,i2,1x,a)') jinst,InstName(ip)(1:slen)
		end do
		write(*,*)
		go to 10
	    else
		read(ainst,*,err=10) iinst
		if(iinst.le.0 .or. iinst.gt.NumInstPlat(iplat)) iinst = 0
	    end if
	end if
    
999	continue
	end
