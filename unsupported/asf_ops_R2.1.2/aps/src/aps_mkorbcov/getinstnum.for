*.
*   GetInstNum - get instrument number given instrument name or number
*
*   Args
*	thisinst	I	output	instrument number
*
*	11/15/90 15:56
*..
	subroutine GetInstNum(thisinst,iname,slen)
	character*80 iname

	  character*100 SccsFileID
     -/'@(#)getinstnum.for	5.1 98/01/08 APS/ASF\0'/
** Includes
	include 'eosinclude:instruments.inc'

** Variables

	integer thisinst
	character*80 testname
	integer slen,slen2,iinst

** Functions
	integer StringLen

	thisinst = 0

*	write(*,*)
*10	write(*,'(a,$)')
*     .		' Enter instrument name or # (or ? for list): '
*	read(*,'(a)',err=10,end=999) iname
*	if(iname(1:1).ge.'0' .and. iname(1:1).le.'9') then
*	    read(iname,*,err=10) thisinst
*	    if(thisinst.le.0 .or. thisinst.gt.NumInst) thisinst = 0
*	else if(iname(1:1).eq.'?') then
*	    call ListInst
*	    write(*,*)
*	    go to 10
*	else
*	    slen = len(iname)
	    call ToUpperS(iname(1:slen))
	    do iinst=1,NumInst
		slen2 = StringLen(InstName(iinst))
		testname = InstName(iinst)(1:slen2)
		call ToUpperS(testname(1:slen2))
		if(iname(1:slen).eq.testname(1:slen2)) then
		    thisinst = iinst
		    go to 999
		end if
	    end do
*	end if

999	continue
	end
