*.
*   GetPlatNum - get platform index given platform name or index
*
*   Args
*	thisplat	I	output	platform index number
*
*	11/15/90 15:57
*..
	subroutine GetPlatNum(thisplat)

** Includes
	include 'eosinclude:instruments.inc'

	  character*100 SccsFileID
     -/'@(#)getplatnum.for	5.1 98/01/08 APS/ASF\0'/
** Variables

	integer thisplat
	character*80 pname,testname
	integer slen,slen2,iplat

** Functions
	integer StringLen

	thisplat = 0

	if(NumPlat.eq.1) then
	    thisplat = 1
	else
	    write(*,*)
10	    write(*,'(a,$)') ' Enter platform name or #: '
	    read(*,'(a)',err=10,end=999) pname
	    if(pname(1:1).ge.'0' .and. pname(1:1).le.'9') then
		read(pname,*,err=10) thisplat
		if(thisplat.le.0 .or. thisplat.gt.NumPlat) thisplat = 0
	    else
		slen = StringLen(pname)
		call ToUpperS(pname(1:slen))
		do iplat=1,NumInst
		    slen2 = StringLen(PlatName(iplat))
		    testname = PlatName(iplat)(1:slen2)
		    call ToUpperS(testname(1:slen2))
		    if(pname(1:slen).eq.testname(1:slen2)) then
			thisplat = iplat
			go to 999
		    end if
		end do
	    end if
	end if

999	continue
	end
