
*.
*   NoSpaces - remove leading spaces from a string
*
*	08/23/88 10:45
*..
	subroutine NoSpaces(s)
	character*(*) s

	  character*100 SccsFileID
     -/'@(#)nospaces.for	5.1 98/01/08 APS/ASF\0'/
	integer i,ins,slen,StringLen

	slen = StringLen(s)

c Find first non-space

	do i=1,slen
	    if(s(i:i).ne.' ') then
		ins = i
		go to 10
	    end if
	end do

c All spaces!
	go to 999

c Found a char - remove leading spaces
10	continue

	s = s(ins:slen)

999	continue
	end
