
*.
*   StringLen - find the length of a string (ignoring trailing blanks)
*
*     Call
*	i = StringLen(string)
*
*     Args
*	string		C**	input	string to check
*	i		I*	output	number of characters in string
*
*	06/29/88 8:26
*..
	integer function StringLen(s)
	character*(*) s

	  character*100 SccsFileID
     -/'@(#)stringlen.for	5.1 98/01/08 APS/ASF\0'/
	character c
	integer slen,ic

	slen = len(s)

	do ic=slen,1,-1
	    if(s(ic:ic).ne.' ') then
		StringLen = ic
		return
	    end if
	end do

	StringLen = 0
	return

	end
