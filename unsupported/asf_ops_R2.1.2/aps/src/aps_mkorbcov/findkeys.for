*.
*   FindKeys - find key words and values in a string
*
*   Args
*	InLine		C**	input	input string
*	MaxKey		I	input	maximum number of keys to find
*	NumKey		I	output	number of keys found
*	KeyWord		C**	output	array of keywords
*	WordLen		I	output	array of keyword lengths
*	KeyVal		C**	output	array of key values
*	ValLen		I	output	array of key value lengths
*
*   Routines
*	StringLen (I)
*	ToUpperS
*
*	02/28/89 Original
*
*	03/11/89 14:18
*..
	subroutine FindKeys(InLine,MaxKey,NumKey,
     .				KeyWord,WordLen,KeyVal,ValLen)
	character*(*) InLine
	integer MaxKey,NumKey
	character*(*) KeyWord(*),KeyVal(*)
	integer WordLen(*),ValLen(*)

	  character*100 SccsFileID
     -/'@(#)findkeys.for	5.1 98/01/08 APS/ASF\0'/

	integer ic,ip,ik,iv,ikey
	character tab,c
	logical instring,onequote

	integer slen

** Functions
	integer StringLen

	tab = char(9)

* slen is the number of characters in the string (ignoring trailing spaces)
	slen = StringLen(InLine)
* NumKey is the number of keywords with valid values found
	NumKey = 0

* Ignore leading spaces
*
*   ip is the current character position in the input string

	ip = 1

	do 10 ic=ip,slen
	    if(InLine(ic:ic).ne.' ' .and. InLine(ic:ic).ne.tab) go to 11
	    ip = ip + 1
10	continue
	go to 999
11	continue

* Find keywords and values
*
*   ikey is the ordinal key number

	ikey = 1

	do while(ikey.lt.MaxKey)

*   Find keyword
*
*   ik is the current character position in a candidate keyword

	    ikey = NumKey + 1

	    ik = 0

	    do 20 ic=ip,slen-2

		if(InLine(ic:ic+2).ne.' = ') then
		    ik = ik + 1
		    KeyWord(ikey)(ik:ik) = InLine(ic:ic)
		else
		    ip = ic + 3
		    WordLen(ikey) = ik
		    go to 21
		end if

20	    continue
	    go to 999
21	    continue

*   Find value
*
*   iv is the current character position in a candidate key value
*   instring is true if we have encountered a leading single qoute

	    iv = 0
	    instring = .false.

	    ic = ip
	    do while(ic.le.slen)

		c = InLine(ic:ic)
		if(c.ne.' ' .and. c.ne.'''') then
		    iv = iv + 1
		    KeyVal(ikey)(iv:iv) = c
		else
		    if(c.eq.' ') then
			if(instring) then
			    iv = iv + 1
			    KeyVal(ikey)(iv:iv) = c
			else
			    ip = ic + 1
			    go to 31
			end if
		    else if(c.eq.'''') then
			if(.not.instring) then
			    instring = .true.
			else
			    if(ic.eq.slen
     .				    .or. InLine(ic+1:ic+1).eq.' ') then
				ip = ic + 2
				go to 31
			    else if(InLine(ic+1:ic+1).eq.'''') then
				iv = iv + 1
				KeyVal(ikey)(iv:iv) = ''''
				ic = ic + 1
			    else
				ip = ic + 1
				go to 31
			    end if
			end if
		    end if
		end if

c30	    continue
		ic = ic + 1
	    end do
c	    go to 999
31	    continue

	    if(iv.gt.0) then
		ValLen(ikey) = iv
		NumKey = NumKey + 1
		call ToUpperS(KeyWord(ikey)(1:WordLen(ikey)))
	    end if

	end do

999	continue

	end
