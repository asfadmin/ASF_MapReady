	function twos2fp (itwos)
c/*	function twos2fp (itwos) -------------------------------
c
c  converts input 2's compliment (16bit) value itwos to the real
c  floating point representation between [-1.0 - +1.0-e] where 
c  e is 2**-15
c            
c*/
	integer*2 ival,itwos,it,i,fifteen,minus1
	data fifteen/b'1000000000000000' /, minus1/z'8000'/	
      character*80 sccsid
      data sccsid /'@(#)twos2fp.f	1.3 96/04/09 22:51:58\0'/
	twos2fp=0.
	sign=1.        
	ival=itwos
	if (ival .eq. minus1) then
		twos2fp=-1.0
		return
	end if
	if (and(ival,fifteen).eq. fifteen) then !negative
		sign=-1.
        	ival=ival-1
		ival=not(ival)
	end if
	do i=0,15
	it=2**i	
	if (and(ival,it).eq. it) twos2fp=twos2fp+2.**(-(15-i))
	end do
	twos2fp=sign*twos2fp  
	return
	end               
