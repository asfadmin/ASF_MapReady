C Alaska SAR Processor (ASP) %W% %E% %U%
	function i2twos(value)
c/*	function i2twos(value) ---------------------------
c
c   this function accepts a value, value, and returns the two's
c   compliment (16 bit) value of it.  Assumes value is in the range
c                         [-1.0:+1.0-e]
c   where e is any small fraction.  (value +1.0 not representable in
c   twos compliment.
c       
c   bit values   2^0 + 2^-1 +2^-2 ... 2^-15
c                                           
c*/
	real power(16)
	integer*2 i2twos,one
	logical first
	data first /.true./ one/1/  
c   set up power table if first time in routine
c
	if (first) then
		do i=0,15
		power(i+1)=2.**(-i)
		end do
		first=.false.
	end if
c   check to see if value in acceptable range:
	if (value .ge. +1.0) then
		write(6,*)'i2twos:  value >= +1.0 ',value
		i2twos=0
		return   
	end if
	if (value .lt. -1.0) then
		write(6,*)'i2twos:  value < -1.0 ',value
		i2twos=0
		return   
	end if                                
c
c   value good if here
c             
c   seperate sign and magnitude
	if (value .ne. 0.) sign=value/abs(value)
	vmag =abs(value)       
	i2twos=0
	if (vmag .ge. power(1)) then ! this is -1.0 value  
		i2twos=2**15
		return
	end if
        do i=2,16                             
	i2twos=i2twos*2 !shift left one bit
	if (vmag .ge. power(i)) then ! set bit
		i2twos=i2twos+1 
		vmag=vmag-power(i)
	end if
	end do
c                          
c   add sign
c
	if (sign .lt. 0.) then !negative
	  i2twos=not(i2twos) + one
	endif
	return       
	end

