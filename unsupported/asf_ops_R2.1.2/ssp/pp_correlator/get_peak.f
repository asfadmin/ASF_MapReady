c SccsId = @(#)get_peak.f	2.41 3/24/98
	

	subroutine get_peak(a,n,pos)

        implicit none

        character*128 SccsId_get_peak
        data SccsId_get_peak
     +  /'@(#)PPget_peak.f:2.41'/



	integer i, ipos, n
	real a(n)
	real b(2048)
	real*8 beta, pos, value, v_rght, v_lft, maximum

	maximum = -10.**30.
	pos = 0
	value = 0

	do i = 1, n
	b(i) = a(i)
	b(i+n) = b(i)
	end do

	do i = -10, 10, 1
	if(b(i+n).gt.maximum) then
	maximum = b(i+n)
	ipos = i
	value = maximum
	else
	end if
	end do
c	print *, 'ipos=',ipos
	v_rght = b(ipos+n+1)
	v_lft = b(ipos+n-1)
	beta = (v_rght+v_lft)/2 - value
	if(beta.ne.0) then
	pos = ipos + (v_lft - v_rght)/(4*beta)
	else
	pos = ipos
	end if

	pos = pos - 1.

	return
	end
