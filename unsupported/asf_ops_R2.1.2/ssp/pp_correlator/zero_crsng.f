c SccsId = @(#)zero_crsng.f	2.41 3/24/98


        subroutine zero_crsng(a,n,npeak,pos)

        implicit none

        character*128 SccsId_zero_crsng
        data SccsId_zero_crsng
     +  /'@(#)PPzero_crsng.f:2.41'/



        complex a(n)
        real*8 pos, posp, value, valuep, minimum

        integer i1,ip,n,npeak,i
        real slope

        minimum = 10.**30.
        pos = 0
        value = 0

        do i = npeak-30, npeak+30
        i1 = i
        if(i1.le.0) i1 = i1 + n
        if(i1.gt.n) i1 = i1 - n
        if (abs(a(i1)).lt.minimum) then
        minimum = abs(a(i1))
        pos = i
        value = real(a(i1))
        end if
        end do

c	print *, 'pos=', pos
	ip = pos + 1
	valuep = real(a(ip))

	slope = valuep - value
c	pos = pos - value/slope
	pos = pos - n/2 - 1
c	print *, 'pos=', pos
        if(pos.gt.n/2) pos = pos - n
        if(pos.le.-n/2) pos = pos + n
c	print *, 'pos=', pos

	return
	end
