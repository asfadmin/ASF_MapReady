c SccsId = @(#)peak_pos.f	2.41 3/24/98


        subroutine peak_pos(a,n,npeak)

        implicit none

        character*128 SccsId_peak_pos
        data SccsId_peak_pos
     +  /'@(#)PPpeak_pos.f:2.41'/


        complex a(n)
	real b(8192)

        integer i,j,ij,n,npeak
        real amax

        amax = 0.

        do i = 1, n
        b(i) = 0.
        do j = -20, 20
	ij = i+j
	if(ij.gt.n) ij = ij-n
	if(ij.lt.1) ij = ij+n
        b(i) = b(i)+abs(a(ij))
	end do
        end do

        do i = 1, n
        if (abs(b(i)).gt.amax) then
        amax = abs(b(i))
        npeak = i
        end if
        end do

        return
        end
