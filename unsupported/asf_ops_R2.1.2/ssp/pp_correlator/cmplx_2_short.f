c SccsId = @(#)cmplx_2_short.f	2.41 3/24/98
	subroutine cmplx_2_short(img_cmplx,buff_in,rng_in,az_in,nadd,i,
     +                           scale)
        implicit none

        character*128 SccsId_cmplx_2_short
        data SccsId_cmplx_2_short
     +  /'@(#)PPcmplx_2_short.f:2.41'/


        integer rng_in              !# rng samples procesed each block
	integer az_in               !# az samples procesed each block
	complex buff_in(rng_in,az_in+nadd)
	integer*2 img_cmplx(2*(az_in+nadd))
	integer i                   !ith row in range direction
	integer k, nadd, j
	real*8 scale

	j = i+1                     !c index starts at zero
	do k = 1, az_in+nadd
	   img_cmplx(2*k-1) = real(buff_in(j,k)*scale)
	   img_cmplx(2*k) = aimag(buff_in(j,k)*scale)
	end do

	return
	end
