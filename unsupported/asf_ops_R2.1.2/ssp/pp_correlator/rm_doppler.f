c SccsId = @(#)rm_doppler.f	2.41 3/24/98
	
	subroutine rm_doppler(buff_in,rng_in,az_in,nrs,
     1		        	prf,r_1st,fd0,fdd,fr0,frd,dr)

        implicit none

        character*128 SccsId_rm_doppler
        data SccsId_rm_doppler
     +  /'@(#)PPrm_doppler.f:2.41'/


	integer rng_in,az_in
	complex buff_in(rng_in,az_in)
	real*8 prf,r_1st,fd0,fdd,fr0,frd,dr	
	real*8 r,fd,dt,t,pi,phase

	integer j,i,nrs
	
	pi = 4.*atan(1.)
	dt = 1/prf
	do i = 1, rng_in-nrs
	r = r_1st + (i - 1) * dr
	fd = fd0 + (r - r_1st) * fdd
	do j = 1, az_in
	t = (j-1)*dt
	phase = 2 * pi * (prf/2-fd) * t	!minus sign is to remove	
	buff_in(i,j) = buff_in(i,j) * cmplx(cos(phase),sin(phase))
	end do
	end do

	return 
	end
