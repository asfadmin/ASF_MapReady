c SccsId = @(#)look_1_4_xfunc.f	2.41 3/24/98


	subroutine look_1_4_xfunc(buff_in,rng_in,az_in,
     1	            peak,fr,r_2_r1st,fr0,frd,pbw,prf,n,ngroup)

        implicit none

        character*128 SccsId_look_1_4_xfunc
        data SccsId_look_1_4_xfunc
     +  /'@(#)PPlook_1_4_xfunc.f:2.41'/



	integer rng_in,az_in
	complex buff_in(rng_in,az_in)
	complex buff_lk1(8192),buff_lk4(8192)
	real accum(8192),accum1(8192),a(8192),b(8192)
	real*8 peak(n),fr(n),r_2_r1st(n)
        real*8 fr0,frd,pbw,prf

	integer i,j1,iflag,j,n,ngroup,nband_4

	call init_fft(az_in/4)
	nband_4 = az_in * pbw/prf * .25
	
	do j = 1, n
	   do i = 1, az_in/4
	   accum(i) = 0.0
	   end do

	do j1 = 1, ngroup

	do i = 1, nband_4
	buff_lk1(i) = buff_in((j-1)*ngroup+j1,i+az_in/2-2*nband_4)
	buff_lk4(i) = buff_in((j-1)*ngroup+j1,i+az_in/2+nband_4)
	end do
	do i = nband_4+1, az_in/4
	buff_lk1(i) = cmplx(0.,0.)
	buff_lk4(i) = cmplx(0.,0.)
	end do

	iflag = -1
	call cfft(buff_lk1,1,az_in/4,iflag)
	call cfft(buff_lk4,1,az_in/4,iflag)

	do i = 1, az_in/4
	a(i) = abs(buff_lk1(i))
	b(i) = abs(buff_lk4(i))
	end do	
	call xcorrel(a,b,accum1,az_in/4)
	do i = 1, az_in/4
	accum(i) = accum(i) + accum1(i)
	end do
	end do
	call get_peak(accum,az_in/4,peak(j))

	end do

	return 
	end
