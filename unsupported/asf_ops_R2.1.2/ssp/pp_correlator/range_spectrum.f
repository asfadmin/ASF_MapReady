c SccsId = @(#)range_spectrum.f	2.41 3/24/98

	subroutine range_spectrum(buff_in_rd,tmp_buffer,spectrum,
	1    rng_in,az_in)

        implicit none

        character*128 SccsId_range_spectrum
        data SccsId_range_spectrum
     +  /'@(#)PPrange_spectrum.f:2.41'/

	integer i, j, rng_in, az_in, iflag
	complex buff_in_rd(rng_in,az_in),tmp_buffer(rng_in)
	real*8 spectrum(rng_in)

	call init_fft(rng_in)

c** range cfft *****************

	do j = 1, rng_in
	   spectrum(j) = 0.0
	end do

	do i = 1, 64
	   iflag = 1
	   do j = 1, rng_in
	      tmp_buffer(j) = buff_in_rd(j,i)
	   end do
	   call cfft(tmp_buffer,1,rng_in,iflag)
	   do j = 1, rng_in
	      spectrum(j) = spectrum(j) + (abs(tmp_buffer(j)/rng_in))**2
	   end do
	end do


	do j = 1, rng_in
	   spectrum(j) = spectrum(j)/64.0
	end do

	return
	end


