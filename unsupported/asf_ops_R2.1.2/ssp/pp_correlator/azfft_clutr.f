c SccsId = @(#)azfft_clutr.f	2.41 3/24/98


	subroutine azfft_clutr(buff_in,rng_in,az_in,nrs,iflag)

        implicit none

	include '/usr/lpp/ppe.poe/include/mpif.h'

        character*128 SccsId_azfft_clutr
        data SccsId_azfft_clutr
     +  /'@(#)PPazfft_clutr.f:2.41'/

	
	integer ierror

	integer rng_in,az_in
	complex buff_in(rng_in,az_in)
	complex temp(8192)
	integer iflag

	integer i,j,nrs

	if(az_in .gt. 8192) then
           print *,'azfft_clutr: az_in > 8192 increase temp buffer size'
	   call mpi_abort(mpi_comm_world,22,ierror)
           call exit(22)
        end if

	call init_fft(az_in)

	do i = 1, rng_in-nrs
	   do j = 1, az_in
	      temp(j) = buff_in(i,j)
	   enddo
	   call cfft(temp,1,az_in,iflag)
	   do j = 1, az_in
	      buff_in(i,j) = temp(j) 
	   enddo
	end do

	return
	end
