c SccsId = @(#)doppler_cnt.f	2.41 3/24/98



	subroutine doppler_cnt(az_in,n,ngroup,fd0,fdd,df,dr,
     1		pdf_buf,wght,fd,r_2_r1st)

        implicit none

        character*128 SccsId_doppler_cnt
        data SccsId_doppler_cnt
     +  /'@(#)PPdoppler_cnt.f:2.41'/



	integer az_in
	complex pdf_buf(az_in,n),wght(az_in)
	real*8 fd(n), r_2_r1st(n)
	real*8 fd0,fdd,df,dr
	real*8 position

	integer iflag,j,npeak,n,ngroup,i

	call init_fft(az_in)
	do i = 1, az_in
	wght(i) = cmplx(0.,0.)
	end do
	do i = 1, az_in/2*0.7
	wght(i+1) = cmplx(1.,0.)
	wght(az_in-i+1) = cmplx(-1.,0.)
	end do
	iflag = 1
	call cfft(wght,1,az_in,iflag)
	
        do j = 1, n
        iflag = 1
        call peak_pos(pdf_buf(1,j),az_in,npeak)
        call cfft(pdf_buf(1,j),1,az_in,iflag)
        call cvmul(pdf_buf(1,j),wght,az_in,-1)
        iflag = -1
        call cfft(pdf_buf(1,j),1,az_in,iflag)

        call zero_crsng(pdf_buf(1,j),az_in,npeak,position)
        fd(j) = fd0+fdd*(((j -.5)*ngroup -.5)*dr) + (position)*df
c	print *,i,'position = ', position, npeak, pdf_buf(npeak,j) 
 	print *,j,'position = ', position, npeak, fd(j) 
        end do

        return
        end
