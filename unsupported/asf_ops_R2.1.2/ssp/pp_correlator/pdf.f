c SccsId = @(#)pdf.f	2.41 3/24/98


	subroutine pdf(buff_in,rng_in,az_in,n,ngroup,
     1		pdf_buf,wght,fd,r_2_r1st)

        implicit none

        character*128 SccsId_pdf
        data SccsId_pdf
     +  /'@(#)PPpdf.f:2.41'/


     
	integer rng_in,az_in
	complex buff_in(rng_in,az_in)
	complex pdf_buf(az_in,n),wght(az_in)
	real*8 fd(n), r_2_r1st(n)
	real temp

	integer i,j1,j,n,ngroup

	do j = 1, n
	do i = 1, az_in
	pdf_buf(i,j) = 0.
	end do
	end do
	
	do j = 1, n 
	do j1 = 1, ngroup
	do i = 1, az_in
	temp = abs(buff_in((j-1)*ngroup+j1,i))**2
	pdf_buf(i,j)=pdf_buf(i,j)+cmplx(temp,0.)
	end do
	end do
	end do
	
	
	return 
	end
