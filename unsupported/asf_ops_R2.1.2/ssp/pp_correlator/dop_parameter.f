c SccsId = @(#)dop_parameter.f	2.41 3/24/98

	
	subroutine dop_parameter(fd0,fdd,az_in,n,ngroup,dr,
     1		pdf_buf,wght,fd,r_2_r1st)

        implicit none

        character*128 SccsId_dop_parameter
        data SccsId_dop_parameter
     +  /'@(#)PPdop_parameter.f:2.41'/



	integer az_in
	complex pdf_buf(az_in,n),wght(az_in)
	real*8 fd(n), r_2_r1st(n)
	real*8 fd0,fdd,dr,fdmean,rmean

	integer j,ngroup,n

	rmean = 0.
	do j = 1, n
	r_2_r1st(j) = ((j-.5)*ngroup - .5)*dr
	rmean = rmean+r_2_r1st(j)
	end do
	rmean = rmean/n

	call fdr_reg(r_2_r1st,fd,n,fdmean,fdd)
	fd0 = fdmean -rmean*fdd

	return 
	end
