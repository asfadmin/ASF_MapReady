c SccsId = @(#)fdr_reg.f	2.41 3/24/98


	subroutine fdr_reg(r_2_r1st,fd,n,fdmean,fdd)

        implicit none

        character*128 SccsId_fdr_reg
        data SccsId_fdr_reg
     +  /'@(#)PPfdr_reg.f:2.41'/

	

	real*8 r_2_r1st(512), fd(512)
	real*8 fdmean,rmean,accu,fdd,sum,sigma,dfd
	integer i,n
	
	fdmean = 0
	rmean = 0
	do i = 1, n
	rmean = rmean + r_2_r1st(i)
	fdmean = fdmean + fd(i)
	end do
	rmean = rmean/n
	fdmean = fdmean/n

	fdd = 0
	accu = 0
	do i = 1, n
	fdd = fdd + fd(i)*(r_2_r1st(i)-rmean)
	accu = accu + (r_2_r1st(i)-rmean)**2
	end do
	fdd = fdd/accu

	sum = 0
	do i = 1, n
	dfd = fd(i)-(fdmean+fdd*(r_2_r1st(i)-rmean))
c	print *, 'dfd = ', dfd
	sum = sum+dfd**2
	end do
	sigma = sqrt(sum/n)
c	print *, 'sigma fd = ', sigma

	return
	end
