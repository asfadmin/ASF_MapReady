c SccsId = @(#)xcorrel.f	2.41 3/24/98



	subroutine xcorrel(a,b,c,n)

        implicit none

        character*128 SccsId_xcorrel
        data SccsId_xcorrel
     +  /'@(#)PPxcorrel.f:2.41'/


	real a(n),b(n),c(n)
	real*8 sum, sum1, sum2

	integer i_to,k,ijk,n,i

	do i = 1, n
	c(i) = 0
	end do

	do ijk = -20, 20
	i_to = ijk + 1
	if(i_to.le.0) i_to = i_to + n
	sum = 0.
	sum1 = 0.
	sum2 = 0.
	do k = 1, n
	if(k+ijk.ge.1.and.k+ijk.le.n) then
	if(a(k).ne.0.and.b(k+ijk).ne.0) then
	sum = sum + a(k)*b(k+ijk)
	sum1 = sum1 + a(k)**2
	sum2 = sum2 + b(k+ijk)**2
	end if
	end if
	end do
	if(sum1.gt.0.0.and.sum2.gt.0.0) then 
	   c(i_to) = sum/sqrt(sum1)*1./sqrt(sum2)
	else
	   c(i_to) = 0.0
	end if
	end do

	return
	end
