c SccsId = @(#)weights.f	2.41 3/24/98


	subroutine weights(w)

        implicit none

        character*128 SccsId_weights
        data SccsId_weights
     +  /'@(#)PPweights.f:2.41'/


	
	real w(64,128), pi, sum
	integer i,j

	pi = 4*atan(1.)
	do i = 2, 128
	do j = 1, 64
	w(j,i) = sin(pi*(j-32-(i-1)/128.))/(pi*(j-32-(i-1)/128.))  
	end do
	end do

	do i = 2, 128
	   sum = 0.0
	   do j = 31, 34
	      sum = sum + w(j,i)**2
	   end do
	   sum = sqrt(sum)
	   do j = 31, 34
	      w(j,i) = w(j,i)/sum
	   end do
	end do

	do j = 1, 64
	w(j,1) = 0. 
	end do
	w(32,1) = 1.

	return
	end

