c SccsId = @(#)weight.f	2.41 3/24/98


 
	subroutine weight(w1,w2,w3,w4,n)

        implicit none

        character*128 SccsId_weight
        data SccsId_weight
     +  /'@(#)PPweight.f:2.41'/


	real*8 dp,pi
	integer k,n

	real*8 w1(n),w2(n),w3(n),w4(n)

	pi = 4.*atan(1.)
	do k = 1, n
	   dp = (k-1.)/n
           w1(k)=-(1.+dp)**3 + 5.*(1.+dp)**2 - 8.*(1.+dp) +4.
           w2(k)=      dp**3 -      2.*dp**2               +1.
           w3(k)= (1.-dp)**3 - 2.*(1.-dp)**2               +1.
           w4(k)=-(2.-dp)**3 + 5.*(2.-dp)**2 - 8.*(2.-dp) +4.
        end do

        return
        end

c	w1(k) = sinc(dp+1.,pi)  
c	w2(k) = sinc(dp,pi) 
c	w3(k) = sinc(1.-dp,pi) 
c	w4(k) = sinc(2.-dp,pi) 
c	sum = sqrt(w1(k)**2+w2(k)**2+w3(k)**2+w4(k)**2)
c	w1(k) = w1(k)/sum
c	w2(k) = w2(k)/sum
c	w3(k) = w3(k)/sum
c	w4(k) = w4(k)/sum

