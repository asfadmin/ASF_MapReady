	subroutine matmul (A,V,O)          
c/*	subroutine matmul (A,V,O)          --------------------
c
c = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
c
c          F I L E   M A T H - R.E.CARANDE
c
c   This file contains the following subroutines or functions:
c	matmul - multiplies matrix by a vector
c	vecadd - adds 2 vectors
c	quadform - quadratic formula solution
c	dot - dot product (function)
c	cross_2 - takes cross product of two vectors
c
c   All these routines are DOUBLE PRECISION 
c
c = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
c
c   MATRIX  A by VECTOR V multipy
c*/                                               
	implicit double precision (a-h,o-z)
	double precision A(3,3),V(3),otemp(3),o(3)
      character*80 sccsid
      data sccsid /'@(#)math.f	1.3 96/04/09 22:51:52\0'/
	do i=1,3           
	otemp(i)=0.
	do j=1,3
	otemp(i)=(a(i,j)*v(j) + otemp(i))
  	end do
        end do
c	DO I=1,3 
c	WRITE(6,102) V(I) 
c	END DO 
c	write(6,99)
c	do i=1,3
c	write(6,101)(a(i,j),j=1,3),O(I)   
c	end do
        do i=1,3
	o(i)=otemp(i)
	end do
	return     
99	format(34x,'----------'  )
100	format(34x,'|')
102	format (35x,f10.2)
101	format(3f10.2,4x,'|',F10.2)
	end                   
	subroutine vecadd(a,b,c)           
c/*	subroutine vecadd(a,b,c)           ----------------
c    
c   Add vectors a and b and return results in c.
c*/
	implicit double precision (a-h,o-z)
	double precision a(1),b(1),c(1)    
	do i=1,3
	c(i)=a(i)+b(i)
	end do
	return
        end
	subroutine quadform (a,b,c,ans1,ans2)
c/*	subroutine quadform (a,b,c,ans1,ans2) ------------------
c
c   quadraic formula subroutine
c*/
	implicit double precision (a-h,o-z)
	d=(b**2-4.*a*c)
	if (d .lt. 0) then
		write(6,*)'answers are not real'
		write(6,*)'returning from quadform with no result'
		return
	end if
	ans1=(-b + dsqrt(d))/(2.*a)
	ans2=(-b - dsqrt(d))/(2.*a)
c	write(6,*)'quadratic answers=',ans1,ans2
	return
	end                                               
	function dot(a,b)
c/*	function dot(a,b)  --------------------------
c
c   vector dot product function
c*/
	implicit double precision (a-h,o-z)
	double precision a(1),b(1)   
	dot = a(1)*b(1) + a(2)*b(2) + a(3)*b(3)
	return
	end
	SUBROUTINE CROSS_2(A,B,R)
c/*	SUBROUTINE CROSS_2(A,B,R)  ----------------------
c
c date 		: Feb 24, 1986
c modified	: Chong-Yung Chi
c
C	TAKES THE CROSS PRODUCT A X B = R
c
c input variables:
c
c	A	: a vector with dimension (3)
c	B	: a vector with dimension (3)
c
c output variables:
c
c	R	: cross product of A and B
c*/
	implicit double precision (a-h,o-z)
	double precision A(3),B(3),R(3)
	R(1)=A(2)*B(3)-A(3)*B(2)
	R(2)=A(3)*B(1)-A(1)*B(3)
	R(3)=A(1)*B(2)-A(2)*B(1)
	RETURN
        END
