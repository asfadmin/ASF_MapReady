c SccsId = @(#)cross.f	2.41 3/24/98


	subroutine cross(x1,y1,z1,x2,y2,z2,u1,u2,u3)

        implicit none

        character*128 SccsId_cross
        data SccsId_cross
     +  /'@(#)PPcross.f:2.41'/


	real*8 u1,z2,u2,r,u3,y1,x1,z1,y2,x2

	u1=y1*z2-z1*y2
	u2=z1*x2-x1*z2
	u3=x1*y2-y1*x2
	r=(u1**2+u2**2+u3**2)**.5
	u1=u1/r
	u2=u2/r
	u3=u3/r
	return
	end
