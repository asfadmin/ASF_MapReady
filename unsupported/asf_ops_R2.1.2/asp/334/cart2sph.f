C Alaska SAR Processor (ASP) %W% %E% %U%
	subroutine cart2sph (x,y,z,r,theta,phi)
c/*	subroutine cart2sph (x,y,z,r,theta,phi)  -------------
c
c
c   routine to convert cartesian coordinates to spherical
c   assumes phi angle (azimuth) is positive rotation about z 
c   axis, theta (latitude) is negative rotation about y axis, r is radius.
c*/
	implicit double precision(a-h,o-z)
	r=dsqrt(x*x+y*y+z*z)     
	theta=dasin(z/r)          
	phi=datan2(y,x)
	return
	end                                       
