	subroutine cart2sph (x,y,z,r,theta,phi)
c/*	subroutine cart2sph (x,y,z,r,theta,phi)  -------------
c
c
c   routine to convert cartesian coordinates to spherical
c   assumes phi angle (azimuth) is positive rotation about z 
c   axis, theta (latitude) is negative rotation about y axis, r is radius.
c*/
	implicit double precision(a-h,o-z)
      character*80 sccsid
      data sccsid /'@(#)cart2sph.f	1.3 96/04/09 22:51:39\0'/
	r=dsqrt(x*x+y*y+z*z)     
	theta=dasin(z/r)          
	phi=datan2(y,x)
	return
	end                                       
