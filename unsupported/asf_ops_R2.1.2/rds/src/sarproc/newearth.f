	subroutine newearth (h,xlat)
c/*	subroutine newearth (h,xlat) -----------------------
c                                   
c	Richard E. Carande    October 27, 1988.
c
c    This subroutine is used to calculate a new Earth model within the
c    ASP software that has the following characteristics:
c
c	* Ellipsoid
c	* same flattening factor as present Earth model
c	* Expanded (reduce) equatorial and polar radii to produce an increase 
c		(increase) in the local radius at the input latitude.
c         
c    Input:   h = (real*8) height above (below) present ellipsoid (meters)
c	      xlat = (real*8) latitude of point (degrees)
c
c    Output:  The common block /earth/ is updated with new equatorial (re)
c             and polar (rp) radii.
c
c    NOTE:  THIS SUBROUTINE WILL CHANGE THE COMMON BLOCK DEFINING THE 
C           EARTH SURFACE, AND HENCE ALL CALCULATIONS USING THESE VALUES
C           AFTER THIS CALL
C              
c*/
	implicit double precision (a-h,o-z)
	common /earth/          rp,re              
	data pi /3.141592654/
      character*80 sccsid
      data sccsid /'@(#)newearth.f	1.3 96/04/09 22:51:54\0'/
	write(6,*)'subroutine NEWEARTH called... Earth radii being adjusted'
 	xlatr = xlat * pi / 180.d0
c
c   calculate flattening factor f and some other constants
c
	f = (re-rp)/re                                    
	a = dsqrt( 1.-(2.-f)*f*dcos(xlatr) )
c
c   calculate radius at this latitude using old model
c                                         
	rold=re*(1.-f)/a                            
c 
c   calculate new local, equatorial and polar radii
c
	rnew=rold + h 
 	re=rnew*a/(1.-f)  
	rp=re*(1-f)                                     
	write(6,*)'     new earth radii:  equatorial = ',re
	write(6,*)'      (in meters)           polar = ',rp
	return
	end

