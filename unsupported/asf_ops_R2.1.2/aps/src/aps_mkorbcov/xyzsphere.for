
*.
*   XYZSphere(x,latd,lon,altd) convert cartesian vector to spherical coords
*
*   Args
*	x	double	input	cartesian vector (x,y,z) (km)
*	latd	double	output	geodetic latitude (deg)
*	lon	double	output	longitude (deg)
*	altd	double	output	geodetic altitude (km)
*
*   Reference
*	Atul Nautiyal, "Algorithm to Generate Geodetic Coordinates from
*	Earth-Centered Earth-Fixed Coordinates," Journal of Guidance, Control,
*	and Dynamics, Vol. 11, No. 3, May-June 1988, pp. 281-283
*
*	08/02/89 15:06
*..
	subroutine XYZSphere(x,latd,lon,altd)
c	include 'eosinclude:constants.inc'
	include 'eosinclude:cbody.inc'
	double precision x(3),latd,lon,altd

	  character*100 SccsFileID
     -/'@(#)xyzsphere.for	5.1 98/01/08 APS/ASF\0'/
	double precision xe,ze,e2,A,B,C,D,a0,a1,a2,a3,a4
	double precision t0,t,t2,t3,dt,f,fprime
	double precision tan2phi,rlatc
	integer k,kmax
	double precision tol
	double precision one

* Constants
	double precision pi,twopi,halfpi,rads
	data pi,twopi /3.14159265358979323d0,6.283185307179586d0/
	data halfpi,rads /1.570796326794897d0,0.017453292519943296d0/

** Data
	data kmax,tol /10,1.0d-8/
	data one /1.0d0/


	xe = sqrt(x(1)**2 + x(2)**2)
	ze = x(3)
	e2 = one - (one - dble(flat))**2

	A = (ze/(dble(RBody)*(one-dble(flat))))**2
	B = -e2 / (one - e2)
	C = one / (one - e2)
	D = xe / dble(RBody)

	a0 = B**2
	a1 = 2.0 * B * C * D
	a2 = A + (C*D)**2 - a0
	a3 = -a1
	a4 = -(C*D)**2

	if(x(1).ne.0 .or. x(2).ne.0) then
	    rlatc = atan(x(3)/xe)
	else
	    if(x(3).gt.0) then
		rlatc = halfpi
	    else
		rlatc = -halfpi
	    end if
	end if

	t0 = sqrt(one/(one + tan(rlatc)**2/(one-e2)))
	t = t0

	do 100 k=1,kmax

	    t2 = t * t
	    t3 = t * t2

	    f = a0*t*t3 + a1*t3 + a2*t2 + a3*t + a4
	    fprime = 4.0*a0*t3 + 3.0*a1*t2 + 2.0*a2*t + a3
	    dt = -f/fprime

c	    write(*,99) k,f,fprime,dt,t,dt/t
c99	    format(1x,i2,2(2x,f10.5),3(2x,f15.10))

	    if(abs(dt/t).le.tol) go to 200

	    t = t + dt
	    
100	continue

	call ErrorMessage('XYZSPHERE - no convergence')
	latd = 0.0
	lon = 0.0
	altd = 0.0
	go to 999

200	continue

	t2 = t * t
	tan2phi = (one-t2)/(t2*(one-e2))

	latd = atan(sqrt(tan2phi)) / rads
	IF(X(3).LT.0.0D0) LATD = -LATD		! DJC 04/25/89
	lon = atan2(x(2),x(1)) / rads
	altd = (xe - dble(RBody) * t) * sqrt(one + tan2phi)
	
999	continue
	end
