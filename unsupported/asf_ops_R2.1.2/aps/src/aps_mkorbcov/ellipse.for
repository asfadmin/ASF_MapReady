*.
*   Ellipse - transform orbital elements a,e,i,node,w,ea to Cartesian x,xdot
*
*   Input
*	a, semi-major axis (km)
*	e, eccentricity
*	i, inclination (rad)
*	node, longitude of ascending node (rad)
*	omega, argument of periapsis (rad)
*	ma, mean anomaly (rad)
*
*   Output
*	x, position vector (km)
*	xdot, velocity vector (km/sec)
*
*   Subroutines used
*	kepler - converts mean anomaly to eccentric anomaly
*	xrot - coordinate axis rotation about x-axis
*	zrot - coordinate axis rotation about z-axis
*
*	08/02/89 12:40
*..
	subroutine ellipse(a,e,i,node,omega,ma,x,xdot)
	include 'eosinclude:cbody.inc'

	 character*100 SccsFileID
     -/'@(#)ellipse.for	5.1 98/01/08 APS/ASF\0'/
c Input classical elements (km,rad)
	double precision a,e,i,node,omega,ma

c Output state vector (km,km/sec)
	double precision x(3),xdot(3)

	double precision ea
	double precision xx(3)
	double precision r(3),rlen
	double precision v(3),vlen
	double precision f,p
	double precision sf,cf
	double precision temp1(3),temp2(3),coeff


	call kepler(ma,e,ea)
	f  = 2.0 * atan(sqrt((1.0+e)/(1.0-e)) * tan(ea/2.0))
	sf = sin(f)
	cf = cos(f)

	p = a * (1.0 - e*e)
	rlen = p / (1.0 + e * cos(f))

c Find radius vector

	xx(1) = rlen * cf
	xx(2) = rlen * sf
	xx(3) = 0.0

c Rot line of apsides to line of nodes
	call ZRot(xx,-omega,temp1)
c Rot orbit plane to coincide with X-Y
	call XRot(temp1,-i,temp2)
c Rot line of apsides to x-axis
	call ZRot(temp2,-node,x)

c Find velocity vector

	coeff = sqrt(mu/p)
	xdot(1) = -coeff * sf
	xdot(2) = coeff * (e + cf)
	xdot(3) = 0.0

	call ZRot(xdot,-omega,temp1)
	call XRot(temp1,-i,temp2)
	call ZRot(temp2,-node,xdot)

	end
