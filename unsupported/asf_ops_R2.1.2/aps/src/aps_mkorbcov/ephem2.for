*.
*   Ephem2 - orbital element propogator
*
*   Args
*	a		double	input	semimajor axis (km)
*	e		double	input	eccentricity
*	i		double	input	inclination (rad)
*	node0		double	input	long of asc node at BaseGMT + t0 (rad)
*	omega0		double	input	arg of periapsis at BaseGMT + t0 (rad)
*	m0		double	input	mean anomaly at BaseGMT + t0 (rad)
*	t		double	input	current time since BaseGMT (sec)
*	t0		double	input	epoch for these elements (sec)
*	latd		double	output	geodetic latitude (deg)
*	lon		double	output	longitude (deg)
*	altd		double	output	geodetic altitude (km)
*	x(3)		double	output	position vector (km)
*	xdot(3)		double	output	velocity vector (km/sec)
*
*   Uses
*	Ellipse - convert orbital elements to Cartesian coordinates
*	XYZSphere - convert x,y,z to latd,lon,altd
*	ShortPeriod - find change in a due to j2
*
*	07/28/88
*	03/05/87
*
*	08/03/89 08:50
*..
	subroutine ephem2(a,e,i,node0,omega0,m0,t,t0,latd,lon,altd,x,xdot)

	include 'eosinclude:cbody.inc'
	include 'eosinclude:constants.inc'

	  character*100 SccsFileID
     -/'@(#)ephem2.for	5.1 98/01/08 APS/ASF\0'/
c Inputs
	double precision a,e,i,node0,omega0,m0,t,t0

c Outputs
	double precision latd,lon,altd
	double precision x(3),xdot(3)

c Program variables
	double precision dt,n,coeff,NodeRegress
	double precision da,dnode,domega,danom

	double precision node,omega,ma

c Begin

	dt = t - t0
	n = sqrt(mu/a**3)

	coeff = -3.0d0*n*j2*RBody**2/((1.0-e**2)**2*a**2)
	NodeRegress = 0.5d0*coeff*cos(i)

	dnode = (NodeRegress-RotRate)*dt
	domega = 0.25d0*coeff*(1.0-5.0d0*cos(i)**2)*dt
	danom = -0.25d0*sqrt(1.0-e**2)*coeff*(3.0d0*cos(i)**2-1.0)*dt

	node = node0 + dnode
	omega = omega0 + domega
	ma = m0 + n * dt + danom

c	call ShortPeriod(a,e,i,node,omega,ma,da)

	call ellipse(a,e,i,node,omega,ma,x,xdot)

	call XYZSphere(x,latd,lon,altd)

	end
