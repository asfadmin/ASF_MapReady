
*.
*   LocalTimeToNode - find the longitude of ascending node if the local time of
*			(ascending or descending) node crossing is given
*
*   Args
*	a		double	input	semi-major axis (km)
*	e		double	input	eccentricity
*	i		double	input	inclination (rad)
*	node0		double	output	long. of asc. node (rad)
*	omega0		double	input	argument of periapsis (rad)
*	m0		double	input	mean anomaly (rad)
*	t0		double	input	(epoch-basegmt) (sec)
*	node0hour	double	input	local time of node crossing (hr)
*					  (negative -> descending node time)
*
*   Assumes
*	Central body has been set
*	Epoch has been set (BaseYear,BaseGMT)
*
*	07/26/89
*
*	07/26/89 17:20
*..
	subroutine LocalTimeToNode(a,e,i,node0,omega0,m0,t0,node0hour)
** Args
	double precision a,e,i,node0,omega0,m0,t0,node0hour
** Includes
	include 'eosinclude:cbody.inc'
	include 'eosinclude:time.inc'
	include 'eosinclude:constants.inc'

	double precision sublatd,sublon,period,tof,dnode,domega,danom

	  character*100 SccsFileID
     -/'@(#)localtimetonode.for	5.1 98/01/08 APS/ASF\0'/
* Find the sun
	call SubSolar(BaseYear,BaseGMT+t0,sublatd,sublon)

	if(node0hour.lt.0.0) then
	    if(e.eq.0.0) then
		call NodePeriod(a,i,period)
		tof = period / 2.0
	    else
		call TimeOfFlight(a,e,i,
     .				    -omega0,pi-omega0,tof)
	    end if
	    call Oblate(a,e,i,dnode,domega,danom)
	    node0 = (sublon + 15.0d0 * (abs(node0hour)-12.0d0)
     .				+ 180.0d0) * rads - (dnode-RotRate)*tof
	else
	    node0 = (sublon + 15.0d0 * (node0hour-12.0d0)) * rads
	end if

	node0 = mod(node0,twopi)
	if(node0.gt.pi) then
	    node0 = node0 - twopi
	else if(node0.lt.-pi) then
	    node0 = node0 + twopi
	end if

	end
