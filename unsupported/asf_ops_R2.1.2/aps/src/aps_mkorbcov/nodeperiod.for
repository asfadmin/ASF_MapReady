
*.
*   NodePeriod - find the nodal period of an orbiter
*
*   Args
*	a		double	input	orbiter semi-major axis (km)
*	i		double	input	inclination (rad)
*	Pn		double	output	nodal period (sec)
*
*	Created 06/16/88 13:45
*
*	08/03/89 10:22
*..
	subroutine NodePeriod(a,i,Pn)
	include 'eosinclude:constants.inc'
	include 'eosinclude:cbody.inc'
	double precision a,i,Pn

	  character*100 SccsFileID
     -/'@(#)nodeperiod.for	5.1 98/01/08 APS/ASF\0'/
	double precision n,k,noderate

	n = sqrt(mu/a**3)
	k = 1.5d0 * j2 * (RBody/a)**2
	noderate = -n * k * cos(i)

	Pn = (twopi / n) * (1.0 - (4.0*noderate**2/(n**2*k) - k))

	end
