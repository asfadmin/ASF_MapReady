
*.
*   Oblate - find perturbations in node, argument of periapsis and
*	    mean anomaly due to oblateness of central body
*
*   Args
*	a		double	input	semimajor axis (km)
*	e		double	input	eccentricity
*	i		double	input	inclination (rad)
*	dnode		double	output	change in long of asc node (rad/sec)
*	domega		double	output	change in arg of periapsis (rad/sec)
*	danom		double	output	change in mean anomaly (rad/sec)
*
*	06/21/88
*
*	08/03/89 10:26
*..
	subroutine oblate(a,e,i,dnode,domega,danom)
	include 'eosinclude:cbody.inc'

	  character*100 SccsFileID
     -/'@(#)oblate.for	5.1 98/01/08 APS/ASF\0'/
* Inputs
	double precision a,e,i

* Outputs
	double precision dnode,domega,danom

	double precision n,coeff

	n = sqrt(mu/a**3)

	coeff = -3.0d0*n*j2*RBody**2/((1.0-e**2)**2*a**2)

	dnode = 0.5d0*coeff*cos(i)
c	dnode = (NodeRegress-RotRate)
	domega = 0.25d0*coeff*(1.0-5.0d0*cos(i)**2)
	danom = -0.25d0*sqrt(1.0-e**2)*coeff*(3.0d0*cos(i)**2-1.0)

	end
