*.
*   FindQ - find Q
*
*   Args
*	a		double	input	semimajor axis (km)
*	e		double	input	eccentricity
*	i		double	input	inclination (rad)
*	Q		double	output	orbits/day
*
*	10/05/88
*
*	08/03/89 10:24
*..
	subroutine FindQ(a,e,i,Q)
	include 'eosinclude:cbody.inc'
	include 'eosinclude:constants.inc'
	double precision a,e,i,Q

	  character*100 SccsFileID
     -/'@(#)findq.for	5.1 98/01/08 APS/ASF\0'/
	double precision Pn,dnode,domega,danom

	call NodePeriod(a,i,Pn)
	call Oblate(a,e,i,dnode,domega,danom)
	Q = twopi / (Pn * (RotRate - dnode))

	end
