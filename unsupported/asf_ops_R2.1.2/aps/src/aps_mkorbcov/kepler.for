
*.
*   Kepler - solves Kepler equation
*
*   Args
*	am		double	input	mean anomaly (rad)
*	e		double	input	eccentricity
*	ea		double	output	eccentric anomaly (rad)
*
*   References
*	JPL EM 312/85-140, 1 April 1985
*	JPL IOM 312/79.4-426, Final Report JPL Contract #955140
*	User's guide for MULCON, 1 January 1979
*
*   Comments
*	This program is a stripped version of the original version for
*	MULCON.  This version does not work for hyperbolic cases.
*	The tolerances are set for double precision.  TOL1 should be
*	set to 1/3 of machine precision, TOL2 to 1/2 of machine
*	precision.
*
*	08/02/89 15:00
*..
	subroutine kepler(am,e,ea)
	double precision am,e,ea

	  character*100 SccsFileID
     -/'@(#)kepler.for	5.1 98/01/08 APS/ASF\0'/
	double precision tol1,tol2,se,ce,half,zero,one,pi,tpi
	double precision abm,sm,a,ese,c,d,f,sen,cen
	data tol1 /1.0d-5/, tol2 /1.0d-8/
	data half /0.5d0/, zero /0.0d0/, one /1.0d0/
	data pi /3.141592653589793d0/, tpi /6.283185307179586d0/

	integer k

	k = 0

10	continue
	abm = abs(am)
	if(abm.lt.pi) go to 20
	if(am.gt.pi) am = am - tpi
	if(am.lt.-pi) am = am + tpi
	go to 10

20	continue
	sm = sin(abm)
	ea = abm + e * sm / (one - sin(abm+e) + sm)

30	continue
	se = sin(ea)
	ce = cos(ea)
	k = k + 1
	if(k.gt.10) stop

50	continue
	ese = e * se
	f = abm + ese - ea
	d = one - e * ce
	c = f / (d + half * f * ese / d)
	ea = ea + c
	if(abs(c).gt.tol1) go to 30
	if(abs(c).lt.tol2) go to 40
	a = one - half * c * c
	sen = a * se + c * ce
	cen = a * ce - c * se
	se = sen
	ce = cen
	go to 50

40	continue
	sen = se + c * ce
	cen = ce - c * se
	se = sen
	ce = cen
	if(am.gt.zero) return
	se = -se
	ea = -ea
	return

	end
