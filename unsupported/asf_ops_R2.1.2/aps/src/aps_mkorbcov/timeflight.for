
*.
*   TimeOfFlight - find orbiter time of flight
*
*   Args
*	a			double	input	semimajor axis (km)
*	e			double	input	eccentricity
*	i			double	input	inclination (rad)
*	ta1			double	input	true anomaly start (rad)
*	ta2			double	input	true anomaly end (rad)
*	tof			double	output	time-of-flight (sec)
*
*   Notes
*	Uses nodal period BUT assumes unperturbed flight
*
*	10/05/88 15:28
*..
	subroutine TimeOfFlight(a,e,i,ta1,ta2,tof)
	include 'eosinclude:constants.inc'
	include 'eosinclude:cbody.inc'

	  character*100 SccsFileID
     -/'@(#)timeflight.for	5.1 98/01/08 APS/ASF\0'/
	double precision a,e,i
	double precision ta1,ta2
	double precision tof

	double precision period
	double precision ea1,ea2

	call NodePeriod(a,i,period)

	ea1 = acos((e+cos(ta1))/(1.0+e*cos(ta1)))
	if(ta1.gt.pi .and. ea1.lt.pi) ea1 = twopi - ea1
	if(ta1.lt.-pi .and. ea1.gt.-pi) ea1 = twopi - ea1
	if(ta1.lt.0.0) ea1 = -abs(ea1)

	ea2 = acos((e+cos(ta2))/(1.0+e*cos(ta2)))
	if(ta2.gt.pi .and. ea2.lt.pi) ea2 = twopi - ea2
	if(ta2.lt.-pi .and. ea2.gt.-pi) ea2 = twopi - ea2
	if(ta2.lt.0.0) ea2 = -abs(ea2)

	tof = period * (ea2 - e*sin(ea2) - (ea1 - e*sin(ea1))) / twopi

	end
