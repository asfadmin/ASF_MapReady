
*.
*   PointAxes - find pointing axes used to compute swath location
*
*   Args
*	r(3)		double	input	satellite position vector (km)
*	v(3)		double	input	satellite velocity vector (km)
*	xpoint(3)	double	output	unit vector nearly along velocity vector
*					  equal to (ypoint) cross (zpoint)
*	ypoint(3)	double	output	unit vector directed opposite angular
*					  momentum vector
*	zpoint(3)	double	output	unit vector directed to geocenter
*
*   Uses
*	vCross,vMult,vUnit
*
*   Conventions
*	Vectors are geocentered, geofixed
*
*	07/03/89 14:45
*..
	subroutine PointAxes(r,v,xpoint,ypoint,zpoint)
	include 'eosinclude:constants.inc'
	double precision r(3),v(3)
	double precision xpoint(3),ypoint(3),zpoint(3)

	double precision rhat(3),h(3),hhat(3)

	  character*100 SccsFileID
     -/'@(#)pointaxes.for	5.1 98/01/08 APS/ASF\0'/

	call vUnit(r,rhat)
c..aap	call vMult(-1.0,rhat,zpoint)
	call vMult(dnegone,rhat,zpoint)

	call vCross(r,v,h)
	call vUnit(h,hhat)
c..aap	call vMult(-1.0,hhat,ypoint)
	call vMult(dnegone,hhat,ypoint)

	call vCross(ypoint,zpoint,xpoint)

	end
