
*.
*   SphereXYZ(latd,lon,altd,x) - convert spherical coordinates to cartesian
*
*   Args
*	latd			double	input	geodetic latitude (deg)
*	lon			double	input	longitude (deg)
*	altd			double	input	geodetic altitude (km)
*	x			double	output	cartesian position vector (km)
*
*	04/25/89 - added declination
*	10/05/88
*
*	04/25/89 21:41
*..
	subroutine SphereXYZ(latd,lon,altd,x)
	include 'eosinclude:constants.inc'
	double precision latd,lon,altd,x(3)

	  character*100 SccsFileID
     -/'@(#)spherexyz.for	5.1 98/01/08 APS/ASF\0'/
	double precision latc,rlatc,rlatd,rlon,r,r2,rnad,dlat,ddecl,decl

* Functions
	double precision RNadir,ToLatc


	rlatd = latd * rads
	latc = ToLatc(latd)
	rlatc = latc*rads
	rlon = lon*rads
	dlat = rlatd - rlatc

* find radius r by solving triangle with side rnad, angle 180-(latd-latc),
*   and side altd
	rnad = RNadir(rlatc)
	r2 = rnad**2 + altd**2 + 2.0*rnad*altd*cos(dlat)
	if(r2.ge.0.0) then
	    r = sqrt(r2)
	else
	    r = 0.0
	end if

	ddecl = asin(sin(dlat)*altd/r)
	decl = rlatc + ddecl

	x(1) = r * cos(decl) * cos(rlon)
	x(2) = r * cos(decl) * sin(rlon)
	x(3) = r * sin(decl)

	end
