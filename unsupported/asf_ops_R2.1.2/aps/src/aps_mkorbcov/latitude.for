
*.
*   Latitude - latitude conversions
*
*	ToLatc(latd) - converts geodetic lat latd (deg) to geocentric lat (deg)
*	ToRLatc(rlatd) - converts geodetic lat rlatd (rad) to geocentric lat (rad)
*	ToLatd(latc) - converts geocentric lat latc (deg) to geodetic lat (deg)
*	ToRLatd(rlatc) - converts geocentric lat rlatc (rad) to geodetic lat (rad)
*
*   History
*	 3/25/91 Added ToRLatc,ToRLatd
*	10/05/88
*
*   Last modified
*	3/25/91 11:49
*..
	double precision function ToLatc(latd)
	include 'eosinclude:constants.inc'
	include 'eosinclude:cbody.inc'
	double precision latd

	  character*100 SccsFileID
     -/'@(#)latitude.for	5.1 98/01/08 APS/ASF\0'/
	ToLatc = atan(tan(latd*rads)*(1.0-flat)**2)/rads
c	ToLatc = sign(ToLatc,latd)

	end


	double precision function ToRLatc(rlatd)
	include 'eosinclude:cbody.inc'
	double precision rlatd

	ToRLatc = atan(tan(rlatd)*(1.0-flat)**2)
	ToRLatc = sign(ToRLatc,rlatd)

	end


	double precision function ToLatd(latc)
	include 'eosinclude:constants.inc'
	include 'eosinclude:cbody.inc'
	double precision latc

	ToLatd = atan(tan(latc*rads)/(1.0-flat)**2)/rads
	ToLatd = sign(ToLatd,latc)

	end


	double precision function ToRLatd(rlatc)
	include 'eosinclude:cbody.inc'
	double precision rlatc

	ToRLatd = atan(tan(rlatc)/(1.0-flat)**2)
	ToRLatd = sign(ToRLatd,rlatc)

	end
