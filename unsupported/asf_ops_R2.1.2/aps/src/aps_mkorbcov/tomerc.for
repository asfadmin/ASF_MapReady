
*.
*   ToMerc - convert latitude latd to its Mercator equivalent
*
*   Args
*	latd			double	input	geodetic latitude (deg)
*	ToMerc			double	return
*
*	07/06/89 No longer limit return value to within +/-90
*
*	07/06/89 11:13
*..
	double precision function ToMerc(latd)
	double precision latd

	  character*100 SccsFileID
     -/'@(#)tomerc.for	5.1 98/01/08 APS/ASF\0'/
	include 'eosinclude:constants.inc'

	double precision MaxMerc
	data MaxMerc /271.6993/


*      Pick a latd of +/-89.5 as the highest we'll allow (why not?)
	if(abs(latd) .lt. 89.5d0) then
	    ToMerc = log(tan(0.5d0*(latd+90.0d0)*rads))/rads
	else
	    ToMerc = sign(MaxMerc,latd)
	end if

	end
