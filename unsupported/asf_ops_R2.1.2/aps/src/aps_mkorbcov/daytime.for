*.
*   DayTime - returns true if a point on the central body surface is less than
*		SUNANGLE from the subsolar point
*
*   Args
*	latd,lon	double	input	position (deg) of point of interest
*	SUNANGLE	double	input	max allowed distance to sun nadir (rad)
*	DayTime		logical	return
*
*   Note
*	Main program must update SunLatd,SunLon (in common in file TIME.INC)
*
*	08/03/89 08:11
*..
	logical function daytime(latd,lon,SUNANGLE)
	include 'eosinclude:time.inc'
	real latd,lon,SUNANGLE

	 character*100 SccsFileID
     -/'@(#)daytime.for	5.1 98/01/08 APS/ASF\0'/
** Functions
	double precision DiffAng

	DayTime = (DiffAng(latd,lon,sunlatd,sunlon).le.SUNANGLE)

	end
