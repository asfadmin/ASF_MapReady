*.
*   DegMin2Deg - convert degrees,minutes to degrees
*
*   Args
*	ideg		integer	input	whole number of degrees (like 30)
*	idegmin		integer	input	minutes portion (like 20)
*	deg		double	output	decimal degrees (like 30.333)
*
*   Note
*	If ideg is 0 then for a negative angle idegmin must be negative
*	(like 0 and -30 is converted to -0.5)
*
*	08/02/89 14:55
*..
	subroutine degmin2deg(ideg,idegmin,deg)
	integer ideg,idegmin
	double precision deg

	double precision rdegmin

	 character*100 SccsFileID
     -/'@(#)degmin2deg.for	5.1 98/01/08 APS/ASF\0'/
	deg = abs(ideg)
	deg = deg + abs(idegmin)/60.0
	if(ideg.lt.0 .or. (ideg.eq.0 .and. idegmin.lt.0)) deg = -deg

	end
