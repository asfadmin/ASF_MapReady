*.
*   DiffAng - finds angle difference between two positions on surface
*		given their lat/lons
*
*   Args
*	lat1,lon1	double	input	latd and lon for point 1 (deg)
*	lat2,lon2	double	input	latd and lon for point 2 (deg)
*	DiffAng		double	return	angle between points 1 and 2 (rad)
*
*   Assumes
*	Central body has been set
*
*	08/02/89 15:24
*..
	double precision function DiffAng(lat1,lon1,lat2,lon2)
	include 'eosinclude:constants.inc'	! aap..11/4/93
	double precision lat1,lon1,lat2,lon2

	double precision x1(3),x2(3)

	 character*100 SccsFileID
     -/'@(#)diffang.for	5.1 98/01/08 APS/ASF\0'/
** Functions
	double precision vAng


c..aap	call SphereXYZ(lat1,lon1,0.0,x1)
	call SphereXYZ(lat1,lon1,dzero,x1)
c.aap	call SphereXYZ(lat2,lon2,0.0,x2)
	call SphereXYZ(lat2,lon2,dzero,x2)
	DiffAng = vAng(x1,x2)

	end
