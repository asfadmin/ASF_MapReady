
*.
*   InitIJKtoSEZ - find matrix to transform geocentric coordinates to
*			topocentric
*
*   Args
*	xTopo		double	input	topocenter position vector (km)
*	tmIJKtoSEZ	double	output	transformation matrix
*
*   Reference
*	"Fundamentals of Astrodynamics", Roger R. Bate, Donald D. Mueller,
*	  Jerry E. White, Dover, 1971.  pp. 78-79.
*
*   History
*	 3/25/91 Account for oblate central body
*	 3/22/91 Copied from IJKtoSEZ.  IJKtoSEZ used to find the matrix and
*		   then find topcentric coordinates - from now on will use
*		   matrix found here
*
*   Last modified
*	3/25/91 11:44 djc
*..
	subroutine InitIJKtoSEZ(xTopo,tmIJKtoSEZ)

** Includes
	include 'eosinclude:constants.inc'
** Args
	double precision xTopo(3),tmIJKtoSEZ(3,3)

	double precision xy
	double precision rlatc,rlatd,rlon
	double precision slat,slon,clat,clon

	  character*100 SccsFileID
     -/'@(#)initijktosez.for	5.1 98/01/08 APS/ASF\0'/
** Functions
	double precision ToRLatd

* find the topocenter's latc,lon
	xy = sqrt(xTopo(1)*xTopo(1) + xTopo(2)*xTopo(2))
	if(xy .ne. 0.0d0) then
	    rlatc = atan(xTopo(3)/xy)
	    rlatd = ToRLatd(rlatc)
	    rlon = atan2(xTopo(2),xTopo(1))
	else
	    rlatd = sign(halfpi,xTopo(3))
	    rlon = 0.0d0
	end if

* save time
	slat = sin(rlatd)
	slon = sin(rlon)
	clat = cos(rlatd)
	clon = cos(rlon)

* find transformation matrix
*   row 1
	tmIJKtoSEZ(1,1) = slat * clon
	tmIJKtoSEZ(1,2) = slat * slon
	tmIJKtoSEZ(1,3) = -clat
*   row 2
	tmIJKtoSEZ(2,1) = -slon
	tmIJKtoSEZ(2,2) = clon
	tmIJKtoSEZ(2,3) = 0.0d0
*   row 3
	tmIJKtoSEZ(3,1) = clat * clon
	tmIJKtoSEZ(3,2) = clat * slon
	tmIJKtoSEZ(3,3) = slat

	end
