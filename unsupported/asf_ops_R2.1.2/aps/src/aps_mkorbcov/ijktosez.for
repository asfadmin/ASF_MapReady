*.
*   IJKtoSEZ - transform geocentric coordinates (IJK) to topocentric (SEZ =
*		 South, East, Zenith)
*
*   Args
*	tmIJKtoSEZ	double	input	trans. matrix from IJK to SEZ
*	xObject		double	input	geocentric position vector of object (km)
*	xTopo		double	input	geocentric position vector of station (km)
*	rhoSEZ		double	output	topocentric position vector of object (km)
*	az		double	output	azimuth to target (rad)
*	el		double	output	elevation to target (rad)
*
*   Uses
*	mMult3x1
*
*   Reference
*	"Fundamentals of Astrodynamics", Roger R. Bate, Donald D. Mueller,
*	  Jerry E. White, Dover, 1971.  pp. 78-79.
*
*   History
*	 3/25/91 Re-include geocentric position of station
*	 3/22/91 Use transformation matrix from call to InitIJKtoSEZ
*	 6/06/89 Original
*
*   Last modified
*	3/25/91 11:30 djc
*..
	subroutine IJKtoSEZ(tmIJKtoSEZ,xObject,xTopo,rhoSEZ,az,el)

** Includes
	include 'eosinclude:constants.inc'
** Args
	double precision tmIJKtoSEZ(3,3),xObject(3),xTopo(3),rhoSEZ(3)
	double precision az,el

	double precision rhoIJK(3)
	double precision SE

	  character*100 SccsFileID
     -/'@(#)ijktosez.for	5.1 98/01/08 APS/ASF\0'/

* find the IJK vector from station to object

	call vSub(xObject,xTopo,rhoIJK)

* find the SEZ coordinates of the object

	call mMult3x1(tmIJKtoSEZ,rhoIJK,rhoSEZ)

* find az and el to the object

	if(rhoSEZ(1).ne.0.0 .or. rhoSEZ(2).ne.0.0) then
	    az = atan2(rhoSEZ(2),-rhoSEZ(1))
	    if(az.lt.0.0) az = az + twopi
	    SE = sqrt(rhoSEZ(1)*rhoSEZ(1) + rhoSEZ(2)*rhoSEZ(2))
	    el = atan(rhoSEZ(3)/SE)
	else
	    az = 0.0
	    el = sign(halfpi,rhoSEZ(3))
	end if

	end
