
*.
*   SwathHotSpot - find position of hot spot (platform "shadow")
*
*   Args
*	r(3)			double	input	satellite radius vector (km)
*	HotLatd			double	output	geodetic latitude of hotspot (deg)
*	HotLon			double	output	longitude of hotspot (deg)
*	HotAltd			double	output	geodetic altitude of hotspot (deg)
*	NoHotSpot		logical	output	flag
*
*   History
*	03/21/89 Original
*
*	08/07/89 16:43
*..
	subroutine SwathHotSpot(r,HotLatd,HotLon,HotAltd,NoHotSpot)
	include 'eosinclude:constants.inc'
	include 'eosinclude:cbody.inc'
	include 'eosinclude:time.inc'	! for SunLatd,SunLon common block

	  character*100 SccsFileID
     -/'@(#)swathhotspot.for	5.1 98/01/08 APS/ASF\0'/
	double precision r(3)
	double precision HotLatd,HotLon,HotAltd
	logical NoHotSpot

	double precision rHat(3),rSun(3),SunHat(3),RxSunHat(3)
	double precision rPerp(3),rPerpHat(3)
	double precision delta,alpha,phi
	double precision HotLoc(3)
	double precision temp1(3),temp2(3)
	double precision rmag

** Functions
	double precision vAng,vMag

c Find position of sun (rSun)
c Find geocentric angle between radius vector and hotspot
c Find vector perpindicular to s/c radius vector in plane defined by
c   radius vector and sun vector (rPerpHat)
c Find location of hostpot point using r and rPerp

	rmag = vMag(r)
	call SphereXYZ(SunLatd,SunLon,RBody,rSun)
	delta = vAng(r,rSun)

** Should use ViewPoint....

	if(delta.lt.asin(RBody/rmag)) then

	    alpha = pi - asin(sin(delta) * rmag / RBody)
	    phi = pi - delta - alpha

	    call vUnit(rSun,SunHat)

	    call vUnit(r,rHat)

	    call vCross(rHat,SunHat,RxSunHat)
	    call vCross(RxSunHat,rHat,rPerp)
	    call vUnit(rPerp,rPerpHat)

	    call vMult(RBody*cos(phi),rHat,temp1)
	    call vMult(-RBody*sin(phi),rPerpHat,temp2)
	    call vAdd(temp1,temp2,HotLoc)
	    call XYZSphere(HotLoc,HotLatd,HotLon,HotAltd)

	    HotAltd = 0.0

	    NoHotSpot = .false.

	else

	    NoHotSpot = .true.

	end if

	end
