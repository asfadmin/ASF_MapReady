
*.
*   SwathGlint - find position of sun glint
*
*   Args
*	r(3)			double	input	satellite radius vector (km)
*	GlintLatd		double	output	geodetic latitude of glint pt (deg)
*	GlintLon		double	output	longitude of glint pt (deg)
*	GlintAltd		double	output	geodetic altitude of glint pt (deg)
*	NoGlint			logical	output	flag
*
*   Uses
*
*   History
*	07/27/88
*
*	03/20/89 12:10
*..
	subroutine SwathGlint(r,GlintLatd,GlintLon,GlintAltd,NoGlint)
	include 'eosinclude:constants.inc'
	include 'eosinclude:cbody.inc'
	include 'eosinclude:time.inc'	! for SunLatd,SunLon common block

	  character*100 SccsFileID
     -/'@(#)swathglint.for	5.1 98/01/08 APS/ASF\0'/
	double precision r(3)
	double precision GlintLatd,GlintLon,GlintAltd
	logical NoGlint

	double precision rHat(3),rSun(3),SunHat(3),RxSunHat(3)
	double precision rPerp(3),rPerpHat(3)
	double precision GlintPhi,theta		! rads
	double precision GlintLoc(3)
	double precision temp1(3),temp2(3)
	double precision rnad

c Find position of sun (rSun)
c Find geocentric angle between radius vector and glitter point
c Find vector perpindicular to s/c radius vector in plane defined by
c   radius vector and sun vector (rPerpHat)
c Find location of glitter point using r and rPerp

	call SphereXYZ(SunLatd,SunLon,RBody,rSun)
	call GlintAng(r,SunLatd,SunLon,GlintPhi,theta,rnad)

** Should modify to use theta and ViewPoint....

	if(GlintPhi.lt.pi) then

	    call vUnit(rSun,SunHat)

	    call vUnit(r,rHat)

	    call vCross(rHat,SunHat,RxSunHat)
	    call vCross(RxSunHat,rHat,rPerp)
	    call vUnit(rPerp,rPerpHat)

	    call vMult(rnad*cos(GlintPhi),rHat,temp1)
	    call vMult(rnad*sin(GlintPhi),rPerpHat,temp2)
	    call vAdd(temp1,temp2,GlintLoc)
	    call XYZSphere(GlintLoc,GlintLatd,GlintLon,GlintAltd)

	    GlintAltd = 0.0

	    NoGlint = .false.

	else

	    NoGlint = .true.

	end if

	end
