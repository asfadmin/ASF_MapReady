
*.
*   Swath - find near/far position of imaging beam from an orbiter
*
*   Args
*	r(3)			double	input	orbiter position vector (km)
*	v(3)			double	input	orbiter velocity vector (km/sec)
*	latd			double	input	orbiter geod. latitude (deg)
*	lon			double	input	orbiter longitude (deg)
*	altd			double	input	orbiter geod. altitude (km)
*	lookang			double	input	off-nadir angle (deg)
*	beamw			double	input	imaging beam width (deg)
*	yaw			double	input	yaw pointing angle (deg)
*	iplat			integer	input	platform number
*	iinst			integer	input	instrument index on platform iplat
*	swlatd()		double	output	swath latd for each track (deg)
*	swlon()			double	output	swath lon for each track (deg)
*	swaltd()		double	output	swath altd for each track (km)
*	swerr			logical	output	error flag
*
*	07/13/89 Use ViewPoint instead of phi
*	06/06/88
*
*	07/13/89 16:38
*..
	subroutine swath(r,v,
     .				latd,lon,altd,
     .				lookang,beamw,yaw,
     .				iplat,iinst,
     .				swlatd,swlon,swaltd,type,swerr)
** Includes
	include 'eosinclude:instruments.inc'
	include 'eosinclude:constants.inc'
** Variables
	double precision r(3),v(3)	! km, km/sec
	double precision lastlatd,lastlon,latd,lon,lookang,beamw,yaw	! deg
	double precision lastaltd,altd	! km
	integer iplat,iinst,itrack
	double precision swlatd(*),slatd
	double precision swlon(*),slon
	double precision swaltd(*),saltd
	integer type
	logical swerr

	  character*100 SccsFileID
     -/'@(#)swath.for	5.1 98/01/08 APS/ASF\0'/
	double precision rlastlatc,rlatc,rlatd,rlastlon,rlon	! rads
	double precision lookrads,rlook(MAXTRACK),rbeamw,ryaw	! rads
	double precision dir,rnad,rsc
	double precision phi0,phi,range,cp

	double precision xp(3),yp(3),zp(3)
	double precision lastr(3),rhat(3),lastv(3),vhat(3),h(3),hhat(3)
	double precision rlen,vlen
	double precision temp1(3),temp2(3)
	double precision s(3)

	double precision AltdObs,LimbC
	double precision vprime(3),p(3),phat(3)

	double precision matharg
	integer ip

** Functions
	double precision rnadir,tolatc,tolatd,vmag

	swerr = .false.

c	rlatd = latd*rads
	rlatc = tolatc(latd)*rads
c	rlon = lon*rads
c	rlastlatc = tolatc(lastlatd)*rads
c	rlastlon = lastlon*rads
	lookrads = lookang*rads
	rbeamw = beamw*rads
	ryaw = yaw * rads

	rsc = vmag(r)
	rnad = rnadir(rlatc)

	if(type.eq.NADIR) then
	    swlatd(1) = latd
	    swlon(1) = lon
	    swaltd(1) = 0.0
	    return
	else if(type.eq.SC) then
	    swlatd(1) = latd
	    swlon(1) = lon
	    swaltd(1) = altd
	    return
	else if(type.eq.BEAM) then
	    rlook(1) = lookrads - sign(.5*rbeamw,lookrads)
	    rlook(2) = lookrads + sign(.5*rbeamw,lookrads)
	else if(type.eq.SCAT) then
	    rlook(1) = lookrads - sign(.5*rbeamw,lookrads)
	    rlook(2) = lookrads + sign(.5*rbeamw,lookrads)
	    rlook(3) = -lookrads - sign(.5*rbeamw,-lookrads)
	    rlook(4) = -lookrads + sign(.5*rbeamw,-lookrads)
	else if(type.eq.SCANR .or. type.eq.SCANY) then
	    rlook(1) = lookrads
	else if(type.eq.LIMB2) then
	    call SwathLimb2(r,v,
     .				latd,lon,altd,
     .				lookang,beamw,
     .				iplat,iinst,
     .				swlatd,swlon,swaltd,type,swerr)
	    return
	else if(type.eq.GLINT) then
	    call SwathGlint(r,swlatd(1),swlon(1),swaltd(1),swerr)
	    return
	else if(type.eq.HOTSPOT) then
	    call SwathHotSpot(r,swlatd(1),swlon(1),swaltd(1),swerr)
	    return
	end if

c	call spherexyz(lastlatd,lastlon,lastaltd,lastr)	! last r vector
c	call spherexyz(latd,lon,altd,r)			! new r vector
c	call vsub(r,lastr,v)				! "velocity" vector
c	call vunit(r,rhat)				! radial unit vector
c	vlen = vmag(v)
c	if(vlen.ne.0.0) then
c	    call vmult(1/vlen,v,vhat)			! velocity unit vector
c	else
c	    swerr = .true.
c	    return
c	end if
c	call vcross(rhat,vhat,hhat)			! ang. mom. unit vector
c

c	rsc = sqrt(altd**2 + rnad**2 + 2*altd*rnad*cos(rlatd-rlatc))

cccccccc for geocenter attitude control
	call PointAxes(r,v,xp,yp,zp)
cccc or, for nadir attitude control
c	call PointAxesN(r,v,xp,yp,zp)
cccccccc

	ip = platform(iplat,iinst)

	do itrack=1,tracks(ip)

c	    if(type.ne.LIMB) then
c		matharg = rsc*sin(rlook(itrack))/rnad
c		if(matharg.gt.1.0) then
c		    matharg = 1.0
c		else if(matharg.lt.-1.0) then
c		    matharg = -1.0
c		end if
c		phi = asin(matharg) - rlook(itrack)
c	    end if

cc	    drange = dble(rsc*rsc)+dble(rnad*rnad)
cc     .			-dble(2.*rsc*rnad)*dcos(dble(phi0))
cc	    if(drange.ge.0.0) then
cc	        range = dsqrt(drange)
cc	    else
cc	        range = 0.0
cc	    end if
cc	    if(range.lt.altd+1.0) range = altd + 1.0
cc	    if(range.lt.altd) range = altd
cc	    phi = 0.01
cc	    dcp = (dble(rsc)**2 + dble(rnad)**2 - dble(range)**2)
cc     .			/dble(2.*rsc*rnad)
cc	    if(dabs(dcp).lt.1.) then
cc		phi = dacos(dcp)
cc	    else
cc		phi = 0.0
cc	    end if

	    if(type.ne.LIMB) then

		call ViewPoint(r,xp,yp,zp,rlook(itrack),ryaw,s,swerr)
		if(swerr) return

	    else

		AltdObs = LimbAltd(iplat,iinst)
		LimbC = sqrt(rsc**2 - (rnad + AltdObs)**2)
		phi = acos((rnad + AltdObs)/rsc)

		call vMult(cos(ryaw),xp,temp1)
		call vMult(sin(ryaw),yp,temp2)
		call vAdd(temp1,temp2,p)
		call vUnit(p,phat)

		call vUnit(r,rhat)
		call vMult(rnad*cos(phi),rhat,temp1)
		call vMult(rnad*sin(phi),phat,temp2)
		call vAdd(temp1,temp2,s)

	    end if

	    call XYZSphere(s,slatd,slon,saltd)
	    swlatd(itrack) = slatd
	    swlon(itrack)  = slon
	    if(type.ne.LIMB) then
		swaltd(itrack) = saltd
	    else
		swaltd(itrack) = AltdObs
	    end if

	end do

999	continue
	end
