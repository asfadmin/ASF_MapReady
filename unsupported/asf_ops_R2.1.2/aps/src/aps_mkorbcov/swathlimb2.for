
*.
*   SwathLimb2 - find position of imaged volume of limb viewing instruments
*
*   History
*	05/11/88 Original
*
*	08/07/89 16:49
*..
	subroutine SwathLimb2(r,v,
     .				latd,lon,altd,
     .				lookang,beamw,
     .				iplat,iinst,
     .				swlatd,swlon,swaltd,type,swerr)
	include 'eosinclude:instruments.inc'
	include 'eosinclude:constants.inc'

	  character*100 SccsFileID
     -/'@(#)swathlimb2.for	5.1 98/01/08 APS/ASF\0'/
	double precision r(3),v(3)	! km, km/sec
	double precision lastlatd,lastlon,latd,lon,lookang,beamw	! deg
	double precision lastaltd,altd	! km
	integer iplat,iinst,itrack
	double precision swlatd(*),slatd
	double precision swlon(*),slon
	double precision swaltd(*),saltd
	integer type
	logical swerr

	double precision rlastlatc,rlatc,rlatd,rlastlon,rlon	! rads
	double precision lookrads,rbeamw		! rads
	double precision dir,rnad,rsc
	double precision phi0,phi,range,cp

	double precision lastr(3),lastv(3)
	double precision xp(3),yp(3),zp(3)
	double precision P(3),PHAT(3),DR(3),DRHAT(3),DELTARANGE
	double precision rlen,vlen
	double precision temp1(3),temp2(3)
	double precision s(3)

	double precision matharg
	integer ip

	double precision ALTDOBS,LIMBC

	LOGICAL KLUGE

** Functions
	double precision rnadir,tolatc,tolatd,vmag


	swerr = .false.
	KLUGE = (ABS(YAWPOINT(IPLAT,IINST)).LE.1.0)

c	rlatd = latd*rads
	rlatc = tolatc(latd)*rads
c	rlon = lon*rads
c	rlastlatc = tolatc(lastlatd)*rads
c	rlastlon = lastlon*rads
c
c	call spherexyz(lastlatd,lastlon,lastaltd,lastr)	! last r vector
c	call spherexyz(latd,lon,altd,r)			! new r vector
c	call vsub(r,lastr,v)				! "velocity" vector
c	call vunit(r,rhat)				! radial unit vector
c	vlen = vmag(v)
c	if(vlen.ne.0.0) then
c	    call vmult(1/vlen,v,vhat)			! velocity unit vector
c	else
c	    swerr = .true.
c	    go to 999
c	end if
c	call vcross(rhat,vhat,hhat)			! ang. mom. unit vector
c

	rnad = rnadir(rlatc)
c	rsc = sqrt(altd**2 + rnad**2 + 2*altd*rnad*cos(rlatd-rlatc))
	rsc = vmag(r)

	ip = platform(iplat,iinst)

	AltdObs = LimbAltd(iplat,iinst)
c	LIMBC = SQRT(RSC**2 - (RNAD + AltdObs)**2)
	PHI = ACOS((RNAD+AltdObs)/RSC)

	call PointAxes(r,v,xp,yp,zp)
	CALL VMULT(COS(YAWPOINT(IPLAT,IINST)*RADS),XP,TEMP1)
	CALL VMULT(SIN(YAWPOINT(IPLAT,IINST)*RADS),YP,TEMP2)
	CALL VADD(TEMP1,TEMP2,P)
	CALL VUNIT(P,PHAT)

* Find S, location of center of observation

	CALL VMULT(-RNAD*COS(PHI),ZP,TEMP1)
	CALL VMULT(RNAD*SIN(PHI),PHAT,TEMP2)
	CALL VADD(TEMP1,TEMP2,S)

	CALL VSUB(S,R,DR)
	CALL VUNIT(DR,DRHAT)

* Move toward and away from S by 1/2 the limb thickness in the view direction

	do itrack=1,tracks(ip)

	    IF(ITRACK.EQ.1) THEN
		DELTARANGE = -LIMBTHICKNESS(IPLAT,IINST)/2.0
	    ELSE
		DELTARANGE = LIMBTHICKNESS(IPLAT,IINST)/2.0
	    END IF

	    IF(.NOT.KLUGE) THEN
		CALL VMULT(DELTARANGE,DRHAT,TEMP1)
	    ELSE
		CALL VMULT(DELTARANGE,YP,TEMP1)
	    END IF

	    CALL VADD(S,TEMP1,TEMP2)

	    call xyzsphere(TEMP2,slatd,slon,saltd)

	    swlatd(itrack) = slatd
	    swlon(itrack)  = slon
c	    swaltd(itrack) = saltd
	    swaltd(itrack) = AltdObs

	end do

999	continue
	end
