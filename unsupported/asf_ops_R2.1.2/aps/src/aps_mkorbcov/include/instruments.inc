     -/'@(#)instruments.inc	5.1 98/01/08 APS/ASF\0'/

*.
*   Instruments.inc - instrument variables
*
*   History
*	03/06/91 Added MASKNITET, changed MASKSUN* to MASKDAY*
*	08/02/89 Original
*
*   Last modified
*	03/06/91 10:03
*..

* Platform
	integer MAXPLAT,NumPlat
	parameter (MAXPLAT=5)
	integer MAXINSTPERPLAT
	parameter (MAXINSTPERPLAT=10)
	character*10 PlatName(MAXPLAT)
	integer platform(MAXPLAT,MAXINSTPERPLAT)
	integer NumInstPlat(MAXPLAT)
	logical active(MAXPLAT)

* Instrument
	integer MAXINST,NumInst
	parameter (MAXINST=99)
	character*10 InstName(MAXINST)
	character*80 InstFullName(MAXINST)

	integer InstType(MAXINST)
	integer MAXINSTTYPE
	parameter (MAXINSTTYPE=12)
	integer SC,NADIR,BEAM,SCAT,SCANS,SCANR,SCANY
	integer LIMB,LIMB2,GLINT,HOTSPOT,NONEINST
	parameter (SC=1,NADIR=2,BEAM=3,SCAT=4)
	parameter (SCANS=5,SCANR=6,SCANY=7)
	parameter (LIMB=8,LIMB2=9,GLINT=10,HOTSPOT=11,NONEINST=12)

	character*15 InstTypeText(MAXINSTTYPE)
	data InstTypeText(SC)       /'Spacecraft'/
	data InstTypeText(NADIR)    /'Nadir'/
	data InstTypeText(BEAM)     /'Beam'/
	data InstTypeText(SCAT)     /'Scatterometer'/
	data InstTypeText(SCANS)    /'Scan (step)'/
	data InstTypeText(SCANR)    /'Scan (roll)'/
	data InstTypeText(SCANY)    /'Scan (yaw)'/
	data InstTypeText(LIMB)     /'Limb'/
	data InstTypeText(LIMB2)    /'Limb II'/
	data InstTypeText(GLINT)    /'Sun glint'/
	data InstTypeText(HOTSPOT)  /'Hot spot'/
	data InstTypeText(NONEINST) /' '/

	double precision LookMin(MAXINST),LookMax(MAXINST)
	double precision YawMin(MAXINST),YawMax(MAXINST)
	double precision BeamWid(MAXINST)
	double precision SwathWid(MAXINST)
	double precision ScanRate(MAXINST)
	integer NumScan(MAXINST)
	double precision SunElevMin(MAXINST)
	double precision LimbAltdMin(MAXINST),LimbAltdMax(MAXINST)
	double precision LimbThicknessMin(MAXINST),LimbThicknessMax(MAXINST)

	integer tracks(MAXINST),MAXTRACK
	parameter (MAXTRACK=4)
	integer MAXMASK,MASKDAYT,MASKDAYP,MASKNITET,MASKNITEP
	integer MASKTDRS,MASKSTN,MASKASC,MASKDSC
	parameter (MAXMASK=8,MASKDAYT=1,MASKDAYP=2,MASKNITET=3,MASKNITEP=4)
	parameter (MASKTDRS=5,MASKSTN=6)
	parameter (MASKASC=7,MASKDSC=8)
	logical mask(MAXMASK,MAXINST)

* Instrument setups

	double precision LookPoint(MAXPLAT,MAXINSTPERPLAT)
	double precision YawPoint(MAXPLAT,MAXINSTPERPLAT)
	integer MAXSCAN
	parameter (MAXSCAN=9)
	double precision ScanLook(MAXPLAT,MAXINSTPERPLAT,MAXSCAN)
	double precision ScanYaw(MAXPLAT,MAXINSTPERPLAT,MAXSCAN)
	double precision LimbAltd(MAXPLAT,MAXINSTPERPLAT)
	double precision LimbThickness(MAXINST,MAXINSTPERPLAT)

* Commons

c..aap	common /eos$instruments/ NumPlat,PlatName,platform,
c..aap     .			NumInstPlat,active,
c..aap     .			NumInst,InstName,InstFullName,InstType,
c..aap     .			LookMin,LookMax,YawMin,YawMax,
c..aap     .			BeamWid,SwathWid,ScanRate,NumScan,
c..aap     .			LimbAltdMin,LimbAltdMax,
c..aap     .			LimbThicknessMin,LimbThicknessMax,
c..aap     .			SunElevMin,tracks,mask,
c..aap     .			LookPoint,YawPoint,ScanLook,ScanYaw,
c..aap     .			LimbAltd,LimbThickness
	common /eos$instruments/ LookMin,LookMax,YawMin,YawMax,
     .			BeamWid,SwathWid,ScanRate,
     .			LimbAltdMin,LimbAltdMax,
     .			LimbThicknessMin,LimbThicknessMax,
     .			SunElevMin,LookPoint,YawPoint,ScanLook,ScanYaw,
     .			LimbAltd,LimbThickness,
     .			NumPlat,platform,NumInstPlat,
     .			NumInst,InstType,NumScan,tracks,
     .			PlatName,InstName,InstFullName,active,mask
