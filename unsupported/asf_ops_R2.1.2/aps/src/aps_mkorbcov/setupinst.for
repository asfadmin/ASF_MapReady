
*.
*   SetUpInst - set instrument parameters
*
*   Args
*	iplat			integer	input	platform number
*	iinst			integer	input	# instrument on plat
*
*   VAX extensions used
*	Q format specifier
*
*   History
*	10/17/88 Original
*
*	03/20/89 11:14
*..
	subroutine SetUpInst(iplat,iinst,yawang)
	include 'eosinclude:instruments.inc'
	integer iplat,iinst

	  character*100 SccsFileID
     -/'@(#)setupinst.for	5.1 98/01/08 APS/ASF\0'/
	integer ip,itype,iscan,iq
	double precision lookang,yawang,limbh,limbt

	ip = platform(iplat,iinst)
	itype = InstType(ip)
*
*	if(itype.eq.BEAM .or. itype.eq.SCAT
*     .		.or. itype.eq.LIMB .or. itype.eq.LIMB2
*     .		.or. itype.eq.SCANS .or. itype.eq.SCANR
*     .		.or. itype.eq.SCANY) then
*	    write(*,11) InstName(ip),iplat
*11	    format(/,' For ',a10,' on platform #',i2,' -')
*	end if

	if(itype.eq.BEAM .or. itype.eq.SCAT) then

	    if(LookMin(ip).eq.LookMax(ip)) then
		lookang = LookMin(ip)
	    else if(LookMax(ip).gt.0.0) then
20		write(*,'(a,$)') '   Enter look angle (deg): '
		read(*,'(f20.0)',err=20,end=999) lookang
	    else
		lookang = 0.0
	    end if

	    LookPoint(iplat,iinst)  = abs(lookang)

	    if(LookPoint(iplat,iinst).ne.0.0 .and. itype.ne.SCAT) then
*30		write(*,'(a,$)')
*     .			'   Enter yaw pointing angle (deg) [90]: '
*		read(*,'(q,f20.0)',err=30,end=999) iq,yawang
		if(iq.eq.0) yawang = 90.0d0
	    else
		yawang = 90.0d0
	    end if

	    YawPoint(iplat,iinst) = yawang

	else if(itype.eq.SCANS) then

	    do iscan=1,NumScan(ip)
40		write(*,'(a,i1,a,$)')
     .'   Enter look angle and yaw pointing angle for scan ',iscan,': '
		read(*,'(2f20.0)',err=40,end=999)
     .		 ScanLook(iplat,iinst,iscan),ScanYaw(iplat,iinst,iscan)
	    end do

 	else if(itype.eq.SCANR .or. itype.eq.SCANY) then

	else if(itype.eq.LIMB .or. itype.eq.LIMB2) then

	    if(LimbAltdMin(ip).eq.LimbAltdMax(ip)) then
		limbh = LimbAltdMin(ip)
	    else if(LimbAltdMax(ip).gt.0.0) then
60		write(*,'(a,$)')
     .		'   Enter altitude of observation point (km): '
		read(*,'(f20.0)',err=60,end=999) limbh
	    else
		limbh = 0.0
	    end if

	    LimbAltd(iplat,iinst)  = abs(limbh)

70	    write(*,'(a,$)')
     .		'   Enter yaw pointing angle (deg) [90]: '
	    read(*,'(q,f20.0)',err=70,end=999) iq,yawang
	    if(iq.eq.0) yawang = 90.0d0

	    YawPoint(iplat,iinst) = yawang

	    if(itype.eq.LIMB2) then

80		write(*,'(a,$)') '   Enter limb thickness (km): '
		read(*,'(f20.0)',err=80,end=999) limbt
		LimbThickness(iplat,iinst) = abs(limbt)

	    end if

	end if

999	continue
	end
