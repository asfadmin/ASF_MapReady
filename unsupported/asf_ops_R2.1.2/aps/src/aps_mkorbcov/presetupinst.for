
*.
*   PreSetUpInst - set instrument parameters to logical defaults
*
*   Args
*	iplat		integer	input	platform number
*	iinst		integer	input	# instrument on plat
*
*   History
*	03/22/89 Original
*
*	03/22/89 21:59
*..
	subroutine PreSetUpInst(iplat,iinst)
	include 'eosinclude:instruments.inc'
	integer iplat,iinst

	  character*100 SccsFileID
     -/'@(#)presetupinst.for	5.1 98/01/08 APS/ASF\0'/
	integer ip,itype,iscan,iq

	ip = platform(iplat,iinst)
	itype = InstType(ip)

	LookPoint(iplat,iinst) = 0.0
	YawPoint(iplat,iinst) = 0.0

	if(itype.eq.BEAM .or. itype.eq.SCAT) then

	    if(LookMin(ip).eq.LookMax(ip)) then
		LookPoint(iplat,iinst)  = abs(LookMin(ip))
	    else if(LookMax(ip).eq.0.0) then
		LookPoint(iplat,iinst) = 0.0
	    end if

	else if(itype.eq.LIMB .or. itype.eq.LIMB2) then

	    if(LimbAltdMin(ip).eq.LimbAltdMax(ip)) then
		LimbAltd(iplat,iinst) = abs(LimbAltdMin(ip))
	    end if

	    if(itype.eq.LIMB2) then
		
		if(LimbThicknessMin(ip).eq.LimbThicknessMax(ip)) then
		    LimbThickness(iplat,iinst)
     .				= abs(LimbThicknessMin(ip))
		end if

	    end if

	end if

	if(YawMin(ip).eq.YawMax(ip)) then
	    YawPoint(iplat,iinst) = YawMin(ip)
	else
	    YawPoint(iplat,iinst) = 90.0d0
	end if

	end
