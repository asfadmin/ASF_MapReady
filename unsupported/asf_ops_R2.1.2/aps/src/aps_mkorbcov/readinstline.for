
*.
*   ReadInstLine - read instrument data
*
*   Args
*	InLine		C**	input	Instrument data string
*	iinst		integer	in/out	instrument number
*	NewInst		logical	output	set true if new instrument line
*	NoKey		logical	output	set true if InLine has no valid keys
*
*	The instrument data file is a keyword file containing the following
*	information:
*
*	Keyword		Value			Example
*
*	INSTNAME	instrument acronym	instname = 'MODIS'
*	FULLINSTNAME	full instrument name	fullinstname = 'Moderate Resolution Imaging Spectrometer'
*	TYPE		instrument type		type = 'beam'
*	LOOKMIN		min look angle (deg)	lookmin = 0.0
*	LOOKMAX		max look angle (deg)	lookmax = 40.0
*	YAWMIN
*	YAWMAX
*	BEAMWIDTH	beam width (deg)	beamwidth = 90.0
*	SWATHWIDTH	swath width (km)	swathwidth = 1500.0
*	MASK		masking			mask = 'suntarget'
*	SUNELEVMIN	min sun elevation	sunelevmin = 10.0
*	LIMBALTMIN	min limb altd (km)
*	LIMBALTMAX	max limb altd (km)
*	LIMBTHICKMIN
*	LIMBTHICKMAX
*	SCANRATE	scan rate (deg/sec)
*
*   Routines
*	FindKeys
*	AtoD (R)
*	ErrorMessage
*
*   Commons
*	eos$instruments - InstName,InstFullName,NumInst,....
*
*   History
*	03/06/91 Changed MASKSUN* to MASKDAY*, added MASKNITE*
*	12/04/89 Added NewInst arg
*	12/03/89 Changed from ReadInst to ReadInstFile/ReadInstLine
*	08/07/89 Changed 'NAME' keyword to 'INSTNAME'
*	02/28/89 Original
*
*   Last modified
*	03/06/91 11:32
*..
	subroutine ReadInstLine(InLine,iinst,NewInst,NoKey)
	character*(*) InLine
	integer iinst
	logical NewInst,NoKey

	  character*100 SccsFileID
     -/'@(#)readinstline.for	5.1 98/01/08 APS/ASF\0'/
** Includes
	include 'eosinclude:instruments.inc'

** Variables

* Keys
	integer MAXKEY
	parameter(MAXKEY=20)
	integer NumKey,WordLen(MAXKEY),ValLen(MAXKEY),ikey
	character*80 KeyWord(MAXKEY),KeyVal(MAXKEY),ThisKey,ThisVal

* Other
	integer slenw,slenv
	character*80 typetext,MaskText
	integer itype,itracks,imask
	integer ios
	logical ValidKey

** Functions
	integer StringLen


	call FindKeys(InLine,MAXKEY,NumKey,
     .			KeyWord,WordLen,KeyVal,ValLen)

	NewInst = .false.
	ValidKey = .false.
	ios = 0

	do 100 ikey=1,NumKey

	    slenw = WordLen(ikey)
	    slenv = ValLen(ikey)
	    ThisKey = KeyWord(ikey)
	    ThisVal = KeyVal(ikey)

	    if(ThisKey(1:slenw).eq.'INSTNAME') then

		iinst = iinst + 1
		if(iinst.gt.MAXINST) return
		NumInst = iinst
		NewInst = .true.

		InstName(iinst) = ThisVal(1:slenv)

		InstType(iinst) = NONEINST
		tracks(iinst) = 0
		LookMin(iinst) = 0.0
		LookMax(iinst) = 360.0d0
		YawMin(iinst) = 0.0
		YawMax(iinst) = 360.0d0
		LimbAltdMin(iinst) = 0.0
		LimbAltdMax(iinst) = 1.0d6
		LimbThicknessMin(iinst) = 0.0
		LimbThicknessMax(iinst) = 1.0d6
		SunElevMin(iinst) = 0.0
		NumScan(iinst) = 0

		do imask=1,MAXMASK
		    mask(imask,iinst) = .false.
		end do

		ValidKey = .true.

	    else if(iinst.gt.0) then

		if(ThisKey(1:slenw).eq.'FULLINSTNAME') then
		    InstFullName(iinst) = ThisVal(1:slenv)
		    ValidKey = .true.

		else if(ThisKey(1:slenw).eq.'TYPE') then

		    typetext = ThisVal(1:slenv)
		    call ToUpperS(typetext(1:slenv))

		    if(typetext(1:slenv).eq.'BEAM') then
			itype = BEAM
		    else if(typetext(1:slenv).eq.'S/C' .or.
     .				typetext(1:slenv).eq.'SC') then
			itype = SC
		    else if(typetext(1:slenv).eq.'NADIR') then
			itype = NADIR
		    else if(typetext(1:slenv).eq.'SCAT') then
			itype = SCAT
		    else if(typetext(1:slenv).eq.'SCANS' .or.
     .				typetext(1:slenv).eq.'SCANSTEP') then
			itype = SCANS
		    else if(typetext(1:slenv).eq.'SCANR' .or.
     .				typetext(1:slenv).eq.'SCANROLL') then
			itype = SCANR
		    else if(typetext(1:slenv).eq.'SCANY' .or.
     .				typetext(1:slenv).eq.'SCANYAW') then
			itype = SCANY
		    else if(typetext(1:slenv).eq.'LIMB') then
			itype = LIMB
		    else if(typetext(1:slenv).eq.'LIMB2') then
			itype = LIMB2
		    else if(typetext(1:slenv).eq.'GLINT') then
			itype = GLINT
		    else if(typetext(1:slenv).eq.'HOTSPOT') then
			itype = HOTSPOT
		    else if(typetext(1:slenv).eq.'NONE') then
			itype = NONEINST
		    else
			itype = NONEINST
		    end if

		    InstType(iinst) = itype 

		    if(itype.eq.SC .or. itype.eq.NADIR .or.
     .			    itype.eq.SCANS .or. itype.eq.LIMB .or.
     .			    itype.eq.GLINT .or. itype.eq.HOTSPOT) then
			itracks = 1
		    else if(itype.eq.BEAM .or. itype.eq.LIMB2 .or.
     .			    itype.eq.SCANR .or. itype.eq.SCANY) then
			itracks = 2
		    else if(itype.eq.SCAT) then
			itracks = 4
		    else if(itype.eq.NONEINST) then
			itracks = 0
		    else
			itracks = 0
		    end if

		    tracks(iinst) = itracks

		    ValidKey = .true.

		else if(ThisKey(1:slenw).eq.'LOOKMIN' .or.
     .				ThisKey(1:slenw).eq.'MINLOOK') then
		    read(unit=ThisVal(1:slenv),fmt='(f20.5)',
     .				iostat=ios,err=90) LookMin(iinst)
		    ValidKey = .true.

		else if(ThisKey(1:slenw).eq.'LOOKMAX' .or.
     .				ThisKey(1:slenw).eq.'MAXLOOK') then
		    read(ThisVal(1:slenv),'(f20.0)',
     .				iostat=ios,err=90) LookMax(iinst)
		    ValidKey = .true.

		else if(ThisKey(1:slenw).eq.'YAWMIN' .or.
     .				ThisKey(1:slenw).eq.'MINYAW') then
		    read(ThisVal(1:slenv),'(f20.0)',
     .				iostat=ios,err=90) YawMin(iinst)
		    ValidKey = .true.

		else if(ThisKey(1:slenw).eq.'YAWMAX' .or.
     .				ThisKey(1:slenw).eq.'MAXYAW') then
		    read(ThisVal(1:slenv),'(f20.0)',
     .				iostat=ios,err=90) YawMax(iinst)
		    ValidKey = .true.

		else if(ThisKey(1:slenw).eq.'BEAMWIDTH') then
		    read(ThisVal(1:slenv),'(f20.0)',
     .				iostat=ios,err=90) BeamWid(iinst)
		    ValidKey = .true.

		else if(ThisKey(1:slenw).eq.'SWATHWIDTH') then
		    read(ThisVal(1:slenv),'(f20.0)',
     .				iostat=ios,err=90) SwathWid(iinst)
		    ValidKey = .true.

		else if(ThisKey(1:slenw).eq.'SCANRATE') then
		    read(ThisVal(1:slenv),'(f20.0)',
     .				iostat=ios,err=90) ScanRate(iinst)
		    ValidKey = .true.

		else if(ThisKey(1:slenw).eq.'MINLIMBALT' .or.
     .				ThisKey(1:slenw).eq.'LIMBALTMIN') then
		    read(ThisVal(1:slenv),'(f20.0)',
     .				iostat=ios,err=90) LimbAltdMin(iinst)
		    ValidKey = .true.

		else if(ThisKey(1:slenw).eq.'MAXLIMBALT' .or.
     .				ThisKey(1:slenw).eq.'LIMBALTMAX') then
		    read(ThisVal(1:slenv),'(f20.0)',
     .				iostat=ios,err=90) LimbAltdMax(iinst)
		    ValidKey = .true.

		else if(ThisKey(1:slenw).eq.'MINLIMBTHICK' .or.
     .				ThisKey(1:slenw).eq.'LIMBTHICKMIN') then
		    read(ThisVal(1:slenv),'(f20.0)',
     .				iostat=ios,err=90) LimbThicknessMin(iinst)
		    ValidKey = .true.

		else if(ThisKey(1:slenw).eq.'MAXLIMBTHICK' .or.
     .				ThisKey(1:slenw).eq.'LIMBTHICKMAX') then
		    read(ThisVal(1:slenv),'(f20.0)',
     .				iostat=ios,err=90) LimbThicknessMax(iinst)
		    ValidKey = .true.

		else if(ThisKey(1:slenw).eq.'MINSUNELEV' .or.
     .				ThisKey(1:slenw).eq.'SUNELEVMIN') then
		    read(ThisVal(1:slenv),'(f20.0)',
     .				iostat=ios,err=90) SunElevMin(iinst)
		    ValidKey = .true.

		else if(ThisKey(1:slenw).eq.'MASK') then
		    MaskText = ThisVal(1:slenv)
		    call ToUpperS(MaskText(1:slenv))
		    if(MaskText(1:slenv).eq.'DAYTARGET') then
			mask(MASKDAYT,iinst) = .true.
			ValidKey = .true.
		    else if(MaskText(1:slenv).eq.'DAYPLATFORM') then
			mask(MASKDAYP,iinst) = .true.
			ValidKey = .true.
		    else if(MaskText(1:slenv).eq.'NITETARGET') then
			mask(MASKNITET,iinst) = .true.
			ValidKey = .true.
		    else if(MaskText(1:slenv).eq.'NITEPLATFORM') then
			mask(MASKNITEP,iinst) = .true.
			ValidKey = .true.
		    else if(MaskText(1:slenv).eq.'TDRS') then
			mask(MASKTDRS,iinst) = .true.
			ValidKey = .true.
		    else if(MaskText(1:slenv).eq.'STATION') then
			mask(MASKSTN,iinst) = .true.
			ValidKey = .true.
		    else if(MaskText(1:slenv).eq.'ASCENDING') then
			mask(MASKASC,iinst) = .true.
			ValidKey = .true.
		    else if(MaskText(1:slenv).eq.'DESCENDING') then
			mask(MASKDSC,iinst) = .true.
			ValidKey = .true.
		    end if

		end if

	    end if

90	    continue
	    if(ios.ne.0) then
		call ErrorMessage('ReadInstLine - bad key value: '
     .				// ThisVal(1:slenv)
     .				// ' for ' // ThisKey(1:slenv))
		call WaitKey
	    end if

100	continue

	NoKey = .not.ValidKey

	end
