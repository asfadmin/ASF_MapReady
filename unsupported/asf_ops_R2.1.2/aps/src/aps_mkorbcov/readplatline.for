
*.
*   ReadPlatLine - read platform data line
*
*   Args
*	InLine		C**	input	Platform data line
*	iplat		I	in/out	platform number
*	NewPlat		L	output	set true if new platform
*	NoKey		L	output	set true if no key found
*
*	The platform data file is a keyword file containing the following
*	information:
*
*	Keyword		Value			Example
*
*	PLATNAME	platform name 		platname = 'Eos-A'
*	A		semi-major axis	(km)	a = 6678.15
*	H		altitude (km)		h = 300.0
*	E		eccentricity		e = 0.001
*	I		inclination (deg)	i = 97.5
*	NODE0		long. asc. node (deg)	node0 = 56.035
*	OMEGA0		arg. periapsis (deg)	omega0 = 90.0
*	M0		mean anomaly (deg)	m0 = -90.0
*	T0		(epoch-basegmt) (hr)	t0 = 0.0
*	NODETIME	local time of node (hr)	nodetime = 13.5
*	Q		orbits per day		Q = 14.5
*	QI		Q = QI + QN/QD		QI = 14
*	QN		Q = QI + QN/QD		QN = 1
*	QD		Q = QI + QN/QD		QD = 2
*
*   Note
*	If H is given, E is assumed 0.0 and is not required
*	The NODETIME key uses orbital elements A,E,I,NODE0,OMEGA0,M0,T0 and
*	    therefore these must be set first
*	Use QI,QN,QD (must be on same line) or Q, not both.  Sun-synch assumed
*
*   Routines
*	FindKeys
*	AtoD (double)
*	ErrorMessage
*
*   Commons
*	eos$instruments - PlatName,active,NumPlat,NumInstPlat
*
*   History
*	06/08/90 Added Q,QI,QN,QD keywords
*	12/04/89 Added NewPlat arg
*	12/03/89 ReadPlat changed to ReadPlatFile/ReadPlatLine
*	08/08/89 Platform status is "active"
*	08/07/89 Changed keyword 'NAME' to 'PLATNAME'
*	07/26/89 Original
*
*	06/08/90 09:52
*..
	subroutine ReadPlatLine(InLine,iplat,NewPlat,NoKey)
	character*(*) InLine
	integer iplat
	logical NewPlat,NoKey

** Includes
	include 'eosinclude:instruments.inc'
	include 'eosinclude:orbit.inc'
	include 'eosinclude:cbody.inc'
	include 'eosinclude:constants.inc'
	
	  character*100 SccsFileID
     -/'@(#)readplatline.for	5.1 98/01/08 APS/ASF\0'/
** Variables
* Keys
	integer MAXKEY
	parameter(MAXKEY=20)
	integer NumKey,WordLen(MAXKEY),ValLen(MAXKEY),ikey
	character*80 KeyWord(MAXKEY),KeyVal(MAXKEY),ThisKey,ThisVal

* Other
	integer slenw,slenv
	double precision h,node0hour
	double precision Q
	integer QI,QN,QD
	integer ios
	logical ValidKey

** Functions
	integer StringLen


* Read Keys

	call FindKeys(InLine,MAXKEY,NumKey,
     .					KeyWord,WordLen,KeyVal,ValLen)

	NewPlat = .false.
	Q = 0.0d0
	QI = 0
	QN = 0
	QD = 0
	ValidKey = .false.
	ios = 0

	do 100 ikey=1,NumKey

	    slenw = WordLen(ikey)
	    slenv = ValLen(ikey)
	    ThisKey = KeyWord(ikey)
	    ThisVal = KeyVal(ikey)

	    if(ThisKey(1:slenw).eq.'PLATNAME') then

		iplat = iplat + 1
		if(iplat.gt.MAXPLAT) return
		NumPlat = iplat
		NewPlat = .true.

		PlatName(iplat) = ThisVal(1:slenv)
		active(iplat) = .true.
		NumInstPlat(iplat) = 0

		ValidKey = .true.

	    else if(iplat.gt.0) then

		if(ThisKey(1:slenw).eq.'A') then
		    read(ThisVal(1:slenv),'(f20.0)',
     .				iostat=ios,err=90) a(iplat)
		    ValidKey = .true.

		else if(ThisKey(1:slenw).eq.'H') then
		    read(ThisVal(1:slenv),'(f20.0)',
     .				iostat=ios,err=90) h
		    a(iplat) = RBody + h
		    e(iplat) = 0.0d0
		    ValidKey = .true.

		else if(ThisKey(1:slenw).eq.'Q') then
		    read(ThisVal(1:slenv),'(f20.0)',
     .				iostat=ios,err=90) Q
		    ValidKey = .true.

		else if(ThisKey(1:slenw).eq.'QI') then
		    read(ThisVal(1:slenv),'(i10)',
     .				iostat=ios,err=90) QI
		    ValidKey = .true.

		else if(ThisKey(1:slenw).eq.'QN') then
		    read(ThisVal(1:slenv),'(i10)',
     .				iostat=ios,err=90) QN
		    ValidKey = .true.

		else if(ThisKey(1:slenw).eq.'QD') then
		    read(ThisVal(1:slenv),'(i10)',
     .				iostat=ios,err=90) QD
		    ValidKey = .true.

		else if(ThisKey(1:slenw).eq.'E') then
		    read(ThisVal(1:slenv),'(f20.0)',
     .				iostat=ios,err=90) e(iplat)
		    ValidKey = .true.

		else if(ThisKey(1:slenw).eq.'I') then
		    read(ThisVal(1:slenv),'(f20.0)',
     .				iostat=ios,err=90) i(iplat)
		    i(iplat) = i(iplat) * rads
		    ValidKey = .true.

		else if(ThisKey(1:slenw).eq.'NODE0') then
		    read(ThisVal(1:slenv),'(f20.0)',
     .				iostat=ios,err=90) node0(iplat)
		    node0(iplat) = node0(iplat) * rads
		    ValidKey = .true.

		else if(ThisKey(1:slenw).eq.'OMEGA0') then
		    read(ThisVal(1:slenv),'(f20.0)',
     .				iostat=ios,err=90) omega0(iplat)
		    omega0(iplat) = omega0(iplat) * rads
		    ValidKey = .true.

		else if(ThisKey(1:slenw).eq.'M0') then
		    read(ThisVal(1:slenv),'(f20.0)',
     .				iostat=ios,err=90) m0(iplat)
		    m0(iplat) = m0(iplat) * rads
		    ValidKey = .true.

		else if(ThisKey(1:slenw).eq.'T0') then
		    read(ThisVal(1:slenv),'(f20.0)',
     .				iostat=ios,err=90) t0(iplat)
		    t0(iplat) = t0(iplat) * 3600.0d0
		    ValidKey = .true.

		else if(ThisKey(1:slenw).eq.'NODETIME') then
		    read(ThisVal(1:slenv),'(f20.0)',
     .				iostat=ios,err=90) node0hour
		    if(node0(iplat).gt.twopi) then
			call LocalTimeToNode(a(iplat),e(iplat),
     .						i(iplat),node0(iplat),
     .						omega0(iplat),m0(iplat),
     .						t0(iplat),node0hour)
		    end if
		    ValidKey = .true.

		end if

	    end if

90	    continue
	    if(ios.ne.0) then
		call ErrorMessage('ReadPlatLine - bad key value: '
     .				// ThisVal(1:slenv)
     .				// ' for ' // ThisKey(1:slenv))
		call WaitKey
	    end if

100	continue

	Q = abs(Q)
	if(Q.lt.1.0d-3) then
	    if(QD.ne.0) Q = dble(abs(QI)) + dble(abs(QN))/dble(abs(QD))
	end if
	if(Q.gt.1.0d-3) then
	    call QtoHSS(Q,h,i(iplat))
	    a(iplat) = RBody + h
	    e(iplat) = 0.0d0
	end if

	NoKey = .not.ValidKey

	end
