
*.
*   ReadTargets - read targets from a GetTargets file
*
*   Args
*	UnitInput		integer	input	unit number of target file
*	latd,lon		double	output	latitude, longitude arrays
*	NumPoints		integer	output	number of lat/lon pairs in target
*	TargetName		C**	output	target name
*	init			logical	in/out	true if first time routine called
*	err,eof			logical	output	read error and eof flags
*
*   Assumes
*	UnitInput is already open
*
*	01/24/89 - added NoSpaces(TargetName)
*	10/31/88 - 
*
*	01/24/89 09:53
*..
	subroutine ReadTargets(UnitInput,latd,lon,NumPoints,TargetName,
     .		init,err,eof)
	integer UnitInput
	double precision latd(*),lon(*)
	integer NumPoints
	character*(*) TargetName
	logical init,err,eof

	  character*100 SccsFileID
     -/'@(#)readtargets.for	5.1 98/01/08 APS/ASF\0'/
	character*80 HeaderInput,DegreeFormat,FmtInput
	integer MaxPointsPerTarget
	parameter(MaxPointsPerTarget=1000)
	integer ilatd(MaxPointsPerTarget),ilatdmin(MaxPointsPerTarget)
	integer ilon(MaxPointsPerTarget),ilonmin(MaxPointsPerTarget)
	logical DegreeInput
	integer ios,i

	err = .false.

	if(init) then
	    read(UnitInput,'(a)',iostat=ios,err=10,end=900) HeaderInput
10	    continue
	    if(ios.gt.0) then
		write(*,'(/a/)') ' *** Error in ReadTargets - no header ***'
		err = .true.
		go to 900
	    end if
c	    if(HeaderInput(1:10) .ne. 'GetTargets' .and.
c     .			HeaderInput(2:11) .ne. 'GetTargets') then
c		write(*,'(/a/)') ' *** This file needs PlotTargetsOld ***'
c		go to 900
c	    end if

	    read(UnitInput,'(a)',iostat=ios,err=20,end=900) FmtInput
20	    continue
	    if(ios.gt.0) then
		write(*,'(/a/)') ' *** Error in ReadTargets - no format ***'
		err = .true.
		go to 900
	    end if

	    read(UnitInput,'(a)',iostat=ios,err=30,end=900) DegreeFormat
30	    continue
	    if(ios.gt.0) then
		write(*,'(/a/)')
     .			' *** Error in ReadTargets - no degree format ***'
		err = .true.
		go to 900
	    end if
	    call NoSpaces(DegreeFormat)
	    call ToUpperS(DegreeFormat)
	    DegreeInput = (DegreeFormat(1:2).ne.'DM')
	    init = .false.

	end if

	read(UnitInput,'(a)',iostat=ios,err=100,end=900) TargetName
100	continue
	if(ios.gt.0) then
	    write(*,'(/a/)') ' *** Error in ReadTargets - bad target name ***'
		err = .true.
	    go to 900
	end if
	call NoSpaces(TargetName)

	read(UnitInput,*,iostat=ios,err=110,end=900) NumPoints
110	continue
	if(ios.gt.0) then
	    write(*,'(/a/)') ' *** Error in ReadTargets - bad numpoints ***'
	    err = .true.
	    go to 900
	end if

	if(DegreeInput) then
	    read(UnitInput,FmtInput,iostat=ios,err=120,end=900)
     .		(latd(i),lon(i),i=1,NumPoints)
	else
	    read(UnitInput,FmtInput,iostat=ios,err=120,end=900)
     .		(ilatd(i),ilatdmin(i),ilon(i),ilonmin(i),i=1,NumPoints)
	end if
120	continue
	if(ios.gt.0) then
	    write(*,'(/a/)') ' *** Error in ReadTargets - bad lat/lons ***'
	    err = .true.
	    go to 999
	end if

	if(.not.DegreeInput) then
	    do i=1,NumPoints
		call DegMin2Deg(ilatd(i),ilatdmin(i),latd(i))
		call DegMin2Deg(ilon(i),ilonmin(i),lon(i))
	    end do
	end if

	go to 999

900	continue
	eof = .true.

999	continue
	end
