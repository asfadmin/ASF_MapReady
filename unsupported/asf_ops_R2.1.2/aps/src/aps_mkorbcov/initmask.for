
*.
*   InitMask - set ground station receiving mask data
*
*   Args
*	FileInput	C**	input	mask data file - if blank, use default
*
*   History
*	 3/26/91 now set MaxElev(istat) = MaxEl for each station (oops!)
*	 3/25/91 Use new IJKtoSEZ routines
*	 5/ 8/90 if AzSteps(i) = 1, set AzSteps = MaxAzSteps
*	 2/22/89
*
*   Last modified
*	3/26/91 11:52
*..
	subroutine InitMask(FileInput)

	include 'eosinclude:files.inc'
	include 'eosinclude:station.inc'

	  character*100 SccsFileID
     -/'@(#)initmask.for	5.1 98/01/08 APS/ASF\0'/
	character*(*) FileInput
	integer uIn
	character*80 fIn,FmtIn
	integer ios
	logical NotOk

	integer istat,iaz,StationList(MaxStations),ilist
	double precision MaxEl,MinEl,el

** Data
	data uIn /81/

** Functions
	integer StringLen

	do istat=1,MaxStations
	    StationList(istat) = 0
	    StationSelected(istat) = .false.
	end do

* Open station mask data file

	if(FileInput(1:1).eq.' ') then
	    fIn = DataDir // 'station.dat'
	else
	    fIn = FileInput
	end if

	inquire(unit=uIn,opened=NotOk)
	if(NotOk) then
	    call ErrorMessage('InitMask - unit already open')
	    return
	end if

	open(unit=uIn,file=fIn,status='old',
     .		form='formatted',iostat=ios,err=10)
c..aap     .		form='formatted',readonly,iostat=ios,err=10)
10	continue
	if(ios.ne.0) then
	    call ErrorMessage('InitMask - couldn''t open station file '
     .				// fIn(1:StringLen(fIn)))
	    return
	end if

* Read station data

	do istat=1,MaxStations
	    read(uIn,'(a)',err=100,end=100) FmtIn
	    read(uIn,FmtIn,err=100,end=100)
     .			StationLatd(istat),StationLon(istat),
     .			StationHeight(istat),
     .			StationName(istat),AzSteps(istat),
     .			(MaskElev(iaz,istat),iaz=1,AzSteps(istat))

	    if(AzSteps(istat) .gt. MaxAzSteps) then
		PAUSE 'Station contains too many azimuth steps'
	    end if

	    if(AzSteps(istat).eq.1) then
		AzSteps(istat) = MaxAzSteps
		el = MaskElev(1,istat)
		do iaz=2,MaxAzSteps
		    MaskElev(iaz,istat) = el
		end do
	    end if

	    MaxEl = -90.0d0
	    do iaz=1,AzSteps(istat)
		MaxEl = max(MaxEl,MaskElev(iaz,istat))
	    end do
	    MaxElev(istat) = MaxEl
		
c	    write(*,50) istat,StationName(istat)
c50	    format(1x,i3,2x,a)

	    NumStations = istat
	end do

100	continue
	if(NumStations.eq.0) go to 999

* Show station names

150	write(*,'(/,a,/)') ' Choose stations:'

	call ShowNames(StationName,NumStations)

	write(*,*)

	do ilist=1,MaxStations
	    write(*,'(a,$)') ' Enter station number [done]: '
200	    read(*,'(i10)',err=200,end=999) StationList(ilist)
	    if(StationList(ilist).eq.0) go to 225
	end do
225	continue

	do ilist=1,NumStations
	    istat = StationList(ilist)
	    if(istat.ne.0) then
		StationSelected(istat) = .true.
		write(*,250) StationName(istat)
250		format(/,1x,'For station ',a)
260		write(*,'(a,$)') '  Input minimum elevation (deg) [0.0]: '
		read(*,'(f20.0)',err=260,end=999) MinElev(istat)
		MaxElev(istat) = max(MaxElev(istat),MinElev(istat))
		MinEl = 90.0d0
		do iaz=1,AzSteps(istat)
		    MinEl = min(MinEl,MaskElev(iaz,istat))
	WRITE(*,*) (IAZ-1)*360/AZSTEPS(ISTAT),MASKELEV(IAZ,ISTAT)
		end do
		MinElev(istat) = max(MinEl,MinElev(istat))
		call SphereXYZ(StationLatd(istat),StationLon(istat),
     .				StationHeight(istat),rStation(1,istat))
		call InitIJKtoSEZ(rStation(1,istat),tmStation(1,1,istat))
	    end if
	end do

999	continue
	close(uIn)

	end
