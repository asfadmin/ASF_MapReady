*.
*   GetProj - get map projection information, send it to SetProj
*
*   Args
*	err		L	output	true if an error occurred
*
*   Routines
*	SetProj - sets map projection variables
*
*   Arguments for SetProj (6 in all)
*
*	Projection	Args
*	----------	----
*	Cylindrical,	lat1,lon1,lat2,lon2
*	  Mercator,
*	  Miller
*	Lambert		lat1,lon1,lat2,lon2,parallel1,parallel2
*	North Pole,	minlat
*	  South Pole
*	Globe		lat0,lon0,tilt,zoom
*
*   History
*	03/06/89 Original
*
*	03/06/89 10:41
*..
	subroutine GetProj(input,arg)
	character input
	double precision arg(*)

	  character*100 SccsFileID
     -/'@(#)getproj.for	5.1 98/01/08 APS/ASF\0'/
** Includes
	include 'eosinclude:map.inc'

* arg are the arguments to SetProj and depend on the projection chosen

** Variables
	integer iproj

	logical err
	err = .false.

*	write(*,*)
*30	write(*,'(a)') ' Choose a projection -'
*	write(*,'(a)') '   Cylindrical (C), Mercator (M), Miller (O),'
*	write(*,'(a,a,$)') '   North Pole (N), South Pole (S),',
*   .				' Globe (G) [Cylindrical]: '
*	read(*,'(a)',err=30,end=999) input


	if(input.eq.'M' .or. input.eq.'m') then
	    iproj = MERC
	else if(input.eq.'L' .or. input.eq.'l') then
	    iproj = LAMB
	else if(input.eq.'O' .or. input.eq.'o') then
	    iproj = MILL
	else if(input.eq.'N' .or. input.eq.'n') then
	    iproj = NPOLE
	else if(input.eq.'S' .or. input.eq.'s') then
	    iproj = SPOLE
	else if(input.eq.'G' .or. input.eq.'g') then
	    iproj = GLOBE
	else
	    iproj = CYL
	end if

* Polar

	if(iproj.eq.NPOLE .or. iproj.eq.SPOLE) then
*35	    continue
*	    if(iproj.eq.NPOLE) then
*		write(*,'(/,a,$)') ' Enter minimum latitude: '
*	    else
*		write(*,'(/,a,$)') ' Enter maximum latitude: '
*	    end if
*	    read(*,'(f20.0)',err=40,end=999) arg(1)
* if(abs(arg(1)).eq.90.0d0) go to 35
* Globe

	else if(iproj.eq.GLOBE) then

*	    write(*,*)
*40	    write(*,'(a,$)')
*     .' Enter lat,lon of center point and axis tilt [0,0,0]: '
*	    read(*,'(3f20.0)',err=40,end=999) arg(1),arg(2),arg(3)

* Rectangular

	else
*	    write(*,*)
*50	    write(*,'(a)') ' Enter lat,lon for upper left, lower right
*     . corner of the map [entire map]:'
*	    read(*,'(4f20.0)',err=50,end=999)
*     .			arg(1),arg(2),arg(3),arg(4)
	    if(arg(1).eq.0.0 .and. arg(2).eq.0.0 .and.
     .			arg(3).eq.0.0 .and. arg(4).eq.0.0) then
		arg(1) = 90.0d0
		arg(2) = -180.0d0
		arg(3) = -90.0d0
		arg(4) = 180.0d0
	    end if

	end if

*	if(iproj.eq.LAMB) then
*	    write(*,*)
*70	    write(*,'(a,$)') ' Enter two standard parallels: '
*	    read(*,'(2f20.0)',err=70,end=999) arg(5),arg(6)
*	end if

*	if(iproj.eq.GLOBE) then
*	    write(*,*) 
*100	    write(*,'(a,$)') ' Enter zoom factor [1]: '
*	    read(*,'(f20.0)',err=100,end=999) arg(4)
*	    if(arg(4).le.0.0) arg(4) = 1.0
*	end if

* SetProj

	call SetProj(iproj,arg)

	go to 1000

999	continue
	err = .true.

1000	continue
	end
