*.
*   DateLine(lastlon,lon,ClipDate) - checks crossing east/west edge of full map
*
*   Args
*	lastlon		double	input	recent longitude (deg)
*	lon		double	input	current longitude (deg)
*	ClipDate	logical	output	true if east/west edge of map crossed
*
*   Assumes
*	Projection has been set
*
*	08/02/89 14:29
*..
	subroutine DateLine(lastlon,lon,ClipDate)
	include 'eosinclude:map.inc'
	double precision lastlon,lon
	logical ClipDate

	double precision LonWest,LonEast

	  character*100 SccsFileID
     -/'@(#)dateline.for	5.1 98/01/08 APS/ASF\0'/
* Functions
	logical dTwixt

	if(.not.MapWrap) then
	    ClipDate = .false.
	    go to 999
	end if

c	if(
c     .		((lastlon.ge.90.0 .and. lastlon.le.180.0)
c     .			.and.
c     .		(lon.ge.-180.0 .and. lon.le.-90.0))
c     .			.or.
c     .		((lastlon.ge.-180.0 .and. lastlon.le.-90.0)
c     .			.and.
c     .		(lon.ge.90.0 .and. lon.le.180.0))
c     .    ) then
c	    ClipDate = .true.
c	else
c	    ClipDate = .false.
c	end if

	LonWest = winx(2) - 90.0
	LonEast = winx(2) + 90.0

	if(dTwixt(-90.0,winx(2),90.0)) then
	    if(
     .		(dTwixt(LonWest,lastlon,winx(2)) .and.
     .		 dTwixt(winx(2),lon,LonEast))
     .				.or.
     .		(dTwixt(LonWest,lon,winx(2)) .and.
     .		 dTwixt(winx(2),lastlon,LonEast))
     .	  ) then
	      ClipDate = .true.
	    else
	      ClipDate = .false.
	    end if
	else if(dTwixt(90.0,winx(2),180.0)) then
	    LonEast = LonEast - 360.0
	    if(
     .		(dTwixt(LonWest,lastlon,winx(2)) .and.
     .		 (dTwixt(winx(2),lon,180.0) .or.
     .		  dTwixt(-180.0,lon,LonEast)))
     .				.or.
     .		(dTwixt(LonWest,lon,winx(2)) .and.
     .		 (dTwixt(winx(2),lon,180.0) .or.
     .		  dTwixt(-180.0,lastlon,LonEast)))
     .	      ) then
		  ClipDate = .true.
		else
		  ClipDate = .false.
		end if
	else if(dTwixt(-90.0,winx(2),-180.0)) then
	    LonWest = 360.0 + LonWest 
	    if(
     .		((dTwixt(LonWest,lastlon,180.0) .or.
     .		 dTwixt(-180.0,lastlon,winx(2)))  .and.
     .		 dTwixt(winx(2),lon,LonEast))
     .				.or.
     .		((dTwixt(LonWest,lon,180.0) .or.
     .		 dTwixt(-180.0,lon,winx(2)))  .and.
     .		 dTwixt(winx(2),lastlon,LonEast))
     .	      ) then
		  ClipDate = .true.
		else
		  ClipDate = .false.
		end if
	end if

999	continue
	end
