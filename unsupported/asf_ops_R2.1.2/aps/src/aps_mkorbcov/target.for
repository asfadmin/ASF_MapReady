
*.
*   Target.for - target drawing routines
*
*	TargetA(latd,lon,np) - draw a target area
*	TargetL(latd,lon,np) - draw a line target
*	TargetP(latd,lon) - draw a target point
*
*   Args
*	latd()			double	input	geod. latitude (deg)
*	lon()			double	input	longitude (deg)
*	np			integer	input	number of points in the target
*
*	2/12/88 9:10
*..
	subroutine TargetA(latd,lon,np)
	include 'eosinclude:map.inc'
	include 'eosinclude:constants.inc'
	double precision latd(*),lon(*)
	integer np

	  character*100 SccsFileID
     -/'@(#)target.for	5.1 98/01/08 APS/ASF\0'/
	integer i,j
	integer x,y,xy(1000)
	logical clip,ClipDate
	double precision lastlon

	ClipDate = .false.
	lastlon = lon(1)
	j = -1

	do i=1,np
c..aap	    call project(latd(i),lon(i),0.0,x,y,clip)
	    call project(latd(i),lon(i),dzero,x,y,clip)
	    if(MapWrap) then
		call DateLine(lastlon,lon(i),ClipDate)
		lastlon = lon(i)
	    end if
	    if(clip .or. ClipDate) then
		if(j.gt.-1) call vector(xy,(j+1)/2)
		j = -1
	    else
		j = j + 2
		xy(j) = x
		xy(j+1) = y
	    end if
	end do

c..aap	call project(latd(1),lon(1),0.0,x,y,clip)
	call project(latd(1),lon(1),dzero,x,y,clip)
	if(MapWrap) then
	    call DateLine(lastlon,lon(1),ClipDate)
	end if
	if(clip .or. ClipDate) then
	    if(j.gt.-1) call vector(xy,(j+1)/2)
	    j = -1
	else
	    j = j + 2
	    xy(j) = x
	    xy(j+1) = y
	end if

	if(j.gt.-1) call vector(xy,(j+1)/2)

	end


	subroutine TargetL(latd,lon,np)
	include 'eosinclude:map.inc'
	include 'eosinclude:constants.inc'
	double precision latd(*),lon(*)
	integer np

	integer i,j
	integer x,y,xy(1000)
	logical clip,ClipDate
	double precision lastlon

	ClipDate = .false.
	lastlon = lon(1)
	j = -1

	do i=1,np
c..aap	    call project(latd(i),lon(i),0.0,x,y,clip)
	    call project(latd(i),lon(i),dzero,x,y,clip)
	    if(MapWrap) then
		call DateLine(lastlon,lon(i),ClipDate)
		lastlon = lon(i)
	    end if
	    if(clip .or. ClipDate) then
		if(j.gt.-1) call vector(xy,(j+1)/2)
		j = -1
	    else
		j = j + 2
		xy(j) = x
		xy(j+1) = y
	    end if
	end do

	if(j.gt.-1) call vector(xy,(j+1)/2)

	end


	subroutine TargetP(latd,lon)
	include 'eosinclude:constants.inc'
	double precision latd,lon

	integer x,y
	logical clip

c..aap	call project(latd,lon,0.0,x,y,clip)
	call project(latd,lon,dzero,x,y,clip)

	if(.not.clip) call cross(x,y)

	end
