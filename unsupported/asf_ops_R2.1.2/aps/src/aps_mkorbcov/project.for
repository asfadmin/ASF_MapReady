
*.
*   Project - transforms latd,lon,altd into screen x,y using current map
*		projection
*
*   Args
*	latd,lon		double	input	geod lat, lon (deg)
*	altd			double	input	geodetic altitude (km)
*	x,y			I*2	output	screen coordinate
*	clip			logical	output	set if x,y is off the map
*
*	08/10/89 14:44
*..
	subroutine project(latd,lon,altd,x,y,clip)
	include 'eosinclude:map.inc'
	include 'eosinclude:cbody.inc'
	include 'eosinclude:constants.inc'
	double precision latd,lon,altd
	integer x,y
	logical clip

	  character*100 SccsFileID
     -/'@(#)project.for	5.1 98/01/08 APS/ASF\0'/
	logical clips
	double precision latc,r,theta,lambda,xx(3),xxx(3),ryzplane
	double precision rho

	double precision xreal,yreal

c Functions
	double precision ToLatc
	logical dtwixt
	double precision ToMerc,ToMill

	clip = .false.
	clips = .false.

	if(proj.eq.CYL) then
	    xreal = scalex * lon + x0
	    if(MapCut .and. dtwixt(-180.0d0,lon,winx(2)))
     .		xreal = xreal + xWrap
	    yreal = scaley * latd + y0
	    x = xreal
	    y = yreal

	else if(proj.eq.MERC) then
	    xreal = scalex * lon + x0
	    if(MapCut .and. dtwixt(-180.0d0,lon,winx(2)))
     .		xreal = xreal + xWrap
	    yreal = scaley * ToMerc(latd) + y0
	    x = xreal
	    y = yreal

	else if(proj.eq.MILL) then
	    xreal = scalex * lon + x0
	    if(MapCut .and. dtwixt(-180.0d0,lon,winx(2)))
     .		xreal = xreal + xWrap
	    yreal = scaley * ToMill(latd) + y0
	    x = xreal
	    y = yreal

	else if(proj.eq.NPOLE .or. proj.eq.SPOLE) then
	    if(proj.eq.NPOLE) then
		theta = (-lon-90.0d0) * rads
		r = (90.0d0-latd)
		if(latd.lt.lolat) clip = .true.
	    else
		theta = (lon-90.0d0) * rads
		r = (-90.0d0-latd)
		if(latd.gt.-lolat) clip = .true.
	    end if
	    xreal = scalex * r * cos(theta) + x0
	    yreal = scaley * r * sin(theta) + y0
	    x = xreal
	    y = yreal

	else if(proj.eq.GLOBE) then

	    call spherexyz(latd,lon,altd,xx)

c	    Rotate x,y,z to view earth centered at desired tilt,lat,lon

	    call xrot(xx,tilt,xxx)
	    call zrot(xxx,lon0,xx)
	    call yrot(xx,-lat0,xxx)

c	    Convert new x,y,z to screen coords

	    ryzplane = sqrt(xxx(2)**2 + xxx(3)**2)
	    if(xxx(1).ge.0 .or. 
     .	(altd.gt.0.0 .and. xxx(1).lt.0.0 .and. ryzplane.ge.RBody)) then
		xreal = scalex * winr * xxx(2) / RBody + x0
		yreal = scaley * winr * xxx(3) / RBody + y0
		x = xreal
		y = yreal
	    else
		clip = .true.
	    end if

	else if(proj.eq.LAMB) then
	    rho = cLambert * tan(0.5*(halfpi-latd*rads))**SinParallel0
	    theta = (lon * rads - lon0) * SinParallel0
	    xreal = scalex * rho * sin(theta) + x0
c	    if(MapCut .and. dtwixt(-180.0,lon,winx(2))) xreal = xreal + xWrap
	    yreal = scaley * rho * cos(theta) + y0
	    x = xreal
	    y = yreal
	else
	    call ErrorMessage('Project - "proj" undefined')
	end if

	if(.not.clip) then
	    call CheckScreenD(xreal,yreal,clip)
	end if

	end
