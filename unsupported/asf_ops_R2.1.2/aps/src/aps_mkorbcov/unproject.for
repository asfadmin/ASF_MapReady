
*.
*   UnProject(x,y,latd,lon) - transforms screen x,y into latd,lon
*
*	06/10/88 12:35
*..
	subroutine UnProject(x,y,latd,lon)
	include 'eosinclude:constants.inc'
	include 'eosinclude:map.inc'
	integer x,y
	double precision latd,lon

	  character*100 SccsFileID
     -/'@(#)unproject.for	5.1 98/01/08 APS/ASF\0'/
	double precision latc,r,theta,lambda,xx(3),xxx(3),matharg
	logical clip,clips,clipw

* Functions
	double precision ToLatc,ToLatd


	if(proj.eq.CYL) then
	    lon = (x-x0)/scalex
	    latd = (y-y0)/scaley

	    if(lon.gt.180.0d0) lon = lon - 360.0d0
	    if(lon.lt.-180.0d0) lon = lon + 360.0d0

	else if(proj.eq.MERC) then
	    lon = (x-x0)/scalex
	    matharg = (y-y0)*rads/scaley
	    if(matharg.gt.0.0) then
		latd = 2.0*atan(log(matharg))/rads - 90.0d0
	    else
		latd = 999.0d0
	    end if

	    if(lon.gt.180.0d0) lon = lon - 360.0d0
	    if(lon.lt.-180.0d0) lon = lon + 360.0d0

	else if(proj.eq.MILL) then		! Miller Cylindrical not implemented
	    lon = (x-x0)/scalex
	    latd = (y-y0)/scaley

	    if(lon.gt.180.0d0) lon = lon - 360.0d0
	    if(lon.lt.-180.0d0) lon = lon + 360.0d0

	else if(proj.eq.LAMB) then		! Lambert (JNC) not implemented
	    lon = (x-x0)/scalex
	    latd = (y-y0)/scaley

	    if(lon.gt.180.0d0) lon = lon - 360.0d0
	    if(lon.lt.-180.0d0) lon = lon + 360.0d0

	else if(proj.eq.NPOLE .or. proj.eq.SPOLE) then

	    if(x.eq.x0 .and. y.eq.y0) then
		if(proj.eq.NPOLE) then
		    latd = 90.0d0
		else
		    latd = -90.0d0
		end if
		lon = 0.0
		return
	    else
		theta = atan2((y-y0)/scaley,(x-x0)/scalex)
	    end if

	    r = (x-x0)/(scalex*cos(theta))

	    if(proj.eq.NPOLE) then
		lon = -theta/rads - 90.0d0
		latd = 90.0d0 - r
	    else
		lon = theta/rads - 90.0d0
		latd = -90.0d0 + r
	    end if

	    if(lon.gt.180.0d0) lon = lon - 360.0d0
	    if(lon.lt.-180.0d0) lon = lon + 360.0d0

	else if(proj.eq.GLOBE) then

c	    Convert screen coords to 3-D

	    xxx(2) = (x-x0)/(scalex*winr)
	    xxx(3) = (y-y0)/(scaley*winr)
	    matharg = 1.0 - xxx(2)**2 - xxx(3)**2
	    if(matharg.ge.0.0) then
		xxx(1) = sqrt(matharg)
	    else
		latd = 999.0d0
		lon = 999.0d0
		return
	    end if

c	    Unrotate x,y,z

	    call yrot(xxx,lat0,xx)	! lat rotation
	    call zrot(xx,-lon0,xxx)	! lon rotation
	    call xrot(xxx,-tilt,xx)	! tilt rotation

c	    Convert x,y,z coords to lat,lon

	    if(abs(xx(3)).le.1.0) then
		theta = asin(xx(3))
	    else
		latd = 999.0d0
		lon = 999.0d0
		return
	    end if
	    lambda = atan2(xx(2),xx(1))

	    latc = theta/rads
	    latd = tolatd(latc)
	    lon = lambda/rads

	end if

	end
