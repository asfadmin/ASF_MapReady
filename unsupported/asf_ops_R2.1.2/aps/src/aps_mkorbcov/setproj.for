
*.
*   SetProj - set plotting variables
*
*   Args
*	iproj		I	input	projection number
*	arg()		double	input	see below
*
*	Projection	arg
*	----------	----
*	Cylindrical,	lat1,lon1,lat2,lon2
*	  Mercator,
*	  Miller
*	Lambert		lat1,lon1,lat2,lon2,parallel1,parallel2
*	North Pole,	minlat
*	  South Pole
*	Globe		lat0,lon0,tilt,zoom
*
*   Common blocks
*	constants.inc
*	display.inceos$map
*	map.inc
*
*   Routines
*	ToMerc (double)
*	ToMill (double)
*
*	03/06/89 Original
*
*	04/20/89 18:14
*..
	subroutine SetProj(iproj,arg)
	integer iproj
	double precision arg(*)

	  character*100 SccsFileID
     -/'@(#)setproj.for	5.1 98/01/08 APS/ASF\0'/
** Includes
	include 'eosinclude:constants.inc'
	include 'eosinclude:display.inc'
	include 'eosinclude:map.inc'

** Variables
	double precision parallel1,parallel2,n1,n2,e
	double precision rho1,rho2,dlon
	integer iarg

** Functions
	double precision ToMerc,ToMill

** Data
	data e /0.00672d0/


	proj = iproj

	RectProj = (proj.eq.CYL .or. proj.eq.MERC .or. proj.eq.MILL)

* Save args in case display window changes
* Then we can call SetProj again:
*
*	call SetProj(proj,ProjArg)
*
	do iarg=1,NUMPROJARG
	    ProjArg(iarg) = arg(iarg)
	end do

*   MapCut is true if the dateline will show on a rectangular map
*   MapWrap is true if the map is nearly 360 degrees wide
	MapCut = .false.
	MapWrap = .false.

* Polar

	if(proj.eq.NPOLE .or. proj.eq.SPOLE) then
	    lolat = abs(arg(1))

	    x0 = dble(swidex/2) + dble(screenx(1))
	    y0 = dble(swidey/2) + dble(screeny(1))

	    winr = 0.45d0*dble(min(abs(swidex),abs(swidey)))

	    scalex = sign(winr/(90.0d0-lolat),dble(swidex))
	    scaley = sign(winr/(90.0d0-lolat),dble(swidey))

	    if(proj.eq.SPOLE) then
		scalex = -scalex
		scaley = -scaley
	    end if

* Globe

	else if(proj.eq.GLOBE) then

	    x0 = dble(swidex/2) + dble(screenx(1))
	    y0 = dble(swidey/2) + dble(screeny(1))

	    winr = 0.45d0*dble(min(abs(swidex),abs(swidey)))

	    scalex = sign(1.0d0,dble(swidex))
	    scaley = -sign(1.0d0,dble(swidey))

	    lat0 = arg(1) * rads
	    lon0 = arg(2) * rads
	    tilt = arg(3) * rads

* Rectangular or Lambert

	else if(RectProj .or. proj.eq.LAMB) then

	    winx(1) = arg(2)
	    winy(1) = arg(1)
	    winx(2) = arg(4)
	    winy(2) = arg(3)

	    if(winx(1).gt.winx(2)) then
		MapCut = .true.
	    else
		MapCut = .false.
	    end if

	    if(proj.eq.MERC) then
		winy(1) = ToMerc(winy(1))
		winy(2) = ToMerc(winy(2))
	    else if(proj.eq.MILL) then
		winy(1) = ToMill(winy(1))
		winy(2) = ToMill(winy(2))
	    end if
	end if

	if(RectProj) then

	    widex = winx(2) - winx(1)
	    widey = winy(2) - winy(1)

	    if(MapCut) widex = widex + 360.0d0

	    if(abs(WideX-360.0d0).le.0.01d0) MapWrap = .true.

	    scalex = swidex / widex
	    scaley = swidey / widey

	    x0 = dble(screenx(1)) - scalex * winx(1)
	    y0 = dble(screeny(1)) - scaley * winy(1)

	else if(proj.eq.LAMB) then

	    parallel1 = arg(5) * rads
	    parallel2 = arg(6) * rads

	    n1 = 1.0/sqrt(1.0 - (e*sin(parallel1))**2)
	    n2 = 1.0/sqrt(1.0 - (e*sin(parallel2))**2)

	    Sinparallel0 = (log(n1*cos(parallel1)) - log(n2*cos(parallel2)))
     .		/ (log(tan(0.5*(halfpi-parallel1)))
     .			- log(tan(0.5*(halfpi-parallel2))))
	    cLambert = n2 * cos(parallel2)
     .		/ (SinParallel0*(tan(0.5*(halfpi-parallel2)))**SinParallel0)

	    rho0 = cLambert * tan(0.5*(halfpi-asin(SinParallel0)))

	    rho1 = cLambert * tan(0.5*(halfpi-winy(1)*rads))
	    rho2 = cLambert * tan(0.5*(halfpi-winy(2)*rads))
	    dlon = abs(winx(2) - winx(1)) * rads / 2.0

	    widex = 2.0 * rho2 * sin(dlon)
	    widey = rho2 - rho1 * cos(dlon)

	    scalex = swidex / widex
	    scaley = swidey / widey

	    lon0 = winx(1) * rads + dlon
	    x0 = dble(swidex) / 2.0d0
	    y0 = rho1 * cos(dlon)

	end if

	if(proj.eq.GLOBE) then
	    zoom = arg(4)
	else
	    zoom = 1.0
	end if

	scalex = scalex * zoom
	scaley = scaley * zoom

	if(.not.RectProj) then
	    scalex = scalex * devscalex
	    scaley = scaley * devscaley
	end if

	if(MapCut) xWrap = scalex * 360.0d0

	end
