c***********************************************************************
c	Jet Propulsion Laboratory
c	Section:	334, Radar Science and Engineering
c	Project:	RadarSAT
c	Task:		ScanSAR Data Processor
c	Code:           Fortran 77 
c-----------------------------------------------------------------------
c	Module Name:	ssmi_ll.f
c
c		This subroutine converts the Polar Stereographic
c		x,y coordinates to the geodetic latitude and longitude
c		in the polar region.
c
c	Input:
c	        1.x(m) :x coordinate in the Polar Stereographic
c			 projection.
c	        2.y(m) :y coordinate in the Polar Stereographic
c			 projection.
c		3.a(m) :Equatorial radius of the Earth model.
c		4.e2   :square of eccentricity of the Earth model.
c		5.slat(degree) :standard parallel (latitude without 
c			        distortion)
c		6.along0(degree):longitude of the central meridian
c		7.sgn   :positive for North Pole area and negative
c			 for South Pole area.
c
c	Output: 
c	        1.alat(degree) : geodetic latitude.
c	        1.along(degree) : geodetic longitude.
c
c	Function:    
c		The ssmi_ll processing includes
c
c		1). Check the sign to identify North Pole or
c		    South Pole area.
c		2). Compute the polar distance r & angle chi of
c		    the x,y coordinate.
c		3). According the equations given in ref., the
c		    geodetic latitude and longitude are computed.
c
c	Reference:  
c		1.J.P. Snyder," Map Projections Used by the 
c		  U.S. Geological Survey"
c		2.T.Cheng, "Algorithms for Image Projections of 
c		  Radarsat ScanSAR Processor",JPL IOM 3344-93-039
c----------------------------------------------------------------------------
c
c	8/22/94	    A. Chu   Adding geocentric latitude
c	4/27/94	    A. Chu   1.Revised the longitude greater than 360.
c		             2.Revised  handling of lat=90 case.
c	                     3. delete redundant input parameter,sgn.
c	6/18/93     T. Cheng Initial code.
c
c****************************************************************************
	subroutine ssmi_ll(x,y, lat,lon, slat)

	implicit none
        real*8          pi
        parameter       (pi = 3.1415926535897938238d0)

        real*8          re                      !mean equatorial radius in m
        parameter       (re=6378144.d0)         !GEM 6 model

        real*8          ecc_e                   !earth eccentricity
        parameter       (ecc_e=8.1827385e-2)

        real*8          ecc_2                   !earth ecc. squared
        parameter       (ecc_2=ecc_e*ecc_e)

        real*8          along0		  !reference long for polar stereo
	parameter       (along0=-45.0d0)  !ssmi longitude= -45.0
                                                                     
c	Input parameters

	real*8  x,y	
        real*8  slat            !reference lat for polar stereo
        			!ssmi lat= 70.0

c	Output parameters

	real*8	lat		!Geocentric lat.
	real*8  lon		!Geocentric lon.

c	Local parameters

	real*8	alat		!Geodetic lat.
	real*8	along		!Geodetic Longitude
	real*8	xx,xy,
	1	xslat,xlong0,
	1	tc,mc,
	1	r,t,
	1	e,e_2,
	1	chi,
	1	a,e2,
	1	sgn

	character*80 sccsid
	data sccsid /'@(#)ssmi.f	1.4 98/01/09 19:10:15\0'/

	e2 = ecc_2
	a = re
	e = sqrt(e2)
	e_2 = e/2.

c	---------------------------------------------
c	Check Pole area
c	---------------------------------------------
	sgn=1.
	if (slat .lt. 0.) sgn=-1.	!A.Chu 4/27/94
	xlong0 = sgn*along0
	xslat  = slat			!sgn*slat
	xx     = sgn*x
	xy     = sgn*y

c	--------------------------------------------------
c	Compute polar distance r and angle chi
c	--------------------------------------------------
	r = dsqrt(xx**2+xy**2)

	if (r .le. 0.0d0) then
	   alat = sgn*90.
	   along= 0.d0
c	   type *,' ***Longitude set to 0.0 degree ***'
	else

	tc=tand(45.-xslat/2.)/((1-e*dsind(xslat))/(1+e*dsind(xslat)))**(e_2)
	mc = dcosd(xslat)/dsqrt(1.0-e2*(dsind(xslat)**2))	

	t = r*tc/(a*mc)
	chi = pi/2 - 2 * atan(t)

c	--------------------------------------------------
c	Compute geodetic latitude & longitude
c	--------------------------------------------------

	alat = chi + (e2/2.+5.*e2**2/24.+e2**3/12.)*dsin(2*chi)
     *	       +(7*e2**2/48.+29.*e2**3/240.)*dsin(4.*chi)+(7.*e2**3/120.)
     *         *dsin(6*chi)

	alat = sgn*alat*180./pi
	along = sgn*(xlong0+datan2d(xx,-xy))

d	type *,'r tc mc t chi alat'
d	type *,r,tc,mc,t,chi,alat
d	type *,'alat along:',alat,along

	if ( along .le. -180.d0 ) along = along + 360.d0

cc	if ( along .lt. 0.d0 ) along = along + 360.d0
cc	if ( along .gt. 360.d0 )along = along - 360.d0
	endif

	lat = atand((1-e2)*tand(alat))
	lon = along

	return
	end
c***********************************************************************


c***********************************************************************
c	Jet Propulsion Laboratory
c	Section:	334, Radar Science and Engineering
c	Project:	RadarSAT
c	Task:		ScanSAR Data Processor
c	Code:           Fortran 77 
c-----------------------------------------------------------------------
c	Module Name:	ll_ssmi.f
c
c		This subroutine converts the geocentric latitude and 
c		longitude in the polar region to the Polar
c		Stereographic x,y coordinates.
c
c	Input:
c	        1.lat(degree) : geocentric latitude.
c	        2.long(degree) : geocentric longitude.
c		3.a(m) :Equatorial radius of the Earth model.
c		4.e2   :square of eccentricity of the Earth model.
c		5.slat(degree) :standard parallel (latitude without 
c			        distortion)
c		6.along0(degree):longitude of the central meridian
c		7.sgn   :positive for North Pole area and negative
c			 for South Pole area.
c
c	Output: 
c	        1.x(m) :x coordinate in the Polar Stereographic
c			 projection.
c	        2.y(m) :y coordinate in the Polar Stereographic
c			 projection.
c
c	Function:    
c		The ll_ssmi processing includes
c
c		1). Check the sign to identify North Pole or
c		    South Pole area.
c		2). Compute the polar distance r 
c		3). According the equations given in ref., the
c		    (x,y) coordinates are computed.
c
c	Reference:  
c		1.J.P. Snyder," Map Projections Used by the 
c		  U.S. Geological Survey"
c		2.T.Cheng, "Algorithms for Image Projections of 
c		  Radarsat ScanSAR Processor",JPL IOM 3344-93-039
c----------------------------------------------------------------------------
c	11/14/95    T. Truong	Accept input geocentric latitude
c	4/27/94	    A. Chu   1.delete the redundant "sgn" input parameter
c	                     2.revised  handling of lat=90 case.
c	6/18/93     T. Cheng Initial code.
c****************************************************************************
	subroutine ll_ssmi (lat,lon, x,y, slat)

	implicit none
        real*8          pi
        parameter       (pi = 3.1415926535897938238d0)

        real*8          re                      !mean equatorial radius in m
        parameter       (re=6378144.d0)         !GEM 6 model

        real*8          ecc_e                   !earth eccentricity
        parameter       (ecc_e=8.1827385e-2)

        real*8          ecc_2                   !earth ecc. squared
        parameter       (ecc_2=ecc_e*ecc_e)

        real*8          along0		  !reference long for polar stereo
	parameter       (along0=-45.0d0)  !ssmi longitude= -45.0
                                                                     
c	Input parameters

	real*8	lat		!Geocentric lat.
	real*8  lon		!Geocentric lon.
        real*8  slat 		!reference lat for polar stereo
        			!ssmi lat= 70.0

c	Output parameters

	real*8	x,y

c	Local parameters

	real*8	alat		!Geodetic lat.
	real*8	along		!Geodetic Longitude
	real*8	xlat,xlong,
	1	xslat,xlong0,
	1	tc,mc,
	1	r,t,
	1	a,e2,
	1	e,e_2,
	1	sgn

	along = lon
	alat = atand(tand(lat) / (1-ecc_2))

	if (alat .eq. 90.d0) then
           x=0.d0
           y=0.d0
           return
	endif

	a = re
	e2 = ecc_2
	e = sqrt(e2)
	e_2 = e/2.

c	---------------------------------------------------------------
c	Identify the North Pole or South Pole area
c	---------------------------------------------------------------
	sgn=1.0
	if (slat .lt. 0) sgn=-1.	!A.Chu 4/27/94
	xlong0 = sgn*along0
	xslat  = slat			!sgn*slat
	xlat   = sgn*alat
	xlong  = sgn*along

c	----------------------------------------------------------------
c	Compute the Polar Stereographic (x,y) coordinates 
c	----------------------------------------------------------------

	t = tand(45.-xlat/2.)/((1-e*sind(xlat))/(1+e*sind(xlat)))**(e_2)
	tc= tand(45-xslat/2.)/((1-e*sind(xslat))/(1+e*sind(xslat)))**(e_2)
	mc= cosd(xslat)/sqrt(1.0-e2*(sind(xslat)**2))	

	r = a*mc*t/tc

	x = sgn * r * sind(xlong-xlong0)
	y = -sgn * r * cosd(xlong-xlong0)

	return
	end
