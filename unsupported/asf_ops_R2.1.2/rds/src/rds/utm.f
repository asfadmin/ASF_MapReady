c***********************************************************************
c	Jet Propulsion Laboratory
c	Section:	334, Radar Science and Engineering
c	Preoject:	RadarSat
c	Task:		ScanSAR
c	Code:		Fortran 77
c-----------------------------------------------------------------------
c	Module Name: ll_utm.f
c
c	Abstract:
c             convert lat/lon to universal transverse mercator(utm)
c             coordinates (x:easting,y: northing)
c
c 	User-defined Input parameters:
c
c         1).lat0  - geocentric latitude coordinates
c         2).lon0  - geocentric longitude coordinates
c
c 	Output parameters:
c
c         1).x - array of easting coordinates
c         2).y - array of northing coordinates
c
c         long0- central meridian
c           dtr- degree to radian
c          xadj- easting adjustment
c          yadj- northing adjustment
c
c Reference: Synder, J. P. 1983, Map projection used by the US
c            geological survey(bulletin 1532)
c
c------------------------------------------------------------------
c	11/15/95 T. Truong	Change inputs to geocentric
c	4/10/94  A.Chu		Adopted from SIRC
c******************************************************************
        subroutine ll_utm(lat0, lon0, x, y, yadj, zone)

        implicit none
        real*8          re                      !mean equatorial radius in m
        parameter       (re=6378144.d0)         !GEM 6 model

        real*8          ecc_e                   !earth eccentricity
        parameter       (ecc_e=8.1827385e-2)

        real*8          ecc_2                   !earth ecc. squared
        parameter       (ecc_2=ecc_e*ecc_e)

        real*8          k0_utm                  !distance scale factor for UTM
        parameter       (k0_utm=0.9996d0)
	
c	-------------
c	PASSING IN PARAMETERS
c	-------------
        real*8    lat0		!Geocentric Latitude in degree
        real*8    lon0		!Geocentric Longitude in degree
	integer*4 zone		!UTM zone number

c	-------------
c	PASSING OUT PARAMETERS
c	-------------
        real*8 x, y		!UTM grid (east,north)=(x,y)

c	-------------
c	LOCAL VARIABLES
c	-------------
        real*8    a		!Earth equatorial radius
        real*8    esq		!Earth eccentricity squared
        real*8    k0		!Center meridian scale factor
        real*8   epsq, lat, long, long0, a1, a2, a3, rn
        real*8   t, b1, b2, rm, rm0, dtr, eoffset, noffset
        real*8   tanlat, c
        real*8   yadj, xadj
        real*8   e4, e6, c1, c2, c3, c4
        integer*4  n,  i
        real*8    tlat		!Geodetic Latitude in degree
        real*8    tlon		!Geodetic Longitude in degree

	character*80 sccsid
	data sccsid /'@(#)utm.f	1.5 97/06/16 17:12:38\0'/

c	-------------
c       READ from KEY_CONST.INC
c	-------------
	a = re		!from key_const.inc
	esq = ecc_2
	k0 = k0_utm

        tlon = lon0
        tlat = atand(tand(lat0) / (1-ecc_2))
	
c       -------------
c       Calculate the zone no. and central meridian (long0)
c       -------------
c       zone = (180 + tlon)/6+1

c	-------------
c       Calculate the central meridian (long0)
c	-------------
        dtr = 4.d0 * datan(1.d0)/180.d0	!Degree to radian
        epsq = esq/(1.d0 - esq)
        long0 = float(zone-1)*6.d0 + 3.d0 - 180.d0

        xadj = 5.d5
cc	-------------
c	Need info to determine hemisphere ????
cc	-------------
cTNT 	if (tlat .ge. 0.d0) then    
cTNT       yadj = 0.d0			
cTNT 	else	
cTNT       yadj = 1.d7		!Refernce pt: Southern Hemisphere
cTNT	endif

c	-------------
c       Calculate the UTM (x,y)
c	-------------
          lat = tlat * dtr
          long = tlon * dtr
          rn = a/dsqrt(1.d0 - esq*(dsin(lat))**2)
          tanlat = dtan(lat)
          t = dtan(lat)**2
          c = epsq * (dcos(lat))**2
          a1 = dcos(lat) * (tlon - long0) * dtr
 
          a2 = (1.d0 - t + c) * a1**3 / 6.d0
          a3 = (5.d0 - 18.d0*t + t**2 + 72.d0*c - 58.d0*epsq)
     *         * a1**5 / 120.d0

          x = k0 * rn * (a1 + a2 + a3) + xadj 

          e4 = esq * esq
          e6 = e4 * esq
          c1 = 1.d0 - esq/4.d0 - 3.d0*e4/64.d0 - 5.d0*e6/256.d0
          c2 = 3.d0*esq/8.d0 + 3.d0*e4/32.d0 + 45.d0*e6/1024.d0
          c3 = 15.d0*e4/256.d0 + 45.d0*e6/1024.d0
          c4 = 35.d0*e6/3072.d0
          rm = a*(c1*lat - c2*dsin(2.d0*lat) + c3*dsin(4.d0*lat)
     *                   - c4*dsin(6.d0*lat))
          rm0 = 0.0
          b1 = (a1**2)/2.d0
     *         + (5.d0 - t + 9.d0*c + 4.d0 * c**2) * a1**4 / 24.d0
          b2 = (61.d0 - 58.d0*t + t**2 + 600.d0*c
     *                + 330.d0*epsq) * a1**6 / 720.d0

          y = k0 * (rm - rm0 + rn*tanlat*(b1 + b2)) + yadj 

      return
      end
c************************* end of ll_utm.f **************************


c***********************************************************************
c	Jet Propulsion Laboratory
c	Section:	334, Radar Science and Engineering
c	Preoject:	RadarSat
c	Task:		ScanSAR
c	Code:		Fortran 77, Alliant Multiprocessors. 
c-----------------------------------------------------------------------
c	Module Name: utm_ll.f
c
c	Abstract:
c	
c        Convert the utm coordinates (y:northing & x:easting) of
c        a target to lat/lon.
c        
c	 Note:
c	 Adjust y is alway 1.d7 also on the northern hemisphere.
c	 Therefore the northing is 1.d7 larger than the true northing
c	 for latitudes larger than 0. 
c	 The advantage of this approach is that inversion is possible
c	 without additional information on whether the location in on
c	 the southern or northern henisphere.	
c
c 	Input parameters:
c
c        1).x     : array of easting coordinates (in meters)
c        2).y     : array of northing coordinates (in meters)
c
c 	Output parameters:
c
c        1).lat : geocentric latitude coordinates (in degrees)
c        2).lon : geocentric longitude coordinates (in degrees)
c
c Reference: Synder, J. P. 1983, Map projection used by the US
c            geological survey(bulletin 1532)
c
c----------------------------------------------------------------------
c	11/15/95 T. Truong 	Pass zone as input parameter
c	8/22/94  A.Chu		Adding geocentric latitude
c	4/10/93  A.Chu		Adopted from SIRC
c***********************************************************************
	subroutine utm_ll(x, y, lat, lon, yadj, zone)

        implicit none

        real*8          re                      !mean equatorial radius in m
        parameter       (re=6378144.d0)         !GEM 6 model

        real*8          ecc_e                   !earth eccentricity
        parameter       (ecc_e=8.1827385e-2)

        real*8          ecc_2                   !earth ecc. squared
        parameter       (ecc_2=ecc_e*ecc_e)

        real*8          k0_utm                  !distance scale factor for UTM
        parameter       (k0_utm=0.9996d0)
	
c	----------------------
c	PASSING IN PARAMETERS
c	----------------------
        real*8	  x, y		!UTM grid (east,north)=(x,y)
	integer*4 zone		!UTM zone

c	----------------------
c	PASSING OUT PARAMETERS
c	----------------------
        real*8    lat          !Geocentric Latitude in degree
        real*8    lon          !Longitude in degree

c	---------------
c	LOCAL VARIABLES
c	---------------

        real*8    a             !Earth equatorial radius
        real*8    esq           !Earth eccentricity squared
        real*8    k0            !Center meridian scale factor
        real*8    lat_d         !Geodetic Latitude in degree
	integer*4  i
        real*8     u1, u2, u3, lat1, esqsin2, lat1d, long0, dtr
        real*8     rm, e1, u, epsq, t1, c1, rn1, r1, rm0
        real*8     tanlat1, d
        real*8     xadj, yadj	!Northing origin:(5.d5,0.d0)
	                        !Southing orogin:(5.d5,1.d7)

c	-------------
c	READ from KEY_CONST.INC
c	-------------
	a=re		!read from key_const.inc
	esq=ecc_2
	k0=k0_utm

c	-------------
c	GET CENTRAL LONGITUDE
c	-------------
      	dtr = 4.d0 * datan(1.d0)/180.d0
      	rm0 = 0.d0
	lat_d = atand(tand(lat) / (1-ecc_2))

cTNT 	if (lat_d .ge. 0.d0) then
cTNT       yadj = 0.d0		
cTNT 	else	
cTNT       yadj = 1.d7	!Reference pt: Southern Hemisphere
cTNT	endif

      	xadj = 5.d5

c       -------------
c       Calculate the zone no. and central meridian (long0)
c       -------------
c       zone = (180 + lon)/6+1
        long0 = float(zone-1)*6.d0 + 3.d0 - 180.d0    !zone from proj_const.inc

c	-------------
c	COMPUTE LAT. & LONG. from A UTM POINT(x,y)
c	-------------
          rm = (y - yadj)/k0 + rm0
          e1 = (1.d0 - dsqrt(1.d0 - esq))/(1.d0 + dsqrt(1.d0 - esq))
          u = rm/(a * (1.d0 - esq/4.d0 - (3.d0 * esq * esq/64.d0)
     *                      - (5.d0 * esq * esq * esq/256.d0) ) )
          u1 = (3.d0 * e1 / 2.d0 - (27.d0 * e1**3)/32.d0) * dsin(2.d0*u)
          u2 = (21.d0 * e1**2/16.d0 - (55.d0 * e1**4)/32.d0)
     *             * dsin(4.d0*u)
          u3 = (151.d0 * e1**3 / 96.d0) * dsin(6.d0*u)
          lat1 = u + u1 + u2 + u3
          lat1d = lat1/dtr

          esqsin2 = 1.d0 - esq*(dsin(lat1))**2
          epsq = esq/(1.d0 - esq)
          c1 = epsq * (dcos(lat1))**2
          tanlat1 = dsin(lat1)/dcos(lat1)
          t1 = tanlat1 * tanlat1
          rn1 = a/dsqrt(esqsin2)
          r1 = a*(1.d0 - esq)/dsqrt(esqsin2 * esqsin2 * esqsin2)
          d = (x - xadj)/(rn1 * k0)

          lat_d = lat1d - ((rn1 * tanlat1/r1) * (d*d*0.5d0
     *             - (5.d0 + 3.d0*t1 - 10.d0*c1 + 4.d0*c1*c1
     *                     - 9.d0*epsq) * (d**4)/24.d0
     *             + (61.d0 + 90.d0*t1 + 298.d0*c1 + 45.d0*t1*t1
     *                      - 252.d0*epsq - 3.d0*c1*c1)
     *                 *(d**6)/720.d0) )/dtr

          lat = atand((1-esq)*tand(lat_d))

          lon = long0 + ((1.d0/dcos(lat1)) * (d
     *              - (1.d0 + 2.d0*t1 + c1) * (d**3)/6.d0
     *              + (5.d0 - 2.d0*c1 + 28.d0*t1 - 3.d0*c1*c1
     *                      + 8.d0*epsq + 24.d0*t1*t1)
     *                 *(d**5)/120.d0) )/dtr

      return
      end

c************************* end of utm_ll.f **************************
