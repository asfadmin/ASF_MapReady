C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:  covformat.for
C--
C--  Description:
C--
C--  Notes:
C--
C-- ==========================================================================
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c  Name: CovFormat
c  Module Type: Subroutine  Language: FORTRAN
c  Purpose: 
c  Takes previously calculated state vector info and formats it for output
c   the input also contains sensor coverage on the earth 
c  Input Parameters:
c  Name     Type        Description
c  dbproc   *DBPROC     address of structure with sybase info in it.
c  IQUICK   INTEGER     = 0:  ORDINARY COVERAGE RUN
C               = 1:  QUICK COVERAGE RUN:  NO DB CHANGES, 
C                     NO METADATA; JUST THE COVERAGE FILE.
c  IFILE    INTEGER     = 0:  ORDINARY COVERAGE RUN
C               = 1:  FILE COVERAGE RUN:  WRITE FILE AND MAKE DB CHANGES, 
c  ACT      CHAR*(*)    'IN'    ENTER MASK RECORD
C               'OUT'   EXIT MASK RECORD
C               '-' NEITHER ENTER OR EXIT.
c  x        real*8(12)  state vector
c  xnlat,xnlon  Real*8      lat/lons of sensor swath
c  xflat,xflon
c  se       real*8(3)   Cartesian coords of the center point on the 
c               earth of the satellite swath
C  marker   INT     Number corresponding to this coverage run.
C  nodeflag CH*1        D = descending node
c               N = ascending node
C  amask    CH*1        ASF = inside the asf mask
C  sat      CH*2        Satellite:  E1, J1, RS etc.
C  sensor   CH*3        SAR or OPS etc.
c  Output Parameters:
c  Name     Type        Description
C  NRECSC   INT     NUMBER OF cvrg RECS APPENDED.
c  Modification History:
c  Author   Revision    Date
c  HERB     1.0     17 Apr 1989 14:52:00 
c  CRAIG    1.1     02 May 1989 13:24:42
c  $Author$ $Revision$ $Date$
c
c ****************************************************************************
      SUBROUTINE CovFormat(dbproc, IQUICK, IFILE, ACT, x, 
     ?                     xnlat, xnlon, xflat, xflon, 
     1                     se, marker, nodeflag, amask, sat, sensor, 
     ?                     NRECSC )

      character*100 SccsFileID
     -/'@(#)covformat.for	5.1 98/01/08 APS/ASF\0'/
c
      IMPLICIT NONE
C==========      IMPLICIT DOUBLE PRECISION (a,b,d-h,o-z)
c--port--##    DECLARE
      integer*4 dbproc
      external insert_cvrg !$pragma C(insert_cvrg)
      CHARACTER*(*) ACT
      CHARACTER*3 station_id
      REAL*8 x(12), se(3), ss(7), SUBSAT(3), absXsub, abssssub
c
      REAL*4 SUNCOS4
c
      CHARACTER*21 asftime
      CHARACTER svis, nodeflag, ad, XNODE
      CHARACTER*(*) amask, sat, sensor
      INTEGER irev, marker, IQUICK, IFILE, NRECSC 
      INTEGER IER
      DOUBLE PRECISION sslat,sslon,xnlat,xnlon,xflat,xflon,hite,sunang
      DOUBLE PRECISION clatd, qhite, jtime
      SAVE ad
      REAL*8 DOTPROD, re, zscale
      INTEGER I, IVIS, nrecs
      REAL*8 glatd, glond, rr, trgf, GLAT
      COMMON /COM_GDLAT/ glatd, glond, clatd, rr(3), hite, trgf(6)
C---      INCLUDE 'MPS_LIB:MPS_CONST_EARTH.FOR'
      INCLUDE 'APS_HOME:include/local/mps_const_earth.inc'
      INCLUDE 'APS_HOME:include/local/timeconv.inc'
c
      NRECSC = 0
C-    Set station id.  a 3-character database field now:  
      station_id = '---'
      if ( amask .eq. 'A' ) station_id = 'ASF'
      if ( amask .eq. 'M' ) station_id = 'MCM'
c   CHECK Z-COMPONENT OF VELOCITY FOR ASCENDING OR DESCENDING.  
      if (x(6) .ge. 0.0 ) ad = 'A'
      if (x(6) .lt. 0.0 ) ad = 'D'
C---    OR STATIONARY.  
      if (x(6) .EQ. 0.0 ) ad = 'S'
c
      irev = x(8) + 0.5d0
      jtime = x(7)
C---      CALL ET2ASF(jtime,asftime)
      IER = tc_et2asf(%VAL(jtime), asftime)
c  Calculate the ZENITH sun angle AT SUBSAT POINT and SOLAR visability OF
C  SATELLITE.  
c  ss       -   sun vector
c  SUBSAT   -   sub satellite point vector
c
c  Calculate geodetic coords
      SUBSAT(1) = Glat(x, 0)
C---          glatd is sslat in degrees
C---          glond is sslon in degrees
      CALL GLL2XYZ(GLATD,GLOND,SUBSAT)
c  X        -   satellite vector
c  vector from subsat to satellite:     X - subsat
c  vector from subsat to sun:       ss - subsat
c  The cosine of the Sun Angle is given by taking the dot product of the 
c  two vectors and dividing by the product ot the two vector lengths:
c   (X - subsat) <dot> (ss - subsat)  /   (|X-subsat|)*(|ss-subsat|)
c
      CALL Sephem(x(7), ss)
      dotprod = 0.0d0
      absXsub = 0.0d0
      abssssub = 0.0d0
      DO 10, i=1, 3
         dotprod = dotprod + (X(i) - subsat(i)) * (ss(i) - subsat(i)) 
         absXsub = absXsub + (X(i) - subsat(i))**2
         abssssub = abssssub + (ss(i) - subsat(i))**2
 10   CONTINUE
C---    THE COSINE OF THE SUN ANGLE:  REAL*4  the "abs" numbers are the 
c---    SQUARES of the absolute values.  
C---    USING REAL*4 TO LESSEN THE LIKELYHOOD OF GETTING AN INVALID ARGUMENT
C---    TO THE ACOS FUNCTION VIA A VALUE VERY VERY SLIGHTLY ABOVE 1.0 DUE
C---    TO COMPUTATIONAL NOISE.  THE NOISE WILL NEVER BE ENOUGH TO ALTER
C---    THE REAL*4 VALUE TO ABOVE 1.  THE PARENS FORCE THE COMPUTATION 
C---    TO BE DONE IN REAL*8.  
      SUNCOS4 = (dotprod / SQRT(absXsub * abssssub))
      sunang = ACOSD(SUNCOS4)
c
c  Compute Sun Visability, I.E. IF SUN IS VISABLE AT SPACECRAFT.
c  The Earth is an ellipsiod.  Its narrowest cross sectional length is along
c  the Z-axis.  The coordinate system will be scaled along the Z-axis so the
c  Earth will be a sphere in the new coordinate system.  The position vector of
c  the space craft is (x1, x2, x3).
c
      re = ERAD  ! These are physical constants earth radius & ellipticity
C---    zscale = 1/(1-f) where f = (a-b)/a where a and b are the semimajor
c---    and semiminor axes of the earth ref ellipsoid.  1/f = FLAT = 298.257
c---    This works out to zscale = a/b.   Now using FLAT from the 
c---    MPS_CONST_EARTH file:
      zscale = 1.0D0 / ( 1.0D0 - 1.0D0/FLAT )
      x(3) = x(3) * zscale
      SS(3) = SS(3) * zscale
      CALL Vis(x, SS, re, ivis)
      if (ivis .GT. 0) then
         svis = 'S'
      else
         svis = '-'
      endif
      x(3)  =  x(3) / zscale  ! Change vectors back to original values
      SS(3) = SS(3) / zscale
c
c  Calculate geodetic altitude and coords
c
      sslat = Glat(x, 0)
      sslat = glatd           ! glatd is sslat in degrees
      sslon = glond - x(12)   ! x(12) is the Greenwich hour angle
      DO WHILE (sslon .LT. 0.0d0)
         sslon = sslon + 360.0d0
      ENDDO
      DO WHILE (sslon .GT. 180.0d0)
         sslon = sslon - 360.0d0
      ENDDO
c      sslon = ATAN2D (SIND (sslon), COSD (sslon))
      qhite = hite
      IF(qhite .GT. 300.0d0) GO TO 99
      WRITE(*,*)' COVFORMAT:  ERROR:  sat altitude LT 300 km.  '
      WRITE(*,*)'  Check the state vector used in the ephemeris run.'
      CALL GCBOMB
   99 CONTINUE
c
      IF(IFILE .EQ. 1  .OR.  IQUICK .EQ. 1 )  THEN
C         WRITE TO ASCII FILE IF IFILE OR IQUICK FLAGS ARE SET.
c         NODEFLAG SHOULD BE PRINTED AS A BLANK.  
          XNODE = nodeflag

          IF (      XNODE .NE. 'A' 
     ?        .AND. XNODE .NE. 'N' 
     ?        .AND. XNODE .NE. 'D' 
     ?        .AND. XNODE .NE. 'P' )  XNODE = ' '

          WRITE (81,100) asftime(1:4), asftime(6:21), irev, sslat,
     &            sslon, xnlat, xnlon, xflat, xflon, qhite, svis,
     &            sunang , amask, '-   ', '0 ', XNODE, '             '
 100      FORMAT (a4,1x,a16,1x,i5,1x,7(f7.2,1x),a1,1x,f6.2,1x,
     &    a1,1x,a4,1x,a2,1x,a1,a13)
      ENDIF

      IF(IQUICK .EQ. 1) GO TO 9999
C     WRITE TO DATABASE IF IQUICK FLAG IS NOT SET TO 1.  
C
      call insert_cvrg(%VAL(dbproc), marker, sat, sensor, jtime,
     ?   irev, sslat, sslon, xnlat, xnlon, xflat, xflon, qhite, svis,
     ?   sunang, station_id, '-   ', 0, nodeflag, ad, nrecs)

      NRECSC = NRECSC + nrecs

 9999 CONTINUE
      RETURN
      END
      SUBROUTINE Sephem(tj,x)
c **********************************************************************
c  Input: time in julian days
c
c  Output: Solar state vector - r,v format
c **********************************************************************
      IMPLICIT DOUBLE PRECISION (a-h, o-z)
      DIMENSION x(6), es(7), em(7)
c
c========      tjsec = tj * 3600.0  ! Convert hours to seconds
      tjsec = tj * 86400.0  ! Convert days to seconds
      CALL Setsun(tjsec, es)
      isun = 1
      imoon = 0
      isrp = 0
      CALL A_Ephem(isun,imoon,isrp,tjsec,es,em)
      CALL A_Kepler(es(6), es(2), ea, se, ce)
      es(6) = ea
      ge = 3.9860045D5 ! for earth km**2 / sec**3
      CALL Coord(es,ge,x)
      RETURN
      END
********************************************************************
*  Name:    GLL2XYZ
*  Module Type: SUBROUTINE  Language: FORTRAN
*  Purpose: GIVEN GEODETIC LAT/LON, COMPUTE THE XYZ ON THE EARTH.
*  Functions called:
*  VECTOR LIBRARY:  
*  Input Parameters:
*  Name         Type    Definition
*  GLATD    REAL*8  GEODETIC LATITUDE
*  GLOND    REAL*8  GEODETIC LONGITUDE
*  Output Parameters:
*  Name         Type    Definition
*  XYZ(3)   REAL*8  XYZ OF THE POINT ON THE REFERENCE GEODE.
*  Variables:
*  Locals :
*  Externals :
*  Modification History:                                            
*  Date         Revision    Author
*  $Date$ $Revision$ $Author$
*                                                                   
*********************************************************************/
      SUBROUTINE GLL2XYZ(GLATD,GLOND,XYZ)
      IMPLICIT NONE
      REAL*8 GLATD, GLOND, XYZ(3)
      REAL*8 a, e2, X
      INCLUDE 'APS_HOME:include/local/mps_const_earth.inc'
C---    ERAD IS THE EARTH EQUATORIAL RADIUS:  a  in the earth reference geoid
C---    FLAT = 298.257 = a/(a-b)                 in the earth reference geoid
C---    and:    b   = a(FLAT-1)/FLAT
C---    also:   b/a = (FLAT-1)/FLAT or (1 - (1/FLAT) )
C---    also:   e**2 =(c/a)**2 = 1 - (b/a)**2  for an ellipse.
c---                 = .006694385        in the earth reference geoid.
C
C---    FROM FUNDAMENTALS OF ASTRODYNAMICS:  DOVER PUBLICATIONS:
C---    pp 94-98:  sec. 2.8.3 Station Coordinates
C
C---    X = aCosL / [  1 - (eSinL)**2  ]**(1/2)     (2.8-5)
C---    Z = a(1-e**2)SinL / [ 1 - (eSinL)**2 ]**(1/2)   (2.8-5)
c
c   WHERE:  
c       X = distance from earth North-South axis to point on the 
c           surface of the earth.
c       Z = distance from earth equator to point on the surface of 
c           the earth.  
c       a = semimajor axis of ellipse (EARTH equatorial radius)
c       L = Geodetic latitude of the poiint on the earth.
c       e = eccentricity of earth reference ellipsoid
c
c---    IN THIS PROGRAM:
c---    a = ERAD
C---    L = GLATD
C---    e**2 = 1 - ( 1 - 1/FLAT )**2
C---    X = ( XYZ(1)**2 + XYZ(2)**2 )**(1/2)
c---        I.E., X is the hypotenuse of the triangle with legs 
c---        being the x and y components of the XYZ we are trying to 
c---        compute.
C---    Z = XYZ(3)
c---        I.E. Z is the z component of the XYZ we are trying to compute.
CCCCC      WRITE(*,*)'  GLL2XYZ:  GLATD = ', GLATD
CCCCC      WRITE(*,*)'            GLOND = ', GLOND
      a = ERAD
CCCCC      WRITE(*,*)'            a = ', a
CCCCC      b   = a * (FLAT-1.0d0)/FLAT
CCCCC      WRITE(*,*)'            b = ', b
      e2 = 1.0d0 - ( 1.0d0 - 1.0d0/FLAT )**2 
CCCCC      WRITE(*,*)'            e2 = ', e2
C
C---    Z = a(1-e**2)SinL / [ 1 - (eSinL)**2 ]**(1/2)   (2.8-5)
      XYZ(3) = a * ( 1.0d0 - e2) * SIND(GLATD)
      XYZ(3) = XYZ(3) / SQRT( 1.0D0 - e2*SIND(GLATD)**2 )
C---    X = aCosL / [  1 - (eSinL)**2  ]**(1/2)     (2.8-5)
      X = a*COSD(GLATD)
      X = X           / SQRT( 1.0D0 - e2*SIND(GLATD)**2 )
C---    NOW WE CAN COMPUTE THE x AND y COMPONENTS VIA THIS X AND THE GIVEN 
C---    LONGITUDE.  
      XYZ(1) = X * COSD(GLOND)
      XYZ(2) = X * SIND(GLOND)
CCCCC      WRITE(*,*)'            X = ', X
CCCCC      WRITE(*,*)'            XYZ = ', XYZ
      RETURN
      END
