C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.

********************************************************************
*  Name:	SSCVCB
*  Module Type: SUBROUTINE 	Language: FORTRAN
*  $Logfile:   ACS003:[BLD.MPS.LIB.SRC]SSCVCB.FOV  $
*  Purpose:	For a Circle, Build a search area in min/max lat/lon.
*  Functions called:
*  VECTOR LIBRARY:  SPHERE
*  Input Parameters:
*  Name         Type    Definition
*  CLL(2)	REAL*8	CENTER OF CIRCLE IN LAT/LON.
*  RDEG		REAL*8	RADIUS OF THE SEARCH IN DEGREES
*  Output Parameters:
*  Name         Type    Definition
*  SLAT		REAL*8	SOUTHERN BOUNDARY OF SEARCH AREA IN DEG LAT.
*  NLAT		REAL*8	NORTHERN BOUNDARY OF SEARCH AREA IN DEG LAT.
*  ELON		REAL*8	EASTERN BOUNDARY OF SEARCH AREA IN DEG LON.
*  WLON		REAL*8	WESTERN BOUNDARY OF SEARCH AREA IN DEG LON.
*  INORML	INTEGER	= 0 	IF A NORMAL QUERY.  FOR EXAMPLE, IF THE CIRCLE
*				IS SMALL, NEAR THE EQUATOR, AND NOT STRADDLING
*				THE DATE LINE.  YOU THEN WANT 
*				POINTS E OF THE WEST LONG and W OF EAST LONG.
*			= 1	ABNORMAL QUERY:  IF CIRCLE INCLUDES A POLE AND
*				ALL LONGITUDES ARE WANTED.  OR THE CIRCLE 
*				STRADDLES THE DATE LINE AND YOU THEN WANT 
*				POINTS E OF THE WEST LONG or W OF EAST LONG.
*			REMEMBER LONGITUDES IN THE DATABASE ARE STORED AS 
*			> -180.0 AND <= 180.0.
*  Variables:
*  Locals :
*  Externals :
*  Modification History:                                            
*  Date			Revision	Author
*  $Date$ $Revision$ $Author$
*                                                                   
*********************************************************************/
      SUBROUTINE SSCVCB (CLL,RDEG,NLAT,SLAT,ELON,WLON,INORML)
      character*100 SccsFileID
     -/'@(#)sscvcb.for	5.1 98/01/08 APS/ASF\0'/

      IMPLICIT NONE
      REAL*8 CLL(2),RDEG,NLAT,SLAT,ELON,WLON
      INTEGER INORML
      REAL*8 POLE(3), CXYZ(3), ADEG, a, c, RRAD, DN, DS
      INCLUDE 'APS_HOME:include/local/mps_const_math.inc'
CCC      WRITE(*,*)'SSCVCB:  CLL = ',CLL
CCC      WRITE(*,*)'SSCVCB:  RDEG = ',RDEG
      RRAD = RDEG / DG2RAD
CCC      WRITE(*,*)'SSCVCB:  RRAD = ',RRAD
C---	GET NORTH AND SOUTH LATS - EASY.
      NLAT = CLL(1) + RDEG
      SLAT = CLL(1) - RDEG
C---	SET UP FOR POLE INSIDE THE SEARCH CIRCLE.  
      INORML = 1
      WLON = -181.0D0
      ELON =  181.0D0
      IF(NLAT .GE. 90.0D0 ) GO TO 9999
      IF(SLAT .LE.-90.0D0 ) GO TO 9999
C---	POLE WAS NOT COVERED.  SET UP FOR WLON AND ELON.  
C---	MUST SOLVE A RIGHT SPHERICAL TRIANGLE.  
C---	b = pole to a tangent on circle.  
C---	c = pole to center
C---	a = center to tangent ( = radius of circle)
C---	THE RIGHT ANGLE IS AT THE INTERSECTION OF a AND b, WHILE c IS THE 
C---	HYPOTENUSE.  GET ANGLE A IN DEGREES, FROM KNOWN a AND c.  
c---	A IS WHAT YOU ADD AND SUBTRACT TO THE CENTER LONGITUDE TO GET 
C---	THE W AND E LONS.
C---	USE:
C---		sin A = sin a / sin c
C---	
C---	WITH a AND c MEASURED ON THE UNIT SPHERE IN RADIANS.  
      CALL LL2XYZ(CLL,CXYZ)
      POLE(1) = 0.0D0
      POLE(2) = 0.0D0
      POLE(3) = 1.0D0
      CALL GCDIST(DN,CXYZ,POLE)
      POLE(3) = -1.0D0
      CALL GCDIST(DS,CXYZ,POLE)
      c = MIN(DS,DN)
      a = RRAD
      ADEG = ASIND(SIN(a)/SIN(c))
      ELON = CLL(2) + ADEG
      IF(ELON.GT.180.0D0) ELON = ELON - 360.0D0
      WLON = CLL(2) - ADEG
      IF(WLON.LE.-180.0D0) WLON = WLON + 360.0D0
      IF(ELON.LE.WLON) GO TO 9999
C---	NORMAL CASE.  
      INORML = 0
 9999 CONTINUE
      RETURN
      END
