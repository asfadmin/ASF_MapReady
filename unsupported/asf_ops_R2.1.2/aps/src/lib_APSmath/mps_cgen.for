C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.

********************************************************************
*  Name:	MPS_CGEN
*  Module Type: SUBROUTINE 	Language: FORTRAN
*  Purpose:	PROVIDE COUNTER-CLOCKWISE POINTS ON A CIRCLE GIVEN 
*		CENTER, DISTANCE, AND NUMBER OF POINTS.  THE FIRST POINT 
*		IS SOUTH OF THE CENTER.  
*  Functions called:
*  GCATRV
*  MUST LINK VECTOR LIBRARY
*  Input Parameters:
*  Name         Type    Definition
*  LAT		REAL*4	LATITUDE OF CENTER
*  LON		REAL*4	LONGITUDE OF CENTER
*  RKM		REAL*4	RADIUS IN KILOMETERS
*  NPTS		INT*4	REQUESTED NUMBER OF POINTS
*  Output Parameters:
*  Name         Type    Definition
*  LATS(*)	REAL*4	LATITUDES OF POINTS ON THE CIRCLE
*  LONS(*)	REAL*4	LONGITUDES OF POINTS ON THE CIRCLE.
*  Variables:
*  Locals :
*  Externals :
*  Modification History:                                            
*  Date			Revision	Author
*  $Date$ $Revision$ $Author$
*                                                                   
*********************************************************************/
      SUBROUTINE MPS_CGEN (LAT,LON,RKM,NPTS,LATS,LONS)
      character*100 SccsFileID
     -/'@(#)mps_cgen.for	5.1 98/01/08 APS/ASF\0'/

      IMPLICIT NONE
C---	INTERFACE REAL*4
      REAL*4 LAT,LON,RKM,LATS(*),LONS(*)
C---	COMPUTATIONS IN REAL*8
      REAL*8 XLL(2), P1(3), P2(3), Q(3), D, XDEG, ADEG
      INTEGER NPTS, IFLAG, J
      INCLUDE 'APS_HOME:include/local/mps_const_math.inc'
C---	CONVERT FROM KM TO NM, THEN DEGREES, THEN RADIANS ON THE UNIT SPHERE.
      D = (RKM / 1.852D00) / 60.0D0 / DG2RAD
C---	XDEG IS THE DEGREE INCREMENT.  
      XDEG = 360.0D0 / (NPTS - 1)
      XLL(1) = LAT
      XLL(2) = LON
      CALL LL2XYZ(XLL,P2)
C---	SOUTH POLE
      P1(1) = 0.0D0
      P1(2) = 0.0D0
      P1(3) = -1.0D0
      CALL GCLOSE(IFLAG,P1,P2)
      IF(IFLAG .EQ. 0) GO TO 1000
C---	THE CENTER OF THE CIRCLE IS A POLE.  
C---	USE 0,0 LAT/LON.  
      P1(1) = 1.0D0
      P1(2) = 0.0D0
      P1(3) = 0.0D0
 1000 CONTINUE
      DO 2000 J = 1,NPTS
      ADEG = (J-1) * XDEG 
C---	GCATRV STARTS FROM POINT P1, TRAVELS TO POINT P2, THEN TURNS RIGHT AN 
C---	ANGLE ADEG IN DEGREES, THEN TRAVELS A DISTANCE D AND STOPS, 
C---	DEFINING POINT Q.  +90 DEGREES IS A RIGHT TURN, 180 DEGREES IS STRAIGHT
C---	AHEAD.  
      CALL GCATRV(Q,P1,P2,ADEG,D)
      CALL XYZ2LL(Q,XLL)
      LATS(J) = XLL(1)
      LONS(J) = XLL(2)
 2000 CONTINUE
      GO TO 9999
 8888 CONTINUE
      WRITE(*,*)'MPS_CGEN: MUST ASK FOR 5 OR MORE POINTS FOR A CIRCLE.'
      CALL GCBOMB
 9999 CONTINUE
      RETURN
      END
