C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.

      SUBROUTINE SVTRNS(EPOCA,IA,XA,EPOCB,IB,XB,NEWREV)
      character*100 SccsFileID
     -/'@(#)svtrns.for	5.1 98/01/08 APS/ASF\0'/

***************************************************************************
*  NAME: SVTRNS
*  MODULE TYPE: SUBROUTINE          LANGUAGE: F77
*  $LOGFILE$
*  PURPOSE - SUB SVTRNS TRANSFORMS THE COORDINATES OF STATE VECTORS 
*            ELEMENTS FROM THE IA-COORDINATE SYSTEM TO THE IB-
*            COORDINATE SYSTEM AS DEFINED BELOW.
*  INPUT PARAMETERS:
*     NAME    TYPE    DESCRIPTION
*     EPOCA : REAL*8  >0  JULIAN DATE OF EPOCH IN DAYS FOR XA
*                      0  SET EPOCH = DATE OF CURRENT STATE VECTOR
*     EPOCB : REAL*8  JULIAN DATE OF EPOCH IN DAYS FOR XB (SEE EPOCA)
*     IA    : INTG    INPUT COORDINATE SYSTEM, TWO DIGIT INTEGER.
*     IB    : INTG    OUTPUT COORDINATE SYSTEM, TWO DIGIT INTEGER.
*                     UNITS DIGIT: EARTH COORDINATE SYSTEM
*                     1  EQUATOR  EQUINOX OF 1950.
*                     2  EQUATOR  EQUNIOX OF EPOCH
*                     3  EQUATOR  PRIME MERIDIAN OF EPOCH
*                     4  EQUATOR  PRIME MERIDIAN OF DATE
*                     5  EQUATOR  EQUINOX OF J2000. (J2000. SYSTEM)
*                     6  ECLIPTIC EQUINOX OF 1950.
*                     7  ECLIPTIC EQUINOX OF DATE
*                     8  ECLIPTIC EQUINOX OF J2000.
*                     TENS DIGIT: COORDINATE TYPE (UNITS: KM, DEG)
*                     0  SPHERICAL (RADIUS, LAT, LON)
*                     1  CARTESIAN (X,Y,Z)
*                     HUNDREDTH DIGIT:  TRUE OR MEAN OF DATE
*                     0  MEAN OF DATE
*                     1  TRUE OF DATE
*     XA    : REAL*8  INPUT VECTORS IN IA COORD.
*                     AS VECTORS IN UNITS OF KM, SEC R(X,Y,Z), V(X,Y,Z)
*  OUTPUT PARAMETERS:
*     XB    : REAL*8  OUTPUT ORBIT VECTORS IN IB COORD.
*     NEWREV   INT    OUTPUT FLAG:	
*				IF NEAR THE ASCENDING NODE:
*				= 1	IF IN TRANSFORMING THE S.V.
*					THE Z VALUE WENT FROM < 0 TO >= 0
*				= -1	IF IN TRANSFORMING THE S.V.
*					THE Z VALUE WENT FROM >= 0 TO < 0
*				= 0	UNDER ALL OTHER CONDITIONS
*			- USED TO SEE IF THE REV# SHOULD CHANGE.
*			- THE VALUE CAN BE ADDED TO THE ORIGINAL REV#
*  $DATE$      $REVISION$      $AUTHOR%
****************************************************************************
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)   
      DIMENSION XA(6),XB(6)
      ITEST = 0
      CALL COTRNS(EPOCA,IA,XA,EPOCB,IB,XB,ITEST)
      NEWREV = 0
C---	CHECK FOR ASCENDING:  Z COMPONENT OF VELOCITY.
      IF(XA(6) .LE. 0.0) GO TO 9999
C---	ASCENDING.  
C---	CHECK FOR DIFFERENCES IN SIGN OF Z COMPONENT OF POSITION.
      IF (XA(3)*XB(3) .GT. 0.0 ) GO TO 9999
C---	EITHER DIFFERENT SIGNS OR ONE OR BOTH IS ZERO.  
      IF( XA(3) .GE. 0.0 .AND. XB(3) .LT. 0) NEWREV = -1
      IF( XA(3) .LT. 0.0 .AND. XB(3) .GE. 0) NEWREV =  1
 9999 CONTINUE
      RETURN
      END
