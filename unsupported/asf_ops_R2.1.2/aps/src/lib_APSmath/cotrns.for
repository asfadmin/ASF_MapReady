C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.

      SUBROUTINE COTRNS(EPOCA,IA,XA,EPOCB,IB,XB,ITEST)
***************************************************************************
*
*  NAME: COTRNS
*
*  MODULE TYPE: SUBROUTINE          LANGUAGE: F77
*
*  $LOGFILE$
*
*  PURPOSE - SUB COTRNS TRANSFORMS THE COORDINATES OF STATE VECTORS AND 
*            CLASSICAL ELEMENTS FROM THE IA-COORDINATE SYSTEM TO THE IB-
*            COORDINATE SYSTEM AS DEFINED BELOW.
*            NOTE: XA MAY BE ORBITAL ELEMENTS, BUT XB ARE VECTORS.
*
*  INPUT PARAMETERS:
*
*     NAME    TYPE    DESCRIPTION
*    
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
*     ITEST : INTG    0: NO TEST DATA, 1: OUTPUT TEST DATA
*     XA    : REAL*8  INPUT ORBIT ELEMENTS/VECTORS IN IA COORD.
*                     AS VECTORS IN UNITS OF KM, SEC
*                     AS ELTS, XA(6) IS MEAN ANOMALY, KM,DEG,SEC
*
*  OUTPUT PARAMETERS:
*
*     XB    : REAL*8  OUTPUT ORBIT VECTORS IN IB COORD.
*
*  $DATE$      $REVISION$      $AUTHOR%
*
****************************************************************************
C
      character*100 SccsFileID
     -/'@(#)cotrns.for	5.1 98/01/08 APS/ASF\0'/

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)   
      DIMENSION XA(6),XB(6),DUM(6)
C
C I.  CHANGE CARTESIAN REFERENCE FRAME
C
      MJ2000=15  ! MEAN EQUATOR EQUINOX OF J2000.
      CALL ETRANS(DUM,MJ2000,XA,IA,EPOCA)
      CALL ETRANS(XB,IB,DUM,MJ2000,EPOCB)
      IF(ITEST.GT.0) WRITE(*,2100) XA,IA,DUM,XB,IB
 2100 FORMAT(' CART VEC: ',3F18.8/
     +       '   (KM,S)  ',3F18.8,' IA: ',I3/
     +       ' CART VEC: ',3F18.8/
     +       '   (KM,S)  ',3F18.8,' J2000'/
     +       ' CART XB : ',3F18.8/
     +       '   (KM,S)  ',3F18.8,' IB: ',I3/
     +       '********** COTRNS TEST OUTPUT END'//)
C
      RETURN
      END
