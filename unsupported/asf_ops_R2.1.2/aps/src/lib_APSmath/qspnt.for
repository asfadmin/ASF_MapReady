C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.

*************************************************************************
*  Name:	QSPNT
*  Module Type: SUBROUTINE	Language: FORTRAN
*  $Logfile:   ACS003:[BLD.MPS.LIB.SRC]QSPNT.FOV  $
*  Purpose:	DETERMINE THE LATITUDE OF THE CLOSEST POINT IN A QUADRILATERAL 
*		TO THE SOUTH POLE.  
*  Subroutines called:
*  GCSEGP
*  MUST LINK VECTOR LIBRARY
*  Input Parameters:
*  3 POINTS ON A UNIT SPHERE, (X,Y,Z)
*  Name         Type    Definition
*  Q1(3)	REAL*8	THE FOUR POINTS IN THE QUAD ARE INPUT IN CLOCKWISE 
*  Q2(3)	REAL*8	ORDER.  
*  Q3(3)	REAL*8
*  Q4(3)	REAL*8
*  Output Parameters:
*  Name         Type    Definition
*  SLAT		REAL*8	LATITUDE OF SOUTHERNMOST POINT IN QUAD
*  Variables:
*  Locals :
*  Externals :
*  Modification History:                                            
*  Date			Revision	Author
*  $Date$ $Revision$ $Author$
*                                                                   
*********************************************************************/
      SUBROUTINE QSPNT (SLAT,Q1,Q2,Q3,Q4)
      character*100 SccsFileID
     -/'@(#)qspnt.for	5.1 98/01/08 APS/ASF\0'/

      IMPLICIT NONE
      REAL*8 Q1(3), Q2(3), Q3(3), Q4(3), SLAT
      REAL*8 P(3), PC(3), XLL(2), D
      INTEGER IFLAG
      SLAT = -90.0D0
C---	CHECK FOR S POLE INSIDE THE QUAD.
      P(1) = 0
      P(2) = 0
      P(3) = -1
      CALL GCPINQ(IFLAG,Q1,Q2,Q3,Q4,P)
      IF(IFLAG .GE. 0) GO TO 9999
C---	S POLE IS OUTSIDE THE QUAD.  THE CLOSEST POINT TO POLE IS 
C---	IS ON ONE OF THE FOUR SIDES.  
      SLAT = 100.0D0
C---	GET LAT OF CLOSEST POINT ON EACH SIDE IN QUAD.
      CALL GCSEGP(D,PC,Q1,Q2,P)
      CALL XYZ2LL(PC,XLL)
      IF(XLL(1) .LT. SLAT) SLAT = XLL(1)
      CALL GCSEGP(D,PC,Q2,Q3,P)
      CALL XYZ2LL(PC,XLL)
      IF(XLL(1) .LT. SLAT) SLAT = XLL(1)
      CALL GCSEGP(D,PC,Q3,Q4,P)
      CALL XYZ2LL(PC,XLL)
      IF(XLL(1) .LT. SLAT) SLAT = XLL(1)
      CALL GCSEGP(D,PC,Q4,Q1,P)
      CALL XYZ2LL(PC,XLL)
      IF(XLL(1) .LT. SLAT) SLAT = XLL(1)
 9999 CONTINUE
      RETURN
      END
