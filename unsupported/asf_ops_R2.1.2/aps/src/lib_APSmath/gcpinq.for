C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.

*************************************************************************
*  Name:	GCPINQ
*  Module Type: SUBROUTINE	Language: FORTRAN
*  $Logfile:   ACS003:[BLD.MPS.LIB.SRC]GCPINQ.FOV  $
*  Purpose:	DETERMINE IF A POINT P IS IN A QUADRILATERAL Q.  THE 
*		POINTS OF Q ARE IN CLOCKWISE ORDER.  
*  Subroutines called:
*  VECTOR LIBRARY: UCROSSM, DOT
*  Input Parameters:
*  POINTS ON A UNIT SPHERE, (X,Y,Z)
*  Name         Type    Definition
*  Q1(3)	REAL*8	1ST POINT OF QUADRILATERAL
*  Q2(3)	REAL*8	2ND POINT OF QUADRILATERAL
*  Q3(3)	REAL*8	3RD POINT OF QUADRILATERAL
*  Q4(3)	REAL*8	4TH POINT OF QUADRILATERAL
*  P		REAL*8	TEST POINT
*  Output Parameters:
*  Name         Type    Definition
*  IFLAG	INTEGER	
*			=  -1 IF  P IS OUTSIDE THE QUADRILATERAL
*			=   0 IF  P IS ON THE BOUNDARY OF THE QUADRILATERAL
*			=   1 IF  P IS INSIDE THE QUADRILATERAL
*  Variables:
*  Locals :
*  Externals :
*  Modification History:                                            
*  Date			Revision	Author
*  $Date$ $Revision$ $Author$
*                                                                   
*********************************************************************/
      SUBROUTINE GCPINQ (IFLAG,Q1,Q2,Q3,Q4,P)
      character*100 SccsFileID
     -/'@(#)gcpinq.for	5.1 98/01/08 APS/ASF\0'/

      IMPLICIT NONE
      INTEGER IFLAG, IFLAGQ, IBOUND
      REAL*8 Q1(3), Q2(3), Q3(3), Q4(3), P(3)
      REAL*8 QP(3), C
C---	VECTOR LIBRARY FUNCTION
      REAL*8 DOT
      IBOUND = 0
      IFLAG = -1
C---	WE WILL DO TESTS FOR THE FOUR PLANES DEFINED BY THE FOUR SIDES.  
      CALL GCLOSE(IFLAGQ,Q1,Q2)
      IF(IFLAGQ.EQ.-1) CALL GCBOMB
      IF(IFLAGQ.EQ.1)  GO TO 1000
      CALL UCROSS(QP,Q1,Q2)
C---	QP IS PERPENDICULAR TO Q1 Q2 GT CIRCLE; POINTING IN THE OUTSIDE 
C---	DIRECTION.  
      C = DOT(QP,P,3)
C---	NOW C IS THE COSINE OF THE ANGLE BETWEEN QP AND P.  
      IF(C.GT. 9.4D-12) GO TO 9999
      IF(ABS(C).LT. 9.4D-12) IBOUND = 1
 1000 CONTINUE
      CALL GCLOSE(IFLAGQ,Q2,Q3)
      IF(IFLAGQ.EQ.-1) CALL GCBOMB
      IF(IFLAGQ.EQ.1)  GO TO 2000
      CALL UCROSS(QP,Q2,Q3)
      C = DOT(QP,P,3)
      IF(C.GT. 9.4D-12) GO TO 9999
      IF(ABS(C).LT. 9.4D-12) IBOUND = 1
 2000 CONTINUE
      CALL GCLOSE(IFLAGQ,Q3,Q4)
      IF(IFLAGQ.EQ.-1) CALL GCBOMB
      IF(IFLAGQ.EQ.1)  GO TO 3000
      CALL UCROSS(QP,Q3,Q4)
      C = DOT(QP,P,3)
      IF(C.GT. 9.4D-12) GO TO 9999
      IF(ABS(C).LT. 9.4D-12) IBOUND = 1
 3000 CONTINUE
      CALL GCLOSE(IFLAGQ,Q4,Q1)
      IF(IFLAGQ.EQ.-1) CALL GCBOMB
      IF(IFLAGQ.EQ.1)  GO TO 1000
      CALL UCROSS(QP,Q4,Q1)
      C = DOT(QP,P,3)
C---	NOW C IS THE COSINE OF THE ANGLE BETWEEN QP AND P.  
      IF(C.GT. 9.4D-12) GO TO 9999
      IF(ABS(C).LT. 9.4D-12) IBOUND = 1
      IFLAG = 1 - IBOUND
 9999 CONTINUE
      RETURN
      END
