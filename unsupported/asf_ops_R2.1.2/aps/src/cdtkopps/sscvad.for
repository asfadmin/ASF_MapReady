C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.

*************************************************************************
*  Name:	SSCVAD
*  Module Type: SUBROUTINE	Language: FORTRAN
*  $Logfile:   ACS003:[BLD.MPS.LIB.SRC]SSCVAD.FOV  $
*  Purpose:	GIVEN A QUADRILATERAL SITE AND A SET OF SWATH POINTS, COMPUTE
*		THE SWATH POINTS WHERE COVERAGE BEGINS AND ENDS.  
*  Subroutines called:
*  GCBOMB, GCPERP, GCBTWN
*  MUST LINK VECTOR LIBRARY, DOUBLE PRECISION.
*  Input Parameters:
*  Name         Type    Definition
*  S(3)		REAL*8	A POINT BETWEEN P1 AND P2
*  P1(3)	REAL*8	FIRST POINT
*  P2(3)	REAL*8	SECOND POINT
*  Output Parameters:
*  Name         Type    Definition
*  ASCDSC	CH*1	A     IF ASCENDING AT POINT S WHEN GOING FROM P1 TO P2
*			D     IF DESCENDING AT POINT S WHEN GOING FROM P1 TO P2
*  Variables:
*  Locals :
*  Externals :
*  Modification History:                                            
*  Date			Revision	Author
*  $Date$ $Revision$ $Author$
*                                                                   
*********************************************************************/
      SUBROUTINE SSCVAD(S,P1,P2,ASCDSC)
      character*100 SccsFileID
     -/'@(#)sscvad.for	5.1 98/01/08 APS/ASF\0'/

      IMPLICIT NONE
      REAL*8 S(3), P1(3), P2(3), N(3), Q(3), QM(3)
C-----      REAL*8 SLL(2), P1LL(2), P2LL(2)
      CHARACTER*(*) ASCDSC
      INTEGER J, K
C-----      WRITE(*,*)' SSCVAD START============'
C-----      CALL XYZ2LL(S, SLL)
C-----      CALL XYZ2LL(P1, P1LL)
C-----      CALL XYZ2LL(P2, P2LL)
C-----      WRITE(*,*)' SLL = ',  SLL
C-----      WRITE(*,*)' P1LL = ', P1LL
C-----      WRITE(*,*)' P2LL = ', P2LL
C---	NORTH POLE:  N
      N(1) = 0.0D0
      N(2) = 0.0D0
      N(3) = 1.0D0
C---	FIND Q, THE CLOSEST POINT ON THE P1 P2 PLANE TO NORTH POLE:
      CALL GCPERP(Q,P1,P2,N)
C---	CHECK TO SEE IF Q OR -Q IS BETWEEN P1 AND P2:
      CALL GCBTWN(J,P1,P2,Q)
      IF(J .EQ. 0) THEN
C---		ORDINARY, SIMPLE CASE.  Q IS NOT BETWEEN P1 AND P2
C---		SIMPLY COMPARE THE Z-COMPONENTS.  
      	IF(P1(3) .LT. P2(3)) ASCDSC = 'A'
      	IF(P1(3) .GT. P2(3)) ASCDSC = 'D'
      ELSEIF (J .EQ. 1) THEN
C---		THE CLOSEST POINT Q IS BETWEEN P1 AND P2.  
	CALL GCBTWN(K,Q,P2,S)
C---		NOW IF S IS BETWEEN Q AND P2, THEN Q IS "BEHIND" S, SO 
C---		S IS PAST Q AND NOW DESCENDING.  OTHERWISE, ASCENDING:
	IF(K .EQ. 1) ASCDSC = 'D'
	IF(K .NE. 1) ASCDSC = 'A'
      ELSEIF (J .EQ. -1) THEN
C---		-Q IS BETWEEN P1 AND P2:  WE ARE CLOSE TO THE SOUTN POLE.
	QM(1) = -Q(1)
	QM(2) = -Q(2)
	QM(3) = -Q(3)
C---		QM = -Q = CLOSEST APPROACH TO THE SOUTH POLE.  
	CALL GCBTWN(K,QM,P2,S)
C---		NOW IF S IS BETWEEN QM AND P2, THEN QM IS "BEHIND" S, SO 
C---		S IS PAST Q AND NOW ASCENDING.  OTHERWISE, DESCENDING:
	IF(K .EQ. 1) ASCDSC = 'A'
	IF(K .NE. 1) ASCDSC = 'D'
      ELSE
	CALL GCBOMB
      ENDIF 
C-----      WRITE(*,*)' SSCVAD:  END:  ASCDSC = ', ASCDSC
      RETURN
      END
