C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.

*************************************************************************
*  Name:	GCTRAV
*  Module Type: SUBROUTINE	Language: FORTRAN
*  $Logfile:   ACS003:[BLD.MPS.LIB.SRC]GCTRAV.FOV  $
*  Purpose:	TRAVEL A DISTANCE D FROM P1 TOWARDS P2, DERIVING Q
*		IF YOU LIKE, D CAN BE NEGATIVE, IN WHICH CASE YOU GO THE 
*		OPPOSITE DIRECTION.
*  Subroutines called:
*  GCLOSE, GCBOMB
*  VECTOR LIBRARY: UCROSM, VCOMB
*  Input Parameters:
*  2 POINTS ON A UNIT SPHERE, (X,Y,Z)
*  Name         Type    Definition
*  P1(3)		REAL*8	FIRST POINT 
*  P2(3)	REAL*8	SECOND 
*  D		REAL*8	DISTANCE TO TRAVEL.  CAN BE NEGATIVE, IN WHICH 
*			CASE THE MOTION IS IN THE OPPOSITE DIRECTION.
*  Output Parameters:
*  Name         Type    Definition
*  Q(3)		REAL*8	RESULT OF THE JOURNEY
*  Variables:
*  Locals :
*  Externals :
*  Modification History:                                            
*  Date			Revision	Author
*  $Date$ $Revision$ $Author$
*                                                                   
*********************************************************************/
      SUBROUTINE GCTRAV (Q,P1,P2,D)
      character*100 SccsFileID
     -/'@(#)gctrav.for	5.1 98/01/08 APS/ASF\0'/

      IMPLICIT NONE
      REAL*8 Q(3), P1(3), P2(3), D
      REAL*8 X(3), Y(3), S, C
      INTEGER IFLAG
      IF (D.NE.0.0D00) GO TO 1000
C---	D = 0.
      Q(1) = P1(1)
      Q(2) = P1(2)
      Q(3) = P1(3)
      GO TO 9999
 1000 CONTINUE
C---	SPECIAL CASE P1=P2 OR P1=-P2.  THE DESIRED RESULT IS UNDEFINED.
C---	IF THIS IS THE CASE, BOMB THE PROGRAM.
      CALL GCLOSE(IFLAG,P1,P2)
      IF(IFLAG .NE. 0 ) CALL GCBOMB
C---	O.K.  WE CAN WORK.  
C---	WE WILL USE P1 AS OUR X-AXIS, THE CENTER OF THE UNIT SPHERE AS OUR
C---	ORIGIN, AND P1 x P2 x P1 AS THE DIRECTION OF OUR Y-AXIS.  IN THIS WAY,
C---	P2 LIES IN THE X-Y PLANE, AND IT LIES ON THE POSITIVE Y SIDE OF THE 
C---	X-AXIS.  
C---	THEN ALL WE HAVE TO DO IS TO DETERMINE THE ANGLE FROM X TO THE 
C---	DESTINATION AND USE THE OLD FORMULA:  X*COS(A) + Y*SIN(A) TO GET Q.
      CALL UCROSM(S,X,P1,P2)
      CALL UCROSM(S,Y,X,P1)
C---	THE DISTANCE D ON THE UNIT SPHERE IS THE ANGLE IN RADIANS.
      C = COS(D)
      S = SIN(D)
C---	REMEMBER WE USE P1 AS X AND Y AS Y.  
      CALL VCOMB(Q,P1,C,Y,S,3)
 9999 CONTINUE
      RETURN
      END
