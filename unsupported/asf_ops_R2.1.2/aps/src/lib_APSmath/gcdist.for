C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.

*************************************************************************
*  Name:	GCDIST
*  Module Type: SUBROUTINE	Language: FORTRAN
*  $Logfile:   ACS003:[BLD.MPS.LIB.SRC]GCDIST.FOV  $
*  Purpose:	DETERMINE THE DISTANCE BETWEEN 2 POINTS ON A UNIT SPHERE:
*		GREAT CIRCLE DISTANCE.  
*  Subroutines called:
*  VECTOR LIBRARY: UCROSSM, DOT
*  Input Parameters:
*  2 POINTS ON A UNIT SPHERE, (X,Y,Z)
*  Name         Type    Definition
*  P1		REAL*8	FIRST POINT 
*  P2		REAL*8	SECOND 
*  Output Parameters:
*  Name         Type    Definition
*  D		REAL*8	= DISTANCE BETWEEN THE 2 POINTS.
*  Variables:
*  Locals :
*  Externals :
*  Modification History:                                            
*  Date			Revision	Author
*  $Date$ $Revision$ $Author$
*                                                                   
*********************************************************************/
      SUBROUTINE GCDIST (D,P1,P2)
      character*100 SccsFileID
     -/'@(#)gcdist.for	5.1 98/01/08 APS/ASF\0'/

      IMPLICIT NONE
      REAL*8 P1(3), P2(3), D
      REAL*8 X(3), S, C
C---	VECTOR LIBRARY FUNCTION
      REAL*8 DOT
C---	S IS THE SINE AND C IS THE COSINE OF THE ANGLE THE TWO VECTORS MAKE.
C---	TAKE THE RESULT IN RADIANS.  THAT IS THE DISTANCE ON THE UNIT SPHERE.
      CALL UCROSM(S,X,P1,P2)
      C = DOT(P1,P2,3)
      D = ABS( ATAN2(S,C) )
      RETURN
      END
