C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.

*************************************************************************
*  Name:	GCTRVF
*  Module Type: SUBROUTINE	Language: FORTRAN
*  $Logfile:   ACS003:[BLD.MPS.LIB.SRC]GCTRVF.FOV  $
*  Purpose:	TRAVEL FROM P1 TOWARDS P2 AND GO A FRACTION(F) OF THE WAY.
*		THIS POINT IS Q.
*  Subroutines called:
*  GCDIST, GCTRAV
*  MUST LINK VECTOR LIBRARY
*  Input Parameters:
*  2 POINTS ON A UNIT SPHERE, (X,Y,Z)
*  Name         Type    Definition
*  P1(3)	REAL*8	FIRST POINT 
*  P2(3)	REAL*8	SECOND 
*  F		REAL*8	FRACTION, TYPICALLY BETWEEN 0 AND 1.
*			BUT YOU CAN USE LARGE NEGATIVE OR POSITIVE NUMBERS.
*  Output Parameters:
*  Name         Type    Definition
*  Q	(3)	REAL*8	RESULT POINT ON THE SEGMENT P1 P2.
*  Variables:
*  Locals :
*  Externals :
*  Modification History:                                            
*  Date			Revision	Author
*  $Date$ $Revision$ $Author$
*                                                                   
*********************************************************************/
      SUBROUTINE GCTRVF (Q,P1,P2,F)
      character*100 SccsFileID
     -/'@(#)gctrvf.for	5.1 98/01/08 APS/ASF\0'/

      IMPLICIT NONE
      REAL*8 P1(3), P2(3), Q(3), F
      REAL*8 D
      CALL GCDIST(D,P1,P2)
      D = D * F
      CALL GCTRAV(Q,P1,P2,D)
      RETURN
      END
