C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.

*************************************************************************
*  Name:	GCFRAC
*  Module Type: SUBROUTINE	Language: FORTRAN
*  $Logfile:   ACS003:[BLD.MPS.LIB.SRC]GCFRAC.FOV  $
*  Purpose:	DETERMINE THE RATIO OF THE DISTANCE BETWEEN P1 AND P2 TO THE 
*		DISTANCE BETWEEN P1 AND P3.  
*		GREAT CIRCLE DISTANCE.  
*  Subroutines called:
*  GCDIST
*  VECTOR LIBRARY MUST BE LINKED
*  Input Parameters:
*  3 POINTS ON A UNIT SPHERE, (X,Y,Z)
*  Name         Type    Definition
*  P1(3)	REAL*8	FIRST POINT 
*  P2(3)	REAL*8	SECOND 
*  P3(3)	REAL*8	THIRD
*  Output Parameters:
*  Name         Type    Definition
*  F		REAL*8	(DIST P1 P2) / (DIST P1 P3)
*  Variables:
*  Locals :
*  Externals :
*  Modification History:                                            
*  Date			Revision	Author
*  $Date$ $Revision$ $Author$
*                                                                   
*********************************************************************/
      SUBROUTINE GCFRAC (F,P1,P2,P3)
      character*100 SccsFileID
     -/'@(#)gcfrac.for	5.1 98/01/08 APS/ASF\0'/

      IMPLICIT NONE
      REAL*8 P1(3), P2(3), P3(3), F
      REAL*8 D3, D2
      CALL GCDIST(D2,P1,P2)
      CALL GCDIST(D3,P1,P3)
      F = D2 / D3
      RETURN
      END
