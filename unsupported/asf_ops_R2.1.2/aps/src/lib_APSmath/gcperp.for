C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.

*************************************************************************
*  Name:	GCPERP
*  Module Type: SUBROUTINE	Language: FORTRAN
*  $Logfile:   ACS003:[BLD.MPS.LIB.SRC]GCPERP.FOV  $
*  Purpose:	GIVEN A SEGMENT P1 P2, CONSTRUCT X BY BRINGING A PERPENDICULAR 
*		FROM POINT Q TO THE P1 P2 PLANE.  X MAY OR MAY NOT BE WITHIN
*		THE SEGMENT P1 P2.  
*  Subroutines called:
*  GCLOSE, GCBOMB
*  VECTOR LIBRARY: UCROSS
*  Input Parameters:
*  3 POINTS ON A UNIT SPHERE, (X,Y,Z)
*  Name         Type    Definition
*  P1(3)	REAL*8	FIRST POINT 
*  P2(3)	REAL*8	SECOND 
*  Q(3)		REAL*8	TEST POINT.
*  Output Parameters:
*  Name         Type    Definition
*  X(3)		REAL*8	A POINT ON THE P1 P2 PLANE SUCH THAT XQ IS 
*			PERPENDICULAR TO IT.  X MAY OR MAY NOT BE BETWEEN
*			P1 AND P2.
*  Variables:
*  Locals :
*  Externals :
*  Modification History:                                            
*  Date			Revision	Author
*  $Date$ $Revision$ $Author$
*                                                                   
*********************************************************************/
      SUBROUTINE GCPERP (X,P1,P2,Q)
      character*100 SccsFileID
     -/'@(#)gcperp.for	5.1 98/01/08 APS/ASF\0'/

      IMPLICIT NONE
      REAL*8 P1(3), P2(3), Q(3), X(3)
      REAL*8 PP(3), XVEC(3)
      INTEGER IFLAG
C---	SPECIAL CASE P1=P2 OR P1=-P2.  THE PLANE P1 P2 IS UNDEFINED.
C---	IF THIS IS THE CASE, BOMB THE PROGRAM.
      CALL GCLOSE(IFLAG,P1,P2)
      IF(IFLAG .NE. 0 ) CALL GCBOMB
C---	SPECIAL CASE:  Q IS PERPENDICULAR TO THE P PLANE AND X IS UNDEFINED.
C---	IF THIS IS THE CASE, BOMB THE PROGRAM.
C---	GET NORMAL TO P PLANE 
      CALL UCROSS(PP,P1,P2)
      CALL GCLOSE(IFLAG,PP,Q)
      IF(IFLAG .NE. 0 ) CALL GCBOMB
C---	O.K.  WE CAN WORK. 
C---	NOW X WILL BE (PP x Q) x PP, USING UNIT CROSSES.  
C---              XVEC = PP x Q
      CALL UCROSS(XVEC,PP,Q)
C---              X = XVEC x PP
      CALL UCROSS(X,XVEC,PP)
C---	AS PROMISED, X LIES IN THE P PLANE BECAUSE IT IS NORMAL TO PP.
C---	             X ALSO IS NORMAL TO XVEC AND THEREFORE LIES IN THE PLANE 
C---			OF PP AND Q, WHICH IS NORMAL TO THE P PLANE.
      RETURN
      END
