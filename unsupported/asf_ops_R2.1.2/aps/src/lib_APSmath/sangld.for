C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.

********************************************************************
*  Name:	SANGLD
*  Module Type: SUBROUTINE	Language: FORTRAN
*  $Logfile:   ACS003:[BLD.MPS.LIB.SRC]SANGLD.FOV  $
*  Purpose:	COMPUTE SPHERICAL ANGLE IN DEGREES FROM THREE POINTS ON THE 
*		UNIT SPHERE
*  Subroutines called:
*  GCANGL
*  MUST LINK VECTOR LIBRARY.  
*  Input Parameters:
*  3 POINTS ON A UNIT SPHERE, (X,Y,Z)
*  Name         Type    Definition
*  P1		REAL*8	FIRST POINT 
*  P2		REAL*8	SECOND POINT
*  P3		REAL*8	THIRD POINT
*  Output Parameters:
*  Name         Type    Definition
*  ADEG		REAL*8	ANGLE WITH P2 AT THE VERTEX.  -180 < ADEG <= 180.
*			AS YOU GO FROM P1 TO P2 AND THEN TO P3, IF YOU TURN
*			RIGHT, THEN ADEG IS POSITIVE;  LEFT:  ADEG IS NEGATIVE.
*			THINK OF P2 AS THE ORIGIN ON THE XY PLANE.  P1 IS OUT
*			ON THE X-AXIS IN THE POSITIVE X DIRECTION SOMEWHERE.
*			THE P2 P3 FORMS A RAY IN THE XY PLANE AT AN ANGLE
*			USUALLY KNOWN AS THETA.  THE POSITIVE AND NEGATIVE 
*			ANGLE CONVENTION COMES FROM THERE.  
*  Variables:
*  Locals :
*  Externals :
*  Modification History:                                            
*  Date			Revision	Author
*  $Date$ $Revision$ $Author$
*                                                                   
*********************************************************************/
      SUBROUTINE SANGLD (ADEG,P1,P2,P3)
      character*100 SccsFileID
     -/'@(#)sangld.for	5.1 98/01/08 APS/ASF\0'/

      IMPLICIT NONE
      REAL*8 P1(3),P2(3),P3(3), ADEG, ANGRAD
      INCLUDE 'APS_HOME:include/local/mps_const_math.inc'
      CALL SANGL(ANGRAD,P1,P2,P3)
      ADEG = ANGRAD * DG2RAD
      RETURN
      END
