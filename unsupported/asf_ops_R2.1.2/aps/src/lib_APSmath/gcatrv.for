C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.

*************************************************************************
*  Name:	GCATRV
*  Module Type: SUBROUTINE	Language: FORTRAN
*  $Logfile:   ACS003:[BLD.MPS.LIB.SRC]GCATRV.FOV  $
*  Purpose:	TRAVEL FROM P1 TO P2, TURN AN ANGLE A (DEG) TO THE RIGHT, THEN
*		TRAVEL A DISTANCE D, DEFINING POINT Q.  
*		GOING STRAIGHT FORWARD IS 180 DEGREES.  RIGHT TURN IS +90 DEG
*		LEFT TURN IS -90 DEGREES.  
*  Subroutines called:
*  GCTRAV
*  MUST LINK VECTOR LIBRARY: VROTAT
*  Input Parameters:
*  2 POINTS ON A UNIT SPHERE, (X,Y,Z)
*  Name         Type    Definition
*  P1(3)	REAL*8	FIRST POINT 
*  P2(3)	REAL*8	SECOND POINT
*  A		REAL*8	ANGLE IN DEGREES
*  D		REAL*8	DISTANCE ON THE UNIT SPHERE TO TRAVEL  
*  Output Parameters:
*  Name         Type    Definition
*  Q	(3)	REAL*8	RESULT POINT.
*  Variables:
*  Locals :
*  Externals :
*  Modification History:                                            
*  Date			Revision	Author
*  $Date$ $Revision$ $Author$
*                                                                   
*********************************************************************/
      SUBROUTINE GCATRV (Q,P1,P2,ADEG,D)
      character*100 SccsFileID
     -/'@(#)gcatrv.for	5.1 98/01/08 APS/ASF\0'/

      IMPLICIT NONE
      REAL*8 P1(3), P2(3), Q(3), ADEG, D
      REAL*8 X(3), ARAD
      INTEGER IFLAG
      INCLUDE 'APS_HOME:include/local/mps_const_math.inc'
      ARAD = ADEG / DG2RAD
      CALL GCLOSE(IFLAG,P1,P2)
      IF(IFLAG.NE.0) CALL GCBOMB
      CALL VROTAT(X,P1,P2,ARAD)
      CALL GCTRAV(Q,P2,X,D)
      RETURN
      END
