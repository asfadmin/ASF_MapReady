C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.

*************************************************************************
*  Name:	GCLOSE
*  Module Type: SUBROUTINE	Language: FORTRAN
*  $Logfile:   ACS003:[BLD.MPS.LIB.SRC]GCLOSE.FOV  $
*  Purpose:	DETERMINE IF TWO POINTS ARE CLOSE ENOUGH TO BE CONSIDERED 
*		IDENTICAL, OR IF THEY ARE CLOSE ENOUGH TO BEING OPPOSITE TO 
*		BE CONSIDERED THEIR OPPOSITES.
*  Subroutines called:
*  VECTOR LIBRARY: UCROSSM, DOT
*  Input Parameters:
*  2 POINTS ON A UNIT SPHERE, (X,Y,Z)
*  Name         Type    Definition
*  P1		REAL*8	FIRST POINT 
*  P2		REAL*8	SECOND 
*  Output Parameters:
*  Name         Type    Definition
*  IFLAG	INTEGER	
*			=  1 IF  P1 AND P2 ARE CLOSE.
*			= -1 IF  P1 AND P2 ARE OPPOSITE
*			=  0 IF NEITHER 
*  Variables:
*  Locals :
*  Externals :
*  Modification History:                                            
*  Date			Revision	Author
*  $Date$ $Revision$ $Author$
*                                                                   
*********************************************************************/
      SUBROUTINE GCLOSE (IFLAG,P1,P2)
      character*100 SccsFileID
     -/'@(#)gclose.for	5.1 98/01/08 APS/ASF\0'/

      IMPLICIT NONE
      INTEGER IFLAG
      REAL*8 P1(3), P2(3)
      REAL*8 X(3), S, C
C---	VECTOR LIBRARY FUNCTION
      REAL*8 DOT
      IFLAG = 0
C---	TEST FOR A UNIT VECTOR.  IF THE TEST FAILS, THEN BOMB THE PROGRAM.
      C = DOT(P1,P1,3)
      IF(ABS(C-1.0) .GT. 0.000000000001D00 ) CALL GCBOMB
      C = DOT(P2,P2,3)
      IF(ABS(C-1.0) .GT. 0.000000000001D00 ) CALL GCBOMB
C---	S WILL BE THE ABSOLUTE VALUE OF THE SINE OF THE ANGLE BETWEEN 
C---	P1 AND P2.  WE USE THE MAGNITUDE OF THE CROSS PRODUCT AND THE FACT THAT 
C---	P1 AND P2 ARE UNIT VECTORS.  
      CALL UCROSM(S,X,P1,P2)
      IF(S .GT. 1.0D-09) GO TO 9999
C---	THE ANGLE BETWEEN THEM WAS GREATER THAN 5.73 x 10**-8 DEGREES.
C---	ON THE EARTH, THIS WOULD BE 3.77 x 10**-6 NAUTICAL MILES.  
C---				 OR 6.98 x 10**-6 KILOMETERS
C---				 OR 6.98          MILLIMETERS
C--	(THEY COULD ALSO BE THIS CLOSE TO BEING OPPOSITES.)
C
C---	P1 AND P2 COULD BE CLOSE OR OPPOSITES.  CHECK FOR COSINE OF THE ANGLE 
C---	BETWEEN THEM.
      C = DOT(P1,P2,3)
      IF (C.GT.0) IFLAG = 1
      IF (C.LE.0) IFLAG = -1
 9999 CONTINUE
      RETURN
      END
