C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.

      SUBROUTINE VINTRD(W1,W2,V1,V2,VA)
      character*100 SccsFileID
     -/'@(#)vintrd.for	5.1 98/01/08 APS/ASF\0'/

**************************************************************************
*  Name:  VINTRD
c--port not necessary.  *	COMPILE IN D_FLOATING	
c--port not necessary.  *	COMPILE IN D_FLOATING	
c--port not necessary.  *	COMPILE IN D_FLOATING	
*  Module Type: SUBROUTINE	Language: FORTRAN 77
*  Purpose:
*	THIS ROUTINE COMPUTES A VECTOR VA BY A WEIGHTED INTERPOLATION 
*       BETWEEN V1 AND V2.  ALSO DOES A WEIGHTED INTERPOLATION WITH THE 
*	OTHER 6 WORDS.
*  Functions called: DINTRP (IN THIS SAME SOURCE FILE)
*  Input Parameters:
*  Name         Type    Definition
*  W1		REAL	WEIGHT OF VECTOR V1
*  W2		REAL	WEIGHT OF VECTOR V2
*  V1(12)	REAL	VECTOR V1.  IT CONTAINS R AND V IN CARTESIAN COORDS.
*  V2(12)	REAL	VECTOR V2.  IT CONTAINS R AND V IN CARTESIAN COORDS.
*			ALSO, THEY CONTAIN:
*			  TIME, REV#, LONG ASC NODE, SSLAT, SSLONG, 
*			  GREENWICH HOUR ANGLE.
*  Output Parameters:
*  Name         Type    Definition
*  VA(12)	REAL	VECTOR VA.  IT CONTAINS R AND V IN CARTESIAN COORDS.
*			
*  Variables:
*  Name         Type    Definition
*  Locals :
*  Externals :
*  Modification History:                                            
*  Date			Revision	Author
*  $Date$ $Revision$ $Author$
*  $Logfile:   ACS003:[BLD.MPS.LIB.SRC]VINTRD.FOV  $
*                                                                   
*********************************************************************/
*	
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
c
      DIMENSION V1(12),V2(12),VA(12)
C	    COMPUTE VA, A WEIGHTED AVERAGE OF VECTORS V1 AND V2.
C	        FIRST THE CHORDAL APPROXIMATION VECTORS WDS 1-6.
CCC      WRITE(*,*) 'W1=',W1,' W2=',W2
      DO 1000 I = 1,6
      VA(I) = W1*V1(I) + W2*V2(I)
 1000 CONTINUE
C---	VA NOW REACHES OUT TO A CHORD CONNECTING V1 AND V2.
C---	MUST EXTEND OUT TO A WEIGHTED AVERAGE OF THE MAGNITUDES OF V1 AND V2.
C	FOR x,y,z, AND Vx,Vy,Vz.
      V1MAG = DSQRT( V1(1)**2 + V1(2)**2 + V1(3)**2 )
      V2MAG = DSQRT( V2(1)**2 + V2(2)**2 + V2(3)**2 )
      VACRD = DSQRT( VA(1)**2 + VA(2)**2 + VA(3)**2 )
CCC      WRITE(*,*) 'V1MAG=',V1MAG, ' V2MAG=',V2MAG, ' VACRD=',VACRD
C---	COMPUTE WEIGHTED AVERAGE.
      VAMAG = W1*V1MAG + W2*V2MAG
C---	EXTEND VECTOR TO VAMAG.  
      DO 1001 I = 1,6
 1001 VA(I) = VA(I) * VAMAG/VACRD
C
C---	2 VECTORS NOW DONE; NOW AVERAGE THE LAST 6 WORDS.  
      VA(7) =  W1*V1(7)  + W2*V2(7)
      VA(8) =  W1*V1(8)  + W2*V2(8)
C---	MAKE SURE IN THESE ANGLE CHANGES ONE ISN'T LIKE -179 AND THE OTHER +179
C---	AND THEREFORE ONLY 2 DEGREES APART.  WANT A RESULT BETWEEN THE TWO, 
C---	NOT A RESULT LIKE 0.  NEEDED FOR WORDS 9, 11, 12.
      CALL DINTRP(W1,W2,V1( 9),V2( 9),VA( 9) )
      VA(10) = W1*V1(10) + W2*V2(10)
      CALL DINTRP(W1,W2,V1(11),V2(11),VA(11) )
      CALL DINTRP(W1,W2,V1(12),V2(12),VA(12) )
      RETURN
      END
      SUBROUTINE DINTRP(W1,W2,D1,D2,C )
C---	INTERPOLATION FOR DEGREES WHEN A AND B MIGHT STRADDLE A DISCONTINUITY 
C---	IN DEG.  SMALL MOVEMENTS ARE ASSUMED (.LT.180 DEG) AND MOVEMENT CAN 
C---	BE CLOCKWISE OR COUNTER CLOCKWISE, AS DEDUCED FROM THE SMALL MOVEMENT
C---	ASSUMPTION.  
C---	FOR SMALL ANGULAR MOVEMENTS ONLY.  
C---	FOR SMALL ANGULAR MOVEMENTS ONLY.  
C---	FOR SMALL ANGULAR MOVEMENTS ONLY.  
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      A = D1
      B = D2
C---      WRITE(*,*)' DINTRP:',W1,W2,A,B
      IF( ABS(A-B) .LE. 180.0) GO TO 7000
      IF ( A .LT. B ) GO TO 1000
      B = B + 360.0D0
      GO TO 7000
 1000 CONTINUE
      A = A + 360.0D0
 7000 CONTINUE
      C = W1*A + W2*B
      C = DMOD(C,360.0D0)
C---      WRITE(*,*)' DINTRP:',A,B,C
C---      WRITE(*,*)' '
      RETURN
      END
