C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.

*************************************************************************
*  Name:	MPSINTRP
*  Module Type: SUBROUTINE	Language: FORTRAN
*  $Logfile:   ACS003:[BLD.MPS.LIB.SRC]MPSINTRP.FOV  $
*  Purpose:	COMPUTE EVENLY SPACED POINTS ON A GREAT CIRCLE BETWEEN P1 AND 
*		P2.
*  Subroutines called:
*  GCBOMB, GCDIST, GCLOSE, GCTRAV, LL2XYZ, XYZ2LL
*  MUST LINK IN THE MASL VECTOR LIBRARY: REAL*8
*  Input Parameters:
*  Name         Type    Definition
*  P1LAT	REAL*4	FIRST POINT LATITUDE
*  P1LON	REAL*4	FIRST POINT LONGITUDE
*  P2LAT	REAL*4	FIRST POINT LATITUDE
*  P2LON	REAL*4	FIRST POINT LONGITUDE
*  N		INTEGER	NUMBER OF POINTS ON P1 P2 INCLUDING BOTH P1 AND P2.
*  Output Parameters:
*  Name         Type    Definition
*  PLATS(N)	REAL*4	LATITUDES OF POINTS 1 THROUGH N.  (PLATS(N)=P2LAT
*  PLATS(N)	REAL*4	LONGITUDES OF POINTS 1 THROUGH N. (PLONS(N)=P2LON
*  Variables:
*  Locals :
*  Externals :
*  Modification History:                                            
*  Date			Revision	Author
*  $Date$ $Revision$ $Author$
*                                                                   
*********************************************************************/
      SUBROUTINE MPSINTRP (P1LAT,P1LON,P2LAT,P2LON,N,PLATS,PLONS)
      character*100 SccsFileID
     -/'@(#)mpsintrp.for	5.1 98/01/08 APS/ASF\0'/

      IMPLICIT NONE
      REAL*4 P1LAT,P1LON,P2LAT,P2LON,PLATS(*),PLONS(*)
C---	REAL*8 VARIABLES.
      REAL*8 XLL(2),P1(3),P2(3),Q(3),D12,DQ
      INTEGER N, I, IFLAG
C---	CHECK FOR ERROR CONDITIONS.
      IF(N.LE.1) CALL GCBOMB
      PLATS(1) = P1LAT
      PLONS(1) = P1LON
      PLATS(N) = P2LAT
      PLONS(N) = P2LON
C---	IF(N.EQ.2) GO TO 9999
 1000 CONTINUE
C---	N = 3 OR MORE 
C---	CONVERT TO XYZ.  
      XLL(1) = P1LAT
      XLL(2) = P1LON
      CALL LL2XYZ(XLL,P1)
      XLL(1) = P2LAT
      XLL(2) = P2LON
      CALL LL2XYZ(XLL,P2)
C---	CHECK FOR P1 CLOSE TO P2.  
      CALL GCLOSE(IFLAG,P1,P2)
      IF(IFLAG.EQ.0) GO TO 2000
C---	CHECK FOR P1=-P2 AND UNDEFINED GREAT CIRCLE. 
      IF(IFLAG.EQ.-1) CALL GCBOMB
C---	IFLAG = 1 AND P1=P2.  LOAD THE ARRAYS WITH P1 VALUES:
      DO 1400 I = 2,N - 1
      PLATS(I) = P1LAT
      PLONS(I) = P1LON
 1400 CONTINUE
      GO TO 9999
 2000 CONTINUE
C---	NORMAL CASE.
      CALL GCDIST(D12,P1,P2)
      DO 3000 I = 1,N-2
      DQ = I * (D12 / (N - 1))
      CALL GCTRAV(Q,P1,P2,DQ)
      CALL XYZ2LL(Q,XLL)
C---	HERE IS A CONVERSION FROM REAL*8 TO REAL*4:
      PLATS(I+1) = XLL(1)
      PLONS(I+1) = XLL(2)
 3000 CONTINUE
 9999 CONTINUE
      RETURN
      END
