C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.

*************************************************************************
*  Name:	SWANGL
*  Module Type: SUBROUTINE	Language: FORTRAN
*  $Logfile:   ACS003:[BLD.MPS.LIB.SRC]SWANGL.FOV  $
*  Purpose:	THIS ROUTINE DETERMINES COVERAGE OR THE LACK OF IT
*		AT THE BEGINNING OF A SWATH BOX.  IT IS CALLED ONLY
*		IF THE LEFT OR RIGHT POINT IN THE START OF THE SWATH 
*		COINCIDES WITH ONE OF THE FOUR POINTS IN A QUADRILATERAL.
*		IT DETERMINES COVERAGE BY CHECKING THE ANGLES MADE BY THE 
*		SWATH AND QUAD BORDERS.  
*  Subroutines called:
*  GCBOMB, GCDIST, GCENTC
*  MUST LINK VECTOR LIBRARY, DOUBLE PRECISION:  DOT
*  Input Parameters:
*  Name         Type    Definition
*  SL(3)	REAL*8	POINT ON THE SWATH TO THE LEFT OF X.
*  X(3)		REAL*8	THE POINT IN COMMON WITH START OF SWATH AND QUAD
*  SR(3)	REAL*8	POINT ON THE SWATH TO THE RIGHT OF X.
*  QL(3)	REAL*8	POINT ON THE QUAD TO THE LEFT OF X.
*  QR(3)	REAL*8	POINT ON THE QUAD TO THE RIGHT OF X.
*  Output Parameters:
*  Name         Type    Definition
*  IFLAG	INTEGER	=  1 IF COVERAGE EXISTS
*			= -1 IF NOT.
*  Variables:
*  Locals :
*  Externals :
*  Modification History:                                            
*  Date			Revision	Author
*  $Date$ $Revision$ $Author$
*                                                                   
*********************************************************************/
      SUBROUTINE SWANGL (SL,X,SR,QL,QR,IFLAG)
      character*100 SccsFileID
     -/'@(#)swangl.for	5.1 98/01/08 APS/ASF\0'/

      IMPLICIT NONE
      REAL*8 SL(3), X(3), SR(3), QL(3), QR(3)
      REAL*8 ASL,AQL,AQR
      INTEGER IFLAG
C---	CHECK THE SPHERICAL ANGLES OF THE FOUR POINTS USING X AS THE VERTEX
C---	IN EACH CASE.  
C---	MEASURE EACH ANGLE STARTING AT SR - X.  THINK OF THE XY PLANE, WITH
C---	SR AT THE X AXIS, X AT THE ORIGIN, AND THE THIRD POINT COMPLETING
C---	THE ANGLE.  THE RESULTS ARE <= 180 AND > -180
      IFLAG = 1
      CALL SANGLD(AQR,SR,X,QR)
C---	IF COVERAGE IS ON THE SAME SIDE OF THE PLANE AS THE AREA, THEN 
C---	WE HAVE COVERAGE.  
      IF(AQR.EQ.0.00D00) GO TO 9999
      IF(AQR.GT.0.00D00) GO TO 1000
C---	AQR .LT. 0  
C---MUST HAVE X - SR BETWEEN THE QL AND QR RAYS EMANATING 
C---	FROM X.  
      CALL SANGLD(AQL,SR,X,QL)
      IF(AQL.GT.0.00D00) GO TO 9999
C---	NO COVERAGE. 
      IFLAG = -1
      GO TO 9999
 1000 CONTINUE
C---	AQR .GT. 0.  MUST HAVE X - QR BETWEEN THE SL AND SR RAYS.  
      CALL SANGLD(ASL,SR,X,SL)
      IF(ASL.GT.AQR) GO TO 9999
      IFLAG = -1
 9999 CONTINUE
      RETURN
      END
