C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.

*************************************************************************
*  Name:	MPSCLOSE
*  Module Type: SUBROUTINE	Language: FORTRAN
*  $Logfile:   ACS003:[BLD.MPS.LIB.SRC]MPSCLOSE.FOV  $
*  Purpose:	DETERMINE IF TWO POINTS ARE CLOSE ENOUGH TO BE CONSIDERED 
*		IDENTICAL, OR IF THEY ARE CLOSE ENOUGH TO BEING OPPOSITE TO 
*		BE CONSIDERED THEIR OPPOSITES.
*  Subroutines called:
*  GCLOSE, LL2XYZ
*  Input Parameters:
*  2 POINTS ON A UNIT SPHERE, (LAT/LON)
*  Name         Type    Definition
*  P1LAT	REAL*4	FIRST POINT LATITUDE
*  P1LON	REAL*4	FIRST POINT LONGITUDE
*  P2LAT	REAL*4	SECOND POINT LATITUDE
*  P2LON	REAL*4	SECOND POINT LONGITUDE
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
*  7/27/94 Nadia Adhami ported to UNIX/C    
* 
*********************************************************************/
      SUBROUTINE MPSCLOSE (P1LAT,P1LON,P2LAT,P2LON,IFLAG)
      character*100 SccsFileID
     -/'@(#)mpsclose.for	5.1 98/01/08 APS/ASF\0'/

      IMPLICIT NONE
      INTEGER IFLAG
      REAL*4 P1LAT,P1LON,P2LAT,P2LON
      REAL*8 P1(3), P2(3), XLL(2)
      XLL(1) = P1LAT
      XLL(2) = P1LON
      CALL LL2XYZ(XLL,P1)
      XLL(1) = P2LAT
      XLL(2) = P2LON
      CALL LL2XYZ(XLL,P2)
      CALL GCLOSE(IFLAG,P1,P2)
      RETURN
      END
