C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.

********************************************************************
*  Name:	XYZ2LL
*  Module Type: SUBROUTINE	Language: FORTRAN
*  $Logfile:   ACS003:[BLD.MPS.LIB.SRC]XYZ2LL.FOV  $
*  Purpose:	CHANGE VECTOR FROM XYZ ON UNIT SPHERE TO LAT/LON .
*  Functions called:
*  MASL VECTOR LIBRARY:  SPHERE, VUNITM
*  Input Parameters:
*  XYZ(3)	REAL*8	XYZ POINT ON UNIT SPHERE.
*  Name         Type    Definition
*  Output Parameters:
*  Name         Type    Definition
*  XL(2)	REAL*8	LAT/LON IN DEGREES
*  Variables:
*  Locals :
*  Externals :
*  Modification History:                                            
*  Date			Revision	Author
*  $Date$ $Revision$ $Author$
*                                                                   
*********************************************************************/
      SUBROUTINE XYZ2LL (PXYZ,PLL)
      character*100 SccsFileID
     -/'@(#)xyz2ll.for	5.1 98/01/08 APS/ASF\0'/

      IMPLICIT NONE
      REAL*8 PLL(2), PXYZ(3), PSPH(3), U(3), T
C---	MAKE PXYZ INTO A UNIT VECTOR FOR PRECISION.
      CALL VUNITM(T,U,PXYZ,3)
C---	TRANSFORM U TO SPHERICAL COORDINATES.  
      CALL SPHERE(PSPH,U,'TO','LATLON','DEGREE')
C---	LATITUDE
      PLL(1) = PSPH(2)
C---	EAST LONGITUDE
      PLL(2) = PSPH(3)
      IF(PLL(2).GT. 180.0D00) PLL(2) = PLL(2) - 360
      IF(PLL(2).LE.-180.0D00) PLL(2) = PLL(2) + 360
      RETURN
      END
