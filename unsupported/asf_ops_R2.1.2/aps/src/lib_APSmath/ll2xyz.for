C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.

********************************************************************
*  Name:	LL2XYZ
*  Module Type: SUBROUTINE	Language: FORTRAN
*  $Logfile:   ACS003:[BLD.MPS.LIB.SRC]LL2XYZ.FOV  $
*  Purpose:	CHANGE VECTOR FROM LAT/LON TO XYZ ON UNIT SPHERE.
*  Functions called:
*  VECTOR LIBRARY:  SPHERE
*  Input Parameters:
*  Name         Type    Definition
*  XL(2)	REAL*8	LAT/LON IN DEGREES
*  Output Parameters:
*  Name         Type    Definition
*  XYZ(3)	REAL*8	XYZ POINT ON UNIT SPHERE.
*  Variables:
*  Locals :
*  Externals :
*  Modification History:                                            
*  Date			Revision	Author
*  $Date$ $Revision$ $Author$
*                                                                   
*********************************************************************/
      SUBROUTINE LL2XYZ (PLL,PXYZ)
      character*100 SccsFileID
     -/'@(#)ll2xyz.for	5.1 98/01/08 APS/ASF\0'/

      IMPLICIT NONE
      REAL*8 PLL(2), PXYZ(3), PSPH(3)
C---	INTERFACE TO VECTOR LIBRARY
C---	LENGTH
      PSPH(1) = 1.0D00
C---	LATITUDE
      IF(PLL(1) .GT. 90.00D00) PLL(1) = PLL(1) - 360.00
      PSPH(2) = PLL(1)
C---	EAST LONGITUDE
      IF(PLL(2) .GT. 180.00D00) PLL(2) = PLL(2) - 360.00
      PSPH(3) = PLL(2)
      CALL SPHERE(PXYZ,PSPH,'FROM','LATLON','DEGREE')
      RETURN
      END
