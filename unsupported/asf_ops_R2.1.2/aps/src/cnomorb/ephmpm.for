C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	ephmpm.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================
      SUBROUTINE EPHMPM (ETIME, PM, IT)

      character*100 SccsFileID
     -/'@(#)ephmpm.for	5.1 98/01/08 APS/ASF\0'/

**************************************************************************
*  Name:  EPHMPM
*
*  Module Type: SUBROUTINE	Language: FORTRAN 77
*
*  $Logfile:   ACS003:[BLD.MPS.EPHM.SRC]EPHMPM.FOV  $
*  Purpose:
*	COMPUTES EARTH PRIME PRIME MERIDIAN ANGLE GIVEN A TIME.
*
*  Functions called: DMAG, CROSS (FROM MATHNEW IN MARTIN'S LIBRARY)
*
*  Input Parameters:
*                                                                   
*  Name         Type    Definition
*  ETIME	REAL	EPHEMERIS TIME.  REAL JULIAN DAYS.
*  IT		REAL	TEST PRINT UNIT NUMBER.  USE 0 FOR NO PRINT.
*  Output Parameters:
*  Name         Type    Definition
*  PM		REAL	PRIME MERIDIAN ANGLE IN DEGREES
*  
*  Variables:
*  Name         Type    Definition
*
*  Locals :
*
*  Externals :
*                                                                   
*  Modification History:                                            
*                                                                   
*  Date			Revision	Author
*  $Date$ $Revision$ $Author$
*                                                                   
*********************************************************************/
*	
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
c
      DIMENSION PMV(6), PMVA(6)
      REAL*16 XYUNIT(2), XN16
      INTEGER IT
      IF(IT.NE.0)WRITE(IT,*) '$$EPHMPM:',ETIME,IT
C---	GET EARTH PRIME MERIDIAN VECTOR AT ETIME IN 114 COORD.
C---	TRUE EQUATOR PRIME MERIDIAN OF DATE CARTESIAN. 
      PMV(1) = 1.0000000000
      PMV(2) = 0.0000000000
      PMV(3) = 0.0000000000
      IF(IT.NE.0)WRITE(IT,*) 'EPHMPM:',(PMV(I),I=1,3)
C---	TRANSFORM 114 TO ASAP COORDINATES.
      IASAP = 12
      IVSTD = 114
      CALL ETRANS(PMVA,IASAP,PMV,IVSTD,ETIME)
      XN16 = PMVA(1)**2 + PMVA(2)**2 
      XN16 = SQRT(XN16)
CCC      WRITE(*,*)' XYNORM**2 XN16=',XN16
C---	MAKING A UNIT VECTOR IN THE XY PLANE USING REAL*16.
      XYUNIT(1) = PMVA(1) / XN16
      XYUNIT(2) = PMVA(2) / XN16
CCC      WRITE(*,*) 'XYUNIT=',XYUNIT
C---	XYUNIT IS A UNIT VECTOR IN THE XY LANE IN THE MERIDIAN DIRECTION.  
C---	XYUNIT(1) = COS(PM), XYUNIT(2) = SIN(PM)
C---	ASK FOR ACOS IN DEGREES:
c--port-- note that the generic arc cosine in degrees is used, so that the 
c---	result will have the same precision as the argument.  
      PM = ACOSD(XYUNIT(1))
C---	NOW WE HAVE TWO CASES:
      IF ( XYUNIT(2) .LT. 0.0 ) GO TO 2000
C---	CASE 1. FIRST OR SECOND QUADRANT.  0 <= PM <= 180.0 DEGREES.
      GO TO 9999
 2000 CONTINUE
CCC      WRITE(*,*) 'CASE 2.'
C---	CASE 2. SECOND OR THIRD QUADRANT.  180.0 < PM < 360.0 DEGREES.
      PM = 360.000000000000 - PM
 9999 CONTINUE
      IF(IT.NE.0)WRITE(IT,*) '@@EPHMPM:',ETIME,PM,(PMVA(I),I=1,3)
      RETURN
      END
