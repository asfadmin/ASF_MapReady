C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:  mps2gha.for
C--
C--  Description:
C--
C--  Notes:
C--
C-- ==========================================================================

**************************************************************************
*  Name:  MPS2GHA
*  Module Type: SUBROUTINE	Language: FORTRAN 77
*  $Logfile:   ACS003:[BLD.MPS.GHA.SRC]MPS2GHA.FOV  $
*  Purpose:
*	COMPUTES GREENWICH HOUR ANGLE IN DEGREES 0-360 GIVEN A UTC TIME.
*  Functions called: DMAG, CROSS, VECTOR LIBRARY.
*  Input Parameters:
*  Name         Type    Definition
*  ASFT		CHAR*21	ASF TIME.  
*  IT		INT	TEST PRINT UNIT NUMBER.  USE 0 FOR NO PRINT.
*  Output Parameters:
*  Name         Type    Definition
*  GHA		REAL	GREENWICH HOUR ANGLE IN DEGREES 0-360
*  Variables:
*  Name         Type    Definition
*  Locals :
*  Externals :
*  Modification History:                                            
*  Date			Revision	Author
*  $Date$ $Revision$ $Author$
*                                                                   
*********************************************************************/
*	
      SUBROUTINE MPS2GHA (ASFT, GHA, UT1, IT)
      character*100 SccsFileID
     -/'@(#)mps2gha.for	5.1 98/01/08 APS/ASF\0'/

c      IMPLICIT NONE
      CHARACTER*21 ASFT
c
      REAL*8 PMV(6), PMVA(6), ETIME, UT1, GHA
      REAL*16 XYUNIT(2), XN16
      INTEGER IT, IER, J
      INTEGER IPM, IEQNOX
      INTEGER   tc_asf2et
      EXTERNAL  tc_asf2et 

      IER = tc_asf2et(ASFT//char(0),ETIME)  

C--   IF(IER.NE.0) 						Lisa 1/96
      IF(IER.NE.1) 
     ?WRITE(*,*)' MPS2GHA:  ERROR IN ASF TIME FORMAT IN MPS2GHA.'
C--   IF(IER.NE.0) CALL GCBOMB					Lisa 1/96
      IF(IER.NE.1) CALL GCBOMB
      IF(IT.GT.0)WRITE(*,*) ' $$MPS2GHA:',ASFT,ETIME,IT
      CALL ET2UT1(ETIME,UT1)
      IF(IT.NE.0)WRITE(*,*)'  MPS2GHA:  UT1 = ', UT1
C---	GET EARTH PRIME MERIDIAN VECTOR AT ETIME IN 114 COORD.
C---	TRUE EQUATOR PRIME MERIDIAN OF DATE CARTESIAN. 
      PMV(1) = 1.0
      PMV(2) = 0.0
      PMV(3) = 0.0
      IF(IT.GT.0)WRITE(*,*) 'MPS2GHA:',(PMV(J),J=1,3)
C---	TRANSFORM TO TRUE EQUATOR EQUINOX OF DATE CARTESIAN. 
      IPM = 114
      IEQNOX = 112
      CALL ETRANS(PMVA,IEQNOX,PMV,IPM,ETIME)
      XN16 = PMVA(1)**2 + PMVA(2)**2  
      XN16 = SQRT(XN16)
CCC      WRITE(*,*)' XYNORM**2 XN16=',XN16
C---	MAKING A UNIT VECTOR IN THE XY PLANE USING REAL*16.
      XYUNIT(1) = PMVA(1) / XN16
      XYUNIT(2) = PMVA(2) / XN16
CCC      WRITE(*,*) 'XYUNIT=',XYUNIT
C---	XYUNIT IS A UNIT VECTOR IN THE XY PLANE IN THE MERIDIAN DIRECTION.  
C---	XYUNIT(1) = COS(GHA), XYUNIT(2) = SIN(GHA)
C---	USE ATAN2D FOR BEST ACCURACY:  
      GHA = ATAN2D(XYUNIT(2),XYUNIT(1)) 
      IF(GHA .LT. 0.0D0) GHA = GHA + 360.0D0
 9999 CONTINUE
      IF(IT.GT.0)WRITE(*,*) '@@MPS2GHA:',ETIME,GHA,(PMVA(J),J=1,3)
      RETURN
      END
