C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	chkphase.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================

****************************************************************************
*  NAME: CHKPHASE
*  MODULE TYPE: SUBROUTINE 	LANGUAGE: EQUEL FORT77
*  $LOGFILE$
*  PURPOSE:  TO IDENTIFY AND COUNT ERRORS IN THE PHASE RELATION.  
*  INPUT PARAMETERS:
*  Name         Type    Definition
*  SAT		CH*2	Satellite
*  PHASE_NAME	CH*1	Name of the phase
*  OUTPUT PARAMETERS
*  IER		INT	NUMBER OF ERRORS IDENTIFIED.  
*  Externals : v_CHKPHASE
*  Modification History:                                            
*  $DATE$      $REVISION$      $AUTHOR$
*********************************************************************/
      SUBROUTINE CHKPHASE(SAT,PHASE_NAME, PHASE_START,
     ?               PHASE_LON  ,
     ?               PHASE_DAYS ,
     ?               PHASE_ORBITS,
     ?               LAST_REV   ,
     ?               CYCLE_DAYS ,
     ?               CYCLE_REVS ,
     ?               ORB_A      ,
     ?               ORB_E      ,
     ?               ORB_I      ,
     ?               ORB_ARG_PERI, 
     ?             IER)

      character*100 SccsFileID
     -/'@(#)chkphase.for	5.1 98/01/08 APS/ASF\0'/

      IMPLICIT NONE
C---    this INCLUDE is for tc_asf2et
      INCLUDE 'APS_HOME:include/local/timeconv.inc'
      CHARACTER*(*) SAT, PHASE_NAME, PHASE_START
      real*8 PHASE_LON, ET
      integer PHASE_DAYS, PHASE_ORBITS, LAST_REV, CYCLE_DAYS,CYCLE_REVS
      real*8 ORB_A, ORB_E, ORB_I, ORB_ARG_PERI

      INTEGER IER, ICOUNT
      IER = 0

C---    NOW REVIEW THE VALUES:
      ICOUNT = 0
      IER = tc_asf2et(PHASE_START,ET)
      IF(IER .NE. 1) THEN
        print *, 'chkphase.for:  ERROR IN PHASE START TIME SYNTAX.'
        ICOUNT = ICOUNT + 1
      ENDIF
      IF(PHASE_DAYS .LE. 0) THEN
	print *,'chkphase.for:  ERROR:  PHASE LENGTH SHOULD BE > 0 DAYS.'
         ICOUNT = ICOUNT + 1
      ENDIF
      IF(PHASE_ORBITS .LE. 0) THEN
	print *,'chkphase.for:  ERROR:  NUMBER OF COMPLETE ORBITS SHOULD BE > 0.'
         ICOUNT = ICOUNT + 1
      ENDIF
      IF(LAST_REV .LE. 0) THEN
	print *,'ERROR:  LAST REV# SHOULD BE > 0.  '
         ICOUNT = ICOUNT + 1
      ENDIF
      IF(CYCLE_DAYS .LE. 0) THEN
	print *,'chkphase.for:  ERROR:  REPEAT CYCLE SHOULD BE > 0 DAYS.  '
         ICOUNT = ICOUNT + 1
      ENDIF
      IF(CYCLE_REVS .LE. 0) THEN
	print *,'chkphase.for:  ERROR:  REPEAT CYCLE SHOULD BE > 0 REVS.  '
         ICOUNT = ICOUNT + 1
      ENDIF
      IF(ORB_A .LT. 6800.0D0 ) THEN
	print *,'chkphase.for:  ERROR:  SEMI-MAJOR AXIS VALUE IS TOO SMALL.  '
         ICOUNT = ICOUNT + 1
      ENDIF
      IF(ORB_A .GT. 7800.0D0) THEN
	print *,'chkphase.for:  ERROR:  SEMI-MAJOR AXIS VALUE IS TOO LARGE.  '
         ICOUNT = ICOUNT + 1
      ENDIF
      IF(ORB_E .GT. 0.002) THEN
	print *,'chkphase.for:  ERROR:  ECCENTRICITY VALUE IS TOO LARGE.  '
         ICOUNT = ICOUNT + 1
      ENDIF
      IF(ORB_E .LT. 0.0D0) THEN
	print *,'chkphase.for:  ERROR:  ECCENTRICITY VALUE IS < 0.  '
         ICOUNT = ICOUNT + 1
      ENDIF
      IF(ORB_I .GT. 105.0D0) THEN
	print *,'chkphase.for:  ERROR:  INCLINATION VALUE IS TOO LARGE.  '
         ICOUNT = ICOUNT + 1
      ENDIF
      IF(ORB_I .LT. 95.0D0) THEN
	print *,'chkphase.for:  ERROR:  INCLINATION VALUE IS TOO SMALL.  '
         ICOUNT = ICOUNT + 1
      ENDIF
      IF(PHASE_LON .LT. -180.0D0) THEN
	print *,'chkphase.for:  ERROR:  SUBSAT LONGITUDE < -180.  = ', PHASE_LON
         ICOUNT = ICOUNT + 1
      ENDIF
      IF(PHASE_LON .GT. 360.0D0 ) THEN
	print *,'chkphase.for:  ERROR:  SUBSAT LONGITUDE > 360.  = ', PHASE_LON
         ICOUNT = ICOUNT + 1
      ENDIF
      IER = ICOUNT
      return
      end
