C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	time2rev.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================

********************************************************************
*  Name:	TIME2REV
*  Module Type: SUBROUTINE	Language: EQUEL FORTRAN
*  $Logfile:   ACS003:[BLD.MPS.LIB.SRC]TIME2REV.QFV  $
*  Purpose:	GIVEN A SAT AND TIME, COMPUTES REV#.
*  Functions called:
*  Input Parameters:
*  Name         Type    Definition
*  dbproc	*DBPROCESS pointer to info about the database.  
*  SAT		CH*2	SATELLITE NAME:  E1, J1, OR RS
*  ET0		REAL*8	EPHEMERIS TIME IN REAL JULIAN DAYS
*			IF THE VALUE IS <= 0.0, THIS ROUTINE COMPUTES 
*			EPHEMERIS TIME FROM THE GIVEN ASF TIME.  
*  ASFTIME	CH*21	ASFTIME:  yyyy:ddd:hh:mm:ss.sss
*			IF THE VALUE OF ET0 IS > 0.0, THIS ROUTINE COMPUTES 
*			ASF TIME FROM THE GIVEN ET0 EPHEMERIS TIME.  
*  Output Parameters:
*  IREV1	INT	WHEN IER = 0, 2, AND 3, IREV1 = REV NUMBER OF THE 
*			LATEST NODE BEFORE OR EQUAL TO THE INPUT TIME.  
*			IREV1=0 OTHERWISE.
*  IREV2	INT	WHEN IER = 0, 1, AND 2, IREV2 = REV NUMBER OF THE FIRST 
*			NODE AFTER THE INPUT TIME.  IREV2=0 OTHERWISE.
*  IER		INT	CONDITION CODE:
*			=-1	THE INPUT ASFTIME WAS ILLEGAL.  
*  			= 0	NORMAL CONDITION.
*			= 1	THE TIME WAS BEFORE ALL NODES.
*			= 2	THE TIME WAS BETWEEN 2 NODES SEPARATED BY A GAP
*		 		IN TIME LARGER THAN ONE NODAL PERIOD.
*			= 3	THE TIME WAS AFTER ALL NODES.
*			= 4	NO RECORDS WERE FOUND IN THE PHASE RELATION 
*				FOR THIS SATELLITE.
*			
*  Variables:
*  Locals :
*  Externals :
*  Modification History:                                            
*  Date			Revision	Author
*  18 Jul 1990 17:00:24  2.0  DBMAN  
*  $Date$ $Revision$ $Author$
*  Date         Author  Summary
*  11/17/95     QS      Change include file path to include/local
*                                                                   
*********************************************************************/
      SUBROUTINE TIME2REV(dbproc,SAT,ET0,ASFTIME,IREV1,IREV2,IER)
      character*100 SccsFileID
     -/'@(#)time2rev.for	5.1 98/01/08 APS/ASF\0'/

      IMPLICIT NONE
c--port--##    DECLARE FORMS
C---      for tc_et2asf, tc_asf2et
      INCLUDE 'APS_HOME:include/local/timeconv.inc'
      integer*4 dbproc
      CHARACTER *(*) SAT, ASFTIME
      REAL*8 ET0
      INTEGER IREV1, IREV2, IER
      CHARACTER*22 ASFT, PHASE_START
      INTEGER NRECS, PHASE_DAYS, PHASE_ORBITS, CYCLE_DAYS, 
     ?                                        CYCLE_REVS, LAST_REV
      INTEGER NERR, INEXT, IPREV, NREVS
      REAL*8 ETP1, TNODE, ETP2, ETDIF
      REAL*8 ET, xdum
      character *2 chardum
      character *1 fphase_name
      real*8 phase_lon, orb_a, orb_e, orb_i
      external get_phase_t !$pragma C(get_phase_t)

c----      WRITE(*,*)' @TIME2REV:  SAT = ', SAT(1:2)
c----      WRITE(*,*)'  TIME2REV:  ET0 = ', ET0
c----      WRITE(*,*)'  TIME2REV:  ASFTIME = ', ASFTIME(1:21)

      IREV1 = 0
      IREV2 = 0
      ASFT = ASFTIME(1:21)
      ET = ET0
      NERR = 0
      IF (ET0 .GT. 0.0D0) NERR = tc_et2asf(%VAL(ET0),ASFT)
      IF (ET0 .LE. 0.0D0) NERR = tc_asf2et(ASFT,ET)
C---	CHECK FOR ERROR IN THE INPUT TIME.  
      IF(NERR .NE. 1) THEN
		 IER = -1
         return
      ENDIF
      INEXT = 0
      IPREV = 0
      NRECS = 0
      call get_phase_t(%VAL(dbproc), SAT, ET, 
     ?	             fphase_name,
     ?               PHASE_START,
     ?               PHASE_LON  ,
     ?               PHASE_DAYS ,
     ?               PHASE_ORBITS,
     ?               LAST_REV   ,
     ?               CYCLE_DAYS ,
     ?               CYCLE_REVS ,
     ?               ORB_A      ,
     ?               ORB_E      ,
     ?               ORB_I	,
     ?               xdum, 
     ?               chardum, 
     ?               IER  )

C     IREV1:	WHEN IER = 2, AND 3, IREV1 = REV NUMBER OF THE 
C			LATEST NODE BEFORE OR EQUAL TO THE INPUT TIME.  
C			IREV1=0 OTHERWISE.
      IF(IER .EQ. 3 ) IREV1 = LAST_REV
C     IREV2	WHEN IER = 1, AND 2, IREV2 = REV NUMBER OF THE FIRST 
C			NODE AFTER THE INPUT TIME.  IREV2=0 OTHERWISE.
      IF(IER .EQ. 1 .OR. IER .EQ. 2) IREV2 = LAST_REV - PHASE_ORBITS+1


      IF(IER .EQ. 2) THEN
C---		need to retrieve the phase previous to the one just retrieved. 
C---		subtract 15 days from the current time and then call again.  
C---		then use the last rev from the previous phase for IREV1.  
          ET = ET - 15.0
          call get_phase_t(%VAL(dbproc), SAT, ET, 
     ?	             fphase_name,
     ?               PHASE_START,
     ?               PHASE_LON  ,
     ?               PHASE_DAYS ,
     ?               PHASE_ORBITS,
     ?               LAST_REV   ,
     ?               CYCLE_DAYS ,
     ?               CYCLE_REVS ,
     ?               ORB_A      ,
     ?               ORB_E      ,
     ?               ORB_I	,
     ?               xdum, 
     ?               chardum, 
     ?               IER  )
          IREV1 = LAST_REV
          IER = 2
      ENDIF

      IF(IER .NE. 0) RETURN 

c--port--##  RETRIEVE (
c--port--##	PHASE_START	= phase.phase_start,
c--port--##	PHASE_DAYS		= phase.phase_days,
c--port--##	PHASE_ORBITS		= phase.phase_orbits,
c--port--##	LAST_REV	= phase.last_rev,
c--port--##	CYCLE_DAYS		= phase.cycle_days,
c--port--##	CYCLE_REVS		= phase.cycle_revs  )
c--port--##	WHERE  phase.#sat = SAT
c--port--##	SORT BY #PHASE_START
c--port--## {

C---	THE INPUT TIME IS WITHIN THIS PHASE.  
C---	GET TIME OF END PHASE.  
C---	START OF PHASE (ETP1)
      NERR = tc_asf2et(PHASE_START,ETP1)
C---	ETP1 IS O.K.
C--- COMPUTE NODAL PERIOD.  (TNODE)
      TNODE = CYCLE_DAYS 
      TNODE = TNODE / CYCLE_REVS
c---      WRITE(*,*)' TIME2REV:  TNODE,CYCLE_DAYS,CYCLE_REVS=',
c---     ?TNODE,CYCLE_DAYS,CYCLE_REVS
C--- COMPUTE LENGTH OF PHASE FROM PHASE_ORBITS AND ADD TO THE PHASE START.
C---	END OF PHASE (ETP2)
      ETP2 = ETP1 + (TNODE * PHASE_ORBITS)
C---	O.K.  ET IS WITHIN THIS PHASE.  
      ETDIF = ET - ETP1
C---	NUMBER OF COMPLETE REVS SINCE FIRST NODE OF PHASE.  
      NREVS = (ETDIF / TNODE)
      IREV1 = (LAST_REV - PHASE_ORBITS + 1) + NREVS
      IREV2 = IREV1 + 1
C---	NOW FINISHED, IN THE NORMAL CASE.  
c---      WRITE(*,*)' $TIME2REV:  IREV1 = ', IREV1
c---      WRITE(*,*)' $TIME2REV:  IREV2 = ', IREV2
c---      WRITE(*,*)' $TIME2REV:  IER = ', IER
      RETURN
      END
