C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	rev2time.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================

********************************************************************
*  Name:	REV2TIME
*  Module Type: SUBROUTINE	Language: FORTRAN
*  Purpose:	GIVEN A SAT AND REV, COMPUTES start and end time.
*  Functions called:
*  get_phase_r.c
*  Input Parameters:
*  Name         Type    Definition
*  dbproc	*DBPROCESS pointer to info about the database.  
*  SAT		CH*2	SATELLITE NAME:  E1, J1, OR RS
*  REV		INT	rev number
*  Output Parameters:
*  ET1          REAL*8  EPHEMERIS TIME IN REAL JULIAN DAYS OF THE START OF THE 
*			REV.  
*  ASF1   	CH*22	ASFTIME:  yyyy:ddd:hh:mm:ss.sss
*			equivalent to ET1.  
*  ET2          REAL*8  EPHEMERIS TIME IN REAL JULIAN DAYS OF THE END OF THE 
*			REV.  This is computed as .99 milliseconds less than 
*			the start of the next rev, so these times are within
*			this rev, uniquely.  
*  ASF2   	CH*22	ASFTIME:  yyyy:ddd:hh:mm:ss.sss
*			equivalent to ET2.  
*  IER		INT	CONDITION CODE:
*  			= 0	NORMAL CONDITION.
*			= 1	THE REV WAS BEFORE ALL PHASES.
*			= 2	THE REV WAS BETWEEN 2 PHASES 
*			= 3	THE TIME WAS AFTER ALL PHASES.
*			= 4	NO RECORDS WERE FOUND IN THE PHASE RELATION 
*				FOR THIS SATELLITE.
*			
*  Variables:
*  Locals :
*  Externals :
*  Modification History:                                            
*  Date         Author  Summary
*  11/17/95     QS      Change include file path to include/local
*
*  18 Jul 1990 17:00:24  2.0  DBMAN  
*  $Date$ $Revision$ $Author$
*                                                                   
*********************************************************************/
      SUBROUTINE REV2TIME(dbproc,SAT,REV, ET1,ASF1,ET2,ASF2,IER)
      character*100 SccsFileID
     -/'@(#)rev2time.for	5.1 98/01/08 APS/ASF\0'/

      IMPLICIT NONE
c--port--##    DECLARE FORMS
C---       for tc_asf2et, tc_et2asf
      INCLUDE 'APS_HOME:include/local/timeconv.inc'
      integer*4 dbproc
      INTEGER REV
      CHARACTER *(*) SAT
      REAL*8 ET1, ET2
      INTEGER IER
      CHARACTER*22 ASF1, ASF2, PHASE_START
      INTEGER NRECS, PHASE_DAYS, PHASE_ORBITS, CYCLE_DAYS, 
     ?                                        CYCLE_REVS, LAST_REV
      INTEGER NERR, INEXT, IPREV, NREVS, FIRST_REV
      REAL*8 TNODE, ETIME
      REAL*8 xdum
      character *1 fphase_name
      real*8 phase_lon, orb_a, orb_e, orb_i
      external get_phase_r !$pragma C(get_phase_r)

c---      WRITE(*,*)' @REV2TIME:  SAT = ', SAT(1:2)
c---      WRITE(*,*)'  REV2TIME:  REV = ', REV

      ET1 = 0.0
      ET2 = 0.0
      ASF1 = '                     ' // char(0)
      ASF2 = '                     ' // char(0)
      NERR = 0
      INEXT = 0
      IPREV = 0
      NRECS = 0
      call get_phase_r(%VAL(dbproc), SAT, REV, 
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
     ?               IER  )

c---	the return code IER is already set for the various errors.
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

C---	THE INPUT REV IS WITHIN THIS PHASE.  
C--- COMPUTE NODAL PERIOD.  (TNODE)
      TNODE = CYCLE_DAYS 
      TNODE = TNODE / CYCLE_REVS
C---      WRITE(*,*)' REV2TIME:  TNODE,CYCLE_DAYS,CYCLE_REVS=',
C---     ?TNODE,CYCLE_DAYS,CYCLE_REVS

C---	COMPUTE FIRST REV IN THE PHASE.
      FIRST_REV = LAST_REV - PHASE_ORBITS + 1

C---	COMPUTE NUMBER OF REVS FROM PHASE START TO START OF INPUT REV
      NREVS = REV - FIRST_REV

C---	GET ELAPSED TIME FROM PHASE START TO START OF INPUT REV.
      ETIME = NREVS * TNODE

C---	GET START TIME OF THE PHASE
      NERR = tc_asf2et(PHASE_START, ET1)

C---	COMPUTE START TIME OF INPUT REV BY ADDING ELAPSED TIME
      ET1 = ET1 + ETIME

C---	COMPUTE END OF REV AS REV START PLUS ONE NODAL PERIOD 
C---		MINUS .99 MILLISECONDS.
      ET2 = ET1 + TNODE - 0.990/24/3600/1000

C---	CONVERT TO ASF TIME AND RETURN THAT, TOO.  
      NERR = tc_et2asf(%VAL(ET1), ASF1)
      NERR = tc_et2asf(%VAL(ET2), ASF2)

C---	NOW FINISHED, IN THE NORMAL CASE.  
c---      WRITE(*,*)' $REV2TIME:  ET1 = ', ET1
c---      WRITE(*,*)' $REV2TIME:  ET2 = ', ET2
c---      WRITE(*,*)' $REV2TIME:  ASF1 = ', ASF1
c---      WRITE(*,*)' $REV2TIME:  ASF2 = ', ASF2
c---      WRITE(*,*)' $REV2TIME:  IER = ', IER
      RETURN
      END
