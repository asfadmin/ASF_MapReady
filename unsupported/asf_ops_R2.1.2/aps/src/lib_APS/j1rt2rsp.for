C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	j1rt2rsp.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================

********************************************************************
*  Name:	J1RT2RSP.FOR
*  Module Type: SUBROUTINE	Language: FORTRAN 
*  Purpose:	GIVEN A J-ERS-1 REV AND START/STOP TIMES, COMPUTE 
*		THE J1 RSP PATH AND START/STOP ANGLES AND TIMES.  
*  Input Parameters:
*  Name         Type    Definition
*  dbproc		int*4	pointer to sybase process info
*  IREV		INT	INPUT REV NUMBER 
*  ASFT1	CH*21	ASF FORMAT START TIME.  
*  ASFT2	CH*21	ASF FORMAT STOP TIME.  
*  Output Parameters:
*  IRSP		INT 	RSP PATH NUMBER CORRESPONDING TO IREV.
*  XANGL1	REAL*8	RSP ANGLE CORRESPONDING TO ASFT1
*  XANGL2	REAL*8	RSP ANGLE CORRESPONDING TO ASFT2
*  IER		INT	ERROR CODE:
*			-2= INPUT ERROR; BAD REV.
*			-1= INPUT ERROR; BAD ASF TIME.  
*  			0 = NORMAL CONDITION
*  			1 = 1ST INPUT TIME DOES NOT FALL WITHIN THE GIVEN REV.
*			2 = THE INPUT REV DOES NOT FALL WITHIN A PHASE.
*			3 = NO RECORDS WERE FOUND IN THE PHASE RELATION.
*  			11= 2ND INPUT TIME DOES NOT FALL WITHIN THE GIVEN REV.
*  Variables:	CYREVS = THE NUMBER OF REVS IN ONE REPEAT CYCLE.
*  Locals :
*  Externals :
*  Modification History:                                            
*  Date         Author  Summary
*  11/17/95     QS      Change include file path to APS_HOME:include/local
* 
*  $Date$ $Revision$ $Author$
**********************************************************************/
      SUBROUTINE J1RT2RSP(dbproc,
     ?		IREV,ASFT1,ASFT2, IRSP,XANGL1,XANGL2, IER)
      character*100 SccsFileID
     -/'@(#)j1rt2rsp.for	5.1 98/01/08 APS/ASF\0'/

      IMPLICIT NONE
C---     for tc_asf2et:
      INCLUDE 'APS_HOME:include/local/timeconv.inc'
      INTEGER*4 dbproc
      CHARACTER*22 ASFT1, ASFT2 
      INTEGER IRSP, IREV, IER, J_INT
      REAL*8 XANGL1, XANGL2
      INTEGER ICYDAYS, ICYREVS, IPFIRSTREV, IPDAYS, IPREVS, IPLASTREV
      CHARACTER*22 PSTARTASF
      REAL*8 P0LON, PHLON
      INTEGER NERR, NRECS, NREVS
      INTEGER N, jdum
      REAL*8 ETP1, TNODE, xdum
      REAL*8 ET1, ET2, ETR1, ETR2, XDLON, XLASC
	  integer rcode, system
	  character*2 phase_name
	  external j1_get_phase_t !$pragma C(j1_get_phase_t)

CC      WRITE(*,*)' J1RT2RSP:   dbproc  =', dbproc
CC      WRITE(*,*)'             IREV    =', IREV
CC      WRITE(*,*)'             ASFT1   =', ASFT1
CC      WRITE(*,*)'             ASFT2   =', ASFT2
      IRSP = 0
      XANGL1 = 0.0D0
      XANGL2 = 0.0D0
      IER = 0
      ET1 = 0.0D0
      ET2 = 0.0D0
      NERR = 0
      NERR = tc_asf2et(ASFT1,ET1)
C---	CHECK FOR ERROR IN THE INPUT TIME.  
      IF(NERR .NE. 1) THEN
		IER = -1
		GO TO 9999
      ENDIF
      IF(ASFT2 .NE. ' ') THEN
        NERR =  tc_asf2et(ASFT2,ET2)
C---	CHECK FOR ERROR IN THE INPUT TIME.  
        IF(NERR .NE. 1) THEN
          IER = -1
          GO TO 9999
        ENDIF
      ENDIF
      IF(IREV .LE. 0) THEN
C---	INPUT REV IS IN ERROR.  
		IER = -2
		GO TO 9999
      ENDIF
C
C---	OBTAIN DATA FROM THE PHASE RELATION.  
      NRECS = 0
c* old embedded QUEL code
c	##  RETRIEVE (
c	##	PSTARTASF	= phase.phase_start,
c	##	IPDAYS		= phase.phase_days,
c	##	IPREVS		= phase.phase_orbits,
c	##	IPLASTREV	= phase.last_rev,
c	##	ICYDAYS		= phase.cycle_days,
c	##	ICYREVS		= phase.cycle_revs,
c	##	PHLON		= phase.phase_lon,
c	##      P0LON		= phase.rsp_0_lon )
c	##	WHERE  phase.#sat = "J1"
c	##	SORT BY #PSTARTASF
c	## {
      call j1_get_phase_t(%VAL(dbproc), ET1,
     ?               phase_name,
     ?               PSTARTASF,
     ?               PHLON,
     ?               IPDAYS ,
     ?               IPREVS,
     ?               IPLASTREV ,
     ?               ICYDAYS ,
     ?               ICYREVS ,
     ?               xdum,
     ?               xdum,
     ?               xdum,
     ?               xdum,
     ?               P0LON,
     ?               jdum,
     ?               jdum,
     ?               jdum,
     ?               IER  )
      if(IER .NE. 0) GO TO 8001
C--		the correct phase record was retrieved.  
C---	CHECK TO SEE IF THE INPUT REV IS WITHIN THIS PHASE.  
c		it is.  
C---	NOW COMPUTE THE FIRST REV NUMBER IN THIS PHASE.  (IPFIRSTREV)
      IPFIRSTREV = IPLASTREV - IPREVS + 1
      IF( IREV .GT. IPLASTREV) GO TO 8002 
      IF( IREV .LT. IPFIRSTREV) GO TO 8002 
C---	O.K.  ET IS WITHIN THIS PHASE.  
C---	COMPUTE THE START TIME OF THE PHASE
C---	START OF PHASE (ETP1)
      NERR = tc_asf2et(PSTARTASF,ETP1)
      IF(NERR .NE. 1) GO TO 6001
C---	ETP1 IS O.K.
C---	COMPUTE NODAL PERIOD.  (TNODE) 
C---	FORCE CONVERSION TO FLOATING DIVISION.  
      TNODE = ICYDAYS 
      TNODE = TNODE / ICYREVS 
CC      WRITE(*,*)'    TNODE = ', TNODE
C---	NOW COMPUTE THE NUMBER OF COMPLETE REVS SINCE FIRST NODE OF PHASE.  
      NREVS = IREV - IPFIRSTREV 
CC      WRITE(*,*)'    NREVS = ', NREVS
C---	COMPUTE THE TIME OF THE START OF THE REV.  (ETR1)
      ETR1 = ETP1 + NREVS*TNODE
CC      WRITE(*,*)'  START OF REV = ', ETR1
CC      WRITE(*,*)'           ET1 = ', ET1
C---	COMPUTE THE TIME OF THE END OF THE REV.    (ETR2)
      ETR2 = ETP1 + (NREVS+1)*TNODE
C---	VERIFY THE FIRST INPUT TIME AS BEING WITHIN THIS REV.
CC      WRITE(*,*)'  ETR1 - ET1 = ', ETR1 - ET1 
CC      WRITE(*,*)'  ET1 - ETR2 = ', ET1 - ETR2 
C---	NOTE THAT THE SCREWEY 5.787D-09 DAYS IS ABUT 1/5 MILLISECOND.  
C---	ALL EPHEMERIS TIMES IN THIS ROUTINE ARE IN REAL JULIAN DAYS.  
C---	BUT THE INPUT TIMES ARE IN DISCREET MILLISECONDS.  WE WANTED 
C---	TO MAKE SURE THAT THE TIME AT AN ASCENDING NODE WAS PART OF THE 
C---	START OF THE REV AND NOT PART OF THE PREVIOUS REV.  ON TESTING 
C---	FOR ONE MILLISECOND BEFORE AND AFTER THE NODE, AND FOR THE PREVIOUS AND 
C---	CURRENT REV, THIS IF STATEMENT EVOLVED AND DOES GET THE DESIRED 
C---	VALUE FOR IER ON EACH INPUT OF ASFT1.  THE SAME IS FOR ASFT1
C---	BELOW.  

      IF(ETR1-ET1 .GT. 5.787D-09  .OR.  ET1-ETR2 .GT. -5.787D-09) THEN
			IER = 1
			GO TO 9999
      ENDIF

C---	COMPUTE THE FIRST OUTPUT ANGLE.  TO .0001 PRECISION.
C---	WAS GETTING MINUS ANGLES WHICH SHOULD BE 0.  .0001 IS THE LIMIT, 
C---	DUE TO THE LIMIT OF THE PRECISION ON TIME VALUES.  
      J_INT = 10000D0*(360.0D0 * (ET1 - ETR1)/TNODE) + 0.49999999D0
      XANGL1 = J_INT / 10000.0D0
CC      WRITE(*,*)'  XJINT=', XJ_INT, ' ', J_INT, ' ', XANGL1
CC      WRITE(*,*)'     XANGL1 = ', XANGL1
C---	VERIFY THE SECOND INPUT TIME AS BEING WITHIN THIS REV.
      IF(ET2 .GT. 0.0D0) THEN
		IF(ETR1-ET2 .GT. 5.787D-09 .OR. ET2-ETR2 .GT. -5.787D-09) THEN
			IER = 11
			GO TO 9999
		ENDIF
C---	    COMPUTE THE SECOND OUTPUT ANGLE.
		J_INT = 10000D0*(360.0D0 * (ET2 - ETR1)/TNODE)+0.49999999D0
		XANGL2 = J_INT / 10000.0D0
      ENDIF
C---	NOW DETERMINE IRSP FROM IREV.  
C---	FIRST COMPUTE THE LONGITUDE OF ASCENDING NODE FOR IREV:
C---	N=NUMBER OF REVS SINCE THE CURRENT REPEAT CYCLE BEGAN:
      N = MOD(IREV-IPFIRSTREV , ICYREVS)
CCC      WRITE(*,*)'     N=', N
C---	XDLON = WESTWARD DELTA LONGITUDE OF CURRENT ASCENDING NODE FROM 
C---	LONGITUDE AT FIRST NODE OF THE CURRENT REPEAT CYCLE.
      XDLON = MOD(N*ICYDAYS*(360.0D0/ICYREVS) , 360.0D0)
CCC      WRITE(*,*)'     XDLON = ', XDLON
C---	LONGITUDE AT NODE IREV = LONG AT START OF CYCLE MINUS(=WESTWARD) XDLON
      XLASC = MOD(PHLON - XDLON , 360.0D0)
CCC      WRITE(*,*)'     XLASC = ', XLASC
C---	XDLON = WESTWARD DELTA LONGITUDE OF CURRENT ASCENDING NODE FROM 
C---	LONGITUDE AT 0 RSP.  (SAME AS ICYREVS RSP)
      XDLON = MOD(P0LON-XLASC , 360.0D0)
      IF(XDLON .LT. 0.0D0) XDLON = XDLON + 360.0D0
CCC      WRITE(*,*)'  XDLON = ', XDLON
C---	COMPUTE THE NUMBER OF RSP PATHWIDTHS BETWEEN THE ASCENDING NODE 
C---	LONGITUDE AND THE LONGITUDE FOR RSP PATH 0 (SAME AS RSP FOR 
C---	PATH=ICYREVS).  MEASURE WESTWARD FROM P0LON
      IRSP = XDLON/(360.0D0/ICYREVS) + 0.4999999999999
CC      WRITE(*,*)'     IRSP = ', IRSP
C---	THE .49999 ETC IS BECAUSE WE WANT TO ROUND OFF THE COMPUTED NUMBER, 
C---	NOT TRUNCATE.  
C
C---	NOW FINISHED.  
C---	FINISHED WITH THE DATABASE.  
      GO TO 9999
 6001 CONTINUE
C---	ILLEGAL ASFTIME IN THE PHASE RELATION,  
      WRITE(*,*)' J1RT2RSP:  ILLEGAL TIME IN phase.phase_start'
      WRITE(*,*)'            phase.SAT = J1'
      WRITE(*,*)'            phase.phase_start = ', PSTARTASF
      rcode = system('banner ERROR')
      CALL GCBOMB
 8001 CONTINUE
      WRITE(*,*)' J1RT2RSP:  ERROR retrieving from phase relation.'
      WRITE(*,*)' return code from j1_get_phase.c = ', IER
      WRITE(*,*)' inputs to J1RT2RSP:'
      WRITE(*,*)'             IREV    =', IREV
      WRITE(*,*)'             ASFT1   =', ASFT1
      WRITE(*,*)'             ASFT2   =', ASFT2
      rcode = system('banner ERROR')
      CALL GCBOMB
 8002 CONTINUE
c---	IREV IS NOT WITHIN THE PHASE.  
      IER = 2
 9999 CONTINUE
CC      WRITE(*,*)' $J1RT2RSP:  IRSP    = ', IRSP
CC      WRITE(*,*)' $J1RT2RSP:  XANGL1 = ', XANGL1
CC      WRITE(*,*)' $J1RT2RSP:  XANGL2 = ', XANGL2
CC      WRITE(*,*)' $J1RT2RSP:  IER     = ', IER
      RETURN
      END
