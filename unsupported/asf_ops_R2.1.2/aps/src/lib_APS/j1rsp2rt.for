C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	j1rsp2rt.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================

********************************************************************
*  Name:	J1RSP2RT.FOR
*  Module Type: SUBROUTINE	Language: FORTRAN 
*  Purpose:	GIVEN A DATE AND J1 RSP PATH/ANGLE, COMPUTE THE REV NUMBER
*		AND TIME.  
*  Input Parameters:
*  Name         Type    Definition
*  DBPROC		INT*4	pointer to Sybase info.  
*  ASFTIME	CH*21	Time in ASF-SIS format
*  ET0		REAL*8	Time in Ephemeris time julian days
*  IFRWRD	INT	FLAG:  
*			= +1	LOOKING FROM THE INPUT TIME 24 HOURS FORWARD.
*			= -1	LOOKING FROM THE INPUT TIME 24 HOURS BACKWARD.
*  IRSP		INT	RSP PATH; BETWEEN 1 AND ICREVS.  
*  XANGL	REAL*8	RSP ANGLE; BETWEEN 0 AND 360.
*  Output Parameters:
*  IREV		INT	REV NUMBER CORRESPONDING TO THE RSP.
*  ASFTOUT	CH*21	ASF TIME CORRESPONDING TO THE RSP PATH/ANGLE.
*  IER		INT	ERROR CODE:
*			-3= INPUT ERROR; XANGL. SHOULD BE BETWEEN ) AND 360.
*			-2= INPUT ERROR; BAD ASF TIME.  
*			-1= INPUT ERROR; THE RSP IS NOT WITHIN [1-CYREVS]
*  			0 = NORMAL CONDITION
*  			1 = THE REV DOES NOT OCCUR WITHIN 1 DAY OF THE INPUT
*			    TIME.
*			2 = THE INPUT TIME DOES NOT FALL WITHIN A PHASE.
*			3 = NO RECORDS WERE FOUND IN THE PHASE RELATION.
*  Variables:	CYREVS = THE NUMBER OF REVS IN ONE REPEAT CYCLE.
*  Locals :
*  Externals :
*  Modification History:                                            
*  Date         Author  Summary
*  11/17/95     QS      Change include file path to APS_HOME:include/local
* 
*  $Date$ $Revision$ $Author$
*********************************************************************/
      SUBROUTINE J1RSP2RT(dbproc,
     ?                 ASFTIME,ET0,IFRWRD,IRSP,XANGL,IREV,ASFTOUT,IER)
      character*100 SccsFileID
     -/'@(#)j1rsp2rt.for	5.1 98/01/08 APS/ASF\0'/

      IMPLICIT NONE
      INTEGER*4 dbproc
      CHARACTER*22 ASFTIME, ASFTOUT
      INTEGER IRSP, IREV, IER
      INTEGER ICDAYS, ICREVS, IPFIRSTREV, IPDAYS, IPREVS, IPLASTREV
C---       for tc_asf2et and tc_et2asf
      INCLUDE 'APS_HOME:include/local/timeconv.inc'
      CHARACTER*21 PSTARTASF, ASFT
      REAL*8 P0LONG		
      REAL*8 ET0	
      REAL*8 ET
      REAL*8 PHLONG
      REAL*8 ET1
      REAL*8 xdum
      INTEGER jdum
      INTEGER NERR, NRECS, NREVS, JCREV, IRSP1, IREVTRY
      INTEGER IRSPTRY, IFRWRD, ITRY1, ITRY2
      REAL*8 ETP1, ETP2, TNODE, ETDIF, XLDELTA, XANGL, ETOUT, ETNODE
      character*2 phase_name

      external j1_get_phase_t !$pragma C(j1_get_phase_t)

CC    WRITE(*,*)' J1RSP2RT:  ASFTIME  =', ASFTIME
CC    WRITE(*,*)'             ET0     =', ET0
CC    WRITE(*,*)'             IFRWRD  =', IFRWRD
CC    WRITE(*,*)'             IRSP    =', IRSP
CC    WRITE(*,*)'             XANGL   =', XANGL
CC    WRITE(*,1001)dbproc
CC1001 format('         dbproc = ', Z8 )
      IREV = 0
      ASFT = ASFTIME
      ET = ET0
      ASFTOUT = '                     '
      IF (ET0 .GT. 0.0D0) NERR = tc_et2asf(%VAL(ET0),ASFT)
      NERR = 0
      IF (ET0 .LE. 0.0D0) NERR = tc_asf2et(ASFT,ET)
CC    print *,'after tc_asf2et:  ET = ', ET, '   NERR = ', NERR
C---	CHECK FOR ERROR IN THE INPUT TIME.  
      IF(NERR .NE. 1) GO TO 8000
      IF(IRSP .LE. 0) THEN
C---	INPUT RSP IS IN ERROR.  
		  IER = -1
		  GO TO 9999
      ENDIF
      IF(XANGL .LT. 0.0D0 .OR. XANGL .GE. 360.0D0) THEN
C---	INPUT XANGL IS IN ERROR.  
		IER = -3
		GO TO 9999
      ENDIF
      NRECS = 0
C
C---	OBTAIN DATA FROM THE PHASE RELATION.  
	  ET1 = ET
CC print *,'J1RSP2RT.FOR:  ET1 = ', ET1
      call j1_get_phase_t(%VAL(dbproc), ET1,
     ?               phase_name,
     ?               PSTARTASF,
     ?               PHLONG,
     ?               IPDAYS ,
     ?               IPREVS,
     ?               IPLASTREV ,
     ?               ICDAYS ,
     ?               ICREVS ,
     ?               xdum,
     ?               xdum,
     ?               xdum,
     ?               xdum,
     ?               P0LONG,
     ?				 jdum,
     ?				 jdum,
     ?				 jdum,
     ?               IER  )
	  if(IER .NE. 0) GO TO 8001

C---	O.K.  the phase data was found. 
C---    GET TIME OF END PHASE.
C---    START OF PHASE (ETP1)
      IER = tc_asf2et(PSTARTASF,ETP1)
      IF(IER .NE. 1) GO TO 6001
C---    ETP1 IS O.K.
CC    print *,'j1rsp2rt.for:  ETP1 = ', ETP1
C---    NOW CHECK TO SEE IF INPUT RSP IS IN ERROR:
      IF(IRSP .GT.ICREVS) THEN
C---    IRSP IS IN ERROR.
		IER = -1
		GO TO 9999
      ENDIF
C--- COMPUTE NODAL PERIOD.  (TNODE)
C---    FORCE CONVERSION TO FLOATING DIVISION.
      TNODE = ICDAYS
      TNODE = TNODE / ICREVS
CC    WRITE(*,*)' J1RSP2RT:  TNODE,ICDAYS,ICREVS=',TNODE,ICDAYS,ICREVS
C--- COMPUTE LENGTH OF PHASE FROM IPREVS AND ADD TO THE PHASE START.
C---    END OF PHASE (ETP2)
      ETP2 = ETP1 + (TNODE * IPREVS)
CC    print *,'j1rsp2rt.for:  ETP2 = ', ETP2

C---	NOW COMPUTE THE FIRST REV NUMBER IN THIS PHASE.  (IPFIRSTREV)
      IPFIRSTREV = IPLASTREV - IPREVS + 1
CC    print *,'j1rsp2rt.for:  IPFIRSTREV = ', IPFIRSTREV
C---	NOW COMPUTE THE CURRENT REV NUMBER.  
C---	FIRST THE TIME SINCE THE START OF THE PHASE (ETDIF)
      ETDIF = ET - ETP1
CC    print *,'j1rsp2rt.for:  ETDIF = ', ETDIF
C---	NOW COMPUTE THE NUMBER OF COMPLETE REVS SINCE FIRST NODE OF PHASE.  
      NREVS = (ETDIF / TNODE)
CC    print *,'j1rsp2rt.for:  NREVS = ', NREVS
C---	NOW COMPUTE THE CURRENT REV NUMBER JCREV.
      JCREV = IPFIRSTREV + NREVS
CC    print *,'j1rsp2rt.for:  JCREV = ', JCREV
C---	NOW COMPUTE THE RSP FOR THE FIRST REV NUMBER IN THIS PHASE (IRSP1)
C---	THIS IS DONE BY COMPUTING THE LONGITUDE DIFFERENCE BETWEEN:
C---	1) THE 0th (OR CYREVth) PATH ASCENDING NODE LONGITUDE
C---	2) THE ASCENDING NODE LONGITUDE OF THE FIRST NODE OF THE PHASE.
C---	EACH RSP PATH IS WEST OF THE PREVIOUS ONE BY A LONGITUDE OF 
C---	(360 DEGREES)/CYREVS.  THEY ARE NUMBERED CONSECUTIVELY IN THIS WAY.
      XLDELTA = DMOD(P0LONG - PHLONG,360.0D0)
      IF(XLDELTA .LE.   0.0D0) XLDELTA = XLDELTA + 360.0D0
CCC      WRITE(*,*)'     XLDELTA = ', XLDELTA
C---	SO THE RSP CORRESPONDING TO THE FIRST PHASE REV IS THE TOTAL DELTA
C---	LONGITUDE DIVIDED BY THE LONGITUDE DELTA BETWEEN NEIGHBORING PATHS.  
C---	THE 0.4999... IS BECAUSE WE WANT ROUNDOFF.  
      IRSP1 = ( ( XLDELTA / (360.0D0/ICREVS)  ) + 0.499999999D0 )
CC    WRITE(*,*)'     IRSP1 = ', IRSP1
C---	NOW COMPUTE THE REV NUMBER FOR THE INPUT RSP FOR THIS DATE.
C
C---	WHAT FOLLOWS IS TRIAL AND ERROR.  WE JUST TAKE THE NEIGHBORING REV 
C---	NUMBERS, (+ OR - 1 DAY) CALCULATE THE RSP, AND COMPARE TO THE INPUT 
C---	RSP.  
C---	IF WE GET A MATCH, THEN WE USE THE REV NUMBER AS THE RESULT.  
C
      IF(IFRWRD .EQ. 1) THEN
C---	SEARCHING FROM THE CURRENT REV FORWARD.
		ITRY1 = JCREV
		ITRY2 = JCREV + (ICREVS / ICDAYS) + 1
      ELSE
C---	SEARCHING FROM THE CURRENT REV BACKWARD.
		ITRY2 = JCREV
		ITRY1 = JCREV - (ICREVS / ICDAYS) - 1
      ENDIF
CC    WRITE(*,*)' J1RSP2RT:   IRTY1 = ', ITRY1, '  ITRY2 = ', ITRY2
      DO 4000 IREVTRY = ITRY1, ITRY2
		IF(IREVTRY .LT. IPFIRSTREV .OR. IREVTRY .GT. IPLASTREV) 
     ?  	GO TO 4000
CC      WRITE(*,*)'     IREVTRY = ', IREVTRY
C--- 	COMPUTE RSP FOR THE REV TRY:  
C---	NOTE THAT IF YOU HAVE A REV/RSP PAIR, THEN THE RSP FOR REV+1 IS
C---	JUST REV + ICDAYS.  BUT WHEN YOU GET TO ICREVS+1, IT IS RESET TO 1.  
		IRSPTRY = MOD( ((IREVTRY-IPFIRSTREV)*ICDAYS + IRSP1),ICREVS)
C---	IF THE MODULO IS 0, THEN WE HAVE RSP = ICREVS, NOT RSP = 0.  
		IF(IRSPTRY .LE. 0) IRSPTRY = IRSPTRY + ICREVS
C---	CHECK IF WE ARE DONE:
CC      WRITE(*,*)'     IRSPTRY = ', IRSPTRY
		IF(IRSPTRY .EQ. IRSP) THEN
C---		FOUND THE DESIRED REV NUMBER.
			IREV = IREVTRY
C---		NOW COMPUTE THE TIME OF THE NODE.  
			ETNODE = ETP1 + (IREV - IPFIRSTREV)*TNODE
C---		NOW ADD IN THE FRACTION OF THE NODAL PERIOD IMPLIED BY XANGL.
C---		ASSUMING THE SIMPLE CASE OF A CIRCULAR ORBIT, CONSTANT ANGULAR 
C---		MOTION.  
			ETOUT = ETNODE + (XANGL/360.0D0) * TNODE
C---		NOW CONVERT TO ASFTIME FORMAT
			IER = tc_et2asf(%VAL(ETOUT),ASFTOUT)
C---		NOW WE ARE FINISHED.
			IER = 0
			GO TO 9999
		ENDIF
C---	NOT DONE YET.  
C---	END OF DO 4000
 4000 CONTINUE
      WRITE(*,*)'     INPUT RSP NOT FOUND'
C
C---	ERROR.  INPUT RSP NOT FOUND WITHIN ONE DAY OF THE INPUT TIME.  
      IER = 1
C---	NOW FINISHED.  
      GO TO 9999
 5000 CONTINUE
C---	ET IS AFTER THE CURRENT PHASE.  WE MAY BE IN A GAP.  
C---	READ NEXT PHASE RECORD.
      GO TO 9999
 6000 CONTINUE
C---	ET IS NOT WITHIN A PHASE.
      IER = 2
      GO TO 9999
 6001 CONTINUE
C---    ILLEGAL ASFTIME IN THE PHASE RELATION,
      WRITE(*,*)' J1RSP2RT:  ILLEGAL TIME IN phase.phase_start'
	  WRITE(*,*)'            phase.SAT = J1'
	  WRITE(*,*)'            phase.phase_start = ', PSTARTASF
	  CALL GCBOMB
 8000 CONTINUE
C---	ERROR IN THE ASF TIME SYNTAX.  
      IER = -2
      GO TO 9999
 8001 CONTINUE
C---	NO PHASE RECORDS WERE FOUND FOR THE IMPUT TIME.  
      IER = 2
      GO TO 9999
 8004 CONTINUE
C---	NO RECORDS WERE FOUND.  
      IER = 3
 9999 CONTINUE
CC    WRITE(*,*)' $J1RSP2RT:  IREV    = ', IREV
CC    WRITE(*,*)' $J1RSP2RT:  ASFTOUT = ', ASFTOUT
CC    WRITE(*,*)' $J1RSP2RT:  IER     = ', IER
      RETURN
      END
