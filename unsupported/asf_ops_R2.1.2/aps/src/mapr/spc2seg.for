C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:  spc2seg.for
C--
C--  Description:   
C-- 
C--  Notes:
C--
C-- ==========================================================================

C-----------------------------------------------------------------------
C
C SUBROUTINE SPC2SEG
C
C PURPOSE
C       SPECIFIC SITE COVERAGE RECORDS TO SEGMENTS PROCESSING
C       ALLOWS USER TO CREATE FILE OF SEGMENTS FROM SSCVRG OVERLAYS
C       FILE CAN THEN BE READ IN BY SEGM LOAD OPERATION
C
C $Logfile:   ACS003:[BLD.MPS.MAPR.SRC]SPC2SEG.QFV  $
C
C VARIABLES
C INPUT:
C       NSEG            NUMBER OF SEGMENTS CREATED
C       CRSEGN          OVERLAY STATUS ARRAY
C       CRSEGT          OVERLAY NAME ARRAY
C       CRDARID         OVERLAY DARID ARRAY
C
C INTERNAL:
C       OUTFILE         OUTPUT FILENAME
C       SEGNAME         OVERLAY NAME
C       TEX             OVERLAY NAME SUBSET
C       SAT,SEN         SAT/SENSOR ENTERED BY USER
C       SSAT,SSEN       SAT/SENSOR RETRIEVED FROM SSCVRG REL
C       ASCDSC          ASCENDING/DESCENDING FLAG FROM SSCVRG REL
C       STRTT,STOPT     START/STOP TIMES FROM SSCVRG REL
C       TEMP            TEMP ARRAY OF ALREADY WRITTEN OVERLAYS
C       I               COUNTER
C       DIGITS          COUNTER
C       DARID           DARID OF OVERLAY TO BE WRITTEN TO FILE
C       SEGNUM          NUMBER " "       "  "  "       "  "
C       SEGREV          REV   "  "       "  "  "       "  "
C       SREV            REV RETRIEVED FROM SSCVRG REL
C       ERRNO,RCOUNT    INGRES OPERATION FLAGS
C       IOS             INPUT/OUTPUT STATUS NUMBER
C       STRTL,STOPL     START/STOP LATS FROM SSCVRG REL
C       NRLAT1,NRLON1,FRLAT1,FRLON1,
C       NRLAT2,NRLON2,FRLAT2,FRLON2     LATS/LONS FROM SSCVRG REL
C       
C FUNCTIONS
C       SLEN            RETURNS LENGTH OF CHAR STRING
C
C WRITTEN BY CRAIG K. FUJIMOTO - SEP 89
C
C MODIFICATIONS
C $Date$ $Revision$ $Author$
C 7/25/94  Nadia Adhami -port to UNIX/C (INGRIS -> SYBASE)
C 3/23/95  Nadia Adhami Support for ERS-2 -> sat name is now 2 chars instead
C          Nadia Adhami of 1 char. So when extracting sat/sen/rev from segname
C          Nadia Adhami offsets should be adjusted. old code is commented.
C 3/23/95  Nadia Adhami add flag FILE_IS_OPEN to close segment file if open
C
C-----------------------------------------------------------------------

       SUBROUTINE SPC2SEG ( NSEG,CRSEGN,CRSEGT,CRDARID,
     1   LOAD2DB,p_userid,p_password )

      character*100 SccsFileID
     -/'@(#)spc2seg.for	5.1 98/01/08 APS/ASF\0'/

       IMPLICIT NONE

       INCLUDE 'mapr_db_extern.inc'
       INCLUDE 'APS_ROOT:include/local/f77_db_sscvrg.inc'

C INPUT:
       CHARACTER*(*) CRSEGT(*) 
       INTEGER NSEG,CRSEGN(*),CRDARID(*)
       LOGICAL LOAD2DB
       CHARACTER*100 p_userid,p_password 

C INTERNAL:
       EXTERNAL  f77_aps_fullpath !$pragma c(f77_aps_fullpath)


C---  Special declarations for the multi-user modification:
      INTEGER RCODE, PERMID, J
      COMMON /active_dar_activities_columns/
     ?            active_dar_activities_columns
      INTEGER*4 active_dar_activities_columns
      INTEGER*4 mu_permission_terminate
      EXTERNAL  mu_permission_terminate
     ?          !$pragma c(mu_permission_terminate)
      INTEGER*4 create_dyn_llist
      EXTERNAL  create_dyn_llist
     ?          !$pragma c(create_dyn_llist)
      EXTERNAL  db_print_list          !$pragma c(db_print_list)
      EXTERNAL db_print_planning_list !$pragma c(db_print_planning_list)
      EXTERNAL  mu_print_error_msg     !$pragma c(mu_print_error_msg)
      INTEGER*4 mu_permission_request
      EXTERNAL  mu_permission_request
     ?          !$pragma c(mu_permission_request)

C VARIABLES FOR GETTING PLANNING PERMISSION:
      INTEGER SELECT_PERMID
       CHARACTER*21 MIN_STRTTIME, MAX_STOPTIME
       CHARACTER*80 ANSWER
C---  End Special declarations for the multi-user modification:


       CHARACTER*57 SEGFILE
           INTEGER bufsize
       CHARACTER*200 OUTFILE
       CHARACTER*20 SEGNAME                       
       CHARACTER*10 TEX
       CHARACTER*2 SAT,SSAT
       CHARACTER*3 SEN,SSEN
       CHARACTER*1 ASCDSC
       CHARACTER*21 STRTT,STOPT

       INTEGER TEMP(200),I,DIGITS
       INTEGER DARID
       INTEGER SEGNUM,SEGREV,SREV
       INTEGER IOS
       INTEGER SEGCOUNT

       REAL STRTL,STOPL
       REAL NRLAT1,NRLON1,FRLAT1,FRLON1
       REAL NRLAT2,NRLON2,FRLAT2,FRLON2
      
       real nrlat(2),frlat(2),nrlon(2),frlon(2)
       integer SLEN
       integer LASTC
       integer NRECS
       double precision ddumy

       integer FILE_IS_OPEN
       integer status,system
       integer   LOCAL_DUMMY_VALUEHOLDER
       character*180 string
 
       INCLUDE 'APS_HOME:include/local/timeconv.inc'


C FUNCTIONS
C-----------------------------------------------------------------------

C initialize flag and initialize list of segments already written:  
      FILE_IS_OPEN = 0
      DO 1500 I = 1,NSEG
         TEMP(I) = 0
 1500 CONTINUE


C IF NO SEGMENTS EXIST, RETURN

      IF (NSEG .EQ. 0) THEN
         CALL DISMSG('No segments were created.')
         CALL DISMSG('A segment file is not created.')
         GOTO 9000
      END IF

C ASK FOR DARID
 1510 CONTINUE
	  SELECT_PERMID = 0
      CALL ASK_DARID(DARID)
      IF (DARID .EQ. 0) THEN
           CALL DISMSG('A segment file is not created.')
           GOTO 9000
      END IF

C VERIFY THAT THERE IS AT LEAST ONE SEGMENT TO OFFER
      DO 1550 J = 1,NSEG
 
C         DISPLAY IF ITS DARID MATCHES
C         AND IF IT HAS NOT BEEN REMOVED OR DELETED FROM MAPPER SESSION
C         AND IF IT HAS NOT BEEN WRITTEN TO THE FILE PREVIOUSLY
 
         IF (CRDARID(J).EQ.DARID .AND. CRDARID(J).NE.0 
     ?       .AND. CRSEGN(J).GT.0 ) SEGCOUNT = SEGCOUNT + 1
 1550 CONTINUE
C     IF NO SEGMENTS TO BE CHOSEN, SKIP ALL OF THE FOLLOWING.  
      IF( SEGCOUNT .EQ. 0 ) THEN
           WRITE(*,*) 'NO SEGMENTS AVAILABLE FOR THAT DARID'
           GO TO 1510
      ENDIF
C     YES.  THERE ARE SEGMENTS TO SELECT.  NOW GET PERMISSION IF 
C     LOADING DTKS 2 DB.  .  

      IF (LOAD2DB) THEN

          llistptr = create_dyn_llist()
          RCODE = mu_permission_request(%VAL(MAPR_DBPROC),
     ?        %VAL(0),
     ?        'SelectSiteCovForDAR' // char(0),
     ?        'DAR' // char(0),
     ?        %VAL(llistptr),
     ?        %VAL(0), %VAL(0), %VAL(0),
     ?        %VAL(DARID) )

          IF( RCODE .LT. 0 ) THEN
              CALL mu_print_error_msg(%VAL(RCODE))
              WRITE(*,*)'PERMISSION ERROR OCCURRED.'
              GO TO 1510
          ENDIF
 
          IF ( RCODE .EQ. 0 ) THEN
C             PERMISSION DENIED.  PRINT BLOCKING
C             ACTIVITIES AND LET USER TRY AGAIN
              WRITE(*,*)'Permission DENIED.'
              WRITE(*,*)'Blocking activities:'
              CALL db_print_list( %VAL(llistptr),
     ?              active_dar_activities_columns )
C             PERMISSIONS PRINTED.  FREE THE LLIST
              CALL db_ftn_free_llist(%VAL(llistptr) )
              GO TO 1510
          ENDIF
 
C         PERMISSION GRANTED.  FREE THE LLIST
          CALL db_ftn_free_llist(%VAL(llistptr) )
          SELECT_PERMID = RCODE

      ENDIF

C ASK FOR FILENAME
 1600 CONTINUE
      CALL ASK_FNAME(SEGFILE,1)
C--   0 is used to quit:
      IF( SEGFILE(1:1) .EQ. '0') GO TO 9000

C ADD DIRECTORY PATH APS_TEMP TO INPUT FILE NAME: 
      bufsize = 200
      CALL f77_aps_fullpath('APS_TEMP', SEGFILE, OUTFILE, bufsize)
      IF (bufsize .EQ. 0) THEN
           CALL DISMSG('Error forming output segment file name.')
           CALL DISMSG('A segment file is not created.')
           GO TO 9000
      ENDIF

C OPEN THE SEGMENT FILE FOR WRITING

      CALL DISMSG('Segfile name = ' // OUTFILE(1:LASTC(OUTFILE)) )
      OPEN(UNIT=10,FILE=OUTFILE(1:LASTC(OUTFILE)),
     ?         STATUS='NEW',IOSTAT=IOS)
      FILE_IS_OPEN = 1

      IF (IOS .NE. 0) THEN                        
           CALL DISMSG(
     ? 'Error opening this file for output; it might already exist.')
           CALL DISMSG( 'Try again or use 0 to quit:')
           GO TO 1600
      END IF

      MIN_STRTTIME = '9999'
      MAX_STOPTIME = '1111'

C--  START OF SEGMENT SELECTION LOOP; KEEP A COUNT OF PICKS
      SEGCOUNT = 0
 1000 CONTINUE         

      CALL SHOW_SEGS(NSEG,CRSEGN,CRSEGT,CRDARID,
     1                TEMP,DARID,SEGNUM)
 
      IF (SEGNUM .EQ. 0) THEN
           IF (FILE_IS_OPEN .EQ. 1) THEN
               GOTO 3000
           ELSE
               GOTO 9000
           END IF
      ELSE
          SEGCOUNT = SEGCOUNT + 1
      ENDIF
 
C EXTRACT THE SAT/SENSOR/REV INFORMATION
C
C--          SSC R1/SW2/12345A
C--          12345678901234567
      SEGNAME = CRSEGT(SEGNUM)

C SATELLITE INFO
C--          SSC R1/SW2/12345A
C--          12345678901234567
      SAT = SEGNAME(5:6)

C SENSOR INFO
C--          SSC R1/SW2/12345A
C--          12345678901234567
      SEN = SEGNAME(8:10)

C REV NUMBER
C--          SSC R1/SW2/12345A
C--          12345678901234567
      TEX = SEGNAME(12:17)

      DO 205 I = 1,7
         IF (TEX(I:I) .EQ. 'A' .OR. TEX(I:I) .EQ. 'D') THEN
           DIGITS = I - 1
           GO TO 206
         END IF
  205 CONTINUE
  206 CONTINUE
      IF (DIGITS .EQ. 0) THEN
           CALL DISMSG('Error: Program error (in segnam syntax).')
           GO TO 1000
      END IF

C CONVERT THE REV TEXT TO REV INTEGER
      DECODE (DIGITS,'(I)',TEX,IOSTAT=IOS) SEGREV
      IF (IOS .NE. 0) THEN
           CALL DISMSG('Error: Program error.')
           CALL DISMSG('(in converting rev text to int).')
           GO TO 1000
      END IF
                      
C RETRIEVE THE RECORD FROM THE SSCVRG RELATION

c--port##     REPEAT RETRIEVE (SSAT=SSCVRG.#SAT,SSEN=SSCVRG.#SENSOR,
c##         SREV   = SSCVRG.#REV,
c##         STRTT  = SSCVRG.#STRTTIME,STOPT  = SSCVRG.#STOPTIME,
c##         STRTL  = SSCVRG.#STRTLAT ,STOPL  = SSCVRG.#STOPLAT,
c##         NRLAT1 = SSCVRG.#NRLAT1  ,NRLON1 = SSCVRG.#NRLON1,
c##         FRLAT1 = SSCVRG.#FARLAT1 ,FRLON1 = SSCVRG.#FARLON1,
c##         NRLAT2 = SSCVRG.#NRLAT2  ,NRLON2 = SSCVRG.#NRLON2,
c##         FRLAT2 = SSCVRG.#FARLAT2 ,FRLON2 = SSCVRG.#FARLON2,
c##         ASCDSC = SSCVRG.#ASCDSC)
c##       WHERE SSCVRG.#DARID = @DARID                     
c##         AND SSCVRG.#SAT = @SAT AND SSCVRG.#SENSOR = @SEN
c##         AND SSCVRG.#REV = @SEGREV
c##     {              
c##     }        

      WRITE(WHERE_CLAUSE, 1) DARID,SAT,SEN,SEGREV
    1 FORMAT('where darid = ',I,
     ?   ' and sat = "',A,'" and sensor = "',A,'" ',
     ?   ' and rev = ',I)
      WHERE_CLAUSE = WHERE_CLAUSE(1:LASTC(WHERE_CLAUSE)) // char(0)
 
      llistptr = db_get_records(%VAL(MAPR_DBPROC), 'sscvrg'//char(0),
     ?   WHERE_CLAUSE, char(0),SSCVRG_COLUMNS,%VAL(ALL_COLS))
      IF ( llistptr .EQ. 0 ) THEN
         CALL DISMSG('ERROR in db query of sscvrg relation.')
      ENDIF 

      call get_no_elements(llist,NRECS)
      IF (NRECS .EQ. 0) THEN                    
         CALL DISMSG('No Specific site coverage record found.')
         CALL db_ftn_free_llist(llist)
         GO TO 1000
      END IF

      P2_DATA_RECORD = db_ftn_first_record(llist, ptrptr)
C-----------------------------------------------------------------------
C FOLLOWING ASSIGNMENT REMOVES WARNING MESSAGE FOR UNUSED VARIABLE PTR
      LOCAL_DUMMY_VALUEHOLDER = PTR


      call get_sscvrg_rec(%VAL(P2_DATA_RECORD),
     ?  SSAT,SSEN,SREV, 
     ?  STRTT,STOPT,ddumy,ddumy,STRTL,STOPL,
     ?  NRLAT, NRLON, FRLAT, FRLON,ASCDSC)

      CALL db_ftn_free_llist(llist)
 
      NRLAT1 = NRLAT(1)
      NRLAT2 = NRLAT(2)
      NRLON1 = NRLON(1)
      NRLON2 = NRLON(2)
      FRLAT1 = FRLAT(1)
      FRLAT2 = FRLAT(2)
      FRLON1 = FRLON(1)
      FRLON2 = FRLON(2)
                          
C VERIFY RECORD DATA
      IF (STRTL  .LT. -90.0  .OR. STRTL  .GT. 90.0  .OR.
     1     STOPL  .LT. -90.0  .OR. STOPL  .GT. 90.0  .OR.
     2     NRLAT1 .LT. -90.0  .OR. NRLAT1 .GT. 90.0  .OR.
     3     NRLON1 .LT. -180.0 .OR. NRLON1 .GT. 180.0 .OR.
     4     FRLAT1 .LT. -90.0  .OR. FRLAT1 .GT. 90.0  .OR.
     5     FRLON1 .LT. -180.0 .OR. FRLON1 .GT. 180.0 .OR.
     6     NRLAT2 .LT. -90.0  .OR. NRLAT2 .GT. 90.0  .OR.
     7     NRLON2 .LT. -180.0 .OR. NRLON2 .GT. 180.0 .OR.
     8     FRLAT2 .LT. -90.0  .OR. FRLAT2 .GT. 90.0  .OR.
     9     FRLON2 .LT. -180.0 .OR. FRLON2 .GT. 180.0 .OR.
     1    (ASCDSC(1:1) .NE. 'A' .AND. ASCDSC(1:1) .NE. 'D') .OR.
     2     SREV .LT. 1 .OR. SREV .GT. 99999) THEN

          CALL DISMSG('Error: Invalid SSCVRG record found.')
          GO TO 9000
      END IF

C VERIFY START AND STOP TIMES
      IOS = tc_validate_asf_datetime(STRTT // char(0) )
      IF (IOS .NE. 1) THEN
          CALL DISMSG('Error: Invalid SSCVRG record.')
          GO TO 9000
      END IF
      IOS = tc_validate_asf_datetime(STOPT // char(0) )
      IF (IOS .NE. 1) THEN
          CALL DISMSG('Error: Invalid SSCVRG record.')
          GO TO 9000  
      END IF                 

C UPDATE MIN AND MAX TIMES FOR PLANNING PERMISSION REQUEST
      IF( MIN_STRTTIME .GT. STRTT ) MIN_STRTTIME = STRTT
      IF( MAX_STOPTIME .LT. STOPT ) MAX_STOPTIME = STOPT

C WRITE THE RECORD TO THE FILE

      WRITE (10,400) DARID,SAT,SEN,SREV,STRTT,STOPT,STRTL,STOPL,
     1                NRLAT1,NRLON1,FRLAT1,FRLON1,
     1                NRLAT2,NRLON2,FRLAT2,FRLON2,
     1                ASCDSC

  400 FORMAT (I10,1X,A2,1X,A3,1X,I5,1X,A21,1X,A21,1X,10(F8.3,1X),A1)


C MARK THE RECORD AS HAVING BEEN WRITTEN
      TEMP(SEGNUM) = 1

C LIST THE MENU AGAIN
      GO TO 1000


 3000 CONTINUE

C     IF NO SEGMENTS ACTUALLY CHOSEN, DELETE THE FILE AND SKIP 
C     ALL OF THE FOLLOWING.  
      IF( SEGCOUNT .EQ. 0 ) THEN
           CLOSE ( 10, DISPOSE = 'DELETE' )
           WRITE(*,*) 'NO SEGMENTS CHOSEN; FILE IS DELETED'
           GO TO 9000
      ENDIF

      CLOSE (10)
C set the flag too
      FILE_IS_OPEN = 0

      WRITE(6,500) OUTFILE
  500 FORMAT (/,' SEG File ',A<SLEN(OUTFILE)>,' completed.')

      IF (LOAD2DB) THEN

C         NOW GET PLANNING ACTIVITY PERMISSION FOR SEGMENT LOADING
C         DETERMINE PERMID ALREADY IN EFFECT FOR SELECTION AND USE THAT
C         VALUE
          PERMID = SELECT_PERMID

 7500     CONTINUE
C         FIRST GET DAR ACTIVITY PERMISSION
          llistptr = create_dyn_llist()
          RCODE = mu_permission_request(%VAL(MAPR_DBPROC),
     ?                   %VAL(PERMID),
     ?                   'CreateDataTakesFrmDAR' // char(0),
     ?                   'DAR' // char(0),
     ?                   %VAL(llistptr),
     ?                   %VAL(0), %VAL(0), %VAL(0),
     ?                   %VAL(DARID) )
          IF( RCODE .LT. 0 ) THEN
                 CALL mu_print_error_msg(%VAL(RCODE))
                 WRITE(*,*)'ERROR OCCURRED DURING PERMISSION REQUEST.'
                 WRITE(*,*)'DAR activity:  CreateDataTakesFrmDAR'
                 WRITE(*,*)'NOTE ERROR MESSAGE AND GET HELP.'
                 WRITE(*,*)'SEGMENTS NOT LOADED'
                 GO TO 9000
          ENDIF
 
          IF ( RCODE .EQ. 0 ) THEN
C             PERMISSION DENIED.  PRINT BLOCKING ACTIVITIES
              WRITE(*,*)'DAR Activity Permission DENIED.'
              WRITE(*,*)'Blocking activities:'
              CALL db_print_list( %VAL(llistptr),
     ?              active_dar_activities_columns )
C             PERMISSIONS PRINTED.  FREE THE LLIST
              CALL db_ftn_free_llist(%VAL(llistptr) )
              WRITE(6,7551)
 7551         FORMAT(/,1x,'Try for permission again? [Y/N] ? ', $)
              READ (5,*,IOSTAT = IOS) ANSWER
              IF( ANSWER .EQ. 'N'  .OR.  ANSWER .EQ. 'n' ) GO TO 9000
              GO TO 7500
          ENDIF
 
C         PERMISSION GRANTED.  FREE THE LLIST
          CALL db_ftn_free_llist(%VAL(llistptr) )
          PERMID = RCODE
 
C         NEXT GET PLANNING ACTIVITY PERMISSION

 7600     CONTINUE
C--       TRY OR RE-TRY:
          llistptr = create_dyn_llist()
          RCODE = mu_permission_request(%VAL(MAPR_DBPROC),
     ?                   %VAL(PERMID),
     ?                   'CreateDataTakesFrmDAR' // char(0),
     ?                   'planning' // char(0),
     ?                   %VAL(llistptr),
     ?                   MIN_STRTTIME // char(0), 
     ?                   MAX_STOPTIME // char(0), 
     ?                   'ALL' // char(0), 
     ?                   %VAL(0) )
          IF( RCODE .LT. 0 ) THEN
              CALL mu_print_error_msg(%VAL(RCODE))
              WRITE(*,*)'ERROR OCCURRED DURING PERMISSION REQUEST.'
              WRITE(*,*)'planning activity:  CreateDataTakesFrmDAR'
              WRITE(*,*)'NOTE ERROR MESSAGE AND GET HELP.'
              WRITE(*,*)'SEGMENTS NOT LOADED'
              GO TO 9000
          ENDIF
 
          IF ( RCODE .EQ. 0 ) THEN
C             PERMISSION DENIED.  PRINT BLOCKING ACTIVITIES
              WRITE(*,*)'Planning Activity Permission DENIED.'
              WRITE(*,*)'Blocking activities:'
              CALL db_print_planning_list( %VAL(llistptr))
C             PERMISSIONS PRINTED.  FREE THE LLIST
              CALL db_ftn_free_llist(%VAL(llistptr) )

              WRITE(6,7551)
C7551---      FORMAT(/,1x,'Try for permission again? [Y/N] ? ', $)
              READ (5,*,IOSTAT = IOS) ANSWER
              IF( ANSWER .NE. 'N'  .AND.  ANSWER .NE. 'n' ) GO TO 7600

              WRITE(*,*)'Segments will not be loaded right now.'

C             MUST NOW TERMINATE THE DAR ACTIVITY PERMISSION
C             THAT WAS JUST OBTAINED. 
              RCODE = mu_permission_terminate(%VAL(MAPR_DBPROC),
     ?                   %VAL(PERMID),
     ?                   'CreateDataTakesFrmDAR' // char(0),
     ?                   'DAR' // char(0) )
              IF( RCODE .LT. 0 ) THEN
                 WRITE(*,*)'ERROR OCCURRED DURING MULTI-USER FUNCTION:'
                 WRITE(*,*)'mu_permission_terminate()'
                 WRITE(*,*)'Trying to terminate permission for:'
                 WRITE(*,*)'DAR activity:  CreateDataTakesFrmDAR'
                 WRITE(*,*)'due to planning permission not granted.'
                 WRITE(*,*)'Error; get software maintenence help. '
                 WRITE(*,*)'Note error message:'
                 CALL mu_print_error_msg(%VAL(RCODE))
                 GO TO 9000
              ENDIF
              IF( RCODE .NE. PERMID ) THEN
                 WRITE(*,*)'MULTI-USER ERROR.  '
                 WRITE(*,*)'PERMISSION NOT TERMINATED.'
                 WRITE(*,*)'Trying to terminate DAR permission'
                 WRITE(*,*)'due to planning permission not granted.'
                 WRITE(*,*)'Error; get software maintenence help. '
                 GO TO 9000
              ENDIF
              GO TO 9000

C------   IF ( RCODE .EQ. 0 ) THEN
          ENDIF
 
C         BOTH PERMISSIONS GRANTED.  FREE THE LLIST
          CALL db_ftn_free_llist(%VAL(llistptr) )
          PERMID = RCODE

C--------------------------------------------------------------------
C         NOW CAN LOAD THE SEGMENTS:

          WRITE (string, 102) PERMID, p_userid(1:LASTC(p_userid)),
     1      p_password(1:LASTC(p_password)),
     2      OUTFILE(1:LASTC(OUTFILE))//char(0)
  102     FORMAT('aps_dtkm_segload -p ',I,' -U ',A,' -P ',A,' ',A)
          WRITE (6,101) string
  101     FORMAT (/,'executing ...', A)

          CALL FLUSH(6)
          status = system( string )
C--------------------------------------------------------------------

C         WORK IS DONE; NOW TERMINATE BOTH PERMISSIONS.
C         TERMINATE THE DAR ACTIVITY PERMISSION
          RCODE = mu_permission_terminate(%VAL(MAPR_DBPROC),
     ?                   %VAL(PERMID),
     ?                   'CreateDataTakesFrmDAR' // char(0),
     ?                   'DAR' // char(0) )
          IF( RCODE .LT. 0 ) THEN
                 WRITE(*,*)'ERROR OCCURRED DURING MULTI-USER FUNCTION:'
                 WRITE(*,*)'mu_permission_terminate()'
                 WRITE(*,*)'Trying to terminate permission for:'
                 WRITE(*,*)'DAR activity:  CreateDataTakesFrmDAR'
                 WRITE(*,*)'Error; get software maintenence help. '
                 WRITE(*,*)'Note error message:'
                 CALL mu_print_error_msg(%VAL(RCODE))
                 GO TO 9000
          ENDIF
          IF( RCODE .NE. PERMID ) THEN
                 WRITE(*,*)'MULTI-USER ERROR.  '
                 WRITE(*,*)'PERMISSION NOT TERMINATED.'
                 WRITE(*,*)'Trying to terminate permission for:'
                 WRITE(*,*)'DAR activity:  CreateDataTakesFrmDAR'
                 WRITE(*,*)'Error; get software maintenence help. '
                 GO TO 9000
          ENDIF
C         TERMINATE THE PLANNING ACTIVITY PERMISSION
          RCODE = mu_permission_terminate(%VAL(MAPR_DBPROC),
     ?                   %VAL(PERMID),
     ?                   'CreateDataTakesFrmDAR' // char(0),
     ?                   'planning' // char(0) )
          IF( RCODE .LT. 0 ) THEN
                 WRITE(*,*)'ERROR OCCURRED DURING MULTI-USER FUNCTION:'
                 WRITE(*,*)'mu_permission_terminate()'
                 WRITE(*,*)'Trying to terminate permission for:'
                 WRITE(*,*)'planning activity:  CreateDataTakesFrmDAR'
                 WRITE(*,*)'Error; get software maintenence help. '
                 WRITE(*,*)'Note error message:'
                 CALL mu_print_error_msg(%VAL(RCODE))
                 GO TO 9000
          ENDIF
          IF( RCODE .NE. PERMID ) THEN
                 WRITE(*,*)'MULTI-USER ERROR.  '
                 WRITE(*,*)'PERMISSION NOT TERMINATED.'
                 WRITE(*,*)'Trying to terminate permission for:'
                 WRITE(*,*)'planning activity:  CreateDataTakesFrmDAR'
                 WRITE(*,*)'Error; get software maintenence help. '
                 GO TO 9000
          ENDIF

C         NOW RETURN TO PREVIOUS MENU.  

C---  IF (LOAD2DB) THEN
      ENDIF
 9000 CONTINUE
      IF( SELECT_PERMID .NE. 0 ) THEN
          RCODE = mu_permission_terminate(%VAL(MAPR_DBPROC),
     ?        %VAL(SELECT_PERMID),
     ?        'SelectSiteCovForDAR' // char(0),
     ?        'DAR' // char(0) )
          IF( RCODE .LT. 0 ) THEN
              WRITE(*,*)'ERROR OCCURRED DURING MULTI-USER FUNCTION:'
              WRITE(*,*)'mu_permission_terminate()'
              WRITE(*,*)'Trying to terminate permission for.'
              WRITE(*,*)'DAR activity:  SelectSiteCovForDAR'
              WRITE(*,*)'Error; get software maintenence help. '
              WRITE(*,*)'Note error message:'
              CALL mu_print_error_msg(%VAL(RCODE))
              WRITE(*,*)'spc2seg.for'
			  GO TO 9999
          ENDIF
          IF( RCODE .NE. SELECT_PERMID ) THEN
              WRITE(*,*)'MULTI-USER ERROR.  '
              WRITE(*,*)'PERMISSION NOT TERMINATED.'
              WRITE(*,*)'Trying to terminate permission for.'
              WRITE(*,*)'DAR activity:  SelectSiteCovForDAR'
              WRITE(*,*)'Error; get software maintenence help. '
              WRITE(*,*)'spc2seg.for'
			  GO TO 9999
          ENDIF

C         OK NO PROBLEM.

       ENDIF

 9999 CONTINUE

       RETURN
       END
