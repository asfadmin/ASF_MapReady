C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.

c********************************************************************
*  Name:    SSCVBATCH
*  Module Type: integer subroutine  Language: FORTRAN
*  Purpose: principle driver routine FOR BATCH SPECIFIC SITE COVERAGE.
*  Functions called:
*  VECTOR LIBRARY:  SPHERE
*  Input Parameters:
*  Name         Type    Definition
*  PROGNAME CHAR*(*) PROGNAME NAME
*  dbproc   INT     Sybase process pointer.  
*  SAT      CHAR*2  SATELLITE NAME
*  SENSOR   CHAR*3  SENSOR
*  DARID    INT*4   ID OF DAR
*  SITENAME CHAR*32 NAME OF SITE
*  STRTTIME CHAR*14 START TIME OF ANALYSIS
*  STOPTIME CHAR*14 END TIME OF ANALYSIS
*  STARTREV INT*4   REV NUMBER AT START OF ANALYSIS
*  ENDREV   INT*4   REV NUMBER AT END OF ANALYSIS
*  ASCDSC   CHAR*1  ASCENDING/DESCENDING FLAG
*  Variables:
*  SHAPE    CHAR*1  SHAPE FLAG:  P, Q, OR R.
*  RADIUS   REAL*8  IF SHAPE = P, RADIUS OF THE CIRCLE.  
*           AND POINT 1 IS THE CENTER.  
*  NWLAT    REAL*8  LATITUDE IN DEGREES OF POINT 1
*  NWLON    REAL*8  LONGITUDE IN DEGREES OF POINT 1
*  NELAT    REAL*8  LATITUDE IN DEGREES OF POINT 2
*  NELON    REAL*8  LONGITUDE IN DEGREES OF POINT 2
*  SELAT    REAL*8  LATITUDE IN DEGREES OF POINT 3
*  SELON    REAL*8  LONGITUDE IN DEGREES OF POINT 3
*  SWLAT    REAL*8  LATITUDE IN DEGREES OF POINT 4
*  SWLON    REAL*8  LONGITUDE IN DEGREES OF POINT 4
*  Locals :
*  Externals :
*  Modification History:                                            
*  Date         Revision    Author
*  $Date$ $Revision$ $Author$
*                                                                   
*********************************************************************/
      integer function SSCVBATCH(PROGNAME, dbproc, SAT, SENSOR, DARID, 
     ?     SITENAME, STRTTIME, STOPTIME, STARTREV,ENDREV, ASCDSC )
      character*100 SccsFileID
     -/'@(#)sscvbatch.for	5.1 98/01/08 APS/ASF\0'/

      IMPLICIT NONE
c--port--##  DECLARE
      INCLUDE 'APS_HOME:include/local/timeconv.inc'
      INCLUDE 'APS_HOME:include/local/aps_log_msg.inc'

      INTEGER DARID,STARTREV,ENDREV, N
      CHARACTER*99 PROGNAME
      CHARACTER*99 SITENAME
      CHARACTER*2 SHAPE
      CHARACTER*2 ASCDSC
      CHARACTER*3 SAT
      CHARACTER*4 SENSOR
      CHARACTER*150 MSG
      character*22 asftime
      INTEGER NDELS, TREVC1, TREVC2
      CHARACTER*22 STRTTIME, STOPTIME   
      REAL*8 RADIUS,NWLAT,NWLON,NELAT,NELON,SELAT,SELON,SWLAT,SWLON
      REAL*8 TIMEC1, TIMEC2, TIMEP, TIMEP1, TIME1, TIMEA, TIMEZ
      INTEGER ISTAT, IRECS, ARECS, DRECS, CRECS, IBATCH
      INTEGER CYDAYS, CYREVS, IERR
      CHARACTER*22 CHTIME, CHTIME1, CHTIME2
      integer rcode, system, LASTC, istat

c---    STATEMENTS TO SUPPORT SYBASE CALLS TO DBLIB
      integer*4 dbproc
c---    pass dbproc by value to C routines:
      external get_phase_t      !$pragma C(get_phase_t)
      external get_dar_sscv     !$pragma C(get_dar_sscv)
      external get_site_sscv    !$pragma C(get_site_sscv)
      external delete_sscvrg    !$pragma C(delete_sscvrg) 
      external tcycle           !$pragma c(tcycle) 
      external tcopy            !$pragma c(tcopy)
      external get_satsensor    !$pragma c(get_satsensor)
c---    STATEMENTS TO SUPPORT SYBASE CALLS TO DBLIB
      integer IMODE
      character*40 CMODE
      real*8 BRAD, LOOK
C----------------------------------------------------------------

      rcode = system('banner CREATE DATA_TAKE OPPS' // char(0))
      call sleep (2)

      print *, PROGNAME(1:LASTC(PROGNAME)), ' February 1996 mod'
      print *,'Starting sscvbatch.for.'
      print *,'sat = ', sat(1:lastc(sat))
      print *,'sensor = ', sensor(1:lastc(sensor))
      print *,'darid = ', darid 
      print *,'sitename = ', sitename(1:lastc(sitename))
      print *,'strttime = ', strttime(1:lastc(strttime))
      print *,'stoptime = ', stoptime(1:lastc(stoptime))
      print *,'startrev = ', startrev
      print *,'endrev = ', endrev
      print *,'ascdsc = ', ascdsc(1:lastc(ascdsc))
C--   print *,'lastc(strttime) = ', lastc(strttime)
c---    check for mandatory supplied values:  
      if(SAT(1:1) .eq. ' ') then
        print *,'a value for sat must be supplied'
        sscvbatch = 1
        return 
      endif
      if(SENSOR(1:1) .eq. ' ') then
        print *,'a value for sensor must be supplied'
        sscvbatch = 1
        return 
      endif
      if(SENSOR(1:3) .eq. 'DMP') then
        print *,
     ?    'DMP is not a sensor; value for sensor must be supplied'
        sscvbatch = 1
        return 
      endif

      print *, 'argument values:'
      print *, 'SAT = ', SAT
      print *, 'SENSOR = ', SENSOR
      if(DARID .eq. 0) then
          print *, 'SITENAME = ', SITENAME(1:lastc(SITENAME))
      else
          print *, 'DARID = ', DARID
      endif
      print *, 'ASCDSC = ', ASCDSC(1:1)
      if(STARTREV .eq. 0) then
          print *, 'STRTTIME = ', STRTTIME
          print *, 'STOPTIME = ', STOPTIME
      else 
          print *, 'STARTREV = ', STARTREV
          print *, 'ENDREV   = ', ENDREV
      endif

C----------------------------------------------------------------

c--port--C---   READ ALL THE INPUT VALUES FROM THE FILE NAMED BY LOGICAL
c-- instead, use arguments already read from the command line.  
c--port--      WRITE(*,*)'SSCVBATCH:  START'
c--port--      OPEN (97,FILE='MPS_SSCVINFILE',STATUS='OLD',READONLY)
c--port--      WRITE(*,*)'SSCVBATCH:  OPENED INPUT FILE'

      if(DARID .ne. 0) then

c--port--      READ(97,*) DARID
          WRITE(*,*)'SSCVBATCH:  DARID=', DARID

      else

c--port--      READ(97,9701) SITENAME
c--port-- 9701 FORMAT(1X,A32)
          WRITE(*,*)'SSCVBATCH:  SITENAME=>',
     ?    SITENAME(1:lastc(SITENAME)) // char(0), '<'

      endif


      if(darid .ne. 0) then

c---    this is a DAR.  
c---    get dar info from the database.

        call get_dar_sscv(%VAL(dbproc), DARID, sitename, SHAPE, 
     ?  RADIUS, NWLAT, NWLON, NELAT, NELON, SELAT, SELON, SWLAT, SWLON,
     ?  IERR)
        if(IERR .ne. 0) then
            print *,' '
            print *,' '
            print *,' '
            print *,
     ?      'sscvbatch.for:  data not found for dar id ',DARID
            print *,
     ?      '                check in dar relation'
            print *,
     ?      '                now terminating this run.'
            sscvbatch = 202

            ENCODE(10, '(I9)', MSG) DARID
            CALL APS_LOG_MSG_FORTRAN( PROGNAME, APS_ERROR,
     ?         'data not found for dar id ' // MSG(1:LASTC(MSG)) , 
     ?         DO_SYSLOG, DO_PRINT )

            return 
        endif
        WRITE(*,*)'SSCVBATCH:  SITENAME = ',SITENAME(1:lastc(sitename))

      else

c---    this is a site, not a DAR.  
c---    get site info from the database.

        call get_site_sscv(%VAL(dbproc), 
     ?       SITENAME(1:lastc(SITENAME)) // char(0), SHAPE, RADIUS,
     ?       NWLAT, NWLON, NELAT, NELON, SELAT, SELON, SWLAT, SWLON,
     ?       IERR    )

        if(IERR .ne. 0) then
            print *,' '
            print *,' '
            print *,' '
            print *,
     ?            'sscvbatch.for:  data not found for sitename ',
     ?            SITENAME(1:lastc(SITENAME))
            print *,
     ?      '                check in site relation'
            print *,
     ?      '                now terminating this run.'
            sscvbatch = 203
            CALL APS_LOG_MSG_FORTRAN( PROGNAME, APS_ERROR,
     ?         'data not found for sitename ' // 
     ?         SITENAME(1:lastc(SITENAME)), DO_SYSLOG, DO_PRINT )

            return 
        endif
      endif
    
      WRITE(*,*) 'SSCVBATCH:  SHAPE = ',SHAPE(1:1)
c--port--      READ(97,*) RADIUS
      WRITE(*,*) 'SSCVBATCH:  RADIUS = ',RADIUS
c--port--      READ(97,*) NWLAT
      WRITE(*,*) 'SSCVBATCH:  NWLAT = ',NWLAT
c--port--      READ(97,*) NWLON
      WRITE(*,*) 'SSCVBATCH:  NWLON = ',NWLON
c--port--      READ(97,*) NELAT
      WRITE(*,*) 'SSCVBATCH:  NELAT= ',NELAT
c--port--      READ(97,*) NELON
      WRITE(*,*) 'SSCVBATCH:  NELON = ', NELON
c--port--      READ(97,*) SELAT
      WRITE(*,*) 'SSCVBATCH:  SELAT = ', SELAT
c--port--      READ(97,*) SELON
      WRITE(*,*) 'SSCVBATCH:  SELON = ', SELON
c--port--      READ(97,*) SWLAT
      WRITE(*,*) 'SSCVBATCH:  SWLAT = ', SWLAT
c--port--      READ(97,*) SWLON
      WRITE(*,*) 'SSCVBATCH:  SWLON = ', SWLON

c--port--      READ(97,*) SAT
      WRITE(*,*) 'SSCVBATCH:  SAT = ', SAT

c--port--      READ(97,*) SENSOR
      WRITE(*,*) 'SSCVBATCH: SENSOR = ', SENSOR

c--port--      READ(97,*) ASCDSC
      WRITE(*,*) 'SSCVBATCH:  ASCDSC = ', ASCDSC(1:1)

c--port--      READ(97,*) STRTTIME
      WRITE(*,*) 'SSCVBATCH:  STRTTIME = ', STRTTIME

c--port--      READ(97,*) STOPTIME
      WRITE(*,*) 'SSCVBATCH:  STOPTIME = ', STOPTIME

c--port--      READ(97,*) STARTREV
      WRITE(*,*) 'SSCVBATCH:  STARTREV = ', STARTREV

c--port--      READ(97,*) ENDREV
      WRITE(*,*) 'SSCVBATCH:  ENDREV = ', ENDREV

c--port--      CLOSE (97)
c--port--      WRITE(*,*)'SSCVBATCH:  INPUT FILE CLOSED'
C---    BATCH FLAG.  1 = BATCH.  USED IN CALLS TO ROUTINES.  
      IBATCH = 1
      IRECS = 0
      ARECS = 0
      DRECS = 0
      CRECS = 0
C---    validate the satellite & sensor:
      call get_satsensor(%VAL(dbproc), SAT(1:LASTC(SAT)) // char(0), 
     ?                     SENSOR(1:LASTC(SENSOR)) // char(0), IMODE,
     ?                     CMODE, BRAD, LOOK, IERR)
      if(IERR .ne. 0) then
C---    error in getting satellite values.  
          print *,' '
          print *,' '
          print *,'SSCVBATCH.FOR:  ERROR in satellite/sensor. '
          print *,'                check satellite, sensor values:'
          print *,'                SAT = ', SAT
          print *,'                SENSOR = ', SENSOR
C---        error code:  
          sscvbatch = 1
          CALL APS_LOG_MSG_FORTRAN( PROGNAME, APS_ERROR,
     ?      'error in getting satellite values', 
     ?      DO_SYSLOG, DO_PRINT )
         return 
      endif
C---    CHECK FOR REV-BASED RUN OR TIME-BASED RUN.  
      IF(STARTREV .EQ. 0 .or. ENDREV .EQ. 0) GO TO 1000
      CALL REVS2T(dbproc,IBATCH,SAT,STARTREV,ENDREV,
     ?      TIMEA,CHTIME1,TIMEZ,CHTIME2,IERR)
      if(IERR .ne. 0) then
C---    error in converting from revs to time; check satellite, rev values:
          print *,' '
          print *,' '
          print *,'SSCVBATCH.FOR:  ERROR converting from revs to time.'
          print *,'                check satellite, rev values:'
          print *,'                SAT = ', SAT
          print *,'                STARTREV = ', STARTREV
          print *,'                ENDREV = ', ENDREV
C---        error code:  
          sscvbatch = 1
          CALL APS_LOG_MSG_FORTRAN( PROGNAME, APS_ERROR,
     ?       'Error converting from revs to time', 
     ?       DO_SYSLOG, DO_PRINT )
         return 
      endif
      GO TO 2000
 1000 CONTINUE
C---    TIME-BASED USER REQUEST.  
      CHTIME = STRTTIME(1:21) // char(0)
C---      CALL ASF2ET(CHTIME,TIMEA,IERR)
      IERR = tc_asf2et(CHTIME, TIMEA )
      if(IERR .ne. 1) then
C---    error in input start time.  
          print *, 'SSCVBATCH.FOR:  ERROR in input start time '
C---        error code:  
          print *,'               start time = ', STRTTIME
          sscvbatch = 1
          CALL APS_LOG_MSG_FORTRAN( PROGNAME, APS_ERROR,
     ?       'Error in input start time.', DO_SYSLOG, DO_PRINT )
          return
      endif
      CHTIME = STOPTIME(1:21) // char(0)
C---      CALL ASF2ET(CHTIME,TIMEZ,IERR)
      IERR = tc_asf2et( CHTIME, TIMEZ )
      if(IERR .ne. 1) then
C---    error in input end time.  
          print *,
     ?    'SSCVBATCH.FOR:  ERROR in input end time '
C---        error code:  
          print *,'               end time = ', STOPTIME
          sscvbatch = 1
          CALL APS_LOG_MSG_FORTRAN( PROGNAME, APS_ERROR,
     ?       'Error in input end time.', DO_SYSLOG, DO_PRINT )
          return
      endif

C---    TIME-BASED START.  GET THE REV BRACKET
      CALL TIMES2R(dbproc,IBATCH,SAT,TIMEA,' ',TIMEZ,' ',
     ?      STARTREV,ENDREV,IERR)
      if(IERR .ne. 0) then
C---    error in converting from times to revs; check satellite, time values:
          print *,'SSCVBATCH.FOR:  ERROR converting from times to revs.'
          print *,'                check satellite, input time values:'
          print *,'                SAT = ', SAT
          print *,'                start time = ', STRTTIME
          print *,'                stop time = ', STOPTIME
C---        error code:  
          sscvbatch = 1
          CALL APS_LOG_MSG_FORTRAN( PROGNAME, APS_ERROR,
     ?       'Error converting from times to revs.', 
     ?       DO_SYSLOG, DO_PRINT )
         return 
      endif
 2000 CONTINUE
C---    NOW THE REV/TIME BRACKETS ARE SET.  
C---    NOW DELETE THE sscvrg RECORDS PREVIUSLY DONE WITH THE CURRENT
C---    SAT, SENSOR, TIME BRACKET, AND SITE/DAR.  TO BE DELETED, THE WHOLE 
C---    TIME DURATION OF THE PREVIOUS COVERAGE MUST BE WITHIN THE CURRENT 
C---    TIME BRACKET.  
c-port-## DELETE sscvrg WHERE sscvrg.#darid = DARID
c-port-##               AND   sscvrg.#sitename = SITENAME
c-port-##               AND   sscvrg.#strtet >= TIMEA
c-port-##               AND   sscvrg.#stopet <= TIMEZ
c-port-##        AND   sscvrg.#sat = SAT
c-port-##               AND   sscvrg.#sensor = SENSOR
c-port-##               AND   sscvrg.#ascdsc = ASCDSC
c-port-##  inquire_equel (NDELS = rowcount)

      call delete_sscvrg(%VAL(dbproc), DARID, 
     ?     SITENAME(1:lastc(SITENAME)) // char(0), TIMEA, TIMEZ,
     ?     SAT, SENSOR, ASCDSC(1:1) // char(0) , NDELS )

      IF(IBATCH .EQ. 1)
     ?WRITE(*,*) ' SSCVBATCH:  Previous SSCVRG recs deleted = ', NDELS
      TIME1 = TIMEA
 6000 CONTINUE
C---    LOOP START
C---    BREAK OUT COVERAGE, REPLICATION.  
      WRITE(*,*)' TIME1,TIMEZ=',TIME1,TIMEZ
      CALL TCYCLE(%VAL(dbproc),SAT,TIME1,TIMEZ,TIMEC1,TIMEC2,
     ?                  TIMEP,TIMEP1,
     ?          CYDAYS,CYREVS)

C---      CALL ET2ASF(TIMEC1,CHTIME)
      IERR = tc_et2asf(%VAL(TIMEC1), CHTIME )
      WRITE(*,*)' SSCVBATCH:  TIMEC1 = ', CHTIME
C---      CALL ET2ASF(TIMEC2,CHTIME)
      IERR = tc_et2asf(%VAL(TIMEC2), CHTIME )
      WRITE(*,*)' SSCVBATCH:  TIMEC2 = ', CHTIME
C---      CALL ET2ASF(TIMEP,CHTIME)
      IERR = tc_et2asf(%VAL(TIMEP), CHTIME )
      WRITE(*,*)' SSCVBATCH:  TIMEP = ', CHTIME
      IF (TIMEP1 .LE. 0.0D0 ) GO TO 6010
C---      CALL ET2ASF(TIMEP1,CHTIME)
      IERR = tc_et2asf(%VAL(TIMEP1), CHTIME )
      WRITE(*,*)' SSCVBATCH:  TIMEP1 = ', CHTIME
      GO TO 6020
 6010 CONTINUE
      WRITE(*,*)' SSCVBATCH:  TIMEP1 = ', TIMEP1
 6020 CONTINUE
C---    TIME1   = START OF REMAINING TIME INTERVAL
C---    TIMEZ   = END OF REV INTERVAL
C---    TIMEC1  = START OF INTERVAL TO RUN SSCV ROUTINE - DO GEOMETRY
C---    TIMEC2  = END OF ONE CYCLE FOR RUN OF SSCV ROUTINE
C---    TIMEP   = END OF CURRENT PHASE:  FROM TIMEC2 + 0.0001 TO TIMEP WE CAN 
C---            JUST REPLICATE THE RESULTS FROM TIMEC1 TO TIMEC2.
C---    TIMEP1  = START OF NEXT PHASE.  IT WILL BE THE NEW TIME1 WHEN THE LOOP
C---            STARTS AGAIN.  
C---    CYDAYS  = NUMBER OF DAYS IN CURRENT CYCLE.  
C---    CYREVS  = NUMBER OF REVS IN CURRENT CYCLE.  
C---
C---    CHECK TO SEE IF WE ARE THROUGH YET.  
      IF(TIMEC1 .LT. 0) GO TO 9999
C---    NOW CURRENT CYCLE FIRST. 
C---    FIRST COMPUTE THE REVS AS A HELP FOR THE QUERY:
      CALL TIMES2R(dbproc,IBATCH,SAT,TIMEC1,' ',TIMEC2,' ',
     ?          TREVC1,TREVC2,IERR)
C---    NOTE THAT IF THE USER REQUESTS BOTH ASCENDING AND DESCENDING, THIS
C---    IS ACCOMPLISHED IN SEPARATE CALLS:  
      if(IERR .ne. 0) then
        print *,'sscvbatch.for:  error in computing a rev bracket from'
        print *,'                a time bracket.'
        print *,'                SAT = ', SAT
        print *,'                TIMEC1 = ', TIMEC1
C---        call et2asf(TIMEC1, asftime)
        istat = tc_et2asf(%VAL(TIMEC1), asftime )
        print *,'                  equals ', asftime
        print *,'                TIMEC2 = ', TIMEC2
C---        call et2asf(TIMEC2, asftime)
        istat = tc_et2asf(%VAL(TIMEC2), asftime )
        print *,'                  equals ', asftime
        print *,'                now terminating this run.'
        sscvbatch = 201
          CALL APS_LOG_MSG_FORTRAN( PROGNAME, APS_ERROR,
     ?       'Error converting time bracket to rev bracket.', 
     ?       DO_SYSLOG, DO_PRINT )
        return 
      endif
      N = 0

c---    note that if ASCDSC = '*', then BOTH of the next 2 calls are made.
      WRITE(*,*) 'SSCVBATCH:  SHAPE = ',SHAPE(1:1)
      WRITE(*,*) 'SSCVBATCH:  ASCDSC = ', ASCDSC(1:1)
      IF(ASCDSC(1:1) .NE. 'D')print *,'sscvbatch.for:  ascdsc   != D'
      IF(ASCDSC(1:1) .NE. 'A')print *,'sscvbatch.for:  ascdsc   != A'
      IF(SHAPE(1:1) .EQ. 'P') print *,'sscvbatch.for:  shape1   = P'
      IF(SHAPE(1:1) .NE. 'P') print *,'sscvbatch.for:  shape1   != P'


      IF(SHAPE(1:1) .EQ. 'P' .AND. ASCDSC(1:1) .NE. 'D') THEN
C---        ascending:  CIRCLE/TIME - ONE CYCLE
          CALL SSCVC(dbproc,IBATCH,DARID,
     ?      SITENAME(1:lastc(SITENAME)) // char(0),
     ?         RADIUS, NWLAT, NWLON,
     ?      SAT,SENSOR,'A',TIMEC1,TIMEC2,TREVC1,TREVC2,N,ISTAT)
      IF(ISTAT.NE.0) GO TO 9999
      IRECS = IRECS + N
      ARECS = ARECS + N
      print *,'SSCVBATCH: ', N, ' ASCENDING RECORDS APPENDED BY SSCVC'
      print *,' '
      ENDIF
      N = 0
      IF(SHAPE(1:1) .EQ. 'P' .AND. ASCDSC(1:1) .NE. 'A') THEN
C---        descending:  CIRCLE/TIME - ONE CYCLE
          CALL SSCVC(dbproc,IBATCH,DARID,
     ?              SITENAME(1:lastc(SITENAME)) // char(0),
     ?                            RADIUS,NWLAT,NWLON,
     ?      SAT,SENSOR,'D',TIMEC1,TIMEC2,TREVC1,TREVC2,N,ISTAT)
      IF(ISTAT.NE.0) GO TO 9999
      IRECS = IRECS + N
      DRECS = DRECS + N
      print *,'SSCVBATCH: ', N, ' DESCENDING RECORDS APPENDED BY SSCVC'
      print *,' '
      ENDIF
      N = 0
C---    NOTE THAT IF THE USER REQUESTS BOTH ASCENDING AND DESCENDING, THIS
C---    IS ACCOMPLISHED IN SEPARATE CALLS:  
      IF(SHAPE(1:1) .NE. 'P' .AND. ASCDSC(1:1) .NE. 'D') THEN
C---        ascending:  QUADRILATERAL/TIME - ONE CYCLE
          CALL SSCVQ(dbproc,IBATCH,DARID,
     ?      SITENAME(1:lastc(SITENAME)) // char(0),
     ?      NWLAT,NWLON,NELAT,NELON,SELAT,SELON,SWLAT,SWLON,
     ?          SAT,SENSOR,'A',TIMEC1,TIMEC2,TREVC1,TREVC2,N,ISTAT)
      IF(ISTAT.NE.0) GO TO 9999
      IRECS = IRECS + N
      ARECS = ARECS + N
      print *,'SSCVBATCH: ', N, ' ASCENDING RECORDS APPENDED BY SSCVQ'
      print *,' '
      ENDIF
      N = 0
      IF(SHAPE(1:1) .NE. 'P' .AND. ASCDSC(1:1) .NE. 'A') THEN
C---        descending:  QUADRILATERAL/TIME - ONE CYCLE
          CALL SSCVQ(dbproc,IBATCH,DARID,
     ?      SITENAME(1:lastc(SITENAME)) // char(0),
     ?      NWLAT,NWLON,NELAT,NELON,SELAT,SELON,SWLAT,SWLON,
     ?          SAT,SENSOR,'D',TIMEC1,TIMEC2,TREVC1,TREVC2,N,ISTAT)
      IF(ISTAT.NE.0) GO TO 9999
      IRECS = IRECS + N
      DRECS = DRECS + N
      print *,'SSCVBATCH: ', N, ' DESCENDING RECORDS APPENDED BY SSCVQ'
      print *,' '
      ENDIF
      N = 0
C---    CHECK TO SEE IF WE ARE THROUGH YET.  
      IF(TIMEZ .LE. TIMEC2) GO TO 9999
C---    NOW REPLICATE THIS RUN UNTIL THE END OF THE CURRENT PHASE
c---    replicate, for both ascending and descending, 
c---    using the repeat cycle info, for times during 
c---    the cycle already processed, and insert records for times
c---    until the end of the target time or end of phase
c---    whichever comes first.  this is TIMEP.
c-- source of copy:  TIMEC1 to TIMEC2.
c---    destination of copies:  TIMEC2 to TIMEP.
      IF(ASCDSC(1:1) .NE. 'A') THEN
          CALL TCOPY(%VAL(dbproc),IBATCH,DARID,
     ?  SITENAME(1:lastc(SITENAME)) // char(0),SAT,SENSOR,'D'//char(0),
     ?  TIMEC1,TIMEC2,TIMEP,CYDAYS,CYREVS,N,ISTAT)
          IF(ISTAT.NE.0) GO TO 9999
          print *,
     ?    'SSCVBATCH: ', N, ' DESCENDING RECORDS APPENDED BY TCOPY'
          print *,' '
          IRECS = IRECS + N
          CRECS = CRECS + N
          N = 0
      endif
      IF(ASCDSC(1:1) .NE. 'D') THEN
          CALL TCOPY(%VAL(dbproc),IBATCH,DARID,
     ?  SITENAME(1:lastc(SITENAME)) // char(0),SAT,SENSOR,'A'//char(0),      
     ?  TIMEC1,TIMEC2,TIMEP,CYDAYS,CYREVS,N,ISTAT)
          IF(ISTAT.NE.0) GO TO 9999
          print *,
     ?    'SSCVBATCH: ', N, ' ASCENDING RECORDS APPENDED BY TCOPY'
          print *,' '
          IRECS = IRECS + N
          CRECS = CRECS + N
          N = 0
      endif
C---    CHECK TO SEE IF WE ARE THROUGH YET.  
      IF ( TIMEP1 .LT. 0 ) GO TO 9999
C---    PREPARE FOR NEXT PHASE
      TIME1 = TIMEP1
C---    END OF LOOP FOR TIME-BASED COVERAGE.
      GO TO 6000
 9999 CONTINUE

      PRINT *,' '
      PRINT *,' Create Data-take Opportunities:  end of run ',
     ?  'for ', sat(1:lastc(sat)), ' ',sensor(1:lastc(sensor))
      if(darid .ne. 0) print *,' darid = ', darid 
      if(darid .eq. 0) 
     ?       print *,' sitename = ', sitename(1:lastc(sitename))
      print *,' time bracket:  ', strttime(1:lastc(strttime)), '  ', 
     ?       stoptime(1:lastc(stoptime))
      print *,' rev bracket:   ', startrev, ' ', endrev
      if(ascdsc(1:1) .eq. '*') print *,
     ?   ' both ascending and descending data-takes were requested'
      if(ascdsc(1:1) .eq. 'D')
     ?       print *,' descending data-takes were requested'
      if(ascdsc(1:1) .eq. 'A')
     ?       print *,' ascending data-takes were requested'
      WRITE(*,9001)ARECS
 9001 FORMAT('  ascending recs appended: ', I4)
      WRITE(*,9002)DRECS
 9002 FORMAT('  descending recs appended:', I4)
      if(CRECS .gt. 0) then
        WRITE(*,9005)CRECS
 9005 FORMAT('  recs appended via ground track repeat cycle copying:',
     ?  I4)
      endif

      WRITE(*,9003)IRECS
 9003 FORMAT('  Total recs appended:', I4)

C---    Normal return code:  
      sscvbatch = 0
      return 
      END

