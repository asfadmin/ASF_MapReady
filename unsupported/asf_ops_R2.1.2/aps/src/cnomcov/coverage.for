C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:  coverage.for
C--
C--  Description:   
C-- 
C--  Notes:
C--
C-- ==========================================================================
c **********************************************************************
c  Name: Coverage
c  Module Type: subroutine              Language: FORTRAN
c  coverage.for
c  Purpose: 
c  Main program for swath sensor coverage generation
c  Input Parameters:
c  Name            Type   Description
c  progname        char   name of the program, for calls to APS_LOG_MSG_FORTRAN
c  start_time      char   ASF format start time for run 
c  end_time        char   ASF format end time for run 
c  maskid          char   ASF = ASF mask only; GBL = global.
c  sat             char   satellite:  E1, J1, etc.  
c  sensor          char   sensor:  SAR, OPS, etc.  
c  ISTEP           int    minutes between coverage points. usually 1.   
c  IQUICK          int    quick coverage flag:  =1 means write to ascii file 
c                         but do not write to db.
c  IFILE           int    file coverage flag:  = 1 means write to ascii file.
c  REPFLAG         char   "Y" means use ground track repeat cycle for 
c                         replication of ephemeris from nominal orbit.
c
c  Other variables:
c      --  determine ephemeris file name from satellite, phase name.  
c            fname = $APS_DATA/nof/<sat>_<phase>.EPHM
c          example:  $APS_DATA/nof/E1_A.EPHM
c                       fname                   c57     Ephemeris File Name
c       brev                    int     Rev number to start coverage
c       erev                    int     Rev number to end coverage
c       rev                             int     First revnumber in file
c       maskid                  c3      ID of station mask for coverage
c  Read from Ephemeris file UNIT 80 = EUNIT 
c       x(1), x(2), x(3)        real*8  (x, y, z) position
c       x(4), x(5), x(6)        real*8  (x, y, z) velocity
c       x(7)                    real*8  Time, ephemeris in Julian days
c       x(8)                    real*8  rev number of state vector
c       x(9)                    real*8  Longitude of ascending node
c       x(10)                   real*8  Subsat lat, geocentric
c       x(11)                   real*8  Subsat lon, geocentric
c       x(12)                   real*8  Greenwich Hour Angle
c  Output Parameters:
c  Name         Type            Description
c  Output is done in CovFormat
c
c  Modification History:
c  Author       Revision        Date
c  HERB         1.0             17 Apr 1989 14:51:54 
c  CRAIG        1.1             02 May 1989 13:25:38 
c  DBMAN  2.0  18 Jul 1990 17:57:24  
c  $Author$ $Revision$ $Date$
c
c ****************************************************************************
      INTEGER FUNCTION COVERAGE(progname, dbproc, 
     ?  s_start_time,s_end_time,
     ?  s_maskid,s_sat,s_sensor,
     ?  ISTEP, IQUICK, IFILE, S_REPFLAG )

      character*100 SccsFileID
     -/'@(#)coverage.for	5.1 98/01/08 APS/ASF\0'/

      IMPLICIT NONE
      CHARACTER*(200) progname
      CHARACTER*22 s_start_time, s_end_time
      CHARACTER*21 start_time, end_time
      CHARACTER*4 s_maskid
      CHARACTER*3 maskid
      CHARACTER*3 s_sat
      CHARACTER*2 sat
      CHARACTER*4 s_sensor
      CHARACTER*3 sensor
      INTEGER ISTEP, IQUICK, IFILE
      CHARACTER*2 S_REPFLAG
      CHARACTER REPFLAG

      integer ndum, ndum2
      integer EUNIT
      real*8 xdum1
      character*22 bufdays

      character *200 ibuf
      character *100 filename80, filename81, filename99
      character *22 nowtime
      CHARACTER*100 fname 
      CHARACTER*20 ephm_basename 
      CHARACTER*20 cvrg_basename 
      CHARACTER*93 IBUF93
      CHARACTER*157 IBUF157
      CHARACTER*15 CMODE
      CHARACTER*21 epochasf
      character *2 PHASE
      character *2 ANTARCTIC_MODE
      CHARACTER nflag, nodeflag, amask
      integer rcode, system, LASTC, lc,time_count
      integer n_cvrg 
      INTEGER IMODE, run_shortened
      INTEGER marker, CYDAYS, CYREVS, J
      INTEGER IREVD1, IREVD2
C---    RGetx reads the next record from the nominal orbit ephemeris 
C---    file, which has just one orbit in it.  RGetx "virtually" 
C---    "expands" the file via the ground track repeat cycle into 
C---    an ephemeris file with data between the 
C---    time bracket given as parameters in the call.  
C---    Rgetx is called repeatedly just as a file is read repeatedly.  
C---    Thus repeated calls to RGets appear like repeated reads from a 
C---    "large" ephemeris file.  
      LOGICAL RGetx
      LOGICAL done, firsttime, Getx
      LOGICAL IN_CIRCLE, FIRSTGETX
      REAL*8 epoch, TNODE, start_et, end_et, XDUM
      REAL*8 start_et_search
      REAL*8 time_cut
      REAL*8 start_et_expanded, end_et_expanded
      REAL*8 XNLAT, XNLON, XFLAT, XFLON, XSSLAT, XSSLON
      REAL*8 SSSLAT, SSSLON
      REAL*8 SNLAT, SNLON, SFLAT, SFLON, ALOOK, BEAMW
      REAL*8 x(12), savevec(12), xc(3)
      REAL*8 SMAXIS,XINCL,ARGPER, TPREV, TSTEP, TMINIT
      REAL*8 Y(12), yc(3), ysslat, ysslon, ynlat, ynlon, yflat, yflon
C---    data to compute the end time of the phase.  
      REAL*8 phase_start_et, phase_end_et
      INTEGER phase_orbits
      CHARACTER*22 phase_start, phase_end
C
      LOGICAL NOW_IN_ASF, PREV_IN_ASF
      LOGICAL NOW_IN_MCM, PREV_IN_MCM
C---    RUN MASK AND MASK DATA:
C---    MCM (McMurdo) data:
      REAL*8 MCMLAT, MCMLON, MCMRAD, MCMRMRAD, MCMRMBUF
C---    ASF data:
      REAL*8 ASFLAT, ASFLON, ASFRAD, ASFRMRAD, ASFRMBUF
C
      INTEGER IREV, IER, NRECS_FOUND, NRECSC, NC 
      INCLUDE 'APS_HOME:include/local/mps_const_math.inc'
      INCLUDE 'APS_HOME:include/local/mps_const_earth.inc'
      INCLUDE 'APS_HOME:include/local/timeconv.inc'
      INCLUDE 'APS_HOME:include/local/aps_log_msg.inc'

c---    DECLARE C-routine calls to the compiler:

      external aps_chmod        !$pragma c(aps_chmod)
      external aps_access       !$pragma c(aps_access)
      integer   unlink

c---    STATEMENTS TO SUPPORT SYBASE CALLS TO DBLIB
      integer*4 db_open
      external db_open !$pragma c(db_open)
      external db_open_errs !$pragma c(db_open_errs)
      external error_handler_exit !$pragma c(error_handler_exit)
      integer*4 dbproc
c---    pass dbproc by value to C routines:  
      external get_satsensor    !$pragma c(get_satsensor)
      external integer get_station      !$pragma c(get_station)
          INTEGER get_station
      external delete_cov       !$pragma c(delete_cov)
      external cvrg_metadata    !$pragma c(cvrg_metadata)
      external get_phase_t      !$pragma C(get_phase_t)
      external dbexit           !$pragma c(dbexit)
c---    END OF STATEMENTS TO SUPPORT SYBASE CALLS TO DBLIB

C---    passing strings from C to fortran is not recommended; the input 
C---    arguments are copied to internal fortran variables at the start.  
C---    the other string arguments are passed when the database is opened 
C---    and taken care of at that point.  
      start_time = s_start_time(1:21)
      end_time = s_end_time(1:21)
      maskid = s_maskid(1:3)
      sat = s_sat(1:2)
      sensor = s_sensor(1:3)
      REPFLAG = S_REPFLAG(1:1)

C---    sometimes, the end time can be beyond the current phase, so that
C---    replication cannot occur beyond that time.  In which case the 
C---    run is shortened.  The end time is changed to the end of the phase.
      run_shortened = 0
C--   Initialize record count for writing DB records to the cvrg relation.  
      NRECSC = 0
      rcode = system('banner START COVERAGE')
      call sleep (1)
      print *,'coverage:  satellite = ', sat
      print *,'coverage:  sensor = ', sensor
      print *,'coverage:  begin_time = ', start_time
      print *,'coverage:  end_time = ', end_time
      print *,'coverage:  mask = ', maskid
      print *,'coverage:  step (minutes) =', ISTEP
      if(REPFLAG .eq. 'Y') 
     ?    print *,'coverage:  replicating from nominal orbit.'
      if(REPFLAG .ne. 'Y') 
     ?    print *,'coverage:  NOT replicating from nominal orbit.'
      if(IQUICK .eq. 1) 
     ?    print *,
     ?'coverage:  a QUICK COVERAGE run; no writing to the database.'
      if(IQUICK .ne. 1) 
     ?        print *,'coverage:  not a quick coverage run.'
      if(IFILE .eq. 1) 
     ?        print *,'coverage:  write to ASCII file.'
      print *,' '
      print *,' '
c          INITIALIZE VALUE OF MASK FIELD.
      amask = '-'
      FIRSTGETX = .TRUE.
      firsttime = .TRUE.

C---    obtain satsensor data, validate sat, sensor values as well.  

      IF(SENSOR(1:3) .EQ. 'DMP') THEN
          WRITE(*,*)' COVERAGE.FOR:  ERROR:  sensor = DMP not allowed.'
          WRITE(*,*)'          SAT = ', SAT
          WRITE(*,*)'          SENSOR = ', SENSOR
          CALL APS_LOG_MSG_FORTRAN( PROGNAME, APS_ERROR,
     ?            'sensor value = DMP not allowed.',
     ?            DO_SYSLOG, DO_PRINT )
          coverage = APS_EXIT_ERROR
          return
      ENDIF

      call get_satsensor(%VAL(dbproc), SAT, SENSOR, 
     ?                            IMODE, CMODE, BEAMW, ALOOK, IER)
      WRITE(*,*) 'COVERAGE:  ALOOK, BEAMW=', ALOOK, BEAMW
      WRITE(*,*) 'COVERAGE:  opermode   = ', IMODE
      WRITE(*,*) 'COVERAGE:  chopermode = ', CMODE
      IF(IER .NE. 0) THEN
          WRITE(*,*)' '
          WRITE(*,*)' '
          WRITE(*,*)' COVERAGE.FOR:  ERROR retrieving satsensor data.'
          WRITE(*,*)'          check input values for sat, sensor.'
          WRITE(*,*)'          SAT = ', SAT
          WRITE(*,*)'          SENSOR = ', SENSOR
          CALL APS_LOG_MSG_FORTRAN( PROGNAME, APS_ERROR,
     ?            'when retrieving satsensor data',
     ?            DO_SYSLOG, DO_PRINT )
          coverage = APS_EXIT_ERROR
          return
      ENDIF

      WRITE(*,*) ' COVERAGE:  start_time =',start_time 
C---      CALL ASF2ET(start_time, start_et,IER)
      IER = tc_asf2et(start_time, start_et )
      if(IER .ne. 1) then
          print *,' '
          print *,'coverage.for:  ERROR in start time'
          print *,'               start time = ', start_time
          print *,'               terminating the run.'
          CALL APS_LOG_MSG_FORTRAN( PROGNAME, APS_ERROR,
     ?            'start time value',
     ?            DO_SYSLOG, DO_PRINT )
          coverage = APS_EXIT_ERROR
          return
      endif
      WRITE(*,*) ' COVERAGE:  start et = ', start_et
      WRITE(*,*) ' COVERAGE:  end_time =',end_time 
C---      CALL ASF2ET(end_time, end_et,IER)
      IER = tc_asf2et(end_time, end_et)
      if(IER .ne. 1) then
          print *,' '
          print *,'coverage.for:  ERROR in end time, near column',
     ?                IER
          print *,'               end time = ', end_time
          print *,'               terminating the run.'
          CALL APS_LOG_MSG_FORTRAN( PROGNAME, APS_ERROR,
     ?            'end time value',
     ?            DO_SYSLOG, DO_PRINT )
          coverage = APS_EXIT_ERROR
          return
      endif
      WRITE(*,*) ' COVERAGE;  end_et = ', end_et
c--port--         WRITE(*,*) ' COVERAGE:  brev =',brev 
c--port--         WRITE(*,*) ' COVERAGE:  erev =',erev 
      WRITE(*,*) ' COVERAGE:  maskid =',maskid 
      WRITE(*,*) ' COVERAGE:  sat =',sat 
      WRITE(*,*) ' COVERAGE:  sensor =',sensor 
      WRITE(*,*) ' COVERAGE:  ISTEP =',ISTEP 
      WRITE(*,*) ' COVERAGE:  IQUICK =',IQUICK 
      WRITE(*,*) ' COVERAGE:  IFILE =',IFILE 
      WRITE(*,*) ' COVERAGE:  REPFLAG = ', REPFLAG
C>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
C>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
C---    NOW GET CYDAYS, CYREVS - INFO FOR SWATH ALGORITHM.  
C---    USE start_et TO PICK UP PHASE DATA FROM THE DB.  
c---    note that we might shade the floating start_et value by a 
c---    millisecond in order to help obtain the phase data.  

c---    pass dbproc by value to C routines:  
      call get_phase_t(%VAL(dbproc), SAT, start_et, 
     ?  PHASE, phase_start, xdum, 
     ?   ndum, phase_orbits, ndum2, 
     ?    CYDAYS,CYREVS,SMAXIS,xdum1, 
     ?     XINCL,ARGPER, ANTARCTIC_MODE, IER)
      print *,'coverage:  from get_phase_t:  PHASE = ', PHASE, 
     ?  ' CYDAYS = ', CYDAYS, 
     ?     ' CYREVS = ', CYREVS 
      print *,'           SMAXIS = ', SMAXIS, ' XINCL = ', XINCL, 
     ?     ' ARGPER = ', ARGPER
      print *,'           phase_start = ', phase_start
      print *,'           phase_orbits = ', phase_orbits
      print *,'           ANTARCTIC_MODE = ', ANTARCTIC_MODE

      IF(IER.NE.0) THEN
C---      add about 1 milliseconds to start_et for a second try.  */
          start_et_search = start_et + 1.0/80000000
          call get_phase_t(%VAL(dbproc), SAT, start_et_search, 
     ?       PHASE, phase_start, xdum, 
     ?       ndum, phase_orbits, ndum2, 
     ?       CYDAYS,CYREVS,SMAXIS,xdum1, 
     ?       XINCL,ARGPER, ANTARCTIC_MODE, IER)
          print *,'coverage:  from get_phase_t:  PHASE = ', PHASE, 
     ?        ' CYDAYS = ', CYDAYS, 
     ?        ' CYREVS = ', CYREVS 
          print *,'           SMAXIS = ', SMAXIS, ' XINCL = ', XINCL, 
     ?        ' ARGPER = ', ARGPER
          print *,'           phase_start = ', phase_start
          print *,'           phase_orbits = ', phase_orbits
          print *,'           ANTARCTIC_MODE = ', ANTARCTIC_MODE
      ENDIF
      IF(IER.NE.0) THEN
          print *,' '
          WRITE(*,*)' ERROR:  DATA NOT FOUND IN phase DB RELATION.  '
          WRITE(*,*)' COULD NOT FIND ORBITAL ELEMENTS FOR SAT'
          WRITE(*,*)' AT THE START TIME OF THE RUN.  '  
          print *,' check the satellite parameter of this run.'
          print *,' SAT = ', SAT
          print *,' If OK, check the phase relation for the satellite',
     ?        ' and START TIME of the run.  '
          print *,' Probably the start time was wrong because the ',
     ?        'phase'
          print *,' relation does not have data in it for that time.'
          print *,' START TIME = ', start_time
          print *,' Now terminating the run.  '
          CALL APS_LOG_MSG_FORTRAN( PROGNAME, APS_ERROR,
     ?         'DATA NOT FOUND IN phase DB RELATION for ' //
     ?         'satellite, start time',
     ?         DO_SYSLOG, DO_PRINT )
          coverage = APS_EXIT_ERROR
          return
      ENDIF

C---  PHASE data was retrieved OK.  
C---  If we are in antarctic mode, that is, if the sensors are 
C---  now left-looking instead of right looking, (due to satellite 
C---  rotation of 180 degrees) then multiply the sensor look angle (ALOOK)
C---  by minus one (-1.0) so that the sensor will look left instead 
C---  of right, but have the same angle between downward and the 
C---  sensor beam center:  
      IF ( ANTARCTIC_MODE(1:1) .EQ. 'Y' ) ALOOK = ALOOK * -1.0

C---  check to see that end_et is within this phase:  
C---  compute the end time of this phase:  
C---      CALL ASF2ET(phase_start,phase_start_et,IER)
      IER = tc_asf2et(phase_start,phase_start_et)
C---    compute the end of the phase via number of orbits and time per orbit.
C---    force floating point, not integer truncation, with this code:  
C--             number of orbits:  
      phase_end_et = phase_orbits
C--             number of orbits multiplied by the time per each orbit::  
      phase_end_et = phase_end_et*CYDAYS/CYREVS
C--             add the start time of the phase to this time:  
      phase_end_et = phase_start_et + phase_end_et
      print *,' phase_end_et = ', phase_end_et
C---      CALL ET2ASF(phase_end_et, phase_end)
      IER = tc_et2asf(%VAL(phase_end_et), phase_end)
      print *,' phase_end = ', phase_end
      if(end_et .GT. phase_end_et) THEN
          run_shortened = 1
          time_cut = end_et - phase_end_et
          end_et = phase_end_et
C---          CALL ET2ASF(end_et, end_time)
          IER = tc_et2asf(%VAL(end_et), end_time)
          PRINT *, 'COVERAGE:  This run had a start time in phase ',
     ?    PHASE(1:1), ' and an end time '
          PRINT *, 'that was beyond the end of that phase.  For',
     ?    ' that reason, this '
          PRINT *, ' run was shortened and ends at the end of that',
     ?    ' phase.  The run '
          PRINT *, ' was shortened by', time_cut, ' days.'    
          PRINT *, ' The new stop time is:  ', end_time
      ENDIF

      if(end_et - start_et .gt. 0.5) then
          print *,' '
          print *,'coverage:  the time period for the run is: ' 
          write (bufdays,9911) int(end_et - start_et + 0.5)
 9911     format(I3) 
          if(bufdays(1:1) .eq. ' ') bufdays = bufdays(2:)
          if(bufdays(1:1) .eq. ' ') bufdays = bufdays(2:)
          if(end_et - start_et .lt. 1.5 ) then 
              bufdays = 'banner "1 DAY"'
          else
              bufdays = 'banner "' 
     ?         // bufdays(1:lastc(bufdays)) // ' DAYS"' // char(0)
          endif
          print *,bufdays(9:lastc(bufdays)-1)
          if(end_et - start_et .gt. 200.5) then
              print *,'coverage:  this run is for over 200 days; '
              print *,'           re-run with an earlier stoptime.'
              print *,'           terminating the run.'
              CALL APS_LOG_MSG_FORTRAN( PROGNAME, APS_ERROR,
     ?            'this run is for over 200 days; re-run ' //
     ?            'with an earlier stoptime',
     ?            DO_SYSLOG, DO_PRINT )
              coverage = APS_EXIT_ERROR
              return
          endif
      endif
      call sleep(1)
c---  name of ephemeris file to read.  
c      --  determine ephemeris file name from satellite, phase name.  
c            fname = $APS_DATA/nof/<sat>_<phase>.EPHM
c          example:  $APS_DATA/nof/E1_A.EPHM
      IBUF = 'APS_DATA/nof/' // 
     ?               SAT // '_' // PHASE(1:1) // '.EPHM' // char(0)

C---  file name only, without path.  for metadata...
      ephm_basename = SAT // '_' // PHASE(1:1) // '.EPHM' // char(0) 
      call tfname(IBUF,fname) 
      WRITE(*,*)'COVERAGE:  ephemeris file =',fname(1:lastc(fname))
      IER = tc_systime2asf(nowtime)

      IF ( IFILE .NE. 0  .OR. IQUICK .NE. 0 ) THEN
C---      OPEN THE COVERAGE FILE.  
c----     this is the coverage file for output.  It is ASCII and is for
c----     you to use when testing.  It used to be an external requirement.
C---      file name only, without path.  for metadata...
          cvrg_basename = SAT // '_' // PHASE(1:1) //'_'//SENSOR//'.COV'
C---      complete file name 
          ibuf = 'APS_DATA/cvrg/' // SAT // '_' // PHASE(1:1) // '_' // 
     ?                  SENSOR // '.COV'
C---      complete file name, with the APS_DATA translated.   
          call tfname(ibuf, filename81)
          lc = lastc(filename81)
          filename81(lc+1:) = char(0)
          print *,'coverage:  output coverage file = ',
     ?                                filename81(1:lastc(filename81))
          call writecheck(filename81, ier)
          if(ier .ne. 0) then
              print *,'coverage:  terminating the run.'
              CALL APS_LOG_MSG_FORTRAN( PROGNAME, APS_ERROR,
     ?        'no permission to write to coverage file ' // 
     ?        filename81(1:lastc(filename81)),
     ?        DO_SYSLOG, DO_PRINT )
              coverage = APS_EXIT_ERROR
              return
          endif
c---      remove the next file if it already exists: 
          rcode = unlink(filename81)
          OPEN(UNIT=81, file=filename81, FORM='FORMATTED',STATUS='NEW')
c---
C---      GET METADATA MESSAGE.  
c--       the metadata message is written from the arguments in the command 
c---      line.  

C---      metadata record.  1st rec in the coverage file:
          IBUF93 = SAT // ' ' // SENSOR // ' ' //
     ?      start_time(1:4)   // ' ' // start_time(6:21)   // ' ' //
     ?      end_time(1:4)   // ' ' // end_time(6:21)   // ' ' //
     ?      nowtime(1:4) // ' ' // nowtime(6:21) // ' ' //
     ?      '2.00  ' //
     ?      ephm_basename(1:LASTC(ephm_basename))
C---      metadata MESSAGE:
C---      consists of the Common header, the Metadata record, and the 
C---      coverage file name without directory path.  
          IBUF157(1:157) = nowtime(1:4) //' '// nowtime(6:21) //' '//
     ?      'CF ACS APS                  ' // 
     ?      IBUF93 // 
     ?      cvrg_basename(1:LASTC(cvrg_basename))
c---      create the metadata message file.  
          ibuf = 'APS_DATA/cvrg/' // SAT // '_' // PHASE(1:1) // '_' // 
     ?       SENSOR // '_COV.MSG'
          call tfname(ibuf, filename99)
          lc = lastc(filename99)
          print *,'coverage:  output metadata message file = ', 
     ?          filename99(1:lc)
          filename99(lc+1:) = char(0)
          call writecheck(filename99, ier)
          if(ier .ne. 0) then
              print *,
     ?'ERROR:  no permission to write to metadata message file ' // 
     ?        filename99(1:lastc(filename99))
              print *,'coverage:  terminating the run.'
              CALL APS_LOG_MSG_FORTRAN( PROGNAME, APS_ERROR,
     ?            'no permission to write to metadata ' //
     ?            'message file ' // filename99(1:lastc(filename99)),
     ?            DO_SYSLOG, DO_PRINT )
              coverage = APS_EXIT_ERROR
              return
          endif
c---      remove the old file if it already exists: 
          rcode = unlink(filename99)
c---      open the metadata message file.  
          open(99, file = filename99, status = 'new')
c---      write the message record to the metadata message file.  
          write(99,9901) IBUF157
 9901     format(A)
C---      WRITE THE METADATA RECORD INTO THE COVERAGE FILE.  
          WRITE(81,9902) IBUF93
 9902     FORMAT(A93)
C--   ENDIF on IFILE and IQUICK
      endif

C---  TPREV IS THE TIME OF THE PREVIOUS EPHEMERIS RECORD.  
      TPREV = 0.0D0
C---  MAKE A REAL TIME STEP FROM ISTEP.  
      TSTEP = ISTEP - 0.99999D0
C---  NOTE THAT TSTEP IS USED TO REJECT A PRINT OF A RECORD.  SINCE THE NEXT 
C---  RECORD CAN BE UP TO A MINUTE AFTER, AND THE TIME STEP IS A MAXIMUM 
C---  ALLOWABLE TIME DIFFERENCE, WE NEED TO DROP ISTEP BY ALMOST 1 MINUTE
C---  WHEN BUILDING TSTEP; BY (1 MIN - 0.0005 SECOND)
C---  SINCE THE NEXT RECORD MIGHT BE ALMOST A MINUTE LATER, WE HAVE TO PRINT
C---  THE CURRENT RECORD IF THE NEXT ONE WILL BE MORE THAT ISTEP MINUTES
C---  AFTER THE PREVIOUSLY PRINTED RECORD.  SO TSTEP IS 0.0005 SEC. MORE THAN
C---  (ISTEP - 1)
C---  CONVERT TIME STEP FROM INTEGER MINUTES TO REAL DAYS: TSTEP
      TSTEP = TSTEP / ( 60.0D0 * 24.0D0 )
C---  TMINIT IS SLIGHTLY MORE THAN 1 MINUTE IN DAY UNITS.  IT IS USED TO 
C---  INCLUDE THE VERY LAST RECORD IN THE DESIRED TIME / REV INTERVAL.  
      TMINIT = 1.00001D0 / (60.0D0 * 24.0D0)
c

c---  translate the file name.  
      ibuf = fname
      call tfname(ibuf, filename80)
      lc = lastc(filename80)
      print *,'coverage:  ephemeris file name = ', filename80(1:lc)
      filename80(lc+1:) = char(0)

      call aps_access(filename80, 'F', ier)
      if(ier .ne. 0) then
          print*,' '
          print*,' '
          print*, 
     ?        'coverage:  ERROR:  nominal orbit file does not exist:'
          rcode = system('ls -l ' // 
     ?        filename80(1:lastc(filename80)) // char(0) )
          print *, 
     ?        '          you must run create nominal orbit for sat ', 
     ?        SAT, ' and phase ', PHASE(1:1)
          print *,'coverage:  terminating the run.'
          CALL APS_LOG_MSG_FORTRAN( PROGNAME, APS_ERROR,
     ?        'input nominal orbit file does not exist:  ' //
     ?        filename80(1:lastc(filename80))                      // 
     ?        '  You must run create nominal orbit for satellite ' // 
     ?        SAT // ' phase ' // PHASE,
     ?        DO_SYSLOG, DO_PRINT )
          coverage = APS_EXIT_ERROR
          return
      endif
      call aps_access(filename80, 'r', ier)
      if(ier .ne. 0) then
          print*,' '
          print*,' '
          print*,
     ?     'coverage:  ERROR:  access to file denied.  r permission ',
     ?                         'needed for file:'
          rcode = system('ls -l ' // 
     ?                  filename80(1:lastc(filename80)) // char(0) )
          print *,'coverage:  terminating the run.'
          CALL APS_LOG_MSG_FORTRAN( PROGNAME, APS_ERROR,
     ?        'no read permission for nominal orbit file:  ' //
     ?        filename80(1:lastc(filename80)),
     ?        DO_SYSLOG, DO_PRINT )
          coverage = APS_EXIT_ERROR
          return
      endif

C-    OPEN the ephemeris file to read it.  
      EUNIT = 80
      OPEN (UNIT = EUNIT, FILE = filename80, STATUS = 'old',
     &         FORM = 'unformatted')
C---  FIRST WORD IS THE EPOCH.
      REWIND(EUNIT)
      READ(EUNIT)epoch
      WRITE(*,*) ' COVERAGE:  epoch =',epoch 
C---      CALL ET2ASF(epoch,epochasf)
      IER = tc_et2asf(%VAL(epoch), epochasf)
      WRITE(*,*) ' COVERAGE:  epochasf    =', epochasf
      WRITE(*,*) ' COVERAGE:  phase_start =', phase_start
C---     Check the phase relation data vs the Ephemeris file.  
C---     Compare start of ephemeris file with start of phase.  
C---     This is a gross check.  The phase start (if the phase is 
C---     really long and it started more than 19 months before 
C---     the start of the current time bracket) may be outside 
C---     of the Stoic File containing time coversion data.  
      IF ( ABS(phase_start_et - epoch) .GT. 0.0010 ) THEN
          WRITE(*,*) ' '
          WRITE(*,*) 
     ?    'ERROR:  phase relation and ephemeris file do not agree.' 
          WRITE(*,*) 
     ?    '        The start time of the phase and file are not equal.'
          WRITE(*,*) 
     ?    '        You must re-run create nominal orbit for sat ', 
     ?      SAT, ' and phase ', PHASE(1:1)
          print *,'coverage:  terminating the run.'
          CALL APS_LOG_MSG_FORTRAN( PROGNAME, APS_ERROR,
     ?        'phase relation and ephemeris file do not ' // 
     ?        'agree; You must re-run create nominal orbit for sat ' // 
     ?        SAT // ' phase ' // PHASE,
     ?        DO_SYSLOG, DO_PRINT )
          coverage = APS_EXIT_ERROR
          return
      ENDIF

      WRITE(*,*) 'COVERAGE:  phase relation and ephemeris file agree.'

c---  Insert the CVRGMDAT record (coverage metadata) into the database.
c---  NOTE:
c---  in the original VAX-VMS version, this was done by the user interface.
c---  pass dbproc by value to C routines:  
      IF(IQUICK .NE. 1) call cvrg_metadata(%VAL(dbproc), nowtime, 
     ?     SAT, SENSOR, start_time, 
     ?     end_time, maskid, REPFLAG, 
     ?     ephm_basename(1:lastc(ephm_basename)) // char(0), marker)

C---  Expand the Run Time bracket (ET) by .001 seconds 
C---  TO BE SURE TO PICK UP THE VERY FIRST 
C---  EPHEMERIS STATE VECTOR.  used in call to TIMES2R and Rgetx.
C---  This is a measure to insure that we get what we want when using 
c---  floating point numbers for comparison.  This is done for for 
c---  both start_et and end_et.  
      start_et_expanded = start_et - 1.16D-8

C---  ADD 0.001 SECONDS TO BE SURE TO PICK UP THE VERY LAST
C---  EPHEMERIS STATE VECTOR.  used in call to TIMES2R and Rgetx.
      end_et_expanded = end_et + 1.16D-8
      WRITE(*,*)
     ?' COVERAGE:  SAT, SENSOR, start_et_expanded, end_et_expanded='
      WRITE(*,*)
     ?'            ', SAT, SENSOR, start_et_expanded, end_et_expanded
      WRITE(*,*)'            IQUICK = ', IQUICK
      WRITE(*,*)'            ISTEP (MIN) = ', ISTEP
      WRITE(*,*)'            TSTEP (DAYS) = ', TSTEP
C---  IF IQUICK = 1 THEN DO NOT WRITE TO THE DB.  
      IF(IQUICK .EQ. 1) GO TO 2000
c---  obtain the rev numbers corresponding to the coverage run, 
c---  to help out in the speed of the delete. 
      CALL TIMES2R(dbproc,1,SAT,start_et_expanded,' ',end_et_expanded,
     ?             ' ',IREVD1, IREVD2,IER)
      WRITE(*,*) 
     ?  ' COVERAGE:  Starting to delete cvrg records'
      WRITE(*,*) '            IREVD1, IREVD2 = ', IREVD1, IREVD2
      WRITE(*,*) '            start_et_expanded = ', start_et_expanded
      WRITE(*,*) '            end_et_expanded = ', end_et_expanded

C---  NOW DELETE ALL COVERAGE RECORDS FOR THIS SAT/SENSOR THAT IS 
C---  WITHIN THE TIMES OF THIS RUN.  I.E., BETWEEN 
C---  start_et_expanded and end_et_expanded.  
c---  delete cvrg records:
c---  pass dbproc by value to C routines:  
c---  the '*' in the parameter list indicates to delete cvrg 
c---  from all station masks.  
      call delete_cov(%VAL(dbproc), SAT, SENSOR, 
     ?    IREVD1, IREVD2, start_et_expanded, end_et_expanded, '*', 
     ?    n_cvrg )

      WRITE(*,*) ' COVERAGE:  #CVRG RECS DELETED = ', n_cvrg
 2000 CONTINUE
C---  NOW COMPUTE NODAL PERIOD:  
      TNODE = CYDAYS
      TNODE = TNODE / CYREVS
      WRITE(*,*)' COVERAGE:  NODAL PERIOD TNODE (DAYS) = ', TNODE
C---  CONVERT DEGREES TO RADIANS.  
      XINCL = XINCL / DG2RAD
      ARGPER = ARGPER / DG2RAD

C---     set up conditions for the start of this read loop:  
      x(7) = 0.0d0
      x(8) = 0.0d0
      done = .FALSE.
      rewind (EUNIT)
C----
C----  The nominal ephemeris file contains one record per minute 
C----  for the first rev in a phase.  
C----
C----  This part of the program tries to go to the record that 
C----  corresponds to the desired start time of the run.  Then 
C----  read successive records, cycling from the last rec to the 
C----  first record.  
C----  
C----  When reading an ephemeris file record, the rev, times, 
C----  and longitudes are shifted, according to the orbit repeat 
C----  cycle, in order to match the desired rev and times.  
C----  
C----  
      DO WHILE ((x(7) .LE. start_et_expanded) .OR. (done))
C         skip records until the selected begin time
C---      REPFLAG .EQ. 'Y' MEANS USE REPLICATION TO READ THE EPHM FILE.
          IF(REPFLAG .EQ. 'Y')
     ?         done = rgetx(0,EUNIT,epoch,
     ?                start_et,end_et,TNODE,x,nodeflag)
          IF (FIRSTGETX) THEN
              FIRSTGETX = .FALSE.
              WRITE(*,*)' COVERAGE:  FIRST RECORD FROM EPHEMERIS FILE:'
              WRITE(*,*)'          NODEFLAG = ', NODEFLAG
              WRITE(*,*)'          X = ', X
          ENDIF
      ENDDO
      IREV = X(8)
      WRITE(*,*)' COVERAGE:  NOW GETTING STATION DATA'
C---  ASF station data:
C---  NOW GET ASFLAT, ASFLON, ASFRAD:  LAT/LON, RADIUS(KM) OF THE ASF MASK 
C---  USED AS DATA FOR DATA TAKE FEASABILITY.  
      NRECS_FOUND = get_station(%VAL(dbproc), SAT, 'ASF',
     ?                       ASFLAT, ASFLON, ASFRAD, ASFRMBUF)
      IF( NRECS_FOUND .NE. 1 )  THEN
C---      there is no station record for ASF for this satellite.  
C---      disable the ASF mask code via a non-zero, tiny radius:
          ASFRAD = 0.0000001 
          ASFLAT = 90.0
          ASFLON = 0.0
          ASFRMBUF = 0.0
      ENDIF
      ASFRMRAD = ASFRAD + ASFRMBUF
      WRITE(*,*)' COVERAGE: ASFLAT, ASFLON, ASFRAD=',
     ?                      ASFLAT, ASFLON, ASFRAD
      WRITE(*,*)'           ASFRMBUF, ASFRMRAD=',
     ?                      ASFRMBUF, ASFRMRAD

C---  MCM station data:
C---  NOW GET MCMLAT, MCMLON, MCMRAD:  LAT/LON, RADIUS(KM) OF THE MCM MASK 
C---  USED AS DATA FOR DATA TAKE FEASABILITY.  
      NRECS_FOUND = get_station(%VAL(dbproc), SAT, 'MCM',
     ?                       MCMLAT, MCMLON, MCMRAD, MCMRMBUF)
      IF( NRECS_FOUND .NE. 1 )  THEN
C---      there is no station record for MCM for this satellite.  
C---      disable the MCM mask code via a zero radius:
          MCMRAD = 0.0000001 
          MCMLAT = 90.0
          MCMLON = 0.0
          MCMRMBUF = 0.0
      ENDIF
      MCMRMRAD = MCMRAD + MCMRMBUF
      WRITE(*,*)' COVERAGE: MCMLAT, MCMLON, MCMRAD=',
     ?                      MCMLAT, MCMLON, MCMRAD
      WRITE(*,*)'           MCMRMBUF, MCMRMRAD=',
     ?                      MCMRMBUF, MCMRMRAD

      IF (maskid .EQ. 'GBL') THEN
C---      this is a global run.  no run mask to reduce output.  
C---      set the mask radius to zero, indicating lack of mask.  
          MCMRMRAD = 0
          ASFRMRAD = 0
      ENDIF
c
c        Loop until stop criteria Are met or EOF
c
      print *,' '
      if(end_et - start_et .gt. 0.5) then
          print *,' '
          print *,' '
          print *,' '
          print *,' '
          print *,' '
          print *,' '
          print *,' '
          print *,' '
          print *,' '
          print *,' '
          print *,' '
          print *,'This coverage run is for:'
          rcode = system(bufdays)
      endif
      rcode = system('date')
      IF(IQUICK .EQ. 1) print *,'coverage.for:  this is a QUICK run;',
     ?        '  ASCII file is written instead of DB data.'
      IF(IFILE .EQ. 1) print *,'coverage.for:  this is a FILE run;',
     ?        '  records will also be written to the ASCII file.'
      print *,'coverage.for:  starting to generate coverage records ',
     ?        'for ', sat, ' ', sensor, ' ', maskid, '.'
      if ( ANTARCTIC_MODE(1:1) .EQ. 'Y' ) print *,'ANTARCTIC MODE'
      print *,'               start = ', start_time
      print *,'               end   = ', end_time
      time_count = 0
      DO WHILE ( x(7).LT.end_et_expanded .AND. (.NOT.done)   )
          if(mod(time_count, 10) .eq. 9) 
     ?      call progress(x(7),start_et,end_et)
          time_count = time_count + 1
c
c         check to see if a mask boundary has been crossed if yes then interp
c         check to see if current record is in user mask if yes then write 
c         check for node crossing if yes then interp
c         save record for interp
c
          CALL Swath(sat, sensor, IMODE, SMAXIS,XINCL, ARGPER,
     ?          ALOOK,BEAMW,CYDAYS, CYREVS, epoch, x, 
     ?          xc, xsslat, xsslon, xnlat, xnlon, xflat, xflon)
          NOW_IN_ASF = IN_CIRCLE(xsslat,xsslon,ASFLAT,ASFLON,ASFRAD) 
          NOW_IN_MCM = IN_CIRCLE(xsslat,xsslon,MCMLAT,MCMLON,MCMRAD) 
C
          IF ( IN_CIRCLE(xsslat, xsslon, ASFLAT, ASFLON, ASFRMRAD) 
     ?      .OR. IN_CIRCLE(xsslat, xsslon, MCMLAT, MCMLON, MCMRMRAD) ) 
     ?    THEN
C---          WITHIN THE RUN MASK.  
C---          WRITE CORRENT RECORD.  

C---          Check for interpolated records.  print them first.  
              IF (firsttime) THEN
                  firsttime = .FALSE.
              ELSE
C--               ASF mask entry check
                  IF (NOW_IN_ASF .AND. .NOT. PREV_IN_ASF) THEN  
C--                   Entered mask - interpolate mask entry record. 
                      CALL Minterp(SAT,ASFLAT,ASFLON,ASFRAD,savevec,x,y)
                      CALL Swath(sat, sensor, IMODE, SMAXIS,XINCL, 
     ?                    ARGPER, ALOOK,BEAMW,CYDAYS, CYREVS, epoch, Y, 
     ?                    yc, ysslat, ysslon, ynlat, ynlon, yflat, 
     ?                    yflon)
                      nflag = '-'
C--                   station_mask flag:
                      amask = 'A'
                      CALL CovFormat(dbproc,IQUICK,IFILE,'IN',Y, 
     ?                    ynlat, ynlon, yflat, yflon, yc, 
     ?                    marker, nflag, amask, sat, sensor, 
     ?                    NC )
                      NRECSC = NRECSC + NC
                  ENDIF
C--               ASF mask exit check
                  IF (PREV_IN_ASF .AND. .NOT. NOW_IN_ASF) THEN        
C--                   Left mask - interpolate mask exit record.  
                      CALL MINTERP(SAT,ASFLAT,ASFLON,ASFRAD,x,savevec,Y)
                      CALL Swath(sat, sensor, IMODE, SMAXIS,XINCL, 
     ?                    ARGPER, ALOOK,BEAMW,CYDAYS, CYREVS, epoch, Y, 
     ?                    yc, ysslat, ysslon, ynlat, ynlon, yflat, 
     ?                    yflon)
                      nflag = '-'
C--                   station_mask flag:
                      amask = 'A'
                      CALL CovFormat(dbproc,IQUICK,IFILE,'OUT',Y, 
     ?                    ynlat, ynlon, yflat, yflon, yc, 
     ?                    marker, nflag, amask, sat, sensor, 
     ?                    NC )
                      NRECSC = NRECSC + NC
                  ENDIF

C--               MCM mask entry check
                  IF (NOW_IN_MCM .AND. .NOT. PREV_IN_MCM) THEN  
C--                   Entered mask - interpolate mask entry record. 
                      CALL Minterp(SAT,MCMLAT,MCMLON,MCMRAD,savevec,x,y)
                      CALL Swath(sat, sensor, IMODE, SMAXIS,XINCL, 
     ?                    ARGPER, ALOOK,BEAMW,CYDAYS, CYREVS, epoch, Y, 
     ?                    yc, ysslat, ysslon, ynlat, ynlon, yflat, 
     ?                    yflon)
                      nflag = '-'
C--                   station_mask flag:
                      amask = 'M'
                      CALL CovFormat(dbproc,IQUICK,IFILE,'IN',Y, 
     ?                    ynlat, ynlon, yflat, yflon, yc, 
     ?                    marker, nflag, amask, sat, sensor, 
     ?                    NC )
                      NRECSC = NRECSC + NC
                  ENDIF
C--               MCM mask exit check
                  IF (PREV_IN_MCM .AND. .NOT. NOW_IN_MCM) THEN        
C--                   Left mask - interpolate mask exit record.  
                      CALL MINTERP(SAT,MCMLAT,MCMLON,MCMRAD,x,savevec,Y)
                      CALL Swath(sat, sensor, IMODE, SMAXIS,XINCL, 
     ?                    ARGPER, ALOOK,BEAMW,CYDAYS, CYREVS, epoch, Y, 
     ?                    yc, ysslat, ysslon, ynlat, ynlon, yflat, 
     ?                    yflon )
                      nflag = '-'
C--                   station_mask flag:
                      amask = 'M'
                      CALL CovFormat(dbproc,IQUICK,IFILE,'OUT',Y, 
     ?                    ynlat, ynlon, yflat, yflon, yc, 
     ?                    marker, nflag, amask, sat, sensor, 
     ?                    NC )
                      NRECSC = NRECSC + NC
                  ENDIF
              ENDIF
C--           endif on checking for mask entry or exit.  


C---          WITHIN THE RUN MASK.  
C---          WRITE CORRENT RECORD.  
              amask = '-'
              IF(NOW_IN_ASF) amask = 'A'
              IF(NOW_IN_MCM) amask = 'M'
C---          IF THIS IS A NORMAL COVERAGE RUN OR IF THE USER
C---          HAS REQUESTED A "NORMAL" TIME STEP, THEN 
C---          PRINT THIS RECORD IN THE COVERAGE FILE.
C---          ALWAYS PRINT THE ASCENDING NODE.
C---          PRINT IF ENOUGH TIME HAS ELAPSED.  
              IF(     IQUICK .EQ. 0 
     ?        .OR. ISTEP .EQ. 1
C---          MUST PRINT THE ASCENDING NODE.  
     ?        .OR. nodeflag .EQ. 'N' 
C---          MUST PRINT IF WE ARE IN THE FINAL MINUTE.  
     ?        .OR. (end_et_expanded - x(7) .LE. TMINIT)
     ?        .OR. (x(7)-TPREV) .GE. TSTEP) THEN
                  CALL CovFormat(dbproc, IQUICK, IFILE,'-',x, 
     ?                xnlat, xnlon, xflat, xflon,
     ?                xc,marker,nodeflag,amask,sat, sensor, NC )
                  NRECSC = NRECSC + NC
C---              SET TIME OF THE MOST RECENT PRINT.  
                  TPREV = X(7)
              ENDIF
          ELSE  ! NOT IN THE RUN MASK:
              IF (nodeflag .EQ. 'N') THEN
c---                 Write (CURRENT) ascending node record
                  amask = '-'
                  IF(NOW_IN_ASF) amask = 'A'
                  IF(NOW_IN_MCM) amask = 'M'
                  CALL CovFormat(dbproc, IQUICK, IFILE,'-',x,
     ?                  xnlat,xnlon,xflat,xflon,
     &                           xc, marker, nodeflag, amask, sat, 
     &                           sensor, NC )
                  NRECSC = NRECSC + NC
              ENDIF
          ENDIF
c
c         Set values aside for interpolation
c
          DO 10, J=1, 12
               savevec(J) = x(J)
 10       CONTINUE
          ssslat = xsslat
          ssslon = xsslon
          snlat = xnlat
          snlon = xnlon
          sflat = xflat
          sflon = xflon
C---      Save previous mask status.  
          PREV_IN_ASF = NOW_IN_ASF
          PREV_IN_MCM = NOW_IN_MCM
          IF (firsttime) THEN
              firsttime = .FALSE.
          ENDIF
C---      REPFLAG .EQ. 'Y' MEANS USE REPLICATION TO READ THE EPHM FILE.
          IF(REPFLAG .EQ. 'Y')
     ?        done = rgetx(0,EUNIT,epoch, 
     ?                     start_et,end_et,TNODE,x,nodeflag)
          IF(REPFLAG .NE. 'Y')done = Getx(x, nodeflag)
      ENDDO  !  End loop on records in file
c
      CLOSE (UNIT = EUNIT)
      WRITE(*,*)' COVERAGE:  # cvrg      RECS APPENDED = ', NRECSC
      if(run_shortened .eq. 1) then
          PRINT *, 'COVERAGE:  This run had a start time in phase ',
     ?    PHASE(1:1), ' and an end time '
          PRINT *, 'that was beyond the end of that phase.  For',
     ?             ' that reason, this '
          PRINT *, ' run was shortened and ends at the end of that',
     ?             ' phase.  The run '
          PRINT *, ' was shortened by', time_cut, ' days.'    
          PRINT *, ' The stop time used in the run:  ', end_time
      endif
      IF(IFILE .EQ. 1) print *,'coverage.for:  this is a FILE run;',
     ?        '  ascii file is written in addition to the database.'
      IF(IQUICK .EQ. 1) print *,'coverage.for:  this is a QUICK run;',
     ?        '  ASCII file instead of database data is written.'
      print *,'coverage.for:  completed generating coverage records ',
     ?        'for ', sat, ' ', sensor, ' ', maskid, '.'
      IF ( ASFRAD .LE. 0.00001 .AND. ASFRAD .GT. 0.00000001 ) 
     ? print *,'No enter/exit mask data generated for ASF in this run.'
      IF ( MCMRAD .LE. 0.00001 .AND. MCMRAD .GT. 0.00000001 ) 
     ? print *,'No enter/exit mask data generated for MCM in this run.'
      if ( ANTARCTIC_MODE(1:1) .EQ. 'Y' ) print *,'ANTARCTIC MODE'
      print *,'               start = ', start_time
      print *,'               end   = ', end_time
      IF ( IFILE .NE. 0  .OR.  IQUICK .NE. 0 ) THEN
c---      change protections to allow for other users to use the output files.  
          call aps_chmod(filename81, 'RW', ier)
          call aps_chmod(filename99, 'RW', ier)
          print *,' ' 
          print *,'    Coverage File: ' 
          ibuf = 'ls -l ' // filename81(1:lastc(filename81))
          rcode = system(ibuf)
          print *,' ' 
          print *,'    Coverage Message File:' 
          ibuf = 'ls -l ' // filename99(1:lastc(filename99))
          rcode = system(ibuf)
      endif

c---      WRITE(*,*)' COVERAGE:  PROGRAM TERMINATING NORMALLY.'
      print *,' ' 
      rcode = system('date')
      coverage = APS_EXIT_OK
      return
      END


      SUBROUTINE Cart2ll(X, XLAT, XLON)
c Calculates lat/lons from cartesian coords,  Glat is found with
c ERS_Swath source file.  
      IMPLICIT NONE
      REAL*8 X(3), XLAT, XLON
      REAL*8 STUFF, GLAT
      REAL*8 Glatd, Glond
      REAL*8  Clatd, Rr
      REAL*8   Hite
      REAL*8   Trgf
      COMMON /Com_Gdlat/ Glatd, Glond, Clatd, Rr(3), Hite, Trgf(6)
c
      STUFF = Glat(X, 0)
      XLAT = Glatd
      XLON = Glond
      RETURN
      END

      LOGICAL FUNCTION IN_CIRCLE(PLAT,PLON,CLAT,CLON,CRAD)
********************************************************************
*  Name:
*  Module Type: subroutine      Language: fortrat
*  Purpose:
*       determine of the point PLAT/PLON is in the circle CLAT, CLON, CRAD.
*  Menu Options:
*  Functions called:
*  Input Parameters:
*  Name         Type    Definition
*  PLAT                 real*8  latitude of input point.
*  PLON                 real*8  longitude of input point.
*  CLAT                 real*8  latitude of center of circle
*  CLON                 real*8  longitude of center of circle
*  CRAD                 real*8  radius in km of circle.
*  Output Parameters:
*  Variables:
*  Locals :
*  Externals :
*  Modification History:                                            
*  Date                 Revision        Author
*  $Date$ $Revision$ $Author$
*********************************************************************/
C RETURNS TRUE IF THE POINT IS ON OR INSIDE THE CIRCLE.  
      IMPLICIT NONE
      REAL*8 PLAT, PLON, CLAT, CLON, CRAD
      REAL*8 PLL(2), CLL(2), R
      REAL*8 P(3),   C(3), D
c--port--      INCLUDE 'MPS_LIB:MPS_CONST_EARTH.FOR/LIST'
      INCLUDE 'APS_HOME:include/local/mps_const_earth.inc'
C---    KM TO RADIANS.  USE EARTH RADIUS IN KM.  - - - - - - 
CCC      WRITE(*,*)' IN_CIRCLE start:  PLAT,PLON,CLAT,CLON,CRAD=',
CCC     ?       PLAT,PLON,CLAT,CLON,CRAD
      IF(CRAD .GT. 0) GO TO 1000
C---    IF RADIUS = 0, THEN WE RETURN A TRUE VALUE.  NO RESTRICTION.
CCC      WRITE(*,*)' TRUE'
      IN_CIRCLE = .TRUE.
      GO TO 9999
 1000 CONTINUE
      R = CRAD / ERAD
      PLL(1) = PLAT
      PLL(2) = PLON
      CLL(1) = CLAT
      CLL(2) = CLON
      CALL LL2XYZ(PLL,P)
      CALL LL2XYZ(CLL,C)
      CALL GCDIST(D,P,C)
CCC      WRITE(*,*)' D, R=',D,R
      IF(D .LE. R) THEN
         IN_CIRCLE = .TRUE.
CCC      WRITE(*,*)' TRUE'
      ELSE
         IN_CIRCLE = .FALSE.
CCC      WRITE(*,*)' FALSE'
      ENDIF
 9999 CONTINUE
CCC      WRITE(*,*)' IN_CIRCLE:  end'
      END
      SUBROUTINE Minterp(SAT,ASFLAT, ASFLON, ASFRAD, xout, xin, Y)
********************************************************************
*  Name:Minterp
*  Module Type: subroutine      Language: f77
*  Purpose: compute a state vector at mask entry using 2 state vectors.  
*  Functions called:
*  Input Parameters:
*  Name         Type    Definition
*  SAT          REAL*8  SATELLITE
*  ASFLAT       REAL*8  Latitude of station
*  ASFLON       REAL*8  Longitude of station
*  ASFRAD       REAL*8  Radius(km) of station mask for this satellite.
*  xout(12)     REAL*8  state vector outside station mask
c       x(1), x(2), x(3)        real*8  (x, y, z) position
c       x(4), x(5), x(6)        real*8  (x, y, z) velocity
c       x(7)                    real*8  Time, ephemeris in Julian days
c       x(8)                    real*8  rev number of state vector
c       x(9)                    real*8  Longitude of ascending node
c       x(10)                   real*8  Subsat lat, geocentric
c       x(11)                   real*8  Subsat lon, geocentric
c       x(12)                   real*8  Greenwich Hour Angle
*  xin(12)      REAL*8  state vector inside station mask
*  Output Parameters:
*  Y            REAL*8  State vector at entrance to station mask.  
*  Variables:
*  Locals :
*  Externals :
*  Modification History:                                            
*  Date                 Revision        Author
*  $Date$ $Revision$ $Author$
*********************************************************************/
c--------      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      IMPLICIT NONE
      CHARACTER*(*) SAT
      REAL*8 ASFLAT, ASFLON, ASFRAD, XOUT(12), XIN(12), Y(12)
      REAL*8 xlat, xlon, zlat, zlon, b, a, FRAC
      CALL Cart2ll(XOUT, xlat, xlon)
c---    adjust to PM based longitude
      xlon = xlon - XOUT(12)
      CALL Cart2ll(XIN, zlat, zlon)
c---    adjust to PM based longitude
      zlon = zlon - XIN(12)
C---    SEND THE SAT NAME AND THE TIME OF THE POINT INSIDE THE CIRCLE.
      CALL ENTCIR(SAT,xin(7),
     ?  xlat,xlon,zlat,zlon,ASFLAT,ASFLON,ASFRAD,FRAC)
C---    FRAC IS THE FRACTION OF THE DISTANCE BETWEEN XOUT AND XIN TO GET TO 
C---    THE EDGE OF THE CIRCLE.  
      b = FRAC
      a = 1.0D0 - b
      CALL VINTRD(a,b,XOUT,XIN,Y)
C---    NOW Y IS THE INTERPOLATED VECTOR AT THE EDGE OF THE CIRCLE.  
      RETURN
      END
********************************************************************
*  Name:        SWATH
*  Module Type: frame   Language: FORTRAN
*  Purpose:     COMPUTES SWATH POINTS 
*  Menu Options:
*  Functions called:
*  Input Parameters:
*  Name         Type    Definition
*  sat          CH 2    SAT
*  sensor       CH 3    SENSOR
*  IMODE        INT     Mode of satellite orientation.  
*                               0 = Yaw Steering
*                               1 = Earth center orientation
*  SMAXIS       REAL*8  mean semimajor axis (km) of current orbit 
*  XINCL        REAL*8  mean inclination(RAD) of current orbit
*  ARGPER       REAL*8  mean argument of periapsis(RAD)of current orbit
*  ALOOK        REAL*8  look angle of sensor
*  BEAMW        REAL*8  beam radius (radians ) of sensor.
*  CYDAYS       REAL*8  # days in 1 cycle
*  CYREVS       REAL*8  # revs in 1 cycle
*  epoch        REAL*8  EPOCH for coord system of x.  
*                       EPHEMERIS TIME IN JULIAN DAYS
*  x(12)        REAL*8  state vector 
*  OUTPUT PARAMETERS:
*  xc(3)        REAL*8  xyz of swath center point
*  xsslat       REAL*8  subsat lat
*  xsslon       REAL*8  subsat lon
*  xnlat        REAL*8  lat of near swath point
*  xnlon        REAL*8  lon of near swath point
*  xflat        REAL*8  lat of far swath point
*  xflon        REAL*8  lon of far swath point
*  Variables:
*  Locals :
*  Externals :
*  Modification History:                                            
*  Date                 Revision        Author
*  $Date$ $Revision$ $Author$
*********************************************************************/
c STATE VECTORS are in EARTH EQUATOR EQUINOX OF EPOCH
c ***********************************************************************
      SUBROUTINE Swath(sat, sensor, IMODE, SMAXIS,XINCL,ARGPER,
     ?          ALOOK,BEAMW,CYDAYS, CYREVS, epoch, x, xc, 
     ?          xsslat, xsslon, 
     ?          xnlat, xnlon, xflat, xflon)
      IMPLICIT NONE
      CHARACTER*2 sat
      CHARACTER*3 sensor
      INTEGER IMODE
      REAL*8 XNLAT, XNLON, XFLAT, XFLON, XSSLAT, XSSLON
      REAL*8 SMAXIS,XINCL,ARGPER
      REAL*8 TRUEAN, XOM
      REAL*8 x(12), xn(3), xc(3), xf(3), ss(3), vv(3),
     &          alfa3(3), EPOCH, ORBDAY, ALOOK, BEAMW
      INTEGER CYDAYS, CYREVS, J
c--port--      INCLUDE 'MPS_LIB:MPS_CONST_MATH.FOR'
c--port--      INCLUDE 'MPS_LIB:MPS_CONST_EARTH.FOR'
      INCLUDE 'APS_HOME:include/local/mps_const_math.inc'
      INCLUDE 'APS_HOME:include/local/mps_const_earth.inc'
c  USE EARTH constants GME, ERAD, FLAT
c
      orbday = CYREVS
      orbday = orbday / CYDAYS
C--
C--  USE MATH constant DGRAD
C---  ALFA3 is an array with the degrees in radians.  
C---  ALFA(1)    is the angle from nadir to spacecraft to sensor beam near edge
C---  ALFA(2)    is the angle from nadir to spacecraft to sensor beam far edge
C---  ALFA(3)    is the angle from nadir to spacecraft to sensor beam center
C---  Note that if the sensor is left looking ( ALOOK < 0 ), then 
C---  the near and far angles must be computed differently compared to right 
C---  looking ( ALOOK > 0 ), in order to maintain the near/far definition.  
C---  ALFA3 drives the computations of the near and far swath points.  
      IF( ALOOK .GE. 0) THEN
          alfa3(1) = (ALOOK - BEAMW)/DG2RAD
          alfa3(2) = (ALOOK + BEAMW)/DG2RAD
          alfa3(3) = ALOOK/DG2RAD
      ELSE
C---      Left-looking sensor ALOOK < 0 
          alfa3(1) = (ALOOK + BEAMW)/DG2RAD
          alfa3(2) = (ALOOK - BEAMW)/DG2RAD
          alfa3(3) = ALOOK/DG2RAD
      ENDIF
C---    GET CURRENT TRUE ANOMALY IN RADIANS - independent of coord sys.
      CALL GETRUE(X,ARGPER,TRUEAN)
      XOM = X(9) 
C---    CONVERT LONGITUDE OF ASCENDING NODE TO RADIANS FOR INPUT TO ERS_SWATH.  
      XOM = XOM / DG2RAD
      DO 4, J=1, 3
         ss(J) = x(J)
         vv(J) = x(J+3)
 4    CONTINUE
C---    XOM, ss, AND vv are in EARTH EQUATOR EQUINOX OF EPOCH COORDINATES.
C---    Swath_compute() is located in ersswath.for in lib_APSmath:
      CALL Swath_compute(SAT, IMODE, SMAXIS, XINCL, XOM, ARGPER, TRUEAN,
     ?                  ss, ORBDAY, ALFA3, 
     &               xn, xf, xc, 0)
C---    THESE LAT/LONS ARE DEVELOPED FROM THE EQUINOX-BASED COORDS.
      CALL Cart2ll(xn, xnlat,   xnlon)
      CALL Cart2ll(xf, xflat,   xflon)
      CALL Cart2ll(ss, xsslat, xsslon)
C---    NOW SUBTRACT THE GREENWICH HOUR ANGLE FROM THE LONGITUDES TO GET
C---    THE PM-BASED LONGITUDES.  
      xnlon  = xnlon -  X(12)
      IF (xnlon .LE. -180.0d0) xnlon = xnlon + 360.0d0
      xflon  = xflon -  X(12)
      IF (xflon .LE. -180.0d0) xflon = xflon + 360.0d0
      xsslon = xsslon - X(12)
      IF (xsslon .LE. -180.0d0) xsslon = xsslon + 360.0d0
      RETURN
      END
c
c
      LOGICAL FUNCTION Getx(EUNIT,x, nodeflag)
c
c  Reads State vector and returns true on EOF
c
      IMPLICIT DOUBLE PRECISION (a-h, o-z)
c--port--      REAL*8 MTH$CVT_G_D
      DIMENSION x(12)
      CHARACTER nodeflag
          INTEGER EUNIT
c
      READ (UNIT = EUNIT, END = 1000) nodeflag, (x(i), i=1, 12)
      IF(nodeflag .EQ. '_' ) nodeflag = '-'
c--port--      do 2, ii=1, 12
c--port--         x(ii) = MTH$CVT_G_D(x(ii))
c--port-- 2    continue
      Getx = .FALSE.
      RETURN
 1000 Getx = .TRUE.
      RETURN
      END
********************************************************************
*  Name:        GETRUE
*  Module Type: SUBROUTINE      Language: FORTRAN
*  Purpose:     COMPUTES TRUE ANOMALY FROM A STATE VECTOR AND ARGUMENT OF 
*               PERIAPSIS.  
*  Functions called:
*  VECTOR LIBRARY:  
*  Input Parameters:
*  Name         Type    Definition
*  X(12)        REAL*8  STATE VECTOR FROM EPHEMERIS PROGRAM
*  ARGPER       REAL*8  ARGUMENT OF PERIAPSIS IN RADIANS.  
*  Output Parameters:
*  Name         Type    Definition
*  TRUEAN       REAL*8  TRUE ANOMALY IN RADIANS
*  Variables:
*  Locals :
*  Externals :
*  Modification History:                                            
*  Date                 Revision        Author
*  $Date$ $Revision$ $Author$
*                                                                   
*********************************************************************/
      SUBROUTINE GETRUE(X,ARGPER,TRUEAN)
      IMPLICIT NONE
      REAL*8 X(12), ARGPER, TRUEAN
      REAL*8 SU(3), AU(3), ANG, COSDOT, PI2
C---    NOTE USE OF REAL*4 FOR AVOIDING COSDOT > 1.0.  
      REAL*4 COS4
c--port--      INCLUDE 'MPS_LIB:MPS_CONST_MATH.FOR/LIST'
      INCLUDE 'APS_HOME:include/local/mps_const_math.inc'
c       x(1), x(2), x(3)        real*8  (x, y, z) position KM
c       x(9)                    real*8  Longitude of ascending node(DEG)
C---    GET UNIT VECTOR OF SAT VECTOR.
      CALL VUNIT(SU,X,3)
C---    GET UNIT VECTOR IN DIRECTION OF ASCENDING NODE
      AU(1) = COSD(X(9))
      AU(2) = SIND(X(9))
      AU(3) = 0
C---    GET DOT PRODUCT = COS(ANGLE) BETWEEN NODE AND SAT POSITION.
      COSDOT = SU(1)*AU(1) + SU(2)*AU(2) + SU(3)*AU(3)
C---    NOW ROUND OFF TO REAL*4 TO AVOID ARITHMETIC ARGUMENT ERROR.  
      COS4 = COSDOT
C---    GET ANGLE IN RADIANS.  
      ANG = ACOS(COS4)
      IF(X(3) .LT. 0.0D0) GO TO 1000
C---    SAT IS IN NORTHERN HEMISPHERE:  X(3) >= 0.  
C---    TRUE ANOMALY IS ANGLE - ARGPER (SAT IS LEAVING THE NODE.)
      TRUEAN = ANG - ARGPER
      GO TO 9000
 1000 CONTINUE
C---    SAT IS IN SOUTHERN HEMISPHERE:  X(3) < 0.  
C---    TRUE ANOMALY IS -ANGLE - ARGPER (SAT IS APPROACHING THE NODE.)
      TRUEAN = -ANG - ARGPER
 9000 CONTINUE
      PI2 = PI*2.0D0
      IF (TRUEAN .LT. 0.0D0) TRUEAN = TRUEAN + PI2
      IF (TRUEAN .LT. 0.0D0) TRUEAN = TRUEAN + PI2
      IF (TRUEAN .GE. PI2)   TRUEAN = TRUEAN - PI2
      RETURN
      END
********************************************************************
*  Name:        ENTCIR
*  Module Type: SUBROUTINE      Language: FORTRAN
*  Purpose:     BETWEEN 2 POINTS, DETERMINE WHAT FRACTION OF THE DISTANCE
*               MUST BE TRAVELED TO ENTER THE CIRCLE.
*  Menu Options:
*  Functions called:
*  Input Parameters:
*  Name         Type    Definition
*  SAT          CH*2    satellite
*  ETIME        REAL*8  ephemeris time of one of the points.  for an error msg.
*  XLAT         REAL*8  Latitude of outside point
*  XLON         REAL*8  Longitude of outside point
*  YLAT         REAL*8  Latitude of inside point
*  YLON         REAL*8  Longitude of inside point
*  CLAT         REAL*8  Latitude of circle center
*  CLON         REAL*8  Longitude of circle center
*  CRAD         REAL*8  Radius (km) of the circle.  
*  Output Parameters:
*  FRAC         REAL*8
*  Variables:
*  Locals :
*  Externals :
*  Modification History:                                            
*  Date                 Revision        Author
*  $Date$ $Revision$ $Author$
*********************************************************************/
      SUBROUTINE ENTCIR(SAT,ETIME,
     ?          XLAT,XLON,YLAT,YLON,CLAT,CLON,CRAD,FRAC)
      IMPLICIT NONE
      CHARACTER*(*) SAT
      REAL*8 ETIME
      REAL*8 XLAT, XLON, YLAT, YLON, CLAT, CLON, CRAD,FRAC
      REAL*8 XLL(2), YLL(2), CLL(2)
      REAL*8 X(3),   Y(3),   C(3), XIN(3), D, A
      CHARACTER*21 ASFTIME
      INTEGER IFLAG, IER
c--port--      INCLUDE 'MPS_LIB:MPS_CONST_EARTH.FOR/LIST'
      INCLUDE 'APS_HOME:include/local/mps_const_earth.inc'
      INCLUDE 'APS_HOME:include/local/timeconv.inc'
CCC      WRITE(*,*)' ENTCIR start:  XLAT,XLON=',XLAT,XLON
CCC      WRITE(*,*)' ENTCIR: YLAT,YLON=',YLAT,YLON
CCC      WRITE(*,*)' ENTCIR: CLAT,CLON,CRAD=',CLAT,CLON,CRAD
      XLL(1) = XLAT
      XLL(2) = XLON
      YLL(1) = YLAT
      YLL(2) = YLON
      CLL(1) = CLAT
      CLL(2) = CLON
      CALL LL2XYZ(XLL,X)
      CALL LL2XYZ(YLL,Y)
      CALL LL2XYZ(CLL,C)
C---    KM TO RADIANS.  USE EARTH RADIUS IN KM.  - - - - - - 
      D = CRAD / ERAD
      CALL GCENTC(IFLAG,XIN, X,Y,C,D)
      IF(IFLAG .EQ.2) GO TO 9000
C---    
      WRITE(*,*) ' ENTCIR:  WARNING:   IFLAG .NE. 2'
      WRITE(*,*) ' ENTCIR:  ERROR IN CODE IF THIS HAPPENS OFTEN.'
      WRITE(*,*) ' ENTCIR:  IFLAG =', IFLAG
      WRITE(*,*) ' SAT = ', SAT
C---      CALL ET2ASF(ETIME,ASFTIME)
      IER = tc_et2asf(%VAL(ETIME), ASFTIME)
      WRITE(*,*) ' ASFTIME = ',ASFTIME
      WRITE(*,*) 
     ?' REVIEW THE COVERAGE FILE AT THE ABOVE TIME.  THE POINT SHOULD'
      WRITE(*,*) 
     ?' BE INSIDE THE ASF MASK; THE POINT AFTER OR BEFORE IT SHOULD '
      WRITE(*,*) ' BE OUTSIDE THE ASF MASK.  '
      WRITE(*,*) 
     ?' IFLAG = 1 MEANS THAT BOTH POINTS ARE INSIDE THE CIRCLE.'
      WRITE(*,*) 
     ?' IFLAG = 2 MEANS THAT NEITHER POINTS ARE INSIDE THE CIRCLE.'
      WRITE(*,*) 
     ?' (WE ARE TALKING ABOUT THE NON-INTERPOLATED COVERAGE RECORDS.)'
      WRITE(*,*) 
     ?' MAKE SURE THAT THE DISCREPANCY IS SMALL.  '
      IF(IFLAG .EQ. 1) FRAC = 0.01D0
      IF(IFLAG .EQ. 0) FRAC = 0.99D0
      GO TO 9999
 9000 CONTINUE
      CALL GCDIST(A,X,XIN)
      CALL GCDIST(D,X,Y)
      FRAC = A / D
 9999 CONTINUE
CCC      WRITE(*,*)' ENTCIR:  IFLAG = ', iflag
CCC      WRITE(*,*)' ENTCIR:  FRAC = ', FRAC
      RETURN 
      END

c **********************************************************************
c  Name: progress
c  Module Type: Program         Language: FORTRAN
c  Purpose:  given current time and time bracket for the run, prints 
c            the percent of the run completed.  
c  Input Parameters:
c  Name         Type            Description
c  t            real*8          current time
c  t1           real*8          t1 and t2 commpose the time bracket of
c  t2           real*8          the coverage run.  
c **********************************************************************
      subroutine progress(t, t1, t2)
      implicit  none
      real*8 t, t1, t2
      integer ipct, pct(27), j
      integer*4 stime, tarray(9), time
      data pct /  1,  2,  3,  4,  5,  6,  7,  8, 9,
     ?           10, 20, 30, 40, 50, 60, 70, 80, 90, 
     ?           91, 92, 93, 94, 95, 96, 97, 98, 99/ 
      save pct
      ipct = 100 * (t - t1) / (t2 - t1)
      do 1000 j = 1, 27
      if(pct(j) .eq. 0) go to 1000
      if(ipct .lt. pct(j)) go to 1000
c---    since the last update, progress has passed pct(j).  print this.
      stime = time()
      call ltime(stime, tarray)
      print 1001,pct(j), tarray(3), tarray(2), tarray(1)
 1001 format('           progress:',I4,' percent done; time = ',
     ?                                  I2.2,':',I2.2,':',I2.2)
c---    zero out pct(j) to prevent printing it later.  
      pct(j) = 0
 1000 continue
      return
      end
