C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:   ephmsvc1.for
C--
C--  Description:	Creates trial nominal state vectors and orbit files 
C--                 until acceptable values are found.  Then creates the 
C--                 approved nominal state vector and nominal orbit file.  
C--	
C--  Notes:  This routine creates batch jobs, submits them, and reads 
C--          the output file.  The batch job runs the ASAP orbit propagator 
C--          which cannot be linked to this executable because some of its 
C--          routines and common blocks match those from the Vector Library.
C--
C-- ==========================================================================

****************************************************************************
*  NAME: EPHMSVC1
*  MODULE TYPE: SUBROUTINE 
*
*  PURPOSE:  creates a nominal state vector and 
*  ephemeris file.  It calls the asap program in an iterative loop to 
*  arrive at an ephemeris file that complies with the specifications 
*  of the nominal orbit.  
*  LANGUAGE: F77
*  $LOGFILE$
*  INPUT PARAMETERS:
*  Name             Type    Definition
*  dbproc           INT 4   pointer passed to Sybase routines.  
*  dbname           char    name of database, passed to open the database.
*  sybase_userid    char    name of userid for sybase, passed to open the 
*                           database.
*  password         char    password for sybase, passed to open the database.
*  SAT              char    satellite id.  
*  PHASE_NAME       char    phase id for nominal state vector.   
*
*  return value 
*   0 =     no errors.  
*   other   errors occured.  
*
*  Modification History:                                            
*  $DATE$      $REVISION$      $AUTHOR$
*********************************************************************/
      INTEGER FUNCTION EPHMSVC1(PROGNAME, dbproc, dbname, sybase_userid,
     ?  password, SAT,PHASE_NAME)

      character*100 SccsFileID
     -/'@(#)ephmsvc1.for	5.1 98/01/08 APS/ASF\0'/

      IMPLICIT NONE
      INCLUDE 'APS_HOME:include/local/timeconv.inc'
      integer*4 dbproc
      CHARACTER*(*) PROGNAME
      CHARACTER*(*) dbname, sybase_userid, password, SAT, PHASE_NAME
      integer rcode, system, errorcount
c-port--    declare this C routine for calling.  
      external f77_aps_fullpath !$pragma c(f77_aps_fullpath)
C---      external gettime          !$pragma c(gettime)
      external aps_chmod        !$pragma c(aps_chmod)
      external aps_access       !$pragma c(aps_access)
      external get_phase        !$pragma c(get_phase)
      integer unlink
      character *2 chnum(20)
      data chnum /'1 ', '2 ', '3 ', '4 ', '5 ', 
     ?            '6 ', '7 ', '8 ', '9 ', '10',
     ?            '11', '12', '13', '14', '15', 
     ?            '16', '17', '18', '19', '20'/
      CHARACTER*22 PHASE_START, T2ASF
      REAL*8 PHASE_LON
      INTEGER PHASE_DAYS, CYCLE_DAYS, CYCLE_REVS, EPHMSVC1
      INTEGER PHASE_ORBITS, LAST_REV
      REAL*8 ORB_A, ORB_E, ORB_I, ORB_ARG_PERI
      REAL*8 ORB(6), ORBSV(6), XDUM
      INTEGER IORB, IORBSV
c-port--      REAL*8 MTH$CVT_G_D
      CHARACTER nodeflag
      REAL*8 X(12), XLONG, T2ET
      REAL*8 EPOCH, F, ET(2), SSLON, ATNODE, XTNODE, DTNODE, PM
      INTEGER J,IER,ISV,LC,IREV,INODE,IC,LASTC,IA9,IEF
      INTEGER ICOUNT, ITRIES
c--port--      CHARACTER*40 SVFILE, EFILE, CFILE, A9FILE
      CHARACTER*100 SVFILE, EFILE,  CFILE,  LFILE, A9FILE 
      CHARACTER*100         EFILE2, CFILE2, LFILE2, A9FILE2
      CHARACTER*22 nowtime
c--port--      CHARACTER*73 IBUF
      CHARACTER*200 IBUF, SVBUF
      INTEGER IBUFSIZE
      CHARACTER*10 IBUF2
      CHARACTER*101 IBUF101

      INCLUDE 'APS_HOME:include/local/aps_log_msg.inc'

      call aps_access('stoicfile', 'r',ier)
      if(ier .ne. 0) then 
          print *,
     ?        'ephmsvc1:  ERROR:  starting Create Nominal SV & Orbit.'
          print *,'    There must be a STOICFILE for the start of'
          print *,'    this phase in the current directory, with '
          print *,'    read access:'
          rcode = system('pwd')
          rcode = system('ls -l stoicfile')
          print *,'           The file should be named "stoicfile"'
          print *,
     ?        'ephmsvc1:  ERROR:  no STOIC FILE in current directory'
          print *,'           please review the last few messages.' 
          CALL APS_LOG_MSG_FORTRAN( PROGNAME, APS_CRITICAL,
     ?        'ERROR:  stoicfile not accessed.  Check ' //
     ?        'create_nominal_orbit.for, which should check this.', 
     ?        DO_SYSLOG, DO_PRINT )
          go to 8888
      endif

      errorcount = 0
C---      call tfname('MPS_EPHMCMD', IBUF)
C---    check the needed directories; accumulate errorcount.  
      CALL ephmsvc1_checkdir('APS_TEMP', errorcount)
C----      call tfname('MPS_EPHMIN', IBUF)
      CALL ephmsvc1_checkdir('APS_EPHMIN', errorcount)
C----      call tfname('MPS_EPHMOUT', IBUF)
      CALL ephmsvc1_checkdir('APS_EPHMOUT', errorcount)

C---    finished checking directories; now check access to files.  

C---    command file for ASAP tries during iteration loop.  
C----      IBUF = 'MPS_EPHMCMD/' // SAT // '_' // PHASE_NAME(1:1) 
C----     ?                              // '_CREATE_SV.COM'
      IBUFSIZE = 200
      call f77_aps_fullpath('APS_TEMP', 
     ?  SAT // '_' // PHASE_NAME(1:1) // '_CREATE_SV.COM' // CHAR(0),
     ?  IBUF, IBUFSIZE)
      CFILE = IBUF(1:LASTC(IBUF)) // CHAR(0)
      call writecheck(CFILE, ier)
      IF ( ier .NE. 0 )  CALL APS_LOG_MSG_FORTRAN( PROGNAME, 
     ?                        APS_CRITICAL, 'ERROR:  unable to write '
     ?                        // IBUF(1:LASTC(IBUF)), 
     ?                        DO_SYSLOG, DO_PRINT )
      errorcount = errorcount + ier

C---    LOG file for ASAP tries during iteration loop.  
C----      IBUF = 'MPS_EPHMCMD/' // SAT // '_' // PHASE_NAME(1:1) 
C----     ?                              // '_CREATE_SV.LOG'
      IBUFSIZE = 200
      call f77_aps_fullpath('APS_TEMP',
     ?  SAT // '_' // PHASE_NAME(1:1) // '_CREATE_SV.LOG' // CHAR(0),
     ?  IBUF, IBUFSIZE)
      LFILE = IBUF(1:LASTC(IBUF)) // CHAR(0)
      call writecheck(LFILE, ier)
      IF ( ier .NE. 0 )  CALL APS_LOG_MSG_FORTRAN( PROGNAME, 
     ?                        APS_CRITICAL, 'ERROR:  unable to write '
     ?                        // IBUF(1:LASTC(IBUF)), 
     ?                        DO_SYSLOG, DO_PRINT )
      errorcount = errorcount + ier

C---    input data file for ASAP tries during iteration loop.  
C---      IBUF = 'MPS_EPHMCMD/' // SAT // '_' // PHASE_NAME(1:1) 
C---     ?                              // '_CREATE_SV.DAT'
      IBUFSIZE = 200
      call f77_aps_fullpath('APS_TEMP',
     ?  SAT // '_' // PHASE_NAME(1:1) // '_CREATE_SV.DAT' // CHAR(0),
     ?  IBUF, IBUFSIZE)
      A9FILE = IBUF(1:LASTC(IBUF)) // CHAR(0)
      call writecheck(A9FILE, ier)
      IF ( ier .NE. 0 )  CALL APS_LOG_MSG_FORTRAN( PROGNAME, 
     ?                        APS_CRITICAL, 'ERROR:  unable to write '
     ?                        // IBUF(1:LASTC(IBUF)), 
     ?                        DO_SYSLOG, DO_PRINT )
      errorcount = errorcount + ier
 
C---    output ephemeris file for ASAP tries during iteration loop.  
C----      IBUF = 'MPS_EPHMCMD/' // SAT // '_' // PHASE_NAME(1:1) 
C----     ?                              // '_CREATE_SV.EPHM '
      IBUFSIZE = 200
      call f77_aps_fullpath('APS_TEMP',
     ?  SAT // '_' // PHASE_NAME(1:1) // '_CREATE_SV.EPHM' // CHAR(0),        
     ?  IBUF, IBUFSIZE)
      EFILE = IBUF(1:LASTC(IBUF)) // CHAR(0)
      call writecheck(EFILE, ier)
      IF ( ier .NE. 0 )  CALL APS_LOG_MSG_FORTRAN( PROGNAME, 
     ?                        APS_CRITICAL, 'ERROR:  unable to write '
     ?                        // IBUF(1:LASTC(IBUF)), 
     ?                        DO_SYSLOG, DO_PRINT )
      errorcount = errorcount + ier

c---    command file for final run.
C----      IBUF = 'MPS_EPHMCMD/' // SAT // '_' // PHASE_NAME(1:1) 
C----     ?                                    // '_ASAP.COM'
      IBUFSIZE = 200
      call f77_aps_fullpath('APS_TEMP',
     ?  SAT // '_' // PHASE_NAME(1:1) // '_ASAP.COM' // CHAR(0),        
     ?  IBUF, IBUFSIZE)
      CFILE2 = IBUF(1:LASTC(IBUF)) // CHAR(0)
      call writecheck(CFILE2, ier)
      IF ( ier .NE. 0 )  CALL APS_LOG_MSG_FORTRAN( PROGNAME, 
     ?                        APS_CRITICAL, 'ERROR:  unable to write '
     ?                        // IBUF(1:LASTC(IBUF)), 
     ?                        DO_SYSLOG, DO_PRINT )
      errorcount = errorcount + ier

c---    LOG file for final run.
C----      IBUF = 'MPS_EPHMCMD/' // SAT // '_' // PHASE_NAME(1:1) 
C----     ?                                    // '_ASAP.LOG'
      IBUFSIZE = 200
      call f77_aps_fullpath('APS_TEMP',
     ?  SAT // '_' // PHASE_NAME(1:1) // '_ASAP.LOG' // CHAR(0),        
     ?  IBUF, IBUFSIZE)
      LFILE2 = IBUF(1:LASTC(IBUF)) // CHAR(0)
      call writecheck(LFILE2, ier)
      IF ( ier .NE. 0 )  CALL APS_LOG_MSG_FORTRAN( PROGNAME, 
     ?                        APS_CRITICAL, 'ERROR:  unable to write '
     ?                        // IBUF(1:LASTC(IBUF)), 
     ?                        DO_SYSLOG, DO_PRINT )
      errorcount = errorcount + ier

c---    input data file for final run.
C---      IBUF = 'MPS_EPHMCMD/' // SAT // '_' // PHASE_NAME(1:1)
C---     ?                                    // '_ASAP.DAT'
      IBUFSIZE = 200
      call f77_aps_fullpath('APS_TEMP',
     ?  SAT // '_' // PHASE_NAME(1:1) // '_ASAP.DAT' // CHAR(0),        
     ?  IBUF, IBUFSIZE)
      A9FILE2 = IBUF(1:LASTC(IBUF)) // CHAR(0)
      call writecheck(A9FILE2, ier)
      IF ( ier .NE. 0 )  CALL APS_LOG_MSG_FORTRAN( PROGNAME, 
     ?                        APS_CRITICAL, 'ERROR:  unable to write '
     ?                        // IBUF(1:LASTC(IBUF)), 
     ?                        DO_SYSLOG, DO_PRINT )
      errorcount = errorcount + ier

c---    final output nominal ephemeris file 
C---      EFILE2 = 'MPS_EPHMOUT/' // SAT // '_' // PHASE_NAME(1:1)// 
C---     ?                                                    '.EPHM'
      IBUFSIZE = 200
      call f77_aps_fullpath('APS_EPHMOUT',
     ?  SAT // '_' // PHASE_NAME(1:1) // '.EPHM' // CHAR(0),
     ?  IBUF, IBUFSIZE)
      EFILE2 = IBUF(1:LASTC(IBUF)) // CHAR(0)
C---      call tfname(EFILE2_SHORT,EFILE2)
C---    removing this check.  ASAP will remove the file.  
C---      call writecheck(EFILE2, ier)
C---      errorcount = errorcount + ier

C---    state vector file containing the final state vector. 
C---      IBUF = 'MPS_EPHMIN/' // SAT // '_' // PHASE_NAME(1:1) 
C---     ?  // '.' // SAT // 'VN'
      IBUFSIZE = 200
      call f77_aps_fullpath('APS_EPHMIN',
     ?  SAT // '_' // PHASE_NAME(1:1) // '.' // SAT // 'VN' // CHAR(0),
     ?  IBUF, IBUFSIZE)
      SVFILE = IBUF(1:LASTC(IBUF)) // CHAR(0)
      call writecheck(SVFILE, ier)
      IF ( ier .NE. 0 )  CALL APS_LOG_MSG_FORTRAN( PROGNAME, 
     ?                        APS_CRITICAL, 'ERROR:  unable to write '
     ?                        // IBUF(1:LASTC(IBUF)), 
     ?                        DO_SYSLOG, DO_PRINT )
      errorcount = errorcount + ier

      if(errorcount .ne. 0) then
          print *,'ephmsvc1:  terminating this run due lack of ', 
     ?      'permission on directories and/or files.'
          rcode = system('banner ERROR')
          CALL APS_LOG_MSG_FORTRAN( PROGNAME, APS_CRITICAL,
     ?'ERROR:  insufficient permission on directories and/or files.',
     ?                              DO_SYSLOG, DO_PRINT )
          GO TO 8888
      endif

C-port      NRECS = 0
C-port##  retrieve (
C-port##    PHASE_START     = phase.#phase_start,
C-port##    PHASE_LON   = phase.#phase_lon,
C-port ##   PHASE_DAYS  = phase.#phase_days,
C-port ##   PHASE_ORBITS    = phase.#phase_orbits,
C-port ##   LAST_REV    = phase.#last_rev,
C-port ##   CYCLE_DAYS  = phase.#cycle_days,
C-port ##   CYCLE_REVS  = phase.#cycle_revs,
C-port ##   ORB_A       = phase.#orb_a,
C-port ##   ORB_E       = phase.#orb_e,
C-port ##   ORB_I       = phase.#orb_i,
C-port ##   ORB_ARG_PERI    = phase.#orb_arg_peri,
C-port ##   EW_TOL_KM   = phase.#ew_tol_km,
C-port ##   NS_TOL_MIN  = phase.#ns_tol_min  )
C-port ##  WHERE phase.#sat = SAT
C-port ##  AND   phase.#phase_name = PHASE_NAME
C-port ##  inquire_equel ( NRECS = rowcount )
C-port ##   putform(v_sat = SAT )
C-port ##   putform(v_phase_name = PHASE_NAME C-port##  retrieve (

c-port      add terminating nulls to strings for delivering 
c----       arguments to get_phase.  
      ibuf = SAT(1:2) // char(0)
      ibuf2 = PHASE_NAME(1:1) // char(0)
      call get_phase(%VAL(dbproc), 
     ?               ibuf, 
     ?               ibuf2,
     ?               PHASE_START,
     ?               PHASE_LON  ,
     ?               PHASE_DAYS ,
     ?               PHASE_ORBITS,
     ?               LAST_REV   ,
     ?               CYCLE_DAYS ,
     ?               CYCLE_REVS ,
     ?               ORB_A  ,
     ?               ORB_E  ,
     ?               ORB_I  ,
     ?               ORB_ARG_PERI  )
    
      call chkphase(SAT, PHASE_NAME,
     ?               PHASE_START,
     ?               PHASE_LON  ,
     ?               PHASE_DAYS ,
     ?               PHASE_ORBITS,
     ?               LAST_REV   ,
     ?               CYCLE_DAYS ,
     ?               CYCLE_REVS ,
     ?               ORB_A  ,
     ?               ORB_E  ,
     ?               ORB_I  ,
     ?               ORB_ARG_PERI, IER  )

      IF(IER .GT. 0) THEN
          print *,'ephmsvc1: A nominal state vector/orbit cannot ',
     ?               'be created.  '

          CALL APS_LOG_MSG_FORTRAN( PROGNAME, APS_CRITICAL,
     ?        'ERROR:  Problem with phase relation record.  A ' //
     ?        'nominal state vector/orbit cannot be created.  ',
     ?        DO_SYSLOG, DO_PRINT )

          print *,' Terminating this function due to errors.'
          go to 8888 
      ENDIF

      ITRIES = 0
C---    CREATE FIRST TRY NOMINAL STATE VECTOR FROM THE SELECTED PHASE RECORD.
C---    USE THE VECTOR LIBRARY.  
C---    CONVERT THE PHASE START TIME TO EPHEMERIS JULIAN DAY (REAL).  
C---    THIS WILL BE THE EPOCH.  
C---      CALL ASF2ET(PHASE_START,EPOCH,IER)
      IER = tc_asf2et(PHASE_START,EPOCH)
c---      print   *, ' EPHMSVC1:  PHASE_START = ', PHASE_START
c---      write(*,9222) ' EPHMSVC1:  EPOCH = ', EPOCH
 9222 format(1x, A, F17.9)
      IF(IER .EQ. 1) GO TO 2000
C---    BAD PHASE START TIME SYNTAX.  
      print *,'ERROR IN PHASE START TIME SYNTAX. ' 
      print *,'A state vector cannot be created.  '

      CALL APS_LOG_MSG_FORTRAN( PROGNAME, APS_CRITICAL,
     ?    'ERROR:  PHASE RELATION START TIME SYNTAX',
     ?    DO_SYSLOG, DO_PRINT )
      GO TO 8888
 2000 CONTINUE
C---    Now initialize computational environment for VECTOR lib.
C---    Start time = EPOCH, center of coordinate system is planet earth.
      CALL CBODYN(EPOCH,0,3)
C---    NOW SET UP THE IORB FLAG DESCRIBING THE ELEMENTS TO GO INTO THE 
C---    ROUTINE ORBIN:  
C---    SECTION 5.3, PAGE 5-7 ORBIT FLAGS.
C---    DIGIT 1 = 1 --> ORBITAL ELEMENTS a,e,i,o,u,v TO GO IN ORB ARRAY
C---    DIGIT 2 = 3 --> Coordinate system of ORB array inputs is:  
C---                Earth Equator PRIME MERIDIAN of Epoch.  The epoch 
C---                is EPOCH as assigned above.  
C---    DIGIT 3 = 0 --> 
C---    DIGIT 4 = 0 --> 
C---    DIGIT 5 = 0 --> u = ARGUMENT of periapsis
C---    DIGIT 6 = 2 --> v = TRUE anomaly.  
C---                    (ORB(6) = - u at the ascending node.)
C---    DIGIT 7 = 0 --> 
      IORB = 0200031
      ORB(1) =   ORB_A
      ORB(2) =   ORB_E
      ORB(3) =   ORB_I
      ORB(4) =   PHASE_LON
      ORB(5) =   ORB_ARG_PERI
      ORB(6) = - ORB(5)
C---    NOW SET UP THE IORBSV FLAG DESCRIBING THE DESIRED ELEMENTS TO BE 
C---    OBTAINED FROM THE ROUTINE ORBOUT:  
C---    SECTION 5.3, PAGE 5-7 ORBIT FLAGS.
C---    DIGIT 1 = 2 --> ORBITAL ELEMENTS are position and velocity (R&V)
C---    DIGIT 2 = 2 --> Coordinate system of ORB array inputs is:
C---                Earth Equator EQUINOX of Epoch.  The epoch 
C---                is EPOCH as assigned above.  
C---    DIGIT 3 = 1 --> Position and velocity are in x,y,z representation.
C---    DIGIT 4 = 0 --> 
C---    DIGIT 5 = 0 --> 
C---    DIGIT 6 = 0 --> 
C---    DIGIT 7 = 0 --> 
      IORBSV = 0000122
 4000 CONTINUE
C---    START LOOP.  NOW GO WITH THE ORBG DATA AND GET THE ORBSV DATA TO TRY.
      CALL ORBIN(ORB,IORB)
      CALL ORBOUT(ORBSV,IORBSV)
      if(ITRIES .eq. 0) then
c---         print *, 'EPHMSVC1:  START LOOP:  ------------------'
         print *, ' '
         print *, ' '
         print *, 'TRY no. 1'
         print *, '---------'
         print *, ' '
         print *, ' '
      endif
c---      print *,'EPHMSVC1:  orbital elements ORB =', ORB
c---      print *,'  SV:  ORBSV = ',ORBSV
C---    NOW PLACE IN THE SIS STATE VECTOR FORMAT, THEN READ BACK IN TO ORBSV.
C---    CHANGE VELOCITY TO METERS PER SECOND AS PER THE ASF SIS.
      ORBSV(4) = ORBSV(4) * 1000.0D0
      ORBSV(5) = ORBSV(5) * 1000.0D0
      ORBSV(6) = ORBSV(6) * 1000.0D0
c--port--      WRITE(IBUF,6991)ORBSV
      encode(73,6991,SVBUF)ORBSV
 6991 FORMAT (1X,6(F11.5,' '))
C---    THE STATE VECTOR FORMAT HAS PRECISION LIMITATIONS.  WHEN WE WRITE THE 
C---    NOMINAL STATE VECTOR, WE WILL USE WHAT IS NOW IN SVBUF.  
      decode(73,6991,SVBUF)ORBSV
C-----      print   *, ' EPHMSVC1 READ BACK:  ORBSV = ', ORBSV
C---    CHANGE VELOCITY TO KM PER SECOND AS ASAP NEEDS.  
      ORBSV(4) = ORBSV(4) / 1000.0D0
      ORBSV(5) = ORBSV(5) / 1000.0D0
      ORBSV(6) = ORBSV(6) / 1000.0D0
c---      print   *, ' EPHMSVC1 (for asap):  ORBSV = ', ORBSV
C---    GENERATE OUTPUT COMMAND FILE NAME ASAP_sat.COM.  INCLUDE SATELLITE NAME 
C---    FROM arguments.  It will run the command file for this state 
c---    vector try.  
c---      if(ITRIES .eq. 0) then
c---      print *,'ephmsvc1:  create files CFILE, A9FILE, EFILE; remove ', 
c---     ?                                  'them if they already exist:'
c---      endif
c---      if(ITRIES .eq. 0) then
c---      print *,'ephmsvc1:  CFILE=',CFILE(1:LASTC(CFILE))
c---      endif
c--- port  now remove the file if it already exists:  
      call aps_access(CFILE, 'f', ier)
      if(ier .eq. 0) then
          rcode = unlink(CFILE)
      endif
      IC = 70
C---        IC refers to the COMMAND FILE or script:
      OPEN (UNIT=IC,FILE=CFILE,FORM='FORMATTED',
     ?     ERR=8001,STATUS='NEW')
C---    GENERATE .DAT FILE FOR INPUT INTO THE ASAP RUN.  
c---      if(ITRIES .eq. 0) then
c---      print *,'ephmsvc1:  A9FILE=',A9FILE(1:LASTC(A9FILE))
c---      endif
c--- port  now remove the file if it already exists:  
      call aps_access(A9FILE, 'f', ier)
      if(ier .eq. 0) then
c---      the file exists; 
          rcode = unlink(A9FILE)
      endif
      IA9 = 71
      OPEN (UNIT=IA9,FILE=A9FILE,FORM='FORMATTED',
     ?     ERR=8002,STATUS='NEW')
C---    FILES ARE OPEN.  
C---    NOW WRITE THESE TIMES TO THE .DAT FILE.
C---    NOTE THAT EPOCH WILL BE THE EPOCH FOR THE ASAP COORDINATE SYSTEM.
      WRITE (IA9,9111) EPOCH, ' START JD'
 9111 format(1x, F17.9, A)
C---    COMPUTE THE DESIRED NODAL PERIOD:  
      DTNODE = CYCLE_DAYS
      DTNODE = DTNODE / CYCLE_REVS
C---    PROPAGATE FOR ONE NODAL PERIOD + 3 MINUTES.  
      T2ET = EPOCH + DTNODE + 3.0D0 / 60.0D0 / 24.0D0
      WRITE (IA9,9111) T2ET, ' FINAL JD'
      WRITE (IA9,9111) EPOCH, ' EPOCH JD'
C---    NOW COMPUTE THE PRIME MERIDIAN ANGLE AT EPOCH.  USE VECTOR LIB.
      CALL EPHMPM (EPOCH,PM,0)
      WRITE (IA9,*) PM,'  PM (DEG)'
      WRITE (IA9,*) ORBSV
      CLOSE(IA9)
      call aps_chmod(A9FILE,'RW\0', ier)
C---    NOW SUBMIT RUN:  ASAP_sat.COM
c---    now write the shell command file to run the program.  
      LC = LASTC(A9FILE)
      WRITE(IC,1001) '#!/bin/csh -f  '
 1001 FORMAT(A)
c--port--   put in the time this file is created.  
C---      call gettime(nowtime)
      IER = tc_systime2asf(nowtime)
      write(IC,*) 'echo this file was generated at:  ', nowtime
      write(IC,*) 'echo this file is being run at this time:'
      write(IC,*) 'date'
      write(IC,*) 'set echo'
      write(IC,*) 'pwd'
      write(IC,1001) 'setenv MPS_ASAP9 ' // A9FILE(1:LC)  
      call tfname('APS_DATA/asap/asap5.dat ', IBUF)
      LC = LASTC(IBUF)
      write(IC,1001) 'setenv MPS_ASAP5 ' // IBUF(1:LC)
c---      if(ITRIES .eq. 0) then
c---         LC = LASTC(EFILE)
c---         print *,'ephmsvc1:  EFILE=',EFILE(1:LC)
c---      endif
c---    now remove the file if it already exists.  
      call aps_access(EFILE, 'f', ier)
      if(ier .eq. 0) then
          rcode = unlink(EFILE)
      endif
c-port--      WRITE(IC,*)'$ ASSIGN ',EFILE(1:LC), ' FOR080'
      write(IC,1001)'setenv FOR080 ' // EFILE(1:lastc(EFILE)) 
c-port--      WRITE(IC,*) '$ ASSIGN NL:    FOR088'
      write(IC,*)'setenv FOR088 NULL'
      LC = LASTC(CFILE)
c-port--      WRITE(IC,*) '$ ASSIGN ', CFILE(1:LC), ' SYS$INPUT'
c-port--      WRITE(IC,*) '$ ASAP = "$MPS_BLDEXE:ASAPEPHM"'
C---      CALL ET2ASF(T2ET,T2ASF)
      IER = tc_et2asf(%VAL(T2ET), T2ASF)
C---    NOW COMPUTE THE FIRST REV IN THE PHASE:
      IREV = LAST_REV - PHASE_ORBITS + 1
c-port--      WRITE(IC,*) '$ ASAP ',PHASE_START,' ',T2ASF,' ',IREV,' ',SAT,'-'
C-----      write(IC,*) '$MPS_EXE/asapephm ', ' \\'
      write(IC,*) 'aps_asapephm ', PHASE_START, ' \'
      write(IC,*) '         ', T2ASF, ' ', IREV, '\'
      write(IC,9911) '      ', SAT, ' ', EPOCH, ' 1'
 9911 format(1x, A, A, A, 1x, F17.9, A)
c-port--      WRITE(IC,*) EPOCH,'-'
C---    NOTE THE FLAG = 1; WILL NOT AFFECT ANY DB RELATIONS.  
c--port--   this means that the run will not put anything into the database.
c--port--   when a nominal run is made, a record is placed into the 
c--port--   ephemeris [metadata] relation.  
c-port--      WRITE(IC,*)    EFILE(1:LC),  ' 1'
c---    note that the ephemeris file name was removed from the command line;
c---    it was already provided via the invironment variable FOR080.  
c-port--      CLOSE(IC,DISPOSE='SUBMIT')
      CLOSE(IC)
      call aps_chmod(CFILE, 'RW\0', ier)
c---    now execute the command file CFILE in batch mode, output to 
c---    command file .LOG: 
      LC = LASTC(CFILE)
c---    obtain execute permission.  
      IBUF = 'chmod +x ' // CFILE(1:LC)
      rcode = system(IBUF)
c---    now we will force output and error messages into the LOG file,
c---      if(ITRIES .eq. 0) then
c---      print *,'ephmsvc1:  CFILE, A9FILE, EFILE created'
c---      endif
c---      print *,'ephmsvc1: submitting command to run trial ASAP: '
c---    now this command will force output and error messages into the LOG file,
c---    even if the file already exists.  This is the >&! sequence.  
      IBUF = CFILE(1:LASTC(CFILE)) // ' >&! ' // 
     ?                  LFILE(1:LASTC(LFILE)) // char(0)
c---      if(ITRIES .eq. 0) then
c---      print *,IBUF(1:LASTC(IBUF))
c---      endif
      rcode = system(IBUF)
c-port--##  SLEEP 20
      call sleep(01)
      ICOUNT = 0
C---    NORMAL END.  
C
 4100 CONTINUE
C---    NOW WAIT FOR THE JOB TO FINISH.  
c--port--      print *,'       Waiting for the ASAP run to finish...'
c--port--      call sleep (2)
C---    NOW TRY TO READ THE EPHEMERIS FILE AND OBSERVE THE NODAL PERIOD AND THE 
C---    FIRST SUBSAT LONGITUDE OF ASCENDING NODE.  
C---    OPEN THE EPHEMERIS FILE.  
c---      print *,'ephmsvc1:  ASAP done; opening ASAP ephemeris file: '
      call aps_access(EFILE, 'f', ier)

      if(ier .ne. 0) then
          print *,'ephmsvc1:  error:  ASAP run did not make ephemeris',
     ?          ' file.  Check the log file for this run.'
          print *,'      log file name:  ', LFILE(1:LASTC(LFILE))
          print *,'      last 15 lines of this file:'
          ibuf = 'tail -15 ' // LFILE
          rcode = system(ibuf)

          CALL APS_LOG_MSG_FORTRAN( PROGNAME, APS_CRITICAL,
     ?                  'ERROR:  ASAP run did not make ephemeris '  // 
     ?                  'file.  Check the LOG FILE for this run:  ' //
     ?                  LFILE(1:LASTC(LFILE) ),
     ?                  DO_SYSLOG, DO_PRINT )
          print *,'ephmsvc1:  now terminating due to error.'
          rcode = system('banner ERROR')
          GO TO 8888
      endif

      call aps_access(EFILE, 'rw', ier)
      if(ier .ne. 0) then
          print *,'ephmsvc1:  error:  cannot access the ephemeris',
     ?                  ' file.  rw access needed: ' 
          LC = LASTC(EFILE)
          ibuf = 'ls -l ' // EFILE(1:LC) // CHAR(0)
          rcode = system(ibuf)

          CALL APS_LOG_MSG_FORTRAN( PROGNAME, APS_CRITICAL,
     ?                  'ERROR:  cannot access the ephemeris file.  '//
     ?                  'rw access needed for ' // EFILE(1:LC),
     ?                  DO_SYSLOG, DO_PRINT )
          print*,'ephmsvc1:  now terminating due to error.'
          rcode = system('banner ERROR')
          GO TO 8888
      endif

c---      if(ITRIES .eq. 0) then
c---      print *,EFILE(1:LASTC(EFILE))
c---      endif
      IEF = 18
      OPEN (UNIT=IEF, FILE = EFILE, STATUS = 'old',ERR=4400,
     &         FORM = 'unformatted')
      GO TO 4450
 4400 CONTINUE
      print *,'ephmsvc1:  error opening the ephemeris file'
      print *,'           please check permissions and existence:'
      LC = LASTC(EFILE)
      ibuf = 'ls -l ' // EFILE(1:LC) // CHAR(0)
      rcode = system(ibuf)
      print*,'ephmsvc1:  now terminating due to error.'
      rcode = system('banner ERROR')

      CALL APS_LOG_MSG_FORTRAN( PROGNAME, APS_CRITICAL,
     ?                  'ERROR:  when opening the ephemeris file.  '//
     ?                  'rw access needed for ' // EFILE(1:LC),
     ?                  DO_SYSLOG, DO_PRINT )
      GO TO 8888
c-port--      IF(promptfield .EQ. 'Y' .OR. promptfield .EQ. 'y') GO TO 4100
c-port--##  MESSAGE ' State vector not created...'
c-port      print *,'ephmsvc1:  State vector not created...'
c-port      go to 9000
c-port--##  SLEEP 1 
c-port--      GO TO 9000
c-port--      go to 4100
 4450 CONTINUE
      print *,
     ?  'ephmsvc1:  comparing ephemeris file to nominal parameters:' 
      INODE = 0
      REWIND (IEF, IOSTAT = ier)
      READ(IEF)XDUM
 4500 CONTINUE
      READ (IEF,END=8003) NODEFLAG, (X(J), J=1, 12)
      IF(NODEFLAG .NE. 'N') GO TO 4500
C---    A NODE RECORD.  
      INODE = INODE + 1
c--port--      ET(INODE)    = MTH$CVT_G_D(X(7))
      ET(INODE)    = X(7)
c--port--      IF(INODE .LE. 1) SSLON = MTH$CVT_G_D(X(11))
      IF(INODE .LE. 1) SSLON = X(11)
      IF(INODE .LE. 1) GO TO 4500
C---    CLOSE THE EPHEMERIS FILE.
c--port--      CLOSE (IEF,STATUS='DELETE')
      CLOSE (IEF)
      call aps_chmod(EFILE, 'RW\0', ier)
c-port--##  SLEEP 1
C---    NOW COMPUTE THE EXCESS OBSERVED NODAL PERIOD.  
c--port--   this is the difference bewteen the ASAP nodal period and the
c--port--   desired or nominal nodal period computed from the ground 
c--port--   track repeat cycle information.  
      ATNODE = ET(2) - ET(1)
      XTNODE = ATNODE - DTNODE
      print   *, 'EPHMSVC1: excess ASAP nodal period: XTNODE = ',
     ?                                                   XTNODE
C---    NOW COMPUTE THE EXCESS OBSERVED SUBSAT LONGITUDE AT FIRST 
C---    ASCENDING NODE.
      XLONG = SSLON - PHASE_LON
      IF(XLONG .GT. 180.0D0) XLONG = XLONG - 360.0D0 
      IF(XLONG .LE. -180.0D0) XLONG = XLONG + 360.0D0 
      print   *, 'EPHMSVC1: excess longitude: XLONG = ',XLONG
C---    NOW COMPARE THE OBSERVED NODAL PERIOD EXCESS WITH THE DESIRED EXCESS.
C---    MUST HAVE THE OBSERVED NODAL PERIOD >= THE DESIRED, OR LONGER BY 
C---    LESS THAN 1 MILLISECOND.  
      IF(XTNODE .LT. 0.0D0 ) GO TO 5000
      IF(XTNODE .GE. 0.9D0/3600000.0D0/24.0D0) GO TO 5000
      print   *, 'EPHMSVC1:   PASSED XTNODE.  nodal period is OK.'
      print   *, ' #############################################'
C---    NOW COMPARE THE OBSERVED SUBSAT LONGITUDE AT ASCENDING NODE WITH THE
C---    DESIRED ASCENDING NODE.  
      IF(ABS(XLONG) .LE. 0.0000001) then
          print *, 'EPHMSVC1:   PASSED XLONG.  subsat longitude is O.K.' 
          print *, ' #############################################'
          go to 7000
      endif
      print   *, 'EPHMSVC1:  ### FAILED ### XLONG'
 5000 CONTINUE
      ITRIES = ITRIES + 1
      IF(ITRIES .LE. 6) GO TO 5010
c--port--## PROMPT('Need another iteration. Do you want to keep going? [Y/N] ',
c--port--##     promptfield)
c--port--      IF(promptfield .EQ. 'Y' .OR. promptfield .EQ. 'y') GO TO 5010
c--port--##  MESSAGE ' State vector not created...'
c--port--##  SLEEP 1 
      print *,'ephmsvc1.for:  6 tries not enough.  fix this routine.'

      CALL APS_LOG_MSG_FORTRAN( PROGNAME, APS_CRITICAL,
     ?    'ERROR:  6 iterations not enough to create ephemeris file '//
     ?    ' fix this routine ephmsvc1.for', 
     ?    DO_SYSLOG, DO_PRINT )

      GO TO 8888
 5010 CONTINUE
      print *, ' '
      print *, ' '
      print *, 'TRY no. ' // chnum(ITRIES+1) 
      print *, '---------'
      print *, ' '
      print *, ' '
C---    NEED ONE MORE RUN.  
C---    NOW COMPUTE THE FRACTIONAL EXCESS PERIOD.  
C---    XTNODE MAY BE REALLY SMALL AND NOT CAUSE US TO GET INTO POSITIVE 
C---    TERRITORY LIKE WE NEED.
      F = XTNODE / DTNODE
C---    NOW ADJUST THE SEMIMAJOR AXIS BY -2/3 OF THE OBSERVED FRACTIONAL EXCESS
C---    OBSERVED IN THE NODAL PERIOD.  
      ORB(1) = ORB(1) * ( 1.0D0 - .67 * F )
C---    NOW ADJUST THE SUBSAT LONGITUDE AT ASCENDING NODE.  
      ORB(4) = ORB(4) - XLONG
C---    NOW DO ANOTHER TRIAL RUN WITH ASAP.
      GO TO 4000
 7000 CONTINUE
C---    O.K. THE CURRENT STATE VECTOR IS IT.  
      print *,'ephmsvc1:  O.K. THE CURRENT STATE VECTOR IS ACCEPTABLE.'
      print *,'           ############################################'
      print *,'ephmsvc1:  Now submit the nominal orbit ephemeris run.' 
C---    NOW WRITE THE FILE.  
C---    USE THE PHASE NAME (WITHOUT BLANKS) IN THE FILE NAME.  
c--port--##  retrieve (PHASE_NAME = squeeze(PHASE_NAME))
c--port--      LC = LASTC(PHASE_NAME)
      LC = LASTC(PHASE_NAME)
c--port--      DO 7100 J = 1, LC
c--port--      IF(PHASE_NAME(J:J) .EQ. ' ') P_NAME(J:J) = '_'
c--port-- 7100 CONTINUE
c--port--      SVFILE = 'MPS_EPHMIN:' // SAT // '_' // P_NAME(1:LC) 
c-- port  now remove the file if it already exists:
c---      print *,'ephmsvc1:  removing old state vector file:'
c---      print *,'           ', SVFILE(1:LASTC(SVFILE))
      call aps_access(SVFILE, 'f', ier)
      if(ier .eq. 0) then
          rcode = unlink(SVFILE)
      endif
      ISV = 77
c--port--      OPEN (UNIT=ISV,FILE=SVFILE,FORM='FORMATTED',
c--port--     ?     CARRIAGECONTROL='LIST',ERR=8004,STATUS='NEW')
      OPEN (UNIT=ISV,FILE=SVFILE,FORM='FORMATTED',
     ?     ERR=8004,STATUS='NEW')
c--port--      CALL Gettime(%REF(nowtime))
C---      CALL gettime(nowtime)
      IER = tc_systime2asf(nowtime)
      nowtime(5:5) = ' '
c---    write the header for the sv file.  
      WRITE(ISV,6771)nowtime
 6771 FORMAT(A21,' SV APS APS                  ')
      WRITE(ISV,6551)SAT
 6551 FORMAT(A2,' NOMINAL    EQUINOX OF EPOCH    ')
      WRITE(ISV,6441)IREV,PHASE_START(1:4),PHASE_START(6:21),SVBUF(2:73)       
 6441 FORMAT(I5, 1X, A4, 1X, A16, 1X, A72)
      CLOSE (ISV)
      call aps_chmod(SVFILE, 'RW\0', ier)
c--port--      VMESSG = 'New SV file:  ' // SVFILE(1: LASTC(SVFILE) )
c--port--     ?   // '.  Press <RETURN>'
c--port--##  prompt (VMESSG, promptfield )
c--port--   C---    NOW SEE IF COVERAGE CAN RUN WITH THIS SATELLITE.  
c
c
c---    NOTE:  this checking is not ported.  The same checking is done 
c   anyway, when coverage is initialized.  
c
c
c--port--         CALL COVCHK(SAT,'*',PHASE_START,INOM,IER)
c--port--         IF(INOM .NE. 0) THEN
c--port--##       PROMPT (
c--port--## 'The data base will not support Coverage at this time.  <RETURN>',
c--port##    promptfield)
c--port##       PROMPT ('The nominal orbit was not created.  Press <RETURN>',
c--port##       promptfield)
c--port--            EPHMSVC1 = 0
c--port##       message ' Returning to the previous screen...'
c--port##       sleep 2
c--port##    enddisplay
c--port--            return
c--port--         ENDIF
C---    NOW SUBMIT NOMINAL RUN FOR ASAP.  
C---    GENERATE OUTPUT COMMAND FILE NAME ASAP_sat.COM.  INCLUDE SATELLITE NAME 
C---    FROM METADATA REC.
c--- port  now remove the file if it already exists:
c---      print *,'ephmsvc1:  (over)writing old command file:'
c---      print *,'           ', CFILE2(1:LASTC(CFILE2))
      call aps_access(CFILE2, 'f', ier)
      if(ier .eq. 0) then
          rcode = unlink(CFILE2)
      endif
      IC = 70
c--port--      OPEN (UNIT=IC,FILE=CFILE2,FORM='FORMATTED',
c--port--     ?     CARRIAGECONTROL='LIST',ERR=8002,STATUS='NEW')
      OPEN (UNIT=IC,FILE=CFILE2,FORM='FORMATTED',
     ?     ERR=8006,STATUS='NEW')
C---    GENERATE ASAP9_sat.DAT FILE FOR INPUT INTO THE ASAP RUN.  
c--port--      A9FILE2 = 'MPS_EPHMCMD:' // SAT // '_ASAP.DAT'
c--- port  now remove the file if it already exists:
c---      print *,'ephmsvc1:  (over)writing old DAT file:'
c---      print *,'           ', A9FILE2(1:LASTC(A9FILE2))
      call aps_access(A9FILE2, 'f', ier)
      if(ier .eq. 0) then
          rcode = unlink(A9FILE2)
      endif
      IA9 = 71
c--port--      OPEN (UNIT=IA9,FILE=A9FILE2,FORM='FORMATTED',
c--port--     ?     CARRIAGECONTROL='LIST',ERR=8003,STATUS='NEW')
      OPEN (UNIT=IA9,FILE=A9FILE2,FORM='FORMATTED',
     ?     ERR=8007,STATUS='NEW')
C---    FILES ARE OPEN.  
C---    NOTE THAT EPOCH IS THE EPOCH FOR THE ASAP COORDINATE SYSTEM.
      WRITE (IA9,9111) EPOCH, ' START JD'
      WRITE (IA9,9111) T2ET, ' FINAL JD'
      WRITE (IA9,9111) EPOCH, ' EPOCH JD'
      WRITE (IA9,*) PM,'  PM (DEG)'
      WRITE (IA9,*) ORBSV
      WRITE (IA9,*) '''NO REPLICATION'''
      WRITE(IA9,*) '''',SVFILE(1:LASTC(SVFILE)), '''', 
     ?                                          ' (SV FILE NAME) '
      WRITE(IA9,*) '''NOMINAL ''', ' (SV TYPE) '
      WRITE(IBUF101,6442)IREV,PHASE_START(1:4),PHASE_START(6:21),
     ?      SVBUF(2:73)
 6442 FORMAT(1X,I5, 1X, A4, 1X, A16, ' ',A72)
      WRITE(IA9,*) '''',IBUF101(2:53),'''', ' (STATE VECTOR) '
      WRITE(IA9,*) '''',IBUF101(54:101),'''', ' (STATE VECTOR) '
      WRITE(IA9,*) '''',PHASE_NAME(1:1),'''', ' PHASE NAME '
      CLOSE(IA9)
      call aps_chmod(A9FILE2, 'RW\0', ier)
C---    NOW SUBMIT RUN:  ASAP_sat.COM
      WRITE(IC,1001) '#!/bin/csh -f  '
c--port--   put in the time this file is created.  
C---      call gettime(nowtime)
      IER = tc_systime2asf(nowtime)
      write(IC,*) 'echo this file generated at:  ', nowtime
c---port--  now have the file generate the current time.
      write(IC,*) 'echo this file is being run at this time:'
      write(IC,*) 'date'
c--port--      WRITE(IC,*) '$ SET VERIFY  '
      write(IC,*) 'set echo'
c--port--      WRITE(IC,*) '$ SHOW DEFAULT'
      write(IC,*) 'pwd'
c--port--      WRITE(IC,*) '$ ASSIGN ', A9FILE2(1:LC), ' MPS_ASAP9'
      LC = LASTC(A9FILE2)
      write(IC,1001) 'setenv MPS_ASAP9 ' // A9FILE2(1:LC)
c--port--      WRITE(IC,*) '$ ASSIGN MPS_BLDEXE:ASAP5.DAT MPS_ASAP5'
c---    write out whole directory path for APS_DATA:  
      call tfname('APS_DATA/asap/asap5.dat', IBUF)
      write(IC,1001) 'setenv MPS_ASAP5  ' // IBUF(1:LASTC(IBUF))
C---    GET EPHEMERIS FILE NAME:  COVERAGE, NO REPLICATION, NOMINAL:
c--port--      CALL EPHMAFILE(SAT,'Y','N','Y',EFILE2,IER)
c--port--      IF(IER .NE. 0) THEN
c--port--         CALL GCBOMB
c--port--      ENDIF
c--port--      WRITE(IC,*) '$ ASSIGN ', EFILE2(1:LC), ' FOR080'
      write (IC,1001)'setenv FOR080 ' // EFILE2(1:LASTC(EFILE2))
c--port--      WRITE(IC,*) '$ ASSIGN NL:    FOR088'
      write (IC,*) 'setenv FOR088  NULL'
      LC = LASTC(CFILE2)
c--port--      WRITE(IC,*) '$ ASSIGN ', CFILE2(1:LC), ' SYS$INPUT'
c--port--      WRITE(IC,*) '$ ASAP = "$MPS_BLDEXE:ASAPEPHM"'
c--port--      WRITE(IC,*) '$ ASAP ',PHASE_START,' ',T2ASF,' ',IREV,' ',SAT,'-'
      write(IC,*) 'unset echo'

C---      write(IC,*) '$MPS_BLDEXE/asapephm ', PHASE_START, ' \\'
      write(IC,*) 'aps_asapephm ', PHASE_START, ' \'

      write (IC,*)'                   ',T2ASF,' ',IREV,' \'
c--port--      WRITE(IC,*) EPOCH,'-'
      WRITE(IC,9912) '                  ', SAT, ' ', EPOCH, ' 0', ' \'
 9912 format(1x, A, A, A, 1x, F17.9, A, A)
      write(IC,*) '     ', dbname(1:lastc(dbname)), ' \'
      write(IC,*) '     ', sybase_userid(1:lastc(sybase_userid)), ' \'
      write(IC,*) '     ', password(1:lastc(password))
C---    NOTE THE FLAG = 0; WILL NOW AFFECT THE EPHEMERIS RELATION.  
C---    since the ephemeris relation was effected, password was needed.
c--port--      WRITE(IC,*)    EFILE2(1:LC),  ' 0'
c-- note that the name of the ephemeris file is no longer placed in 
c---    the command line; it was already in the environment variable 
c---    FOR080.  
c--port--      CLOSE(IC,DISPOSE='SUBMIT')
      close(IC)
      call aps_chmod(CFILE2, 'RW\0', ier)
c---    now execute the command file CFILE2 in batch mode, output to
c---    command file .LOG:
c---    give execution permission:  
      IBUF = 'chmod +x ' // CFILE2
      rcode = system(IBUF)
c---    std output and error combined into output .LOG file.  note >&! sequence
c---    to overwrite an existing file.  
      IBUF = CFILE2(1:LASTC(CFILE2)) // ' >&! ' // LFILE2

      print *,'ephmsvc1:  nominal ASAP command file submitted:',
     ?                     IBUF(1:LASTC(IBUF))
      rcode = system(IBUF)
C---    SEE VMS USERS MANUAL 3.8.3 BATCH JOB OUTPUT.
C---    NORMAL END.  
c--port--      VMESSG = 'Nominal orbit ASAP run via: '//CFILE2(1:LC)//  
c--port--     ?        '.  Press <RETURN>'
c--port--## prompt (VMESSG, promptfield)
      LC = LASTC(EFILE2)
C---     we got into trouble with the output nominal ephemeris file 
C---     being RW.  When a <ctrl>-C was typed during a coverage run, 
C---     the file would get altered.  But not if the W permission was removed.
      call aps_chmod(EFILE2, 'R\0', ier)
c--port--      VMESSG = 'Nominal Orbit ephemeris file name = '//EFILE2(1:LC)//  
      print *,' '
      print *,' ephmsvc1:  Nominal Orbit ephemeris file:'

C----      IBUF = 'cd $MPS_EPHMOUT; pwd; ls -l ' //  EFILE2(LC-8:LC)
      IBUF = 'cd ' // EFILE2(1:LC-10) // 
     ?                  '; pwd; ls -l ' //  EFILE2(LC-8:LC) // char(0)

      rcode = system(IBUF)
      print *,'ephmsvc1:  Nominal state vector file:  '
      LC = LASTC(SVFILE)

C----      IBUF = 'cd $MPS_EPHMIN; pwd; ls -l ' //  SVFILE(LC-8:LC)
      IBUF = 'cd ' // SVFILE(1:LC-10) // 
     ?                  '; pwd; ls -l ' //  SVFILE(LC-8:LC) // char(0)

      rcode = system(IBUF)
      print *,' '
c---    now print the final 10 lines from the ASAP log file to confirm the
c---    completion of the run.  
      print *,'ephmsvc1:  confirming the completion of the ASAP run'
      print *,'            by printing the last 11 lines of the log',
     ?                   ' file:'
      print *,' log file name = ', LFILE2(1:LASTC(LFILE2))
      IBUF = 'tail -11 ' // LFILE2
      rcode = system(IBUF)
      EPHMSVC1 = APS_EXIT_OK
C----        This is the only APS_EXIT_OK in the routine.  
      go to 9999


 8001 CONTINUE
      print *,'EPHMSVC1:  ERROR ON OPENING OF .COM FILE. ' 
      print *,'CHECK FOR ROOM/PERMITS ON DISK IN DIRECTORY'
      print *,'file name = ', CFILE
      print *,'The state vector was not created. ' 

      CALL APS_LOG_MSG_FORTRAN( PROGNAME, APS_CRITICAL,
     ?    'ERROR:  ON OPENING OF .COM FILE '//CFILE(1:LASTC(CFILE)) //
     ?    '   CHECK FOR ROOM/PERMITS ON DISK IN DIRECTORY', 
     ?    DO_SYSLOG, DO_PRINT )

      GO TO 8888
 8002 CONTINUE
      print *,'EPHMSVC1:  ERROR ON OPENING OF FILE.  '
      print *,'file name = ', A9FILE
      print *, '         CHECK FOR ROOM/PERMITS ON DISK IN DIRECTORY.'
      print *,'The state vector was not created. ' 

      CALL APS_LOG_MSG_FORTRAN( PROGNAME, APS_CRITICAL,
     ?    'ERROR:  ON OPENING OF FILE '//A9FILE(1:LASTC(A9FILE)) //
     ?    '   CHECK FOR ROOM/PERMITS ON DISK IN DIRECTORY', 
     ?    DO_SYSLOG, DO_PRINT )

      GO TO 8888
 8003 CONTINUE
      print *,'EPHMSVC1:  EOF REACHED ON EPHEMERIS FILE. ' 
      print *,'MOST LIKELY, the ASAP run did not make the ephemeris',
     ?          ' file.  Check the log file for this run:'
      print *,'      log file name:  ', LFILE(1:LASTC(LFILE))
      print *,'      last 15 lines of this file:'
      ibuf = 'tail -15 ' // LFILE
      rcode = system(ibuf)
      print *,'IF THE LOG FILE SHOWED NO ERROR, ' 
      print *,'YOU SHOULD DECREASE THE SEMIMAJOR AXIS.  '
      print *,'IN THE PHASE RELATION RECORD FOR ', 
     ?                                   SAT, ', ', PHASE_NAME(1:1)
      print *,'THE REPEAT CYCLE DAYS & REVS YIELDED A NODAL PERIOD'
      print *,'WHICH DID NOT CORRESPOND TO THE SEMIMAJOR AXIS.'
      print *,'The state vector was not created.  '

      CALL APS_LOG_MSG_FORTRAN( PROGNAME, APS_CRITICAL,
     ?   'ERROR:  EOF REACHED ON EPHEMERIS FILE.  CHECK LOG FILE:  ' //
     ?   LFILE(1:LASTC(LFILE)) , 
     ?   DO_SYSLOG, DO_PRINT )

      GO TO 8888
 8004 CONTINUE
      print *,'EPHMSVC1:  ERROR ON OPENING OF STATE VECTOR FILE.'
      print *,'file name = ', SVFILE
      print *,'CHECK FOR ROOM/PERMITS ON DISK IN DIRECTORY.'
      print *,'The state vector was not created.  '

      CALL APS_LOG_MSG_FORTRAN( PROGNAME, APS_CRITICAL,
     ?    'ERROR:  ON OPENING OF STATE VECTOR FILE '  //
     ?    A9FILE(1:LASTC(A9FILE)) //
     ?    '   CHECK FOR ROOM/PERMITS ON DISK IN DIRECTORY', 
     ?    DO_SYSLOG, DO_PRINT )

      GO TO 8888
 8006 CONTINUE
      print *,'EPHMSVC1:  ERROR ON OPENING OF FILE.  '
      print *,'file name = ', CFILE2
      print *, '         CHECK FOR ROOM/PERMITS ON DISK IN DIRECTORY.'
      print *,'The state vector was not created. ' 

      CALL APS_LOG_MSG_FORTRAN( PROGNAME, APS_CRITICAL,
     ?    'ERROR:  ON OPENING OF FILE '  //
     ?    CFILE2(1:LASTC(CFILE2)) //
     ?    '   CHECK FOR ROOM/PERMITS ON DISK IN DIRECTORY', 
     ?    DO_SYSLOG, DO_PRINT )

      GO TO 8888
 8007 CONTINUE
      print *,'EPHMSVC1:  ERROR ON OPENING OF FILE.  '
      print *,'file name = ', A9FILE2
      print *, '         CHECK FOR ROOM/PERMITS ON DISK IN DIRECTORY.'

      CALL APS_LOG_MSG_FORTRAN( PROGNAME, APS_CRITICAL,
     ?    'ERROR:  ON OPENING OF FILE '  //
     ?    A9FILE2(1:LASTC(A9FILE2))      //
     ?    '   CHECK FOR ROOM/PERMITS ON DISK IN DIRECTORY', 
     ?    DO_SYSLOG, DO_PRINT )

 8888 CONTINUE
c-port-- ERROR occurred.  return value = 1
      EPHMSVC1 = APS_EXIT_ERROR

 9999 continue
c-port--    return with the CURRENT code in EPHMSVC1
      RETURN
      END


      SUBROUTINE ephmsvc1_checkdir(LOC, errorcount)
      CHARACTER *(*) LOC 
      CHARACTER *200 IBUF
      INTEGER IBUFSIZE
      INTEGER IER, errorcount
      external aps_access       !$pragma c(aps_access)
      external f77_aps_fullpath !$pragma c(f77_aps_fullpath)
C---    check the directory, first translating.  
      IBUFSIZE = 200
C---        IBUFSIZE is an input and output variable.  
      CALL f77_aps_fullpath(LOC, '', IBUF, IBUFSIZE) 
      IBUF(LASTC(IBUF)+1:) = CHAR(0)
      call aps_access(IBUF, 'rwx', ier)
      if(ier .ne. 0) then
          print *,'ephmsvc1.for:  this function needs rwx access to '
          print *,'               this directory:'
          print *,'               ', IBUF(1:LASTC(IBUF))
          IBUF = 'ls -d -l ' // IBUF
          rcode = system(IBUF(1:LASTC(IBUF)) // char(0))
          rcode = system('banner ERROR' // char(0))
          errorcount = errorcount + 1
      endif
      RETURN
      END
