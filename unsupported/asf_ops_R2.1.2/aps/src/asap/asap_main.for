C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	asap_main.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================
Cport      OPTIONS /G_FLOATING
      Program ASAP_MAIN

      character*100 SccsFileID
     -/'@(#)asap_main.for	5.1 98/01/08 APS/ASF\0'/

c ************************************************************************
c	compile in G_FLOATING.
c
c  ljs:  this is not needed; the sun unix representation provides the 
c        needed byes of storage for the exponent of double precision.
c
c	compile in G_FLOATING.
c	compile in G_FLOATING.
c ************************************************************************
      IMPLICIT NONE 
c-port      declare
c-port      CHARACTER*58 fname, SVFNAME, SVTYPE, REPFLAG
      CHARACTER*100 SVFNAME
      CHARACTER*58 SVTYPE, REPFLAG
      CHARACTER*100 IBUF100, file_name_9, file_name_80, file_name_88
      CHARACTER*20 short_file_name_80 
      CHARACTER*20 short_sv_file_name 
      CHARACTER*31 stime, etime, TFASF, TLASF
      CHARACTER*99 progname
      CHARACTER*12 sat
      CHARACTER*10 PHASE_NAME
      INTEGER srev, erev, ib_rev, ie_rev, IFLAG, I, ireplicate
      integer rcode, system, unlink
      integer lastc, iargc, ncount
      REAL*8 epoch
C---	CONVERT FROM G_FLOATING TO D_FLOATING REPRESENTATION.
c--- port      REAL*8 MTH$CVT_G_D
      REAL*8 TSTART, TFD, TFIRST, TLAST, PNODE, TRD, PM, X(6)
c--- port      Character*256 argline
c-- port      integer stat, Lib$Get_Foreign, irev, ISVFLAG 
      integer irev

      integer db_open
      external db_open !$pragma c(db_open)
	  external db_open_errs !$pragma c(db_open_errs)
	  external error_handler_exit !$pragma c(error_handler_exit)
      integer asap_insert_ephemeris
      external asap_insert_ephemeris !$pragma c(asap_insert_ephemeris)
      integer asap_delete_ephmeris
      external asap_delete_ephmeris !$pragma c(asap_delete_ephmeris)

c---	STATEMENTS TO SUPPORT SYBASE CALLS TO DBLIB
      integer SUCCEED /1/, FAIL /0/
      integer*4 dbproc
      integer return_code, dbproc
c---	STATEMENTS TO SUPPORT SYBASE CALLS TO DBLIB
      character*100 dbname, sybase_userid, password

      common /ascii_opts/I88
      integer I88
c---    I88 is a flag for the ASCII file. I88=0 means no ascii output.

      Common /A_Revs/ ib_rev, ie_rev, TSTART
c--- port      include '($stsdef)'
c
Cport--- get the command line arguments
cport stat = Lib$Get_Foreign ( %descr(argline),,,)
cport stat = Lib$Get_Foreign ( %descr(argline),,,)
cport call parsearg(%ref(argline), %ref(stime), %ref(etime), irev, 
cport1     %ref(sat), epoch, %ref(fname), IFLAG )

c---    get count of arguments, compare with 0
      ncount = iargc()
      if(ncount .le. 0) then
          call getarg(0, progname)
          print *,' '
          print *,'usage:  ', progname(1:lastc(progname)), ' arguments'
          print *,' '
          print *,'    Do not run this program from the command line.'
          print *,'    Run create_nominal_orbit instead.'
          print *,' '
          call exit(0)
      endif

c---    compare with 6
      if(ncount .lt. 6) then
          print *,' Error:  fewer than 6 arguments.  '
          call exit(0)
      endif

      print *,'asap_main.for:  starting the run:'
      rcode = system('date')
      print *,'asap_main.for:  arguments from command line:'
      call getarg(1, stime)
      print *,'  stime = ', stime
      call getarg(2, etime)
      print *,'  etime = ', etime
      call getargi(3, irev, return_code)
      if(return_code .ne. 0) then
          print *,' Error in command line.  no 3rd argument.'
          call exit(0)
      endif
      print *,'  irev = ', irev
      call getarg(4, sat)
      print *,'  sat = ', sat
      call getargxx(5, epoch)
      write(*,1111)'  epoch = ', epoch
 1111 format(1x, A, F17.9)
      call getargi(6, IFLAG, return_code)
      if(return_code .ne. 0) then
          print *,' Error in command line.  no 6th argument.'
          call exit(0)
      endif
      print *,'asap_main.for:  IFLAG = ', IFLAG
	  if(IFLAG .EQ. 0) then

C-      compare with 9 arguments 
        if(ncount .lt. 9) then
            print *,' Error:  fewer than 9 arguments with IFLAG = 0'
            call exit(0)
        endif

C-		database to be opened this run.  need password.  
		call getarg(7, dbname)
        print *,'asap_main.for:  dbname = ', dbname
		call getarg(8, sybase_userid)
        print *,'asap_main.for:  sybase_userid = ', sybase_userid
		call getarg(9, password)
	  endif

      print *,'end of arguments from command line'
c---port now set up the output units 80 and 88, which were manipulated in
c---	the VAXvms via the logical symbols FOR080 and FOR088 in the DCL command
c---	file that ran the program.  There originally were no OPEN statements;
c---	They are inserted here as part of the porting.  
c---port      WRITE(80)epoch
c---port      WRITE(88,*)epoch
c---	the unit 80 file is binary data read by the Coverage program.  
c---	this file is the ephemeris file, a file of state vectors one minute 
c---	apart computed by ASAP from a start state vector.  
c---	get the short name for putting into the ephemeris metadata:
      call getenv('FOR080', file_name_80)
c--port    remove the output file if it already exists.  
      rcode = unlink(file_name_80)
      print *,'asap_main: file_name_80 = ', file_name_80
      open(80,file=file_name_80, form = 'UNFORMATTED', status = 'NEW')

c---	the unit 88 file is ascii test print; it prints out the 
c---	binary unit 80 data.  It is usually a null file.   
c---	The null device concept is ported here via 'if' statements for 
c---	every i/o command based on the filename. 
      call tfname('FOR088', file_name_88)
c--port    remove the output file if it already exists.    
      if ( file_name_88(1:4) .ne. 'NULL') 
     ?	rcode = unlink(file_name_88)
      print *, 'asap_main: file_name_88 = ', file_name_88
      I88 = 1
c---	I88 is the flag for output to 88, the ascii file.  
      if ( file_name_88(1:4) .eq. 'NULL')
     ?             I88 = 0
      if ( file_name_88(1:4) .ne. 'NULL') 
     ?             open(88,file=file_name_88, status = 'NEW')
c
      WRITE(80)epoch
      if(I88 .ne. 0) WRITE(88,*)epoch
      ib_rev = irev
C---	CHECK IFLAG:
c---			= 0	ASAP RUN TO GO INTO COVERAGE.
c---			= 1	NOT FOR COVERAGE - NO DATA BASE ACTIVITY.
      WRITE(*,*) 'ASAP_MAIN:  IFLAG = ', IFLAG
      IF(IFLAG .NE. 0 .AND. IFLAG .NE. 1) GO TO 9999

C---	ASAP RUN.  The routine ASAP is found in EASAP.FOR
      WRITE(*,*)' ASAP CALL STARTED.'
      CALL ASAP
      WRITE(*,*)' ASAP CALL FINISHED.'

c2/94   ljs:  the file MPS_ASAP9 is contains run parameters for this
c---	program.  the file is ASCII and contains annotations in it 
c---	which describe what each number is.  they are 
c---	start time (ephemeris time - et)
c---	stop time (et)
c---	epoch time (et) - usually the same as start time.
c---	prime meridian (deg) also called greenwich hour angle.
c2/94	state vector of satellite at the start.  6 real*8 numbers
c---	the file may end at this point.  if not, there are replication
c---	instructions to follow, which means to create the output for some
c---	other time bracket and rely on the orbit repeat cycle to create 
c---	the ephemeris output from the very first orbit.  
c---	Any orbit can be created from the first orbit and the repeat cycle
c---	information by several mathematical operations.  
C---	CHECK FOR REPLICATION STEP TO FOLLOW.  
c--- port      OPEN(9,FILE='MPS_ASAP9',READONLY,STATUS='OLD')
c---	note:  tfname is a new routine for the port task which 
c---	will decode the envvironment variables into file names.  
      call tfname ('MPS_ASAP9', file_name_9)
      print *, 'asap_main: MPS_ASAP9 = ', file_name_9
      print *, 'The run parameters are printed here:'
      OPEN(9,FILE=file_name_9,STATUS='OLD')
      READ(9,*) TSTART
      WRITE(*,*) 'TSTART = ', TSTART
      READ(9,*) TFD
      WRITE(*,*) 'TFD = ', TFD
      READ(9,*) TRD
      WRITE(*,*) 'TRD = ', TRD
      READ(9,*) PM
      WRITE(*,*) 'PM = ', PM
      READ(9,*) (X(I), I=1, 6)
      WRITE(*,*) 'X = ', X
      ireplicate = 0
      READ(9,*,END=7777) REPFLAG
      IF(REPFLAG .NE. 'REPLICATION') GO TO 9000
      READ(9,*,END=7777) TFIRST
      WRITE(*,*) 'TFIRST = ', TFIRST
      READ(9,*,END=7777) TFASF
      WRITE(*,*) 'TFASF = ', TFASF
      READ(9,*,END=7777) TLAST
      WRITE(*,*) 'TLAST = ', TLAST
      READ(9,*,END=7777) TLASF
      WRITE(*,*) 'TLASF = ', TLASF
      READ(9,*,END=7777) PNODE
      WRITE(*,*) 'PNODE = ', PNODE
C---	O.K. THIS MEANS THERE IS A REPLICATION TO TIME:  TLAST.  
C---	THE FILE UNIT 80 IS REWOUND AND REREAD.  OUTPUT IS TO 
C---	UNIT 90 AND UNIT 99.  
C---	THEN THE FILES FOR080 AND FOR088 ARE DELETED.  
      write(*,*) 'Replication for orbits was requested.  '
      ireplicate = 1
      WRITE(*,*) 'CALL ASAP_COPY'
      WRITE(*,*) 'TSTART,TFD,TFIRST,TLAST,PNODE='
      WRITE(*,*)  TSTART,TFD,TFIRST,TLAST,PNODE
      CALL ASAP_COPY(TSTART,TFD,TFIRST,TLAST,PNODE,ib_rev,ie_rev)
      WRITE(*,*) '                          ib_rev = ', ib_rev
      WRITE(*,*) 'CALL ASAP_COPY FINISHED.  ie_rev = ', ie_rev
C---	NOTE THE START AND END TIMES OF THIS RUN FOR THE DB RECORD.  
      GO TO 9000
 7777 CONTINUE
      CLOSE (9)
 9000 CONTINUE
      if(ireplicate .eq. 0) 
     ?       print *,'Replication for orbits was not requested.'
      IF(IFLAG .EQ. 1 ) GO TO 9999
      READ(9,*,END=7777) SVFNAME
      READ(9,*,END=7777) SVTYPE
      READ(9,*,END=7777) IBUF100(1:52)
      READ(9,*,END=7777) IBUF100(53:100)
      PHASE_NAME = ' '
      READ(9,*,END=9100) PHASE_NAME
 9100 CONTINUE
c
c--- port to unix.  what follows is the ingres embedded quel that will
c---	report the nominal orbit run information to the database.   
      srev = ib_rev
      erev = ie_rev
C---	CONVERT FROM G_FLOATING TO D_FLOATING REPRESENTATION.
c     epoch = MTH$CVT_G_D(epoch)
c---	open the  ingres database:
c     CALL OpenDB(%ref('MPSDB'))

      dbproc = db_open(
     ?   dbname(1:lastc(dbname)) // char(0),
     ?   'asapephm' // char(0),
     ?   sybase_userid(1:lastc(sybase_userid)) // char(0),
     ?   password(1:lastc(password)) // char(0),
     ?   %VAL(0), error_handler_exit, rcode)
      if(rcode .NE. 0) then
            call db_open_errs(
     ?          %VAL(rcode),
     ?          dbname(1:lastc(dbname)) // char(0),
     ?          sybase_userid(1:lastc(sybase_userid)) // char(0))
            rcode = system('banner "  ERROR"')
            call exit(1)
      endif

c---   first delete the previous rec, if any, for this sat/phase.  
      PRINT *,' CALLING asap_delete_ephmeris '
	  return_code = asap_delete_ephmeris( %VAL(dbproc), 
     ?              sat(1:2) // char(0), PHASE_NAME(1:1) // char(0) ) 

C---   Now append the new record.  
c##    append to ephemeris ( 
c##		 filename     = fname ,
c##      starttime    = stime ,
c##      endtime      = etime ,
c##      startrev     = srev ,
c##      endrev       = erev ,
c##      #sat         = sat ,
c##      #epoch       = epoch ,
c##      #phase_name  = PHASE_NAME,
c##	     sv_filename  = SVFNAME,
c##	     sv_type	  = SVTYPE,
c##	     sv_rev		  = IBUF100(1:5),
c##	     sv_time	  = IBUF100(7:27),
c##	     sv_r_x		  = IBUF100(29:39),
c##	     sv_r_y		  = IBUF100(41:51),
c##	     sv_r_z		  = IBUF100(53:63),
c##	     sv_v_x		  = IBUF100(65:75),
c##	     sv_v_y		  = IBUF100(77:87),
c##	     sv_v_z		  = IBUF100(89:99)  )
C---	just get the filename without the path:
      short_file_name_80 = file_name_80(LASTC(file_name_80)-8:)
      short_sv_file_name = SVFNAME(LASTC(SVFNAME)-8:)
C---  add in the : in column 5 to comply with ASF time format:  
C---   rev   IBUF100(1:5) 
C---   time  IBUF100(7:27)   21-character ASF format in database.  
C---   Rx    IBUF100(29:39) 
C---   Ry    IBUF100(41:51) 
C---   Rz    IBUF100(53:63)
C---   Vx    IBUF100(65:75)
C---   Vy    IBUF100(77:87)
C---   Vz    IBUF100(89:99)
      IBUF100(11:11) = ':'
      PRINT *,' CALLING asap_insert_ephemeris '
      return_code = asap_insert_ephemeris( %VAL(dbproc),
     ?     short_file_name_80(1:LASTC(short_file_name_80)) // char(0),
     ?     stime(1:21) // char(0),
     ?     etime(1:21) // char(0),
     ?     srev, 
     ?     erev,
     ?     sat(1:2) // char(0), 
     ?     epoch,
     ?     PHASE_NAME(1:1) // char(0),
     ?     short_sv_file_name(1:LASTC(short_sv_file_name)) // char(0),
     ?     SVTYPE(1:LASTC(SVTYPE)) // char(0),
     ?     IBUF100(1:5) // char(0),
     ?     IBUF100(7:27) // char(0),
     ?     IBUF100(29:39) // char(0),
     ?     IBUF100(41:51) // char(0),
     ?     IBUF100(53:63) // char(0),
     ?     IBUF100(65:75) // char(0),
     ?     IBUF100(77:87) // char(0),
     ?     IBUF100(89:99) // char(0) )

	  if ( return_code .ne. 0) go to 8001

      print *,'asap_main.for:  ephemeris file inserted into the ',
     ? 'ephemeris relation for ',
     ?  sat(1:LASTC(sat)), ', ', PHASE_NAME(1:LASTC(PHASE_NAME)), '.'
c---	close the ingres database
c##    exit
c---	allow the Sybase session to end with the fortran STOP.  
      go to 9999
 8001 continue
c---	error during the append.  
      print *,'asap_main.for:  error occurred during data insertion to'
      print *,'                the ephemeris relation.  '
      print *,'                terminating the run. '
      rcode = system('banner ERROR')
      call exit(255)
c
 9999 CONTINUE
      print *,'Normal end of ASAP reached.  '
      rcode = system('date')
      rcode = system('banner "NORMAL END"')
      call exit(0)
      end
