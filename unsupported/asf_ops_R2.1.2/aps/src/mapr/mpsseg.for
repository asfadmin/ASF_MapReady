C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:  mpsseg.for
C--
C--  Description:   
C-- 
C--  Notes:
C--
C-- ==========================================================================

C-----------------------------------------------------------------------
C
C SUBROUTINE MPSSEG
C
C PURPOSE
C   ALLOW THE USER TO SELECT A SEG BY DARID,SEGID,DTKID,REV,TIME
C   DISPLAY THE TARGET SEG(S) ON THE MAP WITH IDENTIFICATION
C
C $Logfile:   ACS003:[BLD.MPS.MAPR.SRC]MPSSEG.QFV  $
C
C VARIABLES
C
C INPUT
C   WSID        WORKSTATION ID NUMBER
C   PROJN       PROJECTION TYPE
C   OBSLON      CENTER LONGITUDE (DEG)
C   OBSLAT      CENTER LATITUDE (DEG)
C   STMNX,STMXX MIN/MAX WINDOW X
C   STMNY,STMXY MIN/MAX WINDOW Y
C
C OUTPUT
C   NSEG        NUMBER OF SEGMENTS CREATED
C   CRSEGN      SEGMENT STATUS ARRAY
C   CRSEGT      SEGMENT NAMES ARRAY
C
C INTERNAL
C   SEGNAM      SEGMENT NAME
C   IOS     INPUT/OUTPUT STATUS NUMBER
C   STRTT,STOPT START/STOP TIME - ASF
C   DTKID       DATATAKE ID
C   SAT,SEN     SATELLITE AND SENSOR ID
C   DARID,SEGID SEGMENT DARID AND SEGID
C   OPT     USER CHOICE
C   REV     SATELLITE REV NUMBER
C   SEGS        TEMP ARRAY TO STORE TARGET DARID/SEGIDS
C   SEGN        SEG COUNTER
C
C SUBROUTINES CALLED 
C        CRTSEG  INTIALTES THE CREATION OF THE GROUND TRACK SEGMENT.
C        LTRANS  LAT/LON TO X/Y COORDINATED TRANSFORMATION
C        DISGT   DISPLAYS THE GROUND TRACK AND TEXT.
C        CLSSEG  CLOSES THE CREATED GROUND TRACK SEGMENT
C
C WRITTEN BY CRAIG K. FUJIMOTO
C
C MODIFICATIONS
C $Date$ $Revision$ $Author$
C
C 7/11/94  Nadia Adhami -port to UNIX/C db_get_records()
C          Nadia        SENS param -> used SEN  instead
C 1/03/95  Nadia Adhami CAN NOT DISPLAY MORE THAN 1000 SEGMENTS
C 1/03/95  Nadia Adhami CAN NOT DISPLAY MORE THAN 100 POINTS
C 4/24/95  Nadia Adhami ASK_SENSOR(SEN) -> (SAT,SEN)
C-----------------------------------------------------------------------

       SUBROUTINE MPSSEG (WSID,PROJN,
     1                    OBSLAT,OBSLON,
     2                    STMNX,STMXX,STMNY,STMXY,
     2                    NSEG,CRSEGN,CRSEGT)

      character*100 SccsFileID
     -/'@(#)mpsseg.for	5.1 98/01/08 APS/ASF\0'/

       IMPLICIT NONE

C INPUT:
       INTEGER PROJN,WSID
       REAL OBSLON,OBSLAT
       REAL STMNX,STMXX,STMNY,STMXY

C OUTPUT
       CHARACTER*(*) CRSEGT(*) 
       INTEGER NSEG,CRSEGN(*)

C INTERNAL:

       CHARACTER*20 SEGNAM
       CHARACTER*21 STRTT,STOPT
       CHARACTER*15 DTKID
       CHARACTER*2 SAT
       CHARACTER*3 SEN

       INTEGER DARID,SEGID
       INTEGER OPT,REV,SEGS(1000,2),SEGN

C-----------------------------------------------------------------------

C DISPLAY SELECTION MENU
 1000  CONTINUE
       CALL SEGMNU(OPT)

C SELECT SEGS BY SEGID

       IF (OPT .EQ. 1) THEN

         CALL ASK_DARID(DARID)
         IF(DARID .EQ.0 ) GO TO 1000

         CALL ASK_SEGID(SEGID)

C SELECT SEGS BY DARID

       ELSE IF (OPT .EQ. 2) THEN
  
         CALL ASK_DARID(DARID)
         IF(DARID .EQ.0 ) GO TO 1000

C SELECT SEGS BY DTKID

       ELSE IF (OPT .EQ. 3) THEN

         CALL ASK_DTKID(DTKID)


C SELECT SEGS BY TIME

       ELSE IF (OPT .EQ. 4) THEN

         CALL ASK_TIME(STRTT,' START TIME (yyyy:ddd:hh:mm) : ')

         CALL ASK_TIME(STOPT,' STOP TIME (yyyy:ddd:hh:mm) : ')

C SELECT SEGS BY REV

       ELSE IF (OPT .EQ. 5) THEN

         CALL ASK_SAT(SAT)

         CALL ASK_SENSOR(SAT , SEN)

         CALL ASK_REV(REV,' REV [1-99999] : ')
                                     
       ELSE
         GO TO 9999
       END IF   


C FORM SEGNAME
       CALL GET_SEGNAME(OPT,DARID,SEGID,DTKID,
     1              STRTT,STOPT,
     2              SAT,SEN,REV,
     3              SEGNAM)

C RETRIEVE THE DAR AND SEG IDS OF THE SEGMENTS
       CALL GET_SEG(OPT,DARID,SEGID,DTKID,
     1              STRTT,STOPT,
     2              SAT,SEN,REV,
     3              SEGS,SEGN)

       IF (SEGN .EQ. 0) GO TO 9999

C DISPLAY THE SEGS
       CALL DIS_SEG (WSID,PROJN,
     1               OBSLAT,OBSLON,
     2               STMNX,STMXX,STMNY,STMXY,
     3               SEGS,SEGN,
     4               NSEG,CRSEGN,CRSEGT,
     5               SEGNAM)

 9999  CONTINUE
       RETURN
       END


C-----------------------------------------------------------------------
C
C SUBROUTINE GET_SEGNAME
C
C PURPOSE
C   FORM THE GKS SEGMENT NAME
C
C INPUT
C   OPT     USER OPTION
C   DARID       <
C   SEGID       <
C   DTKID       <
C   STRTT,STOPT START/STOP TIME - ASF FORMAT
C   SAT,SEN,REV SATELLITE,SENSOR,REVOLUTION
C
C OUTPUT
C   SEGNAM      SEGMENT NAME
C
C INTERNAL
C   RLEN,DLEN,XLEN  STRING LENGTHS
C   IOS     I/O ERROR STATUS
C   DARTXT,SEGTXT,REVTXT    TEXT TO FORM SEG NAME
C
C WRITTEN BY CRAIG K. FUJIMOTO
C
C-----------------------------------------------------------------------
       SUBROUTINE GET_SEGNAME(OPT,DARID,SEGID,DTKID,
     1              STRTT,STOPT,
     2              SAT,SEN,REV,
     3              SEGNAM)

       IMPLICIT NONE

C INPUT
       INTEGER OPT,DARID,SEGID,REV
       CHARACTER*(*) DTKID,STRTT,STOPT,SAT,SEN

C OUTPUT
       CHARACTER*(*) SEGNAM

C INTERNAL
       INTEGER RLEN,DLEN,XLEN,IOS
       CHARACTER*10 DARTXT,SEGTXT,REVTXT

C-----------------------------------------------------------------------

C USE THE DARID AND SEGID AS THE SEGNAME

       IF (OPT .EQ. 1) THEN

         ENCODE(10,'(I10)',DARTXT,IOSTAT=IOS) DARID
         ENCODE(5,'(I5)',SEGTXT,IOSTAT=IOS) SEGID

         CALL TRIM(DARTXT,10,DLEN)
         CALL TRIM(SEGTXT,5,XLEN)

         SEGNAM = 'SEG ' // DARTXT(1:DLEN) // '/' // SEGTXT(1:XLEN)

C USE THE DARID AS THE SEGNAME
         
       ELSE IF (OPT .EQ. 2) THEN
                 
         ENCODE(10,'(I10)',DARTXT,IOSTAT=IOS) DARID

         CALL TRIM(DARTXT,10,DLEN)

         SEGNAM = 'SEG ' // DARTXT(1:DLEN)

C USE THE DTKID AS THE SEGNAME

       ELSE IF (OPT .EQ. 3) THEN

         CALL TRIM(DTKID,15,DLEN)

         SEGNAM = 'SEG ' // DTKID(1:DLEN)

C USE THE YEAR AND DAY AS THE SEGNAME - YYYY:DDD

       ELSE IF (OPT .EQ. 4) THEN

         DLEN = 8

         SEGNAM = 'SEG ' // STRTT(1:DLEN)

C USE THE REV NUMBER AS THE SEGNAME

       ELSE IF (OPT .EQ. 5) THEN

         ENCODE(5,'(I5)',REVTXT,IOSTAT=IOS) REV

         CALL TRIM(REVTXT,5,RLEN)

         SEGNAM = 'SEG ' // SAT(1:2) // '/' // SEN(1:3) // '/' // 
     1            REVTXT(1:RLEN)

       END IF

 9999  CONTINUE
       RETURN
       END          


C-----------------------------------------------------------------------
C
C SUBROUTINE GET_SEG
C
C PURPOSE
C   RETRIEVE THE SEGMENTS FROM THE MPS SEG RELATION
C
C INPUT
C   OPT     USER OPTION
C   DARID       <
C   SEGID       <
C   DTKID       <
C   STRT,STOPT  START/STOP TIME
C   SAT/SEN/REV SATELLITE/SENSOR/REVOLUTION
C
C OUTPUT
C   SEGS        ARRAY OF SEGMENT DARID/SEGIDS TO RETRIEVE
C   SEGN        NUMBER OF SEGS TO RETRIEVE
C
C INPUT
C   ERRNO,RCOUNT    INGRES ERROR AND ROW COUNT
C
C WRITTEN BY CRAIG K. FUJIMOTO
C
C-----------------------------------------------------------------------
       SUBROUTINE GET_SEG(OPT,DARID,SEGID,DTKID,
     1              STRTT,STOPT,
     2              SAT,SEN,REV,
     3              SEGS,SEGN)

       IMPLICIT NONE

       INCLUDE 'mapr_db_extern.inc'
       INCLUDE 'APS_ROOT:include/local/f77_db_seg.inc'


       CHARACTER*(*) DTKID,STRTT,STOPT,SAT,SEN
       INTEGER OPT,DARID,SEGID,REV,SEGS(1000,2),SEGN

       character*10 schar
       integer      sint
       integer      LASTC
       integer      NRECS
       integer   LOCAL_DUMMY_VALUEHOLDER
       real     sreal(2)


C-----------------------------------------------------------------------
C FOLLOWING ASSIGNMENT REMOVES WARNING MESSAGE FOR UNUSED VARIABLE PTR
      INTEGER*4 TEMP
      DATA TEMP /10/
       PTRPTR = LOC (TEMP)
       LOCAL_DUMMY_VALUEHOLDER = PTR

C INIT COUNTER

       SEGN = 1                           

C RETRIEVE BY SEGID

       IF (OPT .EQ. 1) THEN 

c--port##       RETRIEVE (SEGS(SEGN,1) = SEG.#DARID,SEGS(SEGN,2) = SEG.#SEGID)
c##         WHERE SEG.#DARID = DARID AND SEG.#SEGID = SEGID
c##       {
c           SEGN = SEGN + 1
c##       }

c##       INQUIRE_EQUEL(ERRNO = ERRORNO, RCOUNT = ROWCOUNT)

       WRITE(WHERE_CLAUSE, 1) DARID,SEGID
    1  FORMAT('where darid = ',I,' and segid = ',I)

C RETRIEVE BY DARID

       ELSE IF (OPT .EQ. 2) THEN

c--port##       RETRIEVE (SEGS(SEGN,1) = SEG.#DARID,SEGS(SEGN,2) = SEG.#SEGID)
c##         WHERE SEG.#DARID = DARID
c##       {
c           SEGN = SEGN + 1
c##       }

c##       INQUIRE_EQUEL(ERRNO = ERRORNO, RCOUNT = ROWCOUNT)
                       
       WRITE(WHERE_CLAUSE, 2) DARID
    2  FORMAT('where darid = ',I)

C RETRIEVE BY DTKID

       ELSE IF (OPT .EQ. 3) THEN

         WRITE(6,*) 'DTK:',DTKID

c--port##       RETRIEVE (SEGS(SEGN,1) = SEG.#DARID,SEGS(SEGN,2) = SEG.#SEGID)
c##         WHERE SEG.#DTKID = DTKID
c##       {
c           SEGN = SEGN + 1
c##       }

c##       INQUIRE_EQUEL(ERRNO = ERRORNO, RCOUNT = ROWCOUNT)

c      changed print FORMAT for DTKID from I to A

       WRITE(WHERE_CLAUSE, 3) DTKID
    3  FORMAT('where dtkid = "',A,'"')
 
C RETRIEVE BY TIME

       ELSE IF (OPT .EQ. 4) THEN

c--port##       RETRIEVE (SEGS(SEGN,1) = SEG.#DARID,SEGS(SEGN,2) = SEG.#SEGID)
c##         WHERE SEG.#STRTTIME >= STRTT AND SEG.#STOPTIME <= STOPT
c##       {
c           SEGN = SEGN + 1
c##       }

c##       INQUIRE_EQUEL(ERRNO = ERRORNO, RCOUNT = ROWCOUNT)

       WRITE(WHERE_CLAUSE, 4) STRTT,STOPT
    4  FORMAT('where strttime >= "',A,'" and stoptime <= "',A,'"')
 
C RETRIEVE BY REV

       ELSE IF (OPT .EQ. 5) THEN
         
c--port##       RETRIEVE (SEGS(SEGN,1) = SEG.#DARID,SEGS(SEGN,2) = SEG.#SEGID)
c##         WHERE SEG.#SAT = SAT AND SEG.#SENSOR = SENS AND SEG.#REV = REV
c##       {
c           SEGN = SEGN + 1
c##       }

c##       INQUIRE_EQUEL(ERRNO = ERRORNO, RCOUNT = ROWCOUNT)


       WRITE(WHERE_CLAUSE, 5) SAT,SEN,REV
    5  FORMAT('where sat = "',A,'" and sensor = "',A,'" ',
     ?   'and rev = ',I)

       END IF                                    

       WHERE_CLAUSE = WHERE_CLAUSE(1:LASTC(WHERE_CLAUSE))//char(0)

       llistptr = db_get_records(%VAL(MAPR_DBPROC), 'seg'//char(0),
     ?    WHERE_CLAUSE, char(0),SEG_COLUMNS, %VAL(ALL_COLS))
       if ( llistptr .EQ. 0 ) THEN 
           CALL DISMSG('Error : in database query on seg')
           GO TO 9999
       ENDIF

       call get_no_elements(llist,NRECS)
       IF (NRECS .EQ. 0) THEN
           CALL DISMSG('Error : No Segment record found.')
           CALL db_ftn_free_llist(llist) 
           SEGN = 0
           GOTO 9999
       ENDIF 

       P2_DATA_RECORD = db_ftn_first_record(llist, ptrptr)

       SEGN = 0
       DO 400 WHILE (P2_DATA_RECORD .NE. 0 .AND.
     1      SEGN .LT. 1000)
          SEGN = SEGN + 1
          call get_seg_rec(%VAL(P2_DATA_RECORD),
     ?    schar,schar,sint,SEGS(SEGN,1),SEGS(SEGN,2),
     ?    schar,schar,sreal,sreal,sreal,sreal,schar)
         P2_DATA_RECORD = db_ftn_next_record(llist, ptrptr)
 400   CONTINUE
 
 401   CONTINUE
       IF (SEGN .EQ. 1000) THEN
         WRITE(*,*) 'CAN NOT DISPLAY MORE THAN 1000 SEGMENT OVERLAYS.'
       ENDIF
 
       CALL db_ftn_free_llist(llist) 

 9999  CONTINUE
       RETURN
       END


C-----------------------------------------------------------------------
C
C SUBROUTINE DIS_SEG
C
C PURPOSE
C   DISPLAY THE SEGMENTS AND ASSOCIATED TEXT
C
C INPUT
C   WSID        WORKSTATION ID
C   PROJN       PROJECTION NUMBER
C   OBSLAT,OBSLON   USER SPECIFIED CENTER LAT/LON
C   STMNX,STMXX MIN/MAX WINDOW X
C   STMNY,STMXY MIN/MAX WINDOW Y
C   SEGS        DARID/SEGID ARRAY OF SEGMENTS
C   SEGN        NUMBER OF SEGS TO DISPLAY
C   SEGNAM      SEGMENT NAME
C
C OUTPUT
C   NSEG        GKS SEGMENT COUNTER
C   CRSEGN      GKS SEGMENT STATUS
C   CRSEGT      GKS SEGMENT NAME
C
C INTERNAL
C   STRTJD,STOPJD   START/STOP EPHEMERIS TIME
C   ONEMIN      ONE MINUTE "         "
C   XDATE       TARGET     "         "
C   SX,SY       X/Y POINT
C   SW1(),SW2() FOUR CORNERS X/Y OF SWATH
C   OLDSW(),SWT()   TEMP HOLDERS FOR SWATH POINTS
C   TEXX,TEXY   X/Y OF TEXT
C   NRLAT(),NRLON() NEAR LAT/LON POINTS
C   FRLAT(),FRLON() FAR LAT/LON POINTS
C   CNRLAT(),CNRLON()   COVERAGE NEAR LAT/LON POINTS
C   CFRLAT(),CFRLON()   COVERAGE FAR LAT/LON POINTS
C   ERRNO,RCOUNT    INGRES ERROR NUM AND ROW COUNT
C   ERRFLAG     ERROR FLAG
C   I,J     COUNTERS
C   LAST        FLAG INDICATING LAST PAIR OF POINTS
C   TDARID,TSEGID   TEMP DARID/SEGID
C   REV     SAT REV
C   XLEN,DLEN   STRING LENGTHS
C   TSTRTT,TSTOPT   START/STOP TIMES -ASF FORMAT
C   STRTY,STOPY START/STOP YEAR
C   XYEAR       TARGET YEAR
C   STRTD,STOPD START/STOP TIME
C   XTIME       TARGET TIME
C   SAT,SEN     SATELLITE AND SENSOR
C   ASCDSC      ASCENDING/DESCENDING FLAG
C   DARTXT      DARID TEXT 
C   SEGTXT      SEGID TEXT
C   STXT        SEG TEXT
C   *HID()      FLAGS INDICATING IF POINT IS HIDDEN
C   BRNCUT      FLAG INDICATING IF LINE IS BROKEN
C
C WRITTEN BY CRAIG K. FUJIMOTO
C
C-----------------------------------------------------------------------
       SUBROUTINE DIS_SEG (WSID,PROJN,
     1               OBSLAT,OBSLON,
     2               STMNX,STMXX,STMNY,STMXY,
     3               SEGS,SEGN,
     4               NSEG,CRSEGN,CRSEGT,
     5               SEGNAM)

       IMPLICIT NONE

                               
C INPUT

c--port       INCLUDE 'SYS$LIBRARY:GKSDEFS.BND'
       INCLUDE 'GKS_ROOT:include/fgksenum.inc'
       INCLUDE 'mapr_db_extern.inc'
       INCLUDE 'mapper_port.inc'
       INCLUDE 'APS_ROOT:include/local/f77_db_seg.inc'
       INCLUDE 'APS_ROOT:include/local/f77_db_cvrg.inc'

       INTEGER WSID,PROJN
       REAL OBSLAT,OBSLON
       REAL STMNX,STMXX,STMNY,STMXY

       CHARACTER*(*) SEGNAM

       INTEGER SEGS(1000,2),SEGN

  
C OUTPUT
       INTEGER NSEG,CRSEGN(*)
       CHARACTER*(*) CRSEGT(*)

C INTERNAL                      

       DOUBLE PRECISION STRTET,STOPET
       DOUBLE PRECISION ONEMIN,XDATE         

       REAL
     1      SW1(2,2),SW2(2,2),
     2      OLDSW(2,2),SWT(2,2),
     3      TEXX,TEXY
       REAL NRLAT(2),NRLON(2),FRLAT(2),FRLON(2)
       REAL CNRLAT(100),CNRLON(100),CFRLAT(100),CFRLON(100)

       INTEGER ERRFLAG
       INTEGER I,J,LAST
       INTEGER TDARID,TSEGID,REV
       INTEGER XLEN,DLEN
       INTEGER IOS

       CHARACTER*21 TSTRTT,TSTOPT
       CHARACTER*3 SEN
       CHARACTER*2 SAT                            
       CHARACTER*1 ASCDSC

       CHARACTER*11 DARTXT,SEGTXT
       CHARACTER*15 STXT

       LOGICAL SW1HID(2),SW2HID(2)
       LOGICAL SWTHID(2),OLDHID(2)
       LOGICAL BRNCUT,TEXHID

       INTEGER id

       INCLUDE 'APS_HOME:include/local/timeconv.inc'

C FUNCTIONS

       integer SLEN
       integer LASTC
       integer NRECS
       integer   LOCAL_DUMMY_VALUEHOLDER
       real    rdummy(100)

       REAL  GET_DEF_CH

C-----------------------------------------------------------------------
C FOLLOWING ASSIGNMENT REMOVES WARNING MESSAGE FOR UNUSED VARIABLE PTR
      INTEGER*4 TEMP
      DATA TEMP /10/
       PTRPTR = LOC (TEMP)
       LOCAL_DUMMY_VALUEHOLDER = PTR

       ONEMIN = 1.0 / 24.0 / 60.0

C GRAPHICS PARAMETERS

C TEXT HEIGHT
       CALL GSCHH (GET_DEF_CH())
C TEXT ALIGNMENT
       CALL GSTXAL (GALEFT,GAHALF)

C TEXT COLOR INDEX
       CALL GSTXCI (7)
C POLYLINE COLOR INDEX
       CALL GSPLCI (7)


C CREATE THE SEGMENT

       WRITE(6,100) SEGNAM
  100  FORMAT(/,1X,'Creating overlay ',A<SLEN(SEGNAM)>,'...',$)

       CALL CRTSEG (WSID,NSEG,SEGNAM,CRSEGN,CRSEGT)

C LOOP THROUGH EACH OF THE SEG RECORDS

       DO 1000 I = 1,SEGN

         CALL DISMSGW ('Retrieving Segment record...')

         TDARID = SEGS(I,1)
         TSEGID = SEGS(I,2)

C RETRIEVE THE RECORD FROM THE SEG RELATION

c--port##       RETRIEVE(SAT = SEG.#SAT,SEN = SEG.#SENSOR, REV = SEG.#REV,
c##                TSTRTT = SEG.#STRTTIME, TSTOPT = SEG.#STOPTIME,
c##                NRLAT(1) = SEG.#NRLAT1, NRLON(1) = SEG.#NRLON1,
c##                FRLAT(1) = SEG.#FARLAT1,FRLON(1) = SEG.#FARLON1,
c##                NRLAT(2) = SEG.#NRLAT2, NRLON(2) = SEG.#NRLON2,
c##                FRLAT(2) = SEG.#FARLAT2,FRLON(2) = SEG.#FARLON2,
c##                ASCDSC = SEG.#ASCDSC)
c##         WHERE SEG.#DARID = TDARID AND SEG.#SEGID = TSEGID
c##       {                          
c##       }

c##       INQUIRE_EQUEL(ERRNO=ERRORNO, RCOUNT=ROWCOUNT)


       WRITE(WHERE_CLAUSE, 1) TDARID,TSEGID
    1  FORMAT('where darid = ',I,' and segid = ',I)
       WHERE_CLAUSE = WHERE_CLAUSE(1:LASTC(WHERE_CLAUSE))//char(0)

       llistptr = db_get_records(%VAL(MAPR_DBPROC), 'seg'//char(0),
     ?    WHERE_CLAUSE, char(0),SEG_COLUMNS,%VAL(ALL_COLS))
       if ( llistptr .EQ. 0 ) THEN 
           CALL DISMSG('Error : in database query on seg')
           GO TO 9999
       ENDIF

       call get_no_elements(llist,NRECS)
       IF (NRECS .NE. 0) THEN
 
           P2_DATA_RECORD = db_ftn_first_record(llist, ptrptr)

           call get_seg_rec(%VAL(P2_DATA_RECORD),
     ?      SAT,SEN,REV,id,id,
     ?      TSTRTT,TSTOPT,NRLAT,NRLON,FRLAT,FRLON,ASCDSC)

           CALL db_ftn_free_llist(llist)
       ELSE 
           CALL DISMSG('Error : No Segment record found.')
           CALL db_ftn_free_llist(llist)
           GO TO 9999
       END IF

C VALIDATE THE SEG DATA

         IF (NRLAT(1) .LT. -90.0  .OR. NRLAT(1) .GT. 90.0  .OR.
     1       NRLON(1) .LT. -180.0 .OR. NRLON(1) .GT. 180.0 .OR.
     2       FRLAT(1) .LT. -90.0  .OR. FRLAT(1) .GT. 90.0  .OR.
     3       FRLON(1) .LT. -180.0 .OR. FRLON(1) .GT. 180.0 .OR.
     4       NRLAT(2) .LT. -90.0  .OR. NRLAT(2) .GT. 90.0  .OR.
     5       NRLON(2) .LT. -180.0 .OR. NRLON(2) .GT. 180.0 .OR.
     6       FRLAT(2) .LT. -90.0  .OR. FRLAT(2) .GT. 90.0  .OR.
     7       FRLON(2) .LT. -180.0 .OR. FRLON(2) .GT. 180.0 .OR.
     8       REV .LT. 1 .OR. REV .GT. 99999) THEN

           CALL DISMSG('Error: Invalid Segment record found.')
           GO TO 9999

         END IF

C CONVERT THE FIRST POINT TO X AND Y

         CALL LTRANS (PROJN,OBSLAT,OBSLON,
     1               NRLON(1),NRLAT(1),
     2               SW1(1,1),SW1(1,2),
     3               SW1HID(1),BRNCUT)

         CALL LTRANS (PROJN,OBSLAT,OBSLON,
     1                FRLON(1),FRLAT(1),
     2                SW1(2,1),SW1(2,2),
     3                SW1HID(2),BRNCUT)

C DETERMINE THE POINT FOR THE TEXT TO BE DISPLAYED

         IF (SW1(1,1) .GT. SW1(2,1)) THEN
           TEXX = SW1(1,1)
           TEXY = SW1(1,2)
           TEXHID = SW1HID(1)
         ELSE
           TEXX = SW1(2,1)
           TEXY = SW1(2,2)
           TEXHID = SW1HID(2)
         END IF

C CONVERT THE SECOND POINT TO X AND Y

         CALL LTRANS (PROJN,OBSLAT,OBSLON,
     1              NRLON(2),NRLAT(2),
     2              SW2(1,1),SW2(1,2),
     3              SW2HID(1),BRNCUT)

         CALL LTRANS (PROJN,OBSLAT,OBSLON,
     1              FRLON(2),FRLAT(2),
     2              SW2(2,1),SW2(2,2),
     3              SW2HID(2),BRNCUT)

C CONVERT THE START AND STOP TIMES TO JULIAN DATES

         IOS = tc_asf2et(TSTRTT // char(0),STRTET)
         IF (IOS .NE. 1) THEN
           CALL DISMSG('Error converting ASF time.')
           GO TO 9999
         END IF

         IOS = tc_asf2et(TSTOPT // char(0),STOPET)
         IF (IOS .NE. 1) THEN
           CALL DISMSG('Error converting ASF time.')
           GO TO 9999
         END IF

C CHECK IF THE SEG STRT AND STOP TIMES ARE MORE THAN A MINUTE APART
                        
         IF (STRTET .GE. STOPET) THEN 
                                           
           GO TO 1000

C IF NOT MORE THAN 1 MINUTE APART, THEN DISPLAY THE SEGMENT

         ELSE IF ((STOPET - STRTET) .LT. ONEMIN) THEN 

           IF (.NOT. SW1HID(1) .OR. .NOT. SW1HID(2) .OR.
     1         .NOT. SW2HID(1) .OR. .NOT. SW2HID(2)) THEN

             LAST = 1

             CALL DIS_SWATH (PROJN,
     1                 SW1,SW2,
     2                 SW1HID,SW2HID,
     3                 STMNX,STMXX,STMNY,STMXY,
     4                 LAST)
           END IF

C IF MORE THAN 1 MINUTE APART, THEN GET COVERAGE BETWEEN THE 2 POINTS

         ELSE


C INIT COUNTER                       

           J = 1

C RETRIEVE MATCHING CVRG RECORDS

           CALL DISMSGW ('Retrieving Coverage records...')
                                
           ERRFLAG = 0

c--port##         RETRIEVE (XDATE = CVRG.#MJDATE,
c##                   CNRLAT(J) = CVRG.#NRLAT,
c##                   CNRLON(J) = CVRG.#NRLON,
c##                   CFRLAT(J) = CVRG.#FARLAT,
c##                   CFRLON(J) = CVRG.#FARLON)
c##         WHERE CVRG.#SAT = SAT AND CVRG.#SENSOR = SEN AND CVRG.#REV = REV
c##         AND   CVRG.#MJDATE >= STRTET AND CVRG.#MJDATE <= STOPET
c##         SORT BY #XDATE
c##         {

        WRITE(WHERE_CLAUSE, 6) SAT,SEN,REV,STRTET,STOPET
    6   FORMAT('where sat = "',A,'" and sensor = "',A,'" and rev = ',
     ?  I,' and mjdate >= ',F,' and mjdate <= ',F)
        WHERE_CLAUSE = WHERE_CLAUSE(1:LASTC(WHERE_CLAUSE))//char(0)
 
        llistptr = db_get_records(%VAL(MAPR_DBPROC), 'cvrg'//char(0),
     ?    WHERE_CLAUSE, 'mjdate'//char(0),CVRG_COLUMNS,%VAL(ALL_COLS))
       if ( llistptr .EQ. 0 ) THEN 
           CALL DISMSG('Error : in database query on cvrg')
           GO TO 2200
       ENDIF
 
        call get_no_elements(llist,NRECS)
        IF (NRECS .EQ. 0) THEN
            CALL DISMSG('No Coverage records found.')
            CALL db_ftn_free_llist(llist)
            GOTO 2200
        ENDIF

        P2_DATA_RECORD = db_ftn_first_record(llist, ptrptr)

        J = 0 
        DO 2000 WHILE (P2_DATA_RECORD .NE. 0 .AND.
     1       J .LT. 100)

             J = J + 1
             call get_cvrg_rec(%VAL(P2_DATA_RECORD),
     ?       SAT,SEN,REV,XDATE,rdummy,rdummy,
     ?       CNRLAT(J),CNRLON(J),CFRLAT(J),CFRLON(J),ASCDSC)

C VALIDATE THE COVERAGE RECORD DATA

             IF (CNRLAT(J).LT.-90.0  .OR. CNRLAT(J).GT.90.0  .OR.
     1           CNRLON(J).LT.-180.0 .OR. CNRLON(J).GT.180.0 .OR.
     2           CFRLAT(J).LT.-90.0  .OR. CFRLAT(J).GT.90.0  .OR.
     3           CFRLON(J).LT.-180.0 .OR. CFRLON(J).GT.180.0) THEN
                                                           
               ERRFLAG = 1
C--            exit the loop
               GO TO 2100

             END IF

C CONVERT THE NEAR POINT TO X AND Y

             CALL  LTRANS (PROJN,OBSLAT,OBSLON,
     1                    CNRLON(J),CNRLAT(J),
     2                    SWT(1,1),SWT(1,2),
     3                    SWTHID(1),BRNCUT)

C CONVERT THE FAR POINT TO X AND Y

             CALL LTRANS (PROJN,OBSLAT,OBSLON,
     1                    CFRLON(J),CFRLAT(J),
     2                    SWT(2,1),SWT(2,2),
     3                    SWTHID(2),BRNCUT)
             
C IF FIRST CVRG RECORD, DISPLAY SPC REC AND CVRG REC

             IF (J .EQ. 1) THEN
                            
               IF (.NOT. SW1HID(1) .OR. .NOT. SW1HID(2) .OR.
     1             .NOT. SWTHID(1) .OR. .NOT. SWTHID(2)) THEN

                 CALL DIS_SWATH (PROJN,
     1                 SW1,SWT,
     2                 SW1HID,SWTHID,
     3                 STMNX,STMXX,STMNY,STMXY,
     4                 LAST)

               END IF

C IF NOT 1ST REC, DISPLAY PREVIOUS AND CURRENT CVRG REC

             ELSE

               IF (.NOT. OLDHID(1) .OR. .NOT. OLDHID(2) .OR.
     1             .NOT. SWTHID(1) .OR. .NOT. SWTHID(2)) THEN
                  
                 CALL DIS_SWATH (PROJN,
     1                 OLDSW,SWT,
     2                 OLDHID,SWTHID,
     3                 STMNX,STMXX,STMNY,STMXY,
     4                 LAST)
            
               END IF

             END IF

C SAVE CURRENT RECORD FOR DRAWING THE NEXT PIECE OF SWATH

             OLDSW(1,1) = SWT(1,1)
             OLDSW(1,2) = SWT(1,2)
             OLDSW(2,1) = SWT(2,1)
             OLDSW(2,2) = SWT(2,2)
           
             OLDHID(1) = SWTHID(1)
             OLDHID(2) = SWTHID(2)


         P2_DATA_RECORD = db_ftn_next_record(llist, ptrptr)

 2000    CONTINUE

 2100    CONTINUE

         CALL db_ftn_free_llist(llist)

 2200    CONTINUE

         IF (J .EQ. 100) THEN
              WRITE(*,*) 'CAN NOT DISPLAY MORE THAN 100 POINTS.'
         ENDIF

C---      error in code:  P2_DATA_RECORD is ALWAYS zero here:
C---      IF (ERRFLAG .NE. 0 .OR. P2_DATA_RECORD .EQ. 0) THEN

         IF (ERRFLAG .NE. 0 ) THEN

C SET UP TO DISPLAY THE SEGMENT WITHOUT CVRG RECORDS

           SWT(1,1) = SW1(1,1)
           SWT(1,2) = SW1(1,2)
           SWT(2,1) = SW1(2,1)
           SWT(2,2) = SW1(2,2)

           SWTHID(1) = SW1HID(1)
           SWTHID(2) = SW1HID(2)

         END IF                           

C DRAW THE SWATH FROM THE LAST CVRG REC TO THE SECOND SPC REC
                
         IF (.NOT. SWTHID(1) .OR. .NOT. SWTHID(2) .OR.
     1       .NOT. SW2HID(1) .OR. .NOT. SW2HID(2)) THEN

           LAST = 1

           CALL DIS_SWATH (PROJN,
     1           SWT,SW2,
     2           SWTHID,SW2HID,
     3           STMNX,STMXX,STMNY,STMXY,
     4           LAST)

         END IF

       END IF

C FORM THE SEGMENT TEXT

       IF (.NOT. TEXHID) THEN

         ENCODE(10,'(I10)',DARTXT) SEGS(I,1)
         ENCODE(5,'(I5)',SEGTXT) SEGS(I,2)

         CALL TRIM(DARTXT,10,DLEN)
         CALL TRIM(SEGTXT,5,XLEN)

         STXT = ' ' // DARTXT(1:DLEN) // '/' // SEGTXT(1:XLEN)

C DISPLAY THE TEXT

         CALL GTXS(TEXX,TEXY,LEN(STXT),STXT)

       END IF

C NEXT SEG RECORD

 1000  CONTINUE

 9000  CONTINUE

C CLOSE THE SEGMENT

       CALL CLSSEG
              
 9999  CONTINUE
       RETURN
       END 
