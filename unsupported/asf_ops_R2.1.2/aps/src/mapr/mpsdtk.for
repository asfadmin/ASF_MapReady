C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	mpsdtk.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================


       SUBROUTINE MPSDTK (WSID,PROJN,
     1                    OBSLAT,OBSLON,
     2                    STMNX,STMXX,STMNY,STMXY,
     3                    NSEG,CRSEGN,CRSEGT)

      character*100 SccsFileID
     -/'@(#)mpsdtk.for	5.1 98/01/08 APS/ASF\0'/

C
C SUBROUTINE MPSDTK
C
C PURPOSE
C	ASK THE USER TO SPECIFY THE DTK TO DISPLAY
C	RETRIEVE THE DATATAKE FORM THE MPS DTK RELATION
C	DISPLAT RHE DATATAKE AND ASSOCIATED TEXT
C
C $Logfile:   ACS003:[BLD.MPS.MAPR.SRC]MPSDTK.QFV  $
C
C VARIABLES
C INPUT
C	WSID  		WORKSTATION ID NUMBER
C	PROJN		PROJECTION TYPE
C	OBSLON	     	CENTER LONGITUDE (DEG)
C	OBSLAT		CENTER LATITUDE (DEG)
C	STMNX,STMXX	WINDOW MIN/MAX X
C	STMNY,STMXY	WINDOW MIN/MAX Y
C
C OUTPUT
C	NSEG  		NUMBER OF SEGMENTS CREATED
C	CRSEGN		SEGMENT STATUS ARRAY
C	CRSEGT		SEGMENT NAME ARRAY
C
C INTERNAL
C	OPT		USER OPTION
C	SEGNAM		NAME OF GKS SEGMENT TO CREATE
C	IOS		I/O ERROR STATUS
C	STRTT,STOPT	START/STOP TIME - ASF FORMAT
C	SAT,SEN		SATELLITE/SENSOR
C	REV,ID		DTK REV AND ID
C	DSATS(),DSENS()	SAT/SEN ARRAYS OF TARGET DATATAKES
C	DREVS(),DIDS()	REV/ID ARRAYS FOR TARGET DATATAKES
C	DTKN		NUMBER OF DATATAKES
C
C SUBROUTINES CALLED 
C        CRTSEG  INTIALTES THE CREATION OF THE GROUND TRACK SEGMENT.
C        LTRANS  PERFORMS THE COORDINATED TRANSFORMATION FOR THE
C        DISGT   DISPLAYS THE GROUND TRACK AND TEXT.
C        CLSSEG  CLOSES THE CREATED GROUND TRACK SEGMENT
C
C WRITTEN BY CRAIG K. FUJIMOTO
C
C MODIFICATIONS
C $Date$ $Revision$ $Author$
C
C 7/11/94  Nadia Adhami -port to UNIX/C (include GKSDEFS.BND)
C 7/8/94  Nadia Adhami -port to UNIX/C (INGRIS -> SYBASE)
C 1/3/95  Nadia Adhami CAN NOT DISPLAY MORE THAN 1000 DATATAKES
C
C-----------------------------------------------------------------------

       IMPLICIT NONE

C INPUT:
       INTEGER PROJN,WSID

       REAL OBSLON,OBSLAT
       REAL STMNX,STMXX,STMNY,STMXY

C OUTPUT:
       CHARACTER*(*) CRSEGT(*) 

       INTEGER NSEG,CRSEGN(*)

C INTERNAL:
       CHARACTER*21 SEGNAM
       CHARACTER*21 STRTT,STOPT
       CHARACTER*2 SAT,DSATS(1000)
       CHARACTER*3 SEN,DSENS(1000)

       INTEGER OPT,REV,ID,DREVS(1000),DIDS(1000),DTKN
       INTEGER MASKOPT

C-----------------------------------------------------------------------

C DISPLAY SELECTION MENU

       CALL DTKMNU(OPT)

C SELECT DTKS BY DTKID

       IF (OPT .EQ. 1) THEN

         CALL ASK_SAT(SAT)

         CALL ASK_SENSOR(SAT, SEN)

         CALL ASK_REV(REV,' REV [1-99999] : ')

         CALL ASK_ID(ID)

C SELECT DTKS BY TIME

       ELSE IF (OPT .EQ. 2) THEN            

         CALL ASK_TIME(STRTT,' START TIME (yyyy:ddd:hh:mm) : ')

         CALL ASK_TIME(STOPT,' STOP TIME (yyyy:ddd:hh:mm) : ')

         CALL CVGMASK(MASKOPT)

C SELECT DTKS BY REV

       ELSE IF (OPT .EQ. 3) THEN

         CALL ASK_SAT(SAT)

         CALL ASK_SENSOR(SAT, SEN)

         CALL ASK_REV(REV,' REV [1-99999] : ')

         CALL CVGMASK(MASKOPT)

       ELSE
         GO TO 9999
       END IF


C FORM SEGNAME
       CALL GET_DTKNAME(OPT,
     1              STRTT,STOPT,
     2              SAT,SEN,REV,ID,
     3              SEGNAM)

C RETRIEVE THE DTK INFO
       CALL GET_DTK(OPT,
     1              STRTT,STOPT,
     2              SAT,SEN,REV,ID,
     3              DSATS,DSENS,DREVS,DIDS,DTKN,MASKOPT)

       IF (DTKN .EQ. 0) GO TO 9999

C DISPLAY THE DATATAKES
       CALL DIS_DTK (WSID,PROJN,
     1               OBSLAT,OBSLON,
     2               STMNX,STMXX,STMNY,STMXY,
     3               DSATS,DSENS,DREVS,DIDS,DTKN,
     4               NSEG,CRSEGN,CRSEGT,SEGNAM)

 9999  CONTINUE
       RETURN
       END


C-----------------------------------------------------------------------
C
C SUBROUTINE GET_DTKNAME
C
C PURPOSE
C	FORM GKS SEGMENT NAME FROM DTK INFO
C
C INPUT
C	OPT		USER OPTION
C	STRTT,STOPTT	START/STOP TIME
C	SAT/SEN/REV/ID	DATATAKE IDENTIFIER
C
C OUTPUT
C	SEGNAM		GKS SEGMENT NAME
C
C INTERNAL
C	RLEN,DLEN,ILEN	STRING LENGTHS
C	REVTXT,IDTXT	REV AND ID ASCII TEXT   
C
C WRITTEN BY CRAIG K. FUJIMOTO
C
C-----------------------------------------------------------------------
       SUBROUTINE GET_DTKNAME(OPT,
     1              STRTT,STOPT,
     2              SAT,SEN,REV,ID,
     3              SEGNAM)

       IMPLICIT NONE

C INPUT:
       INTEGER OPT,REV,ID

       CHARACTER*(*) STRTT,STOPT,SAT,SEN

C OUTPUT:
       CHARACTER*(*) SEGNAM

C INTERNAL:
       INTEGER RLEN,DLEN,ILEN

       CHARACTER*10 REVTXT,IDTXT   

C-----------------------------------------------------------------------

C USE THE DTKID AS THE SEGNAME

       IF (OPT .EQ. 1) THEN

         ENCODE(5,'(I5)',REVTXT) REV
         ENCODE(2,'(I2)',IDTXT) ID

         CALL TRIM(REVTXT,5,RLEN)
         CALL TRIM(IDTXT,2,ILEN)

         SEGNAM = 'DTK ' // SAT(1:2) // '/' // SEN(1:3) // '/' // 
     1            REVTXT(1:RLEN) // '.' // IDTXT(1:ILEN)

C USE THE YEAR AND DAY AS THE SEGNAME - YYYY:DDD

       ELSE IF (OPT .EQ. 2) THEN

         DLEN = 8

         SEGNAM = 'DTK ' // STRTT(1:DLEN)

C USE THE REV NUMBER AS THE SEGNAME

       ELSE IF (OPT .EQ. 3) THEN

         ENCODE(5,'(I5)',REVTXT) REV

         CALL TRIM(REVTXT,5,RLEN)

         SEGNAM = 'DTK ' // SAT(1:2) // '/' // SEN(1:3) // '/' // 
     1            REVTXT(1:RLEN)

       END IF

 9999  CONTINUE
       RETURN
       END


C-----------------------------------------------------------------------
C        
C SUBROUTINE GET_DTK
C
C PURPOSE
C	RETRIEVE THE DATATAKE(S) FROM THE MPS DTK RELATION
C
C INPUT
C	STRTT,STOPTT	START/STOP TIME
C	SAT/SEN/REV/ID	DATATAKE IDENTIFIER
C	SEGNAM		GKS SEGMENT NAME
C	OPT		USER OPTION
C
C OUTPUT
C	DSATS,DSENS,DREVS,DIDS	DTK ID ARRAYS
C	DTKN		DATATAKE COUNT
C
C INTERNAL
C	ERRNO,RCOUNT	ERROR NUMBER AND ROW COUNT
C
C WRITTEN BY CRAIG K. FUJIMOTO
C
C-----------------------------------------------------------------------

       SUBROUTINE GET_DTK(OPT,
     1              STRTT,STOPT,
     2              SAT,SEN,REV,ID,
     3              DSATS,DSENS,DREVS,DIDS,DTKN,MASKOPT)

       IMPLICIT NONE

       INCLUDE 'mapr_db_extern.inc'
       INCLUDE 'APS_ROOT:include/local/f77_db_dtk.inc'
       INCLUDE 'APS_ROOT:include/local/f77_db_cvrg.inc'

C INPUT:
       INTEGER OPT,REV,ID,MASKOPT

       CHARACTER*(*) STRTT,STOPT,SAT,SEN

C OUTPUT:
       INTEGER DREVS(*),DIDS(*),DTKN

       CHARACTER*(*) DSATS(1000),DSENS(1000)

C INTERNAL:

c--port declarations needed to port to SUN

       CHARACTER*3 MASK
       INTEGER ERRNO
       INTEGER LASTC
       INTEGER NRECS
	   INTEGER   LOCAL_DUMMY_VALUEHOLDER
       character*1 ascdsc
       real nrlat(2),nrlon(2),farlat(2),farlon(2)
       character*21 starttime,stoptime

C-----------------------------------------------------------------------
C FOLLOWING ASSIGNMENT REMOVES WARNING MESSAGE FOR UNUSED VARIABLE PTR
      INTEGER*4 TEMP
      DATA TEMP /10/
       PTRPTR = LOC (TEMP)
       LOCAL_DUMMY_VALUEHOLDER = PTR

C INIT COUNTER       

       DTKN = 0

C SET THE MASK VARIABLE TO SELECT CVG IN MASK
 
       IF (MASKOPT .EQ. 2) THEN
         MASK = 'ASF'
       ELSE IF (MASKOPT .EQ. 3) THEN
         MASK = 'MCM'
       ELSE
         MASK = '   '
       END IF

C RETRIEVE BY DTKID
  
       IF (OPT .EQ. 1) THEN 

         WRITE(WHERE_CLAUSE, 1) SAT,SEN,REV,ID
    1    FORMAT('where sat = "',A,'" and sensor = "',A,'" and ',
     ?      'rev = ',I,' and dtkid = ',I)
 

C RETRIEVE BY TIME

       ELSE IF (OPT .EQ. 2) THEN

        IF ((MASK .EQ. 'ASF') .OR. (MASK .EQ. 'MCM')) THEN
          WRITE (WHERE_CLAUSE, 2) STRTT,STOPT,MASK
    2     FORMAT('where strttime >= "',A,'" and stoptime <= "',A,'"',
     ?      ' and station_id = "',A,'"')
        ELSE
          WRITE (WHERE_CLAUSE, 3) STRTT,STOPT
    3     FORMAT('where strttime >= "',A,'" and stoptime <= "',A,'"')
        ENDIF
 
C RETRIEVE BY REV

       ELSE IF (OPT .EQ. 3) THEN

         IF ((MASK .EQ. 'ASF') .OR. (MASK .EQ. 'MCM')) THEN
           WRITE(WHERE_CLAUSE, 4) SAT,SEN,REV,MASK
    4      FORMAT('where sat = "',A,'" and sensor = "',A,'" ',
     ?       ' and rev = ',I,' and station_id = "',A,'"')
         ELSE
           WRITE(WHERE_CLAUSE, 5) SAT,SEN,REV
    5      FORMAT('where sat = "',A,'" and sensor = "',A,'" ',
     ?       ' and rev = ',I)
         ENDIF
 
       END IF

       ERRNO = 0

       WHERE_CLAUSE = WHERE_CLAUSE(1:LASTC(WHERE_CLAUSE))//char(0)
       PRINT *, WHERE_CLAUSE

       llistptr = db_get_records(%VAL(MAPR_DBPROC),'dtk'//char(0),
     ?    WHERE_CLAUSE, char(0),DTK_COLUMNS,%VAL(ALL_COLS))
       if ( llistptr .EQ. 0 ) THEN
           CALL DISMSG('Error retrieving Data-take record.')
           DTKN = 0
           GO TO 9999
       ENDIF

       call get_no_elements(llist,NRECS)

       IF (NRECS .EQ. 0) THEN
           CALL DISMSG('Error : No Data-take records found.')
           CALL db_ftn_free_llist(llist)
           DTKN = 0
           GO TO 9999
       END IF

       P2_DATA_RECORD = db_ftn_first_record(llist, ptrptr)
        
       DO 200 WHILE (P2_DATA_RECORD .NE. 0 .AND. 
     1   DTKN .LT. 1000)

         DTKN = DTKN + 1

         call get_dtk_rec(%VAL(P2_DATA_RECORD),
     ?   DSATS(DTKN),DSENS(DTKN),DREVS(DTKN),DIDS(DTKN),
     ?   nrlat,nrlon,farlat,farlon,starttime,stoptime,ascdsc)

         P2_DATA_RECORD = db_ftn_next_record(llist, ptrptr)

 200   CONTINUE

       IF (DTKN .EQ. 1000) THEN
         WRITE(*,*) 'CAN NOT DISPLAY MORE THAN 1000 DATATAKE OVERLAYS.'
       ENDIF

       CALL db_ftn_free_llist(llist)

C RESET NRECS TO THE MAXIMUM ALLOWABLE DTK OVERLAYS
       NRECS = DTKN

 9999  CONTINUE
       RETURN
       END


C-----------------------------------------------------------------------
C
C SUBROUTINE DIS_DTK
C                                                   
C PURPOSE
C	DISPLAY THE DATATAKES FOUND IN GET_DTK
C
C INPUT
C	WSID  		WORKSTATION ID NUMBER
C	PROJN		PROJECTION TYPE
C	OBSLON		CENTER LONGITUDE (DEG)
C	OBSLAT		CENTER LATITUDE (DEG)
C	STMNX,STMXX	WINDOW MIN/MAX X
C	STMNY,STMXY	WINDOW MIN/MAX Y
C	DSATS(),DSENS()	SAT/SEN ARRAYS OF TARGET DATATAKES
C	DREVS(),DIDS()	REV/ID ARRAYS FOR TARGET DATATAKES
C	DTKN		NUMBER OF DATATAKES
C	SEGNAM		NAME OF GKS SEGMENT TO CREATE
C
C OUTPUT
C	NSEG  		NUMBER OF SEGMENTS CREATED
C	CRSEGN		SEGMENT STATUS ARRAY
C	CRSEGT		SEGMENT NAME ARRAY
C
C INTERNAL
C	IOS		I/O ERROR STATUS
C	ERRNO,RCOUNT	ERROR NUMBER AND ROW COUNTER
C	ERRFLAG		ERROR FLAG
C	STRTJD,STOPJD	START/STOP ET TIME
C	ONEMIN		ONE MINUTE OF ET
C	XDATE		TARGET ET TIME
C	NRLAT(),NRLON()	NEAR LAT/LON OF DATATAKE
C	FRLAT(),FRLON() FAR LAT/LON OF  "
C	CNRLAT(),CNRLON()	NEAR LAT/LON OF CVRG
C	CFRLAT(),CFRLON()	FAR LAT/LON  "  "
C	SX,SY		X/Y OF POINT
C	SW1(),SW2()	SWATH POINTS
C	OLDSW(),SWT()	TEMP SWATH POINTS
C	TEXX,TEXY	TEXT X/Y POINT
C	I,J		COUNTERS
C	LAST		FLAG INDICATING LAST POINTS
C	TREV,TID	TEMP REV AND ID
C	RLEN,ILEN,JLEN	STRING LENGTHS
C	TSTRT,TSTOPT	START AND STOP TIME - ASF FORMAT
C	TSAT,TSEN	TEMP SAT/SENSOR 
C	ASCDSC		ASCENDING/DESCENDING FLAG
C	XYEAR,XTIME	TARGET YEAR AND TIME
C	STRTY,STRTD	START YEAR/TIME
C	STOPY,STOPD	STOP  "
C	REVTXT,IDTXT	REV AND ID TEXT
C	STXT		TEXT
C	*HID		FLAGS INDICATING IF POINT IS HIDDEN OR NOT
C	BRNCUT		FLAG INDICATING IF LINE IS CUT AT BORDER
C
C WRITTEN BY CRAIG K. FUJIMOTO
C
C-----------------------------------------------------------------------
       SUBROUTINE DIS_DTK (WSID,PROJN,
     1               OBSLAT,OBSLON,
     2               STMNX,STMXX,STMNY,STMXY,
     3               DSATS,DSENS,DREVS,DIDS,DTKN,
     4               NSEG,CRSEGN,CRSEGT,SEGNAM)

       IMPLICIT NONE

c--port##     DECLARE
                               
C INPUT

c--port       INCLUDE 'SYS$LIBRARY:GKSDEFS.BND'
       INCLUDE 'GKS_ROOT:include/fgksenum.inc'
       INCLUDE 'mapr_db_extern.inc'
       INCLUDE 'mapper_port.inc'
       INCLUDE 'APS_ROOT:include/local/f77_db_cvrg.inc'
       INCLUDE 'APS_ROOT:include/local/f77_db_dtk.inc'

       INTEGER WSID,PROJN

       REAL OBSLAT,OBSLON
       REAL STMNX,STMXX,STMNY,STMXY

       CHARACTER*(*) DSATS(*),DSENS(*),SEGNAM

       INTEGER DREVS(*),DIDS(*),DTKN

C OUTPUT
       INTEGER NSEG,CRSEGN(*)

       CHARACTER*(*) CRSEGT(*)

C INTERNAL                      

       INTEGER RCOUNT,ERRFLAG
 
       DOUBLE PRECISION STRTET,STOPET,ONEMIN,XDATE
  
       REAL NRLAT(2),NRLON(2),FRLAT(2),FRLON(2)
       REAL CNRLAT(100),CNRLON(100),CFRLAT(100),CFRLON(100)

       REAL
     1      SW1(2,2),SW2(2,2),
     2      OLDSW(2,2),SWT(2,2),
     3      TEXX,TEXY

       INTEGER I,J,LAST
       INTEGER TREV,TID
       INTEGER RLEN,ILEN
       INTEGER IOS
                  
       CHARACTER*21 TSTRTT,TSTOPT
       CHARACTER*3 TSEN
       CHARACTER*2 TSAT                            
       CHARACTER*1 ASCDSC

       CHARACTER*11 REVTXT,IDTXT
       CHARACTER*15 STXT

       LOGICAL SW1HID(2),SW2HID(2)
       LOGICAL SWTHID(2),OLDHID(2)
       LOGICAL BRNCUT,TEXHID

C FUNCTIONS

       integer SLEN
       integer NRECS
       integer LASTC
	   integer   LOCAL_DUMMY_VALUEHOLDER
       real sublat,sublon

       REAL  GET_DEF_CH
       INCLUDE 'APS_HOME:include/local/timeconv.inc'

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
       CALL GSTXCI (8)
C POLYLINE COLOR INDEX
       CALL GSPLCI (8)

C CREATE THE SEGMENT

       WRITE(6,100) SEGNAM
  100  FORMAT(/,1X,'Creating overlay ',A<SLEN(SEGNAM)>,'...',$)

       CALL CRTSEG (WSID,NSEG,SEGNAM,CRSEGN,CRSEGT)

C LOOP THROUGH EACH OF THE DTK RECORDS

       DO 1000 I = 1,DTKN

         TSAT = DSATS(I)
         TSEN = DSENS(I)
         TREV = DREVS(I)             
         TID  = DIDS(I)

         CALL DISMSGW ('Retrieving Data-take...')

C RETRIEVE THE RECORD FROM THE DTK RELATION

c--port##       RETRIEVE(TSTRTT = DTK.#STRTTIME, TSTOPT = DTK.#STOPTIME,
c##                NRLAT(1) = DTK.#NRLAT1, NRLON(1) = DTK.#NRLON1,
c##                FRLAT(1) = DTK.#FARLAT1,FRLON(1) = DTK.#FARLON1,
c##                NRLAT(2) = DTK.#NRLAT2, NRLON(2) = DTK.#NRLON2,
c##                FRLAT(2) = DTK.#FARLAT2,FRLON(2) = DTK.#FARLON2,
c##                ASCDSC = DTK.#ASCDSC)
c##         WHERE DTK.#SAT = TSAT AND DTK.#SENSOR = TSEN
c##           AND DTK.#REV = TREV AND DTK.#DTKID  = TID
c##       {                          
c##       }

        WRITE(WHERE_CLAUSE, 4) TSAT,TSEN,TREV,TID
    4   FORMAT('where sat = "',A,'" and sensor = "',A,'" ',
     ?  ' and rev = ',I,' and dtkid = ',I)
        WHERE_CLAUSE = WHERE_CLAUSE(1:LASTC(WHERE_CLAUSE))//char(0)


        llistptr = db_get_records(%VAL(MAPR_DBPROC), 'dtk'//char(0),
     ?    WHERE_CLAUSE, char(0),DTK_COLUMNS,%VAL(ALL_COLS))
        if ( llistptr .eq. 0 ) THEN
            CALL DISMSG('Error retrieving Data-take record.')
            GO TO 9999
        ENDIF

        call get_no_elements(llist,NRECS)
 
        IF (NRECS .EQ. 0) THEN
            CALL DISMSG('No Data-take record found.')
            CALL db_ftn_free_llist(llist)
            GO TO 9999
        ENDIF

        P2_DATA_RECORD = db_ftn_first_record(llist, ptrptr)

        call get_dtk_rec( %VAL(P2_DATA_RECORD),
     ?   TSAT,TSEN,TREV,TID,
     ?   NRLAT,NRLON,FRLAT,FRLON,TSTRTT,TSTOPT,ASCDSC)

        CALL db_ftn_free_llist(llist)

C VALIDATE THE DTK DATA
         IF (NRLAT(1) .LT. -90.0  .OR. NRLAT(1) .GT. 90.0  .OR.
     1       NRLON(1) .LT. -180.0 .OR. NRLON(1) .GT. 180.0 .OR.
     2       FRLAT(1) .LT. -90.0  .OR. FRLAT(1) .GT. 90.0  .OR.
     3       FRLON(1) .LT. -180.0 .OR. FRLON(1) .GT. 180.0 .OR.
     4       NRLAT(2) .LT. -90.0  .OR. NRLAT(2) .GT. 90.0  .OR.
     5       NRLON(2) .LT. -180.0 .OR. NRLON(2) .GT. 180.0 .OR.
     6       FRLAT(2) .LT. -90.0  .OR. FRLAT(2) .GT. 90.0  .OR.
     7       FRLON(2) .LT. -180.0 .OR. FRLON(2) .GT. 180.0) THEN

           CALL DISMSG('Error: Invalid Data-take record found.')
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

C CHECK IF THE DTK STRT AND STOP TIMES ARE MORE THAN A MINUTE APART
                        
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

           CALL DISMSGW('Retrieving Coverage records...')
                                
           ERRFLAG = 0

        WRITE(WHERE_CLAUSE, 6) TSAT,TSEN,TREV,STRTET,STOPET
    6   FORMAT('where sat = "',A,'" and sensor = "',A,'" ',
     ?  'and rev = ',I,' and mjdate >= ',F,
     ?  ' and mjdate <= ',F)

       WHERE_CLAUSE = WHERE_CLAUSE(1:LASTC(WHERE_CLAUSE))//char(0)
       PRINT *, WHERE_CLAUSE

       llistptr = db_get_records(%VAL(MAPR_DBPROC), 'cvrg'//char(0),
     ?    WHERE_CLAUSE,'mjdate'//char(0),CVRG_COLUMNS,%VAL(ALL_COLS))
       if ( llistptr .eq. 0 ) THEN
           CALL DISMSG('Error retrieving cvrg records.')
           GO TO 2200
       ENDIF

       call get_no_elements(llist, RCOUNT)

       IF (RCOUNT .EQ. 0) THEN
           CALL DISMSG('Error : No Coverage records found.')
           CALL db_ftn_free_llist(llist)
           GO TO 2200
       END IF
 
       P2_DATA_RECORD = db_ftn_first_record(llist, ptrptr)

       J = 0
       DO 2000 WHILE (P2_DATA_RECORD .NE. 0)

             J = J + 1

             call get_cvrg_rec(%VAL(P2_DATA_RECORD),
     ?       TSAT,TSEN,TREV,XDATE,sublat,sublon,
     ?       CNRLAT(J),CNRLON(J),CFRLAT(J),CFRLON(J),ascdsc)


C VALIDATE THE COVERAGE RECORD DATA

             IF (CNRLAT(J) .LT. -90.0  .OR. CNRLAT(J) .GT. 90.0  .OR.
     1           CNRLON(J) .LT. -180.0 .OR. CNRLON(J) .GT. 180.0 .OR.
     2           CFRLAT(J) .LT. -90.0  .OR. CFRLAT(J) .GT. 90.0  .OR.
     3           CFRLON(J) .LT. -180.0 .OR. CFRLON(J) .GT. 180.0) THEN
     
               ERRFLAG = 1
c--port##             ENDLOOP
c              break from the loop
               GO TO 2100

             END IF
                 
C CONVERT THE NEAR POINT TO X AND Y

             CALL LTRANS (PROJN,OBSLAT,OBSLON,
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

c##         }

         P2_DATA_RECORD = db_ftn_next_record(llist, ptrptr)

 2000    CONTINUE

 2100    CONTINUE

         CALL db_ftn_free_llist(llist)

         IF (ERRFLAG .NE. 0) THEN

           CALL DISMSG('Error: Invalid Coverage record found.')

         END IF

 2200    CONTINUE


         IF (ERRFLAG .NE. 0 .OR. RCOUNT .EQ. 0) THEN
C--        DISPLAY THE SEGMENT WITHOUT CVRG RECORDS
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

C FORM THE DATATAKE TEXT

       IF (.NOT. TEXHID) THEN

         ENCODE(5,'(I5)',REVTXT) DREVS(I)
         ENCODE(2,'(I2)',IDTXT) DIDS(I)

         CALL TRIM(REVTXT,5,RLEN)
         CALL TRIM(IDTXT,2,ILEN)

         STXT = ' ' // DSATS(I)(1:2) // '/' // DSENS(I)(1:1) // '/' //
     1          REVTXT(1:RLEN) // '.' // IDTXT(1:ILEN) // ASCDSC(1:1)

C DISPLAY THE TEXT

         CALL GTXS(TEXX,TEXY,LEN(STXT),STXT)

       END IF

C NEXT DTK RECORD

 1000  CONTINUE

 9000  CONTINUE

C CLOSE THE SEGMENT

       CALL CLSSEG
              
 9999  CONTINUE
       RETURN
       END 
