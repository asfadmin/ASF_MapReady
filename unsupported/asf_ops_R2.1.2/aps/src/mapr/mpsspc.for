C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:  mpsspc.for
C--
C--  Description:   
C-- 
C--  Notes:
C--
C-- ==========================================================================

C-----------------------------------------------------------------------
C
C SUBROUTINE MPSSPC
C
C PURPOSE
C   CREATE A SPECIFIC SITE COVERAGE OVERLAY
C
C $Logfile:   QDISK:[BLD.MPS.MAPR.SRC]MPSSPC.QFV  $
C
C VARIABLES
C
C INPUT
C   WSID        WORKSTATION ID NUMBER
C   PROJN       PROJECTION TYPE
C   OBSLON,OBSLON   CENTER LAT/LON
C   MINX,MAXX   MIN/MAX WINDOW WIDTH
C   MINY,MAXY   MIN/MAX WINDOW HEIGHT
C
C OUTPUT:
C   NSEG        OVERLAY COUNT
C   CRSEGN()    OVERLAY STATUS ARRAY
C   CRSEGT()    OVERLAY NAME ARRAY
C   CRDARID()   OVERLAY DARIDS
C
C INTERNAL:
C   OPT     OPTION FLAG
C   ROPT        REV OPTION FLAG
C   DARID       DARID OF SSC TO RETRIEVE
C   SITENAME    SITE  "  "   "  "
C   SAT     SAT   "  "   "  "
C   SEN     SEN   "  "   "  "
C   IOS     RETURN STATUS
C
C WRITTEN BY CRAIG K. FUJIMOTO - SEP 1989
C
C MODIFICATIONS
C $Date$ $Revision$ $Author$
C
C 7/8/94  Nadia Adhami -port to UNIX/C (INGRIS -> SYBASE)
C 7/8/94  Nadia Adhami -port to UNIX/C (deleted query from OPERATION database)
C         Nadia        used 21 instead
C 7/26/94 Nadia Adhami -port to UNIX/C replaced UNIQ18 by getpid_str() 
C 10/4/94 Nadia Adhami -port to UNIX/C TIME2REV() add dbproc parameter
C 9/8/95  Nadia Adhami PR#406 use DEF_START_TIME for sps query
C
C-----------------------------------------------------------------------

       SUBROUTINE MPSSPC (WSID,PROJN,
     1                    OBSLAT,OBSLON,
     2                    STMNX,STMXX,STMNY,STMXY,
     3                    NSEG,CRSEGN,CRSEGT,CRDARID)

      character*100 SccsFileID
     -/'@(#)mpsspc.for	5.1 98/01/08 APS/ASF\0'/

       IMPLICIT NONE

C INPUT:
       CHARACTER*(*) CRSEGT(*) 

       INTEGER PROJN,WSID,NSEG,CRSEGN(*),CRDARID(*)

       REAL OBSLON,OBSLAT
       REAL STMNX,STMXX,STMNY,STMXY

C INTERNAL:

       CHARACTER*32 SITENAME
       CHARACTER*3 SEN
       CHARACTER*2 SAT
       CHARACTER*21 STIME(2)

       INTEGER OPT,ROPT,IOS
       INTEGER DARID
       INTEGER SREV(2)

       DOUBLE PRECISION SETIME(2)
C-----------------------------------------------------------------------


C SELECT BY DARID OR SITENAME
  100 CONTINUE
      DARID = 0

       CALL SPC_MENU1(OPT)

       IF (OPT .EQ. 1) THEN

C ASK FOR DARID
         CALL ASK_DARID(DARID)

       ELSE IF (OPT. EQ. 2) THEN

C ASK FOR SITE ID           
         CALL ASK_SITENAME(SITENAME)

       ELSE
C        OPT = NEITHER 1 NOR 2:  ASSUME 0.
         GO TO 9999
       END IF


C ASK FOR SATELLITE
       CALL ASK_SAT(SAT)                          

C ASK FOR SENSOR
       CALL ASK_SENSOR(SAT , SEN)
                                      
C BY TIME OR REVS?
       CALL SPC_MENU2(ROPT)

C GET STRTTIME AND STOPTIME

       IF (ROPT .EQ. 1) THEN

 1000    CONTINUE

         CALL ASK_TIME(STIME(1),' START TIME (yyyy:ddd:hh:mm) : ')

         CALL ASK_TIME(STIME(2),' STOP TIME (yyyy:ddd:hh:mm) : ')

         IF (STIME(1) .GE. STIME(2)) THEN

           CALL DISMSG('Error : Start time must be before stop time.')
           GO TO 1000

         END IF

C GET STRT AND STOP REV NUMBERS

       ELSE IF (ROPT .EQ. 2) THEN
             
 2000    CONTINUE
 
         CALL ASK_REV(SREV(1),' START REV [1-99999] : ')

         CALL ASK_REV(SREV(2),' STOP REV [1-99999] : ')

         IF (SREV(1) .GT. SREV(2)) THEN

           CALL DISMSG('Error : Start rev must be <= stop rev.')
           GO TO 2000

         END IF

      ELSE IF (ROPT .EQ. 0) THEN
C        USER WANTS TO QUIT TO PREVIOUS MENU.
         GO TO 100
 
       ELSE                              
C        ROPT EQUAL NEITHER 0 NOR 1 NOR 2:
         WRITE(*,*)'Error:  select either 0, 1, or 2'
         GO TO 9999
       END IF

C RETRIEVE THE REV NUMBERS
       CALL SSCV_REVS(ROPT,SAT,STIME,SREV,SETIME,IOS)
       IF (IOS .NE. 0) GO TO 9999

C DISPLAY THE SSCV RECORDS
       CALL DIS_SPC (WSID,PROJN,
     1               OBSLAT,OBSLON,
     2               STMNX,STMXX,STMNY,STMXY,
     3               OPT,DARID,SITENAME,SAT,SEN,
     4               SETIME,SREV,
     5               NSEG,CRSEGN,CRSEGT,CRDARID)

 9999  CONTINUE
       RETURN
       END


C-----------------------------------------------------------------------
C
C SUBROUTINE GET_SPCREVS
C        
C PURPOSE
C   FIND THE REV NUMBERS OF THE SSC RECORDS OF INTEREST
C
C VARIABLES             
C
C INPUT
C   ROPT        SEARCH OPTION FLAG
C   SAT     SATELLITE ID
C   STIME()     ASF FORMAT START/STOP TIME
C
C OUTPUT
C   SREV()      START/STOP REV
C   SETIME()    START/STOP EPHEMERIS TIME
C   IOS     STATUS
C
C INTERNAL
C   REV1,REV2   TEMP REV HOLDERS
C
C WRITTEN BY CRAIG K. FUJIMOTO - SEP 89
C-----------------------------------------------------------------------
      SUBROUTINE SSCV_REVS(ROPT,SAT,STIME,SREV,SETIME,IOS)

      IMPLICIT NONE

      INCLUDE 'mapr_db_extern.inc'
      INCLUDE 'APS_ROOT:include/local/f77_db_sscvrg.inc'
      INCLUDE 'mapper_port.inc'

C INPUT:
      INTEGER ROPT
      CHARACTER*(*) SAT
      CHARACTER*21 STIME(2)

C OUTPUT
      INTEGER SREV(2),IOS
      DOUBLE PRECISION SETIME(2)

C INTERNAL:
      INTEGER REV1,REV2
      INTEGER   LOCAL_DUMMY_VALUEHOLDER

      INCLUDE 'APS_HOME:include/local/timeconv.inc'

C-----------------------------------------------------------------------
C FOLLOWING ASSIGNMENT REMOVES WARNING MESSAGE FOR UNUSED VARIABLES   
      INTEGER*4 TEMP
      DATA TEMP /10/
       LLISTPTR =  LOC (TEMP) 
       LOCAL_DUMMY_VALUEHOLDER = LLIST
       PTRPTR = LOC (TEMP)
       LOCAL_DUMMY_VALUEHOLDER = PTR
       P2_DATA_RECORD = LOC (TEMP)
       DATA WHERE_CLAUSE /'ABC'/
C-----------------------------------------------------------------------

      IF (ROPT .EQ. 1) THEN

        IOS = tc_asf2et(STIME(1) // char(0) ,SETIME(1))
        IF (IOS .NE. 1) THEN
          CALL DISMSG('Error converting ASF time.')
          GO TO 9999
        END IF

        IOS = tc_asf2et(STIME(2) // char(0) ,SETIME(2))
        IF (IOS .NE. 1) THEN
          CALL DISMSG('Error converting ASF time.')
          GO TO 9999
        END IF

        CALL TIME2REV( MAPR_DBPROC,
     ?      SAT,SETIME(1),STIME(1),REV1,REV2,IOS)
        IF (IOS .EQ. -1) THEN
          CALL DISMSG('Error finding rev number.')
          GO TO 9999
        ELSE IF (IOS .EQ. 3 .OR. IOS .EQ. 4) THEN
          CALL DISMSG('Error: Coverage records not found.')
          GO TO 9999
        ELSE IF (IOS .EQ. 1) THEN
          SREV(1) = REV2
        ELSE
          SREV(1) = REV1
        END IF

        CALL TIME2REV( MAPR_DBPROC,
     ?      SAT,SETIME(2),STIME(2),REV1,REV2,IOS)
        IF (IOS .EQ. -1) THEN
          CALL DISMSG('Error finding rev number.')
          GO TO 9999
        ELSE IF (IOS .EQ. 1 .OR. IOS .EQ. 4) THEN
          CALL DISMSG('Error: Coverage records not found.')
          GO TO 9999
        ELSE IF (IOS .EQ. 3) THEN
          SREV(1) = REV1
        ELSE
          SREV(2) = REV2
        END IF


C---  this else is for:     IF (ROPT .EQ. 1) THEN
      ELSE

C       PR # 406 use DEF_START_TIME instead
C       STIME(1) = '1978:001:00:00:00.000'

        STIME(1) = DEF_START_TIME
        STIME(2) = '2010:001:00:00:00.000'

        IOS = tc_asf2et(STIME(1) // char(0) ,SETIME(1))
        IF (IOS .NE. 1) THEN
          CALL DISMSG('Error converting ASF time.')
          GO TO 9999
        END IF

        IOS = tc_asf2et(STIME(2) // char(0) ,SETIME(2))
        IF (IOS .NE. 1) THEN
          CALL DISMSG('Error converting ASF time.')
          GO TO 9999
        END IF

      END IF

      IOS = 0

 9999 CONTINUE
      RETURN
      END

C-----------------------------------------------------------------------
       SUBROUTINE DIS_SPC (WSID,PROJN,
     1               OBSLAT,OBSLON,
     2               MINX,MAXX,MINY,MAXY,
     3               OPT,DARID,SITENAME,SAT,SEN,
     4               SETIME,SREV,
     5               NSEG,CRSEGN,CRSEGT,CRDARID)

       IMPLICIT NONE


C INPUT

       INCLUDE 'GKS_ROOT:include/fgksenum.inc'
       INCLUDE 'mapr_db_extern.inc'
       INCLUDE 'mapper_port.inc'
       INCLUDE 'APS_ROOT:include/local/f77_db_sscvrg.inc'
       INCLUDE 'APS_ROOT:include/local/f77_db_cvrg.inc'

       INTEGER WSID,PROJN

       REAL OBSLAT,OBSLON
       REAL MINX,MAXX,MINY,MAXY

       INTEGER OPT,DARID, MAXOVR
       INTEGER SREV(2), REV, IUNIT, IOS, NREC, NRECS
       DOUBLE PRECISION SETIME(2)
       CHARACTER*(*) SITENAME,SAT,SEN
       CHARACTER*18 CHAR18
       CHARACTER*50 FILENAME


C OUTPUT
       INTEGER NSEG,CRSEGN(*),CRDARID(*)
       CHARACTER*(*) CRSEGT(*)

C INTERNAL                      

       DOUBLE PRECISION STRTET,STOPET
       DOUBLE PRECISION ONEMIN,XDATE         

       REAL TEXX,TEXY
       REAL SW1(2,2),SW2(2,2),OLDSW(2,2),SWT(2,2)
       REAL NRLAT(2),NRLON(2),FRLAT(2),FRLON(2)
       REAL CNRLAT(100),CNRLON(100),CFRLAT(100),CFRLON(100)

       INTEGER I,J,JJ,LAST,REVLEN
       INTEGER ERRFLAG,SCNT
                            
       CHARACTER*20 SEGNAM
       CHARACTER*1 ASCDSC, A_D(2)
       CHARACTER*11 REVTXT
       CHARACTER*22 VUTXT
     
       LOGICAL SW1HID(2),SW2HID(2)
       LOGICAL SWTHID(2),OLDHID(2)
       LOGICAL BRNCUT,TEXHID

C FUNCTIONS
       integer idummy
       integer NRECS
       character*30 cdummy
       real rdummy
       integer LASTC
       integer   LOCAL_DUMMY_VALUEHOLDER


       EXTERNAL  getpid_str !$pragma c(getpid_str)
       REAL  GET_DEF_CH

C-----------------------------------------------------------------------
C FOLLOWING ASSIGNMENT REMOVES WARNING MESSAGE FOR UNUSED VARIABLE PTR
      INTEGER*4 TEMP
      DATA TEMP /10/
       PTRPTR = LOC (TEMP)
       LOCAL_DUMMY_VALUEHOLDER = PTR

       ONEMIN = 1.0 / 24.0 / 60.0
       A_D(1) = 'A'
       A_D(2) = 'D'

C GRAPHICS PARAMETERS

C TEXT HEIGHT
       CALL GSCHH (GET_DEF_CH())
C TEXT ALIGNMENT
       CALL GSTXAL (GALEFT,GAHALF)

C TEXT COLOR INDEX
       CALL GSTXCI (6)
C POLYLINE COLOR INDEX
       CALL GSPLCI (6)

C read and write to scratch file OF THE SPECSITE CVRG RECORDS
       CALL DISMSGW ('Searching for Spec. Site Coverage records...')
C---    RETRIEVE THE MAXIMUM ALLOWED OVERLAYS FROM THE OPERATION RELATION.
c--port##   RETRIEVE(MAXOVR = OPERATION.#MAPR_MAX_SSCV ) 
c##         { }

C MAXOVR has to correspond to the size of the internal arrays
C CRSEGN(200),CRDARID(200),CRSEGT(200)

        MAXOVR = 200
 
C---    NOW GENERATE A UNIQUE FILE NAME FOR A SCRATCH FILE.  
c--port      CALL UNIQ18(CHAR18)
      CALL getpid_str(CHAR18)

      FILENAME = '/tmp/APS_SCRATCH_SSCV' // CHAR18 // '.SCRATCH'
      IUNIT = 77
      OPEN(IUNIT,FILE=FILENAME,STATUS='SCRATCH',ERR=8001,
     ?  IOSTAT=IOS, FORM='UNFORMATTED',ACCESS='SEQUENTIAL')

C---    CHECK ON DARID OR SITENAME RETRIEVE.  
C---    START DARID RETRIEVE LOOP ON SSCVRG 
      SCNT = 0

      IF (OPT .EQ. 1) THEN
C RETRIEVE THE RECORD FROM THE SPEC SITE CVRG REL BY DARID

c--port##         RETRIEVE( REV = SSCVRG.#REV,
c##     STRTET = SSCVRG.#STRTET, STOPET = SSCVRG.#STOPET,
c##                  NRLAT(1) = SSCVRG.#NRLAT1, NRLON(1) = SSCVRG.#NRLON1,
c##                  FRLAT(1) = SSCVRG.#FARLAT1,FRLON(1) = SSCVRG.#FARLON1,
c##                  NRLAT(2) = SSCVRG.#NRLAT2, NRLON(2) = SSCVRG.#NRLON2,
c##                  FRLAT(2) = SSCVRG.#FARLAT2,FRLON(2) = SSCVRG.#FARLON2,
c##                  ASCDSC   = SSCVRG.#ASCDSC)
c##           WHERE SSCVRG.#DARID  = DARID AND SSCVRG.#SAT = SAT
c##             AND SSCVRG.#SENSOR = SEN 
c##             AND SSCVRG.#REV >= SREV(1) AND SSCVRG.#REV <= SREV(2)
c##             AND SSCVRG.#STRTET >= SETIME(1)
c##             AND SSCVRG.#STOPET <= SETIME(2)
c##         {                          

        WRITE(WHERE_CLAUSE, 1) DARID,SAT,SEN,
     ?   SREV(1),SREV(2),SETIME(1),SETIME(2)
    1   FORMAT('where darid = ',I,
     ?   ' and sat = "',A,'" and sensor = "',A,'" ',
     ?   'and rev >= ',I,' and rev <= ',I,
     ?   ' and strtet >= ',F,' and stopet <= ',F)
        WHERE_CLAUSE = WHERE_CLAUSE(1:LASTC(WHERE_CLAUSE))//char(0)

        llistptr = db_get_records(%VAL(MAPR_DBPROC), 'sscvrg'//char(0),
     ?   WHERE_CLAUSE, char(0),SSCVRG_COLUMNS,%VAL(ALL_COLS))
        IF ( llistptr .EQ. 0 ) THEN
            GOTO 3301
        ENDIF 

        call get_no_elements(llist,NRECS)
        IF (NRECS .EQ. 0) THEN
              GOTO 3300
        ENDIF

        P2_DATA_RECORD = db_ftn_first_record(llist, ptrptr)
 
        DO 3000 WHILE (P2_DATA_RECORD .NE. 0)

            call get_sscvrg_rec(%VAL(P2_DATA_RECORD),
     ?      cdummy,cdummy,REV, 
     ?      cdummy,cdummy,STRTET, STOPET,rdummy,rdummy,
     ?      NRLAT, NRLON, FRLAT, FRLON,ASCDSC)

            SCNT = SCNT + 1

            IF (SCNT .GT. MAXOVR) THEN
       CALL DISMSG('CAN NOT DISPLAY MORE THAN 200 SSCVRG OVERLAYS.')
                GO TO 3300
            END IF

C---        WRITE THE RECORD TO THE SCRATCH FILE.  
            WRITE ( IUNIT, ERR=8002, IOSTAT = IOS )
     ?        REV, STRTET, STOPET, NRLAT, NRLON, FRLAT, FRLON, ASCDSC

c##        }

            P2_DATA_RECORD = db_ftn_next_record(llist, ptrptr)

 3000   CONTINUE

c       3300 : exit the loop
 3300   CONTINUE

        CALL db_ftn_free_llist(llist)

 3301   CONTINUE
C-  this ELSE is for:    IF (OPT .EQ. 1) THEN
      ELSE
C---       START SITENAME RETRIEVE LOOP ON SSCVRG 
C          RETRIEVE THE RECORD FROM THE SSCVRG RELATION BASED ON SITENAME

c--port##         RETRIEVE( REV = SSCVRG.#REV,
c##                  STRTET = SSCVRG.#STRTET, STOPET = SSCVRG.#STOPET,
c##                  NRLAT(1) = SSCVRG.#NRLAT1, NRLON(1) = SSCVRG.#NRLON1,
c##                  FRLAT(1) = SSCVRG.#FARLAT1,FRLON(1) = SSCVRG.#FARLON1,
c##                  NRLAT(2) = SSCVRG.#NRLAT2, NRLON(2) = SSCVRG.#NRLON2,
c##                  FRLAT(2) = SSCVRG.#FARLAT2,FRLON(2) = SSCVRG.#FARLON2,
c##                  ASCDSC   = SSCVRG.#ASCDSC)
c##           WHERE SSCVRG.#SITENAME = SITENAME AND SSCVRG.#SAT = SAT
c##             AND SSCVRG.#SENSOR = SEN 
c##             AND SSCVRG.#REV >= SREV(1) AND SSCVRG.#REV <= SREV(2)
c##             AND SSCVRG.#STRTET >= SETIME(1)
c##             AND SSCVRG.#STOPET <= SETIME(2)
c##         {                          

        WRITE(WHERE_CLAUSE, 2) SITENAME,SAT,SEN,
     ?    SREV(1),SREV(2),SETIME(1),SETIME(2)
    2   FORMAT('where sitename = "',A,
     ?   '" and sat = "',A,'" and sensor = "',A,'" ',
     ?   ' and rev >= ',I,' and rev <= ',I,
     ?   ' and strtet >= ',F,' and stopet <= ',F)
        WHERE_CLAUSE = WHERE_CLAUSE(1:LASTC(WHERE_CLAUSE))//char(0)
 
        llistptr = db_get_records(%VAL(MAPR_DBPROC), 'sscvrg'//char(0),
     ?   WHERE_CLAUSE, char(0), SSCVRG_COLUMNS,%VAL(ALL_COLS))
        IF ( llistptr .EQ. 0) THEN
           CALL DISMSG('ERROR in query in sscvrg relation.  ')
           GOTO 4201
        ENDIF
 
        call get_no_elements(llist,NRECS)
        IF (NRECS .EQ. 0) THEN
          GOTO 4200
        ENDIF
 
        P2_DATA_RECORD = db_ftn_first_record(llist, ptrptr)
 
        DO 4000 WHILE (P2_DATA_RECORD .NE. 0)
 
             call get_sscvrg_rec(%VAL(P2_DATA_RECORD),
     ?       cdummy,cdummy,REV, 
     ?       cdummy,cdummy,STRTET, STOPET,rdummy,rdummy,
     ?       NRLAT, NRLON, FRLAT, FRLON,ASCDSC)
 

             SCNT = SCNT + 1

             IF (SCNT .GT. MAXOVR) THEN
       CALL DISMSG('CAN NOT DISPLAY MORE THAN 200 SSCVRG OVERLAYS.')
               GO TO 4200
             END IF

C---         WRITE THE RECORD TO THE SCRATCH FILE.  
          WRITE ( IUNIT, ERR=8002, IOSTAT = IOS )
     ?        REV, STRTET, STOPET, NRLAT, NRLON, FRLAT, FRLON, ASCDSC
C---          END SITENAME RETRIEVE LOOP ON SSCVRG
c##    }
          P2_DATA_RECORD = db_ftn_next_record(llist, ptrptr)

 4000   CONTINUE

c       Exit the loop
 4200   CONTINUE

        CALL db_ftn_free_llist(llist)

 4201   CONTINUE

      END IF

C     Set NRECS to maximum number of overlays that can be displayed
      NRECS = SCNT

C---  ALL THE SSCVRG RECORDS WERE WRITTEN.  NOW READ THEM IN A LOOP.  
      ENDFILE IUNIT
C---  REWIND THE FILE 
      REWIND (IUNIT,ERR=8003,IOSTAT=IOS)
C #########################################################################
C---  START 1000 DO LOOP FOR EACH RECORD READ
      DO 1000 NREC = 1, NRECS
C---      READ THE SSCVRG RECORD FROM THE SCRATCH FILE.  
          READ ( IUNIT, ERR=8004, IOSTAT = IOS, END=1001 )
     ?     REV, STRTET, STOPET, NRLAT, NRLON, FRLAT, FRLON, ASCDSC

C---       SET I = THE REV NUMBER OF THE RECORD.  
          I = REV
C---       SET JJ ACCORDING TO ASCENDING/DESCENDING.  
          IF ( ASCDSC .EQ. 'A' ) THEN 
            JJ = 1
          ELSE 
            JJ = 2
          END IF
C VALIDATE THE SPECIFIC SITE CVRG RECORD
          IF (STRTET .GT. STOPET .OR.
     1       NRLAT(1) .LT. -90.0  .OR. NRLAT(1) .GT. 90.0  .OR.
     1       NRLON(1) .LT. -180.0 .OR. NRLON(1) .GT. 180.0 .OR.
     2       FRLAT(1) .LT. -90.0  .OR. FRLAT(1) .GT. 90.0  .OR.
     3       FRLON(1) .LT. -180.0 .OR. FRLON(1) .GT. 180.0 .OR.
     4       NRLAT(2) .LT. -90.0  .OR. NRLAT(2) .GT. 90.0  .OR.
     5       NRLON(2) .LT. -180.0 .OR. NRLON(2) .GT. 180.0 .OR.
     6       FRLAT(2) .LT. -90.0  .OR. FRLAT(2) .GT. 90.0  .OR.
     7       FRLON(2) .LT. -180.0 .OR. FRLON(2) .GT. 180.0) THEN
     
              CALL DISMSG(
     ?      'Error: SKIPPING invalid Spec Site CVRG record.')
              GO TO 1000
          END IF
             
C PUT TOGETHER THE TEXT TO VIEW ON THE SCREEN

          ENCODE(5,'(I5)',REVTXT) I
          CALL TRIM(REVTXT,5,REVLEN)

          VUTXT = ' ' // SAT(1:2) // '/' // SEN(1:3) // '/' //
     1           REVTXT(1:REVLEN) // ASCDSC(1:1)

C IF EACH SPC REC GOES INTO ITS OWN SEGMENT, OPEN SEG WITHIN LOOP

          SEGNAM = 'SSC' // VUTXT
   
          WRITE(6,130) SEGNAM
c  130    FORMAT(/,1X,'Creating overlay ',A<SLEN(SEGNAM)>,'...',$)
  130     FORMAT(/,1X,'Creating overlay ',A<LEN(SEGNAM)>,'...',$)

          CALL CRTSEG (WSID,NSEG,SEGNAM,CRSEGN,CRSEGT)

C ADD THE RECORD TO THE SEG DAR ARRAY IF SPECIFIED BY DARID
          IF (OPT. EQ. 1) THEN
             CALL ADDDAR(DARID,NSEG,CRDARID)
          END IF

C DRAW THE SPEC SITE RECORD

C CONVERT THE FIRST POINT TO X AND Y

          CALL LTRANS (PROJN,OBSLAT,OBSLON,
     1               NRLON(1),NRLAT(1),
     2               SW1(1,1),SW1(1,2),
     3               SW1HID(1),BRNCUT)

          CALL LTRANS (PROJN,OBSLAT,OBSLON,
     1                FRLON(1),FRLAT(1),
     2                SW1(2,1),SW1(2,2),
     3                SW1HID(2),BRNCUT)

C DETERMINE THE POSITION OF THE TEXT STRING

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

C CHECK IF THE SPC SITE CVRG RECORDS ARE MORE THAN A MINUTE APART
                        
          IF ((STOPET-STRTET) .LT. ONEMIN) THEN

C IF NOT MORE THAN 1 MINUTE APART, THEN DISPLAY THE SEGMENT
                              
             IF (.NOT. SW1HID(1) .OR. .NOT. SW1HID(2) .OR.
     1         .NOT. SW2HID(1) .OR. .NOT. SW2HID(2)) THEN

                LAST = 1

                CALL DIS_SWATH (PROJN,
     1                 SW1,SW2,
     2                 SW1HID,SW2HID,
     3                 MINX,MAXX,MINY,MAXY,
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
c##         WHERE CVRG.#SAT = SAT AND CVRG.#SENSOR = SEN
c##         AND   CVRG.#REV = I
c##         AND   CVRG.#MJDATE >= STRTET AND CVRG.#MJDATE <= STOPET
c##         SORT BY #XDATE
c##         {

C VERIFY THE COVERAGE RECORD DATA

             WRITE(WHERE_CLAUSE, 3) SAT,SEN,I,STRTET,STOPET
    3        FORMAT('where sat = "',A,'" and sensor = "',A,'" ',
     ?               ' and rev = ',I,' and mjdate >= ',F,
     ?               ' and mjdate <= ',F)
             WHERE_CLAUSE = WHERE_CLAUSE(1:LASTC(WHERE_CLAUSE))//char(0)
 
             llistptr = db_get_records(%VAL(MAPR_DBPROC), 
     ?                    'cvrg'//char(0),
     ?                    WHERE_CLAUSE, 'mjdate'//char(0),
     ?                    CVRG_COLUMNS,%VAL(ALL_COLS))
             IF (llistptr .EQ. 0) THEN
                 CALL DISMSG('ERROR in query in cvrg relation.  ')
                 GOTO 5200
             ENDIF
 
             call get_no_elements(llist,NRECS)
             IF (NRECS .EQ. 0) THEN
               GOTO 5100
             ENDIF
 
             P2_DATA_RECORD = db_ftn_first_record(llist, ptrptr)

             DO 5000 WHILE (P2_DATA_RECORD .NE. 0)
 
             call get_cvrg_rec(%VAL(P2_DATA_RECORD),
     ?       cdummy,cdummy,idummy,XDATE,rdummy,rdummy,
     ?       CNRLAT(J),CNRLON(J),CFRLAT(J),CFRLON(J),ASCDSC)
 


             IF (CNRLAT(J) .LT. -90.0  .OR. CNRLAT(J) .GT. 90.0  .OR.
     1           CNRLON(J) .LT. -180.0 .OR. CNRLON(J) .GT. 180.0 .OR.
     2           CFRLAT(J) .LT. -90.0  .OR. CFRLAT(J) .GT. 90.0  .OR.
     3           CFRLON(J) .LT. -180.0 .OR. CFRLON(J) .GT. 180.0) THEN

                ERRFLAG = 1
c##             ENDLOOP  
                GO TO 5200

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
     3                 MINX,MAXX,MINY,MAXY,
     4                 LAST)

               END IF

C IF NOT 1ST REC, DISPLAY PREVIOUS AND CURRENT CVRG REC

             ELSE

               IF (.NOT. OLDHID(1) .OR. .NOT. OLDHID(2) .OR.
     1             .NOT. SWTHID(1) .OR. .NOT. SWTHID(2)) THEN
                  
                 CALL DIS_SWATH (PROJN,
     1                 OLDSW,SWT,
     2                 OLDHID,SWTHID,
     3                 MINX,MAXX,MINY,MAXY,
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

             J = J + 1

             P2_DATA_RECORD = db_ftn_next_record(llist, ptrptr)

 5000        CONTINUE
 5100        CONTINUE

             CALL db_ftn_free_llist(llist)

 5200        CONTINUE

             IF (ERRFLAG .NE. 0 .OR. NRECS .EQ. 0) THEN
C--              DISPLAY THE SEGMENT WITHOUT CVRG RECORDS
                 SWT(1,1) = SW1(1,1)
                 SWT(1,2) = SW1(1,2)
                 SWT(2,1) = SW1(2,1)
                 SWT(2,2) = SW1(2,2)
                 SWTHID(1) = SW1HID(1)
                 SWTHID(2) = SW1HID(2)
             ENDIF

  900        CONTINUE
C DRAW THE SWATH FROM THE LAST CVRG REC TO THE SECOND SPC REC

             IF (.NOT. SWTHID(1) .OR. .NOT. SWTHID(2) .OR.
     1             .NOT. SW2HID(1) .OR. .NOT. SW2HID(2)) THEN

                    LAST = 1

                    CALL DIS_SWATH (PROJN,
     1           SWT,SW2,
     2           SWTHID,SW2HID,
     3           MINX,MAXX,MINY,MAXY,
     4           LAST)

             ENDIF

C            END IF MORE THAN 1 MINUTE APART, THEN GET 
C            COVERAGE BETWEEN THE 2 POINTS
          ENDIF
                 

C DISPLAY THE TEXT

          IF (.NOT. TEXHID) THEN
 
               CALL GTXS(TEXX,TEXY,LEN(VUTXT),VUTXT)

          END IF

C CLOSE OVERLAY GSEG
          CALL CLSSEG

C NEXT SSCV RECORD  END LOOP DO 1000
 1000 CONTINUE
 1001 CONTINUE
C---  CLOSE THE SCRATCH FILE
      CLOSE ( IUNIT, DISPOSE = 'DELETE' )

      IF (SCNT .EQ. 0) THEN 
         CALL DISMSG('No Spec. Site Coverage records found.')
      END IF
      GO TO 9999
C---    ERRRORS.  
 8001 CONTINUE
C---    ERROR ON OPENING SCRATCH FILE.  
      CALL DISMSG('ERROR ON OPENING SCRATCH FILE.  ')
      CALL DISMSG('SET PERMISSIONS ON /tmp TO rwed.  ')
      CALL DISMSG('ALSO CHECK DISK SPACE ON /tmp.  ')
      WRITE (*,*)' I/O RETURN CODE = ', IOS
      GO TO 9999
 8002 CONTINUE
C---    ERROR ON WRITING TO SCRATCH FILE.  
      CALL DISMSG('ERROR ON WRITING TO SCRATCH FILE IN MAPPER.  ')
      CALL DISMSG('CHECK DISK SPACE ON /tmp.  ')
      WRITE (*,*)' I/O RETURN CODE = ', IOS
      GO TO 9999
 8003 CONTINUE
C---    ERROR ON REWINDING SCRATCH FILE.  
      CALL DISMSG('ERROR ON REWINDING SCRATCH FILE IN MAPPER.  ')
      WRITE (*,*)' I/O RETURN CODE = ', IOS
      GO TO 9999
 8004 CONTINUE
C---    ERROR ON READING SCRATCH FILE.  
      CALL DISMSG('ERROR ON READING SCRATCH FILE IN MAPPER.  ')
      WRITE (*,*)' I/O RETURN CODE = ', IOS
 9999  CONTINUE
       RETURN
       END 

C-----------------------------------------------------------------------
C
C SUBROUTINE ADDDAR
C
C PURPOSE
C   ADD THE DARID TO THE CRDARID ARRAY
C
C INPUT
C   DARID       <
C
C OUTPUT
C   NSEG        GKS SEG COUNTER
C   CRDARID     GKS SEG DARID ARRAY
C
C WRITTEN BY CRAIG K. FUJIMOTO - SEP 89
C-----------------------------------------------------------------------

       SUBROUTINE ADDDAR(DARID,NSEG,CRDARID)

       IMPLICIT NONE

C INPUT:
       INTEGER DARID

C OUTPUT
       INTEGER NSEG
       INTEGER CRDARID(*)

C-----------------------------------------------------------------------

       CRDARID(NSEG) = DARID
                               
 9999  CONTINUE
       RETURN
       END
