C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	mpscvg.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================

C-----------------------------------------------------------------------
C
C SUBROUTINE MPSCVG
C
C PURPOSE
C	ALLOW USER TO DISPLAY CVRG DATA FROM FILE OR DB AND
C	TO SPECIFY REV/TIME/SAT/SENSOR
C
C $Logfile:   ACS003:[BLD.MPS.MAPR.SRC]MPSCVG.QFV  $
C
C VARIABLES
C INPUT
C	WSID  		WORKSTATION ID NUMBER
C	PROJN		PROJECTION TYPE
C	OBSLAT,OBSLON	CENTER LAT/LON
C	STMNX,STMXX	MIN/MAX WINDOW X
C	STMNY,STMXY	MIN/MAX WINDOW Y
C
C OUTPUT
C	NSEG  		NUMBER OF SEGMENTS CREATED
C	CRSEGN		ARRAY OF SEGMENT ID'S OF CREATED
C	CRSEGT		ARRAY OF SEGMENT NAMES OF CREATD 
C
C INTERNAL
C	DBFILE		USER OPTION FILE/DB
C	OPT		USER OPTION STRT/STOP REV/TIME
C	MASKOPT		USER OPTION WITHIN MASK/GLOBAL
C	GTOPT		USER OPTION GRND TRACK/SWATH/BOTH
C	TXOPT		USER OPTION SAT/SEN/REV / TIME / BOTH
C	FREQ		FREQUENCY OF TEXT
C	STAT		STATUS
C	STRTREV,STOPREV	START/STOP REV
C	SAT,SEN		SATELLITE,SENSOR
C	STRTT,STOPT	START/STOP TIME - ASF FORMAT
C	CVGFILE		COVERAGE FILENAME
C	IOS   		INPUT/OUTPUT STATUS NUMBER
C
C SUBROUTINES CALLED 
C        CRTSEG
C        LTRANS
C        CLSSEG
C
C WRITTEN BY CRAIG K. FUJIMOTO
C
C MODIFICATIONS
C $Date$ $Revision$ $Author$
C
C 6/27/94  Nadia Adhami -port to UNIX/C (INGRIS -> SYBASE)
C 10/4/94  Nadia Adhami TIME2REV() add dbproc input parameter
C 09/8/95  Nadia Adhami PR#406 : use DEF_START_TIME for coverage query
C-----------------------------------------------------------------------
 
       SUBROUTINE MPSCVG (WSID,PROJN,
     1                    OBSLAT,OBSLON,
     2                    STMNX,STMXX,STMNY,STMXY,
     2                    NSEG,CRSEGN,CRSEGT)

      character*100 SccsFileID
     -/'@(#)mpscvg.for	5.1 98/01/08 APS/ASF\0'/

       IMPLICIT NONE

C INPUT:
       INTEGER PROJN,WSID
       REAL OBSLON,OBSLAT
       REAL STMNX,STMXX,STMNY,STMXY

C OUTPUT
       CHARACTER*(*) CRSEGT(*) 
       INTEGER NSEG,CRSEGN(*)

C INTERNAL:
       INTEGER OPT,DBFILE,MASKOPT,GTOPT,TXOPT,FREQ,STAT
       INTEGER STRTREV,STOPREV

       CHARACTER*2 SAT
       CHARACTER*3 SEN
       CHARACTER*21 STRTT,STOPT
       CHARACTER*57 CVGFILE

c--port--    STATEMENTS TO SUPPORT SYBASE CALLS TO DBLIB

      external dbexit           !$pragma c(dbexit)

C-----------------------------------------------------------------------


C ASK IF THEY WANT TO DISPLAY CVRG FROM A FILE OR FROM THE DB

       CALL ASK_DBFILE(DBFILE)

C IF FROM DATABASE, ASK FOR SAT AND SENSOR

       IF (DBFILE .EQ. 1) THEN

         CALL ASK_SAT(SAT)                          

         CALL ASK_SENSOR(SAT, SEN)

C IF FROM FILE, GET SAT AND SENSOR FROM FILE

       ELSE IF (DBFILE .EQ. 2) THEN 

         CALL READ_CVG(CVGFILE,SAT,SEN,STAT)
         IF (STAT .NE. 0) GO TO 9999

       END IF

C DISPLAY CVRG BY TIME OR REV

       CALL CVGMNU(OPT)

C GET STRTTIME AND STOPTIME

       IF (OPT .EQ. 1) THEN

 1000    CONTINUE

         CALL ASK_TIME(STRTT,' START TIME (yyyy:ddd:hh:mm) : ')

         CALL ASK_TIME(STOPT,' STOP TIME (yyyy:ddd:hh:mm) : ')

         IF (STRTT .GE. STOPT) THEN

           CALL DISMSG('Error : Start time must be before stop time.')
           GO TO 1000

         END IF

C GET STRT AND STOP REV NUMBERS

       ELSE IF (OPT .EQ. 2) THEN
             
 2000    CONTINUE
 
         CALL ASK_REV(STRTREV,' START REV [1-99999] : ')

         CALL ASK_REV(STOPREV,' STOP REV [1-99999] : ')

         IF (STRTREV .GT. STOPREV) THEN

           CALL DISMSG('Error : Start rev must be <= stop rev.')
           GO TO 2000

         END IF

       ELSE                              

         GO TO 9999

       END IF

C DISPLAY GLOBAL CVRG OR ONLY IN MASK
               
       CALL CVGMASK(MASKOPT)
       IF (MASKOPT .EQ. 0) GO TO 9999

C DISPLAY GT OR SWATH OR BOTH?

       CALL CVGGTMNU(GTOPT)
       IF (GTOPT .EQ. 0) GO TO 9999

C DISPLAY TEXT SAT INFO OR TIME INFO OR BOTH

       CALL CVGTXMNU(TXOPT)
       IF (TXOPT .EQ. 0) GO TO 9999

C GET TEXT FREQUENCY

       IF (TXOPT .NE. 4) THEN

         CALL ASK_TXFREQ(FREQ)
                    
       END IF


       IF (DBFILE .EQ. 1) THEN

C RECORDS FROM DATABASE

         CALL DIS_CVGD (WSID,PROJN,
     1               OBSLAT,OBSLON,
     2               STMNX,STMXX,STMNY,STMXY,
     3               SAT,SEN,STRTREV,STOPREV,STRTT,STOPT,
     4               OPT,MASKOPT,GTOPT,TXOPT,FREQ,
     5               NSEG,CRSEGN,CRSEGT)

       ELSE

C RECORDS FROM FILE

         CALL DIS_CVGF (WSID,PROJN,
     1               OBSLAT,OBSLON,
     2               STMNX,STMXX,STMNY,STMXY,
     3               SAT,SEN,STRTREV,STOPREV,STRTT,STOPT,
     4               CVGFILE,OPT,MASKOPT,GTOPT,TXOPT,FREQ,
     5               NSEG,CRSEGN,CRSEGT)

       END IF


 9999  CONTINUE
       RETURN
       END


C-----------------------------------------------------------------------
C
C SUBROUTINE READ_CVG
C
C VARIABLES   
C INPUT:
C	CVGFILE		COVERAGE FILENAME
C
C OUTPUT:
C
C	SAT,SEN		SATELLITE/SENSOR
C	STAT		STATUS
C             
C INTERNAL:
C
C	IOS		I/O STATUS
C	STRTY,STRTT	START YEAR/TIME
C	STOPY,STOPT	STOP  "
C	GENY,GENT	GENERATION YEAR/TIME
C	VER		VERSION
C	EPHM		EPHEMERIS FILENAME
C	ANSW		ANSWER TO CONFIRMATION
C	STRT,STOP,GEN	START/STOP/GENERATION TIME - ASF FORMAT
C
C WRITTEN BY CRAIG K. FUJIMOTO
C
C-----------------------------------------------------------------------

       SUBROUTINE READ_CVG(CVGFILE,SAT,SEN,STAT)

       IMPLICIT NONE

C INPUT:
       CHARACTER*(*) CVGFILE,SAT,SEN

C OUTPUT:
       INTEGER STAT

C INTERNAL:
       INTEGER IOS

       CHARACTER*4 STRTY,STOPY,GENY
       CHARACTER*16 STRTT,STOPT,GENT
       CHARACTER*14 EPHM
       CHARACTER*21 STRT,STOP,GEN
       CHARACTER*1 ANSW
       REAL VER
       INCLUDE 'APS_HOME:include/local/timeconv.inc'
C-----------------------------------------------------------------------

 1000  CONTINUE

C GET THE FILENAME

       CALL ASK_FNAME(CVGFILE,0)

C OPEN FILE FOR READING

       OPEN (UNIT=10,FILE=CVGFILE,IOSTAT=IOS,STATUS='OLD')
                                  
       IF (IOS .NE. 0) THEN
         IF (IOS .EQ. 29) THEN
           CALL DISMSG('Error :  Coverage File not found.')
           STAT = 1
           GO TO 9999
         ELSE
           CALL DISMSG('Error opening Coverage file.')
           STAT = 1
           GO TO 9999
         END IF                  
       END IF

C READ FIRST RECORD FROM FILE TO GET SAT AND SENSOR

       READ (10,FMT=150,ERR=2000,END=2010) 
     1       SAT,SEN,STRTY,STRTT,STOPY,STOPT,
     1       GENY,GENT,VER,EPHM
  150  FORMAT (A2,1X,A3,1X,3(A4,1X,A16,1X),A5,1X,A13)

       CLOSE (10)

       STRT = STRTY(1:4) // ':' // STRTT(1:16)
       STOP = STOPY(1:4) // ':' // STOPT(1:16)
       GEN =  GENY(1:4)  // ':' // GENT(1:16)

       STAT = tc_validate_asf_datetime(STRT // char (0) )
       IF (STAT .NE. 1) THEN
         GOTO 2000
       END IF

       STAT = tc_validate_asf_datetime(STOP // char (0) )
       IF (STAT .NE. 1) THEN
         GOTO 2000
       END IF

       STAT = tc_validate_asf_datetime(GEN // char (0) )
       IF (STAT .NE. 1) THEN
         GOTO 2000
       END IF

       WRITE (6,160)
       WRITE (6,161) SAT,SEN
       WRITE (6,162) STRT
       WRITE (6,163) STOP
       WRITE (6,164) GEN
  160  FORMAT (/,' FILE HEADER INFORMATION',/)
  161  FORMAT (/,' SAT/SENSOR : ',A2,'/',A3)
  162  FORMAT (/,' START TIME: ',A21)
  163  FORMAT (/,' STOP  TIME: ',A21)
  164  FORMAT (/,' CREATED   : ',A21)

       WRITE (6,165)
  165  FORMAT (/,' Is this the correct file? [Y/N] ',$)

       READ (5,170) ANSW
  170  FORMAT (A1)

       IF (ANSW .NE. 'Y' .AND. ANSW .NE. 'y') GO TO 1000

       STAT = 0
       GO TO 9999
              
 2000  CONTINUE
       CALL DISMSG('Error reading Coverage file.')
       STAT = 1
       GO TO 9999

 2010  CONTINUE
       CALL DISMSG('Error : Coverage file has no records.')
       STAT = 1

 9999  CONTINUE                  
       RETURN
       END
             


                    
C-----------------------------------------------------------------------
C
C SUBROUTINE DIS_CVGD
C
C PURPOSE
C	DISPLAY USER SELECTED COVERAGE ON THE MAP
C
C VARIABLES
C INPUT
C	WSID			WORKSTATION ID
C	PROJN			PROJECTION NUMBER
C	OBSLAT,OBSLON		CENTER LAT/LON		
C	MINX,MAXX,MINY,MAXY	MIN/MAX X/Y OF WINDOW
C	SAT,SEN			SATELLITE/SENSOR
C	STRTREV,STOPREV		START/STOP REV
C	STRTT,STOPT		START/STOP TIME - ASF FORMAT
C	CVGFILE			CVRG FILENAME
C	OPT			BY REV/BY TIME?
C	MASKOPT			CVRG IN MASK? 1=NO / 2=YES
C	GTOPT			DISPLAY GT? 1=GT / 2=SWATH / 3=BOTH
C	TXOPT			DISPLAY TEXT? 1=SAT/2=TIME/3=BOTH/4=NONE
C	FREQ			FREQUENCY TO DISPLAY TEXT
C
C OUTPUT
C	NSEG			SSEGMENT COUNTER
C	CRSEGN			SSEGMENT NUMBER ARRAY
C	CRSEGT			SSEGMENT NAME ARRAY
C
C INTERNAL
C	ERRNO,RCOUNT		INGRES ERROR NUM AND ROW CNT
C	ERRFLAG			DATA ERROR FLAG
C	TEST	     		TEXT DISPLAY FLAG
C	I,J			COUNTERS
C	TOTAL			CVRG RECORD COUNTER                           
C	LAST			FLAG INDICATING LAST RECORD
C	IOS			RETURN STATUS OF READ
C	REV			REV NUMBER
C	REVLEN			REV CHAR LENGTH
C      	MINDIF			ONE ET MINUTE
C	NEWET,OLDET		TEMP HOLDERS FOR MJDATE OF CVRG RECORD         
C	CSLAT,CSLON		CVRG REC SUBSAT LAT/LON
C	CNRLAT,CNRLON		CVRG REC NEAR LAT/LON
C	CFRLAT,CFRLON		CVRG REC FAR LAT/LON
C	SX,SY			X,Y COORDINATE
C	TEXX,TEXY		TEXT X,Y COORDINATES
C	OLDSW(),SWT()		TEMP SWATH X/Y COORDINATES
C	GT(),OLDGT()		GRND TRACK X/Y COORDINATES
C	REVTXT			REV NUM TEXT
C	SATTXT			SAT/SENS/REV TEXT
C	TIMTXT			DATE/TIME TEXT
C	YEAR,TIME,DATE
C	ASFMASK
C	MASK1
C	MASK2
C	SEGNAM
C	*HID			FLAGS INDICATING IF POINT IS HIDDEN
C	BRNCUT			FLAG INDICATING IF LINE IS BROKEN
C	DUMC
C
C WRITTEN BY CRAIG K FUJIMOTO
C
C-----------------------------------------------------------------------
       SUBROUTINE DIS_CVGD (WSID,PROJN,
     1               OBSLAT,OBSLON,
     2               MINX,MAXX,MINY,MAXY,
     3               SAT,SEN,STRTREV,STOPREV,STRTT,STOPT,
     4               OPT,MASKOPT,GTOPT,TXOPT,FREQ,
     5               NSEG,CRSEGN,CRSEGT)
        
       IMPLICIT NONE
                             
c--port--##     DECLARE

C INPUT

c--port-       INCLUDE 'SYS$LIBRARY:GKSDEFS.BND'

       INCLUDE 'GKS_ROOT:include/fgksenum.inc'
       INCLUDE 'mapr_db_extern.inc'
       INCLUDE 'APS_ROOT:include/local/f77_db_cvrg.inc'
       INCLUDE 'mapper_port.inc'

       INTEGER WSID,PROJN

       REAL OBSLAT,OBSLON
       REAL MINX,MAXX,MINY,MAXY

       CHARACTER*(*) SAT,SEN,STRTT,STOPT

       INTEGER STRTREV,STOPREV,OPT,MASKOPT,GTOPT,TXOPT,FREQ

C OUTPUT

       INTEGER NSEG,CRSEGN(*)

       CHARACTER*(*) CRSEGT(*)

C INTERNAL                      

       INTEGER XREV,REV1,REV2
       INTEGER ERRNO,ERRFLAG,TEST
       INTEGER J,LAST,REVLEN,IOS
       INTEGER TEXP

       DOUBLE PRECISION MINDIF,NEWET,OLDET         
       DOUBLE PRECISION STRTET,STOPET

       REAL CSLAT,CSLON,CNRLAT,CNRLON,CFRLAT,CFRLON
       
       REAL TEXX,TEXY
       REAL OLDSW(2,2),SWT(2,2),GT(2),OLDGT(2)

c--port-- add a character to each string for null terminator

       CHARACTER*4 XSEN
       CHARACTER*3 XSAT
       CHARACTER*3 MASK
       CHARACTER*2 AD
       CHARACTER*22 TTIME

       CHARACTER*30 REVTXT,SATTXT,TIMTXT
       CHARACTER*21 SEGNAM

       LOGICAL SWTHID(2),OLDHID(2),GTHID
       LOGICAL BRNCUT,OLDGTHID

c--port--    STATEMENTS TO SUPPORT SYBASE CALLS TO DBLIB

      integer   SLEN
      integer   LASTC
      integer   NRECS
	  integer   LOCAL_DUMMY_VALUEHOLDER

      REAL  GET_DEF_CH
      REAL  VER_SPACE

      INCLUDE 'APS_HOME:include/local/timeconv.inc'

C FUNCTIONS

C-----------------------------------------------------------------------
C FOLLOWING ASSIGNMENT REMOVES WARNING MESSAGE FOR UNUSED VARIABLE PTR
      INTEGER*4 TEMP
      DATA TEMP /10/
       PTRPTR = LOC (TEMP)
       LOCAL_DUMMY_VALUEHOLDER = PTR

C ONE ET MINUTE
       MINDIF = 1.0 / 24.0 / 60.0 * 1.5

C SET TEXT COLOR AND HEIGHT, AND POLYLINE COLOR

C GRAPHICS PARAMETERS

C POLYMARKER SCALE
       CALL GSMKSC (0.05)
C POLYMARKER TYPE
       CALL GSMK (GPLUS)
                  
C TEXT HEIGHT
       CALL GSCHH (GET_DEF_CH())
C TEXT ALIGNMENT
       CALL GSTXAL (GALEFT,GAHALF)

C POLYLINE COLOR INDEX
       CALL GSPLCI (9)
C POLYMARKER COLOR INDEX
       CALL GSPMCI (9)
C TEXT COLOR INDEX
       CALL GSTXCI (9)
C SAVE DEFAULT CHARACTER HEIGHT
       VER_SPACE = GET_DEF_CH()

C FORM THE SEGMENT NAME

       SEGNAM = 'CVG ' // SAT(1:2) // '/' // SEN(1:3)
   
C SET THE DEFAULT VALUES FOR THE RETRIEVE

       IF (OPT .EQ. 1) THEN

         IOS = tc_asf2et(STRTT // char(0) ,STRTET)
         IF (IOS .NE. 1) THEN
           CALL DISMSG('Error converting ASF time.')
           GO TO 9999
         END IF

         IOS = tc_asf2et(STOPT // char(0) ,STOPET)
         IF (IOS .NE. 1) THEN
           CALL DISMSG('Error converting ASF time.')
           GO TO 9999
         END IF

         CALL TIME2REV( MAPR_DBPROC,
     ?     SAT,STRTET,STRTT // char(0) ,REV1,REV2,IOS)
         IF (IOS .EQ. -1) THEN
           CALL DISMSG('Error finding rev number.')
           GO TO 9999
         ELSE IF (IOS .EQ. 3 .OR. IOS .EQ. 4) THEN
           CALL DISMSG('Error: Coverage records not found.')
           GO TO 9999
         ELSE IF (IOS .EQ. 1) THEN
           STRTREV = REV2
         ELSE
           STRTREV = REV1
         END IF

         CALL TIME2REV( MAPR_DBPROC,
     ?     SAT,STOPET,STOPT,REV1,REV2,IOS)
         IF (IOS .EQ. -1) THEN
           CALL DISMSG('Error finding rev number.')
           GO TO 9999
         ELSE IF (IOS .EQ. 1 .OR. IOS .EQ. 4) THEN
           CALL DISMSG('Error: Coverage records not found.')
           GO TO 9999
         ELSE IF (IOS .EQ. 3) THEN
           STOPREV = REV1
         ELSE
           STOPREV = REV2
         END IF


       ELSE IF (OPT. EQ. 2) THEN

C        invalid start time, use default start time
C        STRTT = '1978:001:00:00:00.000'

         STRTT = DEF_START_TIME
         STOPT = '2010:001:00:00:00.000'

         IOS = tc_asf2et(STRTT // char(0) ,STRTET)
         IF (IOS .NE. 1) THEN
           CALL DISMSG('Error converting ASF time.')
           GO TO 9999
         END IF

         IOS = tc_asf2et(STOPT // char(0) ,STOPET)
         IF (IOS .NE. 1) THEN
           CALL DISMSG('Error converting ASF time.')
           GO TO 9999
         END IF

       END IF

C SET THE MASK VARIABLE TO SELECT CVG IN MASK OR GLOBALLY

       IF (MASKOPT .EQ. 2) THEN   
         MASK = 'ASF'
       ELSE IF (MASKOPT .EQ. 3) THEN   
         MASK = 'MCM'
       ELSE
         MASK = '  '
       END IF

C INIT COUNTERS AND FLAGS                      
       J = 1
       ERRFLAG = 0

C CREATE THE SEGMENT

       WRITE(6,105) SEGNAM
  105  FORMAT(/,1X,'Creating overlay ',A<SLEN(SEGNAM)>,'...',$)

       CALL CRTSEG (WSID,NSEG,SEGNAM,CRSEGN,CRSEGT)

C RETRIEVE MATCHING CVRG RECORDS
                                        
       CALL DISMSGW ('Retrieving Coverage records...')

       IF ((MASK .EQ. 'ASF') .OR. (MASK .EQ. 'MCM')) THEN
           WRITE(WHERE_CLAUSE, 1) SAT,SEN,STRTREV,STOPREV,
     ?        STRTET,STOPET,MASK
    1      FORMAT('where sat = "',A,'" and sensor = "',A,'" and ',
     ?        'rev >= ',I,' and rev <= ',I,' and mjdate >= ',F,
     ?        ' and mjdate <= ',F,' and station_id = "',A,'"')
       ELSE
           WRITE(WHERE_CLAUSE, 2) SAT,SEN,STRTREV,STOPREV,
     ?        STRTET,STOPET
    2      FORMAT('where sat = "',A,'" and sensor = "',A,'" and ',
     ?        'rev >= ',I,' and rev <= ',I,' and mjdate >= ',F,
     ?        ' and mjdate <= ',F)
       ENDIF

       WHERE_CLAUSE = WHERE_CLAUSE(1:LASTC(WHERE_CLAUSE))//char(0)
       PRINT *, WHERE_CLAUSE

       llistptr = db_get_records(%VAL(MAPR_DBPROC), 
     ?    'cvrg'//char(0), WHERE_CLAUSE,'mjdate'//char(0), 
     ?    CVRG_COLUMNS,%VAL(ALL_COLS))
       if ( llistptr .EQ. 0 ) THEN
           WRITE(*,*)'MPSCVG:  ERROR in database query.'
C         go to normal end
           GO TO 8888
       ENDIF

       call get_no_elements(llist,NRECS)
 
       IF (NRECS .EQ. 0) THEN
           CALL db_ftn_free_llist(llist)
           WRITE(*,*)'MPSCVG:  No relevant coverage records found.'
C       go to normal end
           GO TO 8888
       ENDIF

       P2_DATA_RECORD = db_ftn_first_record(llist, ptrptr)
 
       DO 4000 WHILE (P2_DATA_RECORD .NE. 0)
 
        call get_cvrg_rec(%VAL(P2_DATA_RECORD),
     ?    XSAT,XSEN,XREV,NEWET,
     ?    CSLAT, CSLON, CNRLAT, CNRLON, CFRLAT, CFRLON,
     ?    AD )
 
C CHECK LAT/LON DATA FOR CORRECTNESS

         IF (CSLAT  .LT. -90.0  .OR. CSLAT  .GT. 90.0  .OR.
     1       CSLON  .LT. -180.0 .OR. CSLON  .GT. 180.0 .OR.
     2       CNRLAT .LT. -90.0  .OR. CNRLAT .GT. 90.0  .OR.
     3       CNRLON .LT. -180.0 .OR. CNRLON .GT. 180.0 .OR.
     4       CFRLAT .LT. -90.0  .OR. CFRLAT .GT. 90.0  .OR.
     5       CFRLON .LT. -180.0 .OR. CFRLON .GT. 180.0) THEN

           ERRFLAG = 1

C ##         ENDLOOP
C          exit the loop

           GO TO 4200

         END IF

C IF NOT THE 1ST RECORD AND NEW REC - OLD REC > ONE MINUTE..

         IF (J .NE. 1 .AND. (NEWET - OLDET) .GT. MINDIF) THEN

C IF DISPLAYING SWATH..

           IF (GTOPT .EQ. 2 .OR. GTOPT .EQ. 3) THEN

C DRAW LAST ACROSS LINE OF SWATH

             CALL DIS_LINE(PROJN,
     1                 SWT(1,1),SWT(1,2),SWT(2,1),SWT(2,2),
     2                 SWTHID(1),SWTHID(2),
     3                 MINX,MAXX,MINY,MAXY)

           END IF

C RESET THE RECORD COUNTER

           J = 1

         END IF


C DO GROUND TRACK

         IF (GTOPT .EQ. 1 .OR. GTOPT .EQ. 3) THEN

C CONVERT THE SUB SATELLITE POINT TO X AND Y

           CALL LTRANS (PROJN,OBSLAT,OBSLON,
     1                  CSLON,CSLAT,
     2                  GT(1),GT(2),
     3                  GTHID,BRNCUT)
                     
C IF NOT 1ST REC, DISPLAY PREVIOUS AND CURRENT CVRG REC

           IF (J.NE.1 .AND. (.NOT. OLDGTHID .OR. .NOT. GTHID)) THEN
                  
               CALL DIS_LINE(PROJN,
     1              OLDGT(1),OLDGT(2),GT(1),GT(2),
     2              OLDGTHID,GTHID,
     3              MINX,MAXX,MINY,MAXY)


           END IF

C DRAW THE POLYMARKER - PLUS SIGN

           IF (.NOT. GTHID) THEN

             CALL GPM (1,GT(1),GT(2))
  
           END IF

C SAVE THE POINT FOR THE NEXT LINE SEGMENT

           OLDGT(1) = GT(1)
           OLDGT(2) = GT(2)
           OLDGTHID = GTHID

         END IF



C DO SENSOR SWATH
                                        
         IF (GTOPT .EQ. 2 .OR. GTOPT .EQ. 3) THEN

C CONVERT THE NEAR POINT TO X AND Y
                  
           CALL LTRANS (PROJN,OBSLAT,OBSLON,
     1                  CNRLON,CNRLAT,
     2                  SWT(1,1),SWT(1,2),
     3                  SWTHID(1),BRNCUT)

C CONVERT THE FAR POINT TO X AND Y

           CALL LTRANS (PROJN,OBSLAT,OBSLON,
     1                  CFRLON,CFRLAT,
     2                  SWT(2,1),SWT(2,2),
     3                  SWTHID(2),BRNCUT)
             
C IF NOT 1ST REC, DISPLAY PREVIOUS AND CURRENT CVRG REC

           IF ((J.NE.1) .AND.
     1        (.NOT. OLDHID(1) .OR. .NOT. OLDHID(2) .OR.
     2         .NOT. SWTHID(1) .OR. .NOT. SWTHID(2))) THEN
                  
             CALL DIS_SWATH (PROJN,
     1                       OLDSW,SWT,
     2                       OLDHID,SWTHID,
     3                       MINX,MAXX,MINY,MAXY,
     4                       LAST)
                                              
           END IF
                              
C SAVE CURRENT RECORD FOR DRAWING THE NEXT PIECE OF SWATH

           OLDSW(1,1) = SWT(1,1)
           OLDSW(1,2) = SWT(1,2)
           OLDSW(2,1) = SWT(2,1)
           OLDSW(2,2) = SWT(2,2)
           
           OLDHID(1) = SWTHID(1)
           OLDHID(2) = SWTHID(2)

         END IF
 
C SET THE OLD TIME TO THE CURRENT TIME

         OLDET = NEWET

C INCREMENT THE COUNTER

         J = J + 1

c--port- ##     }

         P2_DATA_RECORD = db_ftn_next_record(llist, ptrptr)

C      end of loop
 4000  CONTINUE

 4200  CONTINUE
C---     free the linked list and its members, if any, too.  
       CALL db_ftn_free_llist(llist)
 
C DRAW THE FINAL LINE FOR THE SWATH

       IF (GTOPT .EQ. 2 .OR. GTOPT .EQ. 3) THEN

         CALL DIS_LINE(PROJN,
     1                 SWT(1,1),SWT(1,2),SWT(2,1),SWT(2,2),
     2                 SWTHID(1),SWTHID(2),
     3                 MINX,MAXX,MINY,MAXY)

       END IF


C CLOSE THE SEGMENT

       CALL CLSSEG
              

C IF NO TEXT TO BE DISPLAYED, THEN RETURN

       IF (TXOPT .EQ. 4) GO TO 9999



C THE TEXT SEGMENT

C FORM THE SEGMENT NAME

       SEGNAM = 'CVG ' // SAT(1:2) // '/' // SEN(1:3) //
     1          ' TEXT'
   
C INIT COUNTERS AND FLAGS                      

       J = 1
       ERRFLAG = 0

C CREATE THE TEXT SEGMENT

       WRITE(6,210) SEGNAM
  210  FORMAT(/,1X,'Creating overlay ',A<SLEN(SEGNAM)>,'...',$)

       CALL CRTSEG (WSID,NSEG,SEGNAM,CRSEGN,CRSEGT)

C RETRIEVE MATCHING CVRG RECORDS
         
       IF ((MASK .EQ. 'ASF') .OR. (MASK .EQ. 'MCM')) THEN
         WRITE(WHERE_CLAUSE, 3) SAT,SEN,STRTREV,STOPREV,
     ?     STRTET,STOPET,MASK
    3    FORMAT('where sat = "',A,'" and sensor = "',A,'" and ',
     ?     'rev >= ',I,' and rev <= ',I,' and mjdate >= ',F,
     ?     ' and mjdate <= ',F,' and station_id = "',A,'"')
       ELSE
         WRITE(WHERE_CLAUSE, 4) SAT,SEN,STRTREV,STOPREV,
     ?     STRTET,STOPET
    4    FORMAT('where sat = "',A,'" and sensor = "',A,'" and ',
     ?     'rev >= ',I,' and rev <= ',I,' and mjdate >= ',F,
     ?     ' and mjdate <= ',F)
       ENDIF

       WHERE_CLAUSE = WHERE_CLAUSE(1:LASTC(WHERE_CLAUSE))//char(0)
       PRINT *, WHERE_CLAUSE
 
       llistptr = db_get_records(%VAL(MAPR_DBPROC), 'cvrg'//char(0),
     ?    WHERE_CLAUSE,'mjdate'//char(0),CVRG_COLUMNS,%VAL(ALL_COLS))
       if ( llistptr .EQ. 0 ) THEN
           WRITE(*,*)'MPSCVG:  ERROR in database query.'
C         go to normal end
           GO TO 8888
       ENDIF

       call get_no_elements(llist,NRECS)
 
       IF (NRECS .EQ. 0) THEN
           CALL db_ftn_free_llist(llist)
           WRITE(*,*)'SSCVC:  No relevant coverage records found.'
C       go to normal end
           GO TO 8888
       ENDIF

       P2_DATA_RECORD = db_ftn_first_record(llist, ptrptr)
       DO 5000 WHILE (P2_DATA_RECORD .NE. 0)

        call get_cvrg_rec(%VAL(P2_DATA_RECORD),
     ?    XSAT,XSEN,XREV,NEWET,
     ?    CSLAT, CSLON, CNRLAT, CNRLON, CFRLAT, CFRLON,
     ?    AD )
 
C CHECK LAT/LON DATA FOR CORRECTNESS

         IF (CSLAT  .LT. -90.0  .OR. CSLAT  .GT. 90.0  .OR.
     1       CSLON  .LT. -180.0 .OR. CSLON  .GT. 180.0 .OR.
     2       CNRLAT .LT. -90.0  .OR. CNRLAT .GT. 90.0  .OR.
     3       CNRLON .LT. -180.0 .OR. CNRLON .GT. 180.0 .OR.
     4       CFRLAT .LT. -90.0  .OR. CFRLAT .GT. 90.0  .OR.
     5       CFRLON .LT. -180.0 .OR. CFRLON .GT. 180.0) THEN

           ERRFLAG = 1
c--port- ##         ENDLOOP
C          exit the loop
 
           GO TO 5200

         END IF

C IF NOT THE 1ST RECORD AND NEW REC - OLD REC > ONE MINUTE..

         IF (J .NE. 1 .AND. (NEWET - OLDET) .GT. MINDIF) THEN

           J = 1
                        
         END IF

C FORM THE TEXT TO BE DISPLAYED
                  
         ENCODE(5,'(I5)',REVTXT) XREV
         CALL TRIM(REVTXT,5,REVLEN)

         SATTXT = ' ' // XSAT(1:2) // '/' // XSEN(1:3) //
     1            '/' // REVTXT(1:REVLEN) // AD(1:1)

         IOS = tc_et2asf(%VAL(NEWET),TTIME)

         TIMTXT = ' ' // TTIME(1:17)

C SET THE FLAG TO INDICATE WHETHER TEXT SHOULD BE DISPLAYED AT THIS CENTER

         TEST = MOD(J,FREQ)

C DO GROUND TRACK

         IF (GTOPT .EQ. 1 .OR. GTOPT .EQ. 3) THEN

C CONVERT THE SUB SATELLITE POINT TO X AND Y

           CALL LTRANS (PROJN,OBSLAT,OBSLON,
     1                  CSLON,CSLAT,
     2                  GT(1),GT(2),
     3                  GTHID,BRNCUT)
                     
C CHECK FOR DISPLAY OF TEXT

           IF (.NOT. GTHID .AND. (TEST.EQ.0 .OR. J.EQ.1)) THEN

             IF (TXOPT .EQ. 1) THEN

               SATTXT = SATTXT(1:LASTC(SATTXT)) // char(0)
C TEXT ALIGNMENT
               CALL GSTXAL (GALEFT,GAHALF)
C SATELLITE TEXT
               CALL GTXS(GT(1),GT(2),LEN(SATTXT),SATTXT)

             ELSE IF (TXOPT .EQ. 2) THEN

               TIMTXT = TIMTXT(1:LASTC(TIMTXT)) // char(0)
C TEXT ALIGNMENT
               CALL GSTXAL (GALEFT,GAHALF)
C TIME INFO
               CALL GTXS(GT(1),GT(2),LEN(TIMTXT),TIMTXT)

             ELSE IF (TXOPT .EQ. 3) THEN

               SATTXT = SATTXT(1:LASTC(SATTXT)) // char(0)
               TIMTXT = TIMTXT(1:LASTC(TIMTXT)) // char(0)
C TEXT ALIGNMENT 
               CALL GSTXAL (GALEFT,GABASE)
               CALL GTXS(GT(1),GT(2),LEN(SATTXT),SATTXT)

               GT(2) = GT(2) + VER_SPACE + (VER_SPACE/3)

               CALL GSTXAL (GALEFT,GACAP)
               CALL GTXS(GT(1),GT(2),LEN(TIMTXT),TIMTXT)

             END IF

           END IF

         END IF



C DO SENSOR SWATH

         IF (GTOPT .EQ. 2) THEN

C CONVERT THE NEAR POINT TO X AND Y
                  
           CALL LTRANS (PROJN,OBSLAT,OBSLON,
     1                  CNRLON,CNRLAT,
     2                  SWT(1,1),SWT(1,2),
     3                  SWTHID(1),BRNCUT)

C CONVERT THE FAR POINT TO X AND Y

           CALL LTRANS (PROJN,OBSLAT,OBSLON,
     1                  CFRLON,CFRLAT,
     2                  SWT(2,1),SWT(2,2),
     3                  SWTHID(2),BRNCUT)
             

           IF (SWT(1,1) .GT. SWT(2,1)) THEN
             TEXX = SWT(1,1)
             TEXY = SWT(1,2)
             TEXP = 1
           ELSE
             TEXX = SWT(2,1)
             TEXY = SWT(2,2)
             TEXP = 2
           END IF

           IF (.NOT.SWTHID(TEXP) .AND. (TEST.EQ.0 .OR. J.EQ.1)) THEN

             IF (TXOPT .EQ. 1) THEN

C TEXT ALIGNMENT
               CALL GSTXAL (GALEFT,GAHALF)
C SATELLITE TEXT
               CALL GTXS(TEXX,TEXY,LEN(SATTXT),SATTXT)

             ELSE IF (TXOPT .EQ. 2) THEN

C TEXT ALIGNMENT
               CALL GSTXAL (GALEFT,GAHALF)
C TIME INFO
               CALL GTXS(TEXX,TEXY,LEN(TIMTXT),TIMTXT)

             ELSE IF (TXOPT .EQ. 3) THEN
                               
C TEXT ALIGNMENT 
               CALL GSTXAL (GALEFT,GABASE)
               CALL GTXS(TEXX,TEXY,LEN(SATTXT),SATTXT)

               TEXY = TEXY + VER_SPACE + (VER_SPACE/3)

               CALL GSTXAL (GALEFT,GATOP)
               CALL GTXS(TEXX,TEXY,LEN(TIMTXT),TIMTXT)

             END IF

           END IF

         END IF
 
C SAVE THE CURRENT MJDATE

         OLDET = NEWET

C INCREMENT THE COUNTER

         J = J + 1

c--port- ##     }

         P2_DATA_RECORD = db_ftn_next_record(llist, ptrptr)
 
C     go to the begining of loop
 5000 CONTINUE
 
 5200  CONTINUE
C--      free the list and also its members, if any:   
       CALL db_ftn_free_llist(llist)
 
       ERRNO = 0
 
       IF (ERRNO .NE. 0) THEN

         CALL DISMSG('Error retrieving Coverage records.')
         GO TO 8888

       ELSE IF (ERRFLAG .NE. 0) THEN
                  
         CALL DISMSG('Error : Invalid Coverage Relation record.')
         GO TO 8888


       END IF


 8888  CONTINUE

C CLOSE THE SEGMENT

       CALL CLSSEG

 9999  CONTINUE      
       RETURN
       END 


C-----------------------------------------------------------------------
C
C SUBROUTINE DIS_CVGF
C
C PURPOSE
C	DISPLAY USER SELECTED COVERAGE FROM A FILE
C
C VARIABLES
C INPUT
C	WSID			WORKSTATION ID
C	PROJN			PROJECTION NUMBER
C	OBSLAT,OBSLON		
C	MINX,MAXX,MINY,MAXY
C	SAT
C	SEN
C	STRTREV,STOPREV
C	STRTT,STOPT
C	CVGFILE			CVRG FILENAME
C	OPT
C	MASKOPT			CVRG IN MASK? 1=NO / 2=YES
C	GTOPT			DISPLAY GT? 1=GT / 2=SWATH / 3=BOTH
C	TXOPT			DISPLAY TEXT? 1=SAT/2=TIME/3=BOTH/4=NONE
C	FREQ			FREQUENCY TO DISPLAY TEXT
C
C OUTPUT
C	NSEG			SSEGMENT COUNTER
C	CRSEGN			SSEGMENT NUMBER ARRAY
C	CRSEGT			SSEGMENT NAME ARRAY
C
C INTERNAL
C	ERRNO,RCOUNT		INGRES ERROR NUM AND ROW CNT
C	ERRFLAG			DATA ERROR FLAG
C	TEST			TEXT DISPLAY FLAG
C	I,J			COUNTERS
C	TOTAL			CVRG RECORD COUNTER                           
C	LAST			FLAG INDICATING LAST RECORD
C	IOS			RETURN STATUS OF READ
C	REV			REV NUMBER
C	REVLEN			REV CHAR LENGTH
C      	MINDIF			ONE ET MINUTE
C	NEWET,OLDET		TEMP HOLDERS FOR MJDATE OF CVRG RECORD         
C	CSLAT,CSLON		CVRG REC SUBSAT LAT/LON
C	CNRLAT,CNRLON		CVRG REC NEAR LAT/LON
C	CFRLAT,CFRLON		CVRG REC FAR LAT/LON
C	SX,SY			X,Y COORDINATE
C	TEXX,TEXY		TEXT X,Y COORDINATES
C	OLDSW(2,2)
C	SWT(2,2)
C	GT(2)
C	OLDGT(2)
C
C       CHARACTER*30
C	REVTXT			REV NUM TEXT
C	SATTXT			SAT/SENS/REV TEXT
C	TIMTXT			DATE/TIME TEXT
C	YEAR,TIME,DATE
C	ASFMASK
C	MASK1
C	MASK2     
C	SEGNAM
C	SWTHID(2)
C	OLDHID(2)
C	GTHID
C	BRNCUT
C      	TEXHID
C	OLDGTHID
C	DUMC
C             
C WRITTEN BY CRAIG K FUJIMOTO
C
C-----------------------------------------------------------------------
       SUBROUTINE DIS_CVGF (WSID,PROJN,
     1               OBSLAT,OBSLON,
     2               MINX,MAXX,MINY,MAXY,
     3               SAT,SEN,STRTREV,STOPREV,STRTT,STOPT,
     4               CVGFILE,OPT,MASKOPT,GTOPT,TXOPT,FREQ,
     5               NSEG,CRSEGN,CRSEGT)

       IMPLICIT NONE

C INPUT
c--port-      INCLUDE 'SYS$LIBRARY:GKSDEFS.BND'
       INCLUDE 'GKS_ROOT:include/fgksenum.inc'
       INCLUDE 'mapper_port.inc'

                   
       INTEGER WSID,PROJN

       REAL OBSLAT,OBSLON
       REAL MINX,MAXX,MINY,MAXY

       CHARACTER*(*) SAT,SEN,STRTT,STOPT,CVGFILE

       INTEGER STRTREV,STOPREV,OPT,MASKOPT,GTOPT,TXOPT,FREQ

C OUTPUT
       INTEGER NSEG,CRSEGN(*)

       CHARACTER*(*) CRSEGT(*)

C INTERNAL                      

       INTEGER ERRFLAG,TEST
       INTEGER I,J,TOTAL,LAST,IOS,REV
       INTEGER REVLEN
       INTEGER TEXP
       INTEGER CVGCNT,CVGSTRT
                                            
       DOUBLE PRECISION MINDIF,NEWET,OLDET         

       REAL CSLAT,CSLON,CNRLAT,CNRLON,CFRLAT,CFRLON
       
       REAL TEXX,TEXY
       REAL OLDSW(2,2),SWT(2,2),GT(2),OLDGT(2)

       CHARACTER*30 REVTXT,SATTXT,TIMTXT
       CHARACTER*4 YEAR
       CHARACTER*16 TIME
       CHARACTER*21 DATE
       CHARACTER*1 ASFMASK,MASK1,MASK2
       CHARACTER*21 SEGNAM
       
       LOGICAL SWTHID(2),OLDHID(2),GTHID
       LOGICAL BRNCUT,OLDGTHID

C TEMP VARIABLE FOR READ FROM FILE

       CHARACTER*100 DUMC
       integer  SLEN

       REAL GET_DEF_CH
       REAL VER_SPACE
                  
       INCLUDE 'APS_HOME:include/local/timeconv.inc'

C FUNCTIONS

C-----------------------------------------------------------------------

C ONE ET MINUTE
       MINDIF = 1.0 / 24.0 / 60.0 * 1.5

C GRAPHICS PARAMETERS

C POLYMARKER SCALE
       CALL GSMKSC (0.05)
C POLYMARKER TYPE
       CALL GSMK (GPLUS)

C TEXT HEIGHT
       CALL GSCHH (GET_DEF_CH())

C POLYLINE COLOR INDEX
       CALL GSPLCI (9)
C POLYMARKER COLOR INDEX
       CALL GSPMCI (9)
C TEXT COLOR INDEX
       CALL GSTXCI (9)
       
C SAVE DEFAULT CHARACTER HEIGHT    
       VER_SPACE = GET_DEF_CH()

C FORM THE SEGMENT NAME
                                             
       SEGNAM = 'CVG ' // SAT(1:2) // '/' // SEN(1:3)
      

       IF (OPT .EQ. 1) THEN

C IF SELECTING BY TIME THEN SET DEFAULT REV VALUES

         STRTREV = 1
         STOPREV = 99999

       ELSE IF (OPT. EQ. 2) THEN

C IF SELECTING BY REV THEN SET DEFAULT TIME VALUES

         STRTT = '0000:000:00:00:00.000'
         STOPT = '9999:999:99:99:99.999'

       END IF

C SET MASK TO SELECT CVRG WITHIN ASF MASK OR GLOBALLY
C 1=GLOBAL 2=ASF 3=MCMURDO

       IF (MASKOPT .EQ. 1) THEN
         MASK1 = 'A'
         MASK2 = '-'
       ELSE IF (MASKOPT .EQ. 2) THEN
         MASK1 = 'A'
         MASK2 = 'A'
       ELSE
         MASK1 = 'M'
         MASK2 = 'M'
       END IF

C INIT COUNTERS AND FLAGS                       
       J = 1
       TOTAL = 0
       ERRFLAG = 0
       CVGCNT = 0

C CREATE THE SEGMENT
       WRITE(6,100) SEGNAM
  100  FORMAT(/,1X,'Creating overlay ',A<SLEN(SEGNAM)>,'...',$)

       CALL CRTSEG (WSID,NSEG,SEGNAM,CRSEGN,CRSEGT)

C OPEN FILE FOR READING

       OPEN (UNIT=10,FILE=CVGFILE,IOSTAT=IOS,STATUS='OLD')

       IF (IOS .EQ. 29) THEN
         CALL DISMSG('Error : Coverage File not found.')
         GO TO 8888
       ELSE IF (IOS .NE. 0) THEN
         CALL DISMSG('Error opening Coverage file for input.')
         GO TO 8888
       END IF

C READ FIRST RECORD AND DISCARD - METADATA RECORD

       READ (10,FMT=120,ERR=4000,END=1500) DUMC
  120  FORMAT (A93)

C RETRIEVE CVRG RECORDS FROM FILE
                                        
       CALL DISMSGW ('Retrieving Coverage records...')

 1000  CONTINUE

C INCREMENT COUNTER
       CVGCNT = CVGCNT + 1

C READ RECORD INTO PROGRAM VARIABLES

       READ (10,FMT=140,ERR=4000,END=1500) 
     1       YEAR,TIME,REV,CSLAT,CSLON,
     2       CNRLAT,CNRLON,CFRLAT,CFRLON,       
     3       DUMC,ASFMASK,DUMC

  140  FORMAT (A4,1X,A16,1X,I5,1X,6(F7.2,1X),
     1         A17,A1,1X,A22)

C FORM ASF DATE
       DATE = YEAR(1:4) // ':' // TIME(1:16)

C IF RECORD DOES NOT MEET CRITERIA

C--       IF (DATE .LT. STRTT   .OR. DATE .GT. STOPT   .OR.
C--     1     REV  .LT. STRTREV .OR. REV  .GT. STOPREV .OR.
C--     2    (ASFMASK(1:1) .NE. MASK1(1:1) .AND.
C--     3     ASFMASK(1:1) .NE. MASK2(1:1) )) THEN


       IF (DATE .LT. STRTT   .OR. DATE .GT. STOPT   .OR.
     1     REV  .LT. STRTREV .OR. REV  .GT. STOPREV .OR.
     2     ( .NOT. 
     3      (
     4       (MASKOPT .EQ. 2 .AND. ASFMASK(1:1) .EQ. 'A')
     5       .OR.
     6       (MASKOPT .EQ. 3 .AND. ASFMASK(1:1) .EQ. 'M')
     7       .OR.
     8       (MASKOPT .EQ. 1)
     9      ) 
     1     ) 
     2    ) THEN



C CHECK IF NOT THE FIRST RECORD AND DISPLAYING SWATH..
         IF (J .NE. 1 .AND. GTOPT .EQ. 2 .OR. GTOPT .EQ. 3) THEN

C DRAW LAST STEP LINE OF SWATH

           CALL DIS_LINE(PROJN,
     1                 SWT(1,1),SWT(1,2),SWT(2,1),SWT(2,2),
     2                 SWTHID(1),SWTHID(2),
     3                 MINX,MAXX,MINY,MAXY)

         END IF

C IF RECORD IS AFTER USER SPEC. (REV OR TIME), STOP READING FILE.
         IF ((OPT .EQ. 1 .AND. DATE .GT. STOPT) .OR.
     1       (OPT .EQ. 2 .AND. REV .GT. STOPREV)) THEN
           GOTO 1550
         END IF

C RESET THE RECORD COUNTER
         J = 1

C SKIP AND READ NEXT RECORD.
         GO TO 1000

       END IF
                                
C IF RECORD MEETS CRITERIA, DISPLAY IT

       TOTAL = TOTAL + 1 

C SAVE CVRG COUNTER AS FIRST RECORD
       IF (TOTAL .EQ. 1) THEN
         CVGSTRT = CVGCNT
       END IF       


C VALIDATE CVRG RECORD DATA

       IF (CSLAT  .LT. -90.0  .OR. CSLAT  .GT. 90.0  .OR.
     1     CSLON  .LT. -180.0 .OR. CSLON  .GT. 180.0 .OR. 
     2     CNRLAT .LT. -90.0  .OR. CNRLAT .GT. 90.0  .OR.
     3     CNRLON .LT. -180.0 .OR. CNRLON .GT. 180.0 .OR. 
     4     CFRLAT .LT. -90.0  .OR. CFRLAT .GT. 90.0  .OR.
     5     CFRLON .LT. -180.0 .OR. CFRLON .GT. 180.0) THEN
                                         
          ERRFLAG = 1       
          GO TO 1500

       END IF

C CONVERT NEW RECORD DATE,TIME TO JULIAN

       IOS = tc_asf2et(DATE // char(0) ,NEWET)
       IF (IOS .NE. 1) THEN
         ERRFLAG = 1       
         GO TO 1500
       ENDIF

C CHECK IF NEW RECORD AND OLD RECORD DIFFER BY MORE THAN ONE MINUTE

       IF (NEWET - OLDET .GT. MINDIF) THEN

C IF NOT THE FIRST RECORD AND DISPLAYING SWATH..

         IF (J .NE. 1 .AND. GTOPT .EQ. 2 .OR. GTOPT .EQ. 3) THEN

C DRAW LAST ACROSS LINE OF SWATH

           CALL DIS_LINE(PROJN,
     1                 SWT(1,1),SWT(1,2),SWT(2,1),SWT(2,2),
     2                 SWTHID(1),SWTHID(2),
     3                 MINX,MAXX,MINY,MAXY)
                        
         END IF

C RESET THE RECORD COUNTER

         J = 1

       END IF
                      

C DO GROUND TRACK

         IF (GTOPT .EQ. 1 .OR. GTOPT .EQ. 3) THEN

C CONVERT THE SUB SATELLITE POINT TO X AND Y

           CALL LTRANS (PROJN,OBSLAT,OBSLON,
     1                  CSLON,CSLAT,
     2                  GT(1),GT(2),
     3                  GTHID,BRNCUT)

C IF NOT 1ST REC, DISPLAY PREVIOUS AND CURRENT CVRG REC
    
           IF (J.NE.1 .AND. (.NOT. OLDGTHID .OR. .NOT. GTHID)) THEN
                  
             CALL DIS_LINE(PROJN,
     1            OLDGT(1),OLDGT(2),GT(1),GT(2),
     2            OLDGTHID,GTHID,
     3            MINX,MAXX,MINY,MAXY)

           END IF

C DRAW THE POLYMARKER - PLUS SIGN

           IF (.NOT. GTHID) THEN

             CALL GPM (1,GT(1),GT(2))

           END IF

C SAVE GT POINT FOR NEXT PIECE

           OLDGT(1) = GT(1)
           OLDGT(2) = GT(2)
           OLDGTHID = GTHID

         END IF


C DO SENSOR SWATH

         IF (GTOPT .EQ. 2 .OR. GTOPT .EQ. 3) THEN

C CONVERT THE NEAR POINT TO X AND Y

           CALL LTRANS (PROJN,OBSLAT,OBSLON,
     1                  CNRLON,CNRLAT,
     2                  SWT(1,1),SWT(1,2),
     3                  SWTHID(1),BRNCUT)

C CONVERT THE FAR POINT TO X AND Y

           CALL LTRANS (PROJN,OBSLAT,OBSLON,
     1                  CFRLON,CFRLAT,
     2                  SWT(2,1),SWT(2,2),
     3                  SWTHID(2),BRNCUT)
             
C IF NOT 1ST REC, DISPLAY PREVIOUS AND CURRENT CVRG REC

           IF ((J.NE.1) .AND.
     1        (.NOT. OLDHID(1) .OR. .NOT. OLDHID(2) .OR.
     2         .NOT. SWTHID(1) .OR. .NOT. SWTHID(2))) THEN
                  
             CALL DIS_SWATH (PROJN,
     3                       OLDSW,SWT,
     4                       OLDHID,SWTHID,
     5                       MINX,MAXX,MINY,MAXY,
     7                       LAST)

           END IF

C SAVE CURRENT RECORD FOR DRAWING THE NEXT PIECE OF SWATH

           OLDSW(1,1) = SWT(1,1)
           OLDSW(1,2) = SWT(1,2)
           OLDSW(2,1) = SWT(2,1)
           OLDSW(2,2) = SWT(2,2)
           
           OLDHID(1) = SWTHID(1)
           OLDHID(2) = SWTHID(2)

         END IF
 
C SET THE OLD TIME TO THE NEW TIME
                  
       OLDET = NEWET

C INCREMENT CVRG REC COUNTER

       J = J + 1

C NEXT CVRG RECORD

       GO TO 1000 

 1500  CONTINUE
       IF (TOTAL .LE. 1) THEN 
         CALL DISMSG('No Coverage records found.')
         GO TO 7777
       ELSE IF (ERRFLAG .NE. 0) THEN
         CALL DISMSG('Error within Coverage File.')
         GO TO 7777
       END IF

C DRAW THE FINAL LINE FOR THE SWATH

       IF (GTOPT .EQ. 2 .OR. GTOPT .EQ. 3) THEN

         CALL DIS_LINE(PROJN,
     1                 SWT(1,1),SWT(1,2),SWT(2,1),SWT(2,2),
     2                 SWTHID(1),SWTHID(2),
     3                 MINX,MAXX,MINY,MAXY)

       END IF
              
 1550  CONTINUE

C CLOSE THE FILE
       CLOSE (10)

C CLOSE THE SEGMENT
       CALL CLSSEG

C IF NO TEXT TO BE DISPLAYED, RETURN
       IF (TXOPT .EQ. 4) GO TO 9999


C THE TEXT SEGMENT

C FORM THE SEGMENT NAME
                                             
       SEGNAM = 'CVG ' // SAT(1:2) // '/' // SEN(1:3) // ' TEXT'
      
C INIT COUNTERS AND FLAGS                       

       J = 1
       TOTAL = 0
       ERRFLAG = 0

C CREATE THE SEGMENT

       WRITE(6,200) SEGNAM
  200  FORMAT(/,1X,'Creating overlay ',A<SLEN(SEGNAM)>,'...',$)

       CALL CRTSEG (WSID,NSEG,SEGNAM,CRSEGN,CRSEGT)

C OPEN FILE FOR READING

       OPEN (UNIT=10,FILE=CVGFILE,IOSTAT=IOS,STATUS='OLD')

       IF (IOS .EQ. 29) THEN
         CALL DISMSG('Error : Coverage File not found.')
         GO TO 8888
       ELSE IF (IOS .NE. 0) THEN
         CALL DISMSG('Error opening Coverage file for input.')
         GO TO 8888
       END IF

C READ FIRST RECORD AND DISCARD

       READ (10,FMT=220,ERR=4000,END=3000) DUMC
  220  FORMAT (A93)

C READ TO THE POINT OF THE FIRST RECORD
       DO 1900 I = 1, CVGSTRT
         READ (10,FMT=225,ERR=4000,END=3000) DUMC
  225    FORMAT (A117)
 1900  CONTINUE

C RETRIEVE CVRG RECORDS FROM FILE
       CALL DISMSGW('Retrieving Coverage records...')

 2000  CONTINUE

C READ RECORD INTO PROGRAM VARIABLES

       READ (10,FMT=240,ERR=4000,END=3000) 
     1       YEAR,TIME,REV,CSLAT,CSLON,
     2       CNRLAT,CNRLON,CFRLAT,CFRLON,       
     3       DUMC,ASFMASK,DUMC

  240  FORMAT (A4,1X,A16,1X,I5,1X,6(F7.2,1X),
     1         A17,A1,1X,A22)

C FORM ASF DATE                                

       DATE = YEAR(1:4) // ':' // TIME(1:16)


C IF RECORD DOES NOT MEET CRITERIA

C--       IF (DATE .LT. STRTT   .OR. DATE .GT. STOPT   .OR.
C--     1     REV  .LT. STRTREV .OR. REV  .GT. STOPREV .OR.
C--     2     (ASFMASK(1:1) .NE. MASK1(1:1) .AND.
C--     3      ASFMASK(1:1) .NE. MASK2(1:1) )) THEN

       IF (DATE .LT. STRTT   .OR. DATE .GT. STOPT   .OR.
     1     REV  .LT. STRTREV .OR. REV  .GT. STOPREV .OR.
     2     (.NOT. 
     3       (
     4       (MASKOPT .EQ. 2 .AND. ASFMASK(1:1) .EQ. 'A')
     5       .OR.
     6       (MASKOPT .EQ. 3 .AND. ASFMASK(1:1) .EQ. 'M')
     7       .OR.
     8       (MASKOPT .EQ. 1)
     9       )
     1     )
     2   ) THEN

C IF RECORD IS AFTER USER SPEC. (REV OR TIME), STOP READING FILE.
         IF ((OPT .EQ. 1 .AND. DATE .GT. STOPT) .OR.
     1       (OPT .EQ. 2 .AND. REV .GT. STOPREV)) THEN
           GOTO 7777
         END IF

C RESET THE RECORD COUNTER
         J = 1

C READ THE NEXT RECORD      
         GO TO 2000

       END IF


C IF RECORD MEETS CRITERIA
       TOTAL = TOTAL + 1

C VALIDATE CVRG RECORD DATA
       IF (CSLAT  .LT. -90.0  .OR. CSLAT  .GT. 90.0  .OR.
     1     CSLON  .LT. -180.0 .OR. CSLON  .GT. 180.0 .OR. 
     2     CNRLAT .LT. -90.0  .OR. CNRLAT .GT. 90.0  .OR.
     3     CNRLON .LT. -180.0 .OR. CNRLON .GT. 180.0 .OR. 
     4     CFRLAT .LT. -90.0  .OR. CFRLAT .GT. 90.0  .OR.
     5     CFRLON .LT. -180.0 .OR. CFRLON .GT. 180.0) THEN

          ERRFLAG = 1
          GO TO 3000

       END IF

C FORM THE TEXT STRINGS

       ENCODE(5,'(I5)',REVTXT) REV
       CALL TRIM(REVTXT,5,REVLEN)

       SATTXT = ' ' // SAT(1:2) // '/' // SEN(1:3) //
     1          '/' // REVTXT(1:REVLEN)

       TIMTXT = ' ' // YEAR(1:4) // ':' // TIME(1:12)


C CONVERT NEW RECORD DATE,TIME TO JULIAN

       IOS = tc_asf2et(DATE // char(0) ,NEWET)
       IF (IOS .NE. 1) THEN
         ERRFLAG = 1       
         GO TO 3000
       ENDIF

C CHECK IF NEW RECORD AND OLD RECORD DIFFER BY MORE THAN ONE MINUTE

       IF (NEWET - OLDET .GT. MINDIF) THEN

C RESET THE RECORD COUNTER
         J = 1

       END IF


C CALC THE FLAG TO DETERMINE IF TEXT SHOULD BE DISPLAYED AT THIS CENTER

       TEST = MOD(J,FREQ)
                      

C DO GROUND TRACK

         IF (GTOPT .EQ. 1 .OR. GTOPT .EQ. 3) THEN

C CONVERT THE SUB SATELLITE POINT TO X AND Y

           CALL LTRANS (PROJN,OBSLAT,OBSLON,
     1                  CSLON,CSLAT,
     2                  GT(1),GT(2),
     3                  GTHID,BRNCUT)


C CHECK FOR DISPLAY OF TEXT

           IF (.NOT.GTHID .AND. (TEST.EQ.0 .OR. J.EQ.1)) THEN

             IF (TXOPT .EQ. 1) THEN

C TEXT ALIGNMENT
               CALL GSTXAL (GALEFT,GAHALF)
C SATELLITE TEXT
               CALL GTXS(GT(1),GT(2),LEN(SATTXT),SATTXT)

             ELSE IF (TXOPT .EQ. 2) THEN

C TEXT ALIGNMENT
               CALL GSTXAL (GALEFT,GAHALF)
C TIME INFO
               CALL GTXS(GT(1),GT(2),LEN(TIMTXT),TIMTXT)

             ELSE IF (TXOPT .EQ. 3) THEN

C TEXT ALIGNMENT 
               CALL GSTXAL (GALEFT,GABOTT)
               CALL GTXS(GT(1),GT(2),LEN(SATTXT),SATTXT)

               GT(2) = GT(2) + VER_SPACE + (VER_SPACE/3)

               CALL GSTXAL (GALEFT,GATOP)
               CALL GTXS(GT(1),GT(2),LEN(TIMTXT),TIMTXT)

             END IF

           END IF

         END IF


C DO SENSOR SWATH

         IF (GTOPT .EQ. 2) THEN

C CONVERT THE NEAR POINT TO X AND Y

           CALL LTRANS (PROJN,OBSLAT,OBSLON,
     1                  CNRLON,CNRLAT,
     2                  SWT(1,1),SWT(1,2),
     3                  SWTHID(1),BRNCUT)

C CONVERT THE FAR POINT TO X AND Y

           CALL LTRANS (PROJN,OBSLAT,OBSLON,
     1                  CFRLON,CFRLAT,
     2                  SWT(2,1),SWT(2,2),
     3                  SWTHID(2),BRNCUT)
             

C DETERMINE THE RIGHTMOST POINT

           IF (SWT(1,1) .GT. SWT(2,1)) THEN
             TEXX = SWT(1,1)
             TEXY = SWT(1,2)
             TEXP = 1
           ELSE
             TEXX = SWT(2,1)
             TEXY = SWT(2,2)
             TEXP = 2
           END IF

C CHECK TO SEE IF A TEXT LINE SHOULD BE DISPLAYED

           IF (.NOT.SWTHID(TEXP) .AND. (TEST.EQ.0 .OR. J.EQ.1)) THEN

             IF (TXOPT .EQ. 1) THEN

C TEXT ALIGNMENT
               CALL GSTXAL (GALEFT,GAHALF)
C SATELLITE TEXT
               CALL GTXS(TEXX,TEXY,LEN(SATTXT),SATTXT)

             ELSE IF (TXOPT .EQ. 2) THEN

C TEXT ALIGNMENT
               CALL GSTXAL (GALEFT,GAHALF)
C TIME INFO
               CALL GTXS(TEXX,TEXY,LEN(TIMTXT),TIMTXT)

             ELSE IF (TXOPT .EQ. 3) THEN

C TEXT ALIGNMENT 
               CALL GSTXAL (GALEFT,GABOTT)
               CALL GTXS(TEXX,TEXY,LEN(SATTXT),SATTXT)

               TEXY = TEXY + VER_SPACE + (VER_SPACE/3)

               CALL GSTXAL (GALEFT,GATOP)
               CALL GTXS(TEXX,TEXY,LEN(TIMTXT),TIMTXT)

             END IF

           END IF

         END IF
 
C SET THE OLD TIME TO THE NEW TIME
                  
       OLDET = NEWET

C INCREMENT CVRG REC COUNTER

       J = J + 1

C NEXT CVRG RECORD

       GO TO 2000


C END OF FILE
 3000  CONTINUE
       IF (TOTAL .LE. 1) THEN 
         CALL DISMSG('No Coverage records found.')
         GO TO 7777
       ELSE IF (ERRFLAG .NE. 0) THEN
         CALL DISMSG('Error: Invalid Coverage record found.')
         GO TO 7777
       ELSE
         GO TO 7777
       END IF

C ERROR ON READING FILE
 4000  CONTINUE
       CALL DISMSG('Error retrieving Coverage records.')

C CLOSE THE FILE
 7777  CONTINUE
       CLOSE (10)

C CLOSE THE SEGMENT
 8888  CONTINUE
       CALL CLSSEG

 9999  CONTINUE
       RETURN
       END 
