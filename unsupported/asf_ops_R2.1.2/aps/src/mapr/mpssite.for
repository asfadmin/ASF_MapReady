C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	mpssite.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================

C-----------------------------------------------------------------------
C
C SUBROUTINE MPSSITE
C                              
C PURPOSE
C	TO DISPLAY A SITE ON THE MAP
C
C $Logfile:   ACS003:[BLD.MPS.MAPR.SRC]MPSSITE.QFV  $
C
C VARIABLES
C INPUT
C	WSID		WORKSTATION ID
C	PRON		PROJECTION NUMBER
C	OBSLAT,OBSLON	USER DEFINED CENTER LAT/LON
C	STMNX,STMXX	ORIGINAL MIN/MAX X
C	STMNY,STMXY	ORIGINAL MIN/MAX Y
C
C OUTPUT
C	NSEG		NUMBER OF SEGMENTS
C	CRSEGN		ARRAY OF SEG STATUS VALUES
C	CRSEGT		ARRAY OF SEGMENT NAMES
C
C INTERNAL
C	SEGNAM		NAME OF SEGMENT
C	SITENAME	<
C	VUTXT		TEXT TO BE DISPLAYED
C	STYPE		TYPE OF SITE
C	SLEN		STRING LENGTH
C	SRAD		RADIUS OF SITE
C	SLAT(),SLON()	LAT/LON POINTS OF SITE
C
C WRITTEN BY C.K. FUJIMOTO - 20-JUN-89
C
C $Date$ $Revision$ $Author$
C
C 7/8/94  Nadia Adhami -port to UNIX/C (INGRIS -> SYBASE)
C
C-----------------------------------------------------------------------
       SUBROUTINE MPSSITE (WSID,PROJN,
     1                   OBSLAT,OBSLON,
     2                   STMNX,STMXX,STMNY,STMXY,
     3                   NSEG,CRSEGN,CRSEGT)

      character*100 SccsFileID
     -/'@(#)mpssite.for	5.1 98/01/08 APS/ASF\0'/
     
       IMPLICIT NONE

C INPUT:

       INTEGER WSID,PROJN

       REAL OBSLAT,OBSLON
       REAL STMNX,STMXX,STMNY,STMXY

C OUTPUT:

       CHARACTER*(*) CRSEGT(*)
       
       INTEGER CRSEGN(*),NSEG

C INTERNAL:

       CHARACTER*20 SEGNAM
       CHARACTER*32 SITENAME
       CHARACTER*33 VUTXT

       INTEGER STYPE
       INTEGER SITELEN

       REAL SRAD,SLAT(5),SLON(5)
C-----------------------------------------------------------------------
                    

 1000  CONTINUE

C GET SITENAME

       CALL ASK_SITENAME(SITENAME)

       CALL TRIM(SITENAME, 32, SITELEN)

C TEXT TO VIEW
       VUTXT = ' ' // SITENAME(1:SITELEN) // char(0)

C THE NAME OF THE SEGMENT WILL BE THE SITENAME
       SEGNAM = 'ST ' // SITENAME(1:SITELEN) //char(0)

C RETRIEVE SITE FROM RELATION

       CALL GET_SITE(SITENAME,STYPE,SRAD,SLON,SLAT)

       IF (STYPE .EQ. -1) GO TO 9999

C DRAW SITE

       CALL SITEDISP (WSID,PROJN,
     1               OBSLAT,OBSLON,
     2               STMNX,STMXX,STMNY,STMXY,
     3               VUTXT,STYPE,SRAD,SLON,SLAT,
     4               NSEG,SEGNAM,CRSEGN,CRSEGT)


C END OF SUBROUTINE

 9999  CONTINUE
       RETURN 
       END



C-----------------------------------------------------------------------
C
C      SUBROUTINE GET_SITE
C
C PURPOSE
C	RETRIEVE THE SITE FROM THE SITE RELATION
C
C INPUT
C	SITENAME	<
C
C OUTPUT
C	STYPE		SITE TYPE P/Q/R
C	SRAD		SITE RADIUS FOR STYPE P
C	SLON,SLAT	SITE POINTS IN LAT/LON
C
C INTERNAL
C
C	SSHAPE		SITE SHAPE
C	I		COUNTER
C	CFLAG
C	RCOUNT	INGRES RECORD COUNT AND ERROR NUMBER
C	ST()		TEMP BUFFER FOR POINTS
C
C WRITTEN BY CRAIG K. FUJIMOTO - SEP 89
C
C-----------------------------------------------------------------------
       SUBROUTINE GET_SITE(SITENAME,STYPE,SRAD,SLON,SLAT)

       IMPLICIT NONE

       INCLUDE 'mapr_db_extern.inc'
       INCLUDE 'APS_ROOT:include/local/f77_db_site.inc'

C INPUT:
       CHARACTER*32 SITENAME

C OUTPUT:
       REAL SRAD,SLON(5),SLAT(5)
       INTEGER STYPE

C INTERNAL:
       CHARACTER*1 SSHAPE

       INTEGER I,CFLAG
       INTEGER LASTC
       INTEGER NRECS
	   INTEGER   LOCAL_DUMMY_VALUEHOLDER

C-----------------------------------------------------------------------
C FOLLOWING ASSIGNMENT REMOVES WARNING MESSAGE FOR UNUSED VARIABLE PTR
      INTEGER*4 TEMP
      DATA TEMP /10/
       PTRPTR = LOC (TEMP)
       LOCAL_DUMMY_VALUEHOLDER = PTR

C RETRIEVE THE SITE RECORD FROM THE DATABASE

c--port##     RETRIEVE (SSHAPE = SITE.#SHAPE,
c##               SRAD = SITE.#RADIUS,
c##               SLAT(1) = SITE.#NWLAT,SLON(1) = SITE.#NWLON,
c##               SLAT(2) = SITE.#NELAT,SLON(2) = SITE.#NELON,
c##               SLAT(3) = SITE.#SELAT,SLON(3) = SITE.#SELON,
c##               SLAT(4) = SITE.#SWLAT,SLON(4) = SITE.#SWLON)
c##       WHERE SITENAME = SITE.#SITENAME
c##     {
c##     }

       WRITE(WHERE_CLAUSE, 1) SITENAME(1:LASTC(SITENAME))
    1  FORMAT('where sitename = "',A,'"')
       WHERE_CLAUSE = WHERE_CLAUSE(1:LASTC(WHERE_CLAUSE))//char(0)
 
       llistptr = db_get_records(%VAL(MAPR_DBPROC), 'site'//char(0),
     ?   WHERE_CLAUSE, char(0),SITE_COLUMNS,%VAL(ALL_COLS))
       if ( llistptr .EQ. 0 ) THEN
         CALL DISMSG('Error: in query in site relation.')
         STYPE = -1
         GO TO 9999
       ENDIF 

       call get_no_elements(llist,NRECS)

       IF (NRECS .EQ. 0) THEN
           CALL DISMSG('Error: Could not find site in SITE relation.')
           STYPE = -1
           GO TO 9999
       END IF

       P2_DATA_RECORD = db_ftn_first_record(llist, ptrptr)

       call get_site_rec(%VAL(P2_DATA_RECORD),
     ?   SSHAPE,SRAD,
     ?   SLAT(1),SLON(1),SLAT(2),SLON(2),
     ?   SLAT(3),SLON(3),SLAT(4),SLON(4))

       IF (NRECS .NE. 0) THEN
         CALL db_ftn_free_llist(llist)
       ENDIF

C VERIFY THAT THE SITE DATA IS VALID

       IF (SLAT(1) .LT. -90.0 .OR. SLAT(1) .GT. 90.0 .OR.
     1     SLAT(2) .LT. -90.0 .OR. SLAT(2) .GT. 90.0 .OR.
     2     SLAT(3) .LT. -90.0 .OR. SLAT(3) .GT. 90.0 .OR.
     3     SLAT(4) .LT. -90.0 .OR. SLAT(4) .GT. 90.0 .OR.
     4     SLON(1) .LT. -180.0 .OR. SLON(1) .GT. 180.0 .OR.
     5     SLON(2) .LT. -180.0 .OR. SLON(2) .GT. 180.0 .OR.
     6     SLON(3) .LT. -180.0 .OR. SLON(3) .GT. 180.0 .OR.
     7     SLON(4) .LT. -180.0 .OR. SLON(4) .GT. 180.0 .OR.
     8     SRAD .LT. 0.0 .OR. SRAD .GT. 6378.0) THEN

         CALL DISMSG('Error: Invalid SITE record found.')
         STYPE = -1
         GO TO 9999

       END IF
           
C CHECK FOR TOO LARGE OF A SITE

       DO 1000 I = 1,4

         IF (I .LT. 4) THEN 
           CALL MPSCLOSE(SLAT(I),SLON(I),
     1                 SLAT(I+1),SLON(I+1),CFLAG)
         ELSE                     
           CALL MPSCLOSE(SLAT(I),SLON(I),
     1                 SLAT(I-3),SLON(I-3),CFLAG)
         END IF

         IF (CFLAG .EQ. -1) THEN 
           CALL DISMSG('Error : Invalid SITE record found.')
           STYPE = -1
           GO TO 9999
         END IF

 1000  CONTINUE


C CHECK FOR RECTANGLE / QUADRILATERAL

       IF (SSHAPE .EQ. 'R' .OR. SSHAPE .EQ. 'Q') THEN

         STYPE = 3

C CHECK FOR CIRCLE OR POINT

       ELSE IF (SSHAPE .EQ. 'P') THEN

         IF (SRAD .EQ. 0.0) THEN
           STYPE = 1
         ELSE IF (SRAD .GT. 0.0) THEN
           STYPE = 2
         ELSE
           CALL DISMSG('Error: Invalid SITE record found.')
           STYPE = -1
         END IF

       ELSE

C DON'T RECOGNIZE THE SHAPE - RETURN ERROR

         CALL DISMSG('Error: Invalid SITE record found.')
         STYPE = -1

       END IF

 9999  CONTINUE
       RETURN 
       END
