C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:  mpsdar.for
C--
C--  Description:   
C-- 
C--  Notes:
C--
C-- ==========================================================================

C-----------------------------------------------------------------------
C
C SUBROUTINE MPSDAR
C
C PURPOSE
C   DISPLAY A DATA AQUISITION REQUEST AREA FROM THE MPS DAR RELATION
C                               
C $Logfile:   ACS003:[BLD.MPS.MAPR.SRC]MPSDAR.QFV  $
C
C VARIABLES
C INPUT
C   WSID        WORKSTATION ID
C   PRON        PROJECTION NUMBER
C   OBSLAT,OBSLON   USER DEFINED CENTER LAT/LON
C   STMNX,STMXX ORIGINAL MIN/MAX X
C   STMNY,STMXY ORIGINAL MIN/MAX Y
C
C OUTPUT
C   NSEG        NUMBER OF SEGMENTS
C   CRSEGN      ARRAY OF SEG STATUS VALUES
C   CRSEGT      ARRAY OF SEGMENT NAMES
C
C INTERNAL
C   SEGNAM      NAME OF SEGMENT
C   VUTXT       TEXT TO BE DISPLAYED
C   DARTXT      DARID CONVERTED TO ASCII
C   DARID       <
C   DARTYPE     TYPE OF DAR
C   DARLEN      STRING LENGTH
C   DARRAD      RADIUS OF SITE OF DAR
C   DARLAT,DARLON   LAT/LON POINTS OF DAR
C
C WRITTEN BY CRAIG K. FUJIMOTO - 20-JUN-89
C
C MODIFICATIONS
C $Date$ $Revision$ $Author$
C 7/8/94  Nadia Adhami -port to UNIX/C (INGRIS -> SYBASE)
C 7/8/94  Nadia Adhami -port to UNIX/C (rename GET_DAR() -> GET_DAR_MAPR())
C
C-----------------------------------------------------------------------
       SUBROUTINE MPSDAR (WSID,PROJN,
     1                   OBSLAT,OBSLON,
     2                   STMNX,STMXX,STMNY,STMXY,
     3                   NSEG,CRSEGN,CRSEGT)

      character*100 SccsFileID
     -/'@(#)mpsdar.for	5.1 98/01/08 APS/ASF\0'/
     
       IMPLICIT NONE

C INPUT:
       INTEGER WSID,PROJN

       REAL OBSLAT,OBSLON
       REAL STMNX,STMXX,STMNY,STMXY


C OUTPUT:
       CHARACTER*(*) CRSEGT(*)
       
       INTEGER CRSEGN(*),NSEG

C INTERNAL:
       CHARACTER*10 DARTXT
       CHARACTER*11 VUTXT
       CHARACTER*20 SEGNAM

       INTEGER DARLEN
       INTEGER DARID,DARTYPE
       REAL DARRAD,DARLAT(5),DARLON(5)


C-----------------------------------------------------------------------


 1000  CONTINUE

C GET DARID

       CALL ASK_DARID(DARID)
       IF(DARID .EQ. 0) GO TO 9999

C THE NAME OF THE SEGMENT WILL BE THE DARID

       ENCODE (10,'(I10)',DARTXT) DARID

       CALL TRIM(DARTXT,10,DARLEN)

C TEXT TO VIEW
       VUTXT = ' ' // DARTXT(1:DARLEN)

C TEXT TO ASSOCIATE WITH SEGMENT
       SEGNAM = 'DAR ' // DARTXT(1:DARLEN)


C RETRIEVE DAR FROM RELATION
                          
       CALL GET_DAR_MAPR(DARID,DARTYPE,DARRAD,DARLAT,DARLON)

       IF (DARTYPE .EQ. -1) GO TO 9999

C DRAW DAR

       CALL SITEDISP (WSID,PROJN,
     1               OBSLAT,OBSLON,
     2               STMNX,STMXX,STMNY,STMXY,
     3               VUTXT,DARTYPE,DARRAD,DARLON,DARLAT,
     4               NSEG,SEGNAM,CRSEGN,CRSEGT)


C END OF SUBROUTINE

 9999  CONTINUE
       RETURN 
       END

               

C-----------------------------------------------------------------------
C
C SUBROUTINE GET_DAR
C
C PURPOSE
C   RETRIEVE DAR FROM MPS DAR RELATION
C
C INPUT
C   DARID       <
C
C OUTPUT
C   DARTYPE     TYPE OF DAR
C           1 = POINT
C           2 = CIRCLE
C           3 = RECTANGLE/QUADRILATERAL
C           -1 = ERROR
C   DARRAD      RADIUS OF POINT/CIRLE DAR
C   DARLAT,DARLON   LAT/LON POINTS OF DAR
C
C INTERNAL
C   DARSHAPE    SHAPE OF DAR P/Q/R
C   RCOUNT,ERRNO    database ROW COUNT AND ERROR NUMBER
C   I       COUNTER
C   CFLAG           MPSCLOSE ERROR FLAG
C
C WRITTEN BY CRAIG K. FUJIMOTO - SEP 89
C
C-----------------------------------------------------------------------
       SUBROUTINE GET_DAR_MAPR(DARID,DARTYPE,DARRAD,DARLAT,DARLON)

       IMPLICIT NONE

       INCLUDE 'mapr_db_extern.inc'
       INCLUDE 'APS_ROOT:include/local/f77_db_dar.inc'

c--port-##     DECLARE

C INPUT:
       INTEGER DARID

C OUTPUT
       INTEGER DARTYPE

       REAL DARRAD,DARLAT(5),DARLON(5)

C INTERNAL:
       CHARACTER*1 DARSHAPE

       INTEGER NRECS
       INTEGER ERRNO
       INTEGER I,CFLAG

       INTEGER LASTC
       INTEGER   LOCAL_DUMMY_VALUEHOLDER

C-----------------------------------------------------------------------
C FOLLOWING ASSIGNMENT REMOVES WARNING MESSAGE FOR UNUSED VARIABLE PTR
      INTEGER*4 TEMP
      DATA TEMP /10/
       PTRPTR = LOC (TEMP)
       LOCAL_DUMMY_VALUEHOLDER = PTR

C RETRIEVE THE DAR RECORD FROM THE DATABASE

c--port-##     RETRIEVE (DARSHAPE = DAR.#SHAPE,
C##               DARRAD = DAR.#RADIUS,
C##               DARLAT(1) = DAR.#NWLAT,DARLON(1) = DAR.#NWLON,
C##               DARLAT(2) = DAR.#NELAT,DARLON(2) = DAR.#NELON,
C##               DARLAT(3) = DAR.#SELAT,DARLON(3) = DAR.#SELON,
C##               DARLAT(4) = DAR.#SWLAT,DARLON(4) = DAR.#SWLON)
C##       WHERE DARID = DAR.#DARID
C##     {
C##     }

       WRITE(WHERE_CLAUSE, 1) DARID
    1  FORMAT('where darid = ',I)
       WHERE_CLAUSE = WHERE_CLAUSE(1:LASTC(WHERE_CLAUSE)) // char(0)

       WRITE (6,100) MAPR_DBPROC
  100  FORMAT (/,' MAPR_DBPROC = ', I)
 

       llistptr = db_get_records(%VAL(MAPR_DBPROC), 'dar'//char(0),
     ?   WHERE_CLAUSE, char(0), DAR_COLUMNS,%VAL(ALL_COLS))
C---      check for db error, set flag:
       IF ( llistptr .EQ. 0 ) THEN
         ERRNO = 1
         CALL DISMSG('Error retrieving DAR record.')
         DARTYPE = -1
         GO TO 9999
       END IF

       call get_no_elements(llist,NRECS)

       IF (NRECS .EQ. 0) THEN
         CALL db_ftn_free_llist(llist)
         CALL DISMSG('Error : Could not find DAR in DAR Relation.')
         DARTYPE = -1
         GO TO 9999
       END IF

       P2_DATA_RECORD = db_ftn_first_record(llist, ptrptr)

       call get_dar_rec(%VAL(P2_DATA_RECORD),
     ?   DARSHAPE,DARRAD,
     ?   DARLAT(1),DARLON(1),DARLAT(2),DARLON(2),
     ?   DARLAT(3),DARLON(3),DARLAT(4),DARLON(4))

C--   The memory for the list can now be freed.  
      CALL db_ftn_free_llist(llist)

C VERIFY THAT THE SITE DATA IS VALID
                      
       IF (DARLAT(1).LT. -90.0 .OR. DARLAT(1).GT. 90.0 .OR.
     1     DARLAT(2).LT. -90.0 .OR. DARLAT(2).GT. 90.0 .OR.
     2     DARLAT(3).LT. -90.0 .OR. DARLAT(3).GT. 90.0 .OR.
     3     DARLAT(4).LT. -90.0 .OR. DARLAT(4).GT. 90.0 .OR.
     4     DARLON(1).LT. -180.0 .OR. DARLON(1).GT. 180.0 .OR.
     5     DARLON(2).LT. -180.0 .OR. DARLON(2).GT. 180.0 .OR.
     6     DARLON(3).LT. -180.0 .OR. DARLON(3).GT. 180.0 .OR.
     7     DARLON(4).LT. -180.0 .OR. DARLON(4).GT. 180.0 .OR.
     8     DARRAD .LT. 0.0 .OR. DARRAD .GT. 6378.0) THEN

         CALL DISMSG('Error : Invalid DAR record found.')
         DARTYPE = -1
         GO TO 9999

       END IF

C CHECK FOR TOO LARGE OF A DAR

       DO 1000 I = 1,4

         IF (I .LT. 4) THEN 
           CALL MPSCLOSE(DARLAT(I),DARLON(I),
     1                 DARLAT(I+1),DARLON(I+1),CFLAG)
         ELSE
           CALL MPSCLOSE(DARLAT(I),DARLON(I),
     1                 DARLAT(I-3),DARLON(I-3),CFLAG)
         END IF

         IF (CFLAG .EQ. -1) THEN 
           CALL DISMSG('Error : Invalid DAR record found.')
           DARTYPE = -1
           GO TO 9999
         END IF

 1000  CONTINUE


C CHECK FOR QUADRILATERAL OR RECTANGLE

       IF (DARSHAPE .EQ. 'Q' .OR. DARSHAPE .EQ. 'R') THEN

         DARTYPE = 3

C CHECK FOR CIRCLE OR POINT

       ELSE IF (DARSHAPE .EQ. 'P') THEN

         IF (DARRAD .EQ. 0.0) THEN
           DARTYPE = 1
         ELSE IF (DARRAD .GT. 0.0) THEN
           DARTYPE = 2
         ELSE
           CALL DISMSG('Error : Invalid DAR record found.')
           DARTYPE = -1
         END IF

       ELSE

C DON'T RECOGNIZE THE SHAPE - RETURN ERROR

         CALL DISMSG('Error : Invalid DAR record found.')
         DARTYPE = -1

       END IF                    


 9999  CONTINUE
       RETURN 
       END



C-----------------------------------------------------------------------
C
C SUBROUTINE SITEDISP                      
C        
C PURPOSE
C   DISPLAY A SITE OR DAR IN THE WINDOW AS WELL AS TEXT
C   IDENTIFYING THE RECORD
C
C VARIABLES             
C INPUT
C   PROJN       PROJECTION TYPE
C   OBSLON      CENTERING LATITUDE OF MAP (DEG)
C   OBSLAT      CENTERING LONGITUDE OF MAP (DEG)
C   WSID        WORKSTATION ID NUMBER
C   SEGNAM      SITE OVERLAY NAME
C   VUTXT       TEXT TO DISPLAY BY SITE
C   STYPE       SITE TYPE
C           1 = POINT
C           2 = CIRCLE
C           3 = QUADRILATERAL
C   STMNX,STMXX MIN/MAX X OF WINDOW
C   STMNY,STMXY MIN/MAX Y OF WINDOW
C   SRAD        RADIUS OF SITE
C   SLAT,SLON   LAT/LONS OF SITE
C
C OUTPUT
C   NSEG        SEG COUNTER
C   CRSEGN      ARRAY OF SEGMENT STATUS VALUES
C   CRSEGT      ARRAY OF SEGMENT NAMES
c
C INTERNAL
C   NPTS        COUNTER - NUMBER OF POINTS FOR SITE
C   HIDDEN      LOGICAL INDICATING HIDDEN POINT
C   BRNCUT      LOGICAL INDICATING BREAK BETWEEN POINTS
C   SX,SY       X/Y COORDINATE POINTS
C   OLDX,OLDY   PREVIOUS X/Y COORD POINTS
C   DEG     DEGREES
C   RAD     RADIANS
C   PI      <
C   STLAT,STLON LAT/LON POINTS FOR SITE
C   LATS/LONS   INTERPOLATED POINTS
C   ANGLE       ANGLE FOR CALCULATING CIRCLE SITE
C   TEXLAT,TEXLON   LAT/LON OF TEXT
C   TEXX,TEXY   X/Y OF TEXT
C   HIDDEN      INDICATES IF POINT IS HIDDEN
C   BRNCUT      INDICATES BORDER CUT
C   OLDHID      HIDDEN FLAG FROM PREVIOUS POINT
C   TEXHID      HIDDEN FLAG FOR TEXT POINT
C   I,J,K,N     COUNTERS
C
C SUBROUTINE CALLS
C   CRTSEG      INITIATES THE CREATION OF THE SITE SEGMENT.
C   LTRANS      TRANSFORMATION FROM LAT-LON TO X-Y COORDINATES.
C   CLSSEG      CLOSES THE SITE SEGMENT.
C
C
C ORIGINAL WRITTEN BY RICHARD LEE (DISIT)
C REWORKED BY C.K. FUJIMOTO - 20-JUN-89
C   - ADAPTED FROM DISIT FOR MPS SITES AND DARS
C
C-----------------------------------------------------------------------

       SUBROUTINE SITEDISP (WSID,PROJN,
     1                    OBSLAT,OBSLON,     
     2                    STMNX,STMXX,STMNY,STMXY,
     3                    VUTXT,STYPE,SRAD,SLON,SLAT,
     4                    NSEG,SEGNAM,CRSEGN,CRSEGT)

       IMPLICIT NONE

C INPUT:
c--port-       INCLUDE 'SYS$LIBRARY:GKSDEFS.BND'
       INCLUDE 'GKS_ROOT:include/fgksenum.inc'
       INCLUDE 'mapper_port.inc'


       CHARACTER*(*) VUTXT,SEGNAM

       INTEGER PROJN,WSID,STYPE

       REAL OBSLON,OBSLAT
       REAL STMNX,STMXX,STMNY,STMXY
       REAL SRAD,SLAT(5),SLON(5)

C OUTPUT
       CHARACTER*(*) CRSEGT(*)

       INTEGER NSEG,CRSEGN(*)

C INTERNAL:
       INTEGER I,J,K,N
                            
       REAL SX,SY,OLDX,OLDY,
     2      PI,
     3      STLAT(1630),STLON(1600),LATS(500),LONS(500)

       REAL TEXX,TEXY

       DATA PI / 3.141592653589793 /

       LOGICAL HIDDEN,BRNCUT,OLDHID,TEXHID

       INTEGER slen


C FUNCTIONS

       INTEGER LASTC
       REAL  GET_DEF_CH

C-----------------------------------------------------------------------

C GRAPHICS PARAMETERS

C TEXT HEIGHT
       CALL GSCHH (GET_DEF_CH())

C TEXT ALIGNMENT
       CALL GSTXAL (GALEFT,GAHALF)
C POLYMARKER SCALE
       CALL GSMKSC (0.05)
C POLYMARKER TYPE - ASTERISK
       CALL GSMK (GAST)

C TEXT COLOR INDEX
       CALL GSTXCI (6)
C POLYMARKER COLOR INDEX
       CALL GSPMCI (6)
C POLYLINE COLOR INDEX
       CALL GSPLCI (6)


C CREATE THE SEGMENT

       WRITE(6,100) SEGNAM
  100  FORMAT(/,1X,'Creating overlay ',A<SLEN(SEGNAM)>,'...',$)

       CALL CRTSEG (WSID,NSEG,SEGNAM,CRSEGN,CRSEGT)

C IF POINT SITE
           
       IF (STYPE .EQ. 1) THEN
              
         CALL LTRANS (PROJN,OBSLAT,OBSLON,
     1                SLON(1),SLAT(1),
     2                SX,SY,
     3                HIDDEN,BRNCUT)
                                            
         IF (.NOT. HIDDEN) THEN           

           CALL GPM (1,SX,SY)

         END IF

         TEXX = SX
         TEXY = SY
         TEXHID = HIDDEN

C IF CIRCULAR SITE

       ELSE IF (STYPE .EQ. 2) THEN

C GET LATS/LONS AROUND CIRCULAR SITE
         CALL MPS_CGEN(SLAT(1),SLON(1),SRAD,721,STLAT,STLON)

         DO 1000 J = 1,721

           CALL LTRANS (PROJN,OBSLAT,OBSLON,
     1                  STLON(J),STLAT(J),
     2                  SX,SY,
     3                  HIDDEN,BRNCUT)

           IF (J .NE. 1) THEN
     
             CALL DIS_LINE(PROJN,
     1                   OLDX,OLDY,SX,SY,
     2                   OLDHID,HIDDEN,
     3                   STMNX,STMXX,STMNY,STMXY)

C THE FIRST POINT IS THE TEXT POINT
           ELSE
             TEXX = SX
             TEXY = SY
           END IF

           OLDX = SX
           OLDY = SY
           OLDHID = HIDDEN

 1000    CONTINUE

C IF POLYGON SITE

       ELSE IF (STYPE .EQ. 3) THEN

         SLAT(5) = SLAT(1)         
         SLON(5) = SLON(1)

         K = 1

C LOOP THROUGH THE FOUR POINTS

         DO 1500 I = 1,4

C COMPUTE THE DISTANCE BETWEEN THE POINTS
           CALL LLDIST(SLAT(I),SLON(I),SLAT(I+1),SLON(I+1),N)

C ADD 2 FOR END POINTS
           N = N + 2

C INTERPOLATE FOR N POINTS
           CALL MPSINTRP(SLAT(I),SLON(I),
     1                   SLAT(I+1),SLON(I+1),N,LATS,LONS)
                                                    
C SAVE INTERPOLATED POINTS INTO ARRAY
           DO 1600 J = 1,N
             STLAT(K) = LATS(J)
             STLON(K) = LONS(J)
             K = K+1
 1600      CONTINUE

 1500    CONTINUE


C TRANSLATE THE LAT/LONS AND DRAW THE LINES

         DO 1900 J = 1,K-1

           CALL LTRANS (PROJN,OBSLAT,OBSLON,
     1                  STLON(J),STLAT(J),
     2                  SX,SY,
     3                  HIDDEN,BRNCUT)          

           IF (J .NE. 1) THEN

             CALL DIS_LINE(PROJN,
     1                   OLDX,OLDY,SX,SY,
     2                   OLDHID,HIDDEN,
     3                   STMNX,STMXX,STMNY,STMXY)

           END IF

           OLDX = SX
           OLDY = SY
           OLDHID = HIDDEN

C FIND THE RIGHTMOST, LOWEST POINT TO PLACE THE TEXT AT

           IF (J.EQ.1 .OR. (SX.GE.TEXX .AND. SY.LE.TEXY)) THEN
                                                             
             TEXX = SX
             TEXY = SY
             TEXHID = HIDDEN

           END IF       

 1900    CONTINUE

       END IF


C IF NOT HIDDEN, DISPLAY THE TEXT

       VUTXT = VUTXT(1:LASTC(VUTXT)) // char(0)

       IF (.NOT. TEXHID) THEN

C        SAVE CURRENT TEXT FONT & PRECISION
C        CALL GQTXFP (ERRIND, FONT, PREC)

C        SET TEXT FONT & PRECISION
C        CALL GSTXFP (-2604, GSTRP)

         CALL GTXS (TEXX,TEXY,LEN(VUTXT),VUTXT)

C         SET TEXT FONT & PRECISION
C         CALL GSTXFP (FONT, PREC)

       END IF


C CLOSE SITE SEGMENT

       calL CLSSEG

 9999  CONTINUE
       RETURN
       END


C-----------------------------------------------------------------------
C
C SUBROUTINE LLDIST
C
C PURPOSE
C   COMPUTES DISTANCE BETWEEN TWO LAT.LON POINTS.
C   ROUNDS THE NUMBER UP AND RETURNS INTEGER
C
C INPUT
C   LAT1,LON1   FIRST POINT
C   LAT2,LON2   SECOND POINT
C
C OUTPUT
C   N       DISTANCE BETWEEN 2 POINTS
C
C INTERNAL
C
C-----------------------------------------------------------------------
       SUBROUTINE LLDIST(LAT1,LON1,LAT2,LON2,N)

       IMPLICIT NONE

C INPUT
       REAL LAT1,LON1,LAT2,LON2
C OUTPUT
       INTEGER N

       N = INT( SQRT( (LAT2-LAT1)**2 + (LON2-LON1)**2 ) + 1.0 )

 9999  CONTINUE
       RETURN
       END
