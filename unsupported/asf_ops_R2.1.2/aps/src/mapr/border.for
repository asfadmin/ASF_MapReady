C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	border.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================

C-----------------------------------------------------------------
C SUBROUTINE BORDER
C
C PURPOSE
C	DISPLAY A BORDER AROUND THE MAP AND DISPLAY THE GRID NUMBERS
C
C $LOGFILE$
C
C INPUT
C	PROJN		PROJECTION TYPE TO DISPLAY MAP
C	MINX,MAXX,
C	MINY,MAXY	MIN/MAX X/Y COORDS
C	MINLAT,MAXLAT	MIN/MAX LAT OF WINDOW
C	OBSLAT,OBSLON	CENTER LAT/LON
C	OBSMIN,OBSMAX	MIN/MAX LON OF WINDOW
C	LATDEG(),LONDEG()	LAT/LON GRID DIVISION ARRAYS
C	LTNDEG,LNNDEG	NUMBER OF LAT/LON GRID DIVISIONS
C
C INTERNAL
C	LATTXT(),LONTXT()	LAT/LON TEXT ARRAYS
C
C SUBROUTINES CALLED
C	NUMTXT
C	PRNDEG
C
C QRIGINALLY WRITTEN BY RICHARD P. LEE
C MODIFIED FOR ASF BY CRAIG K. FUJIMOTO - SEP 89
C
C MODIFICATIONS
C $DATE$ $REVISION$ $AUTHOR$
C 7/11/94  Nadia Adhami -port to UNIX/C (include gks header)
C
C-----------------------------------------------------------------
       SUBROUTINE BORDER (PROJN,
     1                    MINX,MAXX,MINY,MAXY,
     3                    MINLAT,MAXLAT,MINLON,MAXLON,
     4                    OBSLAT,OBSLON,OBSMIN,OBSMAX,
     6                    LATDEG,LONDEG,
     7                    LTNDEG,LNNDEG)

      character*100 SccsFileID
     -/'@(#)border.for	5.1 98/01/08 APS/ASF\0'/

       IMPLICIT NONE

C INPUT:
       INTEGER PROJN,LTNDEG,LNNDEG

       REAL MINX,MAXX,MINY,MAXY,
     2      MINLAT,MAXLAT,MINLON,MAXLON,
     4      OBSLAT,OBSLON,OBSMIN,OBSMAX,
     5      LATDEG(*),LONDEG(*)

C INTERNAL:
       CHARACTER*3 LATTXT(25)
       CHARACTER*4 LONTXT(25)

C-----------------------------------------------------------------------

C IF CYLINDRICAL EQUIDISTANT, MERCATOR, OR MILLER CYLINDRICAL

       IF (PROJN .EQ. 2 .OR. PROJN .EQ. 3 .OR. PROJN .EQ. 4) THEN

C CONVERT GRID NUMBERS FROM NUMBERS TO TEXT

         CALL NUMTXT (LATDEG,LONDEG,
     1                LTNDEG,LNNDEG,
     2                LATTXT,LONTXT)

C DISPLAY GRID NUMBERS

         CALL PRNDEG (PROJN,
     1                OBSLAT,OBSLON,
     2                MINLAT,MAXLAT,
     3                OBSMIN,OBSMAX,
     4                LATDEG,LONDEG,
     5                LATTXT,LONTXT,
     6                LTNDEG,LNNDEG,
     7                MINX,MAXX,MINY,MAXY)

       END IF   

C END OF SUBROUTINE

 9999  RETURN
       END


C-----------------------------------------------------------------
C SUBROUTINE NUMTXT
C
C PURPOSE
C	CONVERT GRID NUMBERS TO ASCII TEXT
C
C INPUT
C	LATDEG,LONDEG	LAT/LON GRID DIVISION ARRAYS
C	LTNDEG,LNNDEG	LAT/LON GRID DIVISION COUNTERS
C
C OUTPUT
C	LATTXT,LONTXT	LAT/LON GRID TEXT ARRAYS
C
C INTERNAL
C	I		LOOP COUNTER
C
C SUBROUTINES CALLED
C
C QRIGINALLY WRITTEN BY RICHARD P. LEE
C MODIFIED FOR ASF BY CRAIG K. FUJIMOTO - SEP 89
C
C-----------------------------------------------------------------
       SUBROUTINE NUMTXT (LATDEG,LONDEG,
     1                    LTNDEG,LNNDEG,
     2                    LATTXT,LONTXT)

       IMPLICIT NONE

C INPUT:
       INTEGER LTNDEG,LNNDEG
       REAL LATDEG(*),LONDEG(*)

C OUTPUT:
       CHARACTER*3 LATTXT(*)
       CHARACTER*4 LONTXT(*)

C INTERNAL:
       INTEGER I

C-----------------------------------------------------------------------

C CONVERT THE LATITUDE GRID NUMBERS

       DO 10 I = 1,LTNDEG
         ENCODE (3,'(I3)',LATTXT(I)) NINT(LATDEG(I))
   10  CONTINUE

C CONVERT THE LONGITUDE GRID NUMBERS
 
       DO 20 I = 1,LNNDEG
         ENCODE (4,'(I4)',LONTXT(I)) NINT(LONDEG(I))
         IF (NINT(LONDEG(I)) .GE. 0 .AND. NINT(LONDEG(I)) .LT. 10) 
     1     LONTXT(I) = LONTXT(I)(3:4)//'  '
         IF (NINT(LONDEG(I)) .GE. 10 .OR. (NINT(LONDEG(I)) .LT. 0 
     1       .AND. NINT(LONDEG(I)) .GT. -100)) 
     2      LONTXT(I) = LONTXT(I)(2:4)//' ' 
   20  CONTINUE

C END OF SUBROUTINE

 9999  CONTINUE                
       RETURN                                        
       END            
                    


C-----------------------------------------------------------------
C SUBROUTINE PRNDEG
C
C PURPOSE
C	DISPLAY GRID NUMBERS
C
C INPUT
C	PROJN		PROJECTION TYPE TO DISPLAY MAP
C	MINX,MAXX,
C	MINY,MAXY	MIN/MAX X/Y COORDS
C	MINLAT,MAXLAT	MIN/MAX LAT OF WINDOW
C	OBSLAT,OBSLON	CENTER LAT/LON
C	OBSMIN,OBSMAX	MIN/MAX LON OF WINDOW
C	LATDEG(),LONDEG()	LAT/LON GRID DIVISION ARRAYS
C	LTNDEG,LNNDEG	NUMBER OF LAT/LON GRID DIVISIONS
C	LATTXT(),LONTXT()	LAT/LON TEXT ARRAYS
C
C INTERNAL
C	LATLN,LONLN	TEMPS FOR LAT/LON BOUNDARIES
C	SX,SY		X/Y COORD OF LAT/LON
C	DIFX,DIFY	WIDTH,HEIGHT OF WINDOW
C	HIDDEN,BRNCUT	FLAGS RETURNED FROM LTRANS
C	I,J		LOOP COUNTERS
C
C SUBROUTINES CALLED
C	LTRANS
C
C QRIGINALLY WRITTEN BY RICHARD P. LEE
C MODIFIED FOR ASF BY CRAIG K. FUJIMOTO - SEP 89
C
C-----------------------------------------------------------------
       SUBROUTINE PRNDEG (PROJN,
     1                    OBSLAT,OBSLON,
     2                    MINLAT,MAXLAT,MINLON,MAXLON,
     3                    LATDEG,LONDEG,
     4                    LATTXT,LONTXT,
     5                    LTNDEG,LNNDEG,
     6                    MINX,MAXX,MINY,MAXY)

       IMPLICIT NONE

C INPUT:
c--port       INCLUDE 'SYS$LIBRARY:GKSDEFS.BND'
       INCLUDE 'GKS_ROOT:include/fgksenum.inc'
       INCLUDE 'mapper_port.inc'

       CHARACTER*(*) LATTXT(*),LONTXT(*)

       INTEGER PROJN,LTNDEG,LNNDEG

       REAL OBSLAT,OBSLON,
     1      MINLAT,MAXLAT,MINLON,MAXLON,
     2      LATDEG(*),LONDEG(*),
     3      MINX,MAXX,MINY,MAXY

C INTERNAL:
       INTEGER I,J

       REAL LATLN,LONLN,SX,SY,DIFX,DIFY

       LOGICAL HIDDEN,BRNCUT

       REAL GET_DEF_CH

C-----------------------------------------------------------------------

C TEXT COLOR INDEX FOR THE GRID NUMBERS - WHITE
       CALL GSTXCI (1)

C SET CHARACTER HEIGHT TO SCALE WITH THE ZOOM WINDOW

       CALL GSCHH (GET_DEF_CH())

C WINDOW HEIGHT AND LENGTH

       DIFX = MAXX - MINX
       DIFY = MAXY - MINY

C DISPLAY LATITUDE GRID NUMBERS

       DO 20 I = 1,2

         IF (I .EQ. 1) LONLN = MINLON
         IF (I .EQ. 2) LONLN = MAXLON

C   CALCULATE X-Y POSITION OF THE LATITUDE GRID NUMBERS
                     
         DO 10 J = 1,LTNDEG

           CALL LTRANS (PROJN,OBSLAT,OBSLON,
     1                  LONLN,LATDEG(J),SX,SY,HIDDEN,BRNCUT)

C   ALIGN AND DISPLAY LATITUDE GRID NUMBERS

           IF (I .EQ. 1) THEN

             IF (LATDEG(J) .EQ. MINLAT) THEN
               CALL GSTXAL (GARITE,GABASE)
             ELSE IF (LATDEG(J) .EQ. MAXLAT) THEN
               CALL GSTXAL (GARITE,GACAP)
             ELSE
               CALL GSTXAL (GARITE,GAHALF)
             END IF

             SX = (SX - (DIFX / 100.0))
             SY = SY

             CALL GTXS (SX,SY,3,LATTXT(J))

           ELSE IF (I .EQ. 2) THEN

             IF (LATDEG(J) .EQ. MINLAT) THEN
                 CALL GSTXAL (GALEFT,GABASE)
             ELSE IF (LATDEG(J) .EQ. MAXLAT) THEN
               CALL GSTXAL (GALEFT,GACAP)
             ELSE
               CALL GSTXAL (GALEFT,GAHALF)
             END IF

             SX = (SX + (DIFX / 100.0))
             SY = SY

             CALL GTXS (SX,SY,3,LATTXT(J))

           END IF
   10    CONTINUE
   20  CONTINUE

C DISPLAY LONGIITUDE GRID NUMBERS

       DO 40 I = 1,2
         IF (I .EQ. 1) LATLN = MINLAT
         IF (I .EQ. 2) LATLN = MAXLAT

C   CALCULATE X-Y POSITION OF THE LONGITUDE GRID NUMBERS

         DO 30 J = 1,LNNDEG

           CALL LTRANS (PROJN,OBSLAT,OBSLON,
     1                  LONDEG(J),LATLN,SX,SY,HIDDEN,BRNCUT)
     
     
C   ALIGN AND DISPLAY LATITUDE GRID NUMBERS

           IF (I .EQ. 1) THEN
             CALL GSTXAL (GACENT,GACAP)
             SX = SX
             SY = (SY - (DIFY / 50.0))
             CALL GTXS (SX,SY,4,LONTXT(J))
           ELSE IF (I .EQ. 2) THEN
             CALL GSTXAL (GACENT,GABASE)
             SX = SX
             SY = (SY + (DIFY / 50.0))
             CALL GTXS (SX,SY,4,LONTXT(J))
           END IF
   30    CONTINUE
   40  CONTINUE

C END OF SUBROUTINE

 9999  CONTINUE
       RETURN
       END
