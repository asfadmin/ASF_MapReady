C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	grid_text.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================

C-----------------------------------------------------------------
C SUBROUTINE GRID_TEXT
C
C PURPOSE
C	DISPLAY A BORDER AROUND THE MAP AND DISPLAY THE GRID NUMBERS
C
C $Logfile:   ACS003:[BLD.MPS.MAPR.SRC]GRID_TEXT.FOV  $
C
C INPUT
C	PROJN		PROJECTION TYPE TO DISPLAY MAP
C	MINX,MAXX,
C	MINY,MAXY	MIN/MAX X/Y COORDS
C	MINLAT,MAXLAT,
C	MINLON,MAXLON	MIN/MAX LAT/LON OF WINDOW
C	OBSLAT,OBSLON	CENTER LAT/LON
C	OBSMIN,OBSMAX	MIN/MAX LON OF WINDOW
C	DEG(2,25)	LON/LAT GRID DIVISIONS
C	NDEG(2)		NUMBER OF LON/LAT GRID DIVISIONS
C
C INTERNAL
C	LATTXT(),LONTXT()	LAT/LON TEXT ARRAYS
C
C SUBROUTINES CALLED
C	GRID_NUMTXT
C	GRID_DISTXT
C
C QRIGINALLY WRITTEN BY RICHARD P. LEE
C MODIFIED FOR ASF BY CRAIG K. FUJIMOTO - SEP 89
C
C MODIFICATIONS
C $Date$ $Revision$ $Author$
C-----------------------------------------------------------------
       SUBROUTINE GRID_TEXT (PROJN,
     1                       MINX,MAXX,MINY,MAXY,
     2                       MINLAT,MAXLAT,MINLON,MAXLON,
     3                       OBSLAT,OBSLON,OBSMIN,OBSMAX,
     4                       DEG,NDEG)

      character*100 SccsFileID
     -/'@(#)grid_text.for	5.1 98/01/08 APS/ASF\0'/

       IMPLICIT NONE

C INPUT:
       INTEGER PROJN,NDEG(2)

       REAL MINX,MAXX,MINY,MAXY,
     2      MINLAT,MAXLAT,MINLON,MAXLON,
     4      OBSLAT,OBSLON,OBSMIN,OBSMAX,
     5      DEG(2,25)

C INTERNAL:
       CHARACTER*3 LATTXT(25)
       CHARACTER*4 LONTXT(25)

C-----------------------------------------------------------------------

C CONVERT GRID NUMBERS FROM NUMBERS TO TEXT

      CALL GRID_NUMTXT (DEG,NDEG,LATTXT,LONTXT)

C DISPLAY GRID NUMBERS

      CALL GRID_DISTXT (PROJN,
     1                  OBSLAT,OBSLON,
     2                  MINLAT,MAXLAT,
     3                  OBSMIN,OBSMAX,
     4                  MINX,MAXX,MINY,MAXY,
     5                  DEG,NDEG,
     6                  LATTXT,LONTXT)

 9999  RETURN
       END


C-----------------------------------------------------------------
C SUBROUTINE GRID_NUMTXT
C
C PURPOSE
C	CONVERT GRID NUMBERS TO ASCII TEXT
C
C INPUT
C	DEG(2,25)	LON/LAT GRID DIVISIONS
C	NDEG(2)		NUMBER OF LON/LAT GRID DIVISIONS
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
       SUBROUTINE GRID_NUMTXT (DEG,NDEG,LATTXT,LONTXT)

       IMPLICIT NONE

C INPUT:
       REAL DEG(2,25)
       INTEGER NDEG(2)

C OUTPUT:
       CHARACTER*3 LATTXT(*)
       CHARACTER*4 LONTXT(*)

C INTERNAL:
       INTEGER I,IDEG

C-----------------------------------------------------------------------

C CONVERT THE LATITUDE GRID NUMBERS

       DO 10 I = 1,NDEG(2)

         ENCODE (3,'(I3)',LATTXT(I)) NINT(DEG(2,I))

   10  CONTINUE

C CONVERT THE LONGITUDE GRID NUMBERS
 
       DO 20 I = 1,NDEG(1)

         IDEG = NINT(DEG(1,I))

         ENCODE (4,'(I4)',LONTXT(I)) IDEG

         IF (IDEG .GE. 0 .AND. IDEG .LT. 10) 
     1     LONTXT(I) = LONTXT(I)(3:4)//'  '
         IF (IDEG .GE. 10 .OR. (IDEG .LT. 0 .AND. IDEG .GT. -100)) 
     2      LONTXT(I) = LONTXT(I)(2:4)//' ' 

   20  CONTINUE

 9999  CONTINUE                
       RETURN                                        
       END            
                    


C-----------------------------------------------------------------
C SUBROUTINE GRID_DISTXT
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
C	DEG(2,25)	LON/LAT GRID DIVISIONS
C	NDEG(2)		NUMBER OF LON/LAT GRID DIVISIONS
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
      SUBROUTINE GRID_DISTXT (PROJN,
     1                        OBSLAT,OBSLON,
     2                        MINLAT,MAXLAT,MINLON,MAXLON,
     3                        MINX,MAXX,MINY,MAXY,
     4                        DEG,NDEG,
     5                        LATTXT,LONTXT)

      IMPLICIT NONE

C INPUT:
c--port      INCLUDE 'SYS$LIBRARY:GKSDEFS.BND'
       INCLUDE 'GKS_ROOT:include/fgksenum.inc'

      INTEGER PROJN
      INTEGER NDEG(2)

      REAL OBSLAT,OBSLON
      REAL MINLAT,MAXLAT,MINLON,MAXLON
      REAL MINX,MAXX,MINY,MAXY
      REAL DEG(2,25)

      CHARACTER*(*) LATTXT(*),LONTXT(*)

C INTERNAL:
      INTEGER I,J

      REAL LATLN,LONLN,SX,SY,DIFX,DIFY

      LOGICAL HIDDEN,BRNCUT

c      REAL GET_DEF_CH, CHAR_HEIGHT, TXTSCL
c      INTEGER PLOT

C-----------------------------------------------------------------------


C WINDOW HEIGHT AND LENGTH

      DIFX = MAXX - MINX
      DIFY = MAXY - MINY

C DISPLAY LATITUDE GRID NUMBERS

C FOR LEFT AND RIGHT SIDES

      DO 1000 I = 1,2

        IF (I .EQ. 1) LONLN = MINLON
        IF (I .EQ. 2) LONLN = MAXLON

        DO 2000 J = 1,NDEG(2)

C CALCULATE X-Y POSITION
          CALL LTRANS (PROJN,OBSLAT,OBSLON,
     1                 LONLN,DEG(2,J),SX,SY,HIDDEN,BRNCUT)

C SET ALIGNMENT AND OFFSET

          IF (I .EQ. 1) THEN

C FOR LAT NUMBERS ON LEFT SIDE
            IF (DEG(2,J) .EQ. MINLAT) THEN
              CALL GSTXAL (GARITE,GABASE)
            ELSE IF (DEG(2,J) .EQ. MAXLAT) THEN
              CALL GSTXAL (GARITE,GACAP)
            ELSE
              CALL GSTXAL (GARITE,GAHALF)
            END IF

C            SX = (SX - (DIFX / 100.0))
            SX = (SX - (DIFX / 20.0))

          ELSE IF (I .EQ. 2) THEN

C FOR LAT NUMBER ON RIGHT SIDE
            IF (DEG(2,J) .EQ. MINLAT) THEN
              CALL GSTXAL (GALEFT,GABASE)
            ELSE IF (DEG(2,J) .EQ. MAXLAT) THEN
              CALL GSTXAL (GALEFT,GACAP)
            ELSE
              CALL GSTXAL (GALEFT,GAHALF)
            END IF

            SX = (SX + (DIFX / 100.0))

          END IF


C DISPLAY LAT TEXT
          CALL GTXS (SX,SY,3,LATTXT(J))

 2000   CONTINUE

 1000 CONTINUE


C DISPLAY LONGIITUDE GRID NUMBERS

C FOR TOP AND BOTTOM
      DO 3000 I = 1,2

        IF (I .EQ. 1) LATLN = MINLAT
        IF (I .EQ. 2) LATLN = MAXLAT
      
        DO 4000 J = 1,NDEG(1)

C CALCULATE X/Y POSITION
          CALL LTRANS (PROJN,OBSLAT,OBSLON,
     1                 DEG(1,J),LATLN,SX,SY,HIDDEN,BRNCUT)
     
     
C SET ALIGNMENT AND OFFSET

          IF (I .EQ. 1) THEN

C FOR LON NUMBERS ALONG BOTTOM
            CALL GSTXAL (GACENT,GACAP)
C            SY = (SY - (DIFY / 50.0))
            SY = (SY - (DIFY / 30.0))

          ELSE IF (I .EQ. 2) THEN
       
C FOR LON NUMBERS ALONG TOP
            CALL GSTXAL (GACENT,GABASE)
            SY = (SY + (DIFY / 50.0))

          END IF

C DISPLAY TEXT
          CALL GTXS (SX,SY,4,LONTXT(J))

 4000   CONTINUE

 3000 CONTINUE

 9999 CONTINUE
      RETURN
      END
