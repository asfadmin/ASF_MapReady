C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	grid_display.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================

C-----------------------------------------------------------------------
C SUBROUTINE GRID_DISPLAY
C
C $Logfile:   ACS003:[BLD.MPS.MAPR.SRC]GRID_DISPLAY.FOV  $
C
C PURPOSE
C	DISPLAYS THE GRID LINES IN THE CURRENT WINDOW.
C
C VARIABLES
C INPUT
C	PROJN		PROJECTION TYPE TO DISPLAY MAP
C	OBSLAT,OBSLON	CENTER LAT/LON
C	OBSMIN,OBSMAX	WINDOW MIN/MAX LON
C	GRIDLN		DISPLAY GRID LINES  0=OFF, 1=ON
C	MINLAT,MAXLAT,
C	MINLON,MAXLON	MIN/MAX WINDOW DIMENSIONS (DEG)
C	MINX,MAXX,
C	MINY,MAXY	MIN/MAX X/Y COORDS OF WINDOW
C	LPOS(1,*)	LONGITUDE GRID LINE POSITIONS
C	LPOS(2,*)	LATITUDE GRID LINE POSITIONS
C	NLPOS(1)	NUMBER OF LONGITUDE GRID 
C	NLPOS(2)	NUMBER OF LATITUDE GRID 
C
C INTERNAL
C	LATX1,LATX2	TEMP VARS FOR A X-COORDS
C	LATY1,LATY2	TEMP ARRAYS FOR Y-COORDS
C	LONX1,LONX2	TEMP ARRAYS FOR X-COORDS
C	LONY1,LONY2	TEMP VARS FOR A X-COORDS
C	X(),Y()		ARRAY OF X/Y COORD GRID LINE POSITIONS
C	DIFX,DIFY	LENGTH/HEIGHT OF THE WINDOW
C	HIDDEN		INDICATES WHETHER POINT IS HIDDEN OR NOT
C	BRNCUT		INDICATES WHETHER LINE IS BROKEN OR NOT
C	I     		DO LOOP INDEX
C
C
C SUBROUTINE CALLS
C        LTRANS  CALCULATES THE LAT-LON TO X-Y COORD TRANSFORMATION
C
C ORIGINALLY WRITTEN BY RICHARD P. LEE  5-25-88
C MODIFIED FOR ASF BY CRAIG K. FUJIMOTO
C
C $Date$ $Revision$ $Author$
C-----------------------------------------------------------------------
      SUBROUTINE GRID_DISPLAY(PROJN,GRIDLN,
     1                        MINLAT,MAXLAT,MINLON,MAXLON,
     2                        OBSLAT,OBSLON,OBSMIN,OBSMAX,
     3                        MINX,MAXX,MINY,MAXY,
     4                        LPOS,NLPOS)

      character*100 SccsFileID
     -/'@(#)grid_display.for	5.1 98/01/08 APS/ASF\0'/

      IMPLICIT NONE

C INPUT:                  
c--port      INCLUDE 'SYS$LIBRARY:GKSDEFS.BND'
       INCLUDE 'GKS_ROOT:include/fgksenum.inc'

      INTEGER PROJN,GRIDLN
      INTEGER NLPOS(2)

      REAL MINLAT,MAXLAT,MINLON,MAXLON
      REAL OBSLAT,OBSLON,OBSMIN,OBSMAX
      REAL MINX,MAXX,MINY,MAXY
      REAL LPOS(2,40)

C INTERNAL:           
      INTEGER I

      REAL LATX1,LATX2
      REAL LATY1(25),LATY2(25),LONX1(40),LONX2(40)
      REAL LONY1,LONY2
      REAL X(5),Y(5)
      REAL DIFX,DIFY

      LOGICAL HIDDEN,BRNCUT

C-----------------------------------------------------------------------
      
C WINDOW HEIGHT AND LENGTH

      DIFX = MAXX - MINX
      DIFY = MAXY - MINY

C OUTLINE WINDOW
      X(1) = MINX
      X(2) = MAXX
      X(3) = MAXX
      X(4) = MINX
      X(5) = X(1)
      Y(1) = MAXY
      Y(2) = MAXY
      Y(3) = MINY
      Y(4) = MINY
      Y(5) = Y(1)
    
      CALL GPL (5,X,Y)
      

C CALCULATE THE X-Y GRID LINE POSITIONS

C   LATITUDE GRID LINES

      DO 10 I = 1,NLPOS(2)

        CALL LTRANS (PROJN,OBSLAT,OBSLON,
     1               OBSMIN,LPOS(2,I),
     2               LATX1,LATY1(I),
     3               HIDDEN,BRNCUT)

        CALL LTRANS (PROJN,OBSLAT,OBSLON,
     1               OBSMAX,LPOS(2,I),
     2               LATX2,LATY2(I),
     3               HIDDEN,BRNCUT)

   10 CONTINUE

C   LONGITUDE GRID LINES

      DO 20 I = 1,NLPOS(1)

        CALL LTRANS (PROJN,OBSLAT,OBSLON,
     1               LPOS(1,I),MINLAT,
     2               LONX1(I),LONY1,
     3               HIDDEN,BRNCUT)

        CALL LTRANS (PROJN,OBSLAT,OBSLON,
     1               LPOS(1,I),MAXLAT,
     2               LONX2(I),LONY2,
     3               HIDDEN,BRNCUT)

   20 CONTINUE

C IF CHOSEN TO PLOT GRID LINES, PLOT GRID LINES

      IF (GRIDLN .EQ. 1) THEN

C   LATITUDE GRID LINES

         X(1) = LATX1
         X(2) = LATX2
         DO 30 I = 1,NLPOS(2)
           Y(1) = LATY1(I)
           Y(2) = LATY2(I)
           CALL GPL (2,X,Y)
   30    CONTINUE

C   LONGIITUDE GRID LINES

         Y(1) = LONY1
         Y(2) = LONY2
         DO 40 I = 1,NLPOS(1)
           X(1) = LONX1(I)
           X(2) = LONX2(I)
           CALL GPL (2,X,Y)
   40    CONTINUE

C IF CHOSEN NOT TO PLOT GRID LINES, PLOT GRID TICK MARKS

       ELSE IF (GRIDLN .EQ. 0) THEN

C   LATITUDE GRID TICK MARKS

         DO 50 I = 1,NLPOS(2)
           X(1) = LATX1
           X(2) = (LATX1 + (DIFX / 30.0))
           Y(1) = LATY1(I)
           Y(2) = LATY1(I)
           CALL GPL (2,X,Y)
           X(1) = LATX2
           X(2) = (LATX2 - (DIFX / 30.0))
           Y(1) = LATY2(I)
           Y(2) = LATY2(I)
           CALL GPL (2,X,Y)
   50    CONTINUE

C   LONGITUDE GRID TICK MARKS

         DO 60 I = 1,NLPOS(1)
           X(1) = LONX1(I)
           X(2) = LONX1(I)
           Y(1) = LONY1
           Y(2) = (LONY1 + (DIFY / 25.0))
           CALL GPL (2,X,Y)
           X(1) = LONX2(I)
           X(2) = LONX2(I)
           Y(1) = LONY2
           Y(2) = (LONY2 - (DIFY / 25.0))
           CALL GPL (2,X,Y)
   60    CONTINUE
       END IF

 9999  CONTINUE
       RETURN
       END
