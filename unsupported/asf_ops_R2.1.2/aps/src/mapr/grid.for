C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	grid.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================

C-----------------------------------------------------------------------
C SUBROUTINE GRID
C
C PURPOSE
C	CALCS THE GRID NUMBERS AND GRID LINES AND 
C	DISPLAYS THE GRID FOR THE CHOSEN PROJECTION.
C	THE GRID IS BROKEN INTO THREE GSEGS:
C	504 = THE GRID LINES
C	505 = THE GRID TEXT FOR DISPLAY
C	506 = THE GRID TEXT FOR PLOTTING
C
C $Logfile:   ACS003:[BLD.MPS.MAPR.SRC]GRID.FOV  $
C
C INPUT
C	WSID		WORKSTATION ID
C	PROJN		PROJECTION NUMBER
C	OBSLAT,OBSLON	CENTER LAT/LON
C	GRIDLN		DISPLAY GRID LINES  0 (OFF) OR 1 (ON)
C	MINLAT,MAXLAT,
C	MINLON,MAXLON	MIN/MAX LAT/LON OF WINDOW
C	OBSMIN,OBSMAX	MIN/MAX LONGITUDE OF THE WINDOW 
C	LONPT		ARRAY OF LONGITUDE GRID DIVISIONS 
C	MINX,MAXX,
C	MINY,MAXY	MIN/MAX X/Y COORDS OF ZOOM WINDOW
C
C INTERNAL
C	DIFLAT,DIFLON	HEIGHT/WIDTH OF WINDOW (DEG)
C	DEG(1,*)	LON GRID DIVISIONS
C	DEG(2,*)	LAT GRID DIVISIONS
C	NDEG(1)		NUMBER OF LON DIVISIONS
C	NDEG(2)		NUMBER OF LAT DIVISIONS
C	LPOS(1,*)	LON GRID LINE POSITIONS
C	LPOS(2,*)	LAT GRID LINE POSITIONS
C	NLPOS(1)	NUMBER OF LONG GRID POSITIONS
C	NLPOS(2)	NUMBER OF LAT GRID POSITIONS
C       LATINCR,LONINCR	LAT/LON INCREMENTERS FOR DIVISIONS OF POL STEREO
C       STRTLT,STOPLT   
C	STRTLN,STOPLN   START/STOP LAT/LON FOR POL STEREO
C	PLOT		PLOT FLAG 0=NO PLOT / 1=PLOT
C
C SUBROUTINE CALLS
C	DIFDEG
C	GRID_SETUP
C	GRID_DISPLAY
C	POLGRID
C	TEXT_SCALE
C	GRID_TEXT
C	POLTEXT
C
C GKS ROUTINES
C	GCRSG,GCLSG,GSPLCI,GSCHH,GSTXCI,GDAWK,GACWK
C
C WRITTEN BY CRAIG K. FUJIMOTO - FEB 90
C                         
C MODIFICATIONS
C $Date$ $Revision$ $Author$
C 4/24/95  Nadia Adhami  GSTXCI(1) -> (0) ; white -> black
C-----------------------------------------------------------------------
       SUBROUTINE GRID (WSID,PROJN,GRIDLN,
     1                  MINLAT,MAXLAT,MINLON,MAXLON,
     2                  OBSLAT,OBSLON,OBSMIN,OBSMAX,
     3                  LONPT,
     4                  MINX,MAXX,MINY,MAXY)

      character*100 SccsFileID
     -/'@(#)grid.for	5.1 98/01/08 APS/ASF\0'/

       IMPLICIT NONE
C INPUT:
       INTEGER WSID,PROJN,GRIDLN
       REAL OBSLAT,OBSLON,OBSMIN,OBSMAX,
     1      MINLAT,MAXLAT,MINLON,MAXLON,
     3      LONPT(*),
     4      MINX,MAXX,MINY,MAXY

C INTERNAL:
       INTEGER PLOT
       INTEGER NDEG(2),NLPOS(2)
       REAL DEG(2,25),LPOS(2,40)
       REAL LATINCR,LONINCR
       REAL STRTLT,STOPLT,STRTLN,STOPLN
       REAL DIFLAT,DIFLON
       REAL TXTSCL
C-----------------------------------------------------------------------

C-------------------------------------------------------------
C CREATE THE GRID GSEG FOR DISPLAYING AND PLOTTING
C-------------------------------------------------------------

       INCLUDE 'mapper_port.inc'

C OPEN GRID GSEG
      CALL GCRSG (504)

      CALL GSPLCI (5)

C IF CYLINDRICAL EQUIDISTANT, MERCATOR, OR MILLER CYLINDRICAL
                                                               
      IF (PROJN .EQ. 2 .OR. PROJN .EQ. 3 .OR. PROJN .EQ. 4) THEN

C CALCULATE WINDOW HEIGHT AND LENGTH
        CALL DIFDEG(OBSLON,MINLAT,MAXLAT,MINLON,MAXLON,
     1              DIFLAT,DIFLON)

C SET THE GRID DIVISION ARRAYS AND COUNTERS
        CALL GRID_SETUP (PROJN,MINLAT,MAXLAT,MINLON,MAXLON,
     1                   DIFLAT,DIFLON,LONPT,
     1                   DEG,NDEG,LPOS,NLPOS)

C DISPLAY THE GRID
        CALL GRID_DISPLAY(PROJN,GRIDLN,
     1                    MINLAT,MAXLAT,MINLON,MAXLON,
     2                    OBSLAT,OBSLON,OBSMIN,OBSMAX,
     3                    MINX,MAXX,MINY,MAXY,
     4                    LPOS,NLPOS)

      ELSE IF (PROJN .EQ. 5 .OR. PROJN .EQ. 6) THEN

        CALL POLGRID(PROJN,GRIDLN,OBSLAT,OBSLON,
     1               MINLAT,MAXLAT,MINLON,MAXLON,
     1               LATINCR,LONINCR,
     1               STRTLT,STOPLT,STRTLN,STOPLN)

      END IF

C CLOSE THE GRID GSEG
      CALL GCLSG ()


C-------------------------------------------------------------
C CREATE THE GRID TEXT GSEG FOR DISPLAYING
C-------------------------------------------------------------

C OPEN GSEG
      CALL GCRSG (505)

      PLOT = 0
      CALL TEXT_SCALE(PROJN,PLOT,
     1                MINX,MAXX,MINY,MAXY,
     2                TXTSCL)
      CALL GSCHH(TXTSCL)

      IF (TESTMODE .GT. 0) THEN
      WRITE(6, 123) PLOT,TXTSCL
 123  FORMAT(/,'GRID() :: PLOT=',I,' GSCHH(',F,')')
      ENDIF


C TEXT WHITE -> BLACK
C     CALL GSTXCI(1)
      CALL GSTXCI(0)

C IF CYLINDRICAL EQUIDISTANT, MERCATOR, OR MILLER CYLINDRICAL
                                                               
      IF (PROJN .EQ. 2 .OR. PROJN .EQ. 3 .OR. PROJN .EQ. 4) THEN

        CALL GRID_TEXT (PROJN,
     1                  MINX,MAXX,MINY,MAXY,
     2                  MINLAT,MAXLAT,MINLON,MAXLON,
     3                  OBSLAT,OBSLON,OBSMIN,OBSMAX,
     4                  DEG,NDEG)

      ELSE IF (PROJN .EQ. 5 .OR. PROJN .EQ. 6) THEN

         CALL POLTEXT(PROJN,OBSLAT,OBSLON,
     1                MINLAT,MAXLAT,MINLON,MAXLON,
     1                LATINCR,LONINCR,
     1                STRTLT,STOPLT,STRTLN,STOPLN)

      END IF

C CLOSE THE GRID TEXT DISPLAY GSEG
      CALL GCLSG ()


C-------------------------------------------------------------
C CREATE THE GRID TEXT GSEG FOR PLOTTING
C-------------------------------------------------------------

C TEMPORARILY DEACTIVATE DISPLAY WORKSTATION SO THAT
C PLOT GRID TEXT GSEG IS NOT DISPLAYED
      CALL GDAWK(WSID)

C OPEN GSEG
      CALL GCRSG (506)

C SCALE TEXT
      PLOT = 1
      CALL TEXT_SCALE(PROJN,PLOT,
     1                MINX,MAXX,MINY,MAXY,
     2                TXTSCL)
      CALL GSCHH(TXTSCL)

      IF (TESTMODE .GT. 0) THEN
      WRITE(6, 12) PLOT,TXTSCL
 12   FORMAT(/,'GRID() :: PLOT=',I,' GSCHH(',F,')')
      ENDIF
 
 
C TEXT BLACK
        CALL GSTXCI(0)

C IF CYLINDRICAL EQUIDISTANT, MERCATOR, OR MILLER CYLINDRICAL
                                                               
      IF (PROJN .EQ. 2 .OR. PROJN .EQ. 3 .OR. PROJN .EQ. 4) THEN

        CALL GRID_TEXT (PROJN,
     1                  MINX,MAXX,MINY,MAXY,
     2                  MINLAT,MAXLAT,MINLON,MAXLON,
     3                  OBSLAT,OBSLON,OBSMIN,OBSMAX,
     4                  DEG,NDEG)

      ELSE IF (PROJN .EQ. 5 .OR. PROJN .EQ. 6) THEN

         CALL POLTEXT(PROJN,OBSLAT,OBSLON,
     1                MINLAT,MAXLAT,MINLON,MAXLON,
     2                LATINCR,LONINCR,
     3                STRTLT,STOPLT,STRTLN,STOPLN)

      END IF

C CLOSE THE GRID TEXT PLOT GSEG
      CALL GCLSG ()

C REACTIVATE DISPLAY WORKSTATION
      CALL GACWK(WSID)

 9999 CONTINUE
      RETURN
      END
