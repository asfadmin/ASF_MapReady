C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	globe.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================

C-----------------------------------------------------------------
C
C SUBROUTINE GLOBE                        
C
C PURPOSE
C	DISPLAY THE BACKGROUND AND THE EARTH, A CIRCLE OR RECTANGLE
C	DEPENDING ON THE PROJECTION
C
C $Logfile:   ACS003:[BLD.MPS.MAPR.SRC]GLOBE.FOV  $
C
C INPUT
C	PROJN		PROJECTION NUMBER
C	PLOT		FLAG TO INDICATE IF DRAWING TO HP PLOTTER 0-NO 1-YES
C	OBSLAT,OBSLON	CENTER LAT,LON
C	MINX,MAXX,MINY,MAXY
C
C OUTPUT
C	XEDGE,YEDGE	EDGE POINTS FOR POLAR STEREO AND SAT VIEW PROJS
C
C INTERNAL
C	DR		DATA RECORD DUMMY VAR
C	I		COUNTER
C	IOS		I/O ERROR FLAG
C	BGX,BGY		ARRAY TO FILL ENTIRE WORLD
C	GLOBEX,GLOBEY	GLOBE DIMENSIONS SPECIFIC TO PROJECTION TYPE
C	DISX,DISY	BUFFERED AMOUNT TO ALLOW FOR TEXT IN BORDER
C	RADIUS		<
C	DEGS,RADS	DEGREES AND RADIANS
C	PI		<
C	HIDDEN,BRNCUT	FLAGS FOR LTRANS
C
C SUBROUTINES CALLED
C	GSFAIS,GSFACI,GGDP,GFA
C
C	LTRANS		TRANASLATE L/L TO X/Y
C
C WRITTEN BY CRAIG K. FUJIMOTO
C
C MODIFICATIONS
C $Date$ $Revision$ $Author$
C 4/14/95  Nadia Adhami   fill the map white
C 4/14/95  Nadia Adhami   map is never filled; CALL GSFAIS (GHOLLO)
C-----------------------------------------------------------------
      SUBROUTINE GLOBE(PROJN,PLOT,OBSLAT,OBSLON,
     1                 MINX,MAXX,MINY,MAXY,
     1                 XEDGE,YEDGE)

      character*100 SccsFileID
     -/'@(#)globe.for	5.1 98/01/08 APS/ASF\0'/

      IMPLICIT NONE

C INPUT
c--port      INCLUDE 'SYS$LIBRARY:GKSDEFS.BND'
       INCLUDE 'GKS_ROOT:include/fgksenum.inc'

      INTEGER PROJN,PLOT
      REAL MINX,MAXX,MINY,MAXY
      REAL OBSLAT,OBSLON
     
C OUTPUT
      REAL XEDGE(*),YEDGE(*)

C INTERNAL
      INTEGER I
c--port      INTEGER DR
      character*80 DR(2)
c--port      integer GGFCCP /-201 /			PriorGKS
      integer GGFCCP /-201 /

      REAL BGX(5) / 0.0,0.0,1.0,1.0,0.0 /
      REAL BGY(5) / 0.0,1.0,1.0,0.0,0.0 /
      REAL GLOBEX(1800)
      REAL GLOBEY(1800)
      REAL DISX /0.05/
      REAL DISY /0.05/
      REAL RADIUS,DEGS,RADS
      REAL PI /3.14159265359/

      LOGICAL HIDDEN,BRNCUT

      integer LDR
      integer FILL_COLOR_IND
      REAL    XSQUARE( 4 ),  YSQUARE( 4 )

      DATA XSQUARE  /0.00,0.00,1.00,1.00/
      DATA YSQUARE  /0.00,1.00,1.00,0.00/


C-----------------------------------------------------------------

      LDR = 0

C FILL AREA INTERIOR STYLE - SOLID OR HOLLOW
      IF (PLOT .EQ. 0) THEN
        CALL GSFAIS (GSOLID)
      ELSE
        CALL GSFAIS (GHOLLO)
      END IF

C FILL THE WINDOW WHITE = 1
      FILL_COLOR_IND = 1
      CALL GSFAIS (GSOLID)
      CALL GSFACI( FILL_COLOR_IND )
      CALL GFA (4,XSQUARE,YSQUARE)

C RESET TO HOLLO FOR LAND + OCEAN
      CALL GSFAIS (GHOLLO)

C *************************************************************
C FOR SATELLITE VIEW
C *************************************************************

      IF (PROJN .EQ. 1) THEN

C FILL AREA COLOR INDEX - BLUE
        CALL GSFACI (2)

C THE CENTER
        CALL LTRANS (PROJN,90.0,0.0,
     1              0.0,90.0,
     1              GLOBEX(1),GLOBEY(1),
     1              HIDDEN,BRNCUT)
 
C A POINT ON THE EQUATOR
        CALL LTRANS (PROJN,90.0,0.0,
     1              0.0,0.0,
     1              GLOBEX(2),GLOBEY(2),
     1              HIDDEN,BRNCUT)

c--port  SET FILL AREA COLOUR INDEX to 4 (blue) 
c--port  because the  circular disc
C--port  GDP uses fill area atributes.
c--port  nadia: I think this fixes the bug listed below by Craig
        CALL GSFACI( 2)
 

C FILL IN GLOBE
        CALL GGDP (2,GLOBEX,GLOBEY,GGFCCP,LDR,DR)

        RADIUS = SQRT(ABS(GLOBEY(1) - GLOBEY(2))**2 +
     1               ABS(GLOBEX(1) - GLOBEX(2))**2)

C INITIALIZE XEDGE AND YEDGE ARRAYS
C CALCULATE HALF-DEGREE POINTS ON THE UNIT CIRCLE

        DEGS = 0.0
        DO 1000 I = 1,720
          DEGS = DEGS + 0.5
          RADS = DEGS * PI / 180.0
          XEDGE(I) = 0.5 + (RADIUS * COS(RADS))
          YEDGE(I) = 0.5 + (RADIUS * SIN(RADS))
 1000   CONTINUE


C *************************************************************
C FOR POLAR STEREOGRAPHIC
C *************************************************************

      ELSE IF (PROJN.EQ.5 .OR. PROJN.EQ.6) THEN

C FILL AREA COLOR INDEX - OCEAN BLUE
        CALL GSFACI (2)

C THE CENTER
        CALL LTRANS (PROJN,OBSLAT,OBSLON,
     1              0.0,OBSLAT,
     1              GLOBEX(1),GLOBEY(1),
     1              HIDDEN,BRNCUT)
 
C A POINT ON THE EQUATOR
        CALL LTRANS (PROJN,OBSLAT,OBSLON,
     1              0.0,0.0,
     1              GLOBEX(2),GLOBEY(2),
     1              HIDDEN,BRNCUT)

C DISPLAY THE GLOBE

C XXX
C IF BUG OCCURS WHEN ZOOMING ON POLAR STEREO, COMMENT THIS LINE
C OUT AND ACTIVATE COMMENTED CODE BELOW.
C XXX
c--port        CALL GGDP (2,GLOBEX,GLOBEY,GGFCCP,DR,0)
        CALL GGDP (2,GLOBEX,GLOBEY,GGFCCP,LDR,DR)

C INITIALIZE THE XEDGE AND YEDGE ARRAYS
        RADIUS = SQRT(ABS(GLOBEY(1) - GLOBEY(2))**2 +
     1               ABS(GLOBEX(1) - GLOBEX(2))**2)

C XXX
C THIS CODE IS A FIX FOR THE GKS BUG. THE BUG OCCURRED WHEN AN 
C ATTEMPT WAS MADE TO ZOOM IN ON A GDP CIRCLE (THE GLOBE). OCCASIONALLY
C THE ZOOM WINDOW WOULD DISPLAY BLACK BACKGROUND AT THE CORNERS OF THE 
C IMAGE WHEN IT SHOULD HAVE BEEN ALL BLUE.  THE BUG WAS DETECTED IN 
C VMS 4.7 GKS 3.0
C XXX
C        DEGS = 0.0
C
C        DO 1500 I = 1,1080
C          DEGS = DEGS + 0.33333
C          RADS = DEGS / 180.0 * PI
C          GLOBEX(I) = 0.5 + (RADIUS * COS(RADS))
C          GLOBEY(I) = 0.5 + (RADIUS * SIN(RADS))
C 1500   CONTINUE
C
C FILL IN ENTIRE WINDOW
C        CALL GFA (1080,GLOBEX,GLOBEY)
C XXX

        DEGS = 0.0
                                  
        DO 2000 I = 1,720
          DEGS = DEGS + 0.5
          RADS = DEGS / 180.0 * PI
          XEDGE(I) = 0.5 + (RADIUS * COS(RADS))
          YEDGE(I) = 0.5 + (RADIUS * SIN(RADS))
 2000   CONTINUE


C *************************************************************
C FOR MERCATOR, CYLIN EQUI, AND MILLER CYLIN
C *************************************************************

      ELSE

C FILL AREA COLOR INDEX - BLUE
       CALL GSFACI (2)

       GLOBEX(1) = MINX
       GLOBEX(2) = MAXX
       GLOBEX(3) = MAXX
       GLOBEX(4) = MINX
       GLOBEX(5) = MINX

       GLOBEY(1) = MAXY
       GLOBEY(2) = MAXY
       GLOBEY(3) = MINY
       GLOBEY(4) = MINY
       GLOBEY(5) = MAXY

C FILL IN ENTIRE WINDOW
C-old  CALL GFA (5,GLOBEX,GLOBEY)
                
      END IF

 9999 CONTINUE
      RETURN
      END
