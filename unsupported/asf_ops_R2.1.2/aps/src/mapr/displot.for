C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	displot.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================

C-----------------------------------------------------------------
C SUBROUTINE DISPLOT
C
C PURPOSE
C	DRAW THE CURRENT DISPLAY ON A HP7550 PLOTTER
C	DISPLAY THE FOLLOWING GSEGS:
C	500 - HOLLOW GLOBE
C	502 - HOLLOW WORLD
C	504 - GRID LINES
C	506 - PLOT GRID TEXT
C
C $Logfile:   ACS003:[BLD.MPS.MAPR.SRC]DISPLOT.FOV  $
C
C INPUT
C	WPID		PLOTTER WORKSTATION ID
C	PROJN		PROJECTION NUMBER
C	GRIDLN		GRID LINE FLAG 0=NO 1=YES
C	NSEG		OVERLAY COUNT
C	WNSIZE		WINDOW SIZE (%)
C	MINLAT,MAXLAT,
C	MINLON,MAXLON	WINDOW LATS/LONS - WHAT IS DISPLAYED
C	OBSLAT,OBSLON	CENTER LAT/LON
C	OBSMIN,OBSMAX	180 DEGS OFF CENTER LON
C	LONPT
C	MINX,MAXX,
C	MINY,MAXY	GLOBE MIN/MAX X/Y
C	MINXWN,MAXXWN,
C	MINYWN,MAXYWN	WINDOW MIN/MAX X/Y
C	CRSEGN		OVERLAY STATUS ARRAY
C
C INTERNAL
C	I,J,K,L		TEMP
C	PLOT		PLOTTER FLAG 1=PLOT 0=NO PLOT
C	RES		MAP RESOLUTION 1=LOW,3=HIGH
C
C SUBROUTINES CALLED
C	GOPWK,GACWK,GASGWK,GDAWK,GCLWK		GKS ROUTINES
C
C	WINDOW		DETERMINE WINDOW MIN/MAX X/X
C
C WRITTEN BY CRAIG K. FUJIMOTO
C
C MODIFICATIONS
C $Date$ $Revision$ $Author$
C 8/8/94 Nadia Adhami  LIB$SET_LOGICAL,LIB$DELETE_LOGICAL -> system calls
C
C-----------------------------------------------------------------
      SUBROUTINE DISPLOT(WPID,PROJN,GRIDLN,WNSIZE,
     1            MINLAT,MAXLAT,MINLON,MAXLON,
     2            OBSLAT,OBSLON,OBSMIN,OBSMAX,
     3            LONPT,
     4            MINX,MAXX,MINY,MAXY,
     5            MINXWN,MAXXWN,MINYWN,MAXYWN,
     6            NSEG,CRSEGN)

      character*100 SccsFileID
     -/'@(#)displot.for	5.1 98/01/08 APS/ASF\0'/

      IMPLICIT NONE

C INPUT
c--port      INCLUDE 'SYS$LIBRARY:GKSDEFS.BND'
       INCLUDE 'GKS_ROOT:include/fgksenum.inc'
       INCLUDE 'mapper_port.inc'

      INTEGER WPID,PROJN,GRIDLN
      INTEGER NSEG

      REAL WNSIZE
      REAL MINLAT,MAXLAT,MINLON,MAXLON
      REAL OBSLAT,OBSLON,OBSMIN,OBSMAX
      REAL LONPT(*)
      REAL MINX,MAXX,MINY,MAXY
      REAL MINXWN,MAXXWN,MINYWN,MAXYWN

      INTEGER CRSEGN(*)

C INTERNAL
      CHARACTER*2 ANS,WGS
      CHARACTER*10 PORT
      INTEGER I,J,K,L
      INTEGER PLOT,IOS
C      REAL X(5),Y(5)
      REAL MAX(2)


c--port      integer GCONID      PriorGKS
      integer GCONID
      character*80 string
      integer status
      integer system

C-----------------------------------------------------------------

      GCONID  = 0

C SET FLAG
      PLOT = 1
      PORT = 'TTA2:'

 1000 CONTINUE

      WRITE(6,100)
  100 FORMAT (/,1X,'Plot world [Y/N] ? ',$)
      READ(5,110,IOSTAT=IOS) WGS
  110 FORMAT (A1)
      IF (IOS .NE. 0 .OR.
     1    (WGS .NE. 'y' .AND. WGS .NE. 'Y' .AND.
     2     WGS .NE. 'n' .AND. WGS .NE. 'N')) THEN
        CALL DISMSG(' Error : Invalid input.')
        GO TO 1000
      END IF

 2000 CONTINUE

      WRITE (6,120)
  120 FORMAT (/,1X,'Begin plotting [Y/N] ? ',$)

      READ(5,130,IOSTAT=IOS) ANS
  130 FORMAT (A1)
      IF (IOS .NE. 0 .OR.
     1    (ANS .NE. 'y' .AND. ANS .NE. 'Y' .AND.
     2     ANS .NE. 'n' .AND. ANS .NE. 'N')) THEN
        CALL DISMSG(' Error : Invalid input.')
        GO TO 2000
      END IF

      IF (ANS .EQ. 'N' .OR. ANS .EQ. 'n') GO TO 9999

C SET LOGICAL TO PLOTTER DEVICE
c--port      CALL LIB$SET_LOGICAL('GKS$CONID',PORT)
c--port      preceed the '$' character by a '\'

      WRITE (string, 1) 'GKS\$CONID', PORT
    1 FORMAT( 'setenv ', A, '  ', A )
      string = string // char(0)
      status = system( string )

C OPEN PLOTTER WORKSTATION
      CALL GOPWK(WPID,GCONID,GHP755)

C ACTIVATE PLOT WORSTATION
      CALL GACWK(WPID)

C SET VIEWPORT FOR NORMALIZATION TRANSFORMATION
      CALL GSVP(1,MINXWN,MAXXWN,MINYWN,MAXYWN)

C SET WINDOW FOR NORMALIZATION TRANSFORMATION
      CALL GSWN(1,MINXWN,MAXXWN,MINYWN,MAXYWN)

C SELECT NORMALIZATION TRANSFORMATION
      CALL GSELNT(1)

C INQUIRE MAX DRAWING SPACE
      CALL GQDSP(GHP755,I,J,MAX(1),MAX(2),K,L)

C SET WORKSTATION WINDOW FOR PLOTTER
      CALL GSWKVP (WPID,MAX(1) / 100.0, MAX(1) - (MAX(1) / 100.0),
     1                  MAX(2) / 100.0, MAX(2) - (MAX(2) / 100.0))

C SET PLOT WORKSTATION WINDOW
      CALL GSWKWN (WPID,MINXWN,MAXXWN,MINYWN,MAXYWN)

C DISPLAY THE GKS SEGMENTS


C DISPLAY GLOBE - HOLLOW
      CALL DISMSGW('Plotting the globe...')

      CALL GCSGWK (WPID,500)

C DISPLAY WORLD - HOLLOW
      IF (WGS .EQ. 'Y' .OR. WGS .EQ. 'y') THEN

        CALL DISMSGW('Plotting the world...')

        CALL GCSGWK (WPID,502)

      END IF

      IF (PROJN .NE. 1) THEN

C DISPLAY GRID and TEXT
        CALL DISMSGW('Plotting the grid...')

        CALL GCSGWK (WPID,504)

        CALL DISMSGW('Plotting the grid text...')

        CALL GCSGWK (WPID,506)

      END IF

C DISPLAY OVERLAYS
      IF (NSEG .GT. 0) THEN

        CALL DISMSGW('Plotting the overlays...')

        DO 3000 I = 1,NSEG   
   
          IF (CRSEGN(I) .EQ. 1) THEN
            CALL GCSGWK (WPID,I)
          END IF

 3000   CONTINUE

      END IF

C DEACTIVATE PLOT WORKSTATION
      CALL GDAWK(WPID)                     

C CLOSE PLOT WORKSTATION
      CALL GCLWK(WPID)

C DELETE OUTPUT LOGICAL
c--port      CALL LIB$DELETE_LOGICAL('GKS$CONID')

      WRITE (string, 1) 'GKS\$CONID'
    2 FORMAT( 'unsetenv ', A )
      string = string // char(0)
      status = system( string )


      CALL DISMSG('Plotting has completed.')

C SELECT UNITY NORMALIZATION TRANSLATION
      CALL GSELNT(0)

 9999 CONTINUE
      RETURN
      END
            
