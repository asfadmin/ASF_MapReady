C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	disprint.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================

C-----------------------------------------------------------------
C SUBROUTINE DISPRINT
C
C PURPOSE
C	DRAW THE CURRENT DISPLAY ON HARDCOPY OUTPUT
C	SELECT EITHER A LA75 PRINTER OR A HP7550 PLOTTER
C
C $Logfile:   ACS003:[BLD.MPS.MAPR.SRC]DISPRINT.FOV  $
C
C INPUT
C	WPID		PLOTTER/PRINTER WORKSTATION ID
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
      SUBROUTINE DISPRINT(WPID,PROJN,GRIDLN,WNSIZE,
     1            MINLAT,MAXLAT,MINLON,MAXLON,
     2            OBSLAT,OBSLON,OBSMIN,OBSMAX,
     3            LONPT,
     4            MINX,MAXX,MINY,MAXY,
     5            MINXWN,MAXXWN,MINYWN,MAXYWN,
     6            NSEG,CRSEGN)

      character*100 SccsFileID
     -/'@(#)disprint.for	5.1 98/01/08 APS/ASF\0'/

      IMPLICIT NONE

C INPUT

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
      CHARACTER*2 ANS
      CHARACTER*10 DEV
      CHARACTER*10 PORT
      INTEGER I,J,K,L
      INTEGER PLOT,IOS
C      REAL X(5),Y(5)
      REAL MAX(2)

c--port      integer GCONID 		PriorGKS
      integer GCONID
      character*80 string
      integer status
      integer system

C-----------------------------------------------------------------

       INCLUDE 'GKS_ROOT:include/fgksenum.inc'
       INCLUDE 'mapper_port.inc'

      GCONID = 0

C SET FLAG
      PLOT = 1

      DEV = 'Printing'
c--port      PORT = 'TTA3:'
      PORT = 'asf3:'

 1000 CONTINUE

c      WRITE (6,100) DEV
      WRITE (6,100) 
  100 FORMAT (/,'Begin printing [Y/N] ? ')

      READ(5,150,IOSTAT=IOS) ANS
  150 FORMAT (A1)
      IF (IOS .NE. 0 .OR.
     1    (ANS .NE. 'y' .AND. ANS .NE. 'Y' .AND.
     2     ANS .NE. 'n' .AND. ANS .NE. 'N')) THEN
        CALL DISMSG(' Error : Invalid input.')
        GO TO 1000
      END IF

      IF (ANS .EQ. 'N' .OR. ANS .EQ. 'n') GO TO 9999

C SET LOGICAL TO PRINTER DEVICE
c--port      CALL LIB$SET_LOGICAL('GKS$CONID',PORT)

      WRITE (string, 1) 'GKS\$CONID', PORT
    1 FORMAT( 'setenv ', A, '  ', A )
      string = string // char(0)
      status = system( string )
 

      WRITE (*,*) 'call GOPWK ...'

C OPEN PRINTER WORKSTATION
c--port      CALL GOPWK(WPID,GCONID,GLA75)

      OPEN (UNIT=9, FILE='house.ps', STATUS='UNKNOWN')
      CALL GOPWK(WPID,9,101)

      WRITE (*,*) 'GOPWK completed'

C ACTIVATE PLOT/PRINT WORSTATION
      CALL GACWK(WPID)

C SET VIEWPORT FOR NORMALIZATION TRANSFORMATION
      CALL GSVP(1,MINXWN,MAXXWN,MINYWN,MAXYWN)

C SET WINDOW FOR NORMALIZATION TRANSFORMATION
      CALL GSWN(1,MINXWN,MAXXWN,MINYWN,MAXYWN)

C SELECT NORMALIZATION TRANSFORMATION
      CALL GSELNT(1)

C INQUIRE MAX DRAWING SIZE
      CALL GQDSP(GLA75,I,J,MAX(1),MAX(2),K,L)

C SET WORKSTATION WINDOW TO PRINTER
      CALL GSWKVP (WPID,0.0,MAX(1),0.0,MAX(2))
                                       
C SET PRINT WORKSTATION WINDOW
      CALL GSWKWN (WPID,MINXWN,MAXXWN,MINYWN,MAXYWN)

C DISPLAY THE GKS SEGMENTS

      CALL DISMSGW('Printing the world...')

C DISPLAY GLOBE - HOLLOW
      CALL GCSGWK (WPID,500)

C DISPLAY WORLD - HOLLOW
C      CALL GCSGWK (WPID,502)

      IF (PROJN .NE. 1) THEN

C DISPLAY GRID
        CALL DISMSGW('Printing the grid...')

        CALL GCSGWK (WPID,504)

C DISPLAY GRID TEXT
        CALL DISMSGW('Printing the grid text...')

        CALL GCSGWK (WPID,506)

      END IF

C DISPLAY OVERLAYS
      IF (NSEG .GT. 0) THEN

        CALL DISMSGW('Printing the overlays...')

        DO 3000 I = 1,NSEG   
   
          IF (CRSEGN(I) .EQ. 1) THEN
            CALL GCSGWK (WPID,I)
          END IF

 3000   CONTINUE

      END IF

C DEACTIVATE PLOT/PRINT WORKSTATION
      CALL GDAWK(WPID)

C CLOSE PLOT/PRINT WORKSTATION
      CALL GCLWK(WPID)

C DELETE OUTPUT LOGICAL
c--port      CALL LIB$DELETE_LOGICAL('GKS$CONID')

      WRITE (string, 1) 'GKS\$CONID'
    2 FORMAT( 'unsetenv ', A )
      string = string // char(0)
      status = system( string )
 


      CALL DISMSG('Printing has completed.')

C SELECT UNITY NORMALIZATION TRANSLATION
      CALL GSELNT(0)
                  
 9999 CONTINUE
      RETURN
      END
