C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	minput.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================

C-----------------------------------------------------------------------
C SUBROUTINE MFINPUT
C
C $Logfile:   QDISK:[BLD.MPS.MAPR.SRC]MINPUT.FOV  $
C
C PURPOSE
C	READ IN A METAFILE HEADER FILE RECORD
C
C INPUT
C	MWFIL		METAFILE FILENAME
C	MWHDR		METAFILE HEADER FILENAME
C
C OUTPUT
C	PROJN		PROJECTION NUMBER
C	GRIDLN		GRID LINE FLAG
C	WNSIZE		WORKSTATION SIZE (DEFAULTS TO 1 ALWAYS)
C	OBSLAT,OBSLON	CENTER LAT/LON
C	MINLAT,MAXLAT	MIN/MAX WINDOW LAT
C	MINLON,MAXLON	"       "      LON
C	STMNLT,STMXLT	ABSOLUTE MIN/MAX WINDOW LAT
C	STMNLN,STMXLN	"        "       "      LON
C	START_LL	START WINDOW MIN/MAX LON/LAT
C	NSEG		SEG COUNTER
C	CRSEGN()	SEG STATUS ARRAY
C	CRSEGT()	SEG NAME ARRAY
C	CRDARID()    	SEG DARID ARRAY
C	IOS		STATUS FLAG 1 = ERROR / 0 = NORMAL / -1 = QUIT
C	
C INTERNAL
C	I		LOOP COUNTER
C	UINIT		USER INITIALS
C	COMMNT		USER COMMENTS
C
C WRITTEN BY RICHARD P. LEE
C MODIFIED BY CRAIG K. FUJIMOTO
C
C MODIFICATION
C Date:   18 Jul 1990 17:23:36  Revision:   2.0  Author:   DBMAN  
C $Date$ $Revision$ $Author$
C 10/4/94  Nadia Adhami  Display segment index as a 4-digit number
C-----------------------------------------------------------------------
      SUBROUTINE MFINPUT(MWFIL,MWHDR,PROJN,GRIDLN,WNSIZE,
     1                   OBSLAT,OBSLON,
     2                   MINLAT,MAXLAT,MINLON,MAXLON,
     3                   STMNLT,STMXLT,STMNLN,STMXLN,
     4                   START_LL,
     5                   NSEG,CRSEGN,CRSEGT,CRDARID,
     6                   IOS)

      character*100 SccsFileID
     -/'@(#)minput.for	5.1 98/01/08 APS/ASF\0'/

      IMPLICIT NONE

C INPUT:
      CHARACTER*(*) MWFIL,MWHDR
C OUTPUT:
      INTEGER PROJN,GRIDLN
      INTEGER NSEG,CRSEGN(*),CRDARID(*)
      INTEGER IOS
      REAL WNSIZE
      REAL OBSLAT,OBSLON
      REAL MINLAT,MAXLAT,MINLON,MAXLON
      REAL STMNLT,STMXLT,STMNLN,STMXLN
      REAL START_LL(2,2)
      CHARACTER*(*) CRSEGT(*)
C INTERNAL:
      CHARACTER*1  ANS
      CHARACTER*50 PTXT
      CHARACTER*15 GTXT
      CHARACTER*21 DATE
      CHARACTER*76 COMMNT
            
C FUNCTION
      INTEGER SLEN

 1000 CONTINUE

C GET THE METAFILE NAME AND HEADER FILE NAME
      CALL ASK_MFNAME(MWFIL,MWHDR,0,IOS)
      IF (IOS .NE. 0) THEN
        IOS = 1
        GO TO 9999
      END IF

      CALL DISMSGW('Reading Header file...')

C OPEN THE HEADER FILE

      OPEN (UNIT=10,FILE=MWHDR,IOSTAT=IOS,STATUS='OLD')

      IF (IOS .NE. 0) THEN
        CALL DISMSG('Error opening header file.')
        IOS = 1
        GO TO 9999
      END IF

C  READ IN MAP WINDOW HEADER INFORMATION
            
      READ (10,100,IOSTAT=IOS) COMMNT,DATE,
     1      PROJN,GRIDLN,
     2      MINLAT,MAXLAT,MINLON,MAXLON,
     3      OBSLAT,OBSLON,
     4      STMNLT,STMXLT,STMNLN,STMXLN
                                   
  100 FORMAT (A76,A21,2(I2),10(F9.3))

      IF (IOS .NE. 0) THEN
        CALL DISMSG('Error reading header file record.')
        IOS = 1
        GO TO 8000
      END IF

      IF (PROJN .EQ. 1) THEN
        PTXT = 'SATELLITE VIEW'
      ELSE IF (PROJN .EQ. 2) THEN
        PTXT = 'CYLINDRICAL EQUIDISTANT'
      ELSE IF (PROJN .EQ. 3) THEN
        PTXT = 'MERCATOR'
      ELSE IF (PROJN .EQ. 4) THEN
        PTXT = 'MILLER CYLINDRICAL'
      ELSE IF (PROJN .EQ. 5) THEN
        PTXT = 'POLAR STEREOGRAPHIC - NORTHERN HEMISPHERE'
      ELSE IF (PROJN .EQ. 6) THEN
        PTXT = 'POLAR STEREOGRAPHIC - SOUTHERN HEMISPHERE'
      ELSE
        PTXT = 'NONE'
      END IF

      IF (GRIDLN .EQ. 0) THEN
        GTXT = 'W/O GRID LINES'
      ELSE
        GTXT = 'W/ GRID LINES'
      END IF

       WRITE (6,200)
       WRITE (6,210) PTXT,GTXT
       WRITE (6,220) NINT(OBSLON)
       WRITE (6,230) NINT(MINLAT),NINT(MAXLAT)
       WRITE (6,240) NINT(MINLON),NINT(MAXLON)
       WRITE (6,250) COMMNT
       WRITE (6,260) DATE
       WRITE (6,270)
  200  FORMAT (/,' MAP INFORMATION - ',/)
  210  FORMAT (' Projection: ',A<SLEN(PTXT)>,' ',A<SLEN(GTXT)>)
  220  FORMAT (' Center Lon: ',I4)
  230  FORMAT (' Window:  ',I4,' to  ',I4,' deg latitude')
  240  FORMAT ('          ',I4,' to  ',I4,' deg longitude')
  250  FORMAT (' Comments: ',A78)
  260  FORMAT (' Created : ',A21)
  270  FORMAT (/,' Overlays: ')

C  READ IN REQUIRED MAP WINDOW SEGMENT NAMES

      NSEG = 0

 2000 CONTINUE

C READ IN OVERLAY NAME AND ASSOCIATED DARID IF SSC SEG

      READ(10,300,END=3000,IOSTAT=IOS) CRSEGT(NSEG+1),CRDARID(NSEG+1)
  300 FORMAT (1X,A20,I5)

      IF (IOS .NE. 0) THEN
        CALL DISMSG('Error reading header file record.')
        IOS = 1
        GO TO 8000
      END IF

C INCREMENT THE SEGMENT COUNTER
      NSEG = NSEG + 1

C MARK THE SEGMENT AS VISIBLE
      CRSEGN(NSEG) = 1          

C DISPLAY OVERLAY NUMBER AND NAME FOR VERIFICATION
      WRITE (6,350) NSEG,CRSEGT(NSEG)
  350 FORMAT ('  ',I4,'  ',A20)

      GO TO 2000


C NORMAL CLOSE
 3000 CONTINUE

      CLOSE (10)

      IF (NSEG .EQ. 0) THEN
        WRITE(6,370)
  370   FORMAT ('  No overlays.')
      END IF

 4000 CONTINUE

      WRITE (6,400)
  400 FORMAT (/,' Is this the correct file? [Y/N] ',$)

      READ (5,410,IOSTAT = IOS) ANS
  410 FORMAT (A1)

      IF (IOS .NE. 0) THEN
        CALL DISMSG('Error on input.')
        GO TO 4000
      ELSE IF (ANS .NE. 'Y' .AND. ANS .NE. 'y') THEN
        IOS = -1
        GO TO 9999
      END IF

C SET THE WORKSTATION SIZE
      WNSIZE = 1.0

C SET THE START WINDOW VARIABLES
      START_LL(1,1) = MINLON
      START_LL(1,2) = MAXLON
      START_LL(2,1) = MINLAT
      START_LL(2,2) = MAXLAT
                            
      IOS = 0

      GO TO 9999

C ERROR FORCED CLOSE
 8000 CONTINUE
      CLOSE (10)

 9999 CONTINUE
      RETURN
      END

C-----------------------------------------------------------------------
C SUBROUTINE MFBLKOUT
C
C PURPOSE
C	SET THE WINDOW TO VERY SMALL WHILE REDRAWING THE IMAGE FROM A 
C	METAFILE
C
C INPUT
C	WSID		WORKSTATION ID
C INTERNAL
C	I,J,K,L		DUMMY
C	DVMAXX,DVMAXY	DEVICE MAX X/Y
C
C WRITTEN BY CRAIG K. FUJIMOTO
C
C-----------------------------------------------------------------------
      SUBROUTINE MFBLKOUT(WSID)

      IMPLICIT NONE

C INPUT:
c--port      INCLUDE 'SYS$LIBRARY:GKSDEFS.BND'
      INCLUDE 'GKS_ROOT:include/fgksenum.inc'
      INCLUDE 'mapper_port.inc'

      INTEGER WSID
      INTEGER I,J,K,L

      REAL DVMAXX,DVMAXY


C INQUIRE ABOUT THE MAXIMUM SIZE OF THE WORKSTATION WINDOW
CCCCCCCALL GQDSP (GV2000,I,J,DVMAXX,DVMAXY,K,L)
      CALL GQDSP (GMOTIF,I,J,DVMAXX,DVMAXY,K,L)

C MAKE THE NEW WINDOW VERY SMALL
      CALL GSWKVP (WSID,0.0,0.02*DVMAXX,0.0,0.02*DVMAXY)

C UPDATE THE WORKSTATION
      CALL GUWK(WSID,GPERFO)

 9999 CONTINUE
      RETURN
      END
