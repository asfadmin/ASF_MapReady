C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	metafile.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================

C-----------------------------------------------------------------
C SUBROUTINE METAFILE
C
C PURPOSE
C	READ IN THE GKS METAFILE AND PLACE THE SEGMENTS IN
C	THE GKSM WORKSTATION
C
C $Logfile:   ACS003:[BLD.MPS.MAPR.SRC]METAFILE.FOV  $
C
C INPUT
C	MWID		METAFILE WORKSTATION ID
C	MWFIL		METAFILE FILENAME
C	PROJN		PROJECTION
C	MINLAT,MAXLAT
C	MINLON,MAXLON	MIN/MAX LAT/LON OF SAVED MAP
C	OBSLAT,OBSLON	CENTER LAT/LON OF SAVED MAP
C OUTPUT
C	OBSMIN,OBSMAX	MIN/MAX LON OF SAVED MAP
C	LONPT		LONGITUDE POINTS
C
C INTERNAL
C	DISP	    	DISPLAY - WORLD OR METAFILE
C	ITYPE
C	DRLEN
C	MAXDRL
C	DIMDR
C	ERR,MEM,I,J
C
C WRITTEN BY RICHARD P. LEE
C MODIFIED BY CRAIG K. FUJIMOTO
C
C MODIFICATIONS
C $Date$ $Revision$ $Author$
C 8/8/94 Nadia Adhami  replaced GKS$OPEN_WS with Prior GKS calls
C
C-----------------------------------------------------------------
       SUBROUTINE METAFILE(MWID,MWFIL,PROJN,
     1                     MINLAT,MAXLAT,MINLON,MAXLON,
     2                     OBSLAT,OBSLON,OBSMIN,OBSMAX,
     4                     LONPT)

      character*100 SccsFileID
     -/'@(#)metafile.for	5.1 98/01/08 APS/ASF\0'/
                    
       IMPLICIT NONE

c--port       INCLUDE 'SYS$LIBRARY:GKSDEFS.FOR'
c--port       INCLUDE 'SYS$LIBRARY:GKSDEFS.BND'
       INCLUDE 'GKS_ROOT:include/fgksenum.inc'

C INPUT
       CHARACTER*(*) MWFIL
       INTEGER MWID,PROJN
       REAL MINLAT,MAXLAT,MINLON,MAXLON
       REAL OBSLAT,OBSLON
C OUTPUT
       REAL OBSMIN,OBSMAX
       REAL LONPT(*)
C INTERNAL
       CHARACTER*80 IDR(10000)
       INTEGER ITYPE,DRLEN,MAXDR
       INTEGER DIMDR,J

       DATA DIMDR / 10000 /
       DATA MAXDR / 10000 /
                    
       REAL DIFLON,DIFLAT
       REAL LONPTS(13)

       DATA LONPTS / 180.01,210.0,240.0,270.0,300.0,330.0,
     1               0.0,30.0,60.0,90.0,120.0,150.0,179.99 /

C-----------------------------------------------------------------

C OPEN THE MAP WINDOW FILE
c--port      CALL GKS$OPEN_WS (MWID,MWFIL,GKS$K_GKSM_INPUT)

        OPEN (UNIT=9, FILE=MWFIL, STATUS='UNKNOWN')
        CALL GOPWK( MWID, 9, 2 )


C READ, INTERPRET, AND DISPLAY THE MAP WINDOW FILE

 1000 CONTINUE

      CALL GGTITM (MWID,ITYPE,DRLEN)
      IF (ITYPE .NE. 0) THEN
        CALL GRDITM (MWID,DRLEN,MAXDR,IDR)
        CALL GIITM (ITYPE,DRLEN,DIMDR,IDR)
        GO TO 1000
      END IF

C CLOSE THE MAP WINDOW FILE
      CALL GCLWK (MWID)


C DETERMINE DIFLAT,DIFLON
      CALL DIFDEG(OBSLON,MINLAT,MAXLAT,MINLON,MAXLON,
     1            DIFLAT,DIFLON)

C IF CYLINDRICAL EQUIDISTANT OR MERCATOR, AND CENTER LONGITUDE IS NOT
C EQUAL TO 0 DEG

      IF ((PROJN .EQ. 2 .OR. PROJN .EQ. 3) .AND.
     1    (OBSLON .NE. 0.0)) THEN

C   DETERMINE LONGITUDE GRID DIVISIONS BASED ON THE CENTER LONGITUDE

        DO 20 J = 1,13
          LONPT(J) = LONPTS(J)
   20   CONTINUE

C   IF CHOSEN RE-CENTERING LONGITUDE IS 0,-360,-180,180, RESET THE 
C      RE-CENTERING LONGITUDE 

        IF (OBSLON .EQ. -360.0 .OR. OBSLON .EQ. 0.0 .OR.
     1    OBSLON .EQ. 360.0) THEN
          LONPT(13) = 179.99
        ELSE IF (OBSLON .EQ. -180.0 .OR. OBSLON .EQ. 180.0) THEN
          OBSLON = 180.01
          LONPT(13) = 0.01           
        ELSE IF (OBSLON .EQ. -179.99 .OR. OBSLON .EQ. 180.01) THEN
          OBSLON = 180.0101
          LONPT(13) = OBSLON
        ELSE IF (OBSLON .EQ. -180.01 .OR. OBSLON .EQ. 179.99) THEN
          OBSLON = 179.9899
          LONPT(1) = OBSLON

C   RE-CALCULATE THE LONGITUDE DIVISIONS BASED ON THE RE-CENTERING 
C     LONGITUDE VALUE

        ELSE
          DO 30 J = 1,6
            IF (OBSLON .EQ. LONPT(J) .OR. 
     1         (OBSLON + 360.0) .EQ. LONPT(J)) THEN
              IF ((OBSLON + 360.0) .EQ. LONPT(J)) 
     1           OBSLON = OBSLON + 360.0
              LONPT(J + 6) = LONPT(J + 6) - 0.01
              LONPT(13) = LONPT(J + 6) + 0.02
              GO TO 2000
            END IF
   30     CONTINUE
          DO 40 J = 7,12
            IF (OBSLON .EQ. LONPT(J) .OR.
     1          (OBSLON + 360.0) .EQ. LONPT(J)) THEN
              IF ((OBSLON + 360.0) .EQ. LONPT(J)) 
     1           OBSLON = OBSLON + 360.0
              LONPT(J - 6) = LONPT(J - 6) - 0.01
              LONPT(13) = LONPT(J - 6) + 0.02
              GO TO 2000
            END IF
   40     CONTINUE
          LONPT(13) = LONPT(7)
        END IF
      END IF

 2000 CONTINUE

C SET THE MINIMUM AND MAXIMUM WINDOW LONGITUDE

      OBSMIN = MINLON
      OBSMAX = MAXLON
      IF (DIFLON .GE. 359.97) THEN
        OBSMIN = OBSLON + 180.01
        OBSMAX = OBSLON + 179.99
      END IF
  200 IF (OBSMIN .GT. 360.0) THEN 
        OBSMIN = OBSMIN - 360.0
        GO TO 200
      END IF
  205 IF (OBSMAX .GT. 360.0) THEN 
        OBSMAX = OBSMAX - 360.0
        GO TO 205
      END IF
  210 IF (OBSLON .GT. 360.0) THEN 
        OBSLON = OBSLON - 360.0
        GO TO 210
      END IF
                                
 9999 CONTINUE
      RETURN
      END
