C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	merzoom.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================

C-----------------------------------------------------------------------
C SUBROUTINE MERZOOM
C
C $Logfile:   ACS003:[BLD.MPS.MAPR.SRC]MERZOOM.FOV  $
C
C PURPOSE
C	GET MIN/MAX LATS/LONS FOR MERCATOR AND CYLINDRICAL PROJECTIONS
C
C INPUT
C	DISP		DISPLAY FROM MAP FILE, WORLD MAP
C	PROJN		PROJECTION NUMBER
C	OBSMIN,OBSMAX	179.99 OFF CENTER LON (OBSLON)
C	STMNLT,STMXLT
C	STMNLN,STMXLN	MIN/MAX LATS/LONS OF ORIGINAL WINDOW
C
C OUTPUT
C	MINLAT,MAXLAT
C	MINLON,MAXLON	MIN/MAX LATS/LONS OF WINDOW
C	IOS		STATUS
C
C WRITTEN BY RICHARD P. LEE
C
C $Date$ $Revision$ $Author$
C-----------------------------------------------------------------------

      SUBROUTINE MERZOOM(DISP,PROJN,
     1                   MINLAT,MAXLAT,MINLON,MAXLON,
     1                   OBSMIN,OBSMAX,
     2                   STMNLT,STMXLT,STMNLN,STMXLN,
     4                   IOS)

      character*100 SccsFileID
     -/'@(#)merzoom.for	5.1 98/01/08 APS/ASF\0'/

      IMPLICIT NONE

C INPUT
      INTEGER DISP,PROJN

      REAL OBSMIN,OBSMAX
      REAL STMNLT,STMXLT,STMNLN,STMXLN

C OUTPUT
      REAL MINLAT,MAXLAT,MINLON,MAXLON
      INTEGER IOS

C INTERNAL
      REAL SAVELL(4)
C-----------------------------------------------------------------------

 1000 CONTINUE

      SAVELL(1) = MINLAT
      SAVELL(2) = MAXLAT
      SAVELL(3) = MINLON
      SAVELL(4) = MAXLON

C   MINIMUM ZOOM WINDOW LATITUDE

      IF (PROJN .EQ. 2 .OR. PROJN .EQ. 4) THEN

        CALL ASK_LATLON(MINLAT,'Minimum latitude',-90.0,90.0)   
        IF (MINLAT .EQ. -90.0) MINLAT = -89.99

      ELSE IF (PROJN .EQ. 3) THEN

        CALL ASK_LATLON(MINLAT,'Minimum latitude',-85.0,85.0)   
        IF (MINLAT .LT. -85.0) MINLAT = -85.0
        IF (MINLAT .GT. 85.0) MINLAT = 85.0

      END IF 

C   MAXIMUM ZOOM WINDOW LATITUDE

      IF (PROJN .EQ. 2 .OR. PROJN .EQ. 4)THEN

        CALL ASK_LATLON(MAXLAT,'Maximum latitude',-90.0,90.0)
        IF (MAXLAT .EQ. 90.0) MAXLAT = 89.99

      ELSE IF (PROJN .EQ. 3) THEN

        CALL ASK_LATLON(MAXLAT,'Maximum latitude',-85.0,85.0)
        IF (MAXLAT .LT. -85.0) MAXLAT = -85.0
        IF (MAXLAT .GT. 85.0) MAXLAT = 85.0

      END IF  

C   CHECK TO SEE IF MINIMUM LATITUDE > MAXIMUM LATITUDE

      IF (MINLAT .GE. MAXLAT) THEN
        CALL DISMSG('Error : Min lat must be below max lat.')
        IOS = 1
        GO TO 8000
      END IF

C   CHECK TO SEE IF THE WINDOW HEIGHT < 5 DEGREES

      IF ((MAXLAT - MINLAT) .LT. 5.0) THEN
        CALL DISMSG('Error : Minimum window is 5 by 5 degrees.')
        IOS = 1
        GO TO 8000
      END IF


C   MINIMUM ZOOM WINDOW LONGITUDE

       CALL ASK_LATLON(MINLON,'Minimum longitude',-180.0,180.0)
            
       IF (DISP .EQ. 1) THEN
         IF (MINLON .EQ. (OBSMIN - 0.01)) MINLON = OBSMIN
       ELSE
         IF (MINLON .EQ. (STMNLN - 0.01)) MINLON = STMNLN
       END IF

C       IF (MINLON .LT. 180.0) MINLON = MINLON + 360.0

C   MAXIMUM ZOOM WINDOW LONGITUDE

       CALL ASK_LATLON(MAXLON,'Maximum longitude',-180.0,180.0)
                 
       IF (DISP .EQ. 1) THEN
         IF (MAXLON .EQ. (OBSMAX + 0.01)) MAXLON = OBSMAX
       ELSE
         IF (MAXLON .EQ. (STMXLN + 0.01)) MAXLON = STMXLN
       END IF

C       IF (MAXLON .GT. 180.0) MAXLON = MAXLON - 360.0

C IF THE USER CHOSE THE ENTIRE LENGTH OF THE WINDOW

       IF (MINLON .EQ. MAXLON) THEN
         MINLON = OBSMIN
         MAXLON = OBSMIN
C         MINLON = MINLON + 0.01
C         MAXLON = MAXLON - 0.01
       END IF

       IOS = 0
       GO TO 9999

C UPON ERROR RESTORE ORIGINAL VALUES
 8000  CONTINUE
       MINLAT = SAVELL(1)
       MAXLAT = SAVELL(2)
       MINLON = SAVELL(3)
       MAXLON = SAVELL(4)

 9999  CONTINUE
       RETURN
       END

        
C-----------------------------------------------------------------------
C SUBROUTINE MERCHK
C
C PURPOSE
C	CHECK THE FIVE DEGREE MINIMUM AND DISPLAY THE LINE AROUND
C	THE SELECTED ZOOM AREA
C
C INPUT
C	PROJN		PROJECTION NUMBER
C	OBSLAT,OBSLON	CENTER LAT/LON
C	MINLAT,MAXLAT	MIN/MAX LATITUDE OF WINDOW
C	MINLON,MAXLON	"       LONGITUDE " "
C
C OUTPUT
C	IOS		I/O STATUS
C
C INTERNAL
C	X,Y		X,Y VALUES FOR DRAWING LINE AROUND ZOOM AREA
C	ZMMNX,ZMMXX	ZOOM MIN/MAX X
C	ZMMNY,ZMMXX	ZOOM MIN/MAX Y
C	FIVEDEGS	FIVE DEGREES IN THE X DIRECTION
C	FX1,FX2		TEMP HOLDERS FOR X TO DETERMINE FIVEDEGS
C	TEMP		<
C      	HIDDEN,BRNCUT	LTRANS RETRUN VALUES
C
C WRITTEN BY CRAIG K. FUJIMOTO - SEP 89
C-----------------------------------------------------------------------
       SUBROUTINE MERCHK(PROJN,OBSLAT,OBSLON,
     1                   MINLAT,MAXLAT,MINLON,MAXLON,
     1                   IOS)

       IMPLICIT NONE

C INPUT
       INTEGER PROJN
       REAL OBSLAT,OBSLON
       REAL MINLAT,MAXLAT,MINLON,MAXLON

C OUTPUT
       INTEGER IOS

C INTERNAL
       REAL X(5),Y(5)
       REAL ZMMNX,ZMMNY,ZMMXX,ZMMXY
       REAL FIVEDEGS
       REAL FX1,FX2,TEMP

       LOGICAL HIDDEN,BRNCUT

C-----------------------------------------------------------------------

C GET X DISTANCE FOR 5 DEGREES OF LONGITUDE

      CALL LTRANS (PROJN,OBSLAT,OBSLON,
     1             0.0,0.0,
     2             FX1,TEMP,
     3             HIDDEN,BRNCUT)

      CALL LTRANS (PROJN,OBSLAT,OBSLON,
     1             5.0,0.0,
     2             FX2,TEMP,
     3             HIDDEN,BRNCUT)

      FIVEDEGS = FX2 - FX1
       

C TRANSLATE THE LAT/LONS TO X/Y 

       CALL LTRANS (PROJN,OBSLAT,OBSLON,
     1              MINLON,MINLAT,
     2              ZMMNX,ZMMNY,
     3              HIDDEN,BRNCUT)
       
       CALL LTRANS (PROJN,OBSLAT,OBSLON,
     1              MAXLON,MAXLAT,
     2              ZMMXX,ZMMXY,
     3              HIDDEN,BRNCUT)

C CHECK THAT MIN LON IS TO THE LEFT OF MAX LON

       IF (ZMMNX .GE. ZMMXX) THEN

         CALL DISMSG('Error : Min lon must be to the left of max lon.')
         IOS = 1
         GO TO 9999

C   CHECK TO SEE IF THE WINDOW LENGTH < 5 DEGREES

       ELSE IF (ZMMXX - ZMMNX .LT. FIVEDEGS) THEN

         CALL DISMSG('Error : Minimum window is 5 by 5 degrees.')
         IOS = 1
         GO TO 9999

       END IF
        
C FILL THE DISPLAY ARRAYS

       X(1) = ZMMXX
       X(2) = ZMMNX
       X(3) = ZMMNX
       X(4) = ZMMXX
       X(5) = ZMMXX

       Y(1) = ZMMNY
       Y(2) = ZMMNY
       Y(3) = ZMMXY
       Y(4) = ZMMXY
       Y(5) = ZMMNY

C DISPLAY A LINE AROUND THE ZOOM AREA
       CALL GPL(5,X,Y)

       IOS = 0

 9999 CONTINUE
      RETURN
      END        
