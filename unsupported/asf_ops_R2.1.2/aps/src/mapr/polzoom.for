C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	polzoom.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================

C-----------------------------------------------------------------------
C
C SUBROUTINE POLZOOM
C
C $Logfile:   ACS003:[BLD.MPS.MAPR.SRC]POLZOOM.FOV  $
C
C PURPOSE
C	PROMPT THE USER FOR THE INNER AND OUTER LATS AND BEGIN
C	AND END LONS OF THE ZOOM WINDOW
C
C	THE LONS MUST BE ENTERED IN A CLOCKWISE DIRECTION
C
C INPUT
C	PROJN		PROJECTION NUMBER
C
C OUTPUT
C	MINLAT,MAXLAT   INNER/OUTER LAT FOR POLAR STEREO
C	MINLON,MAXLON	START/STOP LON IN CLOCKWISE DIRECTION
C	IOS		STATUS
C
C INTERNAL
C	SAVELL		SAVED MIN/MAX LATS/LONS
C
C WRITTEN BY CRAIG K. FUJIMOTO - SEP 89
C
C $Date$ $Revision$ $Author$
C-----------------------------------------------------------------------
      SUBROUTINE POLZOOM(PROJN,
     1                   MINLAT,MAXLAT,MINLON,MAXLON,
     2                   IOS)

      character*100 SccsFileID
     -/'@(#)polzoom.for	5.1 98/01/08 APS/ASF\0'/
                         
      IMPLICIT NONE

C INPUT
      INTEGER PROJN

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

C INNER ZOOM WINDOW LATITUDE

      IF (PROJN .EQ. 5) THEN
        CALL ASK_LATLON(MINLAT,'Inner latitude',0.0,90.0)   
      ELSE
        CALL ASK_LATLON(MINLAT,'Inner latitude',-90.0,0.0)   
      END IF

C OUTER ZOOM WINDOW LATITUDE

      IF (PROJN .EQ. 5) THEN
        CALL ASK_LATLON(MAXLAT,'Outer latitude',0.0,MINLAT)
      ELSE
        CALL ASK_LATLON(MAXLAT,'Outer latitude',MINLAT,0.0)
      END IF 
                
C CHECK TO SEE IF MINIMUM LATITUDE > MAXIMUM LATITUDE
                       
      IF (PROJN.EQ.5 .AND. MINLAT.LE.MAXLAT) THEN
        CALL DISMSG('Error : Inner lat must be > outer lat.')
        IOS = 1
        GO TO 8000
      ELSE IF (PROJN.EQ.6 .AND. MINLAT.GE.MAXLAT) THEN
        CALL DISMSG('Error : Inner lat must be < outer lat.')
        IOS = 1
        GO TO 8000
      END IF

C CHECK TO SEE IF THE WINDOW HEIGHT < 5 DEGREES

      IF (ABS(MAXLAT - MINLAT) .LT. 5.0) THEN
        CALL DISMSG('Error : Minimum window is 5 by 5 degrees.')
        IOS = 1
        GO TO 8000
      END IF

C MINIMUM ZOOM WINDOW LONGITUDE

       CALL ASK_LATLON(MINLON,
     1                 'Begin longitude CW direction',-180.0,180.0)
            
C MAXIMUM ZOOM WINDOW LONGITUDE

       CALL ASK_LATLON(MAXLON,
     1                 'End longitude CW direction',-180.0,180.0)
                 

      IOS = 0
      GO TO 9999

C UPON ERROR RESTORE ORIGINAL VALUES
 8000 CONTINUE
      MINLAT = SAVELL(1)
      MAXLAT = SAVELL(2)
      MINLON = SAVELL(3)
      MAXLON = SAVELL(4)

 9999 CONTINUE
      RETURN
      END
