C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	difdeg.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================

C-----------------------------------------------------------------
C SUBROUTINE DIFDEG
C
C PURPOSE
C	CALC THE HEIGHT AND WIDTH OF CURRENT WINDOW IN DEGREES
C
C $Logfile:   ACS003:[BLD.MPS.MAPR.SRC]DIFDEG.FOV  $
C
C INPUT
C	OBSLON		CENTER LON
C	MINLAT,MAXLAT	MIN/MAX LAT OF WINDOW
C	MINLON,MAXLON	MIN/MAX LON OF WINDOW
C
C OUTPUT
C	DIFLAT,DIFLON	HEIGHT,WIDTH ON WINDOW IN DEG
C
C SUBROUTINES CALLED
C
C QRIGINALLY WRITTEN BY RICHARD P. LEE
C MODIFIED FOR ASF BY CRAIG K. FUJIMOTO - SEP 89
C
C MODIFICATIONS
C $Date$ $Revision$ $Author$
C-----------------------------------------------------------------
      SUBROUTINE DIFDEG (OBSLON,MINLAT,MAXLAT,MINLON,MAXLON,
     1                   DIFLAT,DIFLON)

      character*100 SccsFileID
     -/'@(#)difdeg.for	5.1 98/01/08 APS/ASF\0'/

      IMPLICIT NONE
C INPUT:
      REAL OBSLON
      REAL MINLAT,MAXLAT,MINLON,MAXLON
C OUTPUT:
      REAL DIFLAT,DIFLON
C---------------------------------------------------------------------- 

C        WRITE(6,*) 'BEFORE DIFDEG'
C        WRITE(6,*) 'MIN/MAX LT/LN:',MINLAT,MAXLAT,MINLON,MAXLON

C IF MINIMUM OR MAXIMUM LONGITUDE IS -180 OR 180, RESET THE LONGITUDES

      IF (MAXLON .EQ. 180.0) MAXLON = 179.99
      IF (MINLON .EQ. -180.0 .OR. MINLON .EQ. 180.0) MINLON = 180.01

      IF (MINLON .GE. 180.0) MINLON = MINLON - 360.0
      IF (MAXLON .GE. 180.0) MAXLON = MAXLON - 360.0

C WINDOW HEIGHT
 
      DIFLAT = MAXLAT - MINLAT

C WINDOW LENGTH

      IF (MINLON .LT. MAXLON) THEN
        DIFLON = MAXLON - MINLON
      ELSE IF (MINLON .GT. MAXLON) THEN
        DIFLON = 360.0 - MINLON + MAXLON
      END IF

C        WRITE(6,*) 'AFTER DIFDEG'
C        WRITE(6,*) 'MIN/MAX LT/LN:',MINLAT,MAXLAT,MINLON,MAXLON
C        WRITE(6,*) 'DIF LAT/LON:',DIFLAT,DIFLON

 9999 CONTINUE
      RETURN
      END
