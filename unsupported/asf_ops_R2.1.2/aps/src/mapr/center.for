C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	center.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================

C-----------------------------------------------------------------
C SUBROUTINE CENTER
C
C PURPOSE
C	CENTER THE INITIAL WORLD MAP
C
C $LOGFILE$
C
C INPUT/OUTPUT
C	PROJN		PROJECTION NO.
C	OBSLAT,OBSLON	CENTER LAT/LON
C	OBSMIN,OBSMAX	MIN/MAX LON OF WINDOW
C	LONPT		ARRAY OF LONGITUDE DIVISIONS
C
C INTERNAL
C	LONPTS		ARRAY OF INITIAL LON DIVISIONS
C	IOS		I/O STATUS
C	I		LOOP INDEX
C
C SUBROUTINES CALLED
C
C QRIGINALLY WRITTEN BY RICHARD P. LEE
C MODIFIED FOR ASF BY CRAIG K. FUJIMOTO - SEP 89
C
C MODIFICATIONS
C $DATE$ $REVISION$ $AUTHOR$
C-----------------------------------------------------------------
       SUBROUTINE CENTER (PROJN,OBSLAT,OBSLON,OBSMIN,OBSMAX,LONPT)

      character*100 SccsFileID
     -/'@(#)center.for	5.1 98/01/08 APS/ASF\0'/

       IMPLICIT NONE

C INPUT:
       INTEGER PROJN                              

C OUTPUT:
       REAL OBSLAT,OBSLON,OBSMIN,OBSMAX,LONPT(*)

C INTERNAL:
       INTEGER I
       REAL  LONPTS(13)
       DATA LONPTS / 180.01,210.0,240.0,270.0,300.0,330.0,
     1               0.0,30.0,60.0,90.0,120.0,150.0,179.99 /

C-----------------------------------------------------------------------

C SATELLITE VIEW
       IF (PROJN .EQ. 1) THEN

C CENTER LATITUDE
         CALL ASK_LATLON(OBSLAT,'Center latitude',-90.0,90.0)

C CENTER LONGITUDE
         CALL ASK_LATLON(OBSLON,'Center longitude',-180.0,180.0)

C POLAR STEREOGRAPHIC PROJECTIONS
       ELSE IF (PROJN .EQ. 5 .OR. PROJN .EQ. 6) THEN

C CENTER LATITUDE
         IF (PROJN .EQ. 5) THEN
           OBSLAT = 90.0
         ELSE
           OBSLAT = -90.0
         END IF

C CENTER LONGITUDE
         CALL ASK_LATLON(OBSLON,'Longitude reference',-180.0,180.0)

C ALL OTHER PROJECTIONS
        ELSE 

C CENTER LONGITUDE
         CALL ASK_LATLON(OBSLON,'Center longitude',-180.0,180.0)

C CENTER LATITUDE
         OBSLAT = 0.0

       END IF

C SET LONGITUDE DIVISION ARRAY TO INITIAL VALUES

       DO 50 I = 1,13
         LONPT(I) = LONPTS(I)
   50  CONTINUE

C IF CHOSEN CENTER LONGITUDE IS 0,-360,-180,180, RESET THE 
C    CENTER LONGITUDE 

         IF (OBSLON .EQ. -360.0 .OR. OBSLON .EQ. 0.0 .OR.
     1       OBSLON .EQ. 360.0) THEN
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

C CALCULATE THE LONGITUDE DIVISIONS BASED ON THE CENTER 
C   LONGITUDE VALUE

         ELSE

           DO 500 I = 1,6
             IF (OBSLON .EQ. LONPT(I) .OR. 
     1           (OBSLON + 360.0) .EQ. LONPT(I)) THEN
               IF ((OBSLON + 360.0) .EQ. LONPT(I)) 
     1            OBSLON = OBSLON + 360.0
               LONPT(I + 6) = LONPT(I + 6) - 0.01
               LONPT(13) = LONPT(I + 6) + 0.02
               GO TO 1000
             END IF
  500      CONTINUE

           DO 600 I = 7,12
             IF (OBSLON .EQ. LONPT(I) .OR.
     1           (OBSLON + 360.0) .EQ. LONPT(I)) THEN
               IF ((OBSLON + 360.0) .EQ. LONPT(I)) 
     1            OBSLON = OBSLON + 360.0
               LONPT(I - 6) = LONPT(I - 6) - 0.01
               LONPT(13) = LONPT(I - 6) + 0.02
               GO TO 1000
             END IF
  600      CONTINUE

           LONPT(13) = LONPT(7)

         END IF

 1000  CONTINUE

C SET THE MINIMUM AND MAXIMUM WINDOW LONGITUDE AFTER CENTERING

       OBSMIN = OBSLON - 179.99
       OBSMAX = OBSLON + 179.99

       IF (OBSMIN .LE. -180.0) OBSMIN = OBSMIN + 360.0
       IF (OBSMAX .GE. 180.0) OBSMAX = OBSMAX - 360.0


 9999  CONTINUE
       RETURN
       END
