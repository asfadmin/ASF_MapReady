C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	grid_setup.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================

C-----------------------------------------------------------------------
C SUBROUTINE GRID_SETUP
C
C $Logfile:   ACS003:[BLD.MPS.MAPR.SRC]GRID_SETUP.FOV  $
C
C PURPOSE
C	SET UP THE GRID DIVISIONS AND POSITIONS FOR THE:
C		MERCATOR,
C		CYLINDRICAL EQUIDISTANT
C		MILLER CYLINDRICAL
C	PROJECTIONS.
C
C INPUT
C	PROJN		PROJECTION NUMBER
C	MINLAT,MAXLAT,
C	MINLON,MAXLON	DISPLAY WINDOW DIMENSIONS - DEGREES
C	DIFLAT,DIFLON	DISPLAY WINDOW HEIGHT/LENGTH - DEGREES
C	LONPT()		30 DEG LONGITUDE GRID DIVISIONS (DEG)
C
C OUTPUT
C	DEG(1,*)	LONGITUDE GRID DIVISIONS
C	DEG(2,*)	LATITUDE GRID DIVISIONS
C	NDEG(1)		NUMBER OF LONGITUDE DIVISIONS
C	NDEG(2)		NUMBER OF LATITUDE DIVISIONS
C	LPOS(1,*)	LONGITUDE GRID LINE POSITIONS
C	LPOS(2,*)	LATITUDE GRID LINE POSITIONS
C	NLPOS(1)	NUMBER OF LONGITUDE GRID 
C	NLPOS(2)	NUMBER OF LATITUDE GRID 
C
C INTERNAL
C	J1		INCREMENTAL DEGREES COUNTER (DEG)
C	LATPT(*)	LATITUDE GRID DIVISIONS (DEG)
C	LONPT1(*)	LONGITUDE GRID DIVISIONS (DEG)
C	LONPT2(*)	LONGITUDE GRID DIVISIONS (DEG)
C	I		GRID NUMBERS AND LINES COUNTER
C	J,K,L		DO LOOP VARIABLES
C
C ORIGINALLY WRITTEN BY RICHARD P. LEE    19 JUN 1989
C MODIFIED FOR ASF BY CRAIG K. FUJIMOTO
C
C $Date$ $Revision$ $Author$
C-----------------------------------------------------------------------
       SUBROUTINE GRID_SETUP (PROJN,MINLAT,MAXLAT,MINLON,MAXLON,
     1                        DIFLAT,DIFLON,LONPT,DEG,NDEG,
     1                        LPOS,NLPOS)

      character*100 SccsFileID
     -/'@(#)grid_setup.for	5.1 98/01/08 APS/ASF\0'/

       IMPLICIT NONE

C INPUT:
       INTEGER PROJN
       REAL MINLAT,MAXLAT,MINLON,MAXLON
       REAL DIFLAT,DIFLON,LONPT(13)

C OUTPUT:
       INTEGER NDEG(2),NLPOS(2)

       REAL DEG(2,25),LPOS(2,40)

C INTERNAL:
       INTEGER I,J,K,L

       REAL J1,LATPT(19),LONPT1(37),LONPT2(24)

       DATA LATPT / -89.99,-80.0,-70.0,-60.0,-50.0,-40.0,-30.0,-20.0,
     1              -10.0,0.0,10.0,20.0,30.0,40.0,50.0,60.0,70.0,80.0,
     2              89.99 /
       DATA LONPT1 / 180.01,190.0,200.0,210.0,220.0,230.0,240.0,
     1               250.0,260.0,270.0,280.0,290.0,300.0,310.0,
     2               320.0,330.0,340.0,350.0,0.0,10.0,20.0,30.0,
     3               40.0, 50.0,60.0,70.0,80.0,90.0,100.0,110.0,
     4               120.0,130.0,140.0,150.0,160.0,170.0,179.99 /
       DATA LONPT2 / 10.0,20.0,40.0,50.0,70.0,80.0,100.0,110.0,
     1               130.0,140.0,160.0,170.0,190.0,200.0,220.0,
     2               230.0,250.0,260.0,280.0,290.0,310.0,320.0,
     3               340.0,350.0 /

C-----------------------------------------------------------------------

C IF MERCATOR, RESET LATITUDE BOUNDARIES
       IF (PROJN .EQ. 3) THEN
         LATPT(1) = -85.0
         LATPT(19) = 85.0
       ELSE
         LATPT(1) = -89.99
         LATPT(19) = 89.99
       END IF

C GRID LONGITUDES

C SET COUNTER TO ZERO
       I = 0

C IF THE WINDOW LENGTH IS > 90 DEGREES, SET LONGITUDE GRID NUMBERS AT
C 30-DEGREE DIVISIONS AND GRID LINES AT 10-DEGREE DIVISIONS

       IF (DIFLON .GT. 90.0) THEN

C SET LONGITUDE GRID NUMBERS AT 30-DEGREE DIVISIONS

         DO 1000 J = 1,13

           IF (MINLON .GT. MAXLON) THEN
             IF (LONPT(J) .GE. 180.0) THEN
               IF ((LONPT(J) - 360.0) .GE. MINLON) THEN
                 I = I + 1
                 DEG(1,I) = LONPT(J) - 360.0
                 LPOS(1,I) = LONPT(J)
               ELSE IF ((LONPT(J) - 360.0) .LE. MAXLON) THEN
                 I = I + 1
                 DEG(1,I) = LONPT(J) - 360.0
                 LPOS(1,I) = LONPT(J)
               END IF
             ELSE
               IF (LONPT(J) .GE. MINLON) THEN
                 I = I + 1
                 DEG(1,I) = LONPT(J)
                 LPOS(1,I) = LONPT(J)
               ELSE IF (LONPT(J) .LE. MAXLON) THEN
                 I = I + 1
                 DEG(1,I) = LONPT(J)
                 LPOS(1,I) = LONPT(J)
               END IF
             END IF
           ELSE
             IF (LONPT(J) .GE. 180.0) THEN
               IF (((LONPT(J) - 360.0) .GE. MINLON) .AND. 
     1             ((LONPT(J) - 360.0) .LE. MAXLON)) THEN  
                 I = I + 1
                 DEG(1,I) = LONPT(J) - 360.0
                 LPOS(1,I) = LONPT(J)
               END IF
             ELSE
               IF ((LONPT(J) .GE. MINLON) .AND. 
     1             (LONPT(J) .LE. MAXLON)) THEN  
                 I = I + 1
                 DEG(1,I) = LONPT(J)
                 LPOS(1,I) = LONPT(J)
               END IF
             END IF
           END IF
 1000    CONTINUE

C SET THE NUMBER OF LONGITUDE GRID NUMBERS 

         NDEG(1) = I

C SET LONGITUDE GRID LINES AT 10-DEGREE DIVISIONS

         DO 1100 J = 1,24

           IF (MINLON .GT. MAXLON) THEN
             IF (LONPT2(J) .GE. 180.0) THEN
               IF ((LONPT2(J) - 360.0) .GE. MINLON) THEN
                 I = I + 1
                 LPOS(1,I) = LONPT2(J)
               ELSE IF ((LONPT2(J) - 360.0) .LE. MAXLON) THEN
                 I = I + 1
                 LPOS(1,I) = LONPT2(J)
               END IF
             ELSE
               IF (LONPT2(J) .GE. MINLON) THEN
                 I = I + 1
                 LPOS(1,I) = LONPT2(J)
               ELSE IF (LONPT2(J) .LE. MAXLON) THEN
                 I = I + 1
                 LPOS(1,I) = LONPT2(J)
               END IF
             END IF
           ELSE
             IF (LONPT2(J) .GE. 180.0) THEN
               IF (((LONPT2(J) - 360.0) .GE. MINLON) .AND.
     1             ((LONPT2(J) - 360.0) .LE. MAXLON)) THEN
                 I = I + 1
                 LPOS(1,I) = LONPT2(J)
               END IF
             ELSE
               IF ((LONPT2(J) .GE. MINLON) .AND. 
     1             (LONPT2(J) .LE. MAXLON)) THEN  
                 I = I + 1
                 LPOS(1,I) = LONPT2(J)
               END IF
             END IF
           END IF
 1100    CONTINUE

C SET THE NUMBER OF LONGITUDE GRID LINES 

         NLPOS(1) = I

C IF THE WINDOW LENGTH IS > 10 DEGREES, SET LONGITUDE GRID NUMBERS AND 
C GRID LINES AT 5-DEGREE DIVISIONS

       ELSE IF (DIFLON .GT. 10.0) THEN

C SET COUNTER TO ZERO

         I = 0

C SET LONGITUDE GRID NUMBERS AND GRID LINES AT 10-DEGREE DIVISIONS

         DO 1200 J = 1,37

           IF (MINLON .GT. MAXLON) THEN
             IF (LONPT1(J) .GE. 180.0) THEN
               IF ((LONPT1(J) - 360.0) .GE. MINLON) THEN
                 I = I + 1
                 DEG(1,I) = LONPT1(J) - 360.0
                 LPOS(1,I) = LONPT1(J)
               ELSE IF ((LONPT1(J) - 360.0) .LE. MAXLON) THEN
                 I = I + 1
                 DEG(1,I) = LONPT1(J) - 360.0
                 LPOS(1,I) = LONPT1(J)
               END IF
             ELSE
               IF (LONPT1(J) .GE. MINLON) THEN
                 I = I + 1
                 DEG(1,I) = LONPT1(J)
                 LPOS(1,I) = LONPT1(J)  
               ELSE IF (LONPT1(J) .LE. MAXLON) THEN
                 I = I + 1
                 DEG(1,I) = LONPT1(J)
                 LPOS(1,I) = LONPT1(J)
               END IF
             END IF
           ELSE
             IF (LONPT1(J) .GE. 180.0) THEN
               IF (((LONPT1(J) - 360.0) .GE. MINLON) .AND.
     1             ((LONPT1(J) - 360.0) .LE. MAXLON)) THEN
                 I = I + 1
                 DEG(1,I) = LONPT1(J) - 360.0
                 LPOS(1,I) = LONPT1(J)
               END IF
             ELSE
               IF ((LONPT1(J) .GE. MINLON) .AND. 
     1             (LONPT1(J) .LE. MAXLON)) THEN
                 I = I + 1
                 DEG(1,I) = LONPT1(J)
                 LPOS(1,I) = LONPT1(J)
               END IF
             END IF
           END IF
 1200    CONTINUE
         K = I

C SET LONGITUDE GRID NUMBERS AND GRID LINES AT 5-DEGREE DIVISIONS

         DO 1300 L = 1,K
           IF (L .EQ. 1) THEN
             IF ((DEG(1,1) - 5.0) .GE. MINLON) THEN
               I = I + 1
               DEG(1,I) = REAL(NINT(DEG(1,1) - 5.0))
               LPOS(1,I) = REAL(NINT(LPOS(1,1) - 5.0))
             END IF
           END IF
           IF ((DEG(1,L) + 5.0) .LE. MAXLON) THEN
             I = I + 1
             DEG(1,I) = REAL(NINT(DEG(1,L) + 5.0))
             LPOS(1,I) = REAL(NINT(LPOS(1,L) + 5.0))
           END IF
 1300    CONTINUE

C SET THE NUMBER OF LONGITUDE GRID NUMBERS AND GRID LINES 

         NDEG(1) = I
         NLPOS(1) = I

C IF THE WINDOW LENGTH IS < OR = 10 DEGREES, SET LONGITUDE GRID NUMBERS
C AND GRID LINES AT 1-DEGREE DIVISIONS

       ELSE IF (DIFLON .LE. 10.0) THEN
         I = 0
         J1 = 1.0
         IF ((NINT(MINLON) .GE. MINLON) .OR. 
     1       (MINLON .EQ. 180.01)) THEN
           I = I + 1
           DEG(1,I) = MINLON
           LPOS(1,I) = MINLON
         END IF
         IF ((MINLON .GE. 180.0) .AND. 
     1       (MAXLON .LE. 180.0)) THEN
 1400      CONTINUE
           IF (NINT(MINLON + J1) .LT. 360) THEN
             I = I + 1
             DEG(1,I) = MINLON + J1
             LPOS(1,I) = MINLON + J1
             J1 = J1 + 1.0
             GO TO 1400
           ELSE IF (NINT(MINLON + J1) .GE. 360) THEN
             IF (NINT(MINLON + J1 - 360.0) .LE. MAXLON) THEN
               I = I + 1
               DEG(1,I) = MINLON + J1 - 360.0
               LPOS(1,I) = MINLON + J1 - 360.0
               J1 = J1 + 1.0
               GO TO 1400
             END IF
           END IF
         ELSE
 1500      CONTINUE
           IF (NINT(MINLON + J1) .LE. MAXLON) THEN
             I = I + 1
             DEG(1,I) = MINLON + J1
             LPOS(1,I) = MINLON + J1
             J1 = J1 + 1.0
             GO TO 1500
           END IF
         END IF

C SET THE NUMBER OF LONGITUDE GRID NUMBERS AND GRID LINES 

         NDEG(1) = I
         NLPOS(1) = I
       END IF

C GRID LATITUDES

C SET COUNTER TO ZERO

       I = 0

C IF THE WINDOW HEIGHT IS > 10 DEGREES, SET LATITUDE GRID NUMBERS
C AND GRID LINES AT 10-DEGREE DIVISIONS

       IF (DIFLAT .GT. 10.0) THEN
         DO 1600 J = 1,19
           IF ((LATPT(J) .GE. MINLAT) .AND. 
     1         (LATPT(J) .LE. MAXLAT)) THEN
             I = I + 1 
             DEG(2,I) = LATPT(J)
             LPOS(2,I) = LATPT(J)
           END IF
 1600    CONTINUE

C IF THE WINDOW HEIGHT IS < OR = 90 DEGREES, SET LATITUDE GRID NUMBERS
C AND GRID LINES AT 5-DEGREE DIVISIONS

         IF (DIFLAT .LE. 90.0) THEN
           K = I
           DO 1700 L = 1,K
             IF (L .EQ. 1) THEN
               IF ((DEG(2,1) - 5.0) .GE. MINLAT) THEN
                 I = I + 1
                 DEG(2,I) = REAL(NINT(DEG(2,1) - 5.0))
                 LPOS(2,I) = REAL(NINT(LPOS(2,1) - 5.0))
               END IF
             END IF
             IF ((DEG(2,L) + 5.0) .LE. MAXLAT) THEN
               I = I + 1
               DEG(2,I) = REAL(NINT(DEG(2,L) + 5.0))
               LPOS(2,I) = REAL(NINT(LPOS(2,L) + 5.0))
             END IF
 1700      CONTINUE
         END IF

C SET THE NUMBER OF LATITUDE GRID NUMBERS AND GRID LINES 

         NDEG(2) = I
         NLPOS(2) = I

C IF THE WINDOW HEIGHT IS < OR = 10 DEGREES, SET LATITUDE GRID NUMBERS 
C AND GRID LINES AT 1-DEGREE DIVISIONS

       ELSE IF (DIFLAT .LE. 10.0) THEN
         I = 0
         J1 = 1.0
         IF (NINT(MINLAT) .GE. MINLAT) THEN
           I = I + 1
           DEG(2,I) = MINLAT
           LPOS(2,I) = MINLAT
         END IF
 1800    CONTINUE
         IF (NINT(MINLAT + J1) .LE. MAXLAT) THEN
           I = I + 1
           DEG(2,I) = MINLAT + J1
           LPOS(2,I) = MINLAT + J1
           J1 = J1 + 1.0
           GO TO 1800
         END IF

C SET THE NUMBER OF LATITUDE GRID NUMBERS AND GRID LINES 

         NDEG(2) = I
         NLPOS(2) = I
       END IF  
                
C END OF SUBROUTINE

 9999  CONTINUE
       RETURN
       END
