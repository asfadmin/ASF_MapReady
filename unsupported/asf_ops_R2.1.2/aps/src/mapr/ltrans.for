C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	ltrans.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================

C-----------------------------------------------------------------------
C SUBROUTINE LTRANS
C
C PURPOSE
C	CALCULATES THE LAT-LON TO X-Y COORD TRANSFORMATION AND 
C	INDICATES ANY HIDDEN OR CUT-OFF POINTS.
C
C $Logfile:   ACS003:[BLD.MPS.MAPR.SRC]LTRANS.FOV  $
C
C INPUT
C	PROJN 		PROJECTION TYPE TO DISPLAY MAP
C	      		= 1, SATELLITE VIEW
C	      		= 2, CYLINDRICAL EQUISISTANT
C	      		= 3, MERCATOR
C	      		= 4, MILLER CYLINDRICAL
C	      		= 5, POLAR STEREO NORTH
C			= 6, POLAR STEREO SOUTH
C	OBSLAT,OBSLON	CENTER LAT/LON
C	DLON,DLAT	LON/LAT POINT (DEG)
C	SX,SY    	X/Y COOORD OF POINT
C	HIDDEN		LOGICAL INDICATING IF A LAND OR WATER
C	      		MASS IS HIDDEN FROM VIEW
C	BRNCUT		LOGICAL INDICATING IF A LAND OR WATER
C	      		MASS IS CUT OFF AT THE BOUNDARY
C
C INTERNAL
C	LON,LAT		LON/LAT POINT IN RADIANS (RAD)
C	X,Y    		TEMPORARY VARIABLE FOR X/Y COORD 
C	INX,INY		TEMPORARY VARIABLE FOR X/Y COORD 
C	INY   		TEMPORARY VARIABLE FOR Y-COORD
C	LINX  		TEMPORARY VARIABLE FOR X-COORD 
C	OBSLT,OBSLN	CENTER LAT/LON IN RADIANS (RAD)
C	PI    		PI = 3.1415926...
C	XSCL  		X-COORD SCALNG FACTOR
C	SCL   		HEIGHT SCALING FACTOR
C
C SAVE VARIABLES
C	LINX
C
C WRITTEN BY RICHARD P. LEE  5-25-88
C MODIFIED BY CRAIG K. FUJIMOTO
C
C MODIFICATIONS
C $Date$ $Revision$ $Author$
C-----------------------------------------------------------------------

       SUBROUTINE LTRANS (PROJN,OBSLAT,OBSLON,DLON,DLAT,
     1                    SX,SY,HIDDEN,BRNCUT)

      character*100 SccsFileID
     -/'@(#)ltrans.for	5.1 98/01/08 APS/ASF\0'/

       IMPLICIT NONE
C INPUT:
       INTEGER PROJN
       REAL OBSLAT,OBSLON,DLON,DLAT
C OUTPUT:
       REAL SX,SY
       LOGICAL HIDDEN,BRNCUT
C INTERNAL:
       REAL LON,LAT
       REAL X,Y
       REAL INX,INY
       REAL LINX
       REAL OBSLT,OBSLN
       REAL PI
       REAL XSCL,SCL

       PARAMETER (PI = 3.14159265359)
       PARAMETER (XSCL = 1.383 / PI)
C-----------------------------------------------------------------------

C SAVE LINX VARIABLE
       SAVE LINX

C RE-CENTERING LATITUDE AND LONGITUDE IN RADIANS
       OBSLT = OBSLAT * PI / 180.0 
       OBSLN = OBSLON * PI / 180.0

C LATITUDE AND LONGITUDE POINTS IN RADIANS
       LON = DLON * PI / 180.0
       LAT = DLAT * PI / 180.0

C X-Y TRANSFORMATION CALCULATIONS
       X = COS(OBSLT) * COS(LAT) * COS(LON - OBSLN) + SIN(OBSLT)
     1     * SIN(LAT)
       Y = COS(LAT) * SIN(LON - OBSLN)
 
       INX = ATAN2(Y,X)
       INY = ASIN(COS(OBSLT) * SIN(LAT) - SIN(OBSLT) * COS(LAT)
     1       * COS(LON - OBSLN))

C IF SATELLITE VIEW

       IF (PROJN .EQ. 1) THEN

         SX = COS(INY) * SIN (INX)
         SY = SIN(INY)

C SCALE THE PROJECTION
         SX = SX * 0.95
         SY = SY * 0.95

         HIDDEN = (COS(INY) * COS(INX) .LT. 0.0)
         BRNCUT = .FALSE.

C IF CYLINDRICAL EQUIDISTANT

       ELSE IF (PROJN .EQ. 2) THEN

         SX = INX * XSCL
         SY = INY * XSCL

         HIDDEN = .FALSE.
         BRNCUT = (ABS(LINX - INX) .GT. (2.0 * PI))

C IF MERCATOR

       ELSE IF (PROJN .EQ. 3) THEN

         SCL = 0.775
         SX = INX * XSCL
         SY = SCL * LOG(1.0 / COS(INY) + TAN(INY)) * XSCL

         IF (DLAT .GT. 85.0 .OR. DLAT .LT. -85.0) THEN
           HIDDEN = .TRUE.
         ELSE
           HIDDEN = .FALSE.
         END IF

         BRNCUT = (ABS(LINX - INX) .GT. (2.0 * PI)) 

C IF MILLER CYLINDRICAL

       ELSE IF (PROJN .EQ. 4) THEN

         SCL = 1.0
         SX = INX * XSCL
         SY = SCL * LOG(1.0 / COS(INY * 0.8) + TAN(INY * 0.8)) * 
     1        XSCL / 0.8

         HIDDEN = .FALSE.
         BRNCUT = (ABS(LINX - INX) .GT. (2.0 * PI)) 

C IF POLAR STEREOGRAPHIC PROJECTION
       ELSE IF (PROJN .EQ. 5 .OR. PROJN .EQ. 6) THEN

         CALL VAXSTEREO(PROJN,OBSLAT,OBSLON,
     1                  DLAT,DLON,SX,SY,HIDDEN)

       END IF
       
C SCALE THE POINT TO THE WINDOW SIZE

       IF (PROJN .EQ. 1) THEN
         SX = (SX + 1.0) / 2.0
         SY = (SY + 1.0) / 2.0
       ELSE IF (PROJN.EQ.2 .OR. PROJN.EQ.3 .OR. PROJN.EQ.4) THEN
         SX = (SX + 1.55) / 3.1
         SY = (SY + 1.55) / 3.1
       END IF

C RESET LINX VARIABLE

       LINX = INX

C END OF SUBROUTINE

 9999  CONTINUE
       RETURN
       END


