C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	mpsline.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================

C-----------------------------------------------------------------
C SUBROUTINE DIS_SWATH
C
C PURPOSE
C	DISPLAY THE GROUND TRACK OR SENSOR SWATH
C
C $Logfile:   ACS003:[BLD.MPS.MAPR.SRC]MPSLINE.FOV  $
C
C INPUT
C	PROJN		PROJECTION NO.
C	MINX,MAXX,
C	MINY,MAXY	MIN/MAX X/Y COORDS
C	S1,S2		X/Y COORDS OF SWATH POINTS
C	HID1,HID2	HIDDEN FLAGS FOR SWATH POINTS
C	LAST		FLAG INDICATING WHETHER LAST POINT OR NOT
C
C SUBROUTINES CALLED
C	DIS_LINE
C
C WRITTEN BY CRAIG K. FUJIMOTO - SEP 89
C
C MODIFICATIONS
C $Date$ $Revision$ $Author$
C-----------------------------------------------------------------
       SUBROUTINE DIS_SWATH (PROJN,
     1                   SW1,SW2,
     2                   HID1,HID2,
     3                   MINX,MAXX,MINY,MAXY,
     4                   LAST)

      character*100 SccsFileID
     -/'@(#)mpsline.for	5.1 98/01/08 APS/ASF\0'/

       IMPLICIT NONE
C INPUT:
       INTEGER PROJN,LAST
       REAL SW1(2,2),SW2(2,2)
       REAL MINX,MAXX,MINY,MAXY
       LOGICAL HID1(2),HID2(2)
C-----------------------------------------------------------------------

C DRAW THE NEAR LINE

       CALL DIS_LINE(PROJN,
     1                   SW1(1,1),SW1(1,2),SW2(1,1),SW2(1,2),
     2                   HID1(1),HID2(1),
     3                   MINX,MAXX,MINY,MAXY)
 
C DRAW THE FAR LINE

       CALL DIS_LINE(PROJN,
     1                   SW1(2,1),SW1(2,2),SW2(2,1),SW2(2,2),
     2                   HID1(2),HID2(2),
     3                   MINX,MAXX,MINY,MAXY)

C DRAW CROSS LINE

       CALL DIS_LINE(PROJN,
     1                   SW1(1,1),SW1(1,2),SW1(2,1),SW1(2,2),
     2                   HID1(1),HID2(1),
     3                   MINX,MAXX,MINY,MAXY)

C IF NOT THE LAST GROUND TRACK

       IF (LAST .EQ. 1) THEN

C DRAW CROSS LINE

         CALL DIS_LINE(PROJN,
     1                   SW2(1,1),SW2(1,2),SW2(2,1),SW2(2,2),
     2                   HID1(2),HID2(2),
     3                   MINX,MAXX,MINY,MAXY)

       END IF

C END OF SUBROUTINE

 9999  CONTINUE
       RETURN 
       END             



C-----------------------------------------------------------------------
C SUBROUTINE DIS_LINE
C        
C PURPOSE
C	DRAW A LINE BETWEEN THE INPUT POINTS
C
C INPUT
C	PROJN		PROJECTION NUMBER
C	SX1,SY1		FIRST POINT
C	SX2,SY2		SECOND POINT
C	HIDDN1		FLAG TO INDICATE IF FIRST POINT IS HIDDEN
C	HIDDN2		"    "  "        "  SECOND "    "  "
C	MINX,MAXX	MIN/MAX X VALUES
C	MINY,MAXY	"       Y "
C
C INTERNAL
C	X,Y
C	GTX(),GTY()
C	ANGLE
C	XT,YT
C	XP,YP
C
C WRITTEN BY CRAIG K. FUJIMOTO
C-----------------------------------------------------------------------
       SUBROUTINE DIS_LINE(PROJN,
     1                   SX1,SY1,SX2,SY2,
     2                   HIDDN1,HIDDN2,
     3                   MINX,MAXX,MINY,MAXY)

       IMPLICIT NONE
C INPUT:
c--port       INCLUDE 'SYS$LIBRARY:GKSDEFS.BND'
       INCLUDE 'GKS_ROOT:include/fgksenum.inc'

       INTEGER PROJN
       REAL SX1,SY1,SX2,SY2
       REAL MINX,MAXX,MINY,MAXY
       LOGICAL HIDDN1,HIDDN2

C INTERNAL:
       REAL Y
       REAL GTX(2),GTY(2)
       REAL ANGLE,RADIUS
       REAL XT,XP,YT,YP
       REAL CENX,CENY,RADX,RADY,EDGEX,EDGEY,SLOPE,B
       LOGICAL HIDDEN,BRNCUT

C IF SATELLITE OR POLAR

         IF (PROJN .EQ. 1 .OR. PROJN .EQ. 5 .OR. PROJN .EQ. 6) THEN

           IF (PROJN .EQ. 1) THEN

C THE CENTER
             CALL LTRANS (PROJN,90.0,0.0,
     1                    0.0,90.0,
     1                    CENX,CENY,
     1                    HIDDEN,BRNCUT)
 
C A POINT ON THE EQUATOR
             CALL LTRANS (PROJN,90.0,0.0,
     1                    0.0,0.0,
     1                    RADX,RADY,
     1                    HIDDEN,BRNCUT)

             RADIUS = SQRT(ABS(CENX - RADX)**2 +
     1                     ABS(CENY - RADY)**2)

C             RADIUS = 0.322

           ELSE IF (PROJN .EQ. 5) THEN

C THE CENTER
             CALL LTRANS (PROJN,90.0,0.0,
     1                    0.0,90.0,
     1                    CENX,CENY,
     1                    HIDDEN,BRNCUT)
 
C A POINT ON THE EQUATOR
             CALL LTRANS (PROJN,90.0,0.0,
     1                    0.0,0.0,
     1                    RADX,RADY,
     1                    HIDDEN,BRNCUT)

             RADIUS = SQRT(ABS(CENX - RADX)**2 +
     1                     ABS(CENY - RADY)**2)

           ELSE

C THE CENTER
             CALL LTRANS (PROJN,-90.0,0.0,
     1                    0.0,-90.0,
     1                    CENX,CENY,
     1                    HIDDEN,BRNCUT)
 
C A POINT ON THE EQUATOR
             CALL LTRANS (PROJN,-90.0,0.0,
     1                    0.0,0.0,
     1                    RADX,RADY,
     1                    HIDDEN,BRNCUT)

             RADIUS = SQRT(ABS(CENX - RADX)**2 +
     1                     ABS(CENY - RADY)**2)

           END IF



C  IF 2ND POINT IS HIDDEN

           IF (.NOT. HIDDN1 .AND. HIDDN2) THEN

             GTX(1) = SX1
             GTY(1) = SY1
             ANGLE = ATAN2(SY1 - 0.5,SX1 - 0.5)
             GTX(2) = 0.5 + (RADIUS * COS(ANGLE))
             GTY(2) = 0.5 + (RADIUS * SIN(ANGLE))

             CALL GPL (2,GTX,GTY)
                        
C  IF 1ST POINT IS HIDDEN

           ELSE IF (HIDDN1 .AND. .NOT. HIDDN2) THEN

             ANGLE = ATAN2(SY2 - 0.5,SX2 - 0.5)
             GTX(1) = 0.5 + (RADIUS * COS(ANGLE))
             GTY(1) = 0.5 + (RADIUS * SIN(ANGLE))
             GTX(2) = SX2
             GTY(2) = SY2

             CALL GPL (2,GTX,GTY)

C  IF BOTH POINTS ARE VISIBLE

           ELSE IF (.NOT. HIDDN1 .AND. .NOT. HIDDN2) THEN

             GTX(1) = SX1
             GTY(1) = SY1
             GTX(2) = SX2
             GTY(2) = SY2

             CALL GPL (2,GTX,GTY)

           END IF

C IF CYLINDRICAL EQUIDISTANT OR MILLER CYLINDRICAL
         ELSE IF (PROJN.EQ.2 .OR. PROJN.EQ.4) THEN

C  IF SWATH LINE IS NOT BROKEN

           IF (ABS(SX2 - SX1) .LT. 0.4) THEN

             GTX(1) = SX1
             GTY(1) = SY1
             GTX(2) = SX2
             GTY(2) = SY2

             CALL GPL (2,GTX,GTY)

C  IF SWATH LINE IS BROKEN

           ELSE
  
C  IF A POLAR ORBIT, DRAW SWATH LINE AT BOTH ENDS

             IF (SX1 .EQ. SX2) THEN

               IF (SY1 .LT. 0.5) Y = MINY
               IF (SY1 .GT. 0.5) Y = MAXY
               GTX(1) = SX1
               GTY(1) = SY1
               GTX(2) = SX1
               GTY(2) = Y

               CALL GPL (2,GTX,GTY)

               GTX(1) = SX2
               GTY(1) = Y
               GTX(2) = SX2
               GTY(2) = SY2

               CALL GPL (2,GTX,GTY)

C  IF NOT A POLAR ORBIT, DRAW SWATH LINE AT BOTH ENDS

             ELSE IF (SX1 .LT. SX2) THEN

               XT = (SX1 - MINX) + (MAXX - SX2)
               YT = SY2 - SY1
               XP = MAXX - SX2
               YP = YT * (1.0 - (XP / XT))
               GTX(1) = SX1
               GTY(1) = SY1
               GTX(2) = MINX
               GTY(2) = SY1 + YP

               CALL GPL (2,GTX,GTY)

               GTX(1) = MAXX        
               GTY(1) = GTY(2)
               GTX(2) = SX2
               GTY(2) = SY2

               CALL GPL (2,GTX,GTY)

             ELSE IF (SX1 .GT. SX2) THEN

               XT = (MAXX - SX1) + (SX2 - MINX)
               YT = SY2 - SY1
               XP = SX2 - MINX
               YP = YT * (1.0 - (XP / XT))
               GTX(1) = SX1
               GTY(1) = SY1
               GTX(2) = MAXX
               GTY(2) = SY1 + YP

               CALL GPL (2,GTX,GTY)

               GTX(1) = MINX
               GTY(1) = GTY(2)
               GTX(2) = SX2
               GTY(2) = SY2

               CALL GPL (2,GTX,GTY)

             END IF      

           END IF

C MERCATOR PROJECTION
         ELSE IF (PROJN .EQ. 3) THEN

           IF (.NOT. HIDDN1 .AND. HIDDN2) THEN

             SLOPE = (SY2 - SY1) / (SX2 - SX1)
             B = SY1 - SLOPE * SX1

             IF (SY1 .GT. 0.5) THEN
               EDGEY = MAXY
             ELSE
               EDGEY = MINY
             END IF

             EDGEX = (EDGEY - B) / SLOPE

             GTX(1) = SX1
             GTY(1) = SY1
             GTX(2) = EDGEX
             GTY(2) = EDGEY

             CALL GPL(2,GTX,GTY)

           ELSE IF (HIDDN1 .AND. .NOT. HIDDN2) THEN

             SLOPE = (SY2 - SY1) / (SX2 - SX1)
             B = SY1 - SLOPE * SX1

             IF (SY2 .GT. 0.5) THEN
               EDGEY = MAXY
             ELSE
               EDGEY = MINY
             END IF

             EDGEX = (EDGEY - B) / SLOPE

             GTX(1) = EDGEX
             GTY(1) = EDGEY
             GTX(2) = SX2
             GTY(2) = SY2

             CALL GPL(2,GTX,GTY)

           ELSE IF (.NOT. HIDDN1 .AND. .NOT. HIDDN2) THEN

C  IF SWATH LINE IS NOT BROKEN
           IF (ABS(SX2 - SX1) .LT. 0.4) THEN

             GTX(1) = SX1
             GTY(1) = SY1
             GTX(2) = SX2
             GTY(2) = SY2

             CALL GPL (2,GTX,GTY)

C  IF SWATH LINE IS BROKEN

           ELSE
  
C  IF A POLAR ORBIT, DRAW SWATH LINE AT BOTH ENDS

             IF (SX1 .EQ. SX2) THEN

               IF (SY1 .LT. 0.5) Y = MINY
               IF (SY1 .GT. 0.5) Y = MAXY
               GTX(1) = SX1
               GTY(1) = SY1
               GTX(2) = SX1
               GTY(2) = Y

               CALL GPL (2,GTX,GTY)

               GTX(1) = SX2
               GTY(1) = Y
               GTX(2) = SX2
               GTY(2) = SY2

               CALL GPL (2,GTX,GTY)

C  IF NOT A POLAR ORBIT, DRAW SWATH LINE AT BOTH ENDS

             ELSE IF (SX1 .LT. SX2) THEN

               XT = (SX1 - MINX) + (MAXX - SX2)
               YT = SY2 - SY1
               XP = MAXX - SX2
               YP = YT * (1.0 - (XP / XT))
               GTX(1) = SX1
               GTY(1) = SY1
               GTX(2) = MINX
               GTY(2) = SY1 + YP

               CALL GPL (2,GTX,GTY)

               GTX(1) = MAXX        
               GTY(1) = GTY(2)
               GTX(2) = SX2
               GTY(2) = SY2

               CALL GPL (2,GTX,GTY)

             ELSE IF (SX1 .GT. SX2) THEN

               XT = (MAXX - SX1) + (SX2 - MINX)
               YT = SY2 - SY1
               XP = SX2 - MINX
               YP = YT * (1.0 - (XP / XT))
               GTX(1) = SX1
               GTY(1) = SY1
               GTX(2) = MAXX
               GTY(2) = SY1 + YP

               CALL GPL (2,GTX,GTY)

               GTX(1) = MINX
               GTY(1) = GTY(2)
               GTX(2) = SX2
               GTY(2) = SY2

               CALL GPL (2,GTX,GTY)

             END IF      

           END IF

           END IF

         END IF

 9999  CONTINUE
       RETURN 
       END
