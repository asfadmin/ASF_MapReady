C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	polchk.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================

C---------------------------------------------------------------
C SUBROUTINE POLCHK
C
C PURPOSE
C	CALCULATE THE REQUIRED BOX POINTS TO ENCLOSE THE USER 
C	SPECIFIED ZOOM WINDOW
C
C $Logfile:   ACS003:[BLD.MPS.MAPR.SRC]POLCHK.FOV  $
C
C INPUT
C	PROJN		PROJECTION NUMBER
C	OBSLAT,OBSLON	CENTER LAT/LON
C	LAT1,LAT2	INNER/OUTER LATITUDES
C	LON1,LON2	FIRST AND SECOND LONG - CLOCKWISE DIRECTION
C
C OUTPUT
C	XBOX,YBOX	X/Y COORDS THAT ENCLOSE ZOOM WINDOW
C     	IOS		STATUS
C
C INTERNAL 
C	CENTX,CENTY	CENTER X/Y
C	X(),Y()		X,Y COORDINATES OF FOUR CORNERS
C	TEMP		<
C	QD()		QUADRANT ARRAY 1=QUADRANT 1, ETC..
C	CR()		LINE CROSS ARRAY 
C			CR(1)=TRUE MEANS LINE CROSSES Q1/Q2 BOUNDARY
C	HID,BCUT	FLAGS FROM LTRANS
C                     
C WRITTEN BY CRAIG K. FUJIMOTO
C
C MODIFICATIONS
C $Date$ $Revision$ $Author$
C---------------------------------------------------------------
      SUBROUTINE POLCHK (PROJN,OBSLAT,OBSLON,
     1                   LAT1,LAT2,LON1,LON2,
     2                   IOS)

      character*100 SccsFileID
     -/'@(#)polchk.for	5.1 98/01/08 APS/ASF\0'/

      IMPLICIT NONE
 
C INPUT
      INTEGER PROJN
      REAL OBSLAT,OBSLON
      REAL LAT1,LAT2,LON1,LON2
C OUTPUT
      INTEGER IOS
C      REAL XBOX(5),YBOX(5)
C INTERNAL
      REAL CENTX,CENTY
      REAL X(4),Y(4)
      REAL TEMP
      INTEGER QD(4)
      LOGICAL HID,BCUT
      LOGICAL CR(4)

C---------------------------------------------------------------

C FIND THE CENTER POINT

      CALL LTRANS(PROJN,OBSLAT,OBSLON,OBSLON,OBSLAT,
     1            CENTX,CENTY,HID,BCUT)

C MAKE SURE POINTS ARE IN CORRECT ORDER
      IF ((PROJN .EQ. 5 .AND. LAT2 .GT. LAT1) .OR.
     1    (PROJN .EQ. 6 .AND. LAT2 .LT. LAT1)) THEN
        TEMP = LAT1
        LAT1 = LAT2
        LAT2 = TEMP
      END IF

C TRANSLATE LAT/LON TO X/Y                  

      CALL LTRANS(PROJN,OBSLAT,OBSLON,LON1,LAT1,
     1            X(1),Y(1),HID,BCUT)
      CALL LTRANS(PROJN,OBSLAT,OBSLON,LON1,LAT2,
     1            X(2),Y(2),HID,BCUT)
      CALL LTRANS(PROJN,OBSLAT,OBSLON,LON2,LAT2,
     1            X(3),Y(3),HID,BCUT)
      CALL LTRANS(PROJN,OBSLAT,OBSLON,LON2,LAT1,
     1            X(4),Y(4),HID,BCUT)

C DETERMINE WHICH QUADRANTS THE FOUR POINTS ARE IN
      CALL QUADRANT(CENTX,CENTY,X,Y,4,QD)

C DETERMINE IF THE POINTS CROSS A QUADRANT LINE
      CALL LINECROSS(CENTX,CENTY,X,Y,QD,CR)

C DISPLAY ZOOM OUTLINE
      CALL POLBOX(CENTX,CENTY,X,Y)

      IOS = 0

 9999 CONTINUE
      RETURN
      END


C-----------------------------------------------------------------
C SUBROUTINE POLXY
C
C PURPOSE
C	CALCULATE THE REQUIRED BOX POINTS TO ENCLOSE THE USER 
C	SPECIFIED ZOOM WINDOW
C
C INPUT
C	PROJN		PROJECTION NUM
C	OBSLAT,OBSLON	CENTER LAT/LON
C	LAT1,LAT2	INNER/OUTER LAT
C	LON1,LON2	BEGIN/END LON - CLOCKWISE DIRECTION
C
C OUTPUT
C	XBOX,YBOX	X/Y COORDINATES OF BOX TO BE ZOOMED
C	IOS		I/O STATUS
C
C INTERNAL
C	CENTX,CENTY	CENTER X/Y
C	X(),Y()		FOUR CORNER POINTS
C	TEMP		<
C	QD()		QUADRANT ARRAY
C			IF QD(1) = 2, POINT 1 IS IN QUADRANT 2
C	CR()		LINE CROSS ARRAY
C			IF CR(1) = TRUE, ZOOM AREA CROSSES QD1/QD2 LINE
C	HID,BCUT	LTRANS VARIABLES
C                     
C WRITTEN BY CRAIG K. FUJIMOTO
C-----------------------------------------------------------------
      SUBROUTINE POLXY (PROJN,OBSLAT,OBSLON,
     1                  MINLAT,MAXLAT,MINLON,MAXLON,
     1                  MINX,MAXX,MINY,MAXY,
     1                  MINXWN,MAXXWN,MINYWN,MAXYWN)
        
      IMPLICIT NONE
C INPUT
      INTEGER PROJN
      REAL OBSLAT,OBSLON
      REAL MINLAT,MAXLAT,MINLON,MAXLON
C OUTPUT
      REAL MINX,MAXX,MINY,MAXY
      REAL MINXWN,MAXXWN,MINYWN,MAXYWN
C INTERNAL
      REAL CENTX,CENTY
      REAL X(4),Y(4)
      REAL XBOX(2),YBOX(2)
      REAL DIFLAT
      REAL DIFX,DIFY
      INTEGER QD(4)
      LOGICAL CR(4)
      LOGICAL HID,BCUT
C-----------------------------------------------------------------

C SET SIZE OF BORDER
      DIFLAT = ABS(MINLAT - MAXLAT)
      DIFX = 0.00055 * DIFLAT
      DIFY = 0.00055 * DIFLAT

C IF FULL WINDOW
       IF ((PROJN.EQ.5 .AND.
     1      MINLAT.EQ.90.0 .AND. MAXLAT.EQ.0.0 .AND.
     1      MINLON.EQ.MAXLON) .OR.
     1     (PROJN.EQ.6 .AND.
     1      MINLAT.EQ.-90.0 .AND. MAXLAT.EQ.0.0 .AND.
     1      MINLON.EQ.MAXLON)) THEN

C SET ZOOM WINDOW AND DISPLAY WINDOW TO BE THE SAME - FULL WINDOW
         MINX = 0.0
         MINY = 0.0
         MAXX = 1.0
         MAXY = 1.0

         MINXWN = 0.0
         MINYWN = 0.0
         MAXXWN = 1.0
         MAXYWN = 1.0

         GO TO 9999

       END IF

C FOR NOT FULL WINDOW

C FIND THE CENTER POINT

      CALL LTRANS(PROJN,OBSLAT,OBSLON,OBSLON,OBSLAT,
     1            CENTX,CENTY,HID,BCUT)

C TRANSLATE LAT/LON TO X/Y                  

      CALL LTRANS(PROJN,OBSLAT,OBSLON,MINLON,MINLAT,
     1            X(1),Y(1),HID,BCUT)
      CALL LTRANS(PROJN,OBSLAT,OBSLON,MINLON,MAXLAT,
     1            X(2),Y(2),HID,BCUT)
      CALL LTRANS(PROJN,OBSLAT,OBSLON,MAXLON,MAXLAT,
     1            X(3),Y(3),HID,BCUT)
      CALL LTRANS(PROJN,OBSLAT,OBSLON,MAXLON,MINLAT,
     1            X(4),Y(4),HID,BCUT)

C DETERMINE WHICH QUADRANTS THE FOUR POINTS ARE IN
      CALL QUADRANT(CENTX,CENTY,X,Y,4,QD)

C DETERMINE IF THE POINTS CROSS A QUADRANT LINE
      CALL LINECROSS(CENTX,CENTY,X,Y,QD,CR)

C BASED ON THE QUAD AND CROSS CALLS, DETERMINE THE MIN/MAX X/Y'S
      CALL MINMAX(CENTX,CENTY,X,Y,QD,CR,XBOX,YBOX)

C SET ZOOM WINDOW X/Y
      MINX = XBOX(1)
      MAXX = XBOX(2)
      MINY = YBOX(1)
      MAXY = YBOX(2)

C SET DISPLAY WINDOW X/Y
      MINXWN = MINX - DIFX
      MAXXWN = MAXX + DIFX
      MINYWN = MINY - DIFY
      MAXYWN = MAXY + DIFY

 9999 CONTINUE
      RETURN
      END



C---------------------------------------------------------------
C
C SUBROUTINE QUADRANT
C
C PURPOSE
C	DETERMINE WHICH QUADRANT THE POINTS ARE IN
C
C INPUT
C	CENTX,CENTY	CENTER X/Y POINT
C	X,Y		FOUR CORNER POINTS X/Y
C OUTPUT
C	QD		QUADRANT ARRAY
C			QD(1) = 1 MEANS POINT 1 IS IN QUAD 1
C			QD(1) = 2 MEANS POINT 1 IS IN QUAD 2
C INTERNAL
C	I		COUNTER
C
C WRITTEN BY CRAIG K. FUJIMOTO
C---------------------------------------------------------------
      SUBROUTINE QUADRANT(CENTX,CENTY,X,Y,N,QD)

      IMPLICIT NONE

      REAL CENTX,CENTY
      REAL X(*),Y(*)
      INTEGER QD(*)
      INTEGER N
      INTEGER I
C---------------------------------------------------------------

      DO 1000 I = 1,N

        IF (X(I) .EQ. CENTX .AND. Y(I) .EQ. CENTY) THEN
          QD(I) = 0
        ELSE IF (X(I) .GE. CENTX .AND. Y(I) .GT. CENTY) THEN
          QD(I) = 1
        ELSE IF (X(I) .GT. CENTX .AND. Y(I) .LE. CENTY) THEN
          QD(I) = 4
        ELSE IF (X(I) .LE. CENTX .AND. Y(I) .LT. CENTY) THEN
          QD(I) = 3
        ELSE IF (X(I) .LT. CENTX .AND. Y(I) .GE. CENTY) THEN 
          QD(I) = 2
        END IF

 1000 CONTINUE

 9999 CONTINUE                 
      RETURN
      END

C---------------------------------------------------------------
C SUBROUTINE LINECROSS
C
C PURPOSE
C	DETERMINE WHICH QUADRANT LINES THE OUTER POINTS CROSS
C	POINTS ARE GIVEN IN A CLOCKWISE DIRECTION
C
C INPUT
C	CENTX,CENTY	CENTER X/Y
C	X,Y		FOUR CORNER POINTS
C	QD		QUADRANT ARRAY
C OUTPUT
C	CR		CROSS LINE ARRAY
C			CR(1) = TRUE MEANS LINE CROSSES QUAD1/QUAD2 LINE
C INTERNAL
C	I		COUNTER
C
C WRITTEN BY CRAIG K. FUJIMOTO
C---------------------------------------------------------------
      SUBROUTINE LINECROSS(CENTX,CENTY,X,Y,QD,CR)

      IMPLICIT NONE

      REAL CENTX,CENTY
      REAL X(*),Y(*)   
      INTEGER QD(*)
      LOGICAL CR(*)
C---------------------------------------------------------------

C INITIALIZE THE CROSSING ARRAY
      CR(1) = .FALSE.
      CR(2) = .FALSE.
      CR(3) = .FALSE.
      CR(4) = .FALSE.

C FOR THE OUTSIDE FIRST POINT - POINT 2

      IF (QD(2) .EQ. 1) THEN

C IF THE NEXT OUTSIDE POINT

        IF (QD(3) .EQ. 4) THEN
          CR(4) = .TRUE.
        ELSE IF (QD(3) .EQ. 3) THEN
          CR(4) = .TRUE.
          CR(3) = .TRUE.
        ELSE IF (QD(3) .EQ. 2) THEN
          CR(4) = .TRUE.
          CR(3) = .TRUE.
          CR(2) = .TRUE.
        ELSE IF (QD(3) .EQ. 1 .AND. X(3) .LE. X(2)) THEN
          CR(4) = .TRUE.
          CR(3) = .TRUE.
          CR(2) = .TRUE.
          CR(1) = .TRUE.
        END IF

      ELSE IF (QD(2) .EQ. 4) THEN

        IF (QD(3) .EQ. 3) THEN
          CR(3) = .TRUE.
        ELSE IF (QD(3) .EQ. 2) THEN
          CR(3) = .TRUE.
          CR(2) = .TRUE.
        ELSE IF (QD(3) .EQ. 1) THEN
          CR(3) = .TRUE.
          CR(2) = .TRUE.
          CR(1) = .TRUE.
        ELSE IF (QD(3) .EQ. 4 .AND. X(3) .GE. X(2)) THEN
          CR(3) = .TRUE.
          CR(2) = .TRUE.
          CR(1) = .TRUE.
          CR(4) = .TRUE.
        END IF

      ELSE IF (QD(2) .EQ. 3) THEN

        IF (QD(3) .EQ. 2) THEN
          CR(2) = .TRUE.
        ELSE IF (QD(3) .EQ. 1) THEN
          CR(2) = .TRUE.
          CR(1) = .TRUE.
        ELSE IF (QD(3) .EQ. 4) THEN
          CR(2) = .TRUE.
          CR(1) = .TRUE.
          CR(4) = .TRUE.
        ELSE IF (QD(3) .EQ. 3 .AND. X(3) .GE. X(2)) THEN
          CR(2) = .TRUE.
          CR(1) = .TRUE.
          CR(4) = .TRUE.
          CR(3) = .TRUE.
        END IF

      ELSE IF (QD(2) .EQ. 2) THEN

        IF (QD(3) .EQ. 1) THEN
          CR(1) = .TRUE.
        ELSE IF (QD(3) .EQ. 4) THEN
          CR(1) = .TRUE.
          CR(4) = .TRUE.
        ELSE IF (QD(3) .EQ. 3) THEN
          CR(1) = .TRUE.
          CR(4) = .TRUE.
          CR(3) = .TRUE.
        ELSE IF (QD(3) .EQ. 2 .AND. X(3) .LE. X(2)) THEN
          CR(1) = .TRUE.
          CR(4) = .TRUE.
          CR(3) = .TRUE.
          CR(2) = .TRUE.
        END IF

      END IF

 1000 CONTINUE


 9999 CONTINUE
      RETURN
      END

C---------------------------------------------------------------
C SUBROUTINE POLBOX
C
C PURPOSE
C	OUTLINE THE CIRCULAR SECTION THAT THE USER HAS CHOSEN TO 
C	ZOOM INTO.
C
C INPUT
C	CENTX,CENTY	CENTER X/Y
C	X,Y		FOUR CORNER POINTS
C OUTPUT
C	NONE
C INTERNAL
C	DATALEN		LENGTH OF DATA STRUCTURE
C	LIST_INTS	LIST OF INTEGERS OF DATA STRUCTURE
C	MAXSIZE		MAXIMUM SIZE OF DATA STRUCT
C	IOS		I/O FLAG
C	ZOOMX,ZOOMY	TEMP STORAGE FOR ARCS, LINES AND CIRLES
C	DATAREC		DAT RECORD
C	EMPTY_REAL	DUMI REAL
C	EMPTY_CHAR	EMPTY CHARACTER
C	
C WRITTEN BY CRAIG K. FUJIMOTO
C---------------------------------------------------------------
      SUBROUTINE POLBOX(CENTX,CENTY,X,Y)
      IMPLICIT NONE

C INPUT
c--port      INCLUDE 'SYS$LIBRARY:GKSDEFS.BND'
       INCLUDE 'GKS_ROOT:include/fgksenum.inc'
       INCLUDE 'mapper_port.inc'

      REAL CENTX,CENTY
      REAL X(*),Y(*)
C INTERNAL
      INTEGER DATALEN
      INTEGER LIST_INTS(1)
      INTEGER MAXSIZE
      INTEGER IOS
      REAL ZOOMX(4),ZOOMY(4)
      REAL EMPTY_REAL
      CHARACTER*80 DATAREC(5)
      CHARACTER*1 EMPTY_CHAR
C---------------------------------------------------------------

C IF ZOOM WINDOW IS CENTERED AT POLE - COMPLETE CIRCLE
      IF (X(1) .EQ. X(4) .AND. Y(1) .EQ. Y(4) .AND.
     1    X(2) .EQ. X(3) .AND. Y(2) .EQ. Y(3)) THEN

        ZOOMX(1) = CENTX
        ZOOMY(1) = CENTY
        ZOOMX(2) = X(2)
        ZOOMY(2) = Y(2)

C DRAW THE CIRCLE
        CALL GGDP(2,ZOOMX,ZOOMY,GGCCP,0,DATAREC)

      ELSE

C DEFINE/DRAW THE FIRST LINE
        ZOOMX(1) = X(1)
        ZOOMX(2) = X(2)

        ZOOMY(1) = Y(1)
        ZOOMY(2) = Y(2)

        CALL GPL(2,ZOOMX,ZOOMY)

C DEFINE/DRAW THE OUTER ARC - DRAWN COUNTERCLOCKWISE
        ZOOMX(1) = CENTX
        ZOOMX(2) = X(3)
        ZOOMX(3) = X(2)

        ZOOMY(1) = CENTY
        ZOOMY(2) = Y(3)
        ZOOMY(3) = Y(2)                              

C PACK DATA RECORD
        LIST_INTS(1) = GATOPN
        DATALEN = 16
        MAXSIZE = 5                                 

        CALL GPREC(1,LIST_INTS,0,EMPTY_REAL,
     1           0,0,EMPTY_CHAR,
     1           MAXSIZE,IOS,DATALEN,DATAREC)

C DRAW THE ARC
        CALL GGDP(3,ZOOMX,ZOOMY,GGAC2P,DATALEN,DATAREC)

C DEFINE/DRAW THE SECOND LINE
        ZOOMX(1) = X(3)           
        ZOOMX(2) = X(4)

        ZOOMY(1) = Y(3)
        ZOOMY(2) = Y(4)

        CALL GPL(2,ZOOMX,ZOOMY)

C MAKE SURE THE INNER POINTS ARE NOT THE SAME
        IF (X(4) .NE. X(1) .OR. Y(4) .NE. Y(1)) THEN

C DEFINE/DRAW THE INNER ARC - DRAWN COUNTERCLOCKWISE
          ZOOMX(1) = CENTX
          ZOOMX(2) = X(4)
          ZOOMX(3) = X(1)

          ZOOMY(1) = CENTY
          ZOOMY(2) = Y(4)
          ZOOMY(3) = Y(1)
              
C DRAW THE INNER ARC
          CALL GGDP(3,ZOOMX,ZOOMY,GGAC2P,DATALEN,DATAREC)

        END IF

      END IF

 9999 CONTINUE
      RETURN
      END



C---------------------------------------------------------------
C SUBROUTINE MINMAX
C
C PURPOSE
C	DETERMINE THE MINIMUM AND MAXIMUM X/Y VALUES FOR THE 
C	ZOOM WINDOW
C
C INPUT
C	CENTX,CENTY	CENTER X/Y
C	X,Y		FOUR CORNER POINTS
C	QD		QUADRANT ARRAY
C	CR		LINE CROSS ARRAY
C OUTPUT
C	XBOX,YBOX	MIN/MAX X/Y COORDINATES
C INTERNAL 
C	XMIN,XMAX
C	YMIN,YMAX	TEMP HOLDERS FOR MIN/MAX
C	RADIUS		RADIUS FROM CENTER TO OUTERMOST X/Y POINT
C
C WRITTEN BY CRAIG K. FUJIMOTO
C---------------------------------------------------------------

      SUBROUTINE MINMAX(CENTX,CENTY,X,Y,QD,CR,XBOX,YBOX)

      IMPLICIT NONE

C INPUT
      REAL CENTX,CENTY
      REAL X(*),Y(*)
      INTEGER QD(*)
      LOGICAL CR(*)

C OUTPUT
      REAL XBOX(*),YBOX(*)

C INTERNAL
      REAL XMIN,XMAX,YMIN,YMAX
      REAL RADIUS

C---------------------------------------------------------------

      XMIN = MIN(X(1),X(2),X(3),X(4))
      XMAX = MAX(X(1),X(2),X(3),X(4))

      YMIN = MIN(Y(1),Y(2),Y(3),Y(4))      
      YMAX = MAX(Y(1),Y(2),Y(3),Y(4))      

      RADIUS = SQRT(ABS(X(2)-CENTX)**2 + ABS(Y(2)-CENTY)**2)

      IF (CR(4)) THEN
        XMAX = CENTX + RADIUS
      END IF

      IF (CR(3)) THEN
        YMIN = CENTY - RADIUS
      END IF

      IF (CR(2)) THEN
        XMIN = CENTX - RADIUS
      END IF

      IF (CR(1)) THEN
        YMAX = CENTY + RADIUS
      END IF

      XBOX(1) = XMIN
      XBOX(2) = XMAX

      YBOX(1) = YMIN
      YBOX(2) = YMAX

 9999 CONTINUE
      RETURN
      END

                 
C---------------------------------------------------------------
C
C SUBROUTINE STLOND
C
C PURPOSE
C	DETERMINE THE LONGITUDE DISTANCE BETWEEN THE TWO SIDES
C
C INPUT
C	PROJN		PROJECTION NUMBER
C	MINLON,MAXLON	MIN AND MAX LON OF WINDOW
C OUTPUT
C	LONDIST		LONGITUDE DISTANCE
C INTERNAL
C	TMIN,TMAX	TEMP MIN/MAX HOLDERS
C
C WRITTEN BY CRAIG K. FUJIMOTO
C---------------------------------------------------------------
      SUBROUTINE STLOND(PROJN,MINLON,MAXLON,LONDIST)

      IMPLICIT NONE
 
C INPUT
      INTEGER PROJN
      REAL MINLON,MAXLON
C OUTPUT
      REAL LONDIST
C INTERNAL
      REAL TMIN,TMAX
C---------------------------------------------------------------

C CONVERT TO 0 TO 360
      TMIN = MINLON + 180.0
      TMAX = MAXLON + 180.0

C IF THE ENTIRE WORLD
      IF (TMIN .EQ. TMAX) THEN

        LONDIST = 360.0

C FOR NORTH PROJECTION
      ELSE IF (PROJN .EQ. 5) THEN

        IF (TMIN .LT. TMAX) THEN
          LONDIST = 360.0 - (TMAX - TMIN)
        ELSE
          LONDIST = TMIN - TMAX
        END IF 

C FOR SOUTHERN PROJECTION
      ELSE IF (PROJN .EQ. 6) THEN

        IF (TMIN .LT. TMAX) THEN
          LONDIST = TMAX - TMIN
        ELSE
          LONDIST = 360.0 - (TMIN - TMAX)
        END IF 

      END IF

 9999 CONTINUE
      RETURN
      END
