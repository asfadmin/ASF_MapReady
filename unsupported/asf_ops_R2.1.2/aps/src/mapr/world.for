C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	world.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================

C-----------------------------------------------------------------------
C SUBROUTINE WORLD
C
C PURPOSE
C	THIS SUBROUTINE DISPLAYS THE WORLD MAP IN THE CURRENT WINDOW
C	IN THE CHOSEN PROJECTION.
C
C $Logfile:   ACS003:[BLD.MPS.MAPR.SRC]WORLD.FOV  $
C
C INPUT
C	WSID		WORKSTATION ID NUMBER
C	MAPFIL		WORLD MAP FILE NAME
C	PROJN 		PROJECTION TYPE TO DISPLAY MAP
C	PLOT		HP PLOTTER FLAG - 1=YES 0=NO
C	OBSLAT,OBSLON	CENTERING LONGITUDE OF THE MAP (DEG)
C	XEDGE		ARRAY OF X-COORD POINTS ON UNIT CIRCLE
C	YEDGE		ARRAY OF Y-CCORD POINTS ON UNIT CIRCLE
C	MINX		MINIMUM X-COORD OF THE WINDOW
C	MAXX		MAXIMUM X-COORD OF THE WINDOW
C	MINYWN		MINIMUM Y-COORD OF THE WINDOW
C	MAXYWN		MINIMUM Y-COORD OF THE WINDOW
C
C INTERNAL
C	IOS		READ STATEMENT I/O STATUS NUMBER
C	DLAT		TEMPORARY ARRAY OF LATITUDE POINTS (DEG)
C	DLON		TEMPORARY ARRAY OF LONGITUDE POINTS (DEG)
C	SX		X-COORD OF A LONGITUDE POINT
C	SY		Y-COORD OF A LATITUDE POINT
C	HIDDEN		FLAG IF A LAND OR WATER MASS IS HIDDEN FROM VIEW
C	BRNCUT		FLAG IF A LAND OR WATER MASS IS CUT OFF AT THE BOUNDARY
C	X		ARRAY OF X-COORD OF A LINE SEGMENT
C	Y		ARRAY OF Y-COORD OF A LINE SEGMENT
C	X1		ARRAY OF X-COORD OF A BROKEN LINE SEGMENT
C	Y1		ARRAY OF Y-COORD OF A BROKEN LINE SEGMENT
C	SAVLT		TEMPORARY SAVE VARIABLE
C	SAVLN		TEMPORARY SAVE VARIABLE
C	SAVLT1		TEMPORARY SAVE VARIABLE
C	SAVLT1		TEMPORARY SAVE VARIABLE
C	BRLAT		ARRAY OF BREAK POINT LATITUDES (DEG)
C	BRLON		ARRAY OF BREAK POINT LONGITUDES (DEG)
C	BRX		ARRAY OF X-COORD BREAK POINTS
C	BRY		ARRAY OF Y-COORD BREAK POINTS
C	RADBR		ARRAY OF BREAK POINTS ON UNIT CIRCLE IN RADIANS (RAD)
C	DEGBR		ARRAY OF BREAK POINTS ON UNIT CIRCLE IN DEGREES (DEG)
C	TEMPSX		X-COORD OF A LONGITUDE POINT
C	TEMPSY		Y-COORD OF A LATITUDE POINT
C	BRPNT		ARRAY OF LINE SEGMENT BREAK POINT NUMBERS
C	BREAK		FLAG INDICATING IF LINE SEGMENT IS A BROKEN SEGMENT
C			= 0 (NO) OR 1 (YES)
C	SET		FLAG WHICH SIDE OF THE BROKEN
C			LINE SEGMENT THE CURRENT POINT IS PART OF
C			= 0, BEGINNING SIDE
C			= 1, BROKEN SIDE
C	SWITCH		TEMPORARY VARIABLE TO RESET SET VARIABLE
C	BRNUM		NUMBER OF BREAK POINTS                   
C	I		NUMBER OF POINTS IN THE LINE SEGMENT
C	IPNT		COUNTER VARIABLE
C	DR		DUMMY VARIABLE
C	PI		PI = 3.1415926...
C	H,J,K,L		DO LOOP INDICES
C                   
C      SUBROUTINE CALLS
C      ----------------
C      LTRANS    CALCULATES THE LAT-LON TO X-Y COORD TRANSFORMATION
C                AND INDICATES ANY HIDDEN OR CUT-OFF POINTS.
C
C WRITTEN BY RICHARD P. LEE
C MODIFIED BY CRAIG K. FUJIMOTO
C
C MODIFICATION
C $Date$ $Revision$ $Author$
C
C 7/8/94  Nadia Adhami -port to UNIX/C (OPEN (..., READONLY)
C      take out READONLY.
C      READONLY was probably a VAX dependant feature, it is not part
C      of the standard fortran binding feature.
C
C 7/8/94  Nadia Adhami -port to UNIX/C (READ (...)
C	Compiler syntax error on READ statement containing implied loop,
C	converted the implicit loop to an explicit loop
C
C 4/14/95  Nadia Adhami   map is never filled ; CALL GSFAIS (GHOLLO)
C
C-----------------------------------------------------------------------
       SUBROUTINE WORLD (WSID,PROJN,RES,PLOT,
     1                   OBSLAT,OBSLON,
     3                   XEDGE,YEDGE,
     4                   MINX,MAXX,MINY,MAXY)

      character*100 SccsFileID
     -/'@(#)world.for	5.1 98/01/08 APS/ASF\0'/

       IMPLICIT NONE

C INPUT:
c--port       INCLUDE 'SYS$LIBRARY:GKSDEFS.BND'
      INCLUDE 'GKS_ROOT:include/fgksenum.inc'

    
       INTEGER WSID,RES,PROJN,PLOT

       REAL OBSLAT,OBSLON
       REAL XEDGE(*),YEDGE(*)
       REAL MINX,MAXX,MINY,MAXY

C INTERNAL:
       INTEGER IOS,BRPNT(50),BREAK,SET,SWITCH,BRNUM,IPNT
       INTEGER LORES,MEDRES,HIRES
       INTEGER LANDC,OCEANC
       INTEGER H,I,J,K

C SET RESOLUTION FLAGS
       DATA LORES  /1/
       DATA MEDRES /2/
       DATA HIRES  /3/

C SET COLORS FLAGS
       DATA LANDC /3/
       DATA OCEANC /2/

       REAL DLAT(5),DLON(5)
       REAL X10(10)
       REAL SX,SY
       REAL X(6000),Y(6000),X1(6000),Y1(6000)
       REAL SAVLT,SAVLN,SAVLT1,SAVLN1
       REAL BRLAT(50,2),BRLON(50,2)
       REAL BRX(2),BRY(2)
       REAL RADBR(2),DEGBR(2)
       REAL PI

C--    CHARACTER*30 MAPFIL

       PARAMETER (PI = 3.14159265359)

       LOGICAL HIDDEN,BRNCUT

       character*150 buf
       character*20  filename
       integer      bufsize
c       integer      status

       INTEGER*4 f77_getenv
       EXTERNAL  f77_getenv !$pragma c(f77_getenv)
       EXTERNAL  f77_aps_fullpath !$pragma c(f77_aps_fullpath)
       INTEGER*4 aps_fullpath
       EXTERNAL  aps_fullpath !$pragma c(aps_fullpath)

C-----------------------------------------------------------------------

C POLYLINE COLOR INDEX
       CALL GSPLCI (4)

C-old  FILL AREA INTERIOR STYLE TO SOLID OR HOLLOW
C-new  FILL AREA INTERIOR STYLE TO HOLLOW ALWAYS
       IF (PLOT .EQ. 0) THEN
C-new     CALL GSFAIS (GSOLID)
         CALL GSFAIS (GHOLLO)
       ELSE
         CALL GSFAIS (GHOLLO)
       END IF

C ASSIGN THE APPROPRIATE LOGICAL NAME FOR THE RESOLUTION

       IF (RES .EQ. LORES) THEN
         filename = 'maplores.dat' // char(0)
       ELSE IF (RES .EQ. MEDRES) THEN
         filename = 'mapmedres.dat' // char(0)
       END IF


       bufsize = 150
       CALL f77_aps_fullpath('APS_MAPPER',filename,buf,bufsize)

       IF (BUFSIZE .EQ. 0) THEN
         CALL DISMSG('Error: Map file location could not be found.')
         GO TO 9999
       ENDIF

C OPEN WORLD MAP FILE

c--port       OPEN (UNIT=10,FILE=MAPFIL,IOSTAT=IOS,STATUS='OLD',READONLY)
c--port       OPEN (UNIT=10,FILE=MAPFIL,IOSTAT=IOS,STATUS='OLD')
       OPEN (UNIT=10,FILE=buf,IOSTAT=IOS,STATUS='OLD')

C CHECK FOR ERROR
       IF (IOS .NE. 0) THEN
         CALL DISMSG('Error: Map file could not be opened.')
         GO TO 9999
       END IF

C SET FLAGS AND COUNTERS TO ZERO

       I = 0
       J = 0
       SET = 0
       BREAK = 0
       BRNUM = 0

  100  CONTINUE                                  
  
C READ IN THE FIVE PAIRS OF LAT-LON POINTS FROM THE WORLD MAP FILE


c--port  f77 compiler syntax error on implied loop in READ statement->
c--port  Have to convert this implicit READ loop to an explicite loop

c--port       READ (10,500,END = 300) ((DLAT(IPNT),DLON(IPNT)), IPNT =1,5)
c--port  500  FORMAT (5(F8.3,F8.3))

       READ (10,500,END = 300) X10
  500  FORMAT (5(F8.3,F8.3))

       DO 69 IPNT = 1,5

          DLAT(IPNT) = X10( 2 * IPNT - 1 )
          DLON(IPNT) = X10( 2 * IPNT)

  69   CONTINUE

C LOOP THROUGH EACH PAIR

       DO 70 IPNT = 1,5

C IF THE LAT-LON PAIR IS A PICK-UP PEN/FILL MARKER, DISPLAY THE 
C   LINE SEGMENT

         IF (DLAT(IPNT) .EQ. 999.01 .OR. DLAT(IPNT) .EQ. 999.02 .OR.
     1       DLAT(IPNT) .EQ. 999.03) THEN

C  IF THE NUMBER OF POINTS READ IN IS > 2, CONTUNUE

           IF (I .GE. 2) THEN

C  IF SATELLITE VIEW OR STEREOGRAPHIC

             IF (PROJN.EQ.1 .OR. PROJN.EQ.5 .OR. PROJN.EQ.6) THEN

C    IF PICK-UP PEN/FILL MARKER IS A POLITICAL BOUNDARY

               IF (DLAT(IPNT) .EQ. 999.03) THEN

C    IF THE NUMBER OF BREAK POINTS > ZERO, MARK EACH BREAK POINT

                 IF (BRNUM .GT. 0) THEN

C    LOOP THROUGH EACH BREAK POINT

                   DO 4 H = 1,BRNUM
                     K = 0

C    IF FIRST BREAK POINT, SAVE THE POINTS UP TO THIS BREAK POINT

                     IF (H .EQ. 1) THEN
                       DO 2 J = 1,BRPNT(1)
                         K = K + 1      
                         X1(K) = X(J)
                         Y1(K) = Y(J)
    2                  CONTINUE

C    IF NOT FIRST BREAK POINT, SAVE THE POINTS UP TO THIS BREAK POINT

                     ELSE                         
                       DO 3 J = BRPNT(H - 1) + 1,BRPNT(H)
                         K = K + 1
                         X1(K) = X(J)
                         Y1(K) = Y(J)
    3                  CONTINUE
                     END IF

C    IF NUMBER OF POINTS IN THE LINE SEGMENT IS > 1, DISPLAY BOUNDARY

                     IF (K .GT. 1) CALL GPL (K,X1,Y1)
    4              CONTINUE

C    IF THE LAST BREAK POINT IS NOT THE LAST POINT

                   IF (BRPNT(BRNUM) .NE. I) THEN
                     K = 0
                     DO 5 J = BRPNT(BRNUM) + 1,I
                       K = K + 1
                       X1(K) = X(J)
                       Y1(K) = Y(J)
    5                CONTINUE

C    IF NUMBER OF POINTS IN THE LINE SEGMENT IS > 1, DISPLAY BOUNDARY

                     IF (K .GT. 1) CALL GPL (K,X1,Y1)
                   END IF

C    RESET COUNTER AND FLAGS TO ZERO

                   I = 0
                   BREAK = 0
                   BRNUM = 0

C  IF NUMBER OF POINTS READ IN IS NOT > 2, CONITNUE LOOP

                 ELSE
                   GO TO 1000
                 END IF

C IF THE PICK-UP PEN/FILL MARKER IS NOT A POLITICAL BOUNDARY AND 
C   THE NUMBER OF BREAK POINTS > ZERO, MARK EACH BREAK POINT

               ELSE IF (BRNUM .GT. 0) THEN

C   IF THE LAST BREAK POINT IS THE LAST POINT, SET THE BREAK POINT EQUAL
C     TO THE SAVED POINT

                 IF (BRPNT(BRNUM) .EQ. I) THEN
                   BRLAT(BRNUM,2) = SAVLT1
                   BRLON(BRNUM,2) = SAVLN1
                 END IF

C   LOOP THROUGH EACH BREAK POINT

                 DO 50 H = 1,BRNUM
                   K = 0

C   IF SATELLITE VIEW

                   IF (PROJN .EQ. 1) THEN

C   IF FIRST BREAK POINT, SAVE THE POINTS UP TO THIS BEAK POINT

                     IF (H .EQ. 1) THEN
                       IF (BRPNT(BRNUM) .NE. I) THEN
                         DO 8 J = BRPNT(BRNUM) + 1,I
                           K = K + 1
                           X1(K) = X(J)
                           Y1(K) = Y(J)
    8                    CONTINUE
                       END IF
                       DO 10 J = 1,BRPNT(1)
                         K = K + 1
                         X1(K) = X(J)
                         Y1(K) = Y(J)
   10                  CONTINUE

C   CALCULATE THE X-Y TRANFORMATION OF THE BREAK POINT

                       CALL LTRANS (PROJN,OBSLAT,OBSLON,
     1                              BRLON(1,1),BRLAT(1,1),
     1                              BRX(1),BRY(1),
     1                              HIDDEN,BRNCUT)

C   CALCULATE THE X-Y TRANFORMATION OF THE BREAK POINT

                       CALL LTRANS (PROJN,OBSLAT,OBSLON,
     1                              BRLON(BRNUM,2),BRLAT(BRNUM,2),
     1                              BRX(2),BRY(2),
     1                              HIDDEN,BRNCUT)


C   IF NOT FIRST BREAK POINT, SAVE THE POINTS UP TO THIS BEAK POINT

                     ELSE
                       DO 15 J = BRPNT(H - 1) + 1,BRPNT(H)
                         K = K + 1
                         X1(K) = X(J)
                         Y1(K) = Y(J)
   15                  CONTINUE

C   CALCULATE THE X-Y TRANFORMATION OF THE BREAK POINT

                       CALL LTRANS (PROJN,OBSLAT,OBSLON,
     1                              BRLON(H,1),BRLAT(H,1),
     1                              BRX(1),BRY(1),
     1                              HIDDEN,BRNCUT)

C   CALCULATE THE X-Y TRANFORMATION OF THE BREAK POINT

                       CALL LTRANS (PROJN,OBSLAT,OBSLON,
     1                              BRLON(H - 1,2),BRLAT(H - 1,2),
     1                              BRX(2),BRY(2),
     1                              HIDDEN,BRNCUT)

                     END IF

C IF POLAR STEREOGRAPHIC          

                   ELSE

C IF FIRST BREAK POINT, SAVE THE POINTS UP TO THIS BREAK POINT

                     IF (H .EQ. 1) THEN
                               
C IF THE LAST BREAK POINT IS NOT THE LAST POINT, SET THE BREAK 
C POINT EQUAL TO THE SAVED POINT

                       IF (BRPNT(BRNUM) .NE. I) THEN
                         DO 16 J = BRPNT(BRNUM) + 1,I
                           K = K + 1
                           X1(K) = X(J)
                           Y1(K) = Y(J)
   16                    CONTINUE
                       END IF
                       DO 17 J = 1,BRPNT(1)
                         K = K + 1
                         X1(K) = X(J)
                         Y1(K) = Y(J)
   17                  CONTINUE

C CALCULATE THE X-Y TRANSFORMATION OF THE BREAK POINT

                       CALL LTRANS (PROJN,OBSLAT,OBSLON,
     1                              BRLON(1,1),BRLAT(1,1),
     1                              BRX(1),BRY(1),
     1                              HIDDEN,BRNCUT)

C CALCULATE THE X-Y TRANSFORMATION OF THE BREAK POINT

                       CALL LTRANS (PROJN,OBSLAT,OBSLON,
     1                              BRLON(BRNUM,2),BRLAT(BRNUM,2),
     1                              BRX(2),BRY(2),
     1                              HIDDEN,BRNCUT)
                           
C IF NOT FIRST BREAK POINT, SAVE THE POINTS UP TO THIS BREAK POINT

                     ELSE
                       DO 18 J = BRPNT(H - 1) + 1,BRPNT(H)
                         K = K + 1
                         X1(K) = X(J)
                         Y1(K) = Y(J)
   18                  CONTINUE

C CALCULATE THE X-Y TRANSFORMATION OF THE BREAK POINT

                       CALL LTRANS (PROJN,OBSLAT,OBSLON,
     1                              BRLON(H,2),BRLAT(H,2),
     1                              BRX(1),BRY(1),
     1                              HIDDEN,BRNCUT)

C CALCULATE THE X-Y TRANSFORMATION OF THE BREAK POINT

                       CALL LTRANS (PROJN,OBSLAT,OBSLON,
     1                              BRLON(H-1,2),BRLAT(H-1,2),
     1                              BRX(2),BRY(2),
     1                              HIDDEN,BRNCUT)
                     END IF

                   END IF

C CALCULATE THE BREAK POINTS ON THE UNIT CIRCLE IN 
C RADIANS AND DEGREES

                   RADBR(1) = ATAN2(BRY(1) - 0.5,BRX(1) - 0.5)
                   RADBR(2) = ATAN2(BRY(2) - 0.5,BRX(2) - 0.5)
                   DEGBR(1) = RADBR(1) * 180.0 / PI
                   DEGBR(2) = RADBR(2) * 180.0 / PI

C DETERMINE WHICH QUADRANT THE POINTS ARE IN 

                   IF (DEGBR(1) .LT. 0.0) DEGBR(1) = DEGBR(1) + 360.0
                   IF (DEGBR(2) .LT. 0.0) DEGBR(2) = DEGBR(2) + 360.0
                   IF (DEGBR(1) .GE. 270.0 .AND. DEGBR(2) .LE. 90.0)
     1                THEN

C CONNECT THE BREAK POINTS WITH POINTS ON THE UNIT CIRCLE

                     DO 20 J = INT(2.0 * DEGBR(1)),720
                       K = K + 1
                       X1(K) = XEDGE(J)
                       Y1(K) = YEDGE(J)
   20                CONTINUE
                     DO 25 J = 1,INT(2.0 * DEGBR(2))
                       K = K + 1
                       X1(K) = XEDGE(J)
                       Y1(K) = YEDGE(J)
   25                CONTINUE

C DETERMINE WHICH QUADRANT THE POINTS ARE IN 

                   ELSE IF (DEGBR(1) .LE. 90.0 .AND. 
     1                      DEGBR(2) .GE. 270.0) THEN

C CONNECT THE BREAK POINTS WITH POINTS ON THE UNIT CIRCLE

                     DO 30 J = INT(2.0 * DEGBR(1)),1,-1
                       K = K + 1
                       X1(K) = XEDGE(J)
                       Y1(K) = YEDGE(J)
   30                CONTINUE
                     DO 35 J = 720,INT(2.0 * DEGBR(2)),-1
                       K = K + 1
                       X1(K) = XEDGE(J)
                       Y1(K) = YEDGE(J)
   35                CONTINUE

C DETERMINE WHICH QUADRANT THE POINTS ARE IN 

                   ELSE IF (DEGBR(1) .LT. DEGBR(2)) THEN

C CONNECT THE BREAK POINTS WITH POINTS ON THE UNIT CIRCLE

                     DO 40 J = INT(2.0 * DEGBR(1)),INT(2.0 * DEGBR(2))
                       K = K + 1
                       X1(K) = XEDGE(J)
                       Y1(K) = YEDGE(J)
   40                CONTINUE

C DETERMINE WHICH QUADRANT THE POINTS ARE IN 

                   ELSE IF (DEGBR(1) .GT. DEGBR(2)) THEN

C CONNECT THE BREAK POINTS WITH POINTS ON THE UNIT CIRCLE

                     DO 45 J = INT(2.0 * DEGBR(1)),
     1                         INT(2.0 * DEGBR(2)),-1
                       K = K + 1
                       X1(K) = XEDGE(J)
                       Y1(K) = YEDGE(J)
   45                CONTINUE
                   END IF

C SET THE LAST POINT EQUAL TO THE FIRST POINT TO CLOSE 
C THE LINE SEGMENT

                   K = K + 1
                   X1(K) = X1(1)
                   Y1(K) = Y1(1)

C IF THE PICK-UP PEN/FIL MARKER IS A LAND MASS, SET THE FILL AREA 
C COLOR INDEX TO THE COLOR OF LAND, FILL IN THE LAND MASS

                   IF (DLAT(IPNT) .EQ. 999.01) THEN
                     CALL GSFACI (LANDC)
                     IF (K .GT. 2) CALL GFA (K,X1,Y1)  

C IF THE PICK-UP PEN/FIL MARKER IS A WATER MASS, SET THE FILL AREA 
C COLOR INDEX TO THE COLOR OF WATER, FILL IN THE WATER MASS

                   ELSE IF (DLAT(IPNT) .EQ. 999.02) THEN
                     CALL GSFACI (OCEANC)
                     IF (K .GT. 2) CALL GFA (K,X1,Y1)  

C IF THE PICK-UP PEN/FIL MARKER IS A POLITICAL BOUNDARY, DISPLAY THE
C POLITICAL BOUNDARY

                   ELSE IF (DLAT(IPNT) .EQ. 999.03) THEN
                     CALL GPL (K,X1,Y1)
                   END IF
   50            CONTINUE

C RESET COUNTER AND FLAGS TO ZERO

                 I = 0
                 BREAK = 0
                 BRNUM = 0

C IF THE PICK-UP PEN/FILL MARKER IS NOT A POLITICAL BOUNDARY AND 
C THE NUMBER OF BREAK POINTS IS ZERO, DISPLAY THE LINE SEGMENT

               ELSE
 1000            CONTINUE

C IF THE PICK-UP PEN/FIL MARKER IS A LAND MASS, SET THE FILL AREA 
C COLOR INDEX TO THE COLOR OF LAND, FILL IN THE LAND MASS

                 IF (DLAT(IPNT) .EQ. 999.01) THEN
                   CALL GSFACI (LANDC)
                   IF (I .GT. 2) CALL GFA (I,X,Y)  

C IF THE PICK-UP PEN/FIL MARKER IS A WATER MASS, SET THE FILL AREA 
C COLOR INDEX TO THE COLOR OF WATER, FILL IN THE WATER MASS

                 ELSE IF (DLAT(IPNT) .EQ. 999.02) THEN
                   CALL GSFACI (OCEANC)
                   IF (I .GT. 2) CALL GFA (I,X,Y)  

C IF THE PICK-UP PEN/FIL MARKER IS A POLITICAL BOUNDARY, DISPLAY THE
C POLITICAL BOUNDARY

                 ELSE IF (DLAT(IPNT) .EQ. 999.03) THEN
                   CALL GPL (I,X,Y)
                 END IF

C RESET COUNTER TO ZERO

                 I = 0
               END IF

C IF CYLINDRICAL EQUIDISTANT, MERCATOR, OR MILLER CYLINDRICAL
  
             ELSE IF (PROJN .EQ. 2 .OR. PROJN .EQ. 3 .OR. PROJN .EQ. 4)
     1          THEN

C IF THE LINE SEGMENT IS NOT BROKEN, DISPLAY THE LINE SEGMENT

               IF (BREAK .EQ. 0) THEN

C IF THE PICK-UP PEN/FIL MARKER IS A LAND MASS, SET THE FILL AREA 
C COLOR INDEX TO THE COLOR OF LAND, FILL IN THE LAND MASS

                 IF (DLAT(IPNT) .EQ. 999.01) THEN
                   CALL GSFACI (LANDC)
                   IF (I .GT. 2) CALL GFA (I,X,Y)  

C IF THE PICK-UP PEN/FIL MARKER IS A WATER MASS, SET THE FILL AREA 
C COLOR INDEX TO THE COLOR OF WATER, FILL IN THE WATER MASS

                 ELSE IF (DLAT(IPNT) .EQ. 999.02) THEN
                   CALL GSFACI (OCEANC)
                   IF (I .GT. 2) CALL GFA (I,X,Y)  

C IF THE PICK-UP PEN/FIL MARKER IS A POLITICAL BOUNDARY, DISPLAY THE
C POLITICAL BOUNDARY

                 ELSE IF (DLAT(IPNT) .EQ. 999.03) THEN
                   CALL GPL (I,X,Y)
                 END IF

C RESET COUNTER TO ZERO

                 I = 0

C IF THE LINE SEGMENT IS BROKEN, MARK THE BROKEN SEGMENTS

               ELSE

C IF THE PICK-UP PEN/FILL MARKER IS NOT A POLITICAL BOUNDARY 
C AND THE LINE SEGMENT IS AT THE POLES, COMPLETE THE 
C SEGMENT WITH THE EDGE OF THE WINDOW
        
                 IF (DLAT(IPNT) .NE. 999.03 .AND. X(1) .EQ. X1(J) 
     1               .AND. Y(1) .EQ. Y1(J)) THEN

C IF SOUTH POLE

                   IF (Y1(1) .LT. 0.5) THEN
                     IF (X1(1) .LT. 0.5) THEN
                       I = I + 1
                       X(I) = MAXX
                       Y(I) = MINY
                       I = I + 1
                       X(I) = MINX
                       Y(I) = MINY
                     ELSE
                       I = I + 1
                       X(I) = MINX
                       Y(I) = MINY
                       I = I + 1
                       X(I) = MAXX
                       Y(I) = MINY
                     END IF

C IF NORTH POLE

                   ELSE
                     IF (X1(1) .LT. 0.5) THEN
                       I = I + 1
                       X(I) = MAXX
                       Y(I) = MAXY
                       I = I + 1
                       X(I) = MINX
                       Y(I) = MAXY
                     ELSE
                       I = I + 1                              
                       X(I) = MINX
                       Y(I) = MAXY
                       I = I + 1
                       X(I) = MAXX
                       Y(I) = MAXY
                     END IF
                   END IF
                   DO 60 K = 1,J
                     I = I + 1
                     X(I) = X1(K)
                     Y(I) = Y1(K)
   60              CONTINUE

C IF THE PICK-UP PEN/FIL MARKER IS A LAND MASS, SET THE FILL AREA 
C COLOR INDEX TO THE COLOR OF LAND, FILL IN THE LAND MASS

                   IF (DLAT(IPNT) .EQ. 999.01) THEN
                     CALL GSFACI (LANDC)
                     IF (I .GT. 2) CALL GFA (I,X,Y)  

C IF THE PICK-UP PEN/FIL MARKER IS A WATER MASS, SET THE FILL AREA 
C COLOR INDEX TO THE COLOR OF WATER, FILL IN THE WATER MASS

                   ELSE IF (DLAT(IPNT) .EQ. 999.02) THEN
                     CALL GSFACI (OCEANC)
                     IF (I .GT. 2) CALL GFA (I,X,Y)  
                   END IF     

C IF THE LINE SEGMENT IS BROKEN AND IS NOT AT THE POLES, DISPLAY 
C THE LINE SEGMENT

                 ELSE

C IF THE PICK-UP PEN/FIL MARKER IS A LAND MASS, SET THE FILL AREA 
C COLOR INDEX TO THE COLOR OF LAND, FILL IN THE LAND MASS

                   IF (DLAT(IPNT) .EQ. 999.01) THEN
                     CALL GSFACI (LANDC)
                     IF (I .GT. 2) CALL GFA (I,X,Y)  
                     IF (J .GT. 2) CALL GFA (J,X1,Y1)  

C IF THE PICK-UP PEN/FIL MARKER IS A WATER MASS, SET THE FILL AREA 
C COLOR INDEX TO THE COLOR OF WATER, FILL IN THE WATER MASS

                   ELSE IF (DLAT(IPNT) .EQ. 999.02) THEN
                     CALL GSFACI (OCEANC)
                     IF (I .GT. 2) CALL GFA (I,X,Y)  
                     IF (J .GT. 2) CALL GFA (J,X1,Y1)  

C IF THE PICK-UP PEN/FIL MARKER IS A POLITICAL BOUNDARY, DISPLAY THE
C POLITICAL BOUNDARY

                   ELSE IF (DLAT(IPNT) .EQ. 999.03) THEN
                     IF (I .GT. 1) CALL GPL (I,X,Y)  
                     IF (J .GT. 1) CALL GPL (J,X1,Y1)
                   END IF
                 END IF

C RESET COUNTERS AND FLAGS TO ZERO

                 I = 0
                 J = 0
                 BREAK = 0
                 SET = 0
               END IF
             END IF

C IF THE NUMBER OF POINTS READ IN IS NOT > 2, CONTINUE LOOP

           ELSE
C XXXXX
C ADDED TO FIX BUG IN POLAR STEREO MED RES MAP
             I = 0
             BREAK = 0
             BRNUM = 0
C XXXXX
             GO TO 70                 
           END IF

C IF THE LAT-LON POINT IS EQUAL (0.0,0.0), CONTINUE LOOP 

         ELSE IF (DLAT(IPNT) .EQ. 0.0 .AND. DLON(IPNT) .EQ. 0.0) THEN
           GO TO 70

C IF THE LAT-LON POINT IS NOT A PICK-UP PEN/FILL MARKER OR EQUAL TO 
C   (0.0,0.0), CONTINUE

         ELSE

C IF NOT STEREOGRAPHIC, CALCULATE X-Y TRANSFORMATION OF THE LAT-LON

           IF (PROJN .NE. 5 .AND. PROJN .NE. 6) THEN
             CALL LTRANS (PROJN,OBSLAT,OBSLON,
     1                    DLON(IPNT),DLAT(IPNT),
     1                    SX,SY,
     1                    HIDDEN,BRNCUT)

C IF POLAR STEREOGRAPHIC

           ELSE

C IF POINT IS HIDDEN, MARK A BREAK POINT

             IF ((PROJN .EQ. 5 .AND. DLAT(IPNT) .LT. 0.0) .OR.
     1           (PROJN .EQ. 6 .AND. DLAT(IPNT) .GT. 0.0)) THEN
               IF (I .GT. 0 .AND. BREAK .EQ. 0) THEN
                 BRNUM = BRNUM + 1
                 IF (IPNT .EQ. 1) THEN
                   BRLAT(BRNUM,1) = SAVLT
                   BRLON(BRNUM,1) = SAVLN
                 ELSE
                   BRLAT(BRNUM,1) = DLAT(IPNT - 1)
                   BRLON(BRNUM,1) = DLON(IPNT - 1)
                 END IF
                 BRPNT(BRNUM) = I 
                 BREAK = 1
               END IF

C CONTINUE LOOP

               GOTO 70
             END IF
                 
C CALCULATE X-Y TRANSFORMATION OF THE LAT-LON

             CALL LTRANS (PROJN,OBSLAT,OBSLON,
     1                    DLON(IPNT),DLAT(IPNT),
     1                    SX,SY,
     1                    HIDDEN,BRNCUT)

C IF THE LINE SEGMENT IS BROKEN, MARK THE BREAK POINT

             IF (BREAK .EQ. 1) THEN
               BRLAT(BRNUM,2) = DLAT(IPNT)
               BRLON(BRNUM,2) = DLON(IPNT)
               BREAK = 0

C IF THE LINE SEGMENT IS NOT BROKEN, CHECK FOR ERROR IN THE DATA

             ELSE IF (I .GT. 0) THEN
               IF (ABS(SX - X(I)) .GT. 0.05 .OR. 
     1             ABS(SY - Y(I)) .GT. 0.05) GO TO 70
             END IF

C STORE THE LINE SEGMENT POINT INTO AN ARRAY

             I = I + 1
             X(I) = SX
             Y(I) = SY
 
C SAVE THE FIRST POINT OF THE LINE SEGMENT

             IF (I .EQ. 1) THEN
               SAVLT1 = DLAT(IPNT)
               SAVLN1 = DLON(IPNT)
             END IF

C CONITNUE LOOP

             GOTO 70
           ENDIF

C IF POINT IS HIDDEN, SAVE THE BREAK POINT

           IF (HIDDEN) THEN
             IF (I .GT. 0 .AND. BREAK .EQ. 0) THEN
               BRNUM = BRNUM + 1
               IF (IPNT .EQ. 1) THEN
                 BRLAT(BRNUM,1) = SAVLT
                 BRLON(BRNUM,1) = SAVLN
               ELSE
                 BRLAT(BRNUM,1) = DLAT(IPNT - 1)
                 BRLON(BRNUM,1) = DLON(IPNT - 1)
               END IF
               BRPNT(BRNUM) = I 
               BREAK = 1
             END IF

C CONTINUE  LOOP

             GO TO 70

C IF POINT IS NOT HIDDEN

           ELSE
             IF (PROJN .EQ. 1) THEN

C IF LINE SEGMENT IS BROKEN, SAVE THE BREAK POINT

               IF (BREAK .EQ. 1) THEN
                 BRLAT(BRNUM,2) = DLAT(IPNT)
                 BRLON(BRNUM,2) = DLON(IPNT)
                 BREAK = 0

C IF THE LINE SEGMENT IS NOT BROKEN, CHECK FOR ERROR IN THE DATA

               ELSE IF (I .GT. 0) THEN
                 IF (ABS(SX - X(I)) .GT. 0.05 .OR. 
     1               ABS(SY - Y(I)) .GT. 0.05) GO TO 70
               END IF

C STORE THE LINE SEGMENT POINT INTO AN ARRAY

               I = I + 1
               X(I) = SX
               Y(I) = SY

C SAVE THE FIRST POINT OF THE LINE SEGMENT

               IF (I .EQ. 1) THEN
                 SAVLT1 = DLAT(IPNT)
                 SAVLN1 = DLON(IPNT)
               END IF

C IF CYLINDRICAL EQUIDISTANT, MERCATOR, OR MILLER CYLINDRICAL

             ELSE IF (PROJN .EQ. 2 .OR. PROJN .EQ. 3 .OR. PROJN .EQ. 4)
     1          THEN

C IF NOT LINE SEGMENT IS NOT BROKEN, CHECK FOR ERROR IN THE DATA

               IF (.NOT. BRNCUT) THEN

C IF POINTS ARE ON THE BEGINNING SIDE OF THE LINE SEGMENT

                 IF (SET .EQ. 0) THEN
                   IF (I .GT. 0) THEN
                     IF (ABS(SX - X(I)) .GT. 0.05 .OR. 
     1                   ABS(SY - Y(I)) .GT. 0.05) GO TO 200
                   END IF

C STORE THE LINE SEGMENT POINT INTO AN ARRAY 
                         
                   I = I + 1
                   X(I) = SX
                   Y(I) = SY

C IF POINTS ARE ON THE BROKEN SIDE OF THE LINE SEGMENT, CHECK FOR
C ERROR IN THE DATA

                 ELSE
                   IF (J .GT. 0) THEN
                     IF (ABS(SX - X1(J)) .GT. 0.05 .OR. 
     1                   ABS(SY - Y1(J)) .GT. 0.05) GO TO 200
                   END IF

C STORE THE LINE SEGMENT POINT INTO AN ARRAY 

                   J = J + 1
                   X1(J) = SX
                   Y1(J) = SY
                 END IF

C IF THE LINE SEGMENT IS BROKEN, MARK THE BREAK ON BOTH SIDES

               ELSE
  200            CONTINUE
                 IF ((SET .EQ. 0 .AND. I .GE. 1) .OR.
     1               (SET .EQ. 1 .AND. J .GE. 1)) THEN
                   BREAK = 1
                   I = I + 1
                   J = J + 1
                   IF (((SX) .LT. 0.5 .AND. SET .EQ. 0) .OR.
     1                 ((SX) .GT. 0.5 .AND. SET .EQ. 1)) 
     2                THEN 
                     X(I) = MAXX 
                     X1(J) = MINX
                   ELSE IF (((SX) .LT. 0.5 .AND. SET .EQ. 1)
     1                      .OR. ((SX) .GT. 0.5 .AND. 
     2                      SET .EQ. 0)) THEN 
                     X(I) = MINX 
                     X1(J) = MAXX
                   END IF
                   IF (SET .EQ. 0) THEN
                     Y(I) = (Y(I - 1) + SY) / 2.0  
                     Y1(J) = (Y(I - 1) + SY) / 2.0
                     J = J + 1
                     X1(J) = SX
                     Y1(J) = SY
                   ELSE
                     Y(I) = (Y1(J - 1) + SY) / 2.0  
                     Y1(J) = (Y1(J - 1) + SY) / 2.0
                     I = I + 1
                     X(I) = SX
                     Y(I) = SY
                   END IF

C SET RESET VARIABLE 

                   IF (SET .EQ. 0) SWITCH = 1
                   IF (SET .EQ. 1) SWITCH = 0
                 END IF

C RESET SET VARIABLE
    
                 SET = SWITCH
               END IF
             END IF
           END IF
         END IF

C CONTINUE LOOP

   70  CONTINUE

C SAVE THE LAST GROUP OF LAT-LON POINTS 

      DO 80 K = 1,5
         IF (DLAT(K) .NE. 0.0 .AND. DLON(K) .NE. 0.0) THEN
           SAVLT = DLAT(K)
           SAVLN = DLON(K)
         END IF
   80  CONTINUE

C READ IN ANOTHER SET OF LAT-LON POINTS FROM THE WORLD MAP FILE

       GO TO 100

  300  CONTINUE

C CLOSE WORLD MAP FILE

       CLOSE (10)

 9999  CONTINUE
       RETURN
       END
