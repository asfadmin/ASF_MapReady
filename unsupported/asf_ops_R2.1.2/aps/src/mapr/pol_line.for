C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	pol_line.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================

C-----------------------------------------------------------------
C
C SUBROUTINE POL_LINE
C
C PURPOSE
C	DRAW A SIMPLE LINE BETWEEN TWO POINTS REGARDLESS OF
C	FLAGS RETURNED BY LTRANS
C
C $Logfile:   ACS003:[BLD.MPS.MAPR.SRC]POL_LINE.FOV  $
C
C INPUT                                    
C	PROJN		PROJECTION NO.
C	OBSLAT,OBSLON	CENTER LAT/LON
C	LAT1,LON1,
C	LAT2,LON2	LAT/LON POINTS TO DRAW LINE BETWEEN
C
C WRITTEN BY CRAIG K. FUJIMOTO
C
C MODIFICATIONS
C $Date$ $Revision$ $Author$
C
C-----------------------------------------------------------------
      SUBROUTINE POL_LINE(PROJN,OBSLAT,OBSLON,
     1                    LAT1,LON1,LAT2,LON2)

      character*100 SccsFileID
     -/'@(#)pol_line.for	5.1 98/01/08 APS/ASF\0'/

      IMPLICIT NONE

C INPUT
c--port      INCLUDE 'SYS$LIBRARY:GKSDEFS.BND'
       INCLUDE 'GKS_ROOT:include/fgksenum.inc'

      INTEGER PROJN
      REAL OBSLAT,OBSLON
      REAL LAT1,LAT2
      REAL LON1,LON2

C INTERNAL
      REAL X(2),Y(2)
      LOGICAL HIDDEN,BRNCUT

C-----------------------------------------------------------------

      CALL LTRANS(PROJN,OBSLAT,OBSLON,
     1              LON1,LAT1,
     1              X(1),Y(1),
     1              HIDDEN,BRNCUT)

      CALL LTRANS(PROJN,OBSLAT,OBSLON,
     1              LON2,LAT2,
     1              X(2),Y(2),
     1              HIDDEN,BRNCUT)

      CALL GPL(2,X,Y)

 9999 CONTINUE
      RETURN
      END

C-----------------------------------------------------------------
C SUBROUTINE POL_ARC
C
C PURPOSE
C	DRAW AN ARC REGARDLESS OF FLAGS RETURNED BY LTRANS
C
C INPUT
C	PROJN		PROJECTION NO.
C	OBSLAT,OBSLON	CENTER LAT/LON
C	LAT		LATITUDE ARC SHOULD BE DRAWN AT
C	LON1,LON2	ARC TO BE DRAWN FROM LON1 TO LON2
C INTERNAL
C	DATALEN		LENGTH OF DATA RECORD
C	LIST_INTS	DATA FOR PACKING DATA RECORD
C	MAXSIZE		""
C	IOS		I/O STATUS
C	X,Y		X,Y COORDS OF ARC ENDS
C	EMPTY_REAL	DUMMY
C	DATAREC		DATA RECORD FOR ARC ROUTINE
C	ENPTY_CHAR	DUMMY
C	HIDDEN,BRNCUT	LTRANS FLAGS
C
C WRITTEN BY CRAIG K. FUJIMOTO
C-----------------------------------------------------------------
      SUBROUTINE POL_ARC(PROJN,OBSLAT,OBSLON,
     1                   LAT,LON1,LON2)

      IMPLICIT NONE
C INPUT
c--port      INCLUDE 'SYS$LIBRARY:GKSDEFS.BND'
       INCLUDE 'GKS_ROOT:include/fgksenum.inc'
       INCLUDE 'mapper_port.inc'

      INTEGER PROJN
      REAL OBSLAT,OBSLON
      REAL LAT
      REAL LON1,LON2
C INTERNAL
      INTEGER DATALEN
      INTEGER LIST_INTS(1)
      INTEGER MAXSIZE
      INTEGER IOS
      REAL X(3),Y(3)
      REAL EMPTY_REAL
      CHARACTER*80 DATAREC(5)
      CHARACTER*1 EMPTY_CHAR
      LOGICAL HIDDEN,BRNCUT


C-----------------------------------------------------------------


C DETERMINE THE CENTER X AND Y
      CALL LTRANS(PROJN,OBSLAT,OBSLON,
     1              OBSLON,OBSLAT,
     1              X(1),Y(1),
     1              HIDDEN,BRNCUT)

C NOTE - POINTS MUST BE REVERSED AS ARC IS DRAWN IN COUNTERCLOCKWISE DIR
C DETERMINE THE FIRST X,Y
      CALL LTRANS(PROJN,OBSLAT,OBSLON,
     1              LON2,LAT,
     1              X(2),Y(2),
     1              HIDDEN,BRNCUT)

C DETERMINE THE SECOND X,Y
      CALL LTRANS(PROJN,OBSLAT,OBSLON,
     1              LON1,LAT,
     1              X(3),Y(3),
     1              HIDDEN,BRNCUT)

C IF AT THE CENTER OF THE MAP, DRAW DOT
      IF (LAT .EQ. 90.0 .OR. LAT .EQ. -90.0) THEN

C        CALL GPM(1,X(2),X(3))

C IF LONS ARE EQUAL, DRAW CIRCLE
      ELSE IF (LON1 .EQ. LON2 .OR.
     1    (LON1 .EQ. -180.0 .AND. LON2 .EQ. 180.0) .OR.
     2    (LON1 .EQ. 180.0 .AND. LON2 .EQ. -180.0)) THEN

        CALL GGDP(2,X,Y,GGCCP,0,DATAREC)

C ALL OTHER - DRAW ARC
      ELSE

C PACK DATA RECORD
        LIST_INTS(1) = GATOPN
        DATALEN = 16
        MAXSIZE = 5                                 

        CALL GPREC(1,LIST_INTS,0,EMPTY_REAL,
     1           0,0,EMPTY_CHAR,
     1           MAXSIZE,IOS,DATALEN,DATAREC)

C DRAW THE ARC
        CALL GGDP(3,X,Y,GGAC2P,DATALEN,DATAREC)

      END IF

 9999 CONTINUE
      RETURN
      END
