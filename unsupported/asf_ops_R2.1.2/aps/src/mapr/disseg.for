C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	disseg.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================

C-----------------------------------------------------------------
C SUBROUTINE DISSEG
C
C PURPOSE
C	DISPLAY SEGMENTS ON THE MAP
C
C $LOGFILE$
C
C INPUT
C	WSID		WORKSTATION ID
C	PROJN		PROJECTION NO.
C	GRIDLN		GRID LINE FLAG
C	WNSIZE		WINDOW SIZE
C	MINLAT,MAXLAT	MIN/MAX LAT OF WINDOW
C	MINLON,MAXLON	MIN/MAX LON OF WINDOW
C	OBSLAT,OBSLON	CENTER LAT/LON
C	OBSMIN,OBSMAX	MIN/MAX LON OF WINDOW
C	LONPT		LONGITUDE DIVISIONS
C	NSEG		OVERLAY COUNT
C	CRSEGN		OVERLAY STATUS ARRAY
C
C OUTPUT
C	MINX,MAXX,
C	MINY,MAXY	MIN/MAX X/Y COORDS
C	MINXWN,MAXXWN,
C	MINYWN,MAXYWN	MIN/MAX X/Y OF ENTIRE WINDOW
C
C INTERNAL
C	DIFLAT,DIFLON	WINDOW HEIGHT/WIDTH (DEG)
C	XEDGE,YEDGE	X/Y COORD ARRAYS FOR UNIT CIRCLE
C	I		LOOP INDEX
C	PLOT		PLOT FLAG
C
C SUBROUTINES CALLED
C	WINDOW
C	GRID
C
C QRIGINALLY WRITTEN BY RICHARD P. LEE
C MODIFIED FOR ASF BY CRAIG K. FUJIMOTO - SEP 89
C
C MODIFICATIONS
C $DATE$ $REVISION$ $AUTHOR$
C 7/11/94  Nadia Adhami -port to UNIX/C 
C
C-----------------------------------------------------------------
       SUBROUTINE DISSEG (WSID,PROJN,GRIDLN,WNSIZE,
     1                    MINLAT,MAXLAT,MINLON,MAXLON,
     2                    OBSLAT,OBSLON,OBSMIN,OBSMAX,LONPT,
     4                    NSEG,CRSEGN,
     5                    MINX,MAXX,MINY,MAXY,
     6                    MINXWN,MAXXWN,MINYWN,MAXYWN)

      character*100 SccsFileID
     -/'@(#)disseg.for	5.1 98/01/08 APS/ASF\0'/

       IMPLICIT NONE

C INPUT: 
       INCLUDE 'GKS_ROOT:include/fgksenum.inc'

                                    
       INTEGER PROJN,WSID,GRIDLN
       INTEGER CRSEGN(*),NSEG

       REAL OBSLAT,OBSLON
       REAL MINLAT,MAXLAT,MINLON,MAXLON
       REAL OBSMIN,OBSMAX
       REAL WNSIZE,LONPT(*)

C OUTPUT::
       REAL MINX,MAXX,MINY,MAXY
       REAL MINXWN,MAXXWN,MINYWN,MAXYWN

C INTERNAL:
       INTEGER I,PLOT

       REAL DIFLAT,DIFLON
C       REAL XEDGE(720),YEDGE(720)
C-----------------------------------------------------------------------

C INIT THE PLOT FLAG
       PLOT = 0

C CALCULATE THE WINDOW DIMENSIONS
       CALL WINDOW (WSID,PROJN,PLOT,WNSIZE,
     1              MINLAT,MAXLAT,MINLON,MAXLON,
     3              OBSLAT,OBSLON,OBSMIN,OBSMAX,
     7              MINX,MAXX,MINY,MAXY,
     8              MINXWN,MAXXWN,MINYWN,MAXYWN,
     1              DIFLAT,DIFLON)


C   DISPLAY EACH DESIRED SEGMENT ONTO THE MAP

         DO 10 I = 1,NSEG

C   IF FIRST SEGMENT IS THE WORLD, DISPLAY WORLD AND GRID

           IF (I .EQ. 1) THEN

             CALL GASGWK (WSID,I)

             CALL GUWK(WSID,GPERFO)

C DISPLAY GRID LINES AND TEXT
             CALL GRID (PROJN,PLOT,GRIDLN,
     1                    MINLAT,MAXLAT,MINLON,MAXLON,
     2                    DIFLAT,DIFLON,
     3                    OBSLAT,OBSLON,OBSMIN,OBSMAX,
     4                    LONPT,
     5                    MINX,MAXX,MINY,MAXY)

           ELSE IF (CRSEGN(I) .EQ. 1) THEN

             CALL GASGWK (WSID,I)

           END IF

   10    CONTINUE

 9999  CONTINUE
       RETURN
       END
