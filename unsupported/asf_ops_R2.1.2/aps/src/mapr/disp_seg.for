C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	disp_seg.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================

C-----------------------------------------------------------------
C SUBROUTINE DISP_SEG
C
C PURPOSE
C	REDISPLAY GSEGS INCLUDING:
C	501 = THE GLOBE - FILLED
C	503 = THE WORLD - FILLED
C	504 = THE GRID LINES
C	505 = THE GRID TEXT FOR DISPLAYING
C	OVERLAY GSEGS
C
C	IF THE WINDOW HAS BEEN RESET VIA ZOOM, THEN THE GRID
C	GSEGS (504,505,506) ARE DELETED AND RECREATED.
C
C $Logfile:   ACS003:[BLD.MPS.MAPR.SRC]DISP_SEG.FOV  $
C
C INPUT
C	WSID		WORKSTATION ID
C	PROJN		PROJECTION NO.
C	NUGRID		1=CREATE NEW GRID/0=NO NEW GRID
C	GRIDLN		GRID LINE FLAG
C	WNSIZE		WINDOW SIZE
C	MINLAT,MAXLAT,
C	MINLON,MAXLON	MIN/MAX LATS/LONS OF WINDOW
C	OBSLAT,OBSLON	CENTER LAT/LON
C	OBSMIN,OBSMAX	MIN/MAX LON OF WINDOW
C	LONPT		LONGITUDE DIVISIONS
C	NSEG		OVERLAY COUNT
C	CRSEGN		OVERLAY STATUS ARRAY
C	MINX,MAXX,
C	MINY,MAXY	MIN/MAX X/Y COORDS
C
C OUTPUT
C
C INTERNAL
C	I		LOOP INDEX
C
C SUBROUTINES CALLED
C	GRID
C
C GKS ROUTINES
C	GASGWK,GDSG,GUWK
C
C WRITTEN BY CRAIG K. FUJIMOTO - SEP 89
C
C MODIFICATIONS
C $Date$ $Revision$ $Author$
C 7/11/94  Nadia Adhami -port to UNIX/C 
C
C-----------------------------------------------------------------
      SUBROUTINE DISP_SEG (WSID,PROJN,NUGRID,GRIDLN,WNSIZE,
     1                    MINLAT,MAXLAT,MINLON,MAXLON,
     2                    OBSLAT,OBSLON,OBSMIN,OBSMAX,LONPT,
     3                    NSEG,CRSEGN,
     4                    MINX,MAXX,MINY,MAXY)

      character*100 SccsFileID
     -/'@(#)disp_seg.for	5.1 98/01/08 APS/ASF\0'/
                    
      IMPLICIT NONE

C INPUT:                             
       INCLUDE 'GKS_ROOT:include/fgksenum.inc'
                                    
      INTEGER PROJN,WSID,NUGRID,GRIDLN
      INTEGER CRSEGN(*),NSEG

      REAL OBSLAT,OBSLON
      REAL MINLAT,MAXLAT,MINLON,MAXLON
      REAL OBSMIN,OBSMAX
      REAL WNSIZE,LONPT(*)
      REAL MINX,MAXX,MINY,MAXY
      
C INTERNAL:
      INTEGER I
C-----------------------------------------------------------------------

C DISPLAY THE GLOBE AND WORLD GSEGS
      CALL GASGWK(WSID,501)
      CALL GASGWK(WSID,503)
                                               
C UPDATE THE WORKSTATION
      CALL GUWK(WSID,GPERFO)
                           
C IF GRID SHOULD BE RECALCULATED AND REDRAWN
      IF (NUGRID .EQ. 1) THEN

C DELETE THE GRID GSEGS
        CALL GDSG (504)
        CALL GDSG (505)
        CALL GDSG (506)

C DISPLAY GRID LINES AND TEXT
        CALL GRID (WSID,PROJN,GRIDLN,
     1             MINLAT,MAXLAT,MINLON,MAXLON,
     3             OBSLAT,OBSLON,OBSMIN,OBSMAX,
     4             LONPT,
     5             MINX,MAXX,MINY,MAXY)

C IF NO NEW GRID NEEDED, USE THE OLD GRID GSEGS
      ELSE
C GRID LINES
        CALL GASGWK(WSID,504)
C GRID TEXT
        CALL GASGWK(WSID,505)
      END IF


C DISPLAY EACH DESIRED GSEG ONTO THE MAP
      DO 1000 I = 1,NSEG

        IF (CRSEGN(I) .EQ. 1) THEN

          CALL GASGWK (WSID,I)

        END IF

 1000 CONTINUE

 9999 CONTINUE
      RETURN
      END
