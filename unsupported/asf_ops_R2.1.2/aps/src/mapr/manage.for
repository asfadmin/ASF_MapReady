C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	manage.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================

C-----------------------------------------------------------------
C SUBROUTINE MANAGE
C
C PURPOSE
C	ALLOWS USER TO REMOVE,REPLACE,DELETE,REFRESH,SAVE OVERLAYS
C
C $Logfile:   ACS003:[BLD.MPS.MAPR.SRC]MANAGE.FOV  $
C
C INPUT     
C	WSID		WORKSTATION ID
C	PROJN		PROJECTION NO.
C	DISP		MAP DISPLAY - WORLD,METAFILE
C	GRIDLN		GRID LINE FLAG
C	WNSIZE		WINDOW SIZE
C	MINLAT,MAXLAT	MIN/MAX LAT OF WINDOW
C	MINLON,MAXLON	MIN/MAX LON OF WINDOW
C	OBSLAT,OBSLON	CENTER LAT/LON
C	OBSMIN,OBSMAX	MIN/MAX LON OF WINDOW
C	LONPT		LONGITUDE DIVISIONS
C	NSEG		OVERLAY COUNT
C	CRSEGN		OVERLAY STATUS ARRAY
C	CRSEGT		OVERLAY NAME ARRAY
C
C INTERNAL
C	MGN		MANAGE MENU OPTION
C	MINX,MAXX,
C	MINY,MAXY	MIN/MAX X/Y COORDS
C	MINXWN,MAXXWN,
C	MINYWN,MAXYWN	MIN/MAX X/Y OF ENTIRE WINDOW
C	IOS		I/O STATUS
C	PLOT		PLOTTER FLAG
C	NUGRID		0 = NO NEW GRID GSEG NEEDED
C
C SUBROUTINES CALLED
C	RMVSEG
C	RPLSEG
C	DELSEG
C	DISSEG
C	RMVALL
C	SPC2SEG
C
C WRITTEN BY RICHARD P. LEE
C MODIFIED FOR ASF BY CRAIG K. FUJIMOTO - SEP 89
C
C MODIFICATIONS
C $Date$ $Revision$ $Author$
C
C  12/20/94 Nadia Adhami   Redraw the map when overlays are removed, replaced,
C                          deleted, or all removed. (options 1 thru 4)
C
C-----------------------------------------------------------------
       SUBROUTINE MANAGE (p_userid,p_password,
     6                    WSID,PROJN,GRIDLN,WNSIZE,
     1                    MINLAT,MAXLAT,MINLON,MAXLON,
     2                    OBSLAT,OBSLON,OBSMIN,OBSMAX,LONPT,
     3                    NSEG,CRSEGN,CRSEGT,CRDARID,
     4                    MINX,MAXX,MINY,MAXY,
     5                    MINXWN,MAXXWN,MINYWN,MAXYWN)

      character*100 SccsFileID
     -/'@(#)manage.for	5.1 98/01/08 APS/ASF\0'/

       IMPLICIT NONE

C INPUT: 
       CHARACTER*(*) CRSEGT(*)
       CHARACTER*100 p_userid,p_password

       INTEGER PROJN,WSID,GRIDLN,WNSIZE
       INTEGER NSEG,CRSEGN(*),CRDARID(*)

       REAL OBSLAT,OBSLON
       REAL MINLAT,MAXLAT,MINLON,MAXLON
       REAL OBSMIN,OBSMAX
       REAL LONPT(*)

       REAL MINX,MAXX,MINY,MAXY
       REAL MINXWN,MAXXWN,MINYWN,MAXYWN

C INTERNAL:
       INTEGER MGN,DFLAG,PLOT,NUGRID

C-----------------------------------------------------------------------

C PROMPT FOR A SEGMENT FUNCTION

       DFLAG = 0
       MGN = 10
       PLOT = 0
       NUGRID = 0

       DO WHILE (MGN .NE. 0)

        CALL MNGMNU(MGN)


        IF (MGN .EQ. 1) THEN

C REMOVE OVERLAY
          CALL RMVSEG (NSEG,CRSEGN,CRSEGT,DFLAG)

        ENDIF

        IF (MGN .EQ. 2) THEN

C REPLACE OVERLAY
          CALL RPLSEG (NSEG,CRSEGN,CRSEGT,DFLAG)

        ENDIF

        IF (MGN .EQ. 3) THEN

C DELETE OVERLAY
          CALL DELSEG (NSEG,CRSEGN,CRSEGT,DFLAG)

        ENDIF

        IF (MGN .EQ. 4) THEN

C REMOVE ALL OVERLAYS
          CALL RMVALL (NSEG,CRSEGN,CRSEGT,DFLAG)

        ENDIF

C REDRAW MAP FOR OPTIONS 1 THRU 5

        IF (MGN .EQ. 1 .OR. MGN .EQ. 2 .OR. MGN .EQ. 3
     1      .OR. MGN .EQ. 4 .OR. MGN .EQ. 5) THEN

C REDISPLAY THE SEGMENTS

c                always refresh display
c--port          IF (DFLAG .NE. 0) THEN

C REDISPLAY THE WINDOW
            CALL SET_WINDOW (WSID,WNSIZE,
     1                       MINXWN,MAXXWN,MINYWN,MAXYWN)

C DISPLAY THE SEGMENTS
            CALL DISP_SEG (WSID,PROJN,NUGRID,GRIDLN,WNSIZE,
     1              MINLAT,MAXLAT,MINLON,MAXLON,
     2              OBSLAT,OBSLON,OBSMIN,OBSMAX,LONPT,
     3              NSEG,CRSEGN,
     4              MINX,MAXX,MINY,MAXY)

c--port          END IF

          DFLAG = 0

        ENDIF

        IF (MGN .EQ. 6) THEN

C WRITE SEG FILE
          CALL SPC2SEG (NSEG,CRSEGN,CRSEGT,CRDARID,.FALSE.,
     1        char(0),char(0))

        ELSE IF (MGN .EQ. 7) THEN

C WRITE SEG FILE
          CALL SPC2SEG (NSEG,CRSEGN,CRSEGT,CRDARID,.TRUE.,
     1        p_userid,p_password)

        END IF

      END DO

C REDISPLAY THE SEGMENTS BEFORE EXITING

      IF (DFLAG .NE. 0) THEN

C SET THE WINDOW
        CALL SET_WINDOW (WSID,WNSIZE,
     1                   MINXWN,MAXXWN,MINYWN,MAXYWN)

C DISPLAY THE SEGMENTS
        CALL DISP_SEG (WSID,PROJN,NUGRID,GRIDLN,WNSIZE,
     1              MINLAT,MAXLAT,MINLON,MAXLON,
     2              OBSLAT,OBSLON,OBSMIN,OBSMAX,LONPT,
     3              NSEG,CRSEGN,
     4              MINX,MAXX,MINY,MAXY)

      END IF

 9999 CONTINUE
      RETURN
      END
