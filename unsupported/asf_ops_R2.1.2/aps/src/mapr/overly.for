C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	overly.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================

C-----------------------------------------------------------------
C SUBROUTINE OVERLY
C
C PURPOSE
C	DISPLAY OVERLAYS ON THE MAP
C
C $Logfile:   ACS003:[BLD.MPS.MAPR.SRC]OVERLY.FOV  $
C
C INPUT
C	WSID		WORKSTATION ID
C	PROJN		PROJECTION NO.
C	MINLAT,MAXLAT	MIN/MAX LAT OF WINDOW
C	MINLON,MAXLON	MIN/MAX LON OF WINDOW
C	OBSLAT,OBSLON	CENTER LAT/LON
C	OBSMIN,OBSMAX	MIN/MAX LON
C	NSEG		OVERLAY COUNT
C	CRSEGN		OVERLAY STATUS ARRAY
C	CRSEGT		OVERLAY NAME ARRAY
C	CRDARID		OVERLAY DARID ARRAY
C	MINX,MAXX,
C	MINY,MAXY	MIN/MAX X/Y COORDS
C	MINXWN,MAXXWN,
C	MINYWN,MAXYWN	MIN/MAX X/Y OF ENTIRE WINDOW
C	STMNLT,STMXLT,
C	STMNLN,STMXLN	MIN/MAX LAT/LON OF MAP - ORIGINAL
C
C INTERNAL
C	OLN		MENU OPTION
C	STMNX,STMXX,
C	STMNY,STMXY	MIN/MAX X/Y
C	STHID,BRNCUT	LTRANS RETURN FLAGS
C	WINMIN,WINMAX	WINDOW MIN/MAX
C
C SUBROUTINES CALLED
C
C WRITTEN BY CRAIG K. FUJIMOTO - SEP 89
C
C MODIFICATIONS
C $Date$ $Revision$ $Author$
C-----------------------------------------------------------------
       SUBROUTINE OVERLY (WSID,PROJN,
     1                    MINLAT,MAXLAT,MINLON,MAXLON,
     2                    NSEG,CRSEGN,CRSEGT,CRDARID,
     3                    STMNLT,STMXLT,STMNLN,STMXLN,
     1                    MINX,MAXX,MINY,MAXY,
     4                    MINXWN,MAXXWN,MINYWN,MAXYWN,
     6                    OBSLAT,OBSLON,OBSMIN,OBSMAX,
     7                    GRIDLN,WNSIZE,LONPT)

      character*100 SccsFileID
     -/'@(#)overly.for	5.1 98/01/08 APS/ASF\0'/

       IMPLICIT NONE

C INPUT:
       CHARACTER*(*) CRSEGT(200)

       INTEGER PROJN,WSID
       INTEGER NSEG,CRSEGN(200),CRDARID(*)

       REAL OBSLON,OBSLAT
       REAL OBSMIN,OBSMAX
       REAL MINLAT,MAXLAT,MINLON,MAXLON
       REAL STMNLT,STMXLT,STMNLN,STMXLN
       REAL MINX,MAXX,MINY,MAXY
       REAL MINXWN,MAXXWN,MINYWN,MAXYWN

       INTEGER GRIDLN,WNSIZE
       REAL LONPT(*)
 

C INTERNAL:
       INTEGER OLN

       REAL STMNX,STMXX,STMNY,STMXY
       REAL WINMIN,WINMAX

       LOGICAL STHID,BRNCUT

       INTEGER NUGRID

C-----------------------------------------------------------------------

        NUGRID = 0

C CONVERT THE MIN AND MAX WINDOW LAT/LONS TO X/Y

        CALL CALCOBS(OBSLON,
     1               MINLAT,MAXLAT,MINLON,MAXLON,
     1               WINMIN,WINMAX)

        CALL LTRANS (PROJN,OBSLAT,OBSLON,
     1               WINMIN,STMNLT,
     2               STMNX,STMNY,
     3               STHID,BRNCUT)

        CALL LTRANS (PROJN,OBSLAT,OBSLON,
     1               WINMAX,STMXLT,
     2               STMXX,STMXY,
     3               STHID,BRNCUT)

C        CALL LTRANS (PROJN,OBSLAT,OBSLON,
C     1               OBSMIN,STMNLT,
C     2               STMNX,STMNY,
C     3               STHID,BRNCUT)
C
C        CALL LTRANS (PROJN,OBSLAT,OBSLON,
C     1               OBSMAX,STMXLT,
C     2               STMXX,STMXY,
C     3               STHID,BRNCUT)

 1000 CONTINUE

C ASK USER TO CHOOSE AN OVERLAY FUNCTION

        CALL OVERMNU(OLN)

        IF (OLN .EQ. 99) THEN
 
C REDISPLAY THE WINDOW
            CALL SET_WINDOW (WSID,WNSIZE,
     1                       MINXWN,MAXXWN,MINYWN,MAXYWN)
 
C DISPLAY THE SEGMENTS
            CALL DISP_SEG (WSID,PROJN,NUGRID,GRIDLN,WNSIZE,
     1              MINLAT,MAXLAT,MINLON,MAXLON,
     2              OBSLAT,OBSLON,OBSMIN,OBSMAX,LONPT,
     3              NSEG,CRSEGN,
     4              MINX,MAXX,MINY,MAXY)
 
            GOTO 1000

        ENDIF

C DISPLAY SITES

        IF (OLN .EQ. 1) THEN

C  ASKS FOR SITE DISPLAY INFORMATION
                      
          CALL MPSSITE (WSID,PROJN,
     1                  OBSLAT,OBSLON,
     2                  STMNX,STMXX,STMNY,STMXY,
     3                  NSEG,CRSEGN,CRSEGT)

C DISPLAY DARS

        ELSE IF (OLN .EQ. 2) THEN

          CALL MPSDAR (WSID,PROJN,
     1                OBSLAT,OBSLON,
     2                STMNX,STMXX,STMNY,STMXY,
     3                NSEG,CRSEGN,CRSEGT)

C DISPLAY SEGMENTS

        ELSE IF (OLN .EQ. 3) THEN

          CALL MPSSEG (WSID,PROJN,
     1                OBSLAT,OBSLON,
     2                STMNX,STMXX,STMNY,STMXY,
     2                NSEG,CRSEGN,CRSEGT)

C DISPLAY DATATAKES

        ELSE IF (OLN .EQ. 4) THEN

          CALL MPSDTK (WSID,PROJN,
     1                OBSLAT,OBSLON,
     2                STMNX,STMXX,STMNY,STMXY,
     2                NSEG,CRSEGN,CRSEGT)

C DISPLAY CVRG FROM A FILE

        ELSE IF (OLN .EQ. 5) THEN

          CALL MPSCVG (WSID,PROJN,
     1                OBSLAT,OBSLON,
     2                STMNX,STMXX,STMNY,STMXY,
     2                NSEG,CRSEGN,CRSEGT)

C DISPLAY SPECIFIC SITE COVERAGE

        ELSE IF (OLN .EQ. 6) THEN

          CALL MPSSPC (WSID,PROJN,
     1                OBSLAT,OBSLON,
     2                STMNX,STMXX,STMNY,STMXY,
     2                NSEG,CRSEGN,CRSEGT,CRDARID)
                       
        ELSE
          GO TO 9999
        END IF
 
        GO TO 1000

 9999  CONTINUE
       RETURN
       END
