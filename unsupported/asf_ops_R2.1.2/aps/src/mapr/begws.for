C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	begws.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================

C-----------------------------------------------------------------------
C
C SUBROUTINE BEGWS
C
C $Logfile:   QDISK:[BLD.MPS.MAPR.SRC]BEGWS.FOV  $
C
C PURPOSE
C 	INITIALIZES GKS GRAPHICS BY OPENING AND ACTIVATING THE WORKSTATION.
C	IT ALSO INITIALIZES COLORS
C
C VARIABLES
C INPUT
C	WSID		WORKSTATION ID NUMBER
C	WISS		WORKSTATION INDEPENDENT SEGMENT STORAGE ID NUMBER
C                      
C INTERNAL
C	DEVZERO		ZERO DEVICE
C
C ORIGINALLY WRITTEN BY RICHARD P. LEE     5-25-88
C MODIFIED FOR ASF BY CRAIG K. FUJIMOTO  8-01-88
C
C Date:   18 Jul 1990 17:04:30  Revision:   2.0  Author:   DBMAN  
C $Date$ $Revision$ $Author$
C 7/11/94  Nadia Adhami -port to UNIX/C gks include file
C 7/11/94  Nadia Adhami size of window adjusted by parameters to GPREC
C 7/11/94  Nadia Adhami window length = 1000 ; height = 800
C
C-----------------------------------------------------------------------

       SUBROUTINE BEGWS (WSID,WISS)

      character*100 SccsFileID
     -/'@(#)begws.for	5.1 98/01/08 APS/ASF\0'/

       IMPLICIT NONE

C INPUT:
c--port       INCLUDE 'SYS$LIBRARY:GKSDEFS.BND'
       INCLUDE 'GKS_ROOT:include/fgksenum.inc'
       INCLUDE 'mapper_port.inc'


       INTEGER WSID,WISS
       INTEGER IA(10), LSTR(1), ERRIND, LDR, LODR
       CHARACTER*80  DATREC(2), ODR(2), STR
       REAL    RA(1)



C INTERNAL:
       INTEGER DEVZERO

C      ZERO DEVICE
       DEVZERO = 0

c--port GPREC & GESC

c--port Resizing Window :

C**     Use ESCAPE to set the window dimensions for the X window
C**     which will be automatically created by the X11 workstation.
C**     The X coordinate system is inverted so the upper left corner
C**     is located at (0,0) upper left hand corner IA(3) & IA(4) and 
C**     the window size is 1000 by 800 IA(5) & IA(6).

        IA( 1 ) = 1
        IA( 2 ) = 0
        IA( 3 ) = 0
        IA( 4 ) = 0
        IA( 5 ) = 1000
        IA( 6 ) = 800
        STR = WINDOW_NAME
        LSTR( 1 ) = 10
        CALL GPREC( 6, IA, 0, RA, 1, LSTR, STR, 2, ERRIND, LDR, DATREC )
        CALL GESC( -9, LDR, DATREC, 2, LODR, ODR )
 

C OPEN AND ACTIVATE THE WORKSTATION
C--port CALL GOPWK (WSID,DEVZERO,GV2000)

       CALL GOPWK (WSID,GNULL,GMOTIF)
       CALL GSDS (WSID,0,0)

C**     OPEN WORKSTATION 1 (X11 colour workstation).
C**     The X11 workstation uses an optional special connection
C**     where an X window can be opened by the application. If GNULL
C**     is used as the connection, an X window will automatically be
C**     created.
C X11 1100 XGL 1130

       CALL GACWK (WSID)

C OPEN AND ACTIVATE THE WORKSTATION INDEPENDENT SEGMENT STORAGE
c--port       CALL GOPWK (WISS,DEVZERO,GWSWIS)
       CALL GOPWK (WISS,GNULL,GWSWIS)
       CALL GACWK (WISS)

 9999  CONTINUE
       RETURN
       END
