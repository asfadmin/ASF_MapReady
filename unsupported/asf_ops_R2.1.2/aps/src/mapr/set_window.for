C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	set_window.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================

C-----------------------------------------------------------------------
C SUBROUTINE SET_WINDOW
C
C PURPOSE
C	DISPLAYS THE WINDOW FOR THE CHOSEN PROJECTION.
C
C $Logfile:   QDISK:[BLD.MPS.MAPR.SRC]SET_WINDOW.FOV  $
C
C INPUT
C	WSID  		WORKSTATION ID NUMBER
C	PROJN		PROJECTION TYPE TO DISPLAY MAP
C	WNSIZE		SIZE OF THE WINDOW
C	MINXWN		MINIMUM X-COORD OF THE WINDOW
C	MAXXWN		MAXIMUM X-COORD OF THE WINDOW
C	MINYWN		MINIMUM Y-COORD OF THE WINDOW
C	MAXYWN		MINIMUM Y-COORD OF THE WINDOW
C
C CALLS
C	GCLRWK,GQDSP,GSWKVP,GSWKWN,GUWK
C            
C WRITTEN BY CRAIG K. FUJIMOTO  1-17-90
C     
C MODIFICATIONS
C Date:   18 Jul 1990 17:38:46  Revision:   2.0  Author:   DBMAN  
C $Date$ $Revision$ $Author$
C 1/3/94   Nadia Adhami   increase the scale of the view port in the map window
C 1/3/94   Nadia Adhami   LARGE 0.725 -> 0.900
C-----------------------------------------------------------------------
      SUBROUTINE SET_WINDOW (WSID,WNSIZE,
     1                       MINXWN,MAXXWN,MINYWN,MAXYWN)

      character*100 SccsFileID
     -/'@(#)set_window.for	5.1 98/01/08 APS/ASF\0'/

      IMPLICIT NONE

C INPUT:
c--port      INCLUDE 'SYS$LIBRARY:GKSDEFS.BND'

       INCLUDE 'GKS_ROOT:include/fgksenum.inc'
       INCLUDE 'mapper_port.inc'


      INTEGER WSID
      INTEGER DEVICE
      INTEGER I,J,K,L

      REAL LARGE
      REAL WNSIZE
      REAL MINXWN,MAXXWN,MINYWN,MAXYWN
      REAL DVMAXX,DVMAXY

C-----------------------------------------------------------------------

C CLEAR DISPLAY DEVICE
      CALL GCLRWK (WSID,GALWAY)

C SET THE DEVICE
CCCCC DEVICE = GV2000
      DEVICE = GMOTIF
      LARGE = 0.900

C GET THE MAXIMUM DEVICE SIZE
      CALL GQDSP (DEVICE,I,J,DVMAXX,DVMAXY,K,L)

C SET WORKSTATION VIEWPORT
      CALL GSWKVP (WSID,0.0,WNSIZE * (LARGE * DVMAXX),
     1                  0.0,WNSIZE * (LARGE * DVMAXY))

C SET WORKSTATION WINDOW
      CALL GSWKWN (WSID,MINXWN,MAXXWN,MINYWN,MAXYWN)

C UPDATE THE WORKSTATION
      CALL GUWK (WSID,GPERFO)

 9999 CONTINUE
      RETURN
      END
