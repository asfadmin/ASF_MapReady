C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	viewport.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================

C-----------------------------------------------------------------
C SUBROUTINE VIEWPORT
C
C PURPOSE
C	DEFINE THE VIEWPORT
C
C $LOGFILE$
C
C INPUT
C	WSID		WORKSTATION ID
C	WNSIZE		WINDOW SIZE (%)
C	DVMAXX,DVMAXY	DEVICE MAX X/Y
C
C INTERNAL
C	I,J,K,L		TEMPS
C	DEVICE		DEVICE IDENTIFIER
C	DVMAXX,DVMAXY	DEVICE MAX X/Y
C	LARGE		SCALE FACTOR
C
C WRITTEN BY CRAIG K. FUJIMOTO
C
C MODIFICATIONS
C $DATE:$ $REVISION:$ $AUTHOR:$
C-----------------------------------------------------------------
      SUBROUTINE VIEWPORT(WSID,PLOT,WNSIZE)

      character*100 SccsFileID
     -/'@(#)viewport.for	5.1 98/01/08 APS/ASF\0'/

      IMPLICIT NONE

C INPUT
c--port      INCLUDE 'SYS$LIBRARY:GKSDEFS.BND'

       INCLUDE 'GKS_ROOT:include/fgksenum.inc'
       INCLUDE 'mapper_port.inc'


      INTEGER WSID,PLOT
      REAL WNSIZE

C INTERNAL
      INTEGER I,J,K,L
      INTEGER DEVICE

      REAL DVMAXX,DVMAXY
      REAL LARGE

C DETERMINE THE DEVICE
      IF (PLOT .EQ. 1) THEN
        DEVICE = GHP755
        LARGE = 1.0
      ELSE 
CCCCCCC DEVICE = GV2000
        DEVICE = GMOTIF
        LARGE = 0.725
      END IF

C GET THE MAXIMUM DEVICE SIZE
      CALL GQDSP (DEVICE,I,J,DVMAXX,DVMAXY,K,L)

C SET WORKSTATION VIEWPORT
      CALL GSWKVP (WSID,0.0,WNSIZE * (LARGE * DVMAXX),
     1                  0.0,WNSIZE * (LARGE * DVMAXY))

 9999 CONTINUE
      RETURN
      END
