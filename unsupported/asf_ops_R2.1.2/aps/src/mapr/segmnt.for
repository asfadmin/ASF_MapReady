C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	segmnt.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================

C-----------------------------------------------------------------
C SEGMNT.FOR
C
C PURPOSE
C	OVERLAY ROUTINES
C
C $Logfile:   ACS003:[BLD.MPS.MAPR.SRC]SEGMNT.FOV  $
C
C SUBROUTINES
C	CRTSEG
C	CLSSEG
C	ALLSEG
C
C ORIGINALLY WRITTEN BY RICHARD P. LEE
C MODIFIED FOR ASF BY CRAIG K. FUJIMOTO - SEP 89
C
C MODIFICATIONS
C $Date$ $Revision$ $Author$
C-----------------------------------------------------------------

C-----------------------------------------------------------------
C SUBROUTINE CRTSEG
C
C PURPOSE
C	INITIALIZE A NEW OVERLAY
C
C INPUT/OUTPUT
C	WSID		WORKSTATION ID
C	NSEG		OVERLAY COUNT
C	SEGNAM		OVERLAY NAME
C	CRSEGN		OVERLAY STATUS ARRAY
C	CRSEGT		OVERLAY NAME ARRAY
C
C WRITTEN BY CRAIG K. FUJIMOTO - SEP 89
C-----------------------------------------------------------------
       SUBROUTINE CRTSEG (WSID,NSEG,SEGNAM,CRSEGN,CRSEGT)

      character*100 SccsFileID
     -/'@(#)segmnt.for	5.1 98/01/08 APS/ASF\0'/

       IMPLICIT NONE
C INPUT
c--port       INCLUDE 'SYS$LIBRARY:GKSDEFS.BND'

       INCLUDE 'GKS_ROOT:include/fgksenum.inc'

       CHARACTER*(*) SEGNAM,CRSEGT(*)
       INTEGER WSID,NSEG,CRSEGN(*)
C-----------------------------------------------------------------------

C INCREMENT NUMBER OF CREATED SEGMENTS AND SEGMENTS CURRENTLY IN THE
C   WORKSTATION

       NSEG = NSEG + 1

C STORE THE NUMBER AND NAME OF THE SEGMENT TO BE CREATED

       CRSEGN(NSEG) = 1

       CRSEGT(NSEG) = SEGNAM

C INITIATE THE CREATION OF THE SEGMENT

       CALL GCRSG (NSEG)

C END SUBROUTINE

 9999  CONTINUE
       RETURN
       END



C-----------------------------------------------------------------------
C SUBROUTINE CLSSEG
C
C PURPOSE
C	CLOSES THE SEGMENT
C
C WRITTEN BY RICHARD P LEE
C
C-----------------------------------------------------------------------
       SUBROUTINE CLSSEG

C INPUT:
c--port       INCLUDE 'SYS$LIBRARY:GKSDEFS.BND'

       INCLUDE 'GKS_ROOT:include/fgksenum.inc'

C-----------------------------------------------------------------------

C CLOSE SEGMENT JUST CREATED
       CALL GCLSG ()

 9999  CONTINUE
       RETURN
       END

C-----------------------------------------------------------------------
C SUBROUTINE ALLSEG
C
C PURPOSE         
C	REMOVES ALL SEGMENTS FROM THE WORKSTATIONS
C
C INPUT/OUTPUT
C	NSEG		NUMBER OF CREATED SEGMENTS
C	CRSEGN		ARRAY OF SEGMENT STATUS
C	CRSEGT		ARRAY OF SEGMENT NAMES
C	CRDARID		ARRAY OF SEGMENT DARIDS
C              
C INTERNAL
C	I		DO LOOP INDEX
C
C WRITTEN BY RICHARD P. LEE  9-14-88
C MODIFIED BY CRAIG K. FUJIMOTO SEP-89
C-----------------------------------------------------------------------
      SUBROUTINE ALLSEG (NSEG,CRSEGN,CRSEGT,CRDARID)

C INPUT:                                         
c--port      INCLUDE 'SYS$LIBRARY:GKSDEFS.BND'

       INCLUDE 'GKS_ROOT:include/fgksenum.inc'

      INTEGER NSEG,CRSEGN(*),CRDARID(*)
      CHARACTER*(*) CRSEGT(*)
C INTERNAL:
      INTEGER I
C-----------------------------------------------------------------------

C REMOVE WORLD AND GRID OVERLAYS FROM WORKSTATIONS
      DO 1000 I = 500,506
        CALL GDSG(I)
 1000 CONTINUE

C REMOVE OVERLAY SEGMENTS FROM THE WORKSTATIONS
      DO 2000 I = 1,NSEG

        IF (CRSEGN(I) .NE. -1) THEN
          CALL GDSG(I)
        END IF

        CRSEGN(I)  = 0
        CRSEGT(I)  = ' '
        CRDARID(I) = 0

 2000 CONTINUE

C RESET THE SEG COUNTER
      NSEG = 0

 9999 CONTINUE
      RETURN
      END
