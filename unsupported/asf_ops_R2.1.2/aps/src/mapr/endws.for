C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	endws.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================

C-----------------------------------------------------------------------
C
C SUBROUTINE ENDWS
C
C PURPOSE
C	TERMINATES GKS GRAPHICS BY CLOSING AND DEACTIVATING THE WORKSTATION.
C
C $Logfile:   ACS003:[BLD.MPS.MAPR.SRC]ENDWS.FOV  $
C
C INPUT
C	WSID		WORKSTATION ID NUMBER
C	WISS		WORKSTATION INDEPENDENT SEGMENT STORAGE ID NUMBER
C
C ORIGINAL AUTHOR: RICHARD P. LEE     5-25-88
C MODIFIED FOR ASF BY CRAIG K. FUJIMOTO  8-01-88
C
C $Date$ $Revision$ $Author$
C 7/11/94  Nadia Adhami -port to UNIX/C 
C
C-----------------------------------------------------------------------

       SUBROUTINE ENDWS (WSID,WISS)

      character*100 SccsFileID
     -/'@(#)endws.for	5.1 98/01/08 APS/ASF\0'/

       IMPLICIT NONE

       INTEGER WSID,WISS
C-----------------------------------------------------------------------

C SET COLOR REPRESENTATIONS TO BLACK TO MASK QUIT
                            
             CALL GSCR (WSID,0,0.0,0.0,0.0)
             CALL GSCR (WSID,1,0.0,0.0,0.0)
             CALL GSCR (WSID,2,0.0,0.0,0.0)
             CALL GSCR (WSID,3,0.0,0.0,0.0)
             CALL GSCR (WSID,4,0.0,0.0,0.0)
             CALL GSCR (WSID,5,0.0,0.0,0.0)
             CALL GSCR (WSID,6,0.0,0.0,0.0)
             CALL GSCR (WSID,7,0.0,0.0,0.0)

C   DEACTIVATE AND CLOSE WORKSTATIONS
             CALL GDAWK (WISS)
             CALL GCLWK (WISS)
             CALL GDAWK (WSID)
             CALL GCLWK (WSID)

 9999  CONTINUE
       RETURN
       END
