C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	calcobs.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================

C-----------------------------------------------------------------
C SUBROUTINE CALCOBS
C
C PURPOSE
C	CALC THE WINDOW MIN/MAX LON ACCORDING TO THE CURRENT CENTER LON
C
C $Logfile:   ACS003:[BLD.MPS.MAPR.SRC]CALCOBS.FOV  $
C
C INPUT
C	OBSLON		CENTER LON
C	MINLAT,MAXLAT,
C	MINLON,MAXLON	MIN/MAX LATS/LONS OF WINDOW
C
C OUTPUT
C	OBSMIN,OBSMAX	MIN/MAX LON
C
C QRIGINALLY WRITTEN BY RICHARD P. LEE
C MODIFIED FOR ASF BY CRAIG K. FUJIMOTO - SEP 89
C
C MODIFICATIONS
C $Date$ $Revision$ $Author$
C-----------------------------------------------------------------
       SUBROUTINE CALCOBS(OBSLON,
     1                    MINLAT,MAXLAT,MINLON,MAXLON,
     1                    OBSMIN,OBSMAX)

      character*100 SccsFileID
     -/'@(#)calcobs.for	5.1 98/01/08 APS/ASF\0'/

       IMPLICIT NONE

       REAL OBSLON
       REAL OBSMIN,OBSMAX
       REAL MINLAT,MAXLAT,MINLON,MAXLON

C  SET THE MINIMUM AND MAXIMUM WINDOW LONGITUDE

       IF (OBSLON .LT. 0.0) THEN
         OBSMIN = OBSLON + 180.01
         OBSMAX = OBSLON + 179.99
       ELSE IF (OBSLON .GT. 0.0) THEN
         OBSMIN = OBSLON - 179.99
         OBSMAX = OBSLON - 180.01
       ELSE
         OBSMIN = OBSLON - 179.99
         OBSMAX = OBSLON + 179.99
       END IF

 9999 CONTINUE             
      RETURN
      END    

