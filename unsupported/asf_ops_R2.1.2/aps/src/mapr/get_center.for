C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	get_center.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================

C-----------------------------------------------------------------
C SUBROUTINE GET_CENTER
C
C PURPOSE
C	GET THE CENTER LAT/LON FROM THE USER
C
C $Logfile:   ACS003:[BLD.MPS.MAPR.SRC]GET_CENTER.FOV  $
C
C INPUT
C	PROJN		PROJECTION NO.
C OUTPUT
C	OBSLAT,OBSLON	CENTER LAT/LON
C INTERNAL
C	IOS		I/O STATUS
C	I		LOOP INDEX
C
C SUBROUTINES CALLED
C
C QRIGINALLY WRITTEN BY RICHARD P. LEE
C MODIFIED FOR ASF BY CRAIG K. FUJIMOTO - SEP 89
C
C MODIFICATIONS
C $Date$ $Revision$ $Author$
C-----------------------------------------------------------------
       SUBROUTINE GET_CENTER (PROJN,OBSLAT,OBSLON)

      character*100 SccsFileID
     -/'@(#)get_center.for	5.1 98/01/08 APS/ASF\0'/

       IMPLICIT NONE

C INPUT:
       INTEGER PROJN                              

C OUTPUT:
       REAL OBSLAT,OBSLON

C INTERNAL:

C-----------------------------------------------------------------------

C SATELLITE VIEW
      IF (PROJN .EQ. 1) THEN

C CENTER LATITUDE
        CALL ASK_LATLON(OBSLAT,'Center latitude',-90.0,90.0)

C CENTER LONGITUDE
        CALL ASK_LATLON(OBSLON,'Center longitude',-180.0,180.0)

C POLAR STEREOGRAPHIC PROJECTIONS
      ELSE IF (PROJN .EQ. 5 .OR. PROJN .EQ. 6) THEN

C CENTER LATITUDE
        IF (PROJN .EQ. 5) THEN
          OBSLAT = 90.0
        ELSE
          OBSLAT = -90.0
        END IF

C CENTER LONGITUDE
        CALL ASK_LATLON(OBSLON,'Longitude reference',-180.0,180.0)

C ALL OTHER PROJECTIONS
       ELSE 

C CENTER LONGITUDE
        CALL ASK_LATLON(OBSLON,'Center longitude',-180.0,180.0)

C CENTER LATITUDE
        OBSLAT = 0.0

      END IF

 9999 CONTINUE
      RETURN
      END
