C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	text_scale.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================

C-----------------------------------------------------------------------
C SUBROUTINE TEXT_SCALE
C
C PURPOSE
C	SET THE TEXT SCALE FOR DISPLAY AND PLOT
C
C $Logfile:   ACS003:[BLD.MPS.MAPR.SRC]TEXT_SCALE.FOV  $
C
C INPUT
C	PROJN		PROJECTION NUMBER
C	PLOT		0 = NOT FOR PLOT/ 1 = FOR PLOT
C	MINX,MAXX,
C	MINY,MAXY	MIN/MAX X/Y COORDS OF ZOOM WINDOW
C OUTPUT
C	TXTSCL		TEXT SCALE
C INTERNAL
C	DIST		DISTANCE BETWEEN BORDERS - LARGEST
C
C ORIGINALLY CODED BY RICHARD P. LEE     FEB 90
C MODIFIED FOR ASF BY CRAIG K. FUJIMOTO  FEB 90
C                         
C MODIFICATIONS
C $Date$ $Revision$ $Author$
C 10/24/94 Nadia Adhami add PLOT = 1 at all times since it computes the
C          right text scale
C 01/24/95 Nadia Adhami DELETED THE ABOVE
C-----------------------------------------------------------------------
      SUBROUTINE TEXT_SCALE(PROJN,PLOT,
     1                MINX,MAXX,MINY,MAXY,
     2                TXTSCL)

      character*100 SccsFileID
     -/'@(#)text_scale.for	5.1 98/01/08 APS/ASF\0'/

      IMPLICIT NONE

      INCLUDE 'mapper_port.inc'

      INTEGER PROJN,PLOT
      REAL MINX,MAXX,MINY,MAXY

      REAL TXTSCL
      REAL DIST
      REAL GET_DEF_CH


      PLOT = 1

      IF (PLOT .EQ. 0) THEN
        TXTSCL = GET_DEF_CH()
        GO TO 9999
      END IF

      IF ((MAXX-MINX) .GT. (MAXY-MINY)) THEN
        DIST = MAXX - MINX
      ELSE
        DIST = MAXY - MINY
      END IF


      IF (DIST .GE. 0.71) THEN
        TXTSCL = 0.01
      ELSE IF (DIST .GE. 0.67) THEN
        TXTSCL = 0.009 
      ELSE IF (DIST .GE. 0.57) THEN
        TXTSCL = 0.008
      ELSE IF (DIST .GE. 0.50) THEN
        TXTSCL = 0.007
      ELSE IF (DIST .GE. 0.45) THEN
        TXTSCL = 0.006
      ELSE IF (DIST .GE. 0.40) THEN
        TXTSCL = 0.005
      ELSE IF (DIST .GE. 0.34) THEN
        TXTSCL = 0.0045
      ELSE IF (DIST .GE. 0.30) THEN
        TXTSCL = 0.004
      ELSE IF (DIST .GE. 0.26) THEN
        TXTSCL = 0.0035
      ELSE IF (DIST .GE. 0.22) THEN
        TXTSCL = 0.003
      ELSE IF (DIST .GE. 0.18) THEN
        TXTSCL = 0.0025
      ELSE IF (DIST .GE. 0.14) THEN
        TXTSCL = 0.002
      ELSE IF (DIST .GE. 0.12) THEN
        TXTSCL = 0.0015
      ELSE IF (DIST .GE. 0.10) THEN
        TXTSCL = 0.00125
      ELSE IF (DIST .GE. 0.075) THEN
        TXTSCL = 0.001
      ELSE IF (DIST .GE. 0.05) THEN
        TXTSCL = 0.00075
      ELSE IF (DIST .GE. 0.035) THEN
        TXTSCL = 0.0005
      ELSE IF (DIST .GE. 0.03) THEN
        TXTSCL = 0.0004
      ELSE IF (DIST .GE. 0.02) THEN
        TXTSCL = 0.0003
      ELSE
        TXTSCL = 0.0002
      END IF

      IF (TXTSCL .NE. 0.01) THEN
        TXTSCL = TXTSCL + (TXTSCL / 3)
      ENDIF

 9999 CONTINUE

      IF (TESTMODE .GT. 0) THEN
        WRITE(6, 2) TXTSCL
    2   FORMAT(' TEXT_SCALE() = ',F)
      ENDIF

      RETURN
      END
