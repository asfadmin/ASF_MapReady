C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	coord.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================
      SUBROUTINE COORD(X,GM,Y)

      character*100 SccsFileID
     -/'@(#)coord.for	5.1 98/01/08 APS/ASF\0'/

C  VERSION OF 4/1/85
C  PURPOSE
C    TRANSFORM ORBITAL ELEMENT A,E,I,CAPW,W,EA TO CARTESIAN COORD
C    X,Y,Z,XD,ZD,YD.
C  INPUT
C    X(1)   = A, SEMI-MAJOR AXIS
C     (2)   = E, ECCENTRICITY
C     (3)   = I, INCLINATION (RAD)
C     (4)   = CAPW, LONGITUDE OF ASCENDING NODE (RAD)
C     (5)   = W, ARGUMENT OF PERIAPSIS (RAD)
C     (6)   = EA, ECCENTRIC ANOMALY (RAD)
C    GM     = GRAVITATIONAL CONSTANT * MASS OF PLANET (KM**3/SEC**2)
C  OUTPUT
C    Y(1)   = X (KM)
C     (2)   = Y (KM)
C     (3)   = Z (KM)
C     (4)   = XD (KM/SEC)
C     (5)   = YD (KM/SEC)
C     (6)   = ZD (KM/SEC)
C  CALL SUBROUTINES
C    NONE
C  REFERENCES
C    JPL EM 312/87-153, 20 APRIL 1987
C    ORBITAL MOTION, A. E. ROY, 1978, PAGE 102
C  ANALYSIS
C    J. H. KWOK - JPL
C  PROGRAMMER
C    J. H. KWOK - JPL
C  PROGRAM MODIFICATIONS
C    NONE
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION X(6),Y(6),TD(8)
      DATA ONE/1.D0/
      CC=DCOS(X(4))
      SC=DSIN(X(4))
      CW=DCOS(X(5))
      SW=DSIN(X(5))
      CI=DCOS(X(3))
      SI=DSIN(X(3))
      TD(1)=CC*CW-SC*SW*CI
      TD(2)=SC*CW+CC*SW*CI
      TD(3)=SW*SI
      TD(4)=-CC*SW-SC*CW*CI
      TD(5)=-SC*SW+CC*CW*CI
      TD(6)=CW*SI
      TD(7)=DSQRT(ONE-X(2)**2)*X(1)
      TD(8)=DSQRT(GM/X(1)**3)
      CE=DCOS(X(6))
      SE=DSIN(X(6))
      R=X(1)*(ONE-X(2)*CE)
      DO 10 I=1,3
      Y(I)=X(1)*TD(I)*(CE-X(2))+TD(7)*TD(I+3)*SE
   10 Y(I+3)=X(1)*TD(8)*(TD(7)*CE*TD(I+3)-X(1)*SE*TD(I))/R
      RETURN
      END
