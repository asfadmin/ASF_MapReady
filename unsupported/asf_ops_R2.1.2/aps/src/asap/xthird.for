C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	xthird.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================
      SUBROUTINE XTHIRD(T,TR,ES,ET,XS)   

      character*100 SccsFileID
     -/'@(#)xthird.for	5.1 98/01/08 APS/ASF\0'/

C  VERSION OF 4/1/85
C  PURPOSE   
C    COMPUTES POSITION OF PERTURBING THIRD BODY
C  INPUT 
C    T      = CURRENT TIME (SEC)
C    TR     = REFERENCE TIME FOR INITIAL SETTING OF THE
C             GREENWICH HOUR ANGLE, AND THE MEAN ANOMALIES OF
C             THE ANALYTICAL LUNI-SOLAR EPHEMERIDES (SEC)
C    ES     = 7-D ORBITAL ELEMENT SET OF THIRD BODY
C      (1)  = SEMI-MAJOR AXIS (KM)
C      (2)  = ECCENTRICITY OF THE THIRD BODY
C      (3)  = INCLINATION OF THE THIRD BODY RELATIVE TO THE PLANET (RAD)
C      (4)  = LONGITUDE OF ASCENDING NODE OF THE THIRD BODY (RAD)
C      (5)  = AGRUMENT OF PERIAPSIS OF THE THIRD BODY (RAD)
C      (6)  = MEAN ANOMALY OF THE THIRD BODY AT TR (RAD)
C      (7)  = MEAN MOTION (RAD/SEC)
C    ET     = 8-D ELEMENTS TO TRANFORM ORBITAL ELEMENTS TO CART.
C             COORD, THIS IS SET UP BY SETTHD ROUTINE
C      (1)  = L1, COS(NODE)*COS(W) - SIN(NODE)*SIN(W)*COS(I)  
C      (2)  = M1, SIN(NODE)*COS(W) + COS(NODE)*SIN(W)*COS(I)  
C      (3)  = N1, SIN(W)*SIN(I)   
C      (4)  = L2, -COS(NODE)*SIN(W) - SIN(NODE)*COS(W)*COS(I) 
C      (5)  = M2, -SIN(NODE)*SIN(W) + COS(NODE)*COS(W)*COS(I) 
C      (6)  = N2, COS(W)*SIN(I)   
C      (7)  = SEMI-MINOR AXIS, A*SQRT(1-E*E)  
C  OUTPUT
C    XS     = 3-D CARTESIAN STATE OF 3RD BODY (KM)
C  CALLED BY SUBROUTINES 
C    DER   
C  CALL SUBROUTINES  
C    KEPLER
C  REFERENCES
C    JPL EM 312/87-153, 20 APRIL 1987
C    ORBITAL MOTION, A. E. ROY, 1978, P.102
C  ANALYSIS
C    J. H. KWOK - JPL  
C  PROGRAMMER
C    J. H. KWOK - JPL  
C  PROGRAM MODIFICATION  
C    NONE  
C  COMMENTS  
C    NONE  
C   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)   
      DIMENSION ES(7),ET(7),XS(3)   
      DATA TPI/6.283185307179586D0/   
      AM=DMOD(ES(6)+ES(7)*(T-TR),TPI)   
      CALL KEPLER(AM,ES(2),EA,SE,CE)
      DO 10 I=1,3   
   10 XS(I)=ES(1)*ET(I)*(CE-ES(2))+ET(7)*ET(I+3)*SE 
      RETURN
      END
