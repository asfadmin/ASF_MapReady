C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	angles.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================
      SUBROUTINE ANGLES(M,A,B,CN,SN,TN) 

      character*100 SccsFileID
     -/'@(#)angles.for	5.1 98/01/08 APS/ASF\0'/

C  VERSION OF 4/2/87
C  PURPOSE   
C    COMPUTES COS(M*A), SIN(M*A), M*TAN(B) AND STORE VALUES IN 
C    ARRAYS CN, SN, TN 
C  INPUT 
C    M      = ORDER OF SPHERICAL HARMONICS TO BE EVALUATED 
C    A      = LONGITUDE FROM GREENWICH (RAD)
C    B      = LATITUDE (RAD)
C  OUTPUT
C    CN     = M-D ARRAY OF COS VALUES  
C    SN     = M-D ARRAY OF SIN VALUES  
C    TN     = M-D ARRAY OF TAN VALUES  
C  CALLED BY SUBROUTINES 
C    DER   
C  CALL SUBROUTINES  
C    NONE  
C  REFERENCES
C    JPL EM 312/87-153, 20 APRIL 1987
C  ANALYSIS  
C    J. H. KWOK - JPL  
C  PROGRAMMER
C    J. H. KWOK - JPL  
C  PROGRAM MODIFICATIONS 
C    NONE  
C  COMMENTS  
C   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)   
      DIMENSION CN(41),SN(41),TN(41)
      DATA ZERO,ONE,TWO/0.D0,1.D0,2.D0/
      CN(1)=ONE
      SN(1)=ZERO
      TN(1)=ZERO
      IF (M.EQ.0) RETURN
      CN(2)=DCOS(A) 
      SN(2)=DSIN(A) 
      TN(2)=DTAN(B) 
      IF (M.EQ.1) RETURN
      DO 100 I=2,M  
      CN(I+1)=TWO*CN(2)*CN(I)-CN(I-1)   
      SN(I+1)=TWO*CN(2)*SN(I)-SN(I-1)   
  100 TN(I+1)=TN(2)+TN(I)   
      RETURN
      END
