      SUBROUTINE DROTM (N,PHI,C,S,R,RHO) 
C/*   SUBROUTINE DROTM (N,PHI,C,S,R,RHO)   -------------------
C
C             ****** DOUBLE PRECISION SUBROUTINE ******                 
C     GENERATES THE ROTATION MATRIX R CORRESPONDING TO A 
C     ROTATION PHI ABOUT THE UNIT VECTOR RHO. THAT IS, IF B 
C     IS THE RESULT OF ROTATING A, THEN B=R*A                          
C     N=1 PHI IS INPUT IN DEGREES                                 
C     N=2 PHI IS INPUT IN RADIANS                                 
C     N=3 C=COS(PHI), S=SIN(PHI) ARE INPUT
C*/
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                        
      DIMENSION R(3,3),RHO(3)         
      DATA RADPDG/1.74532925199432957692D-2/
      character*80 sccsid
      data sccsid /'@(#)drotm.f	1.3 96/04/09 22:51:45\0'/
      PSI=PHI
      GO TO (1,2,13),N
    1 PSI=RADPDG*PHI
    2 C=DCOS(PSI)                                      
      S=DSIN(PSI)                                                 
   13 A=1.D0-C                                                      
      DO 3 I=1,3                                                  
      DO 3 J=1,3                                                  
      R(I,J)=RHO(I)*RHO(J)*A                                      
      IF (I-J) 5,4,5                                              
    4 R(I,J)=R(I,J)+C                                             
    5 DO 3 K=1,3                                                  
    3 R(I,J)=R(I,J)-DEPSLN(I,J,K)*RHO(K)*S                        
      RETURN                                                      
      END                                                         
