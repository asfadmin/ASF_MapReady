      SUBROUTINE DROTV(N,PHI,C,S,RHO,V1,V2)
C/*   SUBROUTINE DROTV(N,PHI,C,S,RHO,V1,V2) -----------------
C
C           ***** DOUBLE PRECISION SUBROUTINE *****             
C     COMPUTES THE VECTOR V2 WHICH IS THE RESULT OF ROTATING THE   
C     VECTOR V1 THROUGH AN ANGLE PHI ABOUT THE UNIT VECTOR RHO
C     N=1 PHI IS INPUT IN DEGREES                             
C     N=2 PHI IS INPUT IN RADIANS                             
C     N=3 C=DCOS(PHI), S=DSIN(PHI) ARE INPUT
C*/
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION V1(3),V2(3),RHO(3)                           
      DATA RADPDG/1.74532925199432957692D-2/
      character*80 sccsid
      data sccsid /'@(#)drotv.f	1.3 96/04/09 22:51:45\0'/
      PSI=PHI
      GO TO (1,2,13),N                                       
    1 PSI=RADPDG*PHI                                            
    2 C=DCOS(PSI)                                            
      S=DSIN(PSI)                                            
   13 A=1.D0-C                                                 
      DO 11 I=1,3                                            
      VA=0.D0                                                  
      VS=0.D0                                                  
      DO 12 J=1,3                                            
      VA=VA+RHO(I)*RHO(J)*V1(J)                        
      DO 12 K=1,3                                            
   12 VS=VS-DEPSLN(I,J,K)*RHO(K)*V1(J)                 
   11 V2(I)=V1(I)*C+VS*S+VA*A                                
      RETURN                                                 
      END                                                    
