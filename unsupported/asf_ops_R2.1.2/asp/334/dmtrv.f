C Alaska SAR Processor (ASP) %W% %E% %U%
      SUBROUTINE DMTRV (U,F,V,N,M)
C/*   SUBROUTINE DMTRV (U,F,V,N,M) ------------------
C
C          ****** DOUBLE PRECISION SUBROUTINE ******
C     COMPUTES THE COLUMN MATRIX U GIVEN BY U=F*V                     
C     MATRIX ELEMENT X(I,J) REFERS TO ROW I , COLUMN J               
C     N IS THE ORDER OF THE MATRICES                                 
C     M IS THE DIMENSION OF THE ARRAYS IN THE CALLING PROGRAM        
C     N.LE.M                                                         
C*/
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION U(M),F(M,M),V(M)                
      DO 1 I=1,N                                                     
      U(I)=0.D0   
      DO 1 J=1,N                                                     
    1 U(I)=U(I)+F(I,J)*V(J)                                          
      RETURN                                                         
      END                                                            
