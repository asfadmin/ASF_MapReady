C Alaska SAR Processor (ASP) %W% %E% %U%
      SUBROUTINE DSCALAR (N,A,B,S)
C/*   SUBROUTINE DSCALAR (N,A,B,S)   -------------------
C
C     COMPUTES S, THE SCALAR PRODUCT OF THE N-DIMENSIONAL VECTORS A,B
C*/
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(1),B(1)
      S=0.D0
      DO 1 I=1,N
    1 S=S+A(I)*B(I)
      RETURN
      END
