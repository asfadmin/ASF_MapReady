      SUBROUTINE DSCALE1 (B,ALPHA,A,N)
C/*   SUBROUTINE DSCALE1 (B,ALPHA,A,N)  --------------------
C
C     A,B ARE N-DIMENSIONAL VECTORS   
C     ALPHA IS AN INPUT SCALAR
C     THE SUBROUTINE COMPUTES B=ALPHA*A
C*/
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(1),B(1)   
      character*80 sccsid
      data sccsid /'@(#)dscale1.f	1.3 96/04/09 22:51:45\0'/
      DO 3 I=1,N
    3 B(I)=ALPHA*A(I)
      RETURN
      END   
