      SUBROUTINE DSUMV(C,A,B,N,K)
C/*   SUBROUTINE DSUMV(C,A,B,N,K) --------------------
C
C      ***** DOUBLE PRECISION SUBROUTINE *****
C     A,B,C ARE N-DIMENSIONAL VECTORS   
C     K=1   C=A-B   
C     K=2   C=A+B   
C*/
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(1),B(1),C(1)   
      character*80 sccsid
      data sccsid /'@(#)dsumv.f	1.3 96/04/09 22:51:46\0'/
      GO TO (1,2),K 
    1 DO 3 I=1,N
    3 C(I)=A(I)-B(I)
      RETURN
    2 DO 4 I=1,N
    4 C(I)=A(I)+B(I)
      RETURN
      END   
