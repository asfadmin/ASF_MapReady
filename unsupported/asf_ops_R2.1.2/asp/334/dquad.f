C Alaska SAR Processor (ASP) %W% %E% %U%
      SUBROUTINE DQUAD (A,B,C,N,X1R,X1I,X2R,X2I)
C/*   SUBROUTINE DQUAD (A,B,C,N,X1R,X1I,X2R,X2I) -----------
C
C          ***** DOUBLE PRECISION SUBROUTINE *****
C     SOLVES THE QUADRATIC A*X**2+B*X+C=0.D0
C     DERIVED FROM SUBROUTINE KUAD
C     N IS AN OUTPUT FLAG
C
C     N=1  TWO REAL ROOTS
C     N=2  A DOUBLE ROOT
C     N=3  COMPLEX ROOTS
C     N=4  A SINGLE REAL ROOT (A=0.D0)
C     N=5  NO ROOTS (A=B=0.D0  C.NE.0.D0)
C     N=6  ALL X ARE ROOTS (A=B=C=0.D0)
C
C     AVOIDS CANCELLATION ERRORS WHEN N=1
C     X1R.LE.X2R
C     X1I NEGATIVE, X2I POSITIVE
C*/
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      X1I=0.D0
      X2I=0.D0
      IF (A.NE.0.D0) GO TO 8
      IF (B.NE.0.D0) GO TO 11
      IF (C.NE.0.D0) GO TO 10
      N=6
      RETURN
   10 N=5
      RETURN
   11 N=4
      X1R=-C/B
      X2R=X1R
      RETURN
    8 T=.5D0/A
      LA=1
      IF (A.LT.0.D0) LA=2
      D=B*B-4.D0*A*C
      IF (D.GT.0.D0) GO TO 6
      X1R=-B*T
      X2R=X1R
      IF (D.EQ.0.D0) GO TO 5
    4 N=3
      X2I=DABS(DSQRT(-D)*T)
      X1I=-X2I
      RETURN
    5 N=2
    2 RETURN
    6 N=1
      S=DSQRT(D)
      IF (B) 21,20,22
   20 X2R=DSQRT(-C/A)
      X1R=-X2R
      RETURN
   21 SMB=S-B
      X1=(2.D0*C)/SMB
      X2=SMB*T
      GO TO (1,3),LA
   22 SPB=S+B
      X1=-SPB*T
      X2=(-2.D0*C)/SPB
      GO TO (1,3),LA
    1 X1R=X1
      X2R=X2
      RETURN
    3 X1R=X2
      X2R=X1
      RETURN
      END
