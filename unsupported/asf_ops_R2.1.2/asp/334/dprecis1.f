C Alaska SAR Processor (ASP) %W% %E% %U%
      SUBROUTINE DPRECIS1 (*,F,D,B,N)
C/*   SUBROUTINE DPRECIS1 (*,F,D,B,N) ------------------
C
C          ***** DOUBLE PRECISION SUBROUTINE *****
C     COMPUTES F=D-DSQRT(D**2+B) (FOR N=1)
C     OR       F=D+DSQRT(D**2+B) (FOR N=2)
C     THE COMPUTATION IS CARRIED OUT IN SUCH A WAY THAT 
C     CANCELLATION ERRORS ARE AVOIDED.
C     RETURN 1   (D**2+B) .LT. 0.D0
C*/
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      S=D**2+B
      IF (S.LT.0.D0) RETURN 1
      S=DABS(D)+DSQRT(S)
      MARK=1
      IF (D.GE.0.D0) MARK=2
      GO TO (1,2),N
    1 GO TO (11,12),MARK
   11 F=-S
      RETURN
   12 F=-B/S
      RETURN
    2 GO TO (21,22),MARK
   21 F=B/S
      RETURN
   22 F=S
      RETURN
      END
