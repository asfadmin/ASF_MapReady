      SUBROUTINE MAX5 (P,I1,I2,PMAX)
C/*   SUBROUTINE MAX5 (P,I1,I2,PMAX) -----------------
C
C     COMPUTES PMAX WHICH IS THE MAXIMAL VALUE OF P(I) FOR I=I1,I2
C     I2.GT.I1
C     FIRST INDEX OF ARRAY P IS 0.
C*/
      REAL P(0:0)
      character*80 sccsid
      data sccsid /'@(#)max5.f	1.3 96/04/09 22:51:53\0'/
      PMAX=P(I1)
      DO 1 I=I1+1,I2
    1 PMAX=AMAX1(PMAX,P(I))
      RETURN
      END  
