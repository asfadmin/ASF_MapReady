      SUBROUTINE DVECTP (VA,VB,VP)
C/*   SUBROUTINE DVECTP (VA,VB,VP) ----------------------------
C
C     COMPUTES THE VECTOR PRODUCT (VP) OF VA,VB (IN THIS ORDER)
C     DOES NOT DISTURB VA,VB 
C*/
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION VA(3),VB(3),VP(3)
      character*80 sccsid
      data sccsid /'@(#)dvectp.f	1.3 96/04/09 22:51:46\0'/
      VP(1)=VA(2)*VB(3)-VA(3)*VB(2) 
      VP(2)=VA(3)*VB(1)-VA(1)*VB(3) 
      VP(3)=VA(1)*VB(2)-VA(2)*VB(1) 
      RETURN
      END
