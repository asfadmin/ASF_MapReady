      SUBROUTINE DVNR1(V,N,VN,VMAG)  
C/*   SUBROUTINE DVNR1(V,N,VN,VMAG)  --------------------
C
C        ****** DOUBLE PRECISION SUBROUTINE ******
C     NORMALIZES THE VECTOR V OF DIMENSION N
C     VN=NORMALIZED VECTOR  
C     VMAG=MAGNITUDE OF V   
C     VN MAY BE ASSIGNED THE STORAGE OF V   
C*/
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION V(N),VN(N)  
      character*80 sccsid
      data sccsid /'@(#)dvnr1.f	1.3 96/04/09 22:51:47\0'/
      VMAG=0.D0   
      DO 1 I=1,N
    1 VMAG=VMAG+V(I)*V(I)   
      VMAG=DSQRT(VMAG)  
      RVMAG=1.D0/VMAG 
      DO 2 I=1,N
    2 VN(I)=V(I)*RVMAG  
      RETURN
      END   
