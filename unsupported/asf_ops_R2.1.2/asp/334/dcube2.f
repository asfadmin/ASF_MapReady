C Alaska SAR Processor (ASP) %W% %E% %U%
      SUBROUTINE DCUBE2(*,COEFF,ZR,ZI)
C/*   SUBROUTINE DCUBE2(*,COEFF,ZR,ZI) --------------------
C
C           ***** DOUBLE PRECISION SUBROUTINE *****   
C     SOLVES A CUBIC EQUATION.
C     COEFFICIENTS IN COEFF (COEFF(1)=COEFFICIENT OF X**3)
C     REAL PARTS OF ROOTS IN ZR
C     IMAGINARY PARTS OF ROOTS IN ZI
C     RETURN1   DEGREE.LT.3 
C*/
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION XR(3),ZI(3),COEFF(4),ZR(3)   
      DATA PI/3.141592653589793238462643/
      PI3=PI/3.D0 
      IF (COEFF(1).EQ.0.D0) RETURN1   
      A2=COEFF(2)/COEFF(1)  
      A1=COEFF(3)/COEFF(1)  
      A0=COEFF(4)/COEFF(1)  
      A23=A2/3.D0 
      A23S=A23**2   
      P=A23S-A1/3.D0  
      R=(A1*A2-3.D0*A0)/6.D0-A23*A23S   
      IF (P.EQ.0.D0) GO TO 4  
      SQP=DSQRT(DABS(P))  
      SQP3=DSQRT(3.D0*DABS(P))  
      SQPSN=SQP 
      IF (R.LT.0.D0) SQPSN=-SQP   
      D=R/(P*SQP)   
      DMAG=DABS(D)   
      IF (P) 3,4,5  
    5 IF (DMAG.LE.1.D0) GO TO 1   
    2 NC=2  
C     NC IS THE CASE NUMBER 
      BETA=DLOG(DMAG+DSQRT(D**2-1.D0))
      BETA3=BETA/3.D0 
      XR(2)=  -SQPSN*DCOSH(BETA3)
      XR(1)=-2.D0*XR(2)   
      ZI(2)=SQP3*DSINH(BETA3)
      GO TO 7   
    1 NC=1  
      ALPHA=DACOS(D) 
      ALPHA3=ALPHA/3.D0   
      XR(1)=2.D0*SQP*DCOS(ALPHA3)  
      XR(2)=-2.D0*SQP*DCOS(PI3+ALPHA3) 
      XR(3)=-2.D0*SQP*DCOS(PI3-ALPHA3) 
    6 ZI(2)=0.D0  
      GO TO 7   
    4 NC=2  
      XR(1)=DSIGN((2.D0*DABS(R))**(1.D0/3.D0),R)
      XR(2)=-.5D0*XR(1)   
      ZI(2)=XR(2)*DSQRT(3.D0)  
      GO TO 7   
    3 NC=3  
      BETA=DLOG(DMAG+DSQRT(D**2+1.D0))
      BETA3=DSIGN(BETA,D)/3.D0
      XR(2)=SQP*DSINH(BETA3) 
      XR(1)=-2.D0*XR(2)   
      ZI(2)=SQP3*DCOSH(BETA3)
    7 ZI(1)=0.D0  
      DO 10 I=1,2   
   10 ZR(I)=XR(I)-A23   
      GO TO (9,8,8) ,NC 
    8 ZR(3)=ZR(2)
      ZI(3)=-ZI(2)
      RETURN
    9 ZR(3)=XR(3)-A23   
      ZI(3)=0.D0
      RETURN
      END   
