C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.

C********************************************************************
C                                                                   
C  Name: MATH
C
C  Language: FORTRAN
C
C  Purpose
C	Miscellaneous FORTRAN Math routines written by Martin Lo.
C
C  $Logfile:   ACS003:[BLD.MPS.LIB.MATH]MATHNEW.FOV  $
C                                                                   
C  Date			Revision	Author
C  $Date$ $Revision$ $Author$
C                                                                   
C*********************************************************************/
      SUBROUTINE DUP_NEW(X,Y)
      character*100 SccsFileID
     -/'@(#)mathnew.for	5.1 98/01/08 APS/ASF\0'/

      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION X(3),Y(3)
      Y(1)=X(1)
      Y(2)=X(2)
      Y(3)=X(3)
      RETURN
      END
C
      SUBROUTINE DUPN(NMAX,X,Y)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION X(1),Y(1)
      DO 100 N=1,NMAX
      Y(N)=X(N)
  100 CONTINUE
      RETURN
      END
C
      FUNCTION RAD(X)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DATA PI/3.141592653589793238D0/
      DATA DTR/.1745329251994330D-1/
      RAD=X*PI/180.D0
      RAD=X*DTR
      RETURN
      END
C
C
C
      FUNCTION DEG(X)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DATA PI/3.141592653589793238D0/
      DATA DTR/.1745329251994330D-1/
      DEG=X*180.D0/PI
      DEG=X/DTR
      RETURN
      END
C
C
C
      FUNCTION DOT(X,Y)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION X(3),Y(3)
      DOT=X(1)*Y(1)+X(2)*Y(2)+X(3)*Y(3)
      RETURN
      END
C
C
C
      FUNCTION DDOT(A,B)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION A(3),B(3)
      DDOT=A(1)*B(1)+A(2)*B(2)+A(3)*B(3)
      RETURN
      END
C
C
C
      FUNCTION DMAG(A)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION A(3)
      DMAG=DDOT(A,A)
      DMAG=DSQRT(DMAG)
      RETURN
      END
C
C
C
      FUNCTION GHA(DJ)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C     REF: WERTZ, P. 803
      T=(DJ-2415020.D0)/36525.D0
      UT=(DJ-IDINT(DJ)+.5D0)*360.D0
      GHA=99.6910D0+36000.7689D0*T+.0004D0*T*T+UT
      GHA=DMOD(GHA,360.D0)
      GHA=RAD(GHA)
      RETURN
      END
C -----
      FUNCTION TRU2T(GMU,SM,EC,TR)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
C  GIVEN TRUE ANOMALY, TRD, UNRESTRICTED, FIND TIME FROM PERIAPSIS.
C  TIME IS BETWEEN +/- PERIOD/2.
C  THIS VERSION WORKS FOR ELLIPTICAL ORBITS ONLY.
C
      DATA PI    /3.141592653589793238D0/
     +,    PI2   /6.283185307179586476D0/
C
C  I.  ELLIPTICAL ORBIT
C
      IF(EC.LT.1.D0) THEN
         TR0=DMOD(TR,PI2)
         EE=2.D0*DATAN(DTAN(TR0/2.D0)*DSQRT((1.D0-EC)/(1.D0+EC)))
         TRU2T=DSQRT(SM*SM*SM/GMU)*(EE-EC*DSIN(EE))
      END IF
C
C  II. OTHER ORBITS
C
      IF(EC.GE.1.D0) THEN
         PRINT*,' EC: ',EC
         PRINT*,' STOPPED IN TRU2T.'
         STOP
      END IF
C
      RETURN
      END
C -----
      FUNCTION T2TRU(GMU,SM,EC,TIME)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
C  GIVEN TIME, UNRESTRICTED, FIND TRUE ANOMALY BETWEEN +/- PI.
C
C  INPUT
C     GMU   : EARTH GRAVITY
C     SM    : SEMIMAJOR AXIS (KM)
C     EC    : ECCENTRICITY
C     TIME  : TIME FROM PERIAPSIS (SEC)
C
C  OUTPUT
C     T2TRU : TRUE ANOMALY (RAD)
C
      DATA PI  /3.141592653589793238D0/
      DATA PI2 /6.283185307179586476D0/
C
C  I.  ELLIPTICAL ORBIT
C
      IF(EC.LT.1.D0) THEN
         XMOTION=DSQRT(GMU/(SM*SM*SM))
         PERIOD=PI2/XMOTION
         T0=DMOD(TIME,PERIOD)
         TN=T0*XMOTION
         E0=TN
         DO 100 N=1,100
           E1=EC*DSIN(E0)+TN
           IF(DABS(E1-E0).LT.1.D-12) GOTO 101
           E0=E1    
  100    CONTINUE
  101    CONTINUE
C         WRITE(*,1000) N,E0,E1
C 1000    FORMAT(' **************************************************'/
C     +          ' * FCN T2TRU CONVERGED IN ',I5,' ITERATIONS.'/
C     +          ' * E0, E1: ',2F14.8/
C     +          ' **************************************************'/)
         T2TRU=2.D0*DATAN(DTAN(E1/2.D0)*DSQRT((1+EC)/(1-EC)))
      END IF
C
C  II. OTHER ORBITS
C
      IF(EC.GE.1.D0) THEN
         PRINT*,' EC: ',EC
         PRINT*,' STOPPED IN T2TRU.'
         STOP
      END IF
C
      RETURN
      END
C -----
      SUBROUTINE QUAD(A,B,C,X1,X2,IMAG)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DISC=B*B-4.D0*A*C
      IF(DABS(DISC).LT.1.D-15) THEN
           IMAG=1
           X1=-B/A/2.D0
           X2=DSQRT(-DISC)/A/2.D0
      ELSE
           IMAG=0
           DISCRT=DSQRT(DISC)
           X1=(-B+DISCRT)/A/2.D0
           X2=(-B-DISCRT)/A/2.D0
      END IF
      RETURN
      END
C -----
      SUBROUTINE GSHIFT(REQ,ROT,XIN,ARG,TRU,GSH,PHI)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
C   COMPUTE GROUND TRACK SHIFT DUE TO PLANET ROTATION ONLY
C   ROT=PLANET ROTATION RAD/ORBIT
C   GSH=SHIFT IN GROUND TRACK ALONG LATITUDE
C   PHI=LATITUDE IN RAD 
C
      PHI=DASIN(DSIN(XIN)*DSIN(ARG+TRU))     
      GSH=REQ*ROT*DCOS(PHI)
      RETURN
      END
C -----
      SUBROUTINE ROOT(T,FT,B,C,RELERR,ABSERR,IFLAG)                            1
C                                                                              2
C  ROOT COMPUTES A ROOT OF THE NONLINEAR EQUATION F(X)=0                       3
C  WHERE F(X) IS A CONTINUOUS REAL FUNCTION OF A SINGLE REAL                   4
C  VARIABLE X.  THE METHOD USED IS A COMBINATION OF BISECTION                  5
C  AND THE SECAND RULE.                                                        6
C                                                                              7
C  NORMAL INPUT CONSISTS OF A CONTINUOUS FUNCTION F AND AN                     8
C  INTERVAL (B,C) SUCH THAT F(B)*F(C).LE.0.0.  EACH ITERATION                  9
C  FINDS NEW VALUES OF B AND C SUCH THAT THE INTERVAL (B,C) IS                10
C  SHRUNK AND F(B)*F(C).LE.0.0.  THE STOPPING CRITERION IS                    11
C                                                                             12
C          ABS(B-C).LE.2.0*(RELERR*ABS(B)+ABSERR)                             13
C                                                                             14
C  WHERE RELERR=RELATIVE ERROR AND ABSERR=ABSOLUTE ERROR ARE                  15
C  INPUT QUANTITIES.  SET THE FLAG, IFLAG, POSITIVE TO INITIALIZE             16
C  THE COMPUTATION.  AS B,C AND IFLAG ARE USED FOR BOTH INPUT AND             17
C  OUTPUT, THEY MUST BE VARIABLES IN THE CALLING PROGRAM.                     18
C                                                                             19
C  IF 0 IS A POSSIBLE ROOT, ONE SHOULD NOT CHOOSE ABSERR=0.0.                 20
C                                                                             21
C  THE OUTPUT VALUE OF B IS THE BETTER APPROXIMATION TO A ROOT                22
C  AS B AND C ARE ALWAYS REDEFINED SO THAT ABS(F(B)).LE.ABS(F(C)).            23
C                                                                             24
C  TO SOLVE THE EQUATION, ROOT MUST EVALUATE F(X) REPEATEDLY. THIS            25
C  IS DONE IN THE CALLING PROGRAM.  WHEN AN EVALUATION OF F IS                26
C  NEEDED AT T, ROOT RETURNS WITH IFLAG NEGATIVE.  EVALUATE FT=F(T)           27
C  AND CALL ROOT AGAIN.  DO NOT ALTER IFALG.                                  28
C                                                                             29
C  WHEN THE COMPUTATION IS COMPLETE, ROOT RETURNS TO THE CALLING              30
C  PROGRAM WITH IFLAG POSITIVE:                                               31
C                                                                             32
C     IFLAG=1  IF F(B)*F(C).LT.0 AND THE STOPPING CRITERION IS MET.           33
C                                                                             34
C          =2  IF A VALUE B IS FOUND SUCH THAT THE COMPUTED VALUE             35
C              F(B) IS EXACTLY ZERO.  THE INTERVAL (B,C) MAY NOT              36
C              SATISFY THE STOPPING CRITERION.                                37
C                                                                             38
C          =3  IF ABS(F(B)) EXCEEDS THE INPUT VALUES ABS(F(B)),               39
C              ABS(F(C)).   IN THIS CASE IT IS LIKELY THAT B IS CLOSE         40
C              TO A POLE OF F.                                                41
C                                                                             42
C          =4  IF NO ODD ORDER ROOT WAS FOUND IN THE INTERVAL.  A             43
C              LOCAL MINIMUM MAY HAVE BEEN OBTAINED.                          44
C                                                                             45
C          =5  IF TOO MANY FUNCTION EVALUATIONS WERE MADE.                    46
C              (AS PROGRAMMED, 500 ARE ALLOWED.)                              47
C                                                                             48
C  THIS CODE IS A MODIFICATION OF THE CODE  ZEROIN  WHICH IS COMPLETELY       49
C  EXPLAINED AND DOCUMENTED IN THE TEST,  NUMERICAL COMPUTING:  AN            50
C  INTRODUCTION  BY L. F. SHAMPINE AND R. C. ALLEN.                           51
C                                                                             52
C***********************************************************************      53
C*  THE ONLY MACHINE DEPENDENT CONSTANT IS BASED ON THE MACHINE UNIT   *      54
C*  ROUNDOFF ERROR  U  WHICH IS THE SMALLEST POSITIVE NUMBER SUCH THAT *      55
C*  1.0+U .GT. 1.0 .  U  MUST BE CALCULATED AND INSERTED IN THE        *      56
C*  FOLLOWING DATA STATEMENT BEFORE USING  ROOT .  THE ROUTINE MACHIN  *      57
C*  CALCULATES  U .                                                    *      58
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                    58A
      DATA U /2.2D-16/                                                        59
C***********************************************************************      60
C                                                                             61
      IF (IFLAG.GE.0) GO TO 100                                               62
      IFLAG=IABS(IFLAG)                                                       63
      GO TO (200,300,400), IFLAG                                              64
  100 RE=DMAX1(RELERR,U)                                                      65
      AE=DMAX1(ABSERR,0.0D0)                                                  66
      IC=0                                                                    67
      ACBS=DABS(B-C)                                                          68
      A=C                                                                     69
      T=A                                                                     70
      IFLAG=-1                                                                71
      RETURN                                                                  72
  200 FA=FT                                                                   73
      T=B                                                                     74
      IFLAG=-2                                                                75
      RETURN                                                                  76
  300 FB=FT                                                                   77
      FC=FA                                                                   78
      KOUNT=2                                                                 79
      FX=DMAX1(DABS(FB),DABS(FC))                                             80
    1 IF(DABS(FC).GE.DABS(FB)) GO TO 2                                        81
C                                                                             82
C  INTERCHANGE B AND C SO THAT ABS(F(B)).LE.ABS(F(C)).                        83
C                                                                             84
      A=B                                                                     85
      FA=FB                                                                   86
      B=C                                                                     87
      FB=FC                                                                   88
      C=A                                                                     89
      FC=FA                                                                   90
    2 CMB=0.5D0*(C-B)                                                         91
      ACMB=DABS(CMB)                                                          92
      TOL=RE*DABS(B)+AE                                                       93
C                                                                             94
C  TEST STEOPPING CRITERION AND FUNCTION COUNT.                               95
C                                                                             96
      IF (ACMB.LE.TOL) GO TO 8                                                97
      IF (KOUNT.GE.500) GO TO 12                                              98
C                                                                             99
C  CALCULATE NEW ITERATE IMPLICITLY AS B+P/Q                                 100
C  WHERE WE ARRANGE P.GE.0.  THE IMPLICIT                                    101
C  FORM IS USED TO PREVENT OVERFLOW.                                         102
C                                                                            103
      P=(B-A)*FB                                                             104
      Q=FA-FB                                                                105
      IF (P.GE.0.0D0) GO TO 3                                                106
      P=-P                                                                   107
      Q=-Q                                                                   108
C                                                                            109
C  UPDATE A, CHECK IF REDUCTION IN THE SIZE OF BRACKETING                    110
C  INTERVAL IS SATISFACTORY.  IF NOT, BISECT UNTIL IT IS.                    111
C                                                                            112
    3 A=B                                                                    113
      FA=FB                                                                  114
      IC=IC+1                                                                115
      IF (IC.LT.4) GO TO 4                                                   116
      IF (8.0D0*ACMB.GE.ACBS) GO TO 6                                        117
      IC=0                                                                   118
      ACBS=ACMB                                                              119
C                                                                            120
C  TEST FOR TOO SMALL A CHANGE.                                              121
C                                                                            122
    4 IF (P.GT.DABS(Q)*TOL) GO TO 5                                          123
C                                                                            124
C  INCREMENT BY TOLERANCE.                                                   125
C                                                                            126
      B=B+DSIGN(TOL,CMB)                                                     127
      GO TO 7                                                                128
C                                                                            129
C  ROOT OUGHT TO BE BETWEEN B AND (C+B)/2.                                   130
C                                                                            131
    5 IF (P.GE.CMB*Q) GO TO 6                                                132
C                                                                            133
C  USE SECANT RULE.                                                          134
C                                                                            135
      B=B+P/Q                                                                136
      GO TO 7                                                                137
C                                                                            138
C  USE BISECTION.                                                            139
C                                                                            140
    6 B=0.5D0*(C+B)                                                          141
C                                                                            142
C  HAVE COMPLETED COMPUTATION FOR NEW ITERATE B.                             143
C                                                                            144
    7 T=B                                                                    145
      IFLAG=-3                                                               146
      RETURN                                                                 147
  400 FB=FT                                                                  148
      IF (FB.EQ.0.0D0) GO TO 9                                               149
      KOUNT=KOUNT+1                                                          150
      IF (DSIGN(1.0D0,FB).NE.DSIGN(1.0D0,FC)) GO TO 1                        151
      C=A                                                                    152
      FC=FA                                                                  153
      GO TO 1                                                                154
C                                                                            155
C  FINISHED.  SET IFALG                                                      156
C                                                                            157
    8 IF (DSIGN(1.0D0,FB).EQ.DSIGN(1.0D0,FC)) GO TO 11                       158
      IF (DABS(FB).GT.FX) GO TO 10                                           159
      IFLAG=1                                                                160
      RETURN                                                                 161
    9 IFLAG=2                                                                162
      RETURN                                                                 163
   10 IFLAG=3                                                                164
      RETURN                                                                 165
   11 IFLAG=4                                                                166
      RETURN                                                                 167
   12 IFLAG=5                                                                168
      RETURN                                                                 169
      END                                                                    170
C -----
      SUBROUTINE TDRSGM(RSC,SGM1E,SGM1W)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION RSC(3),RTE(3),RTW(3),RT(3)
C               LAT,LON,RHO FOR S/C
      DATA RTE /0.D0,319.D0,42164.2D0/
      DATA RTW /0.D0,189.D0,42164.2D0/
C
      CALL PODREC(RTE,RT)
      CALL SIGMA1(RSC,RT,SGM1E)
      CALL PODREC(RTW,RT)
      CALL SIGMA1(RSC,RT,SGM1W)
      RETURN
      END
C
C
C
      SUBROUTINE SIGMA1(RSC,RT,SGM1)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)   
      DIMENSION RSC(3),RT(3),RTSC(3)
C
      CALL ADD(1.D0,RT,-1.D0,RSC,RTSC)
      COSGM1=DDOT(RTSC,RSC)/DMAG(RTSC)/DMAG(RSC)
      SGM1=DACOS(COSGM1)
      RETURN
      END
C -----
      SUBROUTINE PODREC(POD,REC)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION POD(3),REC(3)
C
C  POD   = LAT,LON,RHO (DEG)
C
      PHI=RAD(POD(1))
      THE=RAD(POD(2))
      COSPHI=DCOS(PHI)
      REC(1)=POD(3)*COSPHI*DCOS(THE)
      REC(2)=POD(3)*COSPHI*DSIN(THE)
      REC(3)=POD(3)*DSIN(PHI)
      RETURN
      END
C
C
C
      SUBROUTINE POLREC(POL,REC)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION POL(3),REC(3)
C
C  POL   = LAT,LON,RHO (RAD)
C
      PHI=POL(1)
      THE=POL(2)
      COSPHI=DCOS(PHI)
      REC(1)=POL(3)*COSPHI*DCOS(THE)
      REC(2)=POL(3)*COSPHI*DSIN(THE)
      REC(3)=POL(3)*DSIN(PHI)
      RETURN
      END
C
C
C
      SUBROUTINE RECPOL(REC,POL)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION REC(3),POL(3)
      RR=REC(1)*REC(1)+REC(2)*REC(2)+REC(3)*REC(3)
      POL(3)=DSQRT(RR)
      POL(1)=DASIN(REC(3)/POL(3))
      COSPHI=DCOS(POL(1))
      COSTHE=REC(1)/COSPHI/POL(3)
      SINTHE=REC(2)/COSPHI/POL(3)
      POL(2)=DATAN2(SINTHE,COSTHE)
      RETURN
      END
C
C
C
      SUBROUTINE RECPOD(REC,POD)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION REC(3),POL(3),POD(3) 
      CALL RECPOL(REC,POL)
      POD(1)=DEG(POL(1))
      POD(2)=DEG(POL(2))
      POD(3)=POL(3)
      RETURN
      END
C -----
      SUBROUTINE ADD(C1,A,C2,B,C)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION A(3),B(3),C(3)
      C(1)=C1*A(1)+C2*B(1)
      C(2)=C1*A(2)+C2*B(2)
      C(3)=C1*A(3)+C2*B(3)
      RETURN
      END
C
C
C
      SUBROUTINE ADD3(C1,A,C2,B,C3,C,ABC)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION A(3),B(3),C(3),ABC(3)
      ABC(1)=C1*A(1)+C2*B(1)+C3*C(1)
      ABC(2)=C1*A(2)+C2*B(2)+C3*C(2)
      ABC(3)=C1*A(3)+C2*B(3)+C3*C(3)
      RETURN
      END
C
C
C
      SUBROUTINE NEG(X,XNEG)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION X(3),XNEG(3)
      XNEG(1)=-X(1)
      XNEG(2)=-X(2)
      XNEG(3)=-X(3)
      RETURN
      END
C
C
C
      SUBROUTINE LOAD(A,B,C,R)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION R(3)
      R(1)=A
      R(2)=B
      R(3)=C
      RETURN
      END
C
C
C
      SUBROUTINE CROSS(A,B,C)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION A(3),B(3),C(3)
      C(1)=A(2)*B(3)-A(3)*B(2)
      C(2)=A(3)*B(1)-A(1)*B(3)
      C(3)=A(1)*B(2)-A(2)*B(1)
      RETURN
      END
C
C
C
      SUBROUTINE UNIT(XX,XXUNIT)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION XX(3),XXUNIT(3)
      C=DMAG(XX)
      XXUNIT(1)=XX(1)/C
      XXUNIT(2)=XX(2)/C
      XXUNIT(3)=XX(3)/C
      RETURN
      END
C -----
      SUBROUTINE ORBNRM(XI,OM,XN)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION XN(3)
C
C     COMPUTE ORBIT NORMAL VECTOR
C
      SINXI=DSIN(XI)
      XN(1)= SINXI*DSIN(OM)
      XN(2)=-SINXI*DCOS(OM)
      XN(3)= DCOS(XI)
      RETURN
      END
C -----       
      SUBROUTINE SUNPO(DJ,RSUN)
C  GIVEN JULIAN DATE, RETURNS SUN VECTOR IN EARTH EQATOR EQUINOX COORD.
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION A(4),B(4),C(4),E(4),T(4),RSUN(3)
      REAL*8 L,M
      DATA E,A,B,C,AA
     E /.409319755D0,-2.270984344D-4,-2.860242848D-8,8.774643316D-9,
     A 1.74003528D0,628.33195D0,5.279620981D-6,0.,
     B 1.76663681D0,.0300052641D0,7.902462992D-6,5.817764166D-8,
     C .01675104D0,-4.18D-5,-1.26D-7,0.,
     A 80776943.86D0/
      TU=(DJ-2415020.D0)/36525.D0
      DO 10 I=1,4
   10 T(I)=TU**(I-1)
      ECL=0.
      L=0.
      WW=0.
      ECC=0.
      DO 20 I=1,4
      ECL=ECL+E(I)*T(I)
      L=L+A(I)*T(I)
      WW=WW+B(I)*T(I)
   20 ECC=ECC+C(I)*T(I)
      M=L-WW
      CALL KPLR(ECC,M,EE)
      XP=AA*(DCOS(EE)-ECC)
      YP=AA*DSQRT(1.D0-ECC**2)*DSIN(EE)
      XEC=XP*DCOS(WW)-YP*DSIN(WW)
      YEC=XP*DSIN(WW)+YP*DCOS(WW)
      RSUN(1)=-XEC*1.852D0
      RSUN(2)=-YEC*DCOS(ECL)*1.852D0
      RSUN(3)=-YEC*DSIN(ECL)*1.852D0
      RETURN
      END
      SUBROUTINE KPLR(E,DM,CAPE)
      IMPLICIT REAL*8 (A-H,O-Z)
      SUM=0.
      DO 100 MM=1,5
      DMME=0.
      NMFAC=1
      NFAC=1
      DO 10 NN=1,6
      N1=NN-1
      NFAC=N1*NFAC
      IF (N1.EQ.0) NFAC=1
      NMFAC=(N1+MM)*NMFAC
   10 DMME=DMME+(((-1.D0)**N1)*(.5D0*FLOAT(MM)*E)**(2*N1+MM))/
     *     FLOAT(NFAC*NMFAC)
      DSN=MM*DM
  100 SUM=SUM+DMME*DSIN(DSN)/FLOAT(MM)
      CAPE=DM+2.D0*SUM
   20 ETEMP=CAPE
      CAPE=CAPE-(CAPE-E*DSIN(CAPE)-DM)/(1.D0-E*DCOS(CAPE))
      IF (DABS(CAPE-ETEMP).GT.1.D-9) GO TO 20
      RETURN
      END
C -----
      SUBROUTINE MOONPO(JD,MOON)
      IMPLICIT REAL*8 (A-H,O-Z)
C             COMPUTES LUNAR POSITION VECTOR(ARRAY MOON) FOR JULIAN
C             DATE JD,USING TRUNCATED VERSION OF LUNAR THEORY
      REAL*8 JD,MOON,ML,NODE,MLE
      DIMENSION MOON(3)
      PI=3.141592653589793D0
      TODEG=180.D0/PI
      REQ=3443.91739
      TU=(JD-2415020.D0)/36525.D0
      ECL=(84428.25D0-46.86D0*TU)/(3600.D0*TODEG)
      ML=DMOD(270.43416D0+TU*(481267.88314D0-TU*.00113D0),
     *360.D0)/TODEG
      TA=DMOD(281.22083D0+TU*(1.71918D0+TU*.00045D0),360.D0)/TODEG
      TAP=DMOD(334.32956D0+TU*(4069.03403D0-TU*(.01033D0+TU*.00001D0)),
     *360.D0)/TODEG
      NODE=DMOD(259.18328D0+TU*(-1934.14201D0+TU*.00208D0),
     *360.D0)/TODEG
      D=DMOD(350.73749D0+TU*(445267.11422D0-TU*.00144D0),
     *360.D0)/TODEG
      AL=ML-TAP
      ALL=ML-D
      ALP=ALL-TA
      F=ML-NODE
      AA=DCOS(AL)
      BB=DSIN(AL)
      CC=DCOS(ALP)
      DD=DSIN(ALP)
      EE=DCOS(D)
      FF=DSIN(D)
      GG=DCOS(F)
      HH=DSIN(F)
      AA1=2.D0*AA*BB
      AA2=2.D0*EE*FF
      AA3=2.D0*GG*HH
      AA4=1.D0-2.D0*BB*BB
      AA5=1.D0-2.D0*FF*FF
      AA6=1.D0-2.D0*HH*HH
      AA7=BB*CC-DD*AA
      AA8=BB*CC+DD*AA
      BB2=AA*CC-BB*DD
      AA9=BB*AA5-AA*AA2
      BB1=AA1*AA5-AA2*AA4
      BB3=AA8*AA5-BB2*AA2
      BB4=BB*AA5+AA*AA2
      BB5=DD*AA5-CC*AA2
      BB6=AA3*AA5-AA6*AA2
      BB7=BB*AA6+AA*AA3
      BB8=BB*AA6-AA*AA3
      CC3=AA*AA5+BB*AA2
      BB9=AA9*AA5-CC3*AA2
      CC1=AA1*AA+AA4*BB
      CC2=AA1*(1.D0-2.D0*AA2*AA2)-2.D0*AA4*AA2*AA5
      EE4=AA*CC+BB*DD
      CC4=AA7*AA5-EE4*AA2
      CC5=DD*AA5+CC*AA2
      CC6=BB*GG+AA*HH
      CC7=HH*AA-GG*BB
      CC8=HH*AA5-GG*AA2
      DD1=GG*AA+HH*BB
      CC9=CC7*AA5+DD1*AA2
      DD2=GG*AA-HH*BB
      DD3=CC6*AA5-DD2*AA2
      DD4=HH*AA5+GG*AA2
      DD5=AA1*GG+AA4*HH
      DD6=CC7*AA5-DD1*AA2
      DD7=HH*AA4-GG*AA1
      DD8=GG*AA5+HH*AA2
      DD9=CC8*CC+DD8*DD
      EE1=AA*AA5-BB*AA2
      EE2=CC*AA5+DD*AA2
      EE3=BB2*AA5+AA8*AA2
      EE5=AA*AA6+BB*AA3
      EE6=AA4*AA-BB*AA1
      EE7=AA*(1.D0-2.D0*AA2*AA2)+2.D0*BB*AA2*AA5
C             RIGHT ASCENSION
      MLE=ML+(22639.580D0*BB-4586.438D0*AA9+2369.899D0*AA2
     X+769.021D0*AA1-668.944D0
     X*DD-411.614D0*AA3-211.658D0*BB1-206.219D0*BB3+191.954D0*BB4
     X-165.351D0*BB5+14
     X7.878D0*AA7-124.785D0*FF-109.804D0*AA8-55.174D0*BB6
     X-45.100D0*BB7+39.532D0*BB8-
     X38.428D0*BB9+36.124D0*CC1-30.773D0+CC2-28.511D0*
     X*CC4-24.451D0*CC5)*.017453293D0/3600.D0
C             DECLINATION RELATIVE TO THE ECLIPTIC
      DECE=(18461.480D0*HH+1010.18D0*CC6-999.695D0*CC7
     X-623.658D0*CC8+199.485D0*CC9
     X-166.577D0*DD3+117.262D0*DD4+61.913D0*DD5-33.359D0*DD6
     X-31.763D0*DD7-29.689D0*DD9)*.017453293D0/3600.D0
C             PARALLAX(I.E. RECIPROCAL OF RADIUS)
      PAE=(3422.54D0+186.5398D0*AA+34.3117D0*CC3
     X+28.2333D0*AA5+10.1657D0*AA4+3.086D0
     X1*EE1+1.9202D0*EE2+1.4455D0*EE3+1.1542D0*EE4
     X-.9752D0*EE-.9502D0*BB2-.7136D0*EE
     X5+.6215D0*EE6+.6008D0*EE7)*.017453293D0/3600.D0
C             TRANSFORMATION TO EARTH-CENTERED EQUATORIAL COORDINATES
      RAM=REQ/PAE
      ZEC=RAM*DSIN(DECE)
      TEMP=RAM*DCOS(DECE)
      MOON(1)=TEMP*DCOS(MLE)*1.852D0
      YEC=TEMP*DSIN(MLE)
      CE=DCOS(ECL)
      SE=DSIN(ECL)
      MOON(2)=(YEC*CE-ZEC*SE)*1.852D0
      MOON(3)=(YEC*SE+ZEC*CE)*1.852D0
      RETURN
      END
C -----
      SUBROUTINE PWINIT(IUNIT,NAME,KDEG,KRITE) 
C *************************************************************************
C  PWINIT INITIALIZES THE COMMON BLOCK CPWFIT FOR PIECE-WISE POLY NOMIAL 
C  INTERPOLATION OF A SET OF DATA.  THE DATA IS STORED IN THE FILE CALLED
C  NAME.  TO USE PWFIT, CALL PWINIT FIRST AND SUPPLY ALL NECESSARY PARAME-
C  TERS AND FILE.  THEN SIMPLY CALL PWINT(X,Y,KRITE) FOR INTERPOLATION.
C
C  VARIABLES -
C
C     IUNIT : FILE UNIT NUMBER.
C     KRITE : IF KRITE>0, WILL PRINT OUT COEFFICIENTS ON SCREEN.
C     NAME  : FILE NAME CONTAINING THE DATA.
C     NDEG  : THE DEGREE OF POLYNOMIALS USED FOR INTERPOLATION.
C     NPTS  : NUMBER OF POINTS IN THE DATA FILE CALLED NAME.
C     X,Y   : THE DATA POINTS. 
C
C  SUBROUTINES -
C
C     PWCOE : THIS SUBROUTINE SETS UP THE POLYNOMIAL COEFFICIENTS.
C
C  FILES-
C
C     NAME  : IUNIT, CONTAINS DATA POINTS.  
C             FORMAT:  NPTS
C                      X(1)   ,Y(1)
C                       ..    ,  ..
C                      X(NPTS),Y(NPTS)
C *************************************************************************
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      CHARACTER*12 NAME
C
      COMMON /CPWFIT/ X(100),Y(100),XNODE(100),COEF(200),NPTS,NDEG,NODES
C
      NDEG=KDEG
      OPEN(IUNIT,FILE=NAME,STATUS='OLD')
      READ(IUNIT,*) NPTS
      DO 100 N=1,NPTS
      READ(IUNIT,*) X(N),Y(N)
  100 CONTINUE
      CALL PWCOE(KRITE)
      CLOSE(IUNIT)
      RETURN
      END
C -----
      SUBROUTINE PWCOE(KRITE)
C
C     PIECEWISE LAGRANGE POLYNOMIAL INTERPOLATION 
C     FIT A PIECEWISE POLYNOMIAL CURVE OF DEGREE NDEG TO THE DATA X,Y. 
C
C     X, Y  : DATA TO BE FITTED
C     COEF  : COEF. OF THE POLYNOMIALS.
C     XNODE : THE END POINTS (NODES) OF THE POLYNOMIAL PIECES.
C     NODES : NUMBER OF NODES.
C     NPTS  : NUMBER OF POINTS TO BE FITTED. (DIM OF X,Y)
C     NDEG  : DEGREE OF THE POLYNOMIALS. 
C     LPTS  : NDEG+1, THE NUMBER OF POINTS FOR EACH LAGRANGE POLYNOMIAL FIT.
C     KRITE : IF > 0 WILL PRINT COEFFICIENTS ON SCREEN.
C
C     SUBROUTINES
C
C     LAGRNG : DETERMINES POLY COEF USING LAGRANGE'S METHOD.
C     LAGINT : INTERPOLATION USING THE LAGRANGE POLY.
C     PWCOE  : SETS THE COEF AND DETERMINES NODES AND XNODE.  
C     PWINT  : PIECEWISE POLY INTERPOLATION. 
C     PWSETL : CALLED BY PWCOE, ACTUALLY SETS COEF.
C
C     SET THE COEFFICIENTS AND NODES OF THE LAGRANGE POLYNOMIALS OF DEGREE
C     NDEG FOR NPTS POINTS OF DATA IN X,Y.  THERE ARE NPIECE NUMBER OF POLY
C     TO APPROXIMATE THIS DATA.  
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /CPWFIT/ X(100),Y(100),XNODE(100),COEF(200),NPTS,NDEG,NODES
C
C  I  SET UP CONSTANTS
C
      NPIECE=(NPTS-1)/NDEG
      IF(NPIECE.LE.0) THEN
         NDEG=NPTS-1
         NPIECE=1
      END IF
      LPTS=NDEG+1
C
C  II  COMPUTE COEFFICIENTS
C
      DO 250 N=1,NPIECE
      L=NDEG*(N-1)
      XNODE(N)=X(L+1)
      CALL PWSETL(LPTS,N)
  250 CONTINUE
      NODES=NPIECE+1
C
C  THIS TAKES CARE OF THE LAST FEW POINTS OF DATA.  THIS OCCURS WHEN THE LAST
C  PIECE OF DATA DO NOT HAVE ENOUGH POINTS FOR AN NDEG FIT.  LPTS IS THE 
C  NUMBER OF POINTS IN THIS PIECE OF DATA.  
C
      ITEST=NPTS-(NDEG*NPIECE+1)
      IF(ITEST.GT.0) THEN
         LPTS=ITEST+1
         NPIECE=NPIECE+1
         L=NDEG*(NPIECE-1)
         CALL PWSETL(LPTS,NPIECE)
         XNODE(NPIECE)=X(L+1)
         NODES=NODES+1
      END IF
      XNODE(NODES)=X(NPTS)
C
      IF(KRITE.GT.0) THEN
         DO 300 N=1,NODES
         WRITE(*,3000) N,(COEF(L+3*N-3),L=1,LPTS)
 3000    FORMAT(I5,5E14.7:/5X,5E14.7) 
  300    CONTINUE
      END IF
C
      RETURN
      END  
C -----
      SUBROUTINE PWSETL(LPTS,LPIECE)
C
C  PWSETL ACTUALLY SETS THE LPIECE-TH POLY COEF.  THIS POLY IS OF DEGREE NDEG
C  WITH LPTS NUMBER OF DATA POINTS.  HOWEVER, THE ACTUAL DEGREE IS GIVEN BY
C  LPTS+1, NDEG MAY BE BIGGER, THE EXTRA COEFFICIENTS ARE JUST 0.  THIS MAY
C  OCCUR AT THE END OF THE PW-FIT WHEN THE NUMBER OF DATA POINTS FOR THE LAST
C  PIECE OF POLY IS NOT SUFFICIENT FOR NDEG DEGREE  LAGRANGE INTERPOLATION.
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /CPWFIT/ X(100),Y(100),XNODE(100),COEF(200),NPTS,NDEG,NODES
      DIMENSION XL(100),YL(100),COEFL(100)
C
      L=NDEG*(LPIECE-1)
      LC=L+(LPIECE-1)
      DO 100 I=1,NDEG+1
         XL(I)=X(L+I)
         YL(I)=Y(L+I)
         COEFL(I)=0.D0
  100 CONTINUE
      CALL LAGRNG(LPTS,XL,YL,COEFL)
      DO 200 I=1,NDEG+1
         COEF(LC+I)=COEFL(I)
  200 CONTINUE
      RETURN
      END
C
      SUBROUTINE PWINT(XX,YY,KRITE)
C
C     USING PIECEWISE POLYNOMIAL INTERPOLATION DETERMINE THE CORRESPONDING 
C     VALUE YY AT XX. 
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /CPWFIT/ X(100),Y(100),XNODE(100),COEF(200),NPTS,NDEG,NODES
      DIMENSION COEFL(100),XL(100)
C
      LPTS=NDEG+1
      DO 100 N=1,NODES-1
         LPIECE=-1
         IF((XX.GE.XNODE(N)).AND.(XX.LE.XNODE(N+1))) THEN
         LPIECE=N
         END IF
      IF(LPIECE.GT.0) GOTO 199
  100 CONTINUE
  199 CONTINUE
C
C  II  SET COEFL
C
      L=(LPTS-1)*(LPIECE-1)
      LC=L+LPIECE-1
C LC KEEPS TRACK OF THE COEFFICIENTS.
      DO 200 I=1,LPTS
      COEFL(I)=COEF(LC+I)
      XL(I)=X(L+I)
  200 CONTINUE
C
C  III CALCULATE YY
C 
      CALL LAGINT(NPTS,XL,COEFL,XX,YY,KRITE)
C
      RETURN
      END
C -----
      SUBROUTINE LAGRNG(NPTS,X,Y,COEF)
C
C     FROM THE NPTS NUMBER OF DATA POINTS IN X, Y, DETERMINE THE LAGRANGE POLY
C     THAT FITS THE DATA.  THE POLY COEF IS IN COEF.
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION X(NPTS),Y(NPTS),COEF(NPTS)
C
      COEF(1)=Y(1)
      DO 500 N=2,NPTS
      TOP=Y(N)
      BOT=1.D0
      DO 300 K=1,N-1
      XN=BOT
      TOP=TOP-COEF(K)*XN
      BOT=XN*(X(N)-X(K))
  300 CONTINUE
      COEF(N)=TOP/BOT
  500 CONTINUE    
      RETURN
      END
C -----
      SUBROUTINE LAGINT(NPTS,X,COEF,XX,YY,KRITE)
C
C     INTERPOLATE THE LAGRANGE POLYNOMIAL WITH COEFFICIENTS IN COEF.
C     YY=POLY(XX)   
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION X(NPTS),COEF(NPTS)
C
      YY=COEF(1)
      XXN=1.D0
      DO 500 N=1,NPTS-1
      XXN=XXN*(XX-X(N))
      YY=YY+XXN*COEF(N+1)
  500 CONTINUE
      IF(KRITE.GT.0) PRINT*,XX,YY
      RETURN
      END
C -----
      SUBROUTINE CLAVEC(GMU,SM,EC,XI,OM,AR,TR,RR,VV)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION RR(3),VV(3),RUNIT(3),DRUNIT(3)
C
      COM=DCOS(OM)
      SOM=DSIN(OM)
      CXI=DCOS(XI)
      SXI=DSIN(XI)
      AT =AR+TR
      CAT=DCOS(AT)
      SAT=DSIN(AT)
      CC =COM*CAT
      SS =SOM*SAT
      CS =COM*SAT
      SC =SOM*CAT
      RUNIT(1) = CC-SS*CXI
      RUNIT(2) = SC+CS*CXI
      RUNIT(3) =   SAT*SXI
      DRUNIT(1)=-CS-SC*CXI
      DRUNIT(2)=-SS+CC*CXI
      DRUNIT(3)=   CAT*SXI
      PP=SM*(1.D0-EC*EC)
      SQRTPP=DSQRT(GMU/PP)
      ECOSTR=1.D0+EC*DCOS(TR)
      RMAG=PP/ECOSTR
      DR  =SQRTPP*EC*DSIN(TR)
      RDTR=SQRTPP*ECOSTR
      RR(1)=RMAG*RUNIT(1)
      RR(2)=RMAG*RUNIT(2)
      RR(3)=RMAG*RUNIT(3)
      VV(1)=DR*RUNIT(1)+RDTR*DRUNIT(1)
      VV(2)=DR*RUNIT(2)+RDTR*DRUNIT(2)
      VV(3)=DR*RUNIT(3)+RDTR*DRUNIT(3)
      RETURN
      END
C
C
C
      SUBROUTINE CLAVED(GMU,SM,EC,XID,OMD,ARD,TRD,RR,VV)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION RR(3),VV(3)
C
      XI=RAD(XID)
      OM=RAD(OMD)
      AR=RAD(ARD)
      TR=RAD(TRD)
      CALL CLAVEC(GMU,SM,EC,XI,OM,AR,TR,RR,VV)
      RETURN
      END
C -----
      SUBROUTINE VECLA(GMU,RR,VV,SM,EC,XI,OM,AR,TR)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION RR(3),VV(3),HH(3)
C
      CALL CROSS(RR,VV,HH)
      RMAG=DMAG(RR)
      VMAG=DMAG(VV)
      HMAG=DMAG(HH)
      CXI=HH(3)/HMAG
      XI=DACOS(CXI)
C
      EE=VMAG*VMAG/2.D0-GMU/RMAG
      ALFA=-2.D0*EE/GMU
      PP=HMAG*HMAG/GMU
      IF(DABS(ALFA).GT.1.D-12) THEN
         SM=1.D0/ALFA
         PPALFA=PP*ALFA
         PPTEST=1.D0-PPALFA
         EC=0.D0
         IF(PPTEST.GT.(1.D-12)) EC=DSQRT(PPTEST)
      ELSE     ! PARABOLA CASE OR ERROR
         IF(PPTEST.LT.0.D0) THEN
            PRINT*,'**********************************'
            PRINT*,'* SUBROUTINE VECLA ERROR, EC**2=',PPTEST
            PRINT*,'**********************************'
            STOP
         ENDIF
         SM=ALFA
         EC=1.D0
      END IF
C
      IF(DABS(XI).GT.1.D-10) THEN
         OM=DATAN2(HH(1),-HH(2))
         SARTR=RR(3)/RMAG/DSIN(XI)
         ARTR=DASIN(SARTR)
      ELSE
         OM=0.D0
         ARTR=DATAN2(RR(2),RR(1))
      END IF
C
      IF(EC.GT.1.D-10) THEN
        CTR=(PP/RMAG-1.D0)/EC
        DCTR=DABS(CTR)
        IF(DCTR.GT.1.D0) CTR=CTR/DCTR
        TR=DACOS(CTR)
      ELSE
        TR=ARTR
      END IF
C
      AR=ARTR-TR
C
      RETURN
      END
C
C
C
      SUBROUTINE VECLA_OLD(GMU,RR,VV,SM,EC,XI,OM,AR,TR)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION RR(3),VV(3),HH(3)
C
      CALL CROSS(RR,VV,HH)
      RMAG=DMAG(RR)
      VMAG=DMAG(VV)
      HMAG=DMAG(HH)
      CXI=HH(3)/HMAG
      XI=DACOS(CXI)
C
      EE=VMAG*VMAG/2.D0-GMU/RMAG
      ALFA=-2.D0*EE/GMU
      PP=HMAG*HMAG/GMU
      IF(DABS(ALFA).GT.1.D-10) THEN
         SM=1.D0/ALFA
         PPALFA=PP*ALFA
         PPTEST=DABS(PPALFA-1.D0)
         EC=0.D0
         IF(DABS(PPTEST).LT.1.D-10) EC=DSQRT(1.D0-PPALFA) 
      ELSE
         SM=ALFA
         EC=1.D0
      END IF
C
      IF(DABS(XI).GT.1.D-10) THEN
         OM=DATAN2(HH(1),-HH(2))
         SARTR=RR(3)/RMAG/DSIN(XI)
         ARTR=DASIN(SARTR)
      ELSE
         OM=0.D0
         ARTR=DATAN2(RR(2),RR(1))
      END IF
C
      IF(EC.GT.1.D-10) THEN
        CTR=(PP/RMAG-1.D0)/EC
        DCTR=DABS(CTR)
        IF(DCTR.GT.1.D0) CTR=CTR/DCTR
        TR=DACOS(CTR)
      ELSE
        TR=ARTR
      END IF
C
      AR=ARTR-TR
C
      RETURN
      END
C
C
C
      SUBROUTINE VECLAD(GMU,RR,VV,SM,EC,XID,OMD,ARD,TRD)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION RR(3),VV(3)
C
      XI=RAD(XID)
      OM=RAD(OMD)
      AR=RAD(ARD)
      TR=RAD(TRD)
      CALL VECLA(GMU,RR,VV,SM,EC,XI,OM,AR,TR)
      RETURN
      END
C -----
      SUBROUTINE PROPG(GMU,R1,V1,DT,R2,V2)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION R1(3),V1(3),R2(3),V2(3)
C
      CALL VECLA(GMU,R1,V1,SM,EC,XI,OM,AR,TR1)
      T1=TRU2T(GMU,SM,EC,TR1)
      T2=T1+DT
      TR2=T2TRU(GMU,SM,EC,T2)
      CALL CLAVEC(GMU,SM,EC,XI,OM,AR,TR2,R2,V2)
      RETURN
      END
C -----
      SUBROUTINE ROOT2(T,FT,B,C,RELERR,ABSERR,IFLAG)                            1
C                                                                              2
C  ROOT COMPUTES A ROOT OF THE NONLINEAR EQUATION F(X)=0                       3
C  WHERE F(X) IS A CONTINUOUS REAL FUNCTION OF A SINGLE REAL                   4
C  VARIABLE X.  THE METHOD USED IS A COMBINATION OF BISECTION                  5
C  AND THE SECAND RULE.                                                        6
C                                                                              7
C  NORMAL INPUT CONSISTS OF A CONTINUOUS FUNCTION F AND AN                     8
C  INTERVAL (B,C) SUCH THAT F(B)*F(C).LE.0.0.  EACH ITERATION                  9
C  FINDS NEW VALUES OF B AND C SUCH THAT THE INTERVAL (B,C) IS                10
C  SHRUNK AND F(B)*F(C).LE.0.0.  THE STOPPING CRITERION IS                    11
C                                                                             12
C          ABS(B-C).LE.2.0*(RELERR*ABS(B)+ABSERR)                             13
C                                                                             14
C  WHERE RELERR=RELATIVE ERROR AND ABSERR=ABSOLUTE ERROR ARE                  15
C  INPUT QUANTITIES.  SET THE FLAG, IFLAG, POSITIVE TO INITIALIZE             16
C  THE COMPUTATION.  AS B,C AND IFLAG ARE USED FOR BOTH INPUT AND             17
C  OUTPUT, THEY MUST BE VARIABLES IN THE CALLING PROGRAM.                     18
C                                                                             19
C  IF 0 IS A POSSIBLE ROOT, ONE SHOULD NOT CHOOSE ABSERR=0.0.                 20
C                                                                             21
C  THE OUTPUT VALUE OF B IS THE BETTER APPROXIMATION TO A ROOT                22
C  AS B AND C ARE ALWAYS REDEFINED SO THAT ABS(F(B)).LE.ABS(F(C)).            23
C                                                                             24
C  TO SOLVE THE EQUATION, ROOT MUST EVALUATE F(X) REPEATEDLY. THIS            25
C  IS DONE IN THE CALLING PROGRAM.  WHEN AN EVALUATION OF F IS                26
C  NEEDED AT T, ROOT RETURNS WITH IFLAG NEGATIVE.  EVALUATE FT=F(T)           27
C  AND CALL ROOT AGAIN.  DO NOT ALTER IFALG.                                  28
C                                                                             29
C  WHEN THE COMPUTATION IS COMPLETE, ROOT RETURNS TO THE CALLING              30
C  PROGRAM WITH IFLAG POSITIVE:                                               31
C                                                                             32
C     IFLAG=1  IF F(B)*F(C).LT.0 AND THE STOPPING CRITERION IS MET.           33
C                                                                             34
C          =2  IF A VALUE B IS FOUND SUCH THAT THE COMPUTED VALUE             35
C              F(B) IS EXACTLY ZERO.  THE INTERVAL (B,C) MAY NOT              36
C              SATISFY THE STOPPING CRITERION.                                37
C                                                                             38
C          =3  IF ABS(F(B)) EXCEEDS THE INPUT VALUES ABS(F(B)),               39
C              ABS(F(C)).   IN THIS CASE IT IS LIKELY THAT B IS CLOSE         40
C              TO A POLE OF F.                                                41
C                                                                             42
C          =4  IF NO ODD ORDER ROOT WAS FOUND IN THE INTERVAL.  A             43
C              LOCAL MINIMUM MAY HAVE BEEN OBTAINED.                          44
C                                                                             45
C          =5  IF TOO MANY FUNCTION EVALUATIONS WERE MADE.                    46
C              (AS PROGRAMMED, 500 ARE ALLOWED.)                              47
C                                                                             48
C  THIS CODE IS A MODIFICATION OF THE CODE  ZEROIN  WHICH IS COMPLETELY       49
C  EXPLAINED AND DOCUMENTED IN THE TEST,  NUMERICAL COMPUTING:  AN            50
C  INTRODUCTION  BY L. F. SHAMPINE AND R. C. ALLEN.                           51
C                                                                             52
C***********************************************************************      53
C*  THE ONLY MACHINE DEPENDENT CONSTANT IS BASED ON THE MACHINE UNIT   *      54
C*  ROUNDOFF ERROR  U  WHICH IS THE SMALLEST POSITIVE NUMBER SUCH THAT *      55
C*  1.0+U .GT. 1.0 .  U  MUST BE CALCULATED AND INSERTED IN THE        *      56
C*  FOLLOWING DATA STATEMENT BEFORE USING  ROOT .  THE ROUTINE MACHIN  *      57
C*  CALCULATES  U .                                                    *      58
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                    58A
      DATA U /2.2D-16/                                                        59
C***********************************************************************      60
C                                                                             61
      IF (IFLAG.GE.0) GO TO 100                                               62
      IFLAG=IABS(IFLAG)                                                       63
      GO TO (200,300,400), IFLAG                                              64
  100 RE=DMAX1(RELERR,U)                                                      65
      AE=DMAX1(ABSERR,0.0D0)                                                  66
      IC=0                                                                    67
      ACBS=DABS(B-C)                                                          68
      A=C                                                                     69
      T=A                                                                     70
      IFLAG=-1                                                                71
      RETURN                                                                  72
  200 FA=FT                                                                   73
      T=B                                                                     74
      IFLAG=-2                                                                75
      RETURN                                                                  76
  300 FB=FT                                                                   77
      FC=FA                                                                   78
      KOUNT=2                                                                 79
      FX=DMAX1(DABS(FB),DABS(FC))                                             80
    1 IF(DABS(FC).GE.DABS(FB)) GO TO 2                                        81
C                                                                             82
C  INTERCHANGE B AND C SO THAT ABS(F(B)).LE.ABS(F(C)).                        83
C                                                                             84
      A=B                                                                     85
      FA=FB                                                                   86
      B=C                                                                     87
      FB=FC                                                                   88
      C=A                                                                     89
      FC=FA                                                                   90
    2 CMB=0.5D0*(C-B)                                                         91
      ACMB=DABS(CMB)                                                          92
      TOL=RE*DABS(B)+AE                                                       93
C                                                                             94
C  TEST STEOPPING CRITERION AND FUNCTION COUNT.                               95
C                                                                             96
      IF (ACMB.LE.TOL) GO TO 8                                                97
      IF (KOUNT.GE.500) GO TO 12                                              98
C                                                                             99
C  CALCULATE NEW ITERATE IMPLICITLY AS B+P/Q                                 100
C  WHERE WE ARRANGE P.GE.0.  THE IMPLICIT                                    101
C  FORM IS USED TO PREVENT OVERFLOW.                                         102
C                                                                            103
      P=(B-A)*FB                                                             104
      Q=FA-FB                                                                105
      IF (P.GE.0.0D0) GO TO 3                                                106
      P=-P                                                                   107
      Q=-Q                                                                   108
C                                                                            109
C  UPDATE A, CHECK IF REDUCTION IN THE SIZE OF BRACKETING                    110
C  INTERVAL IS SATISFACTORY.  IF NOT, BISECT UNTIL IT IS.                    111
C                                                                            112
    3 A=B                                                                    113
      FA=FB                                                                  114
      IC=IC+1                                                                115
      IF (IC.LT.4) GO TO 4                                                   116
      IF (8.0D0*ACMB.GE.ACBS) GO TO 6                                        117
      IC=0                                                                   118
      ACBS=ACMB                                                              119
C                                                                            120
C  TEST FOR TOO SMALL A CHANGE.                                              121
C                                                                            122
    4 IF (P.GT.DABS(Q)*TOL) GO TO 5                                          123
C                                                                            124
C  INCREMENT BY TOLERANCE.                                                   125
C                                                                            126
      B=B+DSIGN(TOL,CMB)                                                     127
      GO TO 7                                                                128
C                                                                            129
C  ROOT OUGHT TO BE BETWEEN B AND (C+B)/2.                                   130
C                                                                            131
    5 IF (P.GE.CMB*Q) GO TO 6                                                132
C                                                                            133
C  USE SECANT RULE.                                                          134
C                                                                            135
      B=B+P/Q                                                                136
      GO TO 7                                                                137
C                                                                            138
C  USE BISECTION.                                                            139
C                                                                            140
    6 B=0.5D0*(C+B)                                                          141
C                                                                            142
C  HAVE COMPLETED COMPUTATION FOR NEW ITERATE B.                             143
C                                                                            144
    7 T=B                                                                    145
      IFLAG=-3                                                               146
      RETURN                                                                 147
  400 FB=FT                                                                  148
      IF (FB.EQ.0.0D0) GO TO 9                                               149
      KOUNT=KOUNT+1                                                          150
      IF (DSIGN(1.0D0,FB).NE.DSIGN(1.0D0,FC)) GO TO 1                        151
      C=A                                                                    152
      FC=FA                                                                  153
      GO TO 1                                                                154
C                                                                            155
C  FINISHED.  SET IFALG                                                      156
C                                                                            157
    8 IF (DSIGN(1.0D0,FB).EQ.DSIGN(1.0D0,FC)) GO TO 11                       158
      IF (DABS(FB).GT.FX) GO TO 10                                           159
      IFLAG=1                                                                160
      RETURN                                                                 161
    9 IFLAG=2                                                                162
      RETURN                                                                 163
   10 IFLAG=3                                                                164
      RETURN                                                                 165
   11 IFLAG=4                                                                166
      RETURN                                                                 167
   12 IFLAG=5                                                                168
      RETURN                                                                 169
      END                                                                    170
C
      SUBROUTINE VIS(R1,R2,REQ,IVIS)
C
C  DATE: 3/11/87
C
C  DESCRIPTION:
C
C  TEST TO SEE IF R1 IS VISIBLE TO R2 GIVEN A SPHERICAL OBSTRUCTING BODY OF
C  RADIUS REQ.  THIS ASSUMES R1, R2 > OR = REQ.
C
C  METHOD:
C
C  R3=R1-R2 IS THE LINE OF SIGHT VECTOR FROM R1 TO R2.
C  THE LENGTH OF R3, GIVEN R1 AND R2, UNIQUELY DETERMINES WHETHER THE LINE
C  OF SIGHT IS OBSTRUCTED BY THE SPHERE OR NOT.  THERE IS A NUMBER R30 WHERE
C       IF R3 > R30, THEN R1 SEES R2;
C       IF R3 < R30, THEN R1 DOES NOT SEE R2.
C  R30 IS THE LINE OF SIGHT VECTOR DETERMINED BY DISPLACING R1 AND R2 SO THAT
C  THE LINE OF SIGHT BETWEEN THEM EXACTLY GRAZES THE SPHERE OF RADIUS REQ.
C
C  VARIABLES:
C
C  R1    : POSITION VECTOR 1
C  R2    : POSITION VECTOR 2
C  REQ   : RADIUS OF OBSTRUCTING SPHERE
C  IVIS  : 1 VISIBLE, -1 NOT VISIBLE
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION R1(3),R2(3)
C
C  R3=R1-R2, LINE OF SIGHT FROM R1 TO R2.  
C  COMPUTE DDOT(R3,R3) BY EXPANSION.
      IVIS=-1
      RR1=DDOT(R1,R1)
      RR2=DDOT(R2,R2)
      R12=DDOT(R1,R2)
      RR3=RR1+RR2-2*R12
      REQ2=REQ*REQ
      R30=DSQRT(RR1-REQ2)+DSQRT(RR2-REQ2)
      RR30=R30*R30
      IF(RR3.LT.RR30) IVIS=1
C  WE COMPARE SQUARES HERE SINCE SQUARE ROOTS ARE LESS EFFICIENT.
      RETURN
      END
C -----
      SUBROUTINE LGNDR(NORDER,X,PNM,DP)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      REAL*8         PNM,DP
      INTEGER        NORDER,NMAX
      PARAMETER      (NMAX=12)
      DIMENSION PNM(0:NMAX,0:NMAX),DP(0:NMAX,0:NMAX)
      DIMENSION A(NMAX),B(NMAX)
C
C
C  I. LEGENDRE FUNCTIONS
C
      XX=1.D0-X*X
      SQXX=DSQRT(XX)
      IF(SQXX.LT.1.D-9) THEN
         WRITE(1,1000) X
 1000    FORMAT(' ***** SUB LGNDR *****'//
     +   ' X TOO CLOSE TO 1. ',F14.4//)
         STOP
      END IF
      PNM(0,0)=1.D0
      PNM(1,0)=X
      DO 100 N=2,NORDER
      OVRN=1.D0/N
      PNM(N,0)=(2.D0-OVRN)*X*PNM(N-1,0)-(1.D0-OVRN)*PNM(N-2,0)
  100 CONTINUE
C
C  II. DERIVATIVES OF LEGENDRE FUNCTIONS
C
      DP(0,0)=0.D0
      DO 250 N=1,NORDER
      A(N)=0.D0
      B(N)=0.D0
      NK=-1
      DO 200 K=0,N-1
         A(N)=A(N)+(2*K+1)*PNM(K,0)
         NK=NK*(-1)
         B(N)=B(N)+(2*K+1)*PNM(K,0)*NK
  200 CONTINUE
      IF(MOD(N,2).NE.0) THEN
         DP(N,0)=(A(N)+B(N))*.5D0
      ELSE
         DP(N,0)=(A(N)-B(N))*.5D0
      END IF
  250 CONTINUE
C
C  III. ASSOCIATED LEGENDRE FUNCTIONS
C
      PNM(1,1)=-SQXX
      PNM(2,1)=-SQXX*X*3.D0
C      PNM(2,2)=3.D0*XX
      DO 350 M=1,NORDER-2
      DO 300 N=M+2, NORDER
         PNM(N,M)=((2*N-1)*X*PNM(N-1,M)-(N+M-1)*PNM(N-2,M))/(N-M)
  300 CONTINUE
      MM=M-1
      DO 310 NN=M+1,M+2
         PNM(NN,MM+2)=-2.D0*(MM+1)*X*PNM(NN,MM+1)/SQXX
     +                -(NN-MM)*(NN+MM+1)*PNM(NN,MM)
  310 CONTINUE
  350 CONTINUE
      MM=NORDER-2
      NN=NORDER
         PNM(NN,MM+2)=-2.D0*(MM+1)*X*PNM(NN,MM+1)/SQXX
     +                -(NN-MM)*(NN+MM+1)*PNM(NN,MM)
C WHEN NORDER=1 ABOVE EQUATION FAIL FOR PNM(1,1).
      PNM(1,1)=-SQXX
C
C  IV. DERIVATIVES OF ASSOCIATED LEGENDRE FUNCTIONS
C
      DO 450 N=1,NORDER
      DO 400 M=1,N
         DP(N,M)=(M*X*PNM(N,M)+(N+M)*(N-M+1)*SQXX*PNM(N,M-1))/XX
  400 CONTINUE
  450 CONTINUE
C
C  V. CHANGE SIGN OF ASSOCIATED LEGENDRE FUNCTIONS
C
      DO 550 N=1,NORDER
      NEG=-1
      DO 500 M=1,N
         PNM(N,M)=PNM(N,M)*NEG
         DP(N,M) = DP(N,M)*NEG
         NEG=NEG*(-1)
  500 CONTINUE
  550 CONTINUE
C
      RETURN
      END
C -----
      SUBROUTINE DELTJ2(GMU,REQ,XJ2,SM,EC,XI,DOM,DAR,DM)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
C  J2 EFFECTS ON NODE, ARGUMENT OF PERIGEE, MEAN MOTION.
C  VARIABLES :
C     (OUTPUT)
C     DOM    : NODAL REGRESSION RATE RADIANS/SEC
C     DAR    : APSIDAL ROTATION RATE RADIANS/SEC
C     DM     : n=dM/dt, MEAN MOTION WITH J2 ACCOUNTED
C              ANOMALISTIC PERIOD = 2PI/n
C  REFERENCE : WERTZ, PG. 67-69
C
      XN=DSQRT(GMU/SM)/SM
      CCC=XN*1.5*XJ2*REQ*REQ/SM/SM/(1-EC*EC)**2
      COSXI=DCOS(XI)
      DOM=-CCC*COSXI
      SINSQ=1.D0-COSXI*COSXI
      DAR= CCC*(2.D0-2.5D0*SINSQ)
      DM=XN+CCC*(1.D0-1.5*SINSQ)*DSQRT(1.D0-EC*EC)
      RETURN
      END
	FUNCTION DANG(R,V)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	DIMENSION R(3),V(3),RCV(3)
	PROD=DMAG(R)*DMAG(V)
	IF(PROD.EQ.0)GO TO 20
	C=DDOT(R,V)/PROD
	IF(DABS(C).GT..999)GO TO 30
	DANG=DACOS(C)
	RETURN
30	CALL CROSS(R,V,RCV)
	S=DMAG(RCV)/PROD
	DANG=DASIN(S)
	IF(C.LT.0) DANG=RAD(1.8D2)-DANG
	RETURN
20	DANG=0
	RETURN
	END
      SUBROUTINE CIRGEN(SALF,CALF,SBETA,CBETA,SPHI,CPHI,STHE,CTHE,
     +                  RR,RRPOD)
C
C  GENERATES A CIRCLE ON THE SPHERE CENTERED ON A
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION RR(3),RRPOD(3)
C
      C1=SALF*CBETA
      C2=C1*CPHI+CALF*SPHI
      C3=SALF*SBETA
      RR(1)=CTHE*C2-C3*STHE
      RR(2)=STHE*C2+C3*CTHE
      RR(3)=CALF*CPHI-C1*SPHI
      CALL RECPOD(RR,RRPOD)
      RETURN
      END
C
