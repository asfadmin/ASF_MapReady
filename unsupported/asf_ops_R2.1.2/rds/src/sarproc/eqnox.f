      SUBROUTINE EQNOX(X,GM,Y)  
C/*   SUBROUTINE EQNOX(X,GM,Y)  --------------------------
C
C  VERSION OF 4/1/85
C  PURPOSE   
C    COMPUTES THE EQUINOCTIAL ELEMENTS A,H,K,P,Q,LAMDA AND 
C    CLASSICAL ELEMENTS E,I,NODE,W,M AND OTHER MISC VARIABLES
C  INPUT 
C    X      = 6-D CARTESIAN COORD X,Y,Z,XD,YD,ZD (KM,KM/SEC)
C    GM     = GRAVITATION CONSTANT * MASS OF PLANET (KM**3/SEC**2)
C  OUTPUT
C    Y(1)   = A, SEMI-MAJOR AXIS (KM)
C    Y(2)   = H, E * SIN(W + NODE) 
C    Y(3)   = K, E * COS(W + NODE) 
C    Y(4)   = P, TAN(I/2) * SIN(NODE)  
C    Y(5)   = Q, TAN(I/2) * COS(NODE)  
C    Y(6)   = LAMDA, M + NODE + W (RAD)
C    Y(7)   = E, ECCENTRICITY
C    Y(8)   = I, INCLINATION (RAD)  
C    Y(9)   = NODE, LONGITUDE OF ASCENDING NODE (RAD)
C    Y(10)  = W, ARGUMENT OF PERIAPSIS (RAD)
C    Y(11)  = MA, MEAN ANOMALY (RAD)
C    Y(12)  = TA, TRUE ANOMALY (RAD)
C    Y(13)  = EA, ECCENTRIC ANOMALY (RAD)
C    Y(14)  = L, TRUE LONGITUDE (RAD)
C    Y(15)  = F, ECCENTRIC LONGITUDE (RAD)
C    Y(16)  = R, RADIUS (KM)
C    Y(17)  = V, VELOCITY (KM/SEC)
C    Y(18)  = ORBITAL PERIOD (SEC)
C  CALL SUBROUTINES
C    NONE
C  REFERENCES
C    JPL EM 312/87-153, 20 APRIL 1987
C    AIAA #75-9, ON THE FORMULATION OF THE GRAVITATIONAL POTENTIAL 
C    IN TERMS OF EQUINOCTIAL VARIABLES, CEFOLA AND BROUCKE, 1975
C  ANALYSIS
C    JOHNNY H. KWOK - JPL  
C  PROGRAMMER
C    JOHNNY H. KWOK - JPL  
C  PROGRAM MODIFICATIONS 
C    NONE  
C  COMMENTS  
C    THIS PROGRAM RECYCLES STORAGE DUMMY VARIABLES.  SO BE CAREFUL
C    WHEN MODIFYING THIS ROUTINE.  THIS PROGRAM DOES NOT WORK FOR 
C    PARABOLIC OR HYPERBOLIC OR RETROGRADE EQUATORIAL ORBITS.  
C    IF EITHER INCLINATION OR ECCENTRICITY IS NEAR ZERO, THE   
C    CLASSICAL ORBITAL ELEMENTS MAY NOT BE DEFINED AND THE USER
C    SHOULD INTERPRET ACCORDINGLY. 
C*/
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)   
      DIMENSION X(6),Y(18)
      DIMENSION V1(3),V2(3),V3(3)   
      DATA ZERO,ONE,TWO/0.D0,1.D0,2.D0/
      DATA TPI/6.283185307179586D0/
      character*80 sccsid
      data sccsid /'@(#)eqnox.f	1.3 96/04/09 22:51:47\0'/
C *** D2 = V2   
C *** D3 = RDV  
      Y(16)=DSQRT(X(1)**2+X(2)**2+X(3)**2)  
      D2=X(4)**2+X(5)**2+X(6)**2
      D3=X(1)*X(4)+X(2)*X(5)+X(3)*X(6)  
      Y(17)=DSQRT(D2)   
      Y(1)=ONE/(TWO/Y(16)-D2/GM)
      Y(18)=TPI/DSQRT(GM/Y(1)**3)
C *** D4 = V2/GM - 1/R  
C *** D5 = RDV/GM   
      D4=D2/GM-ONE/Y(16)
      D5=D3/GM  
C *** V1 = ECCENTRICITY VECTOR  
      DO 10 I=1,3   
   10 V1(I)=D4*X(I)-D5*X(I+3)   
C *** V2 = VECTOR W = R CROSS V, ANGULAR MOMENTUM VECTOR
      V2(1)=X(2)*X(6)-X(3)*X(5) 
      V2(2)=X(3)*X(4)-X(1)*X(6) 
      V2(3)=X(1)*X(5)-X(2)*X(4) 
C *** D1 = MAGNITUDE OF W   
      D1=DSQRT(V2(1)**2+V2(2)**2+V2(3)**2)  
      DO 20 I=1,3   
   20 V2(I)=V2(I)/D1
C *** D1 = 1 + WZ   
      D1 = ONE+V2(3)
      Y(4)=V2(1)/D1 
      Y(5)=-V2(2)/D1
C *** D1 = P**2 
C *** D2 = Q**2 
      D1=Y(4)*Y(4)  
      D2=Y(5)*Y(5)  
C *** D3
C *** D4
      D3=ONE+D1+D2  
      D4=TWO*Y(4)*Y(5)  
C *** V2 = UNIT VECTOR G
C *** V3 = UNIT VECTOR F
      V2(1)=D4  
      V2(2)=ONE+D1-D2   
      V2(3)=TWO*Y(5)
      V3(1)=ONE-D1+D2   
      V3(2)=D4  
      V3(3)=-TWO*Y(4)   
      DO 30 I=1,3   
      V2(I)=V2(I)/D3
   30 V3(I)=V3(I)/D3
      Y(2)=V1(1)*V2(1)+V1(2)*V2(2)+V1(3)*V2(3)  
      Y(3)=V1(1)*V3(1)+V1(2)*V3(2)+V1(3)*V3(3)  
C *** D1 = X1   
C *** D2 = Y1   
      D1=X(1)*V3(1)+X(2)*V3(2)+X(3)*V3(3)   
      D2=X(1)*V2(1)+X(2)*V2(2)+X(3)*V2(3)
      Y(14)=DATAN2(D2,D1)
C *** D3 = DSQRT(1-H*H-K*K) 
C *** D4 = A * DSQRT(1-H*H-K*K) 
C *** D5 = BETA = 1/(1+A*DSQRT(1-H*H-K*K))  
      D3=DSQRT(ONE-Y(2)**2-Y(3)**2) 
      D4=Y(1)*D3
      D5=ONE/(ONE+D3)   
C *** D6 = COS(F)   
C *** D7 = SIN(F)   
      D6=Y(3)+((ONE-Y(3)*Y(3)*D5)*D1-Y(2)*Y(3)*D5*D2)/D4
      D7=Y(2)+((ONE-Y(2)*Y(2)*D5)*D2-Y(2)*Y(3)*D5*D1)/D4
      Y(15)=DATAN2(D7,D6)
      Y(6)=Y(15)-Y(3)*D7+Y(2)*D6
      Y(7)=DSQRT(Y(2)*Y(2)+Y(3)*Y(3))   
      Y(8)=TWO*DATAN(DSQRT(Y(4)*Y(4)+Y(5)*Y(5)))
      IF (Y(8).NE.ZERO) THEN
         Y(9)=DATAN2(Y(4),Y(5)) 
      ELSE  
         Y(9)=ZERO  
      ENDIF 
      IF (Y(7).NE.ZERO) THEN
         Y(10)=DATAN2(Y(2),Y(3))-Y(9)   
      ELSE  
         Y(10)=ZERO 
      ENDIF
      Y(11)=Y(6)-Y(9)-Y(10)
      Y(12)=Y(14)-Y(9)-Y(10)
      Y(13)=Y(15)-Y(9)-Y(10)
C
C *** MAKE ALL ANGLES BETWEEN ZERO AND TWO PI
C
      Y(6)=DMOD(Y(6)+TWO*TPI,TPI)
      DO 40 I=8,15
   40 Y(I)=DMOD(Y(I)+TWO*TPI,TPI)
      RETURN
      END   
