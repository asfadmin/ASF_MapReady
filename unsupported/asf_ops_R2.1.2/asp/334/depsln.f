C Alaska SAR Processor (ASP) %W% %E% %U%
      DOUBLE PRECISION FUNCTION DEPSLN(I,J,K)
C/*   DOUBLE PRECISION FUNCTION DEPSLN(I,J,K) ----------
C
C     DEPSLN(I,J,K)=DBLE(FLOAT(EPSILON(I,J,K)))
C*/
      IF (I*J*K-6) 2,5,2                                            
    5 IF (J-1-MOD(I,3)) 7,4,7                                    
    7 DEPSLN=-1.D0                                                    
      RETURN                                                        
    4 DEPSLN=1.D0                                                     
      RETURN                                                        
    2 DEPSLN=0.D0                                                     
      RETURN                                                        
      END                                                           
