      DOUBLE PRECISION FUNCTION DEPSLN(I,J,K)
C/*   DOUBLE PRECISION FUNCTION DEPSLN(I,J,K) ----------
C
C     DEPSLN(I,J,K)=DBLE(FLOAT(EPSILON(I,J,K)))
C*/
      character*80 sccsid
      data sccsid /'@(#)depsln.f	1.3 96/04/09 22:51:42\0'/
      IF (I*J*K-6) 2,5,2                                            
    5 IF (J-1-MOD(I,3)) 7,4,7                                    
    7 DEPSLN=-1.D0                                                    
      RETURN                                                        
    4 DEPSLN=1.D0                                                     
      RETURN                                                        
    2 DEPSLN=0.D0                                                     
      RETURN                                                        
      END                                                           
