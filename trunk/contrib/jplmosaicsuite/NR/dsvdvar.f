      SUBROUTINE svdvar(v,ma,np,w,cvm,ncvm)
      INTEGER ma,ncvm,np,MMAX
      DOUBLE PRECISION cvm(ncvm,ncvm),v(np,np),w(np)
      PARAMETER (MMAX=20)
      INTEGER i,j,k
      DOUBLE PRECISION sum,wti(MMAX)
      do 11 i=1,ma
        wti(i)=0.d0
        if(w(i).ne.0.d0) wti(i)=1.d0/(w(i)*w(i))
11    continue
      do 14 i=1,ma
        do 13 j=1,i
          sum=0.d0
          do 12 k=1,ma
            sum=sum+v(i,k)*v(j,k)*wti(k)
12        continue
          cvm(i,j)=sum
          cvm(j,i)=sum
13      continue
14    continue
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software 0!5,.
