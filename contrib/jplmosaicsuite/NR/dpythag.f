      FUNCTION pythag(a,b)
      DOUBLE PRECISION a,b,pythag
      DOUBLE PRECISION absa,absb
      absa=abs(a)
      absb=abs(b)
      if(absa.gt.absb)then
        pythag=absa*sqrt(1.d0+(absb/absa)**2)
      else
        if(absb.eq.0.d0)then
          pythag=0.d0
        else
          pythag=absb*sqrt(1.d0+(absa/absb)**2)
        endif
      endif
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software 0!5,.
