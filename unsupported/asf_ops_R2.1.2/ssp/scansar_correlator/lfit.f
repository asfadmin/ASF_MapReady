c SccsId = @(#)lfit.f	2.41 3/24/98
c ===================================================================
c ===================================================================
      subroutine smooth_out(x,y,npoints,iorder)
c
c
c Functions:
c   Fit a 1-d vector with npoints values to a fit (order is based
c   on the iorder). After that it will replace any points, which are
c   2 sigma away from the mean, with the value of the fit curve.
c
c Calling sequence
c        fit_quad 
c        replace
c
c External calls:
c
c Inputs:
c   x            Input x-axis vector
c   y            Input y-axis vector
c   npoints      Number of points to be fitted
c   iorder       Order to fit equation (1: linear, 2: quadratic)
c 
c Processing
c   Given a set of points x[1....npoints], y[1....npoints] , fit the
c   data points by the curve whose order is iorder. After computing
c   the mean and deviation, the values of any points, which are 2 sigma
c   away from the mean, are replaced by the new fitted value.
c
c   Assume that iorder will be less than 10
c
c Outputs:
c   y            Update y-axis vector
c
c ====================================================================
c
      implicit  none

c Include File Declarations

c Constants Declarations

c Argument Declarations
       integer*4  npoints, iorder
       real*8     x(npoints),y(npoints)

c Local Declarations
       integer*4  i,n_outliers
       real*8     coeff(11), performance, mean, sigma
       real*8     wgt(8192)

c Code
       do i=1,npoints
          wgt(i) = 1.0
       end do

       do i=1,iorder+1
          coeff(i) = 0.0
       end do

c Fit the curves
       call fit_quad(x,y,wgt,npoints,iorder,coeff,performance)

c Replace the points
       call replace(x,y,npoints,iorder,coeff,mean,sigma,n_outliers)

c Print out the values if wanted
c      print*,' the mean=',mean,' the sigma=',sigma
c      print*,' number of points to be replaced=',n_outliers

       return 
       end
c $Id$
c ======================================================================
c ======================================================================
c
      subroutine fit_quad(x,y,wgt,npoints,iorder,coeff,performance)
c
c
c Function:
c   Fit a 1-d vector with npoints values to a linear fit (order is
c   based on iorder). It will return 3 coefficients with the performance
c   to be measured.
c
c Calling sequence:
c
c External calls:
c
c
c Inputs:
c   x           Input x-axis vector
c   y           Input y-axis vector
c   wgt         Input weighting for each element 
c   npoints     Number of points to be fitted.
c   iorder      Order of fit equation (1: linear, 2: quadratic)
c
c Processing:
c   Given a set of points x[1...npoints], y[1...npoints] with individual
c   deviations given by wgt[1...npoints], use chisq miniminzation to 
c   determine the coefficient coeff[1...iorder] of the fitting function
c   y=sum(a_i*afunc_i(x)).  
c   The program will return values for the coeff fit parameters and
c   performance ( chisq ).
c
c   Assume that iorder will be less than 10
c
c Outputs:
c   coeff       Coefficient vector for the fit function.
c   performance The chi-squared value from the result fit function. 
c
c =========================================================================
c
      implicit   none

c Include File Declarations

c Constant Declarations

c Argument Declarations
      integer*4  npoints,iorder
      real*8     x(npoints),y(npoints),wgt(npoints)
c     real*8     coeff(iorder+1),performance
      real*8     coeff(3),performance

c Local Declaration 
      integer*4  ma,mfit,ncvm,i,incvm
      parameter  (ncvm=10)
      real*8     chisq,covar(ncvm,ncvm),a(ncvm),lista(ncvm)

c Code
      do i=1,3
         coeff(i) = 0.0
      end do
d     print*,'Welcome to the fit routine with order of ',iorder
      incvm = ncvm
      ma = iorder+1
      mfit = ma
      do i=1,ma
         a(i) = 0.0
      end do
      do i=1,ma
         lista(i) = i
      end  do

      call lfit(x,y,wgt,npoints,a,ma,lista,mfit,covar,incvm,chisq)
      do i=1,ma
         coeff(i) = a(i)
      end do
      performance = chisq
d     do i=1,ma
d        print*,' the variance of output coeff',i,'(value=',
d    1            a(i),') is',covar(i,i)
d     end do
d     print*,' CHI-SQUARED is =',chisq

c End of subroutine
      return
      end      



c ======================================================================
c ======================================================================
c
      subroutine lfit(x,y,sig,ndata,a,ma,lista,mfit,covar,ncvm,
     1                 chisq)
c
c
c Function:
c  this routine will be used as a generalized least squares.
c
c Calling sequence:
c
c External calls:
c
c
c Inputs:
c   x           Input x-axis vector
c   y           Input y-axis vector
c   sig         Input weighting for each element 
c   ndata       Number of points to be fitted.
c   iorder      Order of fit equation (1: linear, 2: quadratic)
c
c Processing:
c
c
c  It is obtained from Numerical Recipes in FORTRAN,Chapter 14.3,pg.513.
c  The following comment is taken from the text:
c 
c  Given a set of NDATA points X(i),Y(i) with individual standard 
c  deviation SIG(i), use chi-squared minimization to determine MFIT of
c  MA coefficients A of a function that depends linearly on A, 
c  y=Sum(A(i)*AFUNC(x)). The array LISTA renumbers the parameters so that
c  the first MFIT elements are held fixed at their input value. The 
c  program returns values for the MA fit parameters A, chi-squared,
c  chi2 and the covariance matrix COVAR(I,J). NCVM is the physical
c  dimension of COVAR(NCVM,NCVM) in the calling routine. The user
c  supplies a subroutine FUNCS(X,AFUNC,MA) that returns the MA basis
c  function evaluated at x=X in the array AFUNC.
c
c
c Outputs:
c  chisq       the CHI-SQUARED value of the fit.
c
c =========================================================================
c =========================================================================
c
      implicit   none
c Include File Declarations

c Constant Declarations
      integer*4  mmax
c Set to the maximum number of coefficients MA
      parameter  (mmax=50)

c Argument Declarations
      integer*4  ndata,ma,mfit,ncvm
      real*8     x(ndata),y(ndata),sig(ndata),a(ma),lista(mfit)
      real*8     covar(ncvm,ncvm),chisq

c Local Declaration 
      integer*4  kk,j,k,ihit,i
      real*8     ym,sig2i,wt,sum
      real*8     beta(mmax,1),afunc(mmax)

      
c Code:                
      kk = mfit + 1
      do 12 j=1,ma
         ihit = 0
         do 11 k=1,mfit
            if(lista(k).eq.j) ihit = ihit+1
11       continue
         if(ihit.eq.0)then
            lista(kk) = j
            kk = kk+1
         else if(ihit.gt.1) then
d           print*,' Improper set in Lista'
            call exit(1)
         end if
12    continue
      if(kk.ne.(ma+1)) then
d        print*,' Improper set in Lista (kk.ne.ma+1)'
         call exit(1)
      end if
c Initialize the symmetric maxtrix
      do 14 j=1,mfit
         do 13 k=1,mfit
            covar(j,k) = 0.0
13       continue
         beta(j,1) = 0.0
14    continue
c Loop over data to accumulate coefficients of the normal equations
      do 18 i=1,ndata
         call funcs(x(i),afunc,ma)
         ym = y(i)
c Subtract off dependences on known pieces of the fitting function
         if(mfit.lt.ma)then
            do 15 j=mfit+1,ma
               ym = ym-a(lista(j))*afunc(lista(j))
15          continue
         end if
         sig2i = 1./sig(i)**2
         do 17 j=1,mfit
            wt = afunc(lista(j))*sig2i
            do 16 k=1,j
               covar(j,k) = covar(j,k)+wt*afunc(lista(k))
16          continue
            beta(j,1) = beta(j,1)+ym*wt
17       continue
18    continue
c Fill in above the diagonal from symmetry
      if(mfit.gt.1)then
         do 21 j=2,mfit
            do 19 k=1,j-1
               covar(k,j)=covar(j,k)
19          continue
21       continue
      end if
c Matrix solution
      call gaussj(covar,mfit,ncvm,beta,1,1)
c Partition solution to approriate coefficients A
      do 22 j=1,mfit
         a(lista(j)) = beta(j,1)
22    continue
c Evaluate CHI2 of the fit
      chisq = 0.
      do 24 i=1,ndata
         call funcs(x(i),afunc,ma)
         sum = 0.
         do 23 j=1,ma
            sum = sum+a(j)*afunc(j)
23       continue
         chisq = chisq+((y(i)-sum)/sig(i))**2
24    continue
c Sort covariance matrix to true order of fitting coeffients
      call covsrt(covar,ncvm,ma,lista,mfit)

c End of subroutine
      return
      end
     

c ======================================================================
c ======================================================================
c
      subroutine covsrt(covar,ncvm,ma,lista,mfit)
c
c
c Function:
c
c Calling sequence:
c
c External calls:
c
c Inputs/Outputs:
c    covar      Array of ncvm by ncvm values, covariance maxtrix
c    ncvm       Dimension of covar array
c    ma         the order of the fit function
c    lista                    
c    mfit                      
c
c Processing:
c Given the covariance matrix COVAR of a fit for MFIT of MA total 
c parameters, and their ordering LISTA(I), repack the covariance 
c matrix to the true order of the parameters. Elements associated 
c with fixed parameters will be zero. NCVM is the physical dimension 
c of COVAR.
c
c
c
c Outputs:
c
c =========================================================================
c =========================================================================
 
      implicit   none

c Include File Declarations

c Constant Declarations

c Argument Declarations
      integer*4  ncvm,ma,mfit
      real*8     covar(ncvm,ncvm),lista(mfit)

c Local Declarations
      integer*4  i,j
      real*8     swap

c Code:
c Zero all elements below diagonal
      do 12 j=1,ma-1
         do 11 i=j+1,ma
            covar(i,j) = 0.0
11       continue
12    continue
c Repack off diagonal elements of fit into correct locations 
c below diagonal
      do 14 i=1,mfit-1
         do 13 j=i+1,mfit
            if(lista(j).gt.lista(i))then
               covar(lista(j),lista(i))=covar(i,j)
            else 
               covar(lista(i),lista(j))=covar(i,j)
            end if
13       continue
14    continue
c Temporarily store original diagonal elements in top row and zero
c the diagonal
      swap = covar(1,1)
      do 15 j=1,ma
         covar(1,j) = covar(j,j)
         covar(j,j) = 0.0
15    continue
      covar(lista(1),lista(1))=swap
c Now sort elements into proper order on diagonal
      do 16 j=2,mfit
         covar(lista(j),lista(j)) = covar(1,j)
16    continue
c Finally, fill in above diagonal by symmetry.
      do 18 j=2,ma
         do 17 i=1,j-1
            covar(i,j) = covar(j,i)
17       continue
18    continue

c End of subroutine
      return
      end


c ======================================================================
c ======================================================================
c
      subroutine gaussj(a,n,np,b,m,mp)
c
c
c Function:
c     Linear equation solution by Gauss-Jordan elimination
c
c Calling sequence:
c
c External calls:
c
c
c Inputs:
c
c Processing:
c
c
c Outputs:
c
c =========================================================================
c =========================================================================
      implicit   none

c Include File Declarations

c Constant Declarations
      integer*4  nmax
      parameter  (nmax=50)

c Argument Declarations
      integer*4  n,np,m,mp
      real*8     a(np,np),b(np,mp)

c Local Declarations
      real*8     big,dum,pivinv
      integer*4  j,i,k,l,ll,irow,icol
      real*8     ipiv(nmax),indxr(nmax),indxc(nmax)

c Code:
      if(n.gt.nmax)then
d        print*,' Sorry gaussj routine can not handle maxtrix',n
         call exit(1) 
      end if

      do 11 j=1,n
         ipiv(j) = 0
11    continue
      do 22 i=1,n
         big = 0.0
         do 13 j=1,n
            if(ipiv(j).ne.1) then
               do 12 k=1,n
                  if(ipiv(k).eq.0)then
                     if(abs(a(j,k)).ge.big)then
                        big=abs(a(j,k))
                        irow = j
                        icol =k
                     end if
                  else if(ipiv(k).gt.1) then
d                    print*,' Singular matrix'
                     call exit(1)
                  end if
12             continue
           end if
13      continue
        ipiv(icol) = ipiv(icol)+1
        if(irow.ne.icol)then
           do 14 l=1,n
              dum = a(irow,l)
              a(irow,l) = a(icol,l)
              a(icol,l) = dum
14         continue
           do 15 l=1,m
              dum = b(irow,l)
              b(irow,l) = b(icol,l)
              b(icol,l) = dum
15         continue
        end if
        indxr(i) = irow
        indxc(i) = icol
        if(a(icol,icol).eq.0) then
d          print*,' Singular matrix a(icol,icol)= 0'
           call exit(1)
        end if
        pivinv = 1.0/a(icol,icol)
        a(icol,icol) = 1.0
        do 16 l=1,n
           a(icol,l) =a(icol,l)*pivinv
16      continue
        do 17 l=1,m
           b(icol,l) = b(icol,l)*pivinv
17      continue
        do 21 ll=1,n
           if(ll.ne.icol)then
              dum = a(ll,icol)
              a(ll,icol) = 0.
              do 18 l=1,n
                 a(ll,l) = a(ll,l) - a(icol,l)*dum
18            continue
              do 19 l=1,m
                 b(ll,l) = b(ll,l) - b(icol,l)*dum
19            continue
           end if
21      continue
22    continue
      do 24 l=n,1,-1
         if(indxr(l).ne.indxc(l))then
            do 23 k=1,n
               dum = a(k,indxr(l))
               a(k,indxr(l)) = a(k,indxc(l))
               a(k,indxc(l)) = dum
23          continue
         end if
24    continue
c End of subroutine
      return
      end


c ======================================================================
c ======================================================================
c
      subroutine funcs(xvalue,afunc,iorder)
c
c
c Function:
c    Define the subroutine for the polynomial 
c
c Calling sequence:
c
c External calls:
c
c Inputs/Outputs:
c   xvalue      Input x value
c   afunc       Returned value in vector of iorder elements
c   iorder      Order of the function
c
c =========================================================================
c =========================================================================
      implicit   none
c Include File Declarations

c Constant Declarations

c Argument Declarations
      integer*4  iorder
      real*8     xvalue,afunc(iorder)

c Local Declarations
      integer*4  i
      real*8     xproduct

c Code:
      xproduct = 1.0
      afunc(1) = xproduct
      do i=2,iorder
         xproduct = xproduct*xvalue
         afunc(i) = xproduct
      end do
c End of subroutine
      return
      end
 

c =====================================================================
c =====================================================================
c
      subroutine replace(x,y,npoints,iorder,coeff,
     .                   mean,sigma,n_outliers)
c
c
c Function:
c
c Calling sequence:
c
c External calls:
c
c
c Inputs:
c   x           Input x-axis vector
c   y           Input y-axis vector
c   sig         Input weighting for each element 
c   ndata       Number of points to be fitted.
c   iorder      Order of fit equation (0: constant, 1: linear, 2: quadratic)
c
c Processing:
c   Calculate the mean and standard deviation of the differences.
c   Replace any data points which is 2*sigma from the estimation.
c
c Outputs:
c   y           Output y-axis vector
c   mean        Average of the differences
c   sigma       Standard deviation of the differences
c   n_outliers  Number of outliers in the fitting process
c ====================================================================
c ====================================================================
c
      implicit   none
c Include File Declarations

c Constants Declarations
      
c Argument Declarations
      integer*4    npoints, iorder, n_outliers
      real*8       x(npoints), y(npoints), coeff(iorder+1)
      real*8       mean, sigma

      
c Local Declarations
      integer*4    i, j
      real*8       diff, sum_diff, sum_2, xvalue, yvalue, threshold

c Find the mean and standard deviation
      sum_diff = 0.0
      sum_2    = 0.0
      do i=1,npoints
          xvalue = x(i)
          yvalue = 0.0
          do j=iorder+1,1,-1
             yvalue = yvalue * xvalue + coeff(j) 
          end do
          diff = y(i) - yvalue
          sum_diff = sum_diff + diff
          sum_2    = sum_2 + diff*diff
d         if(i.eq.2) then
d            print*,'xvalue=',xvalue,' yvalue=',yvalue,' y(i)=',y(i)
d            print*,' iorder=',iorder,' coeff=',
d    .               (coeff(j),j=iorder+1,1,-1)
d         end if
      end do
      mean      = sum_diff /  npoints 
      sigma     = sqrt ( sum_2 /  npoints  )
      threshold = 2.0 * sigma
c     threshold = 1.5 * sigma

d     print *,' mean , std=', mean, sigma

      n_outliers = 0
      do i=1,npoints
         xvalue = x(i)
         yvalue = 0.0
         do j=iorder+1,1,-1
            yvalue = yvalue * xvalue + coeff(j)
         end do
         if( abs(yvalue - y(i)) .gt. threshold) then
d           print*,'at',i,' replace the value from',y(i),' to',yvalue
            y(i) = yvalue
            n_outliers = n_outliers + 1
         end if
      end do
      return
      end

           
