      program  ampscheby


c****************************************************************
c**     
c**   FILE NAME: diffcheby.f
c**     
c**   DATE WRITTEN: //97
c**     
c**   PROGRAMMER: Scott Hensley
c**     
c**   FUNCTIONAL DESCRIPTION: This program will correct the a file by a 
c**   chebyshev fit to a correction set of data.  
c**     
c**   ROUTINES CALLED:
c**     
c**   NOTES: 
c**     
c**   UPDATE LOG:
c**
c**   Date Changed        Reason Changed                  CR # and Version #
c**   ------------       ----------------                 -----------------
c**     
c*****************************************************************


      implicit none


c     INPUT VARIABLES:
        
c     OUTPUT VARIABLES:


c     LOCAL VARIABLES:
      character*100 a_data,a_out,a_corr,a_string
      real*4 r_val,r_xmax,r_xmin,r_cfit(75),r_j,r_data(2*4000), r_cnt(4000)
      integer i_samples,i_deg,i,j,i_ss,i_es,i_sl,i_el,i_skl,iargc


c     COMMON BLOCKS:
      real*4 r_xch(4000),r_ych(4000)
      integer i_npts
      common /fred/ r_xch,r_ych,i_npts


c     EQUIVALENCE STATEMENTS:


c     DATA STATEMENTS:


C     FUNCTION STATEMENTS:
      real*4 chebev,func


c     PROCESSING STEPS:


      write(6,*) ' '
      write(6,*) ' << Amplitude Chebyshev >> '
      write(6,*) ' '


      if(iargc() .lt. 9)then
         write(6,*) 'Usage: ampfcheby data_file i_samples ss es sl el skl deg out_file'
         write(6,*) ' '
         stop
      endif


      call getarg(1,a_data)
      call getarg(2,a_string)
      read(a_string,*) i_samples
      call getarg(3,a_string)
      read(a_string,*) i_ss
      call getarg(4,a_string)
      read(a_string,*) i_es
      call getarg(5,a_string)
      read(a_string,*) i_sl
      call getarg(6,a_string)
      read(a_string,*) i_el
      call getarg(7,a_string)
      read(a_string,*) i_skl
      call getarg(8,a_string)
      read(a_string,*) i_deg
      call getarg(9,a_out)


      open(11,file=a_data,form='unformatted',access='direct',recl=4*i_samples)
      open(13,file=a_out,form='unformatted',access='direct',recl=4*i_samples)


c     read in data a do chebyshev fit to data


      r_xmin = i_ss
      r_xmax = i_es
      i_deg = 60
      i_npts = 0
      do i=1,4000
         r_cnt(i) = 0
         r_ych(i) = 0
      end do


      do i=i_sl,i_el,i_skl


         read(11,rec=i) (r_data(j),j=1,i_samples)
         i_npts = i_npts + 1


         do j=i_ss,i_es
            r_xch(j) = j
            if(r_data(j) .gt. 1.0)then
               r_cnt(j) = r_cnt(j) + 1
               r_ych(j) = r_ych(j) + r_data(j)
            endif
         enddo


      enddo


      do j=i_ss,i_es
         r_ych(j) = r_ych(j)/max(1.,r_cnt(j))
         write(19,*) j, r_ych(j)
      enddo
      do i=1,i_ss - 1
         r_xch(i) = i
         r_ych(i) = r_ych(i_ss)
      enddo
      i_ss = 1
      do i=i_es+1,i_samples
         r_xch(i) = i
         r_ych(i) = r_ych(i_es)
      enddo
      i_es = i_samples



      i_npts = i_es - i_ss + 1
      r_xmin = .9999 
      r_xmax = i_samples
      call chebft(r_xmin,r_xmax,r_cfit,i_deg)
      
      do i=i_ss,i_es            ! 1,i_npts
         r_val =  chebev(r_xmin,r_xmax,r_cfit,i_deg,r_xch(i))
         write(18,'(3(e11.4,x))') r_xch(i),r_val,r_ych(i)
      enddo


      i=1
      do while(1 .eq. 1)


         read(11,rec=i,err=998) (r_data(j),j=1,i_samples)


         do j=1,i_samples
            r_j = float(j)
            r_val =  chebev(r_xmin,r_xmax,r_cfit,i_deg,r_j)
            if(r_val .ge. 1.0)then
               r_data(j) = r_data(j)/r_val
            endif
         enddo


         write(13,rec=i) (r_data(j),j=1,i_samples)


         i = i + 1


         if(mod(i,250) .eq. 0)then
            write(6,*) 'At line: ',i
         endif


      enddo
 998  continue
        
      end  


c*********************************************************************************


      real*4 function func(x)


      real*4 x,r_f,r_omf
      integer i_loc


      real*4 r_xch(4000),r_ych(4000)
      integer i_npts
      common /fred/ r_xch,r_ych,i_npts
      
      call hunt(r_xch,i_npts,x,i_loc)
      i_loc = min(max(1,i_loc),i_npts-1)


      r_f = (x - r_xch(i_loc))/(r_xch(i_loc+1)-r_xch(i_loc))
      r_omf = 1.d0 - r_f


      if(r_f .gt. 1 .or. r_f .lt. 0)then
         func = r_ych(i_loc)
         return
      endif


      func = r_omf*r_ych(i_loc) + r_f*r_ych(i_loc+1)
      
      end


      SUBROUTINE chebft(a,b,c,n)
      INTEGER n,NMAX
      REAL a,b,c(*),func,PI
      EXTERNAL func
      PARAMETER (NMAX=75, PI=3.141592653589793d0)
      INTEGER j,k
      REAL bma,bpa,fac,y,f(NMAX)
      DOUBLE PRECISION sum


      bma=0.5*(b-a)
      bpa=0.5*(b+a)
      do 11 k=1,n
        y=cos(PI*(k-0.5)/n)
        f(k)=func(y*bma+bpa)
11    continue


      fac=2./n
      do 13 j=1,n
        sum=0.d0
        do 12 k=1,n
          sum=sum+f(k)*cos((PI*(j-1))*((k-0.5d0)/n))
12      continue
        c(j)=fac*sum
13    continue
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software $23#1yR.3Z9.



      FUNCTION chebev(a,b,c,m,x)
      INTEGER m
      REAL chebev,a,b,x,c(m)
      INTEGER j
      REAL d,dd,sv,y,y2
      if ((x-a)*(x-b).gt.0.) pause 'x not in range in chebev'
      d=0.
      dd=0.
      y=(2.*x-a-b)/(b-a)
      y2=2.*y
      do 11 j=m,2,-1
        sv=d
        d=y2*d-dd+c(j)
        dd=sv
11    continue
      chebev=y*d-dd+0.5*c(1)
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software $23#1yR.3Z9.



      SUBROUTINE hunt(xx,n,x,jlo)
      INTEGER jlo,n
      REAL x,xx(n)
      INTEGER inc,jhi,jm
      LOGICAL ascnd
      ascnd=xx(n).gt.xx(1)
      if(jlo.le.0.or.jlo.gt.n)then
        jlo=0
        jhi=n+1
        goto 3
      endif
      inc=1
      if(x.ge.xx(jlo).eqv.ascnd)then
1       jhi=jlo+inc
        if(jhi.gt.n)then
          jhi=n+1
        else if(x.ge.xx(jhi).eqv.ascnd)then
          jlo=jhi
          inc=inc+inc
          goto 1
        endif
      else
        jhi=jlo
2       jlo=jhi-inc
        if(jlo.lt.1)then
          jlo=0
        else if(x.lt.xx(jlo).eqv.ascnd)then
          jhi=jlo
          inc=inc+inc
          goto 2
        endif
      endif
3     if(jhi-jlo.eq.1)return
      jm=(jhi+jlo)/2
      if(x.gt.xx(jm).eqv.ascnd)then
        jlo=jm
      else
        jhi=jm
      endif
      goto 3
      END
C  (C) Copr. 1986-92 Numerical Recipes Software $23#1yR.3Z9.
