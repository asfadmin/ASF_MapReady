      subroutine Dop_fit(fd0,fdd,n,ngroup,dr,fd,prf)
      
      implicit   none
      integer*4  n,ngroup
      real*8     dr
      real*8     fd0, fdd, fd(n), prf

c Local variables
      integer   npoints,iorder
      integer   i,j,inum_avg
      real*8    x(2048),y(2048),wgt(2048)
      real*8    coeff(3),performance
      real*8    run_avg

      integer chan, dummy
      character*32 filename
      integer initdk

c Set the order to be linear
      iorder = 1
      npoints = n
      do i=1,npoints
         wgt(i) = 1.0
         x(i) = ((i-0.5)*ngroup -0.5)*dr
         y(i) = fd(i)
      end do
      
c      filename = '/home/tmpdisk/fd.data.33256'
c      chan = initdk(dummy, filename)
c      call iowrit(chan,fd,n*8)
c      call closedk(dummy,chan)

      do i=2,npoints
           run_avg = 0.0
           inum_avg = 0
           do j=i-1,max(1,i-5),-1
             run_avg = run_avg+y(i)
             inum_avg = inum_avg + 1
           end do
           run_avg = run_avg/inum_avg
           y(i) = y(i) - prf*nint((y(i)-run_avg)/prf)
      end do 

c      print*,'x =',(x(i),i=1,npoints)
c      print*,'y =',(y(i),i=1,npoints)
      call fit_quad(x,y,wgt,npoints,iorder,coeff,performance)
      fd0 = coeff(1)
      fdd = coeff(2)

      print*,' the fit coef: dc=',fd0,' linear=',fdd
      print*,' performance fit=',performance
      return
      end


