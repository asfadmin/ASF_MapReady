c SccsId = @(#)cfft.f	2.41 3/24/98
c call IBM's fft
      subroutine cfft(v1,inc,nsize,iflag)
      implicit   none

        character*128 SccsId_cfft
        data SccsId_cfft
     +  /'@(#)PPcfft.f:2.41'/

	include '/usr/lpp/ppe.poe/include/mpif.h'
        
        integer ierror

      integer*4  inc,nsize,iflag
      complex*8  v1(*)

      real*4     scale
      integer*4  naux1,naux2,naux3,naux4
      parameter  (naux1=30000,naux2=30000,naux3=30000,naux4=30000)
      real*8     aux1(naux1),aux2(naux2),aux3(naux3),aux4(naux4)
      common /twid/ aux1,aux2,aux3,aux4

      if(inc.ne.1)then
         print*,'Error in implementation of cfft'
         call mpi_abort(mpi_comm_world,22,ierror)
         call exit(22)
      end if
c     call init_fft(nsize)
      if(iflag .eq. 0) then
         scale = 1.0
         call scft(1,v1,1,nsize,v1,1,nsize,
     .             nsize,1,1,scale,aux1,naux1,aux2,naux2)
         scale = 1.0/float(nsize)
         call scft(1,v1,1,nsize,v1,1,nsize,
     .             nsize,1,-1,scale,aux3,naux3,aux4,naux4)
      else if(iflag .eq. 1) then
         scale = 1.0
         call scft(0,v1,1,nsize,v1,1,nsize,
     .             nsize,1,1,scale,aux1,naux1,aux2,naux2)
      else if(iflag .eq. -1) then
         scale = 1.0/float(nsize)
         call scft(0,v1,1,nsize,v1,1,nsize,
     .             nsize,1,-1,scale,aux3,naux3,aux4,naux4)
      end if

      return
      end


