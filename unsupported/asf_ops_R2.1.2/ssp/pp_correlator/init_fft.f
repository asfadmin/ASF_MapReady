c SccsId = @(#)init_fft.f	2.41 3/24/98
c call IBM's fft routine
      subroutine init_fft(nsize)
      implicit   none

        character*128 SccsId_init_fft
        data SccsId_init_fft
     +  /'@(#)PPinit_fft.f:2.41'/

      integer*4  nsize

      real*4     scale
      complex*8  v1(1)
      integer*4  naux1,naux2,naux3,naux4
      parameter  (naux1=30000,naux2=30000,naux3=30000,naux4=30000)
      real*8     aux1(naux1),aux2(naux2),aux3(naux3),aux4(naux4)
      common /twid/ aux1,aux2,aux3,aux4

         scale = 1.0
         call scft(1,v1,1,nsize,v1,1,nsize,
     .             nsize,1,1,scale,aux1,naux1,aux2,naux2)
         scale = 1.0/float(nsize)
         call scft(1,v1,1,nsize,v1,1,nsize,
     .             nsize,1,-1,scale,aux3,naux3,aux4,naux4)

      return
      end

