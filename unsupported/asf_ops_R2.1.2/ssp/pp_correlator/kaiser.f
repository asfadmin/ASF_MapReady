c SccsId = @(#)kaiser.f	2.41 3/24/98
      subroutine kaiser(a,n,alpha)

        implicit none

        character*128 SccsId_kaiser
        data SccsId_kaiser
     +  /'@(#)PPkaiser.f:2.41'/


c Usage: Genrate the KAISER weight 
c pi*alpha=2.9 -> alpha=0.9230986856 in time domain for azimuth
c pi*alpha=2.8 -> alpha=0.8912675965 in time domain for range
c

        complex*8 a(n)
        real pi,pialpha,bsli0,bsli,ci,vral,ALPHA,PIALPA
        integer nw,nw2,n,i

        pi=3.1415926

        pialpa=pi*alpha
        call bessel(pialpa,bsli0)
c
        nw=n
        nw2=nw/2

        do i=1,nw2
           ci=(1.-((i-.5)/nw*2)**2)**.5*pialpa
           call bessel(ci,bsli)
           vral=bsli/bsli0
           a(i)=cmplx(vral,0.0)
           a(nw-i+1)=a(i)
        end do
        end    
c************  bessel  ************************************************

        subroutine bessel(ai,ao)
        real ai,ao,at
        integer k

        ao=1.
        at=1.
        do k=1,20
          at=at*ai/2/k
          ao=ao+at**2
        end do
        return
        end


      subroutine irgary(a,nsml,nbig)
      integer nsml,nbig,nsml2,nsml21
      complex a(nbig)
      
      nsml2 = nsml/2
      nsml21 = nsml2+1
      do k = 1, nsml2
         a(nbig-nsml+k+nsml2) = a(k+nsml2)
      end do
      
      do k = nsml21,nbig-nsml2
         a(k) = cmplx(0.0,0.0)
      end do

      return 
      end
      
