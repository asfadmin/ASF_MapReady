c SccsId = @(#)cvmul.f	2.41 3/24/98


        subroutine cvmul(a,b,n,iflag)

        implicit none

        character*128 SccsId_cvmul
        data SccsId_cvmul
     +  /'@(#)PPcvmul.f:2.41'/



        complex a(n), b(n)
        integer iflag

        integer i,n

        if(iflag.eq.1) then
          do i = 1, n
          a(i) = a(i) * b(i)
          end do
        else
          do i = 1, n
          a(i) = a(i) * conjg(b(i))
          end do
        end if

        return
        end
