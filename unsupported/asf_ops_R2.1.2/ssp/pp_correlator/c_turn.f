c SccsId = @(#)c_turn.f	2.41 3/24/98



        subroutine c_turn(data,isize)

        implicit none

        character*128 SccsId_c_turn
        data SccsId_c_turn
     +  /'@(#)PPc_turn.f:2.41'/



        integer isize
        complex data(isize,isize),temp

        integer i,j

        do i = 2, isize
        do j = 1, i-1
        temp = data(j,i)
        data(j,i) = data(i,j)
        data(i,j) = temp
        end do
        end do

        return
        end
