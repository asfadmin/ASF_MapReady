c SccsId = @(#)sinc.f	2.41 3/24/98

       
	function sinc(a,pi)

        implicit none

        character*128 SccsId_sinc
        data SccsId_sinc
     +  /'@(#)PPsinc.f:2.41'/


        real a,sinc
        real pi,b

        if(a.eq.0) then
        sinc = 1.
        else
        b = a*pi
        sinc = sin(b)/b
        end if

        return
        end
