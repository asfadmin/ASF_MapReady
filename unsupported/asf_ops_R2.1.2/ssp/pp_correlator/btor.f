c SccsId = @(#)btor.f	2.41 3/24/98
                               

        function btor(a)

        implicit none

        character*128 SccsId_btor
        data SccsId_btor
     +  /'@(#)PPbtor.f:2.41'/


	byte a
	real btor
	integer btoi

	btoi = a
	if(btoi.lt.0) btoi = btoi + 256
	btor = btoi

	return
	end
