c SccsId = @(#)dop_ref.f	2.41 3/24/98

	subroutine dop_ref(i_dop_mode,fd0,fdd,r,fd_const,r_1st,fd_ref)

        implicit none

        character*128 SccsId_dop_ref
        data SccsId_dop_ref
     +  /'@(#)PPdop_ref.f:2.41'/



	integer i_dop_mode
	real*8 fd0,fdd,r,fd_const,r_1st,fd_ref

	if(i_dop_mode.eq.1) fd_ref = 0.
	if(i_dop_mode.eq.2) fd_ref = fd_const
	if(i_dop_mode.eq.3) fd_ref = (r-r_1st)*fdd + fd0

	return
	end
