c SccsId = @(#)frate_parameter.f	2.41 3/24/98


	subroutine frate_parameter(fr0,frd,dr,pbw,prf,n,ngroup,
     1	            peak,fr,r_2_r1st)

        implicit none

        character*128 SccsId_frate_parameter
        data SccsId_frate_parameter
     +  /'@(#)PPfrate_parameter.f:2.41'/



	real*8 peak(n),fr(n),r_2_r1st(n)
	real*8 tp
	real*8 fr0,frd,dr,pbw,prf
	real*8 peak_sum, peak_avg

	integer i,ngroup,n

	tp = pbw/fr0

	peak_sum = 0.0d0
	do i = 1,n 
	peak_sum = peak_sum + peak(i)
	end do

	peak_avg = peak_sum/n

	fr0 = fr0 + peak_avg*4/prf * fr0/(.75*tp)

	return 
	end
