c SccsId = @(#)cluter_focus.f	2.41 3/24/98


c*******************************************************
c*       clutterlock & autofocu written by m. jin      *
c*******************************************************

	subroutine cluter_focus(i_fo_skp,buff_in,rng_in,az_in,nrs,
     1		   pbw,prf,r_1st,fd0,fdd,fr0,frd,dr,df)

        implicit none

	include '/usr/lpp/ppe.poe/include/mpif.h'

        character*128 SccsId_cluter_focus
        data SccsId_cluter_focus
     +  /'@(#)PPcluter_focus.f:2.41'/


	integer ierror

	integer rng_in,az_in
	complex buff_in(rng_in,az_in)
	real*8 pbw,prf,r_1st,fd0,fdd,fr0,frd,dr,df
	complex pdf_buf(4096,128),wght(4096)
	real*8 fd(128), r_2_r1st(128)
	real*8 peak(128),fr(128)
	real*8 fdd_old
        real*8 run_avg

        integer n,iflag,ngroup1,i_fo_skp,nrs,ngroup
	integer i
        integer j,inum_avg
	
	ngroup = 64                     !32
	n = (rng_in-nrs)/ngroup		!n is the number of groups

	if(az_in .gt. 4096) then
	   print *,'cluter_focus: az_in > 4096, increase pdf_buf, wght'
	   call mpi_abort(mpi_comm_world,22,ierror)
           call exit(22)
        end if

	if(n .gt. 128) then
	   print *,'cluter_focus: n >128, increase pdf_buf, fd, r_2_r1st'
	   call mpi_abort(mpi_comm_world,22,ierror)
           call exit(22)
        end if

	if(n .lt. 1) then
	   print *,'cluter_focus: n < 1, something is wrong'
	   call mpi_abort(mpi_comm_world,22,ierror)
           call exit(22)
        end if

c** corner turn input data  *********************

c        call c_turn(buff_in,rng_in)

c** remove doppler centroid frequency ***********

	call rm_doppler(buff_in,rng_in,az_in,nrs,
     1	        	prf,r_1st,fd0,fdd,fr0,frd,dr)

c** azmuth fft, result used for cluter&focus ****

	iflag = 1
	call azfft_clutr(buff_in,rng_in,az_in,nrs,iflag)

c** get pdf by detection & averaging 64 lines ***

	call pdf(buff_in,rng_in,az_in,n,ngroup,
     1		pdf_buf,wght,fd,r_2_r1st)

c** get doppler centroids by li's algorithm ******

	call doppler_cnt(az_in,n,ngroup,fd0,fdd,df,dr,
     1		pdf_buf,wght,fd,r_2_r1st)
c** qdn 1/16/97: fix the Doppler wrap around problem ***
c       do i=2,n
c          fd(i) = fd(i) - prf*nint((fd(i) -fd(i-1))/prf)
c       end do
        do i=2,n
           run_avg = 0.0
           inum_avg = 0
           do j=i-1,max(1,i-4),-1
              run_avg = run_avg + fd(j)
              inum_avg = inum_avg + 1
           end do
           run_avg = run_avg / inum_avg
           if(nint((fd(i)-run_avg)/prf).ne.0) then
              print*,'BEWARE at i=',i,'fd=',fd(i),' run_avg=',run_avg
           end if
           fd(i) = fd(i) - prf*nint((fd(i)-run_avg)/prf)
        end do

c** get doppler centroid of r_1st and fdd *******

c	call dop_parameter(fd0,fdd,az_in,n,ngroup,dr,
c     1		pdf_buf,wght,fd,r_2_r1st)

	call dop_fit(fd0,fdd,n,ngroup,dr,fd,prf)

	if(i_fo_skp.eq.0) then
	   print *, 'No auto focus'
	   return
	end if

c** two end looks (1 & 4) and x_corre func ******
	ngroup1 = 64
	n = (rng_in-nrs)/ngroup1		!n is the number of groups
	call look_1_4_xfunc(buff_in,rng_in,az_in,
     1	            peak,fr,r_2_r1st,fr0,frd,pbw,prf,n,ngroup1)

c** get doppler frate of r_1st and frd **********

	call frate_parameter(fr0,frd,dr,pbw,prf,n,ngroup1,
     1	            peak,fr,r_2_r1st)

d	print * , " fr0, frd, fd0, fdd", fr0, frd, fd0, fdd
 
	return
	end

