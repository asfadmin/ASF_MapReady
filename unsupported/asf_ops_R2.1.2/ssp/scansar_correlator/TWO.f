c SccsId = @(#)TWO.f	2.41 3/24/98

	subroutine post_proc(
     *  ire_sim,proj,prod_type,c1_frm,c2_frm,
     *  file_pj_image,res_prod,
     *  sam_ac2,ct_prof_len,l1_ct_pro,l2_ct_pro,
     *  c1_pxl,c2_pxl,file_image_dump,v_filler,proc_gain,
     *  file_pj_image_low,p1_pxl,
     *  p2_pxl,
     *  ct_profile,istatus,
     *  alpha1,
     *  alpha2,alpha3,grid_ptn_flag,file_ac_image,total_page,
     *  ac_image,kids,mytask,allgrp,file_status,lamb_lat_n,lamb_lat_s,
     *  ps_lat,ps_lon,utm_zone,nlines,nsamples,p1_fm,p2_fm,
     *  zone,slat,along0,plat1,plat2,lat_orig,long_orig,rlocal_mean)
C*****WAYNE******
       include 'ssp2_const.inc'
       real*8       rlocal_mean
       integer*4       prod_type
       integer     ire_sim
       integer     proj
       real*8     c1_frm(2,2)
       real*8     c2_frm(2,2)
       real*8     ps_lat
       real*8     ps_lon
       byte     ac_image(sam_ac,sam_ac)
       byte     dummy_ac_image(sam_ac)
       character*60     file_pj_image
       integer     res_prod
       integer     proj_id
       integer*4     sam_ac2
       integer     ct_prof_len
       integer     l1_ct_pro
       integer     l2_ct_pro
       real*8     c1_pxl
       real*8     c2_pxl
       real*8     ct_profile(sam_ac)
       character*60     file_image_dump
       integer*4     v_filler
       integer*4     istatus
       real*8     proc_gain
       real*8     alpha1
       real*8     alpha2
       real*8     alpha3
       character*60     file_pj_image_low
       real*8     p1_pxl
       real*8     p2_pxl
       integer     nlines
       integer     nsamples
       integer     grid_ptn_flag 
       character*60     file_ac_image
       integer     total_page
       integer     mytask,kids,allgrp
       real*8      ta,tb
       character*256 file_status
C*****WAYNE******
       real*8    lamb_lat_n,lamb_lat_s
       integer*4 utm_zone
       integer*4 iremain_lines



c	include 'key_const.inc'
c	include 'key_pp.inc'
c	include 'proj_const.inc'
	real*8 latc,latc_d,lonc
        real*8 c1_cntr,c2_cntr
        real*8  plat1,plat2
        real*8  p1_fm(2,2),p2_fm(2,2)
        real*8 lat_orig,long_orig
        real*8     slat
        real*8     along0
        integer     zone
        integer    ny_line

        character*1 hem

c        plat1 = 23.0d0
c        plat2 = 35.0d0
         plat1 =lamb_lat_n
         plat2 =lamb_lat_s

	ijk = 0						!test only
cc	if(ijk.ne.0) then				!test only

	if (ire_sim .eq. 0 .and. grid_ptn_flag .eq. 1) then
	   if (proj .eq. ps ) then
              write(6,*)'simulated Grid line for PS Proj' 
           else if (proj .eq. lambert) then
              write(6,*)'simulated Grid line for Lamb
ert Proj'
	   else if (proj .eq. utm) then
              write(6,*)'simulated Grid line for UTM Proj' 

	   endif
           call grid_line(sam_ac2,ac_image)
c	   call point_trg				!test only
	   c1_frm(2,1) = (sam_ac2-1)*c1_pxl+c1_frm(1,1)	!test only
	   c1_frm(2,2) = (sam_ac2-1)*c1_pxl+c1_frm(1,1)	!test only
	   c2_frm(1,2) = (sam_ac2-1)*c2_pxl+c2_frm(1,1)	!test only
	   c2_frm(2,2) = (sam_ac2-1)*c2_pxl+c2_frm(1,1)	!test only
	else
c          call get_ct_prof(
c    *          ct_prof_len,l1_ct_pro,l2_ct_pro,c1_pxl,
c    *          ct_profile        )
c	   if(ire_sim.eq.1.and.prod_type.ne.prod_pvs_2) then
c              call ct_inten_norm(scale_img,
c    *              file_image_dump,v_filler,c1_pxl,ct_profile,
c    *              istatus        )
c              if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
c	   else
c	       scale_img = 1.			!test only
c	       write(6,*)'Sim Data or PVS Product(no ct_norm) generation'
c	   end if
c          if (mytask.eq.0) then 
c            ta= rtc()
c             call read_ac_image(scale_img,
c    *          file_image_dump,proc_gain,ac_image,sam_ac2,total_page,
c    *          istatus)
c             if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
c            tb=rtc()
c            write(6,*)'elapsed time for load back= ',tb-ta
c          else
c            call mp_bcast(ac_image,sam_ac*sam_ac,0,allgrp)
c          endif
	end if

	proj_id = proj
        write(6,*)'WHAT IS PROJ', proj_id
	c1_cntr = (c1_frm(1,1)+c1_frm(2,1))/2.d0
	c2_cntr = (c2_frm(1,1)+c2_frm(1,2))/2.d0
        call ac_to_ll(c1_cntr,c2_cntr,latc,latc_d,lonc,
     *           alpha1,alpha2,alpha3,rlocal_mean       )

        

	if(proj_id.eq.utm) then
c	call zone_det(lonc)		!determine zone for UTM
        zone = (180 + lonc)/6+1
ccc	call zone_det(lonc,zone)		!determine zone for UTM
	else
        if (proj_id.eq.ps) then
         hem = 'n'
         do k1=1,2
            do k2=1,2
               call ac_to_ll(c1_frm(k1,k2),c2_frm(k1,k2),
     .                 latc,latc_d,lonc,
     .                 alpha1,alpha2,alpha3,rlocal_mean)
               if(latc_d.lt.0) hem = 's'
            end do
         end do
         print*,' in PS projection, hem value is ',hem
c        if (latc_d.ge.0.0) then
         if (hem.eq.'n') then
c*  north 
            slat= 70.0d0
            along0=45.0d0
         else
c*  south
            slat  = -70.0d0
            along0=  0.0
         endif
        endif
        if (proj_id.eq.lambert) then
         if (latc_d.gt.49.0d0) then 
           if (lonc .gt. -141.0d0) then
c canada
             plat1 =77.0d0
             plat2 =49.0d0
           else
c alaska
             plat1 =65.0d0
             plat2 =55.0d0
           endif
         else
           if (lonc .gt. -141.0d0) then
c us 48
             plat1 =45.0d0
             plat2 =33.0d0
           else
c us 50
             plat1 =65.0d0
             plat2 =37.0d0
           endif
         endif
        endif

	end if

        do i=1, sam_ac2
           dummy_ac_image(i)=0
        enddo
        ny_line=sam_ac2
        if ((512*total_page).lt.ny_line)  ny_line=512*total_page
        IF ((prod_type.eq. prod_pvs_2 ).or.(prod_type.eq. prod_pvs_3)) then
c
         if (mytask.eq.0) then
           write(6,*)' PVS AT/CT prod:',file_ac_image 
	   do i=1,ny_line
	      nb = sam_ac2
	      ipt= (i-1)*nb 
	      call write_disk(file_pj_image,ac_image(1,i),nb,ipt,istatus)
              if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) go to 1333
	   enddo
           if ((ny_line).lt.sam_ac2) then
             iremain_lines= sam_ac2-(ny_line)
             do i=1,iremain_lines
	      nb = sam_ac2
	      ipt= (i-1)*nb + (ny_line)*sam_ac2
	      call write_disk(file_pj_image,dummy_ac_image,nb,ipt,istatus)
              if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) go to 1333
             enddo
           endif
         endif
1333     continue
         call mpi_bcast(istatus,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
         if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
c
         nlines  =sam_ac2
         nsamples=sam_ac2
	 write(6,*) 'PVS at/ct image size = ', nlines,'  X  ', nsamples
	ELSE if(proj.eq.atct ) then
         if (mytask.eq.0) then
           write(6,*)' Eng AT/CT prod:',file_pj_image 
	   do i=1,ny_line
	      nb = sam_ac2
	      ipt= (i-1)*nb 
	      call write_disk(file_pj_image,ac_image(1,i),nb,ipt,istatus)
              if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) go to 1444
	   enddo
           if ((ny_line).lt.sam_ac2) then
             iremain_lines= sam_ac2-(ny_line)
             do i=1,iremain_lines
              nb = sam_ac2
              ipt= (i-1)*nb + (ny_line)*sam_ac2
              call write_disk(file_pj_image,dummy_ac_image,nb,ipt,istatus)
              if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) go to 1444
             enddo
           endif
 	   if(res_prod.eq.600)
     *          call low_res_prod_original(sam_ac2,
     *          file_pj_image,file_pj_image_low,istatus)
         endif
1444     continue
         call mpi_bcast(istatus,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
         if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
c
         nlines  =sam_ac2
         nsamples=sam_ac2
         if (res_prod.eq.600) then
          nlines  =nlines/4
          nsamples=nsamples/4
         endif
	   write(6,*) 'At/CT image size = ', nlines,'  X  ', nsamples
	ELSE  if (proj .eq. utm .or. proj .eq. ps .or.
     *                proj .eq. lambert) then
           write(6,*)' Standard prod:',file_pj_image 
           ta= rtc()
           call coor_trans(c1_frm,c2_frm,nlines,nsamples,
     *                     p1_pxl,p2_pxl,
     *                     ac_image,proj_id,
     *                     zone,file_pj_image,
     *                     alpha1,alpha2,
     *                     alpha3,slat,along0,hem,
     *                     plat1,plat2,istatus,c1_pxl,c2_pxl,
     *                     mytask,kids,allgrp,res_prod,file_pj_image_low,
     *                     file_status,p1_fm,p2_fm,lat_orig,long_orig,rlocal_mean)
           if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
           if (res_prod.eq.600) then
            nlines  =nlines/4
            nsamples=nsamples/4
           endif
           tb= rtc()
           write(6,*) 'elapsed time for coor_trans ',tb-ta
	   write(6,*) 'projection image size = ', nsamples,'  X  ', nlines
	else
          write(6,*)'PVS single beam o/p'
	end if

	end



	subroutine grid_line(
     *  sam_ac2,ac_image)
c	include 'key_const.inc'
c	include 'key_pp.inc'
C*****WAYNE******
       include 'ssp2_const.inc'
       integer*4     sam_ac2
       byte     ac_image(sam_ac,sam_ac)
C*****WAYNE******


	do i = 1, sam_ac2
	do j = 1, sam_ac2
	ac_image(i,j) = 0
	end do
	end do

	do i = 3, sam_ac2, 50
	do j = 3, sam_ac2
	ac_image(i,j) = 180
	end do
	end do

	do i = 3, sam_ac2
	do j = 3, sam_ac2, 50
	ac_image(i,j) = 180
	end do
	end do

	return
	end

        subroutine flat_ct_prof(ct_profile,sam_ac2)
        include 'ssp2_const.inc'
        real*8     ct_profile(sam_ac)
        integer    sam_ac2
     

        do k = 1, sam_ac2
        ct_profile(k) = 1.
        end do

        return
        end



	
	subroutine get_ct_prof(
     *  ct_prof_len,l1_ct_pro,l2_ct_pro,c1_pxl,
     *  ct_profile        )
C*****WAYNE******
       include 'ssp2_const.inc'
       integer     ct_prof_len
       integer     l1_ct_pro
       integer     l2_ct_pro
       real*8     c1_pxl
       real*8     ct_profile(sam_ac)
C*****WAYNE******
c	include 'key_const.inc'
c	include 'key_pp.inc'
	real*8 temp,x(10),y(10),cc(5,6),xk,x_cntr
	real*8 coef_ct_prof(5)

	do k = 1, ct_prof_len
c	if(k/100*100.eq.k) write(6,*) 'before ct_pro', k, ct_profile(k)
	ct_profile(k) = ct_profile(k)/(l2_ct_pro-l1_ct_pro+1)
	end do

	nblk = 10
	do k = 1, nblk
	x(k) = (k-1+.5)*(ct_prof_len/nblk) - ct_prof_len/2
	temp = 0.
	do k1 = 1, ct_prof_len/nblk
	temp = temp + ct_profile((k-1)*ct_prof_len/nblk+k1-1)
	end do
	y(k) =temp/(ct_prof_len/nblk) 	!change mean to majority mean later
	end do

	m = 5
	nsam = nblk	
	call lsf(x,y,nsam,coef_ct_prof,m,cc)	!get poly coefficients

	do k = 1, sam_ac2*c1_pxl/100
	xk = k
	x_cntr = ct_prof_len/2
	call v_poly(xk,x_cntr,coef_ct_prof,m,ct_profile(k))
	end do

	return
	end

	subroutine ct_inten_norm(scale,
     *  file_image_dump,v_filler,c1_pxl,ct_profile,
     *  istatus        )
C*****WAYNE******
       include 'ssp2_const.inc'
       character*60     file_image_dump
       integer*4     v_filler
       real*8     c1_pxl
       real*8     ct_profile(sam_ac)
       real     scale
       integer*4     istatus
C*****WAYNE******
c	include 'key_const.inc'
c	include 'key_pp.inc'
	real array(sam_ac)
	integer bytes,dk_ptr0,dk_ptr,icount
	real*8 variance
	
	bytes = sam_ac2*4

	variance = 0.
	icount = 0

	do i = 1, sam_ac2
	dk_ptr= (i-1)*bytes
	call read_disk(file_image_dump,array,bytes,dk_ptr,istatus)	!read  A/C image
        if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return

	ipst =1
	do k = 1, sam_ac2/8
	if(array(k).ne.v_filler) then
	ipst = k					!get starting position
	go to 1111					!then continue
	end if
	end do

1111	if(ipst.ne.1) then
	  if(c1_pxl.eq.100) then			!50m or 100m
	    do k = ipst, sam_ac2	
	    array(k) = array(k)/ct_profile(k-ipst+1)	!normalized by ct_prof
	    variance = variance + array(k)**2		!get statistics
	    icount = icount + 1
	    end do
	  else
	    do k = ipst, sam_ac2
	    array(k) = array(k)/ct_profile((k-ipst)/2+1)
	    variance = variance + array(k)**2
	    icount = icount + 1
	    end do
	  end if
	end if

c 	variance = variance / icount
c	scale=0.464/sqrt(variance)*255./3 !Rayleigh std=.46, 99% bound=3

	call write_disk(file_image_dump,array,bytes,dk_ptr,istatus) !write back to disk
        if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return

	end do

C* take this out to average the variance
  	variance = variance / icount
 	scale=0.464/sqrt(variance)*255./3.5 !Rayleigh std=.46, 99% bound=3

	return
	end

	subroutine write_ac_image(
     *  file_image_dump,proc_gain,ac_image,sam_ac2,total_page,istatus)
       include 'ssp2_const.inc'
       character*60     file_image_dump
       real*8     proc_gain
       byte     ac_image(sam_ac,sam_ac)
       integer*4     istatus
       integer*4    sam_ac2
       integer    total_page

       integer bytes,dk_ptr

        bytes = sam_ac2
        do i = 1,  total_page*512
           dk_ptr= (i-1)*bytes
           call write_disk(file_image_dump,ac_image(1,i),bytes,dk_ptr,istatus)
           if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
        enddo
        return
        end

	subroutine read_ac_image(scale,
     *  file_image_dump,proc_gain,ac_image,sam_ac2,total_page,istatus)
C*****WAYNE******
       include 'ssp2_const.inc'
       character*60     file_image_dump
       real*8     proc_gain
       real       scale
       byte     ac_image(sam_ac,sam_ac)
       integer*4     istatus
       integer*4    sam_ac2
       integer    total_page
       integer    ptr_image
C*****WAYNE******
c	include 'key_const.inc'
c	include 'key_pp.inc'
c       pointer (lptr_image,array)
  	real array(sam_ac)


	integer bytes,dk_ptr0,dk_ptr
        real*8  ta,tb

        ta= rtc()

	write(6,*)' load_ac_image sam_ac2:',file_image_dump,sam_ac2,total_page
	write(6,*)' load_ac_image proc_gain:',proc_gain,scale

        bytes = sam_ac2*4
        do i = 1,  total_page*512
           dk_ptr= (i-1)*bytes
           call read_disk(file_image_dump,array,bytes,dk_ptr,istatus)
           if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
           do k = 1, sam_ac2
              ival = array(k)*scale*proc_gain  ! user i/p request
              if (ival.gt.255) ival=255
              ac_image(k,i) = ival
           end do
        end do

        tb= rtc()
        write(6,*)'TIME TIME TIME ',tb-ta



c	write(6,*)' read_ac_image:',i-1,dk_ptr
	return
	end
c********** end of zone_det ********************************************


c***********************************************************************
c	Jet Propulsion Laboratory
c	Section:	334, Radar Science and Engineering
c	Preoject:	RadarSat
c	Task:		ScanSAR
c	Code:		Fortran 77
c-----------------------------------------------------------------------
c	Module Name: ac_to_ll.f
c
c        Convert the Along-track/Cross-track projection to the
c	 Lattitude and Longitude Coordinate
c
c------------------------------------------------------------------
c	
c	8/5/94 A.Chu Initial code
c***********************************************************************
	subroutine ac_to_ll(c1,c2,lat,lat_d,lon,
     *           alpha1,alpha2,alpha3,rlocal_mean       )
C*****WAYNE******
       include 'ssp2_const.inc'
       real*8     alpha1
       real*8     alpha2
       real*8     alpha3
C*****WAYNE******


        implicit none

        character*128 SccsId_TWO
        data SccsId_TWO
     +  /'@(#)PPTWO.f:2.41'/

	
c	include 'key_const.inc'
c	include 'proj_const.inc'
                                                                     
c	-------------
c	PASSING IN PARAMETERS
c	-------------
        real*8    rlocal_mean
        real*8    c1		!Horizatal cross-track in meter
        real*8    c2		!Vertical along-track in meter

c	-------------
c	PASSING OUT PARAMETERS
c	-------------
        real*8    lat,lat_d,lon

c	-------------
c	LOCAL VARIABLES
c	-------------
        real*8    x,y,z
	
c	-------------
c	CONVERT AT/CT (c1,c2) to POLAR COORDINATE (lat,lon)
c	-------------

        call ac_to_ebf(c1,c2,x,y,z,lat,lat_d,lon,rlocal_mean,
     *  alpha1,alpha2,alpha3)


	return
	end
cc
c****************************************************************
        subroutine qsph_to_ebf(qlat,qlon,
     *              rlocal,x,y,z,lat,lat_d,lon,alpha1,alpha2,alpha3)
c       -------------------------------------------
C       Abstract:
C          Converts the spcacraft position from the quasi
C          spherical coordinates into the earth body fixed coordinates.
c       -------------------------------------------
        implicit none
c       include 'proj_const.inc'
C*****WAYNE******
       real*8     alpha1
       real*8     alpha2
       real*8     alpha3
C*****WAYNE******
c       ------------------
C       PASSING IN PARAMETERS
c       ------------------
        real*8 qlat, qlon               !Quasi-spherical angles lat.lon.
        real*8 rlocal                       !Earth radius

c       ------------------
C       PASSING OUT PARAMETERS
c       ------------------
        real*8  x,y,z                   !Sensor position
        real*8 lat,lat_d,lon            !spherical angle        
c       real*8 lat,lat_d,lon            !spherical angles lat.lon.

c       ------------------
C       LOCAL VARIABLES
c       ------------------

        x=rlocal*cosd(qlat)*cosd(qlon)
        y=rlocal*cosd(qlat)*sind(qlon)
        z=rlocal*sind(qlat)

        call rotate_z(x,y,z,alpha3,-1.)
        call rotate_y(x,y,z,alpha2,-1.)
        call rotate_z(x,y,z,alpha1,-1.)

        call ebf_to_ll(x,y,z,lat,lat_d,lon)     !These x,y,z are not real ebf
        call ll_to_ebf(lat,lon,x,y,z)


        return
        end
c**ORIGINAL*low_res_prod***********************************
        subroutine low_res_prod_original(n,file_pj_image,file_pj_image_low,istatus)
        include 'ssp2_const.inc'
        integer n,btoi
        byte buff(20000,4)
c        include 'key_const.inc'
c        include 'key_pp.inc'
        character*60 file_pj_image,file_pj_image_low
        integer      istatus

        nb1 = n
        nb2 = n/4

        do i = 1, n/4
        do k1 = 1, 4
        ipt1 = ((i-1)*4+k1-1)*nb1
        call read_disk(file_pj_image,buff(1,k1),nb1,ipt1,istatus)
        if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
        end do

        do k = 1, n/4
        sum = 0
        do k1 = 1, 4
        do k2 = 1, 4
        itemp = btoi(buff(k*4-4+k1,k2))
        sum = sum + itemp**2
        end do
        end do
        itemp = sqrt(sum/16)
        buff(k,1) = itemp
        end do

        ipt2 = (i-1)*nb2
        call write_disk(file_pj_image_low,buff(1,1),nb2,ipt2,istatus)
        if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
        end do

        write(6,*)' low_res_prod size:',n/4,n/4,file_pj_image_low
        return
        end
c**ORIGINAL*low_res_prod***********************************

        subroutine low_res_prod(iblk,nrow,ncol,
     *          file_pj_image_low,pj_image_dummy,istatus)
C*****WAYNE******
       include 'ssp2_const.inc'
       character*60     file_pj_image_low
       integer*4     istatus
C*****WAYNE******
        integer iblk,dk_ptr0
        integer nrow,ncol,btoi
        integer nrow2,ncol2
        byte buff(20000+1)
c        include 'key_const.inc'
c        include 'key_pp.inc'
       byte     pj_image_dummy(sam_pj1_pj2)

C
        nrow2 = nrow/4
        ncol2 = ncol/4
        dk_ptr0=  iblk*ncol2*nrow2

c
c
          do i=1,nrow2
             ibegin_row=(i-1)*4+1
             do   j=1 , ncol2
               ibegin_col=(j-1)*4+1
               sum = 0
c
              do jj=ibegin_col, ibegin_col+3
               do ii=ibegin_row, ibegin_row+3
                     index= jj+(ii-1)*ncol
                     itemp = btoi(pj_image_dummy(index))
                     sum = sum + itemp**2
               enddo
              enddo
c
               itemp = sqrt(sum/16)
               buff(j) = itemp
             enddo
c
         ipt2 = (i-1)*ncol2+dk_ptr0
         call write_disk(file_pj_image_low,buff,ncol2,ipt2,istatus)
         if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
c        write(6,*)'done  ',i
         enddo
c
c
C

c	write(6,*)' low_res_prod size:',n/4,n/4,file_pj_image_low
        return
        end


	subroutine coor_trans(c1_frame,c2_frame,nlines,nsamples,
     *  p1_pxl,p2_pxl,
     *  ac_image,proj_id,
     *  zone,file_pj_image,
     *  alpha1,alpha2,
     *  alpha3,slat,along0,hem,plat1,plat2,istatus,c1_pxl,c2_pxl,
     *  mytask,kids,allgrp,res_prod,file_pj_image_low,file_status,p1_fm,p2_fm,
     *  lat_orig,long_orig,rlocal_mean)
	implicit real*8 (a-h,o-z)
C*****WAYNE******
       include 'ssp2_const.inc'
       real*8     rlocal_mean
       real*8     p1_pxl
       real*8     p2_pxl
       byte     ac_image(sam_ac,sam_ac)
       integer     nlines
       integer     nsamples
       byte     pj_image(sam_pj1,sam_pj2)
       real*8     alpha1
       real*8     alpha2
       real*8     alpha3
       integer     proj_id
       integer     zone
       real*8     slat
       real*8     along0
       character*1  hem
       character*60     file_pj_image
       character*60     file_pj_image_low
       integer*4     istatus
       real*8     c1_pxl
       real*8     c2_pxl
       real*8     plat1,plat2
       integer   mytask,kids,allgrp
       integer   res_prod
       character*(*) file_status
C*****WAYNE******
c
c	include 'key_const.inc'
c	include 'key_pp.inc'
c	include 'proj_const.inc'
        real*8 lat_orig,long_orig
	real*8 c1_frame(2,2),c2_frame(2,2)
	real*8 lat,lat_d,lon
	real*8 p1_frame(2,2),p2_frame(2,2)
	real*8 p1_frm2(2,2),p2_frm2(2,2)
	real*8 p1_g(3,3),p2_g(3,3)
	real*8 c1_g_t(3,3),c2_g_t(3,3)
	real*8 p1_min,p1_max,p2_min,p2_max,p1size_frm2,p2size_frm2
	integer p1_len,p2_len,dk_ptr0
	real*4	W1(16+1),W2(16+2),W3(16+3),W4(16+4),sum,dp,sinc
	byte data(513,513)
c	common/c_weight/w1,w2,w3,w4
        integer iblk,blk_cpu(16),blk_left,iblk_total
        byte     pj_image_dummy(sam_pj1_pj2)
        character*10 str_buff1
        real a_percent
        real*8 p1_fm(2,2),p2_fm(2,2)

        real*8 qdnlat,qdnlat_d,qdnlon,qdnc1,qdnc2,qdnp1,qdnp2

c	------------
c	RESAMPLING COEFFICIENTS
c	------------

c	do i = 1, 16
c	dp = (i-1)/16.
c	w1(i) = sinc(dp+1.,pi)
c	w2(i) = sinc(dp,pi)
c	w3(i) = sinc(1.-dp,pi)
c	w4(i) = sinc(2.-dp,pi)
c	sum = w1(i)+w2(i)+w3(i)+w4(i)
c       sum = (w1(i)**2+w2(i)**2+w3(i)**2+w4(i)**2)
c	w1(i)=w1(i)/sum
c	w2(i)=w2(i)/sum
c	w3(i)=w3(i)/sum
c	w4(i)=w4(i)/sum
c	end do

	n1_blk = 40
	n2_blk = 40

c Calculate for lambert projection
        call ac_to_ll(c1_frame(1,1),c2_frame(1,1),
     *                lat,lat_d,lon, alpha1,alpha2,alpha3,rlocal_mean)
        lat_orig = lat_d
        long_orig = lon



        write(6,*) ' Inside the coor_trans'
	do k1 = 1, 2
	do k2 = 1, 2
        call ac_to_ll(c1_frame(k1,k2),c2_frame(k1,k2),lat,lat_d,lon,
     *           alpha1,alpha2,alpha3,rlocal_mean)
	write(6,*) 'before call ll_to_proj lat=',lat,lat_d,lon
        call ll_to_proj(lat_d,lon,p1_frame(k1,k2),p2_frame(k1,k2),
     *  proj_id,zone,slat,along0,plat1,plat2,lat_orig,long_orig)
	write(6,*) 'c,l',c1_frame(k1,k2),c2_frame(k1,k2),lat_d,lon
	write(6,*) 'p',p1_frame(k1,k2),p2_frame(k1,k2)
c qdn 5/6/97
        call proj_to_ll(p1_frame(k1,k2),p2_frame(k1,k2),qdnlat,qdnlat_d,qdnlon,
     *  proj_id,zone,slat,along0,hem,plat1,plat2,lat_orig,long_orig)
        call ll_to_ac(qdnlat,qdnlon,qdnc1,qdnc2,
     *  alpha1,alpha2,alpha3,rlocal_mean)
        write(6,*) 'after call proj_to_ll',p1_frame(k1,k2),p2_frame(k1,k2)
        write(6,*) 'lat =',qdnlat,' qlat_d=',qdnlat_d,' qlon=',qdnlon
        write(6,*) 'recalculate c1=',qdnc1,' c2=',qdnc2
	end do
	end do

c qdn 5/7/97
c       qdnc1 = 397875
c       qdnc2 = 384125
c       write(6,*)' hardcoded c1=',qdnc1,' c2=',qdnc2
c       call ac_to_ll(qdnc1,qdnc2,qdnlat,qdnlat_d,qdnlon,
c    *           alpha1,alpha2,alpha3,rlocal_mean)
c       write(6,*) 'after ac_to_ll, lat=',qdnlat,
c    *           ' lat_d=',qdnlat_d,' lon=',qdnlon
c       call ll_to_proj(qdnlat_d,qdnlon,qdnp1,qdnp2,
c    *  proj_id,zone,slat,along0,plat1,plat2,lat_orig,long_orig)
c       write(6,*) 'after ll_to_proj, p1=',qdnp1,' p2=',qdnp2
c       call proj_to_ll(qdnp1,qdnp2,qdnlat,qdnlat_d,qdnlon,
c    *  proj_id,zone,slat,along0,hem,plat1,plat2,lat_orig,long_orig)
c       write(6,*) ' after proj_to_ll, lat=',qdnlat,
c    *             ' lat_d=',qdnlat_d,' lon=',qdnlon
c       call ll_to_ac(qdnlat_d,qdnlon,qdnc1,qdnc2,
c    *  alpha1,alpha2,alpha3,rlocal_mean)
c       write(6,*) 'after ll_to_ac c1=',qdnc1,' c2=',qdnc2
c       if(n1_blk .eq. 40) call exit(1)

	p1_min =min(p1_frame(1,1),p1_frame(2,1),p1_frame(1,2),p1_frame(2,2))
	p1_max =max(p1_frame(1,1),p1_frame(2,1),p1_frame(1,2),p1_frame(2,2))
	p2_min =min(p2_frame(1,1),p2_frame(2,1),p2_frame(1,2),p2_frame(2,2))
	p2_max =max(p2_frame(1,1),p2_frame(2,1),p2_frame(1,2),p2_frame(2,2))

	write(6,*) 'p1_pxl', p1_pxl, p2_pxl,zone

        nlines = (int(((p1_max-p1_min)/p1_pxl)/n1_blk)+4)/4*4*n1_blk
        nsamples = (int(((p2_max-p2_min)/p2_pxl)/n2_blk)+4)/4*4*n2_blk


	if(nsamples.ne.nlines) then
	nsamples = max(nsamples,nlines)
	nlines = max(nsamples,nlines)
	end if

	write(6,*)'nlines,nsam zone proj',nlines,nsamples,zone,proj



        p1size_frm2 = nlines*p1_pxl
	p2size_frm2 = nsamples*p2_pxl

	p1_frm2(1,1) = (int(p1_min/p1_pxl)-1)*p1_pxl
        p1_fm(1,1)   = p1_frm2(1,1)
	p1_frm2(1,2) = p1_frm2(1,1)
	p1_frm2(2,1) = p1_frm2(1,1)+p1size_frm2
	p1_frm2(2,2) = p1_frm2(2,1)
	p2_frm2(1,1) = (int(p2_min/p2_pxl)-1)*p2_pxl
        p2_fm(1,1)   = p2_frm2(1,1)
	p2_frm2(1,2) = p2_frm2(1,1)+p2size_frm2
	p2_frm2(2,1) = p2_frm2(1,1)
	p2_frm2(2,2) = p2_frm2(1,2)

        p1_fm(1,1)   = p1_frm2(1,1)
        p1_fm(1,2)   = p1_frm2(2,1)
        p1_fm(2,1)   = p1_frm2(1,2)
        p1_fm(2,2)   = p1_frm2(2,2)

        p2_fm(1,1)   = p2_frm2(1,1)
        p2_fm(1,2)   = p2_frm2(2,1)
        p2_fm(2,1)   = p2_frm2(1,2)
        p2_fm(2,2)   = p2_frm2(2,2)


C  WAYNE ADDED TO assure 4 pixel lines per set to combine low_res_prod

 	p1_span = p1size_frm2/n1_blk
 	p2_span = p2size_frm2/n2_blk


        idummy=int_to_str(n1_blk,str_buff1,10)
        idummy=printflog(3,'GEO_CODING '//str_buff1//'segments of main-processed image '//'&')

        do i=1, kids
          blk_cpu(i)= n2_blk/kids
        enddo
        blk_left=MOD(n2_blk,kids)
        if (blk_left.ne.0) then
         do i=1, blk_left
          blk_cpu(i)=blk_cpu(i)+1
         enddo
        endif

        itrouble=0
        iblk_total=0
c
c       do iblk =1, blk_cpu(mytask+1)
        do iblk =1, blk_cpu(mytask+1)

        j= (iblk-1)*kids+(mytask+1)


cp1_start = p1_frm2(1,1)+(i-1)*p1_span
        p2_start = p2_frm2(1,1)+(j-1)*p2_span
		do ii = 1, nlines/n1_blk
		do jj = 1, nsamples
		pj_image(jj,ii) = 0
		end do
		end do


cdo j = 1, n2_blk
cp2_start = p2_frm2(1,1)+(j-1)*p2_span

        do i = 1, n1_blk
        p1_start = p1_frm2(1,1)+(i-1)*p1_span

        call contain(p1_start,p2_start,p1_span,p2_span,
     *                  p1_pxl,p2_pxl,c1_frame,c2_frame,iflag,
     *                  proj_id,zone,slat,along0,hem,
     *                  plat1,plat2,lat_orig,long_orig,
     *                  alpha1,alpha2,alpha3,rlocal_mean        )
	if(iflag.eq.0) go to 1111

	do k1 = 1, 3
	do k2 = 1, 3
	p1_g(k1,k2) = p1_start + (k1-1)*p1_span/2
	p2_g(k1,k2) = p2_start + (k2-1)*p2_span/2
	call proj_to_ll(p1_g(k1,k2),p2_g(k1,k2),lat,lat_d,lon,
     *  proj_id,zone,slat,along0,hem,plat1,plat2,lat_orig,long_orig)
c qdn 5/7/97, should we use lat_d (not lat)
        call ll_to_ac(lat,lon,c1_g_t(k1,k2),c2_g_t(k1,k2),
c qdn 5/8/97 switch back to lat (not lat_d)
c       call ll_to_ac(lat_d,lon,c1_g_t(k1,k2),c2_g_t(k1,k2),
     *  alpha1,alpha2,alpha3,rlocal_mean        )
	end do
	end do

	p1_len = nint(p1_span/p1_pxl)
	p2_len = nint(p2_span/p2_pxl)

	call resam2(ac_image,c1_g_t,c2_g_t,p1_g,p2_g,c1_frame,c2_frame,
     *	           c1_pxl,c2_pxl,p1_pxl,p2_pxl,data,p1_len,p2_len)

	k1_off = nint((p1_start-p1_frm2(1,1))/p1_pxl)
	k2_off = nint((p2_start-p2_frm2(1,1))/p2_pxl)


c	   ------------
c	   OUTPUT TO FRAME BUFFER
c	   ------------
	   do k1=1,p1_len
	   do k2=1,p2_len
	   ivalue = data(k2,k1)
c   pj_image(k2+k2_off,k1) = ivalue
	   pj_image(k1+k1_off,k2) = ivalue
	  end do
          end do

1111	continue
	end do

c	dk_ptr0 = (iblk-1)*nlines/n1_blk*nsamples
c	call write_pj_image(dk_ptr0,nlines/n1_blk,nsamples,
c    *  pj_image,file_pj_image,istatus)

        if (mytask.eq.0) then
         do iii=0,kids-1
          if (iblk_total.ge.n1_blk) go to 8787
c         if (iii.eq.0) then
c         else
c          call mpi_bcast(iii,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
c         endif
             call pack_receive_pj(file_pj_image,iii,nlines/n1_blk,nsamples,pj_image,
     *                         pj_image_dummy,iblk_total,res_prod,file_pj_image_low,istatus)
             write(6,*)'DONE processing BLOCK # ',iblk_total+1,' from NODE # ',iii
             idummy=int_to_str(iblk_total+1,str_buff1,10)
             idummy=printflog(3,'GEO_CODING segment # '//str_buff1//'&')
             iblk_total=iblk_total+1
             a_percent= (1.0*iblk_total)/n1_blk*100.0
             idummy=printfstat(file_status,4,a_percent)
         enddo
        else
c          do iii=1, kids-1
c            call mpi_bcast(ii,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
c            if (ii.eq.mytask) then
               call pack_send_pj(nlines/n1_blk,nsamples,pj_image,pj_image_dummy)
c            endif
c          enddo
        endif

8787    continue


	end do
8888    continue
        call mpi_bcast(istatus,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
        if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return


	write(6,*) 'nlines = ', nlines
	write(6,*) 'nsamples = ', nsamples

	return
	end

c**************** end of ac_to_ll.f**********************************


c***********************************************************************
c	Jet Propulsion Laboratory
c	Section:	334, Radar Science and Engineering
c	Preoject:	RadarSat
c	Task:		ScanSAR
c	Code:		Fortran 77
c-----------------------------------------------------------------------
c	Module Name: ll_to_proj.f
c
c        Convert the Latitude and Longitude Coordinate to the 
c	 Universal Transverse Mercator (UTM) or the Polar Stereographic 
c	 (PS) projection coordinate.
c
c------------------------------------------------------------------
c	
c	8/5/94 A.Chu Initial code
c***********************************************************************
	subroutine ll_to_proj(lat_d,lon,p1,p2,
     *  proj_id,zone,slat,along0,plat1,plat2,lat_orig,long_orig)
	implicit none
C*****WAYNE******
       include 'ssp2_const.inc'
       integer     proj_id
       integer     zone
       real*8     slat
       real*8     along0
       real*8     plat1,plat2,lat_orig,long_orig
C*****WAYNE******
c	include 'key_const.inc'
c	include 'proj_const.inc'
c	-------------
c	PASSING IN PARAMETERS
c	-------------
        real*8    lat_d,lon

c	-------------
c	PASSING OUT PARAMETERS
c	-------------
        real*8    p1		!Ground Projection in Horizotal 
        real*8    p2		!Ground Projection in Vertical in meter

	
c	-------------
c	CONVERT from POLAR COORDINATE to TARGET COORDINATE
c	-------------

	if (proj_id .eq. utm) then
           call ll_utm(lat_d,lon,p1,p2,zone)
        else if (proj_id .eq. lambert) then
           call ll_lambert(lat_d,lon,p1,p2,plat1,plat2,lat_orig,long_orig)
	else
c          call ll_ssmi (lat_d,lon,p1,p2,slat,along0)
           call llssmi(lat_d,lon,p1,p2)
	endif

      	return
      	end

c**************************end of ll_to_ac.f********************


c***********************************************************************
c	Jet Propulsion Laboratory
c	Section:	334, Radar Science and Engineering
c	Preoject:	RadarSat
c	Task:		ScanSAR
c	Code:		Fortran 77
c-----------------------------------------------------------------------
c	Module Name: ll_utm.f
c
c	Abstract:
c             convert lat/lon to universal transverse mercator(utm)
c             coordinates (x:easting,y: northing)
c
c 	User-defined Input parameters:
c
c         1).tlat  - array of latitude coordinates
c         2).tlon  - array of longitude coordinates
c
c 	Output parameters:
c
c         1).x - array of easting coordinates
c         2).y - array of northing coordinates
c
c         long0- central meridian
c           dtr- degree to radian
c          xadj- easting adjustment
c          yadj- northing adjustment
c
c Reference: Synder, J. P. 1983, Map projection used by the US
c            geological survey(bulletin 1532)
c
c------------------------------------------------------------------
c
c	4/10/94 A.Chu Adopted from SIRC
c******************************************************************
        subroutine ll_utm(tlat,tlon,x,y,zone)

        implicit none
C*****WAYNE******
       include 'ssp2_const.inc'
       integer     zone
C*****WAYNE******
c	include 'key_const.inc'
c	include 'proj_const.inc'
c	-------------
c	PASSING IN PARAMETERS
c	-------------
        real*8    tlat		!Latitude in degree
        real*8    tlon		!Longitude in degree

c	-------------
c	PASSING OUT PARAMETERS
c	-------------
        real*8 x, y		!UTM grid (east,north)=(x,y)

c	-------------
c	LOCAL VARIABLES
c	-------------
        real*8    a		!Earth equatorial radius
        real*8    esq		!Earth eccentricity squared
        real*8    k0		!Center meridian scale factor
        real*8   epsq, lat, long, long0, a1, a2, a3, rn
        real*8   t, b1, b2, rm, rm0, dtr, eoffset, noffset
        real*8   tanlat, c
        real*8   yadj, xadj
        real*8   e4, e6, c1, c2, c3, c4
        integer*4  n,  i

c	-------------
c       READ from KEY_CONST.INC
c	-------------
	a=re		!from key_const.inc
	esq=ecc_2
	k0=k0_utm

c	-------------
c       Calculate the central meridian (long0)
c	-------------
        dtr = 4.d0 * datan(1.d0)/180.d0	!Degree to radian
        epsq = esq/(1.d0 - esq)
        long0 = float(zone-1)*6.d0 + 3.d0 - 180.d0	!zone from proj_const.inc
        xadj = 5.d5
cc	-------------
c	Need info to determine hemisphere ????
cc	-------------
       	if (tlat .ge. 0.d0) then    
           yadj = 0.d0			
       	else	
           yadj = 1.d7		!Refernce pt: Southern Hemisphere
      	endif

c	-------------
c       Calculate the UTM (x,y)
c	-------------
          lat = tlat * dtr
          long = tlon * dtr
          rn = a/dsqrt(1.d0 - esq*(dsin(lat))**2)
          tanlat = dtan(lat)
          t = dtan(lat)**2
          c = epsq * (dcos(lat))**2
          a1 = dcos(lat) * (tlon - long0) * dtr
 
          a2 = (1.d0 - t + c) * a1**3 / 6.d0
          a3 = (5.d0 - 18.d0*t + t**2 + 72.d0*c - 58.d0*epsq)
     *         * a1**5 / 120.d0

          x = k0 * rn * (a1 + a2 + a3) + xadj 

          e4 = esq * esq
          e6 = e4 * esq
          c1 = 1.d0 - esq/4.d0 - 3.d0*e4/64.d0 - 5.d0*e6/256.d0
          c2 = 3.d0*esq/8.d0 + 3.d0*e4/32.d0 + 45.d0*e6/1024.d0
          c3 = 15.d0*e4/256.d0 + 45.d0*e6/1024.d0
          c4 = 35.d0*e6/3072.d0
          rm = a*(c1*lat - c2*dsin(2.d0*lat) + c3*dsin(4.d0*lat)
     *                   - c4*dsin(6.d0*lat))
          rm0 = 0.0
          b1 = (a1**2)/2.d0
     *         + (5.d0 - t + 9.d0*c + 4.d0 * c**2) * a1**4 / 24.d0
          b2 = (61.d0 - 58.d0*t + t**2 + 600.d0*c
     *                + 330.d0*epsq) * a1**6 / 720.d0

          y = k0 * (rm - rm0 + rn*tanlat*(b1 + b2)) + yadj 

      return
      end
c***********************************************************************


c***********************************************************************
c	Jet Propulsion Laboratory
c	Section:	334, Radar Science and Engineering
c	Project:	RadarSAT
c	Task:		ScanSAR Data Processor
c	Code:           Fortran 77 
c-----------------------------------------------------------------------
c	Module Name:	ll_ssmi.f
c
c		This subroutine converts the geodetic latitude and longitude
c               in the polar region to the Polar Stereographic
c		x,y coordinates.
c
c	Input:
c	        1.alat(degree) : geodetic latitude.
c	        2.along(degree) : geodetic longitude.
c		3.a(m) :Equatorial radius of the Earth model.
c		4.e2   :square of eccentricity of the Earth model.
c		5.slat(degree) :standard parallel (latitude without 
c			        distortion)
c		6.along0(degree):longitude of the central meridian
c		7.sgn   :positive for North Pole area and negative
c			 for South Pole area.
c
c	Output: 
c	        1.x(m) :x coordinate in the Polar Stereographic
c			 projection.
c	        2.y(m) :y coordinate in the Polar Stereographic
c			 projection.
c
c	Function:    
c		The ll_ssmi processing includes
c
c		1). Check the sign to identify North Pole or
c		    South Pole area.
c		2). Compute the polar distance r 
c		3). According the equations given in ref., the
c		    (x,y) coordinates are computed.
c
c	Reference:  
c		1.J.P. Snyder," Map Projections Used by the 
c		  U.S. Geological Survey"
c		2.T.Cheng, "Algorithms for Image Projections of 
c		  Radarsat ScanSAR Processor",JPL IOM 3344-93-039
c----------------------------------------------------------------------------
c
c	4/27/94	    A. Chu   1.delete the redundant "sgn" input parameter
c	                     2.revised  handling of lat=90 case.
c	6/18/93     T. Cheng Initial code.
c****************************************************************************
	subroutine ll_ssmi (alat, along,x, y ,slat,along0)
	implicit none
C*****WAYNE******
       include 'ssp2_const.inc'
       real*8     slat
       real*8     along0
C*****WAYNE******
c	include 'key_const.inc'
c	include 'proj_const.inc'
c	Input parameters

	real*8	alat, along

c	Output parameters

	real*8	x,y

c	Local parameters

	real*8	xlat,xlong,
     1	xslat,xlong0,
     1	tc,mc,
     1	r,t,
     1	a,e2,
     1	e,e_2,
     1	sgn

	if (alat .eq. 90.d0) then
           x=0.d0
           y=0.d0
           return
	endif

	a=re
	e2=ecc_2
	e = sqrt(e2)
	e_2 = e/2.

        slat=70.d0              !Standard parallel no distortion
        along0=45.d0            !meridian offset 45
c	---------------------------------------------------------------
c	Identify the North Pole or South Pole area
c	---------------------------------------------------------------
	sgn=1.0
	if (slat .lt. 0) sgn=-1.	!A.Chu 4/27/94
	xlong0 = sgn*along0
	xslat  = sgn*slat
	xlat   = sgn*alat
	xlong  = sgn* along

c	----------------------------------------------------------------
c	Compute the Polar Stereographic (x,y) coordinates 
c	----------------------------------------------------------------

	t = tand(45.-xlat/2.)/((1-e*sind(xlat))/(1+e*sind(xlat)))**(e_2)
	tc= tand(45-xslat/2.)/((1-e*sind(xslat))/(1+e*sind(xslat)))**(e_2)
	mc= cosd(xslat)/sqrt(1.0-e2*(sind(xslat)**2))	

	r = a*mc*t/tc

	x = sgn * r * sind(xlong-xlong0)
	y = -sgn * r * cosd(xlong-xlong0)

	return
	end

	subroutine contain(p1_start,p2_start,p1_span,p2_span,
     *			p1_pxl,p2_pxl,c1_frame,c2_frame,iflag,
     *  		proj_id,zone,slat,along0,hem,
     *                  plat1,plat2,lat_orig,long_orig,
     *  		alpha1,alpha2,alpha3,rlocal_mean        )
	implicit real*8 (a-h, o-z)
C*****WAYNE******
       include 'ssp2_const.inc'
       integer     proj_id
       integer     zone
       real*8     slat
       real*8     along0
       real*8     plat1,plat2,lat_orig,long_orig
       real*8     alpha1
       real*8     alpha2
       real*8     alpha3
       real*8     rlocal_mean
       character*1 hem
C*****WAYNE******
c	include 'proj_const.inc'
	real*8 c1_frame(2,2),c2_frame(2,2),lat,lat_d,lon

	iflag = 0
	do k1 = 1, 2
	do k2 = 1, 2
	p1 = p1_start+(k1-1)*p1_span
	p2 = p2_start+(k2-1)*p2_span
        call proj_to_ll(p1,p2,lat,lat_d,lon,
     *  proj_id,zone,slat,along0,hem,plat1,plat2,lat_orig,
     *  long_orig)
c qdn 5/7/97 should we use lad_d not lat?
        call ll_to_ac(lat,lon,c1,c2,
c qdn 5/8/97 switch back to lat (not lat_d)
c       call ll_to_ac(lat_d,lon,c1,c2,
     *  alpha1,alpha2,alpha3,rlocal_mean        )
	if(c1.gt.c1_frame(1,1).and.c1.lt.c1_frame(2,2).and.c2.gt.
     *		c2_frame(1,1).and.c2.lt.c2_frame(2,2)) iflag = 1	
	end do
	end do

	return
	end

c**************** end of ll_to_proj.f**********************************


c***********************************************************************
c	Jet Propulsion Laboratory
c	Section:	334, Radar Science and Engineering
c	Preoject:	RadarSat
c	Task:		ScanSAR
c	Code:		Fortran 77
c-----------------------------------------------------------------------
c	Module Name: proj_to_ll.f
c
c        Convert the projection from Universal Transverse
c	 Mercator (UTM) or Polar Stererographic (PS) to the
c	 Latitude and Longitude Coordinate
c
c------------------------------------------------------------------
c	
c	8/9/94 A.Chu Initial code
c***********************************************************************
        subroutine proj_to_ll(p1,p2,lat,lat_d,lon,
     *  proj_id,zone,slat,along0,hem,plat1,plat2,lat_orig,long_orig)
	implicit none
C*****WAYNE******
       include 'ssp2_const.inc'
       integer     proj_id
       integer     zone
       real*8     slat
       real*8     along0
       character*1 hem
       real*8     plat1,plat2,lat_orig,long_orig
C*****WAYNE******
c	include 'key_const.inc'
c	include 'proj_const.inc'
c	-------------
c	PASSING IN PARAMETERS
c	-------------
        real*8    p1		!Ground Projection in Horizotal 
        real*8    p2		!Ground Projection in Vertical in meter

c	-------------
c	PASSING OUT PARAMETERS
c	-------------
        real*8    lat,lat_d,lon

c	-------------
c	LOCAL VARIABLES
c	-------------

c	-------------
c	CONVERT from UTM or PS to the POLAR COORDINATE
c	-------------

	if (proj_id .eq. utm) then
	   call utm_ll(p1,p2,lat,lat_d,lon,zone)
        else if ( proj_id .eq. lambert) then
           call lambert_ll(p1,p2,lat,lat_d,lon,plat1,plat2,
     *                     lat_orig,long_orig)

	else
c          call ssmi_ll(p1,p2,lat,lat_d,lon,slat,along0)
c qdn 5/8/97 old code
c          call ssmill(p1,p2,hem,lat,lon)
c          lat_d = lat
           call ssmill(p1,p2,hem,lat_d,lon)
           lat = atand(tand(lat_d)*(1.0-ecc_e**2))

	endif

      	return
      	end
c************************* end of ll_utm.f **************************


c***********************************************************************
c	Jet Propulsion Laboratory
c	Section:	334, Radar Science and Engineering
c	Preoject:	RadarSat
c	Task:		ScanSAR
c	Code:		Fortran 77, Alliant Multiprocessors. 
c-----------------------------------------------------------------------
c	Module Name: utm_ll.f
c
c	Abstract:
c	
c        Convert the utm coordinates (y:northing & x:easting) of
c        a target to lat/lon.
c        
c	 Note:
c	 Adjust y is alway 1.d7 also on the northern hemisphere.
c	 Therefore the northing is 1.d7 larger than the true northing
c	 for latitudes larger than 0. 
c	 The advantage of this approach is that inversion is possible
c	 without additional information on whether the location in on
c	 the southern or northern henisphere.	
c
c 	Input parameters:
c
c        1).x     : array of easting coordinates (in meters)
c        2).y     : array of northing coordinates (in meters)
c
c 	Output parameters:
c
c        1).lat : array of latitude coordinates (in degrees)
c        2).long: array of longitude coordinates (in degrees)
c
c Reference: Synder, J. P. 1983, Map projection used by the US
c            geological survey(bulletin 1532)
c
c------------------------------------------------------------------
c	
c	8/22/94 A.Chu Adding geocentric latitude
c	4/10/93 A.Chu Adopted from SIRC
c***********************************************************************
	subroutine utm_ll(x,y,lat,lat_d,lon,zone)
        implicit none
C*****WAYNE******
       include 'ssp2_const.inc'
       integer     zone
C*****WAYNE******
c	include 'key_const.inc'
c	include 'proj_const.inc'
c	-------------
c	PASSING IN PARAMETERS
c	-------------
        real*8 x, y             !UTM grid (east,north)=(x,y)

c	-------------
c	PASSING OUT PARAMETERS
c	-------------
        real*8    lat_d        !Geodetic Latitude in degree
        real*8    lat          !Geocentric Latitude in degree
        real*8    lon          !Longitude in degree

c	-------------
c	LOCAL VARIABLES
c	-------------

        real*8    a             !Earth equatorial radius
        real*8    esq           !Earth eccentricity squared
        real*8    k0            !Center meridian scale factor
	integer*4  i
        real*8     u1, u2, u3, lat1, esqsin2, lat1d, long0, dtr
        real*8     rm, e1, u, epsq, t1, c1, rn1, r1, rm0
        real*8     tanlat1, d
        real*8     xadj, yadj	!Northing origin:(5.d5,0.d0)
	                        !Southing orogin:(5.d5,1.d7)

c	-------------
c	READ from KEY_CONST.INC
c	-------------
	a=re		!read from key_const.inc
	esq=ecc_2
	k0=k0_utm

c	-------------
c	GET CENTRAL LONGITUDE
c	-------------
      	dtr = 4.d0 * datan(1.d0)/180.d0
      	rm0 = 0.d0
      	if (lat_d .ge. 0.d0) then
            yadj = 0.d0		
      	else	
           yadj = 1.d7	!Reference pt: Southern Hemisphere
     	endif

      	xadj = 5.d5

      	long0 = float(zone)*6.d0 - 6.d0/2 - 180.d0    !zone from proj_const.inc

c	-------------
c	COMPUTE LAT. & LONG. from A UTM POINT(x,y)
c	-------------
          rm = (y - yadj)/k0 + rm0
          e1 = (1.d0 - dsqrt(1.d0 - esq))/(1.d0 + dsqrt(1.d0 - esq))
          u = rm/(a * (1.d0 - esq/4.d0 - (3.d0 * esq * esq/64.d0)
     *                      - (5.d0 * esq * esq * esq/256.d0) ) )
          u1 = (3.d0 * e1 / 2.d0 - (27.d0 * e1**3)/32.d0) * dsin(2.d0*u)
          u2 = (21.d0 * e1**2/16.d0 - (55.d0 * e1**4)/32.d0)
     *             * dsin(4.d0*u)
          u3 = (151.d0 * e1**3 / 96.d0) * dsin(6.d0*u)
          lat1 = u + u1 + u2 + u3
          lat1d = lat1/dtr

          esqsin2 = 1.d0 - esq*(dsin(lat1))**2
          epsq = esq/(1.d0 - esq)
          c1 = epsq * (dcos(lat1))**2
          tanlat1 = dsin(lat1)/dcos(lat1)
          t1 = tanlat1 * tanlat1
          rn1 = a/dsqrt(esqsin2)
          r1 = a*(1.d0 - esq)/dsqrt(esqsin2 * esqsin2 * esqsin2)
          d = (x - xadj)/(rn1 * k0)

          lat_d = lat1d - ((rn1 * tanlat1/r1) * (d*d*0.5d0
     *             - (5.d0 + 3.d0*t1 - 10.d0*c1 + 4.d0*c1*c1
     *                     - 9.d0*epsq) * (d**4)/24.d0
     *             + (61.d0 + 90.d0*t1 + 298.d0*c1 + 45.d0*t1*t1
     *                      - 252.d0*epsq - 3.d0*c1*c1)
     *                 *(d**6)/720.d0) )/dtr

          lat = atand((1-esq)*tand(lat_d))

          lon = long0 + ((1.d0/dcos(lat1)) * (d
     *              - (1.d0 + 2.d0*t1 + c1) * (d**3)/6.d0
     *              + (5.d0 - 2.d0*c1 + 28.d0*t1 - 3.d0*c1*c1
     *                      + 8.d0*epsq + 24.d0*t1*t1)
     *                 *(d**5)/120.d0) )/dtr

      return
      end

c************************* end of utm_ll.f **************************



c***********************************************************************
c	Jet Propulsion Laboratory
c	Section:	334, Radar Science and Engineering
c	Project:	RadarSAT
c	Task:		ScanSAR Data Processor
c	Code:           Fortran 77 
c-----------------------------------------------------------------------
c	Module Name:	ssmi_ll.f
c
c		This subroutine converts the Polar Stereographic
c		x,y coordinates to the geodetic latitude and longitude
c		in the polar region.
c
c	Input:
c	        1.x(m) :x coordinate in the Polar Stereographic
c			 projection.
c	        2.y(m) :y coordinate in the Polar Stereographic
c			 projection.
c		3.a(m) :Equatorial radius of the Earth model.
c		4.e2   :square of eccentricity of the Earth model.
c		5.slat(degree) :standard parallel (latitude without 
c			        distortion)
c		6.along0(degree):longitude of the central meridian
c		7.sgn   :positive for North Pole area and negative
c			 for South Pole area.
c
c	Output: 
c	        1.alat(degree) : geodetic latitude.
c	        1.along(degree) : geodetic longitude.
c
c	Function:    
c		The ssmi_ll processing includes
c
c		1). Check the sign to identify North Pole or
c		    South Pole area.
c		2). Compute the polar distance r & angle chi of
c		    the x,y coordinate.
c		3). According the equations given in ref., the
c		    geodetic latitude and longitude are computed.
c
c	Reference:  
c		1.J.P. Snyder," Map Projections Used by the 
c		  U.S. Geological Survey"
c		2.T.Cheng, "Algorithms for Image Projections of 
c		  Radarsat ScanSAR Processor",JPL IOM 3344-93-039
c----------------------------------------------------------------------------
c
c	8/22/94	    A. Chu   Adding geocentric latitude
c	4/27/94	    A. Chu   1.Revised the longitude greater than 360.
c		             2.Revised  handling of lat=90 case.
c	                     3. delete redundant input parameter,sgn.
c	6/18/93     T. Cheng Initial code.
c****************************************************************************
	subroutine ssmi_ll(x,y,lat,alat, along,
     *  slat,along0)
	implicit none
C*****WAYNE******
       include 'ssp2_const.inc'
       real*8     slat
       real*8     along0
C*****WAYNE******
c	include 'key_const.inc'
c	include 'proj_const.inc'
                                                                     
c	Input parameters

	real*8  x,y	

c	Output parameters

	real*8	lat		!Geocentric lat.
	real*8	alat		!Geodetic lat.
	real*8	along		!Longitude

c	Local parameters

	real*8	xx,xy,
     1	xslat,xlong0,
     1	tc,mc,
     1	r,t,
     1	e,e_2,
     1	chi,
     1	a,e2,
     1	sgn

	e2=ecc_2
	a=re
	e = sqrt(e2)
	e_2 = e/2.

c	---------------------------------------------
c	Check Pole area
c	---------------------------------------------
	sgn=1.
	if (slat .lt. 0.) sgn=-1.	!A.Chu 4/27/94
	xlong0 = sgn*along0
	xslat  = sgn*slat
	xx     = sgn*x
	xy     = sgn*y

c	--------------------------------------------------
c	Compute polar distance r and angle chi
c	--------------------------------------------------
	r = dsqrt(xx**2+xy**2)

	if (r .le. 0.0d0) then
	   alat = sgn*90.
	   along= 0.d0
	   write(6,*)' ***Longitude set to 0.0 degree ***'
	else

	tc=tand(45.-xslat/2.)/((1-e*dsind(xslat))/(1+e*dsind(xslat)))**(e_2)
	mc = dcosd(xslat)/dsqrt(1.0-e2*(dsind(xslat)**2))	

	t = r*tc/(a*mc)
	chi = pi/2 - 2 * atan(t)

c	--------------------------------------------------
c	Compute geodetic latitude & longitude
c	--------------------------------------------------

	alat = chi + (e2/2.+5.*e2**2/24.+e2**3/12.)*dsin(2*chi)
     *	       +(7*e2**2/48.+29.*e2**3/240.)*dsin(4.*chi)+(7.*e2**3/120.)
     *         *dsin(6*chi)

	alat = sgn*alat*180./pi
	lat = atand((1-e2)*tand(alat))

	along = sgn*(xlong0+datan2d(xx,-xy))

d	write(6,*)'r tc mc t chi alat'
d	write(6,*)r,tc,mc,t,chi,alat
d	write(6,*)'alat along:',alat,along

	if ( along .le. -180.d0 ) along = along + 360.d0

cc	if ( along .lt. 0.d0 ) along = along + 360.d0
cc	if ( along .gt. 360.d0 )along = along - 360.d0
	endif

	return
	end

c**************************end of proj_to_ll.f********************


        subroutine llssmi(lat,long,x,y)
c
c*********************************************************************
c
c       Description:
c
c       This subroutine converts from geodetic latitude and longitude to
c       polar stereographic (x,y) coordinates for the polar regions. The
c       standard parallel (latitude with no distortion) is +/- 70 degrees.
c       The equations are from Synder, J. P., 1982, map projections used
c       by the U.S. geological survey, geological survey bulletin 1532,
c       U.S. government printing office. see jpl technical memorandum
c       3349-85-101 for further details.
c
c
c       Arguments:
c
c       Variable  Type          I/O     Descritpion
c
c       alat     real*8          I      geodetic latitude(degrees, +90 to -90)
c       alon     real*8          I      geodetic longitude(degrees, 0 to 360)
c       x        real*8          O      polar stereographic x coordinate(km)
c       y        real*8          O      polar stereographic y coordinate(km)
c
c       re       real*8          I      radius of the earth (km)
c       e2       real*8          I      eccentricity square (m)
c       slat     real*8          I      standard parallel
c       xlam     real*8          I      meridian along positive y-axis
c
c
c                   Written by C. S. Morris - April 29, 1985
c                   modified for ers-1 project- genreate the xy table
c                   instead of one (x,y) per call to the subroutine.
c                                                         ssp  Jan 1989.
c
c                   add re, e2, slat and xlam to the calling sequence.
c                                                         ssp  Mar 1989.
c
c*****************************************************************************
c
        implicit none
        dimension t(2)
        real*4 sn
        real*8 x,y
        real*8 lat,long
        real*8 alat,along
        real*8 e,e2,cdr,pi,dtr
        real*8 re,slat,xlam,rlat,t,cm,rho
        integer*4 ii

c Start the program
        alat = lat
        along = long
c 10/16/96 Change convention of the longitude
        if(along.le.0) along=along+360
c******************************************************************************
c
c       Definition of constants
c
c       conversion constant from degrees to radians = 57.29577951
        cdr=57.29577951
c       radius of earth = 6378.273 km (huges ellipsoid)
        re=6378.273                 !passed from calling program 3/89
c       eccentricity of the earth  e**2=0.006693883 (huges ellipsoid)
        e2=0.006693883              !passed from calling program
        e=dsqrt(e2)
c       pi=3.141592654
        pi=3.141592654
        dtr=pi/180.d0
c       standard parallel - latitude with no distortion = +/- 70 degrees
        slat=70.                    !passed from calling program
c
c       Set constants for ssm/i frid for northern hemisphere
c
        sn=1.0
        xlam=-45.0                   !passed from calling program
c
c       Test for north or south hemisphere and reset constants if necessary
c
        if(alat .ge. 0.0)go to 200
        sn=-1.0
        xlam=0.0
c
c       Compute x and y in grid coordinates
c
200     continue
c
        alat=sn*alat
        along=sn*along

        rlat=alat
        do ii=1,2
          if(ii .eq. 2)rlat=slat
          t(ii)=dtan((pi/4.0)-(rlat/(2.0*cdr)))/((1.0-e*dsin(rlat*dtr))/
     x          (1.0+e*sin(rlat*dtr)))**(e/2.0)
        end do
        cm=dcos(slat*dtr)/dsqrt(1.0-e2*(sin(slat*dtr)**2))
        rho=re*cm*t(1)/t(2)
        x=(rho*sn*dsin((along-xlam)*dtr))*1.d3
        y=-(rho*sn*dcos((along-xlam)*dtr))*1.d3
c
c       Reset the sign
c
        alat=alat*sn
        along=along*sn
c
        return
        end


        subroutine ssmill(xx,yy,hem,alat,along)
c******************************************************************************
c
c
c       Description:
c
c       This subroutine converts from polar stereographic (x,y) coordinates
c       to geodetic latitude and longitude for the polar regions. The standard
c       parallel (latitude with no distortion) is +/- 70 degrees. The equations
c       are from Synder, J. P., 1982, map projections used by the U.S.
c       geological survey, geological survey bulletin 1532, U.S. goverment
c       printing office. See JPL technical memorandum 3349-85-101 for further
c       details.
c
c
c       Arguments:
c
c       Variable    Type       I/O    Description
c
c       xx,yy       real*8      I     polar sterographic x,y coordinate (m)
c                                                                    ssp 8/89
c       alat        real*8      O     geodetic latitude (degrees, +90 to -90)
c       alon        real*8      O     geodetic longitude (degrees, 0 to 360)
c
c
c                   Written by C. S. Morris - April 29, 1985
c
c*******************************************************************************
c       re                      radius of the earth (km)          8/89
c       e2                      eccentricity square (m)           8/89
c       slat                    standard parallel (degrees)       8/89
c       xlam                    meridian along positive y axis(degrees)
c
        implicit none
        real*8 xx,yy
        real*8 alat
        real*8 along
        real*8 e,e2,cdr,pi,re,slat,xlam,dtr
        character*1 hem
        real*8 sn,rho,cm,t,chi,xpr,ypr,x,y,slatr
c
c       Definition of constants
c
c       Conversion constant from degrees to radians = 57.29577951
        cdr=57.29577951
c       Radius of the earth = 6378.273 km (hughes ellipsoid)
        re=6378.273                      !passed from calling program 8/89
c       eccentricity of the earth e**2=0.006693883 (hughes ellipsoid)
        e2=0.006693883                   !passed from calling program 8/89
        e=dsqrt(e2)
c       pi=3.141592654
        pi=3.141592654
        dtr=pi/180.d0
c
c       Standard parallel - latitude with no distortion = =/- 70 degrees
        slat=70.0                        !passed from calling program 8/89
        slatr=slat*dtr

c       set constants for ssm/i grid for northern hemisphere.
500     sn=1.0
        xlam=-45.                          !passed from calling program 8/89
c       Test for northern or south hemisphere and reset constants if necessary.
c
        if(hem .eq. 'n')go to 200
        sn=-1.0
        xlam=0.
c
c       Compute latitude and longitude
c
200     continue
c
            x=xx*1.d-3
            y=yy*1.d-3
            rho=dsqrt(x*x + y*y)
            if(rho .le. 0.0001)then
              alat=90.
              if(sn .lt. 0.)alat=-90.
              along=0.
c
            else
250           cm=dcos(slatr)/dsqrt(1.0-e2*(dsin(slatr)**2))
              t=dtan((pi/4.)-(slatr/(2.0)))/((1.0-e*dsin(slatr))/
     x        (1.0+e*dsin(slatr)))**(e/2.)
              t=rho*t/(re*cm)
              chi=(pi/2.0)-2.0*datan(t)
              alat=chi+((e2/2.0)+(5.0*e2**2.0/24.0)+
     x                  (e2**3.0/12.0))*dsin(2*chi)+
     x                  ((7.0*e2**2.0/48.0)+(29.0*e2**3/240.0))*
     x                  dsin(4.0*chi)+(7.0*e2**3.0/120.0)*dsin(6.0*chi)
              alat=sn*alat*cdr
              xpr=sn*x
              ypr=sn*y
              along=datan2(xpr,-ypr)*cdr+sn*xlam
              along=sn*along
              if(along .le. -180.d0)along=along+360.d0                !ssp 8/89
              if(along .lt. 0.)along=along+360.                       !ssp 8/89
              if(along .gt. 360.)along=along-360.                     !ssp 8/89

c 10/16/96 Modify the convention of longitude to be between -180 and 180
              if(along.gt.180.0) along = along - 360.d0

            end if
c
        return
        end


c***********************************************************************
c	Jet Propulsion Laboratory
c	Section:	334, Radar Science and Engineering
c	Preoject:	RadarSat
c	Task:		ScanSAR
c	Code:		Fortran 77
c-----------------------------------------------------------------------
c	Module Name: ll_to_ac.f
c
c        Convert the SacnSAR projection from the Latitude and Longitude
c	 Coordinate to the Along-Track/Cross-Track projection
c
c------------------------------------------------------------------
c	
c	8/9/94 A.Chu Initial code
c***********************************************************************
        subroutine ll_to_ac(lat,lon,c1,c2,
     *  alpha1,alpha2,alpha3,rlocal_mean        )
        implicit none
C*****WAYNE******
       include 'ssp2_const.inc'
       real*8     alpha1
       real*8     alpha2
       real*8     alpha3
       real*8     rlocal_mean
C*****WAYNE******
c	include 'key_const.inc'
c	include 'proj_const.inc'
c	-------------
c	PASSING IN PARAMETERS
c	-------------
        real*8    lat,lon	!latitude and longitude

c	-------------
c	PASSING OUT PARAMETERS
c	-------------
        real*8    c1		!Horizatal cross-track in meter
        real*8    c2		!Vertical cross-track in meter

c	-------------
c	LOCAL VARIABLES
c	-------------
        real*8    x,y,z		!psition vector for EBF coordinate

c	-------------
c	CONVERT POLAR COORDINATE to AT/CT
c	-------------

	call ll_to_ebf(lat,lon,x,y,z)
        call ebf_to_ac(x,y,z,rlocal_mean,c1,c2,
     *  alpha1,alpha2,alpha3       )



      	return
      	end

c****************************************************************

	subroutine resam2(ac_image,c1_g_t,c2_g_t,p1_g,p2_g,c1_frame,c2_frame,
     *	            c1_pxl,c2_pxl,p1_pxl,p2_pxl,data,p1_len,p2_len)

	implicit none	
	
c	------------------
c	PASSING IN PARAMETERS	
c	------------------
	real*8  c1_g_t(3,3),c2_g_t(3,3),p1_g(3,3),p2_g(3,3)
	real*8  c1_frame(2,2),c2_frame(2,2)
	real*8  c1_pxl,c2_pxl,p1_pxl,p2_pxl
	integer p1_len,p2_len,ivalue,btoi

c	------------------
c	PASSING OUT PARAMTERS
c	------------------
	byte  data(513,513)	!output data buffer

c	------------------
c	LOCAL VARIABLES
c	------------------
	integer sam_ac
	parameter (sam_ac = 11000)
	byte ac_image(sam_ac,sam_ac)
	real*8	mu1(3,3),mu2(3,3)
	real*8	e1_coef(3),e2_coef(3)
	real*8	dp1,dp2,c1estm,c2estm,d1,d2
	real*8  c1_1st,c2_1st
	real*8  c1_indx,c2_indx
	integer nc1_size,nc2_size
	integer ip1,ip2,idp1,idp2,i,j,k

c	------------------------
c	COMPUTE 9-COEF. FOR AZ & RANGE RESAMPLING
c	------------------------

	c1_1st = c1_frame(1,1)
	c2_1st = c2_frame(1,1)
	nc1_size = nint((c1_frame(2,2)-c1_frame(1,1))/c1_pxl)
	nc2_size = nint((c2_frame(2,2)-c2_frame(1,1))/c2_pxl)

	call rsmplc2(p1_g,p2_g,c1_g_t,c2_g_t,mu1,mu2)
c	------------------------
c	INTERPOLATION PROCESSING
c	------------------------
	do i = 1, p1_len

	     dp1 = (i-1)*p1_pxl + p1_g(1,1) - p1_g(2,2)

	  do k=1,3
             e1_coef(k)=mu1(k,3)*dp1**2+mu1(k,2)*dp1+mu1(k,1)
             e2_coef(k)=mu2(k,3)*dp1**2+mu2(k,2)*dp1+mu2(k,1)
	  enddo

	do j = 1, p2_len

	     dp2 = (j-1)*p2_pxl + p2_g(1,1) - p2_g(2,2)

	     c1estm=e1_coef(3)*dp2**2+e1_coef(2)*dp2+e1_coef(1)+c1_g_t(2,2)
	     c2estm=e2_coef(3)*dp2**2+e2_coef(2)*dp2+e2_coef(1)+c2_g_t(2,2)

	     c1_indx = (c1estm-c1_1st)/c1_pxl+1
	     c2_indx = (c2estm-c2_1st)/c2_pxl+1
	
	     ip1 = c1_indx
	     ip2 = c2_indx

	     d1 = c1_indx - ip1	
	     d2 = c2_indx - ip2	
	
	     if (ip1.le.nc1_size-1.and.ip1.ge.1.and.
     *	         ip2.le.nc2_size-1.and.ip2.ge.1) then

cworst resolution               ivalue =
c    *          sqrt((btoi(ac_image(ip2,ip1)))**2*(1.-d2)*(1-d1)
c    *                    + (btoi(ac_image(ip2+1,ip1)))**2*d2*(1.-d1)
c    *                    + (btoi(ac_image(ip2,ip1+1)))**2*(1.-d2)*d1
c    *                    + (btoi(ac_image(ip2+1,ip1+1)))**2*d2*d1)

                ivalue = btoi(ac_image(ip2,ip1))*(1-d2)*(1-d1)
     *                   + btoi(ac_image(ip2+1,ip1))*d2*(1-d1)
     *                   + btoi(ac_image(ip2,ip1+1))*(1-d2)*d1
     *                   + btoi(ac_image(ip2+1,ip1+1))*d2*d1
	        if(ivalue.lt.0) ivalue = -1*ivalue
	        if(ivalue.gt.255) ivalue = 255
		data(j,i) = ivalue
	     else
	     data(j,i) = 0
	     end if

	end do 
	end do 


	return
	end

	subroutine rsmplc2(c1in,c2in,rin,fin,mu1,mu2)
	implicit real*8 (a-h,o-z)
	real*8 c1in(3,3),c2in(3,3),rin(3,3),fin(3,3)
	real*8 c1(3,3),c2(3,3),r(3,3),f(3,3),mu1(3,3),mu2(3,3)
	real a(9,10),x(9)

	n = 9
	do k1 = 1, 3
	do k2 = 1, 3
	c1(k1,k2) = c1in(k1,k2)-c1in(2,2)
	c2(k1,k2) = c2in(k1,k2)-c2in(2,2)
	r(k1,k2) = rin(k1,k2)-rin(2,2)
	f(k1,k2) = fin(k1,k2)-fin(2,2)
	end do
	end do
	do k1 = 1, 3
	do k2 = 1, 3
	a(k2+k1*3-3,1) = 1
	a(k2+k1*3-3,2) = c1(k1,k2)
	a(k2+k1*3-3,3) = c1(k1,k2)**2
	a(k2+k1*3-3,4) = c2(k1,k2)
	a(k2+k1*3-3,5) = c2(k1,k2)*c1(k1,k2)
	a(k2+k1*3-3,6) = c2(k1,k2)*c1(k1,k2)**2
	a(k2+k1*3-3,7) = c2(k1,k2)**2
	a(k2+k1*3-3,8) = c2(k1,k2)**2*c1(k1,k2)
	a(k2+k1*3-3,9) = c2(k1,k2)**2*c1(k1,k2)**2
	a(k2+k1*3-3,10) = r(k1,k2)
	end do
	end do
	call mats(a,x,n)
	do k1 = 1, 3
	do k2 = 1, 3
	mu1(k1,k2) = x(k2+k1*3-3)
	end do
	end do

	do k1 = 1, 3
	do k2 = 1, 3
	a(k2+k1*3-3,1) = 1
	a(k2+k1*3-3,2) = c1(k1,k2)
	a(k2+k1*3-3,3) = c1(k1,k2)**2
	a(k2+k1*3-3,4) = c2(k1,k2)
	a(k2+k1*3-3,5) = c2(k1,k2)*c1(k1,k2)
	a(k2+k1*3-3,6) = c2(k1,k2)*c1(k1,k2)**2
	a(k2+k1*3-3,7) = c2(k1,k2)**2
	a(k2+k1*3-3,8) = c2(k1,k2)**2*c1(k1,k2)
	a(k2+k1*3-3,9) = c2(k1,k2)**2*c1(k1,k2)**2
	a(k2+k1*3-3,10) = f(k1,k2)
	end do
	end do
	call mats(a,x,n)
	do k1 = 1, 3
	do k2 = 1, 3
	mu2(k1,k2) = x(k2+k1*3-3)
	end do
	end do

	return
	end

c	subroutine write_pj_image(dk_ptr0,nlines,nsamples,
c    *  pj_image,file_pj_image,istatus)
c	include 'key_const.inc'
c	include 'key_pp.inc'
C*****WAYNE******
c      include 'ssp2_const.inc'
c      byte     pj_image(sam_pj1,sam_pj2)
c      character*60     file_pj_image
c      integer*4     istatus
c      byte     pj_image_dummy(sam_pj1_pj2)
C*****WAYNE******
c	integer bytes,dk_ptr0,dk_ptr

c	write(6,*) 'dkptr', dk_ptr0, nlines, nsamples
c       ii=1
c       do i =1 ,nlines
c       do j =1 ,nsamples
c        pj_image_dummy(ii)=pj_image(j,i)
c        ii=ii+1
c       enddo
c       enddo
c 	dk_ptr= dk_ptr0
c	call write_disk(file_pj_image,pj_image_dummy,nsamples*nlines,dk_ptr,istatus)
c       if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return

        

c	return
c	end

	subroutine pack_send_pj(nlines,nsamples,pj_image,pj_image_dummy)
c	include 'key_const.inc'
c	include 'key_pp.inc'
C*****WAYNE******
       include 'ssp2_const.inc'
       byte     pj_image(sam_pj1,sam_pj2)
       byte     pj_image_dummy(sam_pj1_pj2)
C*****WAYNE******
	integer bytes,idest

        idest=0

        ii=1
        do i =1 ,nlines
        do j =1 ,nsamples
         pj_image_dummy(ii)=pj_image(j,i)
         ii=ii+1
        enddo
        enddo
        bytes=nsamples*nlines
        call mpi_send(pj_image_dummy,nsamples*nlines,MPI_BYTE,idest,1,MPI_COMM_WORLD,ierr)

	return
	end
	subroutine pack_receive_pj(file_pj_image,isource,nlines,nsamples,
     *                             pj_image,pj_image_dummy,iblk,res_prod,
     *                             file_pj_image_low,istatus)
c	include 'key_const.inc'
c	include 'key_pp.inc'
C*****WAYNE******
       include 'ssp2_const.inc'
       byte     pj_image(sam_pj1,sam_pj2)
       character*60     file_pj_image
       character*60     file_pj_image_low
       byte     pj_image_dummy(sam_pj1_pj2)
       integer istatus
C*****WAYNE******
	integer bytes,isource,nbytes
        integer dk_ptr,dk_ptr0,iblk
        integer res_prod


        if (isource.eq.0) then
         ii=1
         do i =1 ,nlines
         do j =1 ,nsamples
          pj_image_dummy(ii)=pj_image(j,i)
          ii=ii+1
         enddo
         enddo
        else
         bytes=nsamples*nlines
         call mpi_recv(pj_image_dummy,nsamples*nlines,MPI_BYTE,isource,1,MPI_COMM_WORLD,isuccess,ierr)
        endif

 

 	dk_ptr0 = iblk*nlines*nsamples
        call write_disk(file_pj_image,pj_image_dummy,nsamples*nlines,dk_ptr0,istatus)
        if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return

        if (res_prod.eq.600) then
                 call low_res_prod(iblk,nlines,nsamples,
     *          file_pj_image_low,pj_image_dummy,istatus)
                 if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
        endif


	return
	end
c***********************************************************************
c       Jet Propulsion Laboratory
c       Section:        334, Radar Science and Engineering
c       Preoject:       RadarSat
c       Task:           ScanSAR
c       Code:           Fortran 77
c-----------------------------------------------------------------------
c       Module Name: lambert_ll.f
c
c       Abstract:
c             convert Lambert Conformal Conic(Ellipsoid)
c             coordinates to lat/long
c
c       User-defined Input parameters:
c
c         1).tlat  - array of latitude coordinates
c         2).tlon  - array of longitude coordinates
c         3).plat1 - the first standard parallels
c         4).plat2 - the second standard parallels
c         5).lat0  - original lat
c         6).long0 - original long
c
c       Output parameters:
c
c         1).x - array of easting coordinates
c         2).y - array of northing coordinates
c
c
c Reference: Synder, J. P. 1984, Map projection used by the US
c            geological survey(bulletin 1532)
c
c------------------------------------------------------------------
c
c       6/9/95 Quyen Nguyen from "Map projection used by the US
c              geological survey"
c******************************************************************
        subroutine lambert_ll(x,y,lat,lat_d,lon,
     .                        plat1,plat2,lat0,long0)

        implicit none
        include 'ssp2_const.inc'
c       -------------
c       PASSING IN PARAMETERS
c       -------------
        real*8    x, y		!Lambert grid (east,north)=(x,y)
        real*8    plat1,plat2   !Two standard parallels
        real*8    lat0,long0    !lat and Long of the original point

c       -------------
c       PASSING OUT PARAMETERS
c       -------------
        real*8    lat_d         !Geodetic Latitude in degree
        real*8    lat		!Geocentric Latitude in degree
        real*8    lon		!Longitude in degree

c       -------------
c       LOCAL VARIABLES
c       -------------
        real*8    a		!Earth equatorial radius
        real*8    e             !Earth eccentricity 
        real*8    esq           ! square of Earth eccentricity
        real*8    m1, m2, t1, t2, t0
        real*8    n, F, rho_0, t, rho, theta, chi

c	-------------
c       READ from KEY_CONST.INC
c	-------------
        a = 6378206.4d0  ! mean equatorial radius in meters
        e = 0.0822719d0  ! earth eccentricity
        esq = ecc_2    ! read from ssp2_const.inc

c       -------------
c       Calculate the constants for Lambert Conformal Conic 
c       -------------
        m1 = dcosd(plat1)/sqrt(1.0 - (e*dsind(plat1))**2.)
        m2 = dcosd(plat2)/sqrt(1.0 - (e*dsind(plat2))**2.)

        t1 = dtand(45.0d0 - plat1/2.0d0)/(((1.0d0 - e*dsind(plat1))/
     .                    (1.0d0 + e*dsind(plat1)) )**(e/2.d0))
        t2 = dtand(45.0d0 - plat2/2.0d0)/(((1.0d0 - e*dsind(plat2))/
     .                    (1.0d0 + e*dsind(plat2)) )**(e/2.d0))
        t0 = dtand(45.0d0 - lat0/2.0d0)/(((1.0d0 - e*dsind(lat0))/
     .                    (1.0d0 + e*dsind(lat0)) )**(e/2.d0))
      
        n = log(m1/m2) / log(t1/t2)

        F = m1 / ( n * (t1**n) )

        rho_0 = a * F * (t0 ** n)

d       print*,' n=',n,' F=',F,' rho_0=',rho_0

c       -------------
c       Calculate for each point
c       ------------
 
        rho = sqrt(x*x + (rho_0 - y)**2)
        
c
c If n is negative, the signs of x,y and rho_0 must be reversed
c (see notes in "Map projections used by the USGS)
c
        theta = datand( x / (rho_0 - y) )

        lon   = theta / n + long0
        t  = (rho / (a * F))**(1.0/n)

d       print*,' rho=',rho
d       print*,' lon=',lon
d       print*,' t=',t

c use a series to calculate the inverse formula
        chi = 90.0d0 - 2 * datand(t)
        chi = chi * pi / 180.0d0    ! convert to radian
        lat_d = chi   
     .      +  ((e**2.d0)/2.d0 + 5.d0*(e**4.d0)/24.d0 + (e**6.d0)/12.d0)*dsin(2.d0*chi)
     .      +  (7.d0*(e**4.0d0)/48.d0 + 29.d0*(e**6.d0)/240.d0)*dsin(4.d0*chi)
     .      +  (7.d0*(e**6.d0)/120.d0)*dsin(6.d0*chi)


        lat_d = lat_d * 180.d0/pi    ! convert to deg
        lat  = datand((1-esq)*dtand(lat_d))

d       print*,' lat_d=',lat_d,' lat=',lat


      return
      end

c***********************************************************************
c       Jet Propulsion Laboratory
c       Section:        334, Radar Science and Engineering
c       Preoject:       RadarSat
c       Task:           ScanSAR
c       Code:           Fortran 77
c-----------------------------------------------------------------------
c       Module Name: ll_lambert.f
c
c       Abstract:
c             convert lat/lon to Lambert Conformal Conic(Ellipsoid)
c             coordinates (x:easting,y: northing)
c
c       User-defined Input parameters:
c
c         1).tlat  - array of latitude coordinates
c         2).tlon  - array of longitude coordinates
c         3).plat1 - the first standard parallels
c         4).plat2 - the second standard parallels
c         5).lat0  - original lat
c         6).long0 - original long
c
c       Output parameters:
c
c         1).x - array of easting coordinates
c         2).y - array of northing coordinates
c
c
c Reference: Synder, J. P. 1984, Map projection used by the US
c            geological survey(bulletin 1532)
c
c------------------------------------------------------------------
c
c       6/9/95 Quyen Nguyen from "Map projection used by the US
c              geological survey"
c******************************************************************
        subroutine ll_lambert(lat,lon,x,y,plat1,plat2,lat0,long0)

        implicit none
        include 'ssp2_const.inc'
c       -------------
c       PASSING IN PARAMETERS
c       -------------
        real*8    lat		!Latitude in degree
        real*8    lon		!Longitude in degree
        real*8    plat1,plat2   !Two standard parallels
        real*8    lat0,long0    !lat and Long of the original point

c       -------------
c       PASSING OUT PARAMETERS
c       -------------
        real*8    x, y		!Lambert grid (east,north)=(x,y)

c       -------------
c       LOCAL VARIABLES
c       -------------
        real*8    a		!Earth equatorial radius
        real*8    e             !Earth eccentricity 
        real*8    m1, m2, t1, t2, t0
        real*8    n, F, rho_0, t, rho, theta

c	-------------
c       READ from KEY_CONST.INC
c	-------------
        a = 6378206.4d0  ! mean equatorial radius in meters
        e = 0.0822719d0  ! earth eccentricity

c       -------------
c       Calculate the constants for Lambert Conformal Conic 
c       -------------
        m1 = dcosd(plat1)/sqrt(1.0d0 - (e*dsind(plat1))**2.d0)
        m2 = dcosd(plat2)/sqrt(1.0d0 - (e*dsind(plat2))**2.d0)

        t1 = dtand(45.0d0 - plat1/2.0d0)/(((1.0d0 - e*dsind(plat1))/
     .                       (1.0d0 + e*dsind(plat1)) )**(e/2.d0))
        t2 = dtand(45.0d0 - plat2/2.0d0)/(((1.0d0 - e*dsind(plat2))/
     .                       (1.0d0 + e*dsind(plat2)) )**(e/2.d0))
        t0 = dtand(45.0d0 - lat0/2.0d0)/(((1.0d0 - e*dsind(lat0))/
     .                       (1.0d0 + e*dsind(lat0)) )**(e/2.d0))


d       print*,' t1=',t1,' t2=',t2,' t0=',t0
      
        if( plat1.ne.plat2) then
           n = log(m1/m2) / log(t1/t2)
        else
           n = dsind(plat1)
        end if 

        F = m1 / ( n * (t1**n) )

        rho_0 = a * F * (t0 ** n)

d       print*,' n=',n,' F=',F,' rho_0=',rho_0

c       -------------
c       Calculate for each point
c       ------------
 
        t = dtand(45.0d0 - lat/2.0d0)/(((1.0d0 - e*dsind(lat))/
     .                   (1.0d0 + e*dsind(lat)) )**(e/2.d0))
        rho = a * F * ( t ** n)
        theta = n * (lon - long0)

        x = rho * dsind(theta)
        y = rho_0 - rho * dcosd(theta)

d       print*,' x=',x,' y=',y

      return
      end


