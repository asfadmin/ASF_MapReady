c SccsId = @(#)circ_shft.f	2.41 3/24/98
 	subroutine circ_shft(ir_st, ix_st, nadd_error, 
     +  rng_in,az_in,nadd,n_max,pi,c,pxl_unit,
     +  ers,jers,rsat_s12,rsat_s37,rsat_ws,rsat_fr,rsat_eh,rsat_el,
     +  buff_in,array,buff_in_rd,
     +  mode,
     +  raw_mode,i_dop_mode,nband,nlines,line_offset,
     +  ns,nr_header,ns_header,i_squint,i_pt_sim,i_data_type,nlook,
     +  nfr,nfx,icomplex,i_preproc,i_iqqi,nx_offset,i_dummy,
     +  lambda,fc,bw,dmu,fs,plen,k_chirp,dr,
     +  prf,prf_in,pbw,az_ang,az_res,df,
     +  t_1st_echo,t_eph,t_gha,gha,xs,ys,zs,rsc,vxs,vys,vzs,vs,
     +  re_mean,h,
     +  rs_ref,r0_base,fd0_base,fr0_base,fd0,fr0,fdd,frd,
     +  fd_const,r_ref,r_1st,
     +  pxl_spacing,nx_spacing,
     +  t_start_frm,t_end_frm,
     +  lat1,lon1,rs_crn1,rg_crn1,lookang1,
     +  lat2,lon2,rs_crn2,rg_crn2,lookang2,
     +  lat3,lon3,rs_crn3,rg_crn3,lookang3,
     +  lat4,lon4,rs_crn4,rg_crn4,lookang4,
     +  lat5,lon5,rs_crn5,rg_crn5,lookang5,
     +  raw_file,image_file)


        implicit none

	include '/usr/lpp/ppe.poe/include/mpif.h'

        character*128 SccsId_circ_shft
        data SccsId_circ_shft
     +  /'@(#)PPcirc_shft.f:2.41'/

        integer ierror, nadd_error

        integer rng_in              !# rng samples procesed each block
	integer az_in               !# az samples procesed each block
	integer nadd                !extra az spectr sampls for fd drift 
	integer n_max               !n_max = max(rng_in,az_in)
	real*8 pi		    !constant pai
	real*8 c                    !speed of light
	real*8 pxl_unit		    !pxl_unit = 6.25 meter
	integer ers
	integer jers
	integer rsat_s12
	integer rsat_s37
	integer rsat_ws
	integer rsat_fr
	integer rsat_eh
	integer rsat_el
	complex buff_in(rng_in,az_in+nadd),array(n_max+nadd)
	complex buff_in_rd(rng_in,az_in)
	integer mode                !airsar (1) or spaceborne sar (2) modes
	integer raw_mode            !raw data in real(1) or complex(2)
	integer i_dop_mode	    !process (1) zero dop,(2)const,(3) centroid
	integer nband		    !# of frequency samples processed
	integer nlines              !total # of az. lines to be procsed
	integer line_offset	    !processing start at line_offset+1-th line
	integer ns                  !total # of complex samples per echo
	integer nr_header	    !# of samples in header of each echo pulse
	integer ns_header           !total # of complex samples with header
	integer i_squint	    !1 = squint mode, 0 = none squint
	integer i_pt_sim	    !point target simulation
	integer i_data_type	    !processing data type
	integer nlook               !total # of azimuth looks
	integer nfr		    !image frame size in range
	integer nfx		    !image frame size inazimuth
	integer icomplex	    !1 = output complex image, 0 = real image
	integer i_preproc    	    !1 = preprocessing on going
	integer i_iqqi		    !0 = i first, 1 = q first
	integer nx_offset	    !azimuth offset to adjust 1st image pixel
	integer i_dummy
	real*8 lambda               !wave length 
	real*8 fc                   !radar center frequency (carrier)
	real*8 bw                   !range bandwidth
	real*8 dmu                  !range freq. sample spacing in spectrum
	real*8 fs                   !range sampling frequency
	real*8 plen                 !time interval of radar pulse
	real*8 k_chirp              !radar pulse frequency rate
	real*8 dr                   !range sampling spacing
	real*8 prf                  !pulse repetition frequency 
	real*8 prf_in               !pulse repetition frequency input
	real*8 pbw                  !the processing bandwidth
	real*8 az_ang               !azimuth beam angle
	real*8 az_res               !image azimuth resolution
	real*8 df                   !doppler sample spacing
	real*8 t_1st_echo 	    !time associated with first echo pulse
	real*8 t_eph		    !time associated with given ephemeris
	real*8 t_gha		    !time associated with gha
	real*8 gha		    !longitude reference
	real*8 xs		    !sensor position, x-dimension	
	real*8 ys		    !sensor position, y-dimension	
	real*8 zs		    !sensor position, z-dimension	
	real*8 rsc		    !sensor to earth center distance
	real*8 vxs                  !spacecraft speed in x
	real*8 vys                  !spacecraft speed in y
	real*8 vzs                  !spacecraft speed in z
	real*8 vs                   !spacecraft speed
	real*8 re_mean		    !mean earth radius at local
	real*8 h		    !local mean elevation above ellipsoid surface
	real*8 rs_ref               !slant rng of the 1st range sample in frame
	real*8 r0_base              !slant rng of the 1st input range sample
	real*8 fd0_base             !doppler centroid of the 1st rng sample
	real*8 fr0_base             !doppler rate of the 1st rng sample
	real*8 fd0                  !dop cent of 1st rng sample in block
	real*8 fr0                  !dop rate of 1st rng sample in block
	real*8 fdd                  !fd change per meter in cross-track
	real*8 frd                  !fr change per meter in cross-track
	real*8 fd_const
	real*8 r_ref                !range reference for chirp scaling
	real*8 r_1st                !slant rng of 1st input range sample in block
	real*8 pxl_spacing          !image pixel spacing for ground projection
	real*8 nx_spacing	    !nature spacing in azimuth
	real*8 t_start_frm 	    !time associated with first image frame line
	real*8 t_end_frm 	    !time associated with last image frame line
	real*8 lat1		    !lat of frame corner 1, near range 1st image line
	real*8 lon1		    !lat of frame corner 1, near range 1st image line
	real*8 rs_crn1		    !slant range of frame corner 1, near range 1st image line
	real*8 rg_crn1		    !ground range of frame corner 1, near range 1st image line
	real*8 lookang1		    !look angle of frame corner 1, near range 1st image line
	real*8 lat2		    !lat of frame corner 2, far range 1st image line
	real*8 lon2		    !lat of frame corner 2, far range 1st image line
	real*8 rs_crn2		    !slant range of frame corner 2, near range 1st image line
	real*8 rg_crn2		    !ground range of frame corner 2, near range 1st image line
	real*8 lookang2		    !look angle of frame corner 2, near range 1st image line
	real*8 lat3		    !lat of frame corner 3, near range last image line
	real*8 lon3		    !lat of frame corner 3, near range last image line
	real*8 rs_crn3		    !slant range of frame corner 3, near range last image line
	real*8 rg_crn3		    !ground range of frame corner 3, near range last image line
	real*8 lookang3		    !look angle of frame corner 3, near range last image line
	real*8 lat4		    !lat of frame corner 4, far range last image line
	real*8 lon4		    !lat of frame corner 4, far range last image line
	real*8 rs_crn4		    !slant range of frame corner 4, near range last image line
	real*8 rg_crn4		    !ground range of frame corner 4, near range last image line
	real*8 lookang4		    !look angle of frame corner 4, near range last image line
	real*8 lat5		    !lat of frame center
	real*8 lon5		    !lat of frame center
	real*8 rs_crn5		    !slant range of frame center
	real*8 rg_crn5		    !ground range of frame center
	real*8 lookang5		    !look angle of frame center
	character*64 raw_file	    !raw data file name
	character*64 image_file	    !image data file name

	real*8 r,fd_ref,fd_ref_base,r_far,fd_ref_far
	real*8 t1,t1_near,t1_far,t_period,t2
	complex temp(4096,4096)   !8192 for az_in

	real fd_far,fr_far,fr_min,t_valid,t1_min,fd,fr
	integer naz,ix_st,ir_st,nx_start,i,nv_end,j,j1,num_valid
	integer nv_start,ng_front,ng_end, ns_total


        if(rng_in.gt.4096) then
           print *,'circ_shift: rng_in greater than 4096' 
           print *,'circ_shift: increase 1st dimension of array temp'
	   call mpi_abort(mpi_comm_world,22,ierror)
           call exit(22)
        end if

        if(az_in.gt.4096) then
           print *,'circ_shift: az_in greater than 4096' 
           print *,'circ_shift: increase 2nd dimension of array temp'
	   call mpi_abort(mpi_comm_world,22,ierror)
           call exit(22)
        end if

        if(nadd.gt.4096) then
           print *,'circ_shift: nadd greater than 4096' 
           print *,'circ_shift: increase 2nd dimension of array temp'
	   call mpi_abort(mpi_comm_world,22,ierror)
           call exit(22)
        end if

        r_far = r0_base+dr*(ns-plen*fs)
        fd_far = fd0_base+fdd*(r_far-r0_base)
        fr_far = fr0_base+frd*(r_far-r0_base)
        call dop_ref(i_dop_mode,fd0_base,fdd,r0_base,fd_const,r0_base,
     1       fd_ref_base)
        call dop_ref(i_dop_mode,fd0_base,fdd,r_far,fd_const,r0_base,
     1       fd_ref_far)
        fr_min = fr_far
        
        naz = az_in
c        if(ix_st+az_in-1.gt.nlines) naz = nlines-ix_st+1
        t_valid = naz/prf-pbw/abs(fr_min)
        
        t1_near = -t_valid/2. + (fd0_base-fd_ref_base)/abs(fr0_base)
        t1_far = -t_valid/2. + (fd_far-fd_ref_far)/abs(fr_far)

        t1_min = min(t1_near,t1_far)
        T_period = az_in/prf
        nx_start = mod(t1_min+100.5*T_period,T_period)*prf+1
        

        nadd_error = 0

        if(abs(t1_near-t1_far)*prf.gt.pbw/abs(fr_min)*prf+nadd) then
           print *, 'circ_shift: abs(t1_near-t1_far)*prf .gt.
     + pbw/abs(fr_min)*prf+nadd.'
           print *, 'current nadd is ',nadd, 
     +     ' need to increase nadd by ',
     + nint((abs(t1_near-t1_far)*prf)-(pbw/abs(fr_min)*prf+nadd))
           nadd_error = 1
           return
        end if


c start loop

        do j = 1, az_in
           do i = 1, rng_in - plen*fs
              temp(i,j) = buff_in_rd(i,j)
           end do
        end do
        
        do j = 1, az_in
           j1 = mod(j-1+nx_start-1,az_in)+1
           do i = 1, rng_in - plen*fs
              buff_in(i,j) = temp(i,j1)  
           end do
        end do

        do j = 1,nadd
           do i = 1, rng_in - plen*fs
              temp(i,j) = buff_in(i,j)
           end do
        end do

        do j = 1,nadd
           do i = 1, rng_in - plen*fs
              buff_in(i,j+az_in) = temp(i,j)
           end do
        end do
           

        ns_total = rng_in - plen*fs
        if(ir_st-1 + ns_total .gt. ns - plen*fs) 
     +       ns_total = ns - plen*fs - ir_st +1

        do i = 1, ns_total
           r = r_1st+(i-1)*dr
           call dop_ref(i_dop_mode,fd0,fdd,r,fd_const,r_1st,fd_ref)
           
           fd = fd0 + fdd * (r - r_1st)
           fr = fr0 + frd * (r - r_1st)
           
           t1 = -t_valid/2. + (fd-fd_ref)/abs(fr)
           t2 = t1 + t_valid
           nv_start = mod(t1+100.5*T_period,T_period)*prf+1
           nv_end = mod(t2+100.5*T_period,T_period)*prf+1
           

           ng_front = mod(nv_start-nx_start+az_in,az_in) 
           ng_end = mod(nv_end-nx_start+az_in,az_in)
           if(ng_end.lt.ng_front) ng_end = ng_front + t_valid*prf+4

           do j = 1, ng_front-1
              buff_in(i,j) = cmplx(0.,0.) 
           end do

           do j = ng_end+1, az_in+nadd
              buff_in(i,j) = cmplx(0.,0.) 
           end do


        end do

c set buff beyond ns_total to zero
        do i = ns_total + 1, rng_in
           do j = 1, az_in + nadd
              buff_in(i,j) = cmplx(0.,0.)
           end do
        end do


        return
        end
