c SccsId = @(#)r_fd_fr_init.f	2.41 3/24/98
	

	subroutine r_fd_fr_init(
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
     +  raw_file,image_file, frame_mode)


        implicit none

        character*128 SccsId_r_fd_fr_init
        data SccsId_r_fd_fr_init
     +  /'@(#)PPr_fd_fr_init.f:2.41'/


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
	integer frame_mode

	real*8 lookangn,lookangf

	real*8 vys1,vxs1,zs1,vzs1,zs1_bf,ys1_bf,xs1_bf,ys1
	real*8 vxgf,zgf,ygf,vygf,xs1,yawf,vzgf,xs3,rst_n_3
	real*8 rst_n_2,rst_n_1,rst_f_1,fr_next,rst_f_3,rst_f_2
	real*8 zs3_bf,vxs3,zs3,ys3,vys3,ys3_bf,xs3_bf,vzs3,vxs2
	real*8 zs2,ys2,vys2,ys2_bf,xs2_bf,vzs2,xs2,rnext,r_extend
	real*8 dt,t_frame,delta_t,t_center,t_spacing,zs2_bf,vygn
	real*8 vxgn,zgn,vzgn,xgf,fd_next,yawn,ygn,vzs2_bf,vys2_bf
	real*8 vxs2_bf,px,xgn,pz,py

	dt = 0.1
	r_extend = 10000.
	rnext = r0_base + r_extend

	t_frame = nlines/prf
	t_spacing = 2.e-4

c t_eph is the time when state vector is measured. xs,ys,zs,vxs,vys,vzs
c is state vector at t_1st_echo 
        t_center = t_frame/2

        delta_t = t_center

	call propa_simp(xs,ys,zs,vxs,vys,vzs,
     *            delta_t,t_spacing,xs2,ys2,zs2,vxs2,vys2,vzs2)
        call eme_to_ebf(gha,t_gha,t_center,xs2,ys2,zs2,
     1		xs2_bf,ys2_bf,zs2_bf)
        call eme_to_ebf(gha,t_gha,t_center,vxs2,vys2,vzs2,
     1		vxs2_bf,vys2_bf,vzs2_bf)

c... initial guess for the look angles

	lookangn = lookang1
	lookangf = lookang1

	call gttarg2(r0_base,fd0_base,lambda,
     1		   xs2_bf,ys2_bf,zs2_bf,vxs2_bf,vys2_bf,vzs2_bf,h,
     1             px,py,pz,xgn,ygn,zgn,vxgn,vygn,vzgn,lookangn,yawn,
     1             frame_mode)

	call gttarg2(rnext,fd_next,lambda,
     1		   xs2_bf,ys2_bf,zs2_bf,vxs2_bf,vys2_bf,vzs2_bf,h,
     1             px,py,pz,xgf,ygf,zgf,vxgf,vygf,vzgf,lookangf,yawf,
     1             frame_mode)


        delta_t = t_center - dt
	call propa_simp(xs,ys,zs,vxs,vys,vzs,
     *            delta_t,t_spacing,xs1,ys1,zs1,vxs1,vys1,vzs1)
        call eme_to_ebf(gha,t_gha,t_center,xs1,ys1,zs1,
     1		xs1_bf,ys1_bf,zs1_bf)

        delta_t = t_center + dt
	call propa_simp(xs,ys,zs,vxs,vys,vzs,
     *            delta_t,t_spacing,xs3,ys3,zs3,vxs3,vys3,vzs3)
        call eme_to_ebf(gha,t_gha,t_center,xs3,ys3,zs3,
     1		xs3_bf,ys3_bf,zs3_bf)

	rst_n_1 = sqrt((xs1_bf-xgn)**2+(ys1_bf-ygn)**2+(zs1_bf-zgn)**2)
	rst_n_2 = sqrt((xs2_bf-xgn)**2+(ys2_bf-ygn)**2+(zs2_bf-zgn)**2)
	rst_n_3 = sqrt((xs3_bf-xgn)**2+(ys3_bf-ygn)**2+(zs3_bf-zgn)**2)
	rst_f_1 = sqrt((xs1_bf-xgf)**2+(ys1_bf-ygf)**2+(zs1_bf-zgf)**2)
	rst_f_2 = sqrt((xs2_bf-xgf)**2+(ys2_bf-ygf)**2+(zs2_bf-zgf)**2)
	rst_f_3 = sqrt((xs3_bf-xgf)**2+(ys3_bf-ygf)**2+(zs3_bf-zgf)**2)

	fr0_base = 2*(2*rst_n_2-rst_n_1-rst_n_3)/(lambda*dt**2)
c	print *, 'fr0_base = ', fr0_base
	fr_next = 2*(2*rst_f_2-rst_f_1-rst_f_3)/(lambda*dt**2)
	frd = (fr_next - fr0_base)/r_extend
	fdd = (fd_next - fd0_base)/r_extend


	return
	end



