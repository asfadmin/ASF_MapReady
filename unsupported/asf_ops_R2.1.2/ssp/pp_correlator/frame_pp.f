c SccsId = @(#)frame_pp.f	2.41 3/24/98
	subroutine frame_pp(delta_t1,delta_t2,
     +  image_mean_earth_radius,nadir_mean_earth_radius,yaw,
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
     +  raw_file,image_file,frame_mode)


        implicit none

        character*128 SccsId_frame_pp
        data SccsId_frame_pp
     +  /'@(#)PPframe_pp.f:2.41'/


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
	real*8 yaw
	character*64 raw_file	    !raw data file name
	character*64 image_file	    !image data file name
	integer frame_mode          !left or right looking


        real*8 vxg4
        real*8 vyg4
        real*8 zg4
	real*8 xg4
        real*8 yg4
        real*8 vzg4
        real*8 delta_t3
        real*8 xs3
        real*8 t_center
        real*8 yaw4
        real*8 lat4_c
        real*8 fd_crn4
        real*8 xg3
        real*8 yg3
        real*8 fd_crn3
        real*8 vys2_bf
        real*8 vzs2_bf
        real*8 zg3
        real*8 yaw3
        real*8 lat3_c
        real*8 vzg3
        real*8 vxg3
        real*8 vyg3
        real*8 ys3
        real*8 vyg5
        real*8 vzg5
        real*8 vxg5
        real*8 yg5
        real*8 zg5
        real*8 yaw5
        real*8 del
        real*8 fr0_bas
        real*8 delta_t
        real*8 lat5_c
        real*8 xg5
        real*8 vzs3
        real*8 xs3_bf
        real*8 vys3
        real*8 zs3
        real*8 vxs3
        real*8 ys3_bf
        real*8 vzs3_bf
        real*8 fd_crn5
        real*8 vys3_bf
        real*8 zs3_bf
        real*8 vxs3_bf
        real*8 vxs2_bf
        real*8 vzs1_bf
        real*8 fd_crn1
        real*8 vys1_bf
        real*8 zs1_bf
        real*8 vxs1_bf
        real*8 px
        real*8 yg1
        real*8 zg1
        real*8 xg1
        real*8 py
        real*8 pz
        real*8 ys1_bf
        real*8 delta_t1
        real*8 xs1
        real*8 t_spacing
        real*8 t_offset
        real*8 t_frame
        real*8 ys1
        real*8 vzs1
        real*8 xs1_bf
        real*8 vys1
        real*8 zs1
        real*8 vxs1
        real*8 vxg1
        real*8 ys2
        real*8 zs2
        real*8 xs2
        real*8 lat2_c
        real*8 delta_t2
        real*8 vxs2
        real*8 ys2_bf
        real*8 zs2_bf
        real*8 xs2_bf
        real*8 vys2
        real*8 vzs2
        real*8 yaw2
        real*8 lat1_c
        real*8 fd_crn2
        real*8 yaw1
        real*8 vyg1
        real*8 vzg1
        real*8 xg2
        real*8 vyg2
        real*8 vzg2
        real*8 vxg2
        real*8 yg2
        real*8 zg2

	real*8 fr_far,r_far

	integer nr_total
	real*8 r_center,rg_center,vsg

	real*8 nadir_lat_c, nadir_lon, nadir_lat
	real*8 nadir_x, nadir_y, nadir_z, nadir_vx,nadir_vy, nadir_vz
	real*8 nadir_mean_earth_radius, image_mean_earth_radius

	real*8 t1_near,t_valid,t_diff,t1_far,fd_ref_base,fd_far
	real*8 fr_min,fd_ref_far
	integer naz

c... added the following to calculate nx_spacing, so that we can call 
c... frame_pp directly without calling az_resample first.
	nr_total = ns - (plen*fs)
	r_center = r0_base + dr*(nr_total/2)
	call sr2gr(r_center,rg_center,
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
	vsg = vs * re_mean*cos(rg_center/re_mean)/rsc
	nx_spacing = vsg/prf		!natural spacing in azimuth 


c... original code starts from here

        r_far = r0_base+dr*(ns-plen*fs)
        fd_far = fd0_base+fdd*(r_far-r0_base)
        fr_far = fr0_base+frd*(r_far-r0_base)
        call dop_ref(i_dop_mode,fd0_base,fdd,r0_base,fd_const,r0_base,
     1       fd_ref_base)
        call dop_ref(i_dop_mode,fd0_base,fdd,r_far,fd_const,r0_base,
     1       fd_ref_far)
        fr_min = fr_far
        
        naz = az_in
        t_valid = naz/prf-pbw/abs(fr_min)
        
        t1_near = -t_valid/2. + (fd0_base-fd_ref_base)/abs(fr0_base)
        t1_far = -t_valid/2. + (fd_far-fd_ref_far)/abs(fr_far)

	if(t1_near .lt. t1_far) then
	   t_diff = (fd0_base - fd_ref_base)/abs(fr0_base)
	else
	   t_diff = (fd_far - fd_ref_far)/abs(fr_far)
	end if


	t_offset = abs(pbw/fr_far)/2

	t_offset = t_offset + t_diff

        print *, '(pbw/fr_far)/2 =',abs(pbw/fr_far)/2
        print *,' t_diff =',t_diff
        print*,' t_offset=',t_offset

c... standard and ramp  icomplex = 1 or 2 
	if(icomplex.eq.0 .or. icomplex.eq.2) then
	t_frame = nfx*(pxl_spacing/nx_spacing)/prf
	else
	t_frame = nfx/prf
	end if

	t_spacing = 2.e-4

        t_start_frm = t_1st_echo+t_offset
	delta_t1 = t_offset

	call propa_simp(xs,ys,zs,vxs,vys,vzs,
     *            delta_t1,t_spacing,xs1,ys1,zs1,vxs1,vys1,vzs1)
c	call propagation(xs,ys,zs,vxs,vys,vzs,
c     *            t_eph,delta_t1,xs1,ys1,zs1,vxs1,vys1,vzs1)

        call eme_to_ebf(gha,t_gha,t_start_frm,xs1,ys1,zs1,
     1		xs1_bf,ys1_bf,zs1_bf)
        call eme_to_ebf(gha,t_gha,t_start_frm,vxs1,vys1,vzs1,
     1		vxs1_bf,vys1_bf,vzs1_bf)
	rs_crn1 = max(r0_base,rs_ref)

        call sr2gr(rs_crn1,rg_crn1,
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
  
	if(i_dop_mode .eq. 1) fd_crn1 = 0.0
	if(i_dop_mode .eq. 2) fd_crn1 = fd_const
	if(i_dop_mode .eq. 3) fd_crn1 = fd0_base+fdd*(rs_crn1-r0_base)

	call gttarg1(rs_crn1,fd_crn1,lambda,
     1		   xs1_bf,ys1_bf,zs1_bf,vxs1_bf,vys1_bf,vzs1_bf,h,
     1             px,py,pz,xg1,yg1,zg1,vxg1,vyg1,vzg1,lookang1,yaw1,
     1             frame_mode)
	call ebf_to_ll(xg1,yg1,zg1,lat1_c,lat1,lon1)

	if(icomplex.eq.0) then
        rg_crn2 = rg_crn1 + pxl_spacing*(nfr-1)

        call gr2sr(rg_crn2,rs_crn2,
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
  
	else
        rs_crn2 = rs_crn1 + dr*(nfr-1)

        call sr2gr(rs_crn2,rg_crn2,
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

	end if

	if(i_dop_mode .eq. 1) fd_crn2 = 0.0
	if(i_dop_mode .eq. 2) fd_crn2 = fd_const
	if(i_dop_mode .eq. 3) fd_crn2 = fd0_base+fdd*(rs_crn2-r0_base)

	call gttarg1(rs_crn2,fd_crn2,lambda,
     1		    xs1_bf,ys1_bf,zs1_bf,vxs1_bf,vys1_bf,vzs1_bf,h,
     1             px,py,pz,xg2,yg2,zg2,vxg2,vyg2,vzg2,lookang2,yaw2,
     1             frame_mode)
	call ebf_to_ll(xg2,yg2,zg2,lat2_c,lat2,lon2)

        t_end_frm = t_1st_echo+t_offset+t_frame
	delta_t2 = t_offset + t_frame


	call propa_simp(xs,ys,zs,vxs,vys,vzs,
     *            delta_t2,t_spacing,xs2,ys2,zs2,vxs2,vys2,vzs2)
c	call propagation(xs,ys,zs,vxs,vys,vzs,
c     +            t_eph,delta_t2,xs2,ys2,zs2,vxs2,vys2,vzs2)
        call eme_to_ebf(gha,t_gha,t_end_frm,xs2,ys2,zs2,
     1		xs2_bf,ys2_bf,zs2_bf)
        call eme_to_ebf(gha,t_gha,t_end_frm,vxs2,vys2,vzs2,
     2		vxs2_bf,vys2_bf,vzs2_bf)

	rs_crn3 = rs_crn1
	rg_crn3 = rg_crn1
	rs_crn4 = rs_crn2
	rg_crn4 = rg_crn2


	if(i_dop_mode .eq. 1) fd_crn3 = 0.0
	if(i_dop_mode .eq. 2) fd_crn3 = fd_const
	if(i_dop_mode .eq. 3) fd_crn3 = fd0_base+fdd*(rs_crn3-r0_base)

	call gttarg1(rs_crn3,fd_crn3,lambda,
     1		   xs2_bf,ys2_bf,zs2_bf,vxs2_bf,vys2_bf,vzs2_bf,h,
     1             px,py,pz,xg3,yg3,zg3,vxg3,vyg3,vzg3,lookang3,yaw3,
     1             frame_mode)
	call ebf_to_ll(xg3,yg3,zg3,lat3_c,lat3,lon3)

	if(i_dop_mode .eq. 1) fd_crn4 = 0.0
	if(i_dop_mode .eq. 2) fd_crn4 = fd_const
	if(i_dop_mode .eq. 3) fd_crn4 = fd0_base+fdd*(rs_crn4-r0_base)

	call gttarg1(rs_crn4,fd_crn4,lambda,
     1		   xs2_bf,ys2_bf,zs2_bf,vxs2_bf,vys2_bf,vzs2_bf,h,
     1             px,py,pz,xg4,yg4,zg4,vxg4,vyg4,vzg4,lookang4,yaw4,
     1             frame_mode)
	call ebf_to_ll(xg4,yg4,zg4,lat4_c,lat4,lon4)

	t_center =  (t_start_frm + t_end_frm) /2.
	delta_t3 = (delta_t2+delta_t1)/2.0

	call propa_simp(xs,ys,zs,vxs,vys,vzs,
     *            delta_t3,t_spacing,xs3,ys3,zs3,vxs3,vys3,vzs3)
c	call propagation(xs,ys,zs,vxs,vys,vzs,
c     *            t_eph, delta_t3,xs3,ys3,zs3,vxs3,vys3,vzs3)
        call eme_to_ebf(gha,t_gha,t_center,xs3,ys3,zs3,
     1		xs3_bf,ys3_bf,zs3_bf)
        call eme_to_ebf(gha,t_gha,t_center,vxs3,vys3,vzs3,
     1		vxs3_bf,vys3_bf,vzs3_bf)
	rs_crn5 = (rs_crn1+rs_crn2)/2.

	if(i_dop_mode .eq. 1) fd_crn5 = 0.0
	if(i_dop_mode .eq. 2) fd_crn5 = fd_const
	if(i_dop_mode .eq. 3) fd_crn5 = fd0_base+fdd*(rs_crn5-r0_base)

	call gttarg1(rs_crn5,fd_crn5,lambda,
     1		   xs3_bf,ys3_bf,zs3_bf,vxs3_bf,vys3_bf,vzs3_bf,h,
     1             px,py,pz,xg5,yg5,zg5,vxg5,vyg5,vzg5,lookang5,yaw5,
     1             frame_mode)
	call ebf_to_ll(xg5,yg5,zg5,lat5_c,lat5,lon5)

	yaw = yaw5

c******* mean earth radius at image center and nadir earth radius *******
	image_mean_earth_radius = sqrt(xg5**2+yg5**2+zg5**2)
	call ebf_to_ll(xs3_bf,ys3_bf,zs3_bf,nadir_lat_c, 
     1                 nadir_lat, nadir_lon)
	call get_tv(nadir_lat_c, nadir_lon, nadir_lat, h,
     1    nadir_x, nadir_y, nadir_z, nadir_vx, nadir_vy, nadir_vz)
	nadir_mean_earth_radius = sqrt(nadir_x**2+nadir_y**2+nadir_z**2)

c*******these times should be reported without the offset
c qdn 2/18/98 do not reset the frame starting time
c       t_start_frm = t_1st_echo
c       t_end_frm = t_1st_echo + t_frame

	delta_t1 = delta_t1 - t_offset
	delta_t2 = delta_t2 - t_offset
	
        print *, 't_1st_echo=',t_1st_echo
        print *, 't_start_frm=',t_start_frm
        print *, 't_offset =',t_offset
        print *,' t_diff =',t_diff
	print *, 'image pixel spacing =  ', pxl_spacing
	print *, 'image frame size in along-track (km) =  ', 
     +    pxl_spacing*nfx/1000.
	print *, 'image frame size in cross-track (km) =  ', 
     +    pxl_spacing*nfr/1000.
	print *, 'image frame size in pixels =  ', nfx
	print *, 'image frame size in lines =  ', nfr
	if(icomplex.eq.1) print *, 'number of bytes per image line =  ', 
     +    nfr*4
	if(icomplex.eq.0) print *, 'number of bytes per image line =  ', 
     +    nfr
	if(icomplex.eq.2) print *, 'number of bytes per image line =  ', 
     +    nfr*2
	print *, 'image frame size in pixels =  ', nfx
	print *, '                         '
	print *, 'time of start of image (relative to t_1st_echo) =  ', 
     +    delta_t1
	print *, 'time of end of image (relative to t_1st_echo) =  ', 
     +    delta_t2
	print *, 'time of image center (rel. to t_1st_echo) =  ', 
     +    (delta_t2+delta_t1)/2.
	print *, '                         '
	print *, 'prf =  ', prf
	print *, 'slant range of 1st range sample =  ', r0_base
	print *, 'doppler centroid of 1st range sample =  ', fd0_base
	print *, 'doppler centroid change per meter =  ', fdd
	print *, 'doppler frequency rate of 1st range sample =  ', 
     +    fr0_base
	print *, 'doppler rate change per meter =  ', frd
	print *, '                         '
	print *, 'slant range of 1st corner point', rs_crn1	
	print *, 'ground range of 1st corner point', rg_crn1	
	print *, 'look angle of 1st corner point', lookang1	
	print *, 'latitud of 1st corner point', lat1	
	print *, 'longitude of 1st corner point', lon1	
	print *, '                         '
	print *, 'slant range of 2nd corner point', rs_crn2	
	print *, 'ground range of 2nd corner point', rg_crn2	
	print *, 'look angle of 2nd corner point', lookang2
	print *, 'latitud of 2nd corner point', lat2	
	print *, 'longitude of 2nd corner point', lon2	
	print *, '                         '
	print *, 'slant range of 3rd corner point', rs_crn3	
	print *, 'ground range of 3rd corner point', rg_crn3	
	print *, 'look angle of 3rd corner point', lookang3	
	print *, 'latitud of 3rd corner point', lat3	
	print *, 'longitude of 3rd corner point', lon3	
	print *, '                         '
	print *, 'slant range of 4th corner point', rs_crn4	
	print *, 'ground range of 4th corner point', rg_crn4	
	print *, 'look angle of 4th corner point', lookang4	
	print *, 'latitud of 4th corner point', lat4	
	print *, 'longitude of 4th corner point', lon4	
	print *, '                         '
	print *, 'slant range of center point', rs_crn5	
	print *, 'ground range of center point', rg_crn5	
	print *, 'look angle of center point', lookang5	
	print *, 'latitud of center point', lat5	
	print *, 'longitude of center point', lon5	
	print *, '                         '
	print *, 'image_mean_earth_radius', image_mean_earth_radius
	print *, 'nadir_mean_earth_radius', nadir_mean_earth_radius
	print *, 'nadir_latitude, nadir_longitude', nadir_lat, nadir_lon

	return
	end
