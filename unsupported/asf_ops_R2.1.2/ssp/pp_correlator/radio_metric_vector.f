c SccsId = @(#)radio_metric_vector.f	2.41 3/24/98
c This routine computes the radiometric compensation vector in
c the slant or ground range dimension at the center of the image

      subroutine radio_metric_vector(rdm_vec,
     +  look_ang_cnt, ele_ang_ref, dang_rg, 
     +  ant_patn,
     +  ant_gain_vect, look_ang_vect, frame_mode,
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

        implicit   none

        character*128 SccsId_radio_metric_vector
        data SccsId_radio_metric_vector
     +  /'@(#)PPradio_metric_vector.f:2.41'/


c Input parameters
        integer npt
        real*4  ant_patn(700)
        real*8  look_ang_cnt, ele_ang_ref, dang_rg
c Output vector
        real*8  rdm_vec(nfr)
        integer frame_mode

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

c Local variables
     
      real*8     px, py, pz, xt, yt, zt, vxt, vyt, vzt
      real*8     r_far, fr_far, t_offset, t_spacing
      real*8     t_center, delta_t1
      real*8     t_frame
      real*8     xs1,ys1,zs1,vxs1,vys1,vzs1
      real*8     r0_ground, rs, rgnd, tmp_fd0, lookang,yaw
      real*8     thi_inc,sc_inc, ant_gain
      real*8     xs_bf,ys_bf,zs_bf,vxs_bf,vys_bf,vzs_bf
      real*8     real_index, offset
      integer    i, i_low

      real*8 ant_gain_vect(nfr), look_ang_vect(nfr)

      r_far = r0_base + dr*(ns-plen*fs)
      fr_far = fr0_base + frd*(r_far - r0_base)
      t_offset = abs(pbw/fr_far)/2

      t_spacing = 2.e-4

c Propagate to the center image
      t_start_frm = t_1st_echo + t_offset
      t_end_frm   = t_1st_echo + t_offset + t_frame
      t_center    = (t_start_frm + t_end_frm)/2
     
      delta_t1 = t_center - t_1st_echo

      call propa_simp(xs,ys,zs,vxs,vys,vzs,
     +                delta_t1,t_spacing,
     +                xs1,ys1,zs1,vxs1,vys1,vzs1)

c convert to EBF
      call eme_to_ebf(gha, t_gha, t_center, xs1,ys1,zs1,
     +                xs_bf,ys_bf,zs_bf)
      call eme_to_ebf(gha, t_gha, t_center, vxs1,vys1,vzs1,
     +                vxs_bf,vys_bf,vzs_bf)
      
c Calculate the slant range of 1st pixel
      if(icomplex.eq.0) then
         call sr2gr(r0_base,r0_ground,
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

c Generate the vector
      do i = 1, nfr
         if(icomplex.eq.0) then
            rgnd = r0_ground + (i-1)*pxl_spacing
            call gr2sr(rgnd,rs,
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
            rs = r0_base +  (i-1)*dr
         end if

c Calculate the look angle
         tmp_fd0 = fd0_base+(rs-r0_base)*fdd
         lookang = lookang1
         call gttarg1(rs, tmp_fd0, lambda, 
     +                xs_bf,ys_bf,zs_bf, vxs_bf,vys_bf,vzs_bf, 
     +                h, px, py, pz, xt, yt,zt, vxt, vyt, vzt, 
     +                lookang,yaw,frame_mode)

         lookang = acosd((rsc**2+rs**2-re_mean**2)/(2*rsc*rs))

c Calculate the antenna gain
         real_index = (lookang-look_ang_cnt-ele_ang_ref)/dang_rg+1.0
         if(int(real_index) .lt. 1) then
            ant_gain = ant_patn(1)
         else 
            if(int(real_index) .gt. 699.0) then
               ant_gain = ant_patn(700)
            else 
               i_low = int(real_index)
               offset = real_index - float(i_low)
               ant_gain = (ant_patn(i_low+1)-ant_patn(i_low))*offset+
     +              ant_patn(i_low)  
            endif
         endif

c Calculate the incidence angle in degrees
         call inc_ang(lookang,xs_bf,ys_bf,zs_bf,xt,yt,zt,thi_inc)
         sc_inc = sqrt(sind(thi_inc))

c comment out line: rdm_vec(i) = sc_inc*(rs/r0_base)**1.5/ant_gain
c and activate line: rdm_vec(i) = sc_inc*(rs/r0_base)**1.5
c for derivation of antenna pattern

c comment out line: rdm_vec(i) = sc_inc*(rs/r0_base)**1.5
c and activate line:  rdm_vec(i) = sc_inc*(rs/r0_base)**1.5/ant_gain
c for normal processing

          rdm_vec(i) = sc_inc*(rs/r0_base)**1.5/ant_gain
c        rdm_vec(i) = sc_inc*(rs/r0_base)**1.5

c        write(12,11) i,lookang,ant_gain,rdm_vec(i)

        ant_gain_vect(i) = ant_gain
        look_ang_vect(i) = lookang

      end do

 11   format(i4,7(e16.6,1x))

      
      return
      end


