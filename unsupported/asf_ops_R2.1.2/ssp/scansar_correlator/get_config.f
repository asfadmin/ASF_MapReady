c SccsId = @(#)get_config.f	2.41 3/24/98
	subroutine get_config(file_config,
     *  ire_sim,iquick,n_times_pre,data_cond,
     *  pta_refn_mode,rgc_est_mode,fdc_est_mode,prf_est_mode,
     *  ct_profile_mode,norm_look_mode,dem_ellip,proj,
     *  radio_comp_mode,image_beam_mode,burst_no_dump,stage_dump,
     *  beam_mode,
     *  res_prod,c1_pxl,c2_pxl,file_im_sirc,
     *  file_spc_raw,file_w1_rg,file_w2_rg,file_w3_rg,
     *  file_s5_rg,file_s6_rg,file_s7_rg,file_topo,
     *  file_image_dump,file_pj_image,file_pj_image_low,file_frame,
     *  file_framelet,p1_pxl,p2_pxl,file_ac_image,kids,mytask,istatus,
     *  file_raw,file_pre_to_corr,file_corr_to_post,file_mask,file_data,
     *  file_aux,file_dk,file_dk_temp,file_mask_dump,file_ceos_leader,file_image,
     *  file_pmf,file_cali,file_eph,file_rep,file_scan_results,
     *  snr,burst_good,sv_ref_time,gain_rec,blk_gain,topo_spacing,
     *  plen,nr,prod_type,file_pvs_beam,grid_ptn_flag,proc_gain,
     *  job_mode,echo_file,aux_file,rep_file,eph_file,cali_file,pmf_file,
     *  ceos_leader_file,image_file,scan_results_file,burst_loc_file,topo_file,
     *  fm_sizec1,fm_sizec2,fm_sizec1_in,fm_sizec2_in,
     *  nbeams,bm_id,rng_bm_ang,peak_bm_ang,
     *  ele_ang_eq,az_peak_coef,proc_mode,lamb_lat_n,lamb_lat_s,
     *  ps_lat,ps_lon,utm_zone,file_pvs_look,file_framelet_leader,n_single_look,
     *  avg_terrain,gha,gha_time,orbit_num,
     *  file_pre_to_ceos,file_corr_to_ceos,file_post_to_ceos,file_ceos_template,
     *  burst_start,burst_end,sim_eam_nsp,
     *  sv_x_pos,sv_y_pos,sv_z_pos,sv_x_vel,sv_y_vel,sv_z_vel,fs,file_status,proc_id, 
     *  product_id,sv_type,file_pmf_template,ter_correct,nburst_spec,sitename,
     *  platform,sensor,rev,sequence,record_id,act_id,station_id,
     *  frame_mode,sv_prec,media_id,media_type,media_location,file_pmflet,
     *  file_w1_dyn,file_w2_dyn,file_w3_dyn,file_s5_dyn,file_s6_dyn,file_s7_dyn,file_az_dyn,ant_flag,
     *  subframe_id,prf_num,file_az)



c	include 'key_const.inc'
c	include 'key_pp.inc'
c	include 'proj_const.inc'
C*****WAYNE******
       include 'ssp2_const.inc'
        character*60    platform
        character*60    sensor
        integer         rev
        integer         sequence
        character*60    record_id
        character*60    act_id
        character*60    station_id
        character*60    frame_mode
        character*60    sv_prec
        character*60    media_id
        character*60    media_type
        character*60    media_location

       character*256 file_odl
       character*256 file_config
       integer     ire_sim
       integer     iquick
       integer     n_times_pre
       integer     data_cond
       integer     pta_refn_mode
       integer     rgc_est_mode
       integer     fdc_est_mode
       integer     prf_est_mode
       integer     ct_profile_mode
       integer     norm_look_mode
       integer     dem_ellip
       integer     proj
       integer     radio_comp_mode
       integer     image_beam_mode
       integer     burst_no_dump
       integer     stage_dump
       integer*4     beam_mode
       integer     res_prod
       real*8     c1_pxl
       real*8     c2_pxl
       character*60    file_im_sirc
       character*60     file_spc_raw
       character*60     file_w1_rg
       character*60     file_w2_rg
       character*60     file_w3_rg
       character*60     file_s5_rg
       character*60     file_s6_rg
       character*60     file_s7_rg
       character*60     file_w1_dyn
       character*60     file_w2_dyn
       character*60     file_w3_dyn
       character*60     file_s5_dyn
       character*60     file_s6_dyn
       character*60     file_s7_dyn
       character*60     file_az_dyn
       character*60     file_topo
       character*60     file_az
       character*60     file_image_dump
       character*60     file_mask_dump
       character*60     file_pj_image
       character*60     file_pj_image_low
       character*60     file_frame
       character*60     file_framelet(4)
       character*60     file_framelet_leader(4)
       real*8     p1_pxl
       real*8     p2_pxl
       character*60     file_ac_image
       integer    kids,mytask
       character*60 file_raw,file_dk,file_dk_temp,file_corr_to_post,file_mask,file_data,file_aux
       character*60 file_pre_to_corr
       character*60 file_ceos_leader
       character*60 file_image
       character*60 file_pmf
       character*60 file_cali
       character*60 file_eph,file_rep,file_scan_results
       integer*4 istatus
       real*8 ta,tb,rtc
       real*8     fm_sizec1
       real*8     fm_sizec2
       real*8     fm_sizec1_in
       real*8     fm_sizec2_in
       integer*4     nbeams
       real*8     bm_id(4)
       real*8     rng_bm_ang(4)
       real*8     peak_bm_ang(4)
       real*8     ele_ang_eq(4)
C*****************************/
       real*8          snr(burst_max)
       integer*4       burst_good(burst_max)
       real*8          sv_ref_time
       real*8          gain_rec(burst_max)
       integer*4       blk_gain(iq_blk,burst_max)
       real*8           topo_spacing
       real*8          plen
       integer*4       nr
       integer*4       prod_type
       character*60     file_pvs_beam
       integer         grid_ptn_flag
       real*8          proc_gain
       integer*4       proc_id
       character*60       product_id
       character*256    echo_file,aux_file,burst_loc_file,topo_file,rep_file
       character*256    eph_file,cali_file,ceos_leader_file,image_file
       character*256    scan_results_file,pmf_file
       integer*4       job_mode,ifd
       real*8          az_peak_coef(3,8)
       integer         proc_mode
       real*8          lamb_lat_n,lamb_lat_s
C*****************************/
       integer*4       sv_type
       integer*4       sv_coord_sys
       real*8          gha_time
       real*8          gha
       real*8          sv_x_pos
       real*8          sv_y_pos
       real*8          sv_z_pos
       real*8          sv_x_vel
       real*8          sv_y_vel
       real*8          sv_z_vel
       integer*4       polar_flag
       integer*4       utm_sph
       integer*4       ceos_hdf
       real*8          ps_lat
       real*8          ps_lon
       real*8          slat
       real*8          along0
       integer*4       utm_zone
       integer*4       ter_correct
       character*60    echo_start_time
       character*60    echo_end_time
       real*8          avg_terrain
       integer*4       orbit_num
C*****************************/
       character*60    sitename
       character*60    file_pvs_look
       integer         n_single_look
       character*60 file_pre_to_ceos,file_corr_to_ceos,file_post_to_ceos
       character*60 file_ceos_template
       character*60 file_pmf_template
       character*256 file_status
       character*60     file_pmflet(4)
C*****WAYNE******
       integer*4       sim_eam_nsp(4),burst_start,burst_end
       real*8          fs
       integer         nburst_spec
       integer         ant_flag
       integer         subframe_id
       integer         prf_num
 
      

        write(6,*)file_config
        call load_config(file_config,
     *                   ire_sim,file_odl,file_status,burst_start,burst_end,
     *                   sim_eam_nsp,ant_flag)

        write(6,*)ire_sim
        write(6,*)file_config
        write(6,*)file_odl
        write(6,*)file_status
        write(6,*)burst_start
        write(6,*)burst_end
        write(6,*)sim_eam_nsp(1),sim_eam_nsp(2),sim_eam_nsp(3),sim_eam_nsp(4)
    

        if (mytask.eq.0) idummy=printflog(3,'Loading processing parameters&')
        call prb_keypp(file_odl,
     *      lamb_lat_n,lamb_lat_s,
     *      echo_file,aux_file,rep_file,eph_file,cali_file,ceos_leader_file,
     *      image_file,scan_results_file,burst_loc_file,pmf_file,
     *      proc_id,product_id,
C****
     *      c1_pxl,c2_pxl,p1_pxl,p2_pxl,
     *      proj,radio_comp_mode,prod_type,res_prod,beam_mode,
     *      sv_type,sv_coord_sys,gha_time,gha,sv_ref_time,
     *      sv_x_pos,sv_y_pos,sv_z_pos,sv_x_vel,sv_y_vel,sv_z_vel,
     *      polar_flag,utm_sph,ceos_hdf,ps_lat,ps_lon,slat,along0,
     *      utm_zone,ter_correct,dem_ellip,
     *      echo_start_time,echo_end_time,proc_gain,avg_terrain,orbit_num,
     *      sitename,istatus,
     *      platform,sensor,rev,sequence,record_id,act_id,station_id,
     *      frame_mode,sv_prec,media_id,media_type,media_location,subframe_id)

C*******************************************************************************
        rgc_est_mode=0
c qdn 6/16/97
        rgc_est_mode=1
        fdc_est_mode=1
        prf_est_mode=1
        prf_num= 9999
c       prf_num= 0
        pta_refn_mode=1 !    RUN PTA WITH PARAMETERS
        proc_mode = 1
        ct_profile_mode = 0
C*******************************************************************************
c 11/12/96: take out the optione to overide the antenna flag
c       radio_comp_mode=1

        iquick = 0
        dem_ellip=ellip
c       ire_sim=1               ! simulation
        data_cond=0
        norm_look_mode=1
C*******************************************************************************
	n_times_pre = 6
	image_beam_mode = 0  	!(0=all beams,n=beam n only)
	burst_no_dump = 1	!any burst number within start & end
	stage_dump = 2		!(2=after rdm,3=after geo_r,4=after spa_avg)
	if(iquick.eq.1) then
	   c1_pxl = space_400
           c2_pxl = space_400
	end if
        n_single_look=4
        plen=42.0d-06
        fs  =12.967d6
        nr=nint(plen*fs)
        do j=1, 1472
         burst_good(j)=1
        enddo
        grid_ptn_flag=0
c       snr()=
c       gain_rec()=
c       blk_gain( , )=
        topo_spacing= .083333d0
        nburst_spec=60



        if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return

        write(6,*)'ire_sim= ',ire_sim
        write(6,*)'iquick = ',iquick
        write(6,*)'data_cond= ',data_cond
        write(6,*)'pta_refn_mode= ',pta_refn_mode
        write(6,*)'rgc_est_mode = ',rgc_est_mode
        write(6,*)'fdc_est_mode = ',fdc_est_mode
        write(6,*)'prf_est_mode = ',prf_est_mode 
        write(6,*)'ct_profile_mode = ',ct_profile_mode
        write(6,*)'norm_look_mode = ',norm_look_mode
        write(6,*)'dem_ellip = ',dem_ellip
        write(6,*)'proj = ',proj
        write(6,*)'prod_type = ',prod_type
        write(6,*)'radio_comp_mode = ',radio_comp_mode
        write(6,*)'beam_mode = ',beam_mode
        write(6,*)'res_prod = ',res_prod
        write(6,*)'c1_pxl = ',c1_pxl
        write(6,*)'c2_pxl = ',c2_pxl
        write(6,*)'p1_pxl = ',p1_pxl
        write(6,*)'p2_pxl = ',p2_pxl
        write(6,*)'proc_gain = ',proc_gain
        write(6,*)'proc_id = ',proc_id
        write(6,*)'echo_file = ',echo_file
        write(6,*)'aux_file = ',aux_file
        write(6,*)'burst_loc_file = ',burst_loc_file
C*********************
C*********************





C*******************************************************
        topo_file        ='ssp1-fddi:/u/u3/jwtun/ssp1_data/etopo5.dat'
C*******************************************************
c       file_w1_rg= '/home/tmpdisk/ANT/w1_si.dat'
c       file_w2_rg= '/home/tmpdisk/ANT/w2_si.dat'
c       file_w3_rg= '/home/tmpdisk/ANT/w3_si.dat'
c       file_s5_rg= '/home/tmpdisk/ANT/s5_si.dat'
c       file_s6_rg= '/home/tmpdisk/ANT/s6_si.dat'
c       file_s7_rg= '/home/tmpdisk/ANT/s7_si.dat'
        file_w1_rg= '/home/tmpdisk/ANT/w1_new.dat'
        file_w2_rg= '/home/tmpdisk/ANT/w2_new.dat'
        file_w3_rg= '/home/tmpdisk/ANT/w3_si.dat'
        file_s5_rg= '/home/tmpdisk/ANT/s5_new.dat'
        file_s6_rg= '/home/tmpdisk/ANT/s6_new.dat'
        file_s7_rg= '/home/tmpdisk/ANT/s7_si.dat'
        file_topo = '/home/tmpdisk/ANT/etopo5.dat'
        file_az   = '/home/tmpdisk/ANT/daz_pk.new'
        file_w1_dyn= '/home/tmpdisk/w1_rg.dyn'
        file_w2_dyn= '/home/tmpdisk/w2_rg.dyn'
        file_w3_dyn= '/home/tmpdisk/w3_rg.dyn'
        file_s5_dyn= '/home/tmpdisk/s5_rg.dyn'
        file_s6_dyn= '/home/tmpdisk/s6_rg.dyn'
        file_s7_dyn= '/home/tmpdisk/s7_rg.dyn'
        file_az_dyn= '/home/tmpdisk/daz_pk.dyn'
        file_ceos_template='/home/tmpdisk/ANT/ceos.template'
        file_pmf_template='/home/tmpdisk/ANT/pmf.template'
c
        file_aux  = '/home/tmpdisk/aux.dat'
        file_raw      ='/home/tmpdisk/echo.dat'
        file_dk       ='/home/tmpdisk/DK_FILE'
        file_dk_temp       ='/home/tmpdisk/DK_TEMP'
        file_pre_to_corr   = '/home/tmpdisk/pre_to_corr.dat'
        file_corr_to_post   = '/home/tmpdisk/corr_to_post.dat'
        file_mask     ='/home/tmpdisk/temp_mask'
        file_data     ='/home/tmpdisk/temp_data'
        file_pvs_beam = '/home/tmpdisk/pvs.dat'
        file_pre_to_ceos ='/home/tmpdisk/pre_to_ceos.dat'
        file_corr_to_ceos='/home/tmpdisk/corr_to_ceos.dat'
        file_post_to_ceos='/home/tmpdisk/post_to_ceos.dat'
c
c ceos related files
c**************************************************
        file_pvs_look = '/home/tmpdisk/pvs_look.dat'             ! INPUT FOR JEFF
        file_cali        ='/home/tmpdisk/calib.dat'              ! INPUT FOR JEFF
        file_scan_results='/home/tmpdisk/scan.dat'               ! INPUT FOR JEFF
c***
        file_ceos_leader    ='/home/tmpdisk/ceos_leader.dat'        ! OUTPUT  FOR JEFF
        file_image          ='/home/tmpdisk/image.dat'              ! OUTPUT  FOR JEFF
        file_pmf            ='/home/tmpdisk/pmf.dat'                ! OUTPUT FOR JEFF
        do i=1, n_single_look
         file_framelet(i)       =file_image
         file_pmflet(i)         =file_pmf
         file_framelet_leader(i)=file_ceos_leader
         ifd=str_cat(file_framelet(i),i)                             ! OUTPUT FOR JEFF
         ifd=str_cat(file_pmflet(i),i)                             ! OUTPUT FOR JEFF
         ifd=str_cat(file_framelet_leader(i),i)                      ! OUTPUT FOR JEFF
        enddo
c**************************************************
        file_eph         ='/home/tmpdisk/eph.dat'
        file_rep         ='/home/tmpdisk/rep.dat'
c
        file_image_dump = '/home/tmpdisk/image_dump.dat'
        file_mask_dump  = '/home/tmpdisk/mask_dump.dat'
        file_ac_image = '/home/tmpdisk/ac_image.dat'
        file_pj_image = '/home/tmpdisk/pj_image.dat'
        file_pj_image_low = '/home/tmpdisk/pj_image_low.dat'

        write(6,*)'Id is', proc_id
        ifd=str_cat(file_w1_dyn         ,proc_id);
        ifd=str_cat(file_w2_dyn         ,proc_id);
        ifd=str_cat(file_w3_dyn         ,proc_id);
        ifd=str_cat(file_s5_dyn         ,proc_id);
        ifd=str_cat(file_s6_dyn         ,proc_id);
        ifd=str_cat(file_s7_dyn         ,proc_id);
        ifd=str_cat(file_az_dyn         ,proc_id);
c
        ifd=str_cat(file_aux         ,proc_id);
        ifd=str_cat(file_raw         ,proc_id);
        ifd=str_cat(file_dk          ,proc_id);
        ifd=str_cat(file_dk_temp          ,proc_id);
        ifd=str_cat(file_cali,proc_id);
        ifd=str_cat(file_eph,proc_id);
        ifd=str_cat(file_rep,proc_id);
        ifd=str_cat(file_scan_results,proc_id);
c
c
        ifd=str_cat(file_pre_to_corr      ,proc_id);
        ifd=str_cat(file_pre_to_ceos      ,proc_id);
c
c
        ifd=str_cat(file_corr_to_ceos      ,proc_id);
        ifd=str_cat(file_corr_to_post      ,proc_id);
        ifd=str_cat(file_mask        ,proc_id);
        ifd=str_cat(file_data        ,proc_id);
        ifd=str_cat(file_pvs_look,proc_id);
c
c
        ifd=str_cat(file_pj_image    ,proc_id);
        ifd=str_cat(file_pj_image_low,proc_id);
        ifd=str_cat(file_post_to_ceos      ,proc_id);
c
c
        ifd=str_cat(file_ceos_leader,proc_id);
        ifd=str_cat(file_image,proc_id);
        ifd=str_cat(file_pmf,proc_id);
        do i=1, n_single_look
         ifd=str_cat(file_framelet(i),proc_id)                             ! OUTPUT FOR JEFF
         ifd=str_cat(file_pmflet(i),proc_id)                             ! OUTPUT FOR JEFF
         ifd=str_cat(file_framelet_leader(i),proc_id)                      ! OUTPUT FOR JEFF
        enddo
c
c not use 
        ifd=str_cat(file_image_dump  ,proc_id);
        ifd=str_cat(file_mask_dump   ,proc_id);
        ifd=str_cat(file_ac_image    ,proc_id);
c not use 


        if (job_mode.eq.0) then
        endif

        if (job_mode.eq.1) then
         ifd           = ccreate(file_pre_to_corr)
         if (ifd.eq.-1)  istatus=ierr_2
         ifd           = ccreate(file_pre_to_ceos)
         if (ifd.eq.-1)  istatus=ierr_2
        endif

        if (job_mode.eq.2) then
         ifd           = ccreate(file_corr_to_post)
         if (ifd.eq.-1)  istatus=ierr_2
         ifd            =ccreate(file_mask)
         if (ifd.eq.-1)  istatus=ierr_2
         ifd            =ccreate(file_data)
         if (ifd.eq.-1)  istatus=ierr_2
         ifd            =ccreate(file_pvs_look)
         if (ifd.eq.-1)  istatus=ierr_2
         ifd            =ccreate(file_corr_to_ceos)
         if (ifd.eq.-1)  istatus=ierr_2
        endif

        if (job_mode.eq.3) then
         ifd            =ccreate(file_post_to_ceos)
         if (ifd.eq.-1)  istatus=ierr_2
         if (mytask.eq.0) then
          ifd= ccreate(file_pj_image)
          if (ifd.eq.-1)  istatus=ierr_2
          ifd= ccreate(file_pj_image_low)
          if (ifd.eq.-1)  istatus=ierr_2
           ifd= ccreate(file_image_dump)
           if (ifd.eq.-1)  istatus=ierr_2
           ifd= ccreate(file_mask_dump)
           if (ifd.eq.-1)  istatus=ierr_2
c         ifd= ccreate(file_ac_image)
c         if (ifd.eq.-1)  istatus=ierr_2
         endif
        endif

        if (job_mode.eq.4) then
         if (mytask.eq.0) then
          ifd= ccreate(file_ceos_leader)
          if (ifd.eq.-1)  istatus=ierr_2
          ifd= ccreate(file_image)
          if (ifd.eq.-1)  istatus=ierr_2
          ifd= ccreate(file_pmf)
          if (ifd.eq.-1)  istatus=ierr_2
          do i=1, n_single_look
           ifd= ccreate(file_framelet(i))
           if (ifd.eq.-1)  istatus=ierr_2
           ifd= ccreate(file_pmflet(i))
           if (ifd.eq.-1)  istatus=ierr_2
           ifd= ccreate(file_framelet_leader(i))
           if (ifd.eq.-1)  istatus=ierr_2
          enddo
         endif
        endif

        if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return

c* not used for now*
c       file_frame = '/data02/jwtun/frame.dat'
c       file_spc_raw = '/data08/jwtun/spc_raw.dat'
c       file_im_sirc = '/user/jin/radarsat/sub/im_sirc.dat'
c       ifd= ccreate(file_frame)
c       ifd= ccreate(file_framelet)
c* not used for now*

        call set_beams(
     *  beam_mode,ire_sim,
     *  fm_sizec1,fm_sizec2,fm_sizec1_in,fm_sizec2_in,
     *  nbeams,bm_id,rng_bm_ang,peak_bm_ang,
     *  ele_ang_eq,az_peak_coef       )



        write(6,*)'END OF'


        return
        end
        subroutine load_config(file_config,
     *                   ire_sim,file_odl,file_status,burst_start,burst_end,
     *                   sim_eam_nsp,ant_flag)
        character*256 file_config
        character*256 file_status
        character*256 file_odl
        integer      ire_sim,burst_start,burst_end
        integer*4 sim_eam_nsp(4)
        integer isuccess,f_load_config
        character*256   source_dir
        integer*4  ant_flag

        isuccess=f_load_config(file_config,source_dir,ire_sim,file_odl,file_status,burst_start,burst_end,sim_eam_nsp,ant_flag)
        


        return
        end
	subroutine set_beams(
     *  beam_mode,ire_sim,  
     *  fm_sizec1,fm_sizec2,fm_sizec1_in,fm_sizec2_in,
     *  nbeams,bm_id,rng_bm_ang,peak_bm_ang,
     *  ele_ang_eq,az_peak_coef       )
	implicit real*8 (a-h, o-z)
c	include 'key_const.inc'
c	include 'key_pp.inc'
C*****WAYNE******
       include 'ssp2_const.inc'
       integer*4     beam_mode
       integer     ire_sim
       real*8     fm_sizec1
       real*8     fm_sizec2
       real*8     fm_sizec1_in
       real*8     fm_sizec2_in
       integer*4     nbeams
       real*8     bm_id(4)
       real*8     rng_bm_ang(4)
       real*8     peak_bm_ang(4)
       real*8     ele_ang_eq(4)
       real*8          az_peak_coef(3,8)
C*****WAYNE******
	fm_sizec1 = 550000.d0
	fm_sizec2 = 550000.d0
	fm_sizec1_in = 550000.d0
	fm_sizec2_in = 550000.d0
	if(beam_mode.eq.1) then
	    nbeams = 4
	    bm_id(1) = 1
	    bm_id(2) = 2
	    bm_id(3) = 3
	    bm_id(4) = 7
	    peak_bm_ang(1) = -7.17d0
	    peak_bm_ang(2) = 0.62d0
	    peak_bm_ang(3) = 6.77d0
	    peak_bm_ang(4) = 10.28d0
	    rng_bm_ang(1) = 11.9d0
	    rng_bm_ang(2) = 7.6d0
	    rng_bm_ang(3) = 5.57d0
	    rng_bm_ang(4) = 4.5d0
	    ele_ang_eq(1) = 4.97d0
	    ele_ang_eq(2) = 3.56d0
	    ele_ang_eq(3) = 2.01d0
	else
	if(beam_mode.eq.2) then
	    nbeams = 4
	    bm_id(1) = 1
	    bm_id(2) = 2
	    bm_id(3) = 5
	    bm_id(4) = 6
	    peak_bm_ang(1) = -7.17d0
	    peak_bm_ang(2) = 0.62d0
	    peak_bm_ang(3) = 4.41d0 
	    peak_bm_ang(4) = 8.09d0
	    rng_bm_ang(1) = 11.9d0
	    rng_bm_ang(2) = 7.6d0
	    rng_bm_ang(3) = 5.57d0
	    rng_bm_ang(4) = 4.5d0
	    ele_ang_eq(1) = 4.97d0
	    ele_ang_eq(2) = 1.44d0
	    ele_ang_eq(3) = 1.96d0
	else
	fm_sizec1 = 334000.d0
	fm_sizec2 = 334000.d0
	fm_sizec1_in = 330000.d0
	fm_sizec2_in = 330000.d0
	if(beam_mode.eq.3) then
	    nbeams = 2
	    bm_id(1) = 1
	    bm_id(2) = 2
	    peak_bm_ang(1) = -7.17d0
	    peak_bm_ang(2) = 0.62d0
	    rng_bm_ang(1) = 11.9d0
	    rng_bm_ang(2) = 7.6d0
	    ele_ang_eq(1) = 4.97d0
	else
	    nbeams = 3
	    bm_id(1) = 2
	    bm_id(2) = 5
	    bm_id(3) = 6
	    peak_bm_ang(1) = 0.62d0
	    peak_bm_ang(2) = 4.41d0 
	    peak_bm_ang(3) = 8.09d0
	    rng_bm_ang(1) = 7.6d0
	    rng_bm_ang(2) = 5.57d0
	    rng_bm_ang(3) = 4.5d0
	    ele_ang_eq(1) = 1.44d0
	    ele_ang_eq(2) = 1.96d0
	end if
	end if
	end if

	if(ire_sim.eq.0) then		!sim mode has beam_mode .eq. 1 only 
	    rng_bm_ang(1) = 7.9587d0
	    rng_bm_ang(2) = 6.8211d0
	    rng_bm_ang(3) = 5.6966d0
	    rng_bm_ang(4) = 4.6899d0
	    bm_id(4) = 4
	end if

        call get_azpk_coef(ire_sim,az_peak_coef,beam_mode)


	return
	end
        subroutine get_azpk_coef(ire_sim,az_peak_coef,beam_mode)
        integer ire_sim,beam_mode
        real*8          az_peak_coef(3,8)

        if(beam_mode.eq.1) then
c       az_peak_coef(1,1) =     1.110625932394095
c       az_peak_coef(2,1) = -5.9098772596725073E-02
c       az_peak_coef(3,1) =  7.1488420751549214E-04

c       az_peak_coef(1,2) =    1.498055992299268
c       az_peak_coef(2,2) = -8.4328165328444338E-02
c       az_peak_coef(3,2) =  1.1263873780059358E-03

c       az_peak_coef(1,3) =   1.519337867375915
c       az_peak_coef(2,3) = -8.2508555411594714E-02
c       az_peak_coef(3,3) =  1.0549651027540628E-03

c       az_peak_coef(1,7) =   2.352373974919531
c       az_peak_coef(2,7) = -0.1215890253353615
c       az_peak_coef(3,7) =  1.5093518109408665E-03
c new fd generated
c       az_peak_coef(1,1) = 1.12509043419636190
c       az_peak_coef(2,1) = -0.604383726881495900E-01
c       az_peak_coef(3,1) = 0.737002147881062487E-03

c       az_peak_coef(1,2) = 1.47054871397179854
c       az_peak_coef(2,2) = -0.830985634400631112E-01
c       az_peak_coef(3,2) = 0.112032446494208246E-02

c       az_peak_coef(1,3) = 1.46666061894565369
c       az_peak_coef(2,3) = -0.813990421532893582E-01
c       az_peak_coef(3,3) = 0.105415593962958462E-02

c       az_peak_coef(1,7) = 2.72575698434085201
c       az_peak_coef(2,7) = -0.141234572384217466
c       az_peak_coef(3,7) = 0.176666887192716122E-02



c Coefficient result from before oct2 
        az_peak_coef(1,1) = 1.12352127688321990
        az_peak_coef(2,1) = -0.605098770185644383E-01
        az_peak_coef(3,1) = 0.737831805191321269E-03

        az_peak_coef(1,2) = 1.47023264980150015
        az_peak_coef(2,2) = -0.832346396122188303E-01
        az_peak_coef(3,2) = 0.112174382496341694E-02

        az_peak_coef(1,3) = 1.46867207858438054
        az_peak_coef(2,3) = -0.816395644686400135E-01
        az_peak_coef(3,3) = 0.105687178883414126E-02

        az_peak_coef(1,7) = 2.69497430662714654
        az_peak_coef(2,7) = -0.139846836680772468
        az_peak_coef(3,7) = 0.174910859319544099E-02

        end if


        if(beam_mode.ne.1) then
c       az_peak_coef(1,1) =   1.224588947479825
c       az_peak_coef(2,1) = -6.0051269290348187E-02
c       az_peak_coef(3,1) =  6.6361598066817589E-04

c       az_peak_coef(1,2) = 1.525653626455870
c       az_peak_coef(2,2) = -6.1982792601727954E-02
c       az_peak_coef(3,2) = 7.2394978388730944E-04

c       az_peak_coef(1,5) =   1.937989627392864
c       az_peak_coef(2,5) = -8.5824535815919908E-02
c       az_peak_coef(3,5) = 1.0678298131871254E-03

c       az_peak_coef(1,6) =   2.711963131218099
c       az_peak_coef(2,6) =  -0.1246790811254286
c       az_peak_coef(3,6) =  1.5512274244160367E-03
c new fd generated
c       az_peak_coef(1,1) = 0.782668739044960926 
c       az_peak_coef(2,1) = -0.429509220003629483E-01
c       az_peak_coef(3,1) = 0.717917277891891780E-03

c Shift one PRF forward 
c       az_peak_coef(1,2) = 1.33825571801304499 - 0.2871522376
c       az_peak_coef(2,2) = -0.615766820953015728E-01
c       az_peak_coef(3,2) = 0.103721801956697338E-02

c Shift one PRF backward
c       az_peak_coef(1,5) = 1.57357715870715031 - 0.2789376
c       az_peak_coef(2,5) = -0.735467506801948528E-01
c       az_peak_coef(3,5) = 0.117525053841122931E-02

c       az_peak_coef(1,6) = -0.239684996287104379
c       az_peak_coef(2,6) = 0.961934552544435606E-02
c       az_peak_coef(3,6) = 0.465707860676438205E-04



c Coefficient result from setp 24
c       az_peak_coef(1,1) = 0.780951861517609980 
c       az_peak_coef(2,1) = -0.426185187054352321E-01
c       az_peak_coef(3,1) = 0.705454457010375939E-03

c Shift one PRF forward 
c       az_peak_coef(1,2) = 0.974348007221512713
c       az_peak_coef(2,2) = -0.560028842550175446E-01
c       az_peak_coef(3,2) = 0.938692946014375428E-03

c Shift one PRF backward
c       az_peak_coef(1,5) = 1.34725855936480921
c       az_peak_coef(2,5) = -0.762907101539281457E-01
c       az_peak_coef(3,5) = 0.121076357536275148E-02

c       az_peak_coef(1,6) = -0.216025246239349961
c       az_peak_coef(2,6) = 0.862524456859467548E-02
c       az_peak_coef(3,6) = 0.560120672591322017E-04

c qdn 5/21/97
c qdn Coeffient result from apr19 afternoon 2nd(input from apr19 afternoon)
c qdn 6/16/97 put it in
c       az_peak_coef(1,1) = 0.704052649602235570
c       az_peak_coef(2,1) = -0.355279131245634164E-01
c       az_peak_coef(3,1) = 0.545579013885900488E-03

c       az_peak_coef(1,2) = 1.05053144616780592
c       az_peak_coef(2,2) = -0.608571642228181789E-01
c       az_peak_coef(3,2) = 0.101609876270409324E-02

c       az_peak_coef(1,5) = 0.829557092122199635
c       az_peak_coef(2,5) = -0.462529487607825363E-01
c       az_peak_coef(3,5) = 0.775732894899287709E-03

c       az_peak_coef(1,6) = 1.23732651893409806
c       az_peak_coef(2,6) = -0.680989912227216165E-01
c       az_peak_coef(3,6) = 0.106781138381792438E-02
c qdn 6/18/97 modified again by adding -0.6 for WD2,ST5 and ST6

c       az_peak_coef(1,1) = 0.704052649602235570
c       az_peak_coef(2,1) = -0.355279131245634164E-01
c       az_peak_coef(3,1) = 0.545579013885900488E-03

c       az_peak_coef(1,2) = 1.04453144616780592
c       az_peak_coef(2,2) = -0.608571642228181789E-01
c       az_peak_coef(3,2) = 0.101609876270409324E-02

c       az_peak_coef(1,5) = .8235570921221996355
c       az_peak_coef(2,5) = -0.462529487607825363E-01
c       az_peak_coef(3,5) = 0.775732894899287709E-03

c       az_peak_coef(1,6) = 1.23132651893409806
c       az_peak_coef(2,6) = -0.680989912227216165E-01
c       az_peak_coef(3,6) = 0.106781138381792438E-02

c qdn 7/19/97 using the previous value
c Coefficient result from setp 24
c       az_peak_coef(1,1) = 0.780951861517609980 
c       az_peak_coef(2,1) = -0.426185187054352321E-01
c       az_peak_coef(3,1) = 0.705454457010375939E-03

c Shift one PRF forward 
c       az_peak_coef(1,2) = 0.974348007221512713
c       az_peak_coef(2,2) = -0.560028842550175446E-01
c       az_peak_coef(3,2) = 0.938692946014375428E-03

c Shift one PRF backward
c       az_peak_coef(1,5) = 1.34725855936480921
c       az_peak_coef(2,5) = -0.762907101539281457E-01
c       az_peak_coef(3,5) = 0.121076357536275148E-02

c       az_peak_coef(1,6) = -0.216025246239349961
c       az_peak_coef(2,6) = 0.862524456859467548E-02
c       az_peak_coef(3,6) = 0.560120672591322017E-04

c qdn 7/25/97 friday afternoon using the previous value
c Coefficient result from setp 24
c       az_peak_coef(1,1) = 0.780951861517609980  - 0.563533/2.
c       az_peak_coef(2,1) = -0.426185187054352321E-01
c       az_peak_coef(3,1) = 0.705454457010375939E-03

c Shift one PRF forward 
c       az_peak_coef(1,2) = 0.974348007221512713  - 0.5810126/2.
c       az_peak_coef(2,2) = -0.560028842550175446E-01
c       az_peak_coef(3,2) = 0.938692946014375428E-03

c Shift one PRF backward
c       az_peak_coef(1,5) = 1.34725855936480921  - 0.563533/2.
c       az_peak_coef(2,5) = -0.762907101539281457E-01
c       az_peak_coef(3,5) = 0.121076357536275148E-02

c       az_peak_coef(1,6) = -0.216025246239349961   - 0.5828203/2.
c       az_peak_coef(2,6) = 0.862524456859467548E-02
c       az_peak_coef(3,6) = 0.560120672591322017E-04

c qdn 7/26/97 friday afternoon using the previous value
c Coefficient result from setp 24
        az_peak_coef(1,1) = 0.538762020548654630                
        az_peak_coef(2,1) = -0.454497658359925283E-01
        az_peak_coef(3,1) = 0.748941846861266589E-03

        az_peak_coef(1,2) = 0.748647433254606809                  
        az_peak_coef(2,2) = -0.596333363421478954E-01
        az_peak_coef(3,2) = 0.991024333329837545E-03

        az_peak_coef(1,5) = 1.07457085983965572                   
        az_peak_coef(2,5) = -0.770786926491286023E-01
        az_peak_coef(3,5) = 0.122018397624598283E-02

        az_peak_coef(1,6) = -0.287581089877535667                  
        az_peak_coef(2,6) = -0.259671383262445772E-02
        az_peak_coef(3,6) = 0.202626733350404404E-03

c qdn 7/26/97 saturday afternoon using the previous value
c Coefficient result from setp 24
        az_peak_coef(1,1) = 0.531686285754587984                
        az_peak_coef(2,1) = -0.451664874442228512E-01
        az_peak_coef(3,1) = 0.748103777299052560E-03

        az_peak_coef(1,2) = 0.740851249655210098                  
        az_peak_coef(2,2) = -0.593299782324917258E-01
        az_peak_coef(3,2) = 0.990230840337962340E-03

        az_peak_coef(1,5) = 1.08203288046245283                   
        az_peak_coef(2,5) = -0.776501467903000786E-01
        az_peak_coef(3,5) = 0.123198008580275502E-02

        az_peak_coef(1,6) = 0.811320630354678718                   
        az_peak_coef(2,6) = -0.613667780953181738E-01
        az_peak_coef(3,6) = 0.988020764747090351E-03

c qdn 7/23/97 using the previous value
c Coefficient result from setp 24(1st iteration after SEAICE)
c       az_peak_coef(1,1) = 0.676090807991215570 
c       az_peak_coef(2,1) = -0.341464535540484532E-01
c       az_peak_coef(3,1) = 0.543446118391552584E-03

c       az_peak_coef(1,2) = 1.04689781762995260 
c       az_peak_coef(2,2) = -0.611305357821201392E-01
c       az_peak_coef(3,2) = 0.102865296728321069E-02

c       az_peak_coef(1,5) = 0.674345723178281120
c       az_peak_coef(2,5) = -0.366018120182232606E-01
c       az_peak_coef(3,5) = 0.634776358977851224E-03

c       az_peak_coef(1,6) = 1.29995790429261993  
c       az_peak_coef(2,6) = -0.710403092210348808E-01
c       az_peak_coef(3,6) = 0.110127812564904414E-02



c qdn 7/23/97 using the previous value
c Coefficient result from setp 24(2nd iteration after SEAICE)
c       az_peak_coef(1,1) = 0.692462507687740758 
c       az_peak_coef(2,1) = -0.366116018784455460E-01
c       az_peak_coef(3,1) = 0.626091272456576350E-03

c       az_peak_coef(1,2) = 0.971668708655124558
c       az_peak_coef(2,2) = -0.563339065878830508E-01
c       az_peak_coef(3,2) = 0.955270243507563648E-03

c       az_peak_coef(1,5) = 1.02015419245965733 
c       az_peak_coef(2,5) = -0.563507092682355715E-01
c       az_peak_coef(3,5) = 0.926786424544398090E-03

c       az_peak_coef(1,6) = 1.05157913928106050  
c       az_peak_coef(2,6) = -0.575085101953066932E-01
c       az_peak_coef(3,6) = 0.919492089521093824E-03



c qdn 7/25/97 using the previous value
c Coefficient result from setp 24(2nd iteration after SEAICE)
c BADDDDDDD
c       az_peak_coef(1,1) = 0.924748540094838201 
c       az_peak_coef(2,1) = -0.512831588657953197E-01
c       az_peak_coef(3,1) = 0.763501333548094723E-03

c       az_peak_coef(1,2) =  1.38449853052326377
c       az_peak_coef(2,2) =  -0.810214296738686751E-01
c       az_peak_coef(3,2) =  0.130174884527970401E-02

c       az_peak_coef(1,5) = 1.94204385016798486 
c       az_peak_coef(2,5) = -0.112681715526268128     
c       az_peak_coef(3,5) = 0.171093849432735808E-02

c       az_peak_coef(1,6) = 1.49922324628551440  
c       az_peak_coef(2,6) = -0.917753624743128382E-01
c       az_peak_coef(3,6) = 0.121377247303174085E-02


c qdn 7/25/97 using the previous value
c Coefficient result from setp 24(1st iteration after SEAICE)
c BADDD
c       az_peak_coef(1,1) = 1.04392938019212855  
c       az_peak_coef(2,1) = -0.623081383849628176E-01
c       az_peak_coef(3,1) = 0.100341398057357772E-02

c       az_peak_coef(1,2) =  1.28969386528842311
c       az_peak_coef(2,2) =  -0.747773482602274997E-01
c       az_peak_coef(3,2) =  0.120435040951864824E-02

c       az_peak_coef(1,5) = 1.60489520189166623 
c       az_peak_coef(2,5) = -0.945707817652918642E-01 
c       az_peak_coef(3,5) = 0.146684452670407459E-02

c       az_peak_coef(1,6) = 1.20195393779464954  
c       az_peak_coef(2,6) = -0.682262129861584826E-01
c       az_peak_coef(3,6) = 0.108990791330297056E-02



        end if






        if(ire_sim.eq.0) then
        do k1 = 1, 3
        do k2 = 1, 8
        az_peak_coef(k1,k2) = 0.
        end do
        end do
        end if

        return
        end

        subroutine elapsed_time(in_time)
        real*8  in_time
         in_time=rtc()
        return
        end
