c SccsId = @(#)ZERO.f	2.41 3/24/98
c	get_config.f and jin_get_config.f

        subroutine pre_processor(file_config,istatus,kids,mytask)
C*****WAYNE******
       include 'ssp2_const.inc'
       include 'ssp2_dim.inc'
       integer         grid_ptn_flag
       real*8          proc_gain
       real*8          snr(burst_max)
       integer*4       burst_good(1600)
       real*8          sv_ref_time
       real*8          gain_rec(burst_max)
       integer*4       dk_burst(burst_max)
       integer*4       blk_gain(iq_blk,burst_max)
       real*8     topo_spacing
       real*8          plen
       integer*4       nr
       integer         istatus
C*****WAYNE******
       character*60     file_ac_image
       character*60     file_topo
       integer     ire_sim
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
       real*8     rng_bm_ang(4)
       real*8     c2_pxl
       real*8     fm_sizec2
       real*8     c1_pxl
       complex     tbl(-128:127)
       real*8     chirp_bw
       real*8     fs
       real*8     dtau
       real*8     chirp_rate
       integer*4     nfft_az
       integer*4     max_look
       integer*4     nbeams
       integer     iquick
       real*8     x_sc(burst_max)
       real*8     y_sc(burst_max)
       real*8     z_sc(burst_max)
       real*8     v_x_sc(burst_max)
       real*8     v_y_sc(burst_max)
       real*8     v_z_sc(burst_max)
       integer*2     topo(ns_ew,ns_ns)
       real*8     dlat_topo
       real*8     dlon_topo
       real     ant_patn_r(ns_ant_rg,7)
       real     ant_patn_az(ns_ant_az)
       integer*4     v_filler
       integer*4     v_noise
       integer*4     sam_ac2
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
       integer*4     burst_total
       integer     res_prod
       character*60     file_im_sirc
       character*60     file_spc_raw
       character*60     file_image_dump
       character*60     file_mask_dump
       character*60     file_pj_image
       character*60     file_pj_image_low
       character*60     file_frame
       character*60     file_framelet(4)
       real*8     fm_sizec1
       real*8     fm_sizec1_in
       real*8     fm_sizec2_in
       real*8     bm_id(4)
       real*8     peak_bm_ang(4)
       real*8     ele_ang_eq(4)
       integer*4     beam_no(burst_max)
       integer*4     beam_id(burst_max)
       real*8     t_burst(burst_max)
       integer*4     burst_cnt(burst_max)
       real*8     prf(burst_max)
       integer*4     ns(burst_max)
       integer*4     np(burst_max)
       real*8     look_ang(burst_max)
       integer*4     np_air(burst_max)
       real*8     prf_s(burst_max)
       real*8     df(burst_max)
       real*8     dr(burst_max)
       real*8     r_1st(burst_max)
       real*8     roll_o(burst_max)
       real*8     yaw_o(burst_max)
       real*8     roll(burst_max)
       real*8     yaw(burst_max)
       real*8     pitch_o(burst_max)
       real*8     pitch(burst_max)
       real*8     alpha1
       real*8     alpha2
       real*8     alpha3
       integer     proc_mode
       integer     np_proc
       real*8     i_ang_bip(burst_max)
       real*8     r_bip(burst_max)
       real*8     fr_bip(burst_max)
       real*8     coef_fd(4,burst_max)
       real*8     coef_fr(4,burst_max)
       real*8     coef_lk_ang(4,burst_max)
       real*8     fd_near(burst_max)
       real*8     fr_near(burst_max)
       real*8     fd_far(burst_max)
       real*8     fr_far(burst_max)
       real*8     r_cntr(burst_max)
       real*8     fd_cntr(burst_max)
       real*8     fr_cntr(burst_max)
       real*8     pbw_near(burst_max)
       real*8     fd_low_near(burst_max)
       real*8     fd_high_near(burst_max)
       real*8     pbw_far(burst_max)
       real*8     fd_low_far(burst_max)
       real*8     fd_high_far(burst_max)
       real*8     pbw_cntr(burst_max)
       real*8     fd_low_cntr(burst_max)
       real*8     fd_high_cntr(burst_max)
       real*8     c1_flm(2,2,burst_max)
       real*8     c2_flm(2,2,burst_max)
       real*8     v_sc_mag(burst_max)
       real*8     c1_cell(burst_max)
       real*8     c2_cell(burst_max)
       real*8     c1size_blk(burst_max)
       real*8     c2size_blk(burst_max)
       integer*4     n_c1_blk(burst_max)
       integer*4     n_c2_blk(burst_max)
       real*8     c1_g(3,3,n_az,n_rg,burst_max)
       real*8     c2_g(3,3,n_az,n_rg,burst_max)
       real*8     r_g(3,3,n_az,n_rg,burst_max)
       real*8     fd_g(3,3,n_az,n_rg,burst_max)
       real*8     fd_bip(burst_max)
       real*8     r_low(burst_max)
       real*8     r_high(burst_max)
       real*8     c1_frm(2,2)
       real*8     c2_frm(2,2)
       integer     l1_ct_pro
       integer     l2_ct_pro
       real*8     lat_frm(2,2)
       real*8     lon_frm(2,2)
       real*8     ct_profile(sam_ac)
       integer    ct_prof_len
       integer*4     prod_type
       character*60     file_pvs_beam
       real*8     p1_pxl
       real*8     p2_pxl
       character*60 file_raw,file_corr_to_post,file_mask,file_data,file_aux
       character*60 file_pre_to_corr
       character*60 file_dk,file_dk_temp
       character*256    echo_file,aux_file,burst_loc_file,topo_file
       real*8      variance
       character*256  file_status
       real*8          az_peak_coef(3,8)
C*****WAYNE******
        integer   kids,mytask,ipage,ipage_start
        integer   job_mode
        integer   burst_pvs,pvs_node
C*****WAYNE******
       real*8    lamb_lat_s
       real*8    lamb_lat_n
       real*8    ps_lat,ps_lon
       integer*4 utm_zone
       character*60  file_ceos_leader,file_image,file_pmf,file_cali
       character*60  file_eph,file_rep,file_scan_results
       character*256  rep_file,eph_file,cali_file,pmf_file,ceos_leader_file
       character*256  image_file,scan_results_file
       character*60  file_pvs_look
       character*60  file_framelet_leader(4)
       integer       n_single_look
       real*8        avg_terrain
       real*8    gha_time
       real*8          gha
       integer*4       orbit_num
       character*60 file_pre_to_ceos,file_corr_to_ceos,file_post_to_ceos
       character*60 file_ceos_template
C***************
c	include 'key_const.inc'
c	include 'key_pp.inc'
c	include 'proj_const.inc'
        real b_t,e_t,t_t
        real b1_t,e1_t,t1_t
        real*8 ta,tb,rtc,ta1,tb1
        type tb_type
          sequence
          real usrtime
          real systime
        end type
        type (tb_type) dummy
        character*256  file_config
        integer*4       sim_eam_nsp(4),burst_start,burst_end
C****************************************************
        real*8          t_b(burst_max)
        real*8          rg_gate_delay(burst_max)
        real*8          gain_rep(burst_max)
        integer*4       np_v(burst_max)
        real*8          yaw_o_rate(burst_max)
        real*8          roll_o_rate(burst_max)
        real*8          pitch_o_rate(burst_max)
       real*8          sv_x_pos
       real*8          sv_y_pos
       real*8          sv_z_pos
       real*8          sv_x_vel
       real*8          sv_y_vel
       real*8          sv_z_vel
       integer*4       dk_rep(burst_max)
       integer         isuccess,ssp_get_rep
       integer         proc_id
       character*60         product_id
       real*8          window_start_time(burst_max)
       integer*4         sv_type
       character*60    file_pmf_template
       integer*4       ter_correct
       integer         nburst_spec
        real*8        aux_wdp_chg(max_agc,1600)
        integer*4     aux_wdp_np(max_agc,1600)
        integer*4     aux_agc_np(max_agc,1600)
        integer*4     aux_wdp_cnt(1600)
        real*8        aux_agc_chg(max_agc,1600)
        integer*4     aux_agc_cnt(1600)
        real*8  aux_lna_temp(1600)
        real*8  aux_subsystem_temp(1600)
        real*8  aux_protector_temp(1600)
        character*60   sitename
        real*8           aux_rx_agc(1600)
        real*8           aux_replica_agc(1600)
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
        real*8          aux_calibration_temp(1600),aux_repl_pow
        character*60    file_pmflet(4)
        integer     ant_flag
        integer     subframe_id
        real*8      bit_error_rate
        integer     pre_phase
        integer     prf_num
        character*60  file_az
        real daz(ns_ant_rg,6)
        character*10  aux_yaw_valid(burst_max),aux_roll_valid(burst_max),aux_pitch_valid(burst_max)
        integer burst_start_p,burst_end_p





       




        b_t=etime_(dummyt)
        tb =rtc()
        tb1 =rtc()
c*************************************************
        job_mode =1
        call get_config(file_config,
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




        if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
        if (mytask.eq.0) idummy=printfstat(file_status,2,0.0)

        if (ant_flag.eq.1) then
         call load_ant_parameter(file_w1_dyn,file_w2_dyn,file_w3_dyn,file_s5_dyn,
     *          file_s6_dyn,file_s7_dyn,file_az_dyn,file_cali,istatus,az_peak_coef)
         if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
        endif


        iprocessor =0
 	IF(pta_refn_mode.eq.1) THEN

        if (ire_sim.eq.1) then
          call  load_aux_parameter(file_aux,
     *             burst_start,burst_end,
     *             beam_mode,nbeams,burst_cnt,nr,burst_good,t_b,
     *             gain_rep,gain_rec,fs,prf,plen,yaw_o,yaw_o_rate,
     *             roll_o,roll_o_rate,pitch_o,pitch_o_rate,ns,
     *             np,rg_gate_delay,np_v,beam_no,beam_id,window_start_time,
     *             aux_wdp_chg,aux_wdp_np,aux_agc_np,aux_wdp_cnt,aux_agc_chg,aux_agc_cnt,
     *             aux_replica_agc,aux_lna_temp,aux_subsystem_temp,aux_protector_temp,
     *             aux_rx_agc,istatus,
     *             aux_calibration_temp,aux_repl_pow,bit_error_rate,
     *             aux_yaw_valid,aux_roll_valid,aux_pitch_valid)
            if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
            istatus=ssp_get_rep(file_rep,dk_rep)
c           plen=42.00e-6
            if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
        else
            fs= fs_3
        endif


c       use hardcode plen,fs
c       plen=42.00e-6
c       fs  =fs_3
        if (ire_sim.eq.0) then
         nr  =nint(42.00e-6*12.967e6)
        endif
        


c*************************************************
        call set_process(
     *  plen,topo_spacing,
     *  file_topo,ire_sim,file_w1_rg,file_w2_rg,
     *  file_w3_rg,file_s5_rg,file_s6_rg,file_s7_rg,
     *  rng_bm_ang,c2_pxl,fm_sizec2,c1_pxl,
     *  tbl,chirp_bw,fs,dtau,
     *  chirp_rate,nfft_az,max_look,nbeams,
     *  iquick,
     *  topo,
     *  dlat_topo,dlon_topo,ant_patn_r,ant_patn_az,
     *  v_filler,v_noise,sam_ac2,
     *  burst_start,burst_end,
     *  burst_total,
     *  istatus,
     *  dk_burst,kids,mytask,
     *  burst_start_p,burst_end_p,
     *  file_dk,burst_pvs,pvs_node,
     *  ant_flag,file_w1_dyn,file_w2_dyn,file_w3_dyn,file_s5_dyn,file_s6_dyn,file_s7_dyn,file_az_dyn,file_az,daz,
     *  iprocessor,n_times_pre)
        write(6,*)'TOTAL BURSTS  USED BY PRE-PROCESSOR:',burst_end
        write(6,*)'TOTAL BURSTS  USED BY PRE-PROCESSOR:',burst_start_p,burst_end_p


        ta1 =rtc()
        write(6,*)'set_process elapsed = ',ta1-tb1


        if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return

	write (6,*)'image_dump:',  file_image_dump
        write (6,*)'ac_image',file_ac_image
       	write (6,*)'pj_image',file_pj_image
       	write (6,*)'pj_image_low',file_pj_image_low
       	write (6,*)'pvs_beam',file_pvs_beam
       	write (6,*)'frame',file_frame


        if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
        write(6,*)'reading aux'
        tb1 =rtc()
        call read_aux_data(
     *  burst_start,burst_end,plen,nr,ire_sim,proj,
     *  bm_id,nbeams,sv_ref_time,
     *  nfft_az,fs,
     *  np,istatus,beam_no,beam_id,
     *  t_burst,burst_cnt,prf,ns,
     *  look_ang,np_air,x_sc,y_sc,
     *  z_sc,v_x_sc,v_y_sc,v_z_sc,
     *  prf_s,df,dr,r_1st,
     *  roll_o,yaw_o,yaw,pitch_o,
     *  pitch,roll,file_aux,file_spc_raw,alpha1,alpha2,alpha3,
     *  sim_eam_nsp,peak_bm_ang,
     *  sv_x_pos,sv_y_pos,sv_z_pos,sv_x_vel,sv_y_vel,sv_z_vel,
     *  t_b,gha,rg_gate_delay,np_v,gha_time)

        ta1 =rtc()
        write(6,*)'read_aux elapsed = ',ta1-tb1



c             call pta_refine  !pointing refinement
c        alpha1=0.0
c        alpha2=0.0
c        alpha3=0.0
c        dlon_topo=0.0
c        dlat_topo=0.0
c        do i=1,ns_ew
c        do j=1,ns_ns
c         topo(i,j)=0.0
c        enddo
c        enddo
c       np_proc  =56
        proc_mode=0
c       call     print_pta_var(n_times_pre,burst_total,nbeams,
c    *  ire_sim,gain_rec,ns,np,dr,df,fs,dk_burst,file_spc_raw,
c    *  data_cond,tbl,blk_gain,snr,chirp_rate,nfft_az,prf,plen,
c    *  prf_est_mode,np_proc,beam_no,az_peak_coef,beam_id,
c    *  x_sc,y_sc,z_sc,v_x_sc,v_y_sc,v_z_sc,peak_bm_ang,
c    *  rgc_est_mode,burst_cnt,roll_o,yaw_o,pitch_o,roll,yaw,
c    *  pitch,nr,rng_bm_ang,look_ang,max_look,
c    *  proc_mode,fm_sizec2_in,c1_pxl,c2_pxl,ele_ang_eq,alpha1,
c    *  alpha2,alpha3,file_raw,dem_ellip,dlon_topo,dlat_topo,topo)
c        stop
        call     pta_refine(n_times_pre,burst_total,nbeams,
     *  ire_sim,gain_rec,ns,np,dr,df,fs,dk_burst,file_spc_raw,
     *  data_cond,tbl,blk_gain,snr,chirp_rate,nfft_az,prf,plen,
     *  prf_est_mode,np_proc,beam_no,az_peak_coef,beam_id,
     *  x_sc,y_sc,z_sc,v_x_sc,v_y_sc,v_z_sc,peak_bm_ang,
     *  r_1st,
     *  np_air,
     *  rgc_est_mode,burst_cnt,roll_o,yaw_o,pitch_o,roll,yaw,
     *  pitch,nr,rng_bm_ang,look_ang,max_look,
     *  proc_mode,fm_sizec2_in,c1_pxl,c2_pxl,ele_ang_eq,alpha1,
     *  alpha2,alpha3,file_raw,dem_ellip,dlon_topo,dlat_topo,topo,
     *  kids,mytask,burst_start_p,burst_end_p,
     *  sim_eam_nsp,np_v,dk_rep,gain_rep,file_rep,
     *  aux_wdp_chg,aux_wdp_np,aux_agc_np,aux_wdp_cnt,aux_agc_chg,aux_agc_cnt,
     *  pre_phase,prf_num,ant_patn_r,beam_mode,avg_terrain,istatus,burst_good)
        if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return



           call pre_to_disk(yaw,pitch,roll,pre_phase,file_pre_to_corr,istatus)
           if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
        ENDIF
        if (mytask.eq.0) idummy=printfstat(file_status,2,100.0)

        ta =rtc()
        write(6,*)'elapsed time for pta_refine',ta-tb

	return
	end
