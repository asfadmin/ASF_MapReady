c SccsId = @(#)ONE.f	2.41 3/24/98
c	get_config.f and jin_get_config.f

        subroutine correlator(file_config,istatus,kids,mytask)
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
       real*8          az_peak_coef(3,8)
C*****WAYNE******
        integer   kids,mytask,ipage,ipage_start
        integer   job_mode
C*****WAYNE******
       real*8    lamb_lat_s
       real*8    lamb_lat_n
       real*8    ps_lat,ps_lon
       integer*4 utm_zone
       character*60  file_ceos_leader,file_image,file_pmf,file_cali
       character*60  file_eph,file_rep,file_scan_results
       character*256  rep_file,eph_file,cali_file,pmf_file,ceos_leader_file
       character*256  image_file,scan_results_file
       integer       burst_pvs,pvs_node
       character*60  file_pvs_look
       character*60  file_framelet_leader(4)
       integer       n_single_look
       real*8        avg_terrain
       real*8    gha_time
       real*8          gha
       integer*4       orbit_num
       character*60 file_pre_to_ceos,file_corr_to_ceos,file_post_to_ceos
       character*60 file_ceos_template
       real*4 maxgain
       integer hist_i(256),hist_q(256),hist_i_avg(256),hist_q_avg(256)
       real*4 hist_value(256)
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
        character*256 file_status
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
       integer*4       sv_type
       character*60    file_pmf_template
       real*8    p_x(burst_max),p_y(burst_max),p_z(burst_max)
       integer*4 ter_correct
       complex buff_spec(8192)
       integer nburst_spec
        real*8        aux_wdp_chg(max_agc,1600)
        integer*4     aux_wdp_np(max_agc,1600)
        integer*4     aux_agc_np(max_agc,1600)
        integer*4     aux_wdp_cnt(1600)
        real*8        aux_agc_chg(max_agc,1600)
        integer*4     aux_agc_cnt(1600)
        real*8  aux_lna_temp(1600)
        real*8  aux_subsystem_temp(1600)
        real*8  aux_protector_temp(1600)
        character*60  sitename
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
        integer ant_flag
        integer subframe_id
        real*8          bit_error_rate
        real*8          dfddth
        integer         pre_phase
        real*8     coef_fd_0(4,burst_max)
        real*8     coef_inc(4,burst_max)
        character*60  file_dummy
        integer       prf_num
        character*60  file_az
        real daz(ns_ant_rg,6)
        character*10  aux_yaw_valid(burst_max),aux_roll_valid(burst_max)
        character*10  aux_pitch_valid(burst_max)
        real*8        rlocal_mean
        integer burst_start_p,burst_end_p









        b_t=etime_(dummyt)
        tb =rtc()
        tb1 =rtc()
c*************************************************
        job_mode =2
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
        if (mytask.eq.0) idummy=printfstat(file_status,3,0.0)

        if (ant_flag.eq.1) then
         call load_ant_parameter(file_w1_dyn,file_w2_dyn,file_w3_dyn,file_s5_dyn,
     *          file_s6_dyn,file_s7_dyn,file_az_dyn,file_cali,istatus,az_peak_coef)
         if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
        endif


        iprocessor =1
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
           write(6,*)'FINISHED AUX LOAD ',istatus
           if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
            istatus=ssp_get_rep(file_rep,dk_rep)
c           plen=42.00d-6
            write(6,*)'FINISHED REP LOAD ',istatus
           if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
           call rhist_setup(gain_rec,burst_start,burst_end,
     *           aux_agc_cnt,aux_agc_np,aux_agc_chg,np_v,hist_value,
     *           hist_i,hist_q,maxgain)
        else
            fs = fs_3
        endif

CC RESOLVE DIFFERENT CASES for the values fs,plen,nr
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
        write(6,*)'TOTAL BURSTS  USED BY PROCESSOR:',burst_end
        write(6,*)'TOTAL BURSTS  USED BY PROCESSOR:',burst_start_p,burst_end_p

        write(6,*)'tbl(119) second ',tbl(119)

        ta1 =rtc()
        write(6,*)'set_process elapsed = ',ta1-tb1


        if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return

	write (6,*)'image_dump:',  file_image_dump
        write (6,*)'ac_image',file_ac_image
       	write (6,*)'pj_image',file_pj_image
       	write (6,*)'pj_image_low',file_pj_image_low
       	write (6,*)'pvs_beam',file_pvs_beam
       	write (6,*)'frame',file_frame

c	if (ire_sim .eq. 1) then
c	   write (6,*)' RAW/AUX Data out of range'
c	   istatus= ierr_main_2
c	   return
c	endif

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



 	if(pta_refn_mode.eq.1) then
           write(6,*)'file_pre_to_corr ',file_pre_to_corr
           call disk_to_corr(yaw,pitch,roll,pre_phase,file_pre_to_corr,istatus)
           if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
        endif
c       file_dummy='/spdata/people/jwtun/update15_port/roll.dat'
c       call read_disk(file_dummy,roll,1600*8,0,istatus)
c       if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
c       file_dummy='/spdata/people/jwtun/update15_port/yaw.dat'
c       call read_disk(file_dummy,yaw,1600*8,0,istatus)
c       if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
c       file_dummy='/spdata/people/jwtun/update15_port/pitch.dat'
c       call read_disk(file_dummy,pitch,1600*8,0,istatus)
c       if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
c       do i=1,1600
c       write(6,*)'r,y,p',roll(i),yaw(i),pitch(i)
c       enddo

c	call asfsetprocessstate(3)

        write(6,*)'processing proc'
        tb1 =rtc()
        proc_mode=1
	call proc_parameter(
     *  burst_start,burst_end,burst_cnt,nr,
     *  roll_o,roll,yaw_o,yaw,
     *  pitch_o,pitch,x_sc,y_sc,
     *  z_sc,v_x_sc,v_y_sc,v_z_sc,
     *  beam_id,rng_bm_ang,look_ang,ire_sim,
     *  np,ns,prf,np_proc,
     *  dr,r_1st,max_look,beam_no,
     *  fm_sizec2_in,nfft_az,dem_ellip,c1_pxl,
     *  c2_pxl,sam_ac2,nbeams,dlon_topo,dlat_topo,
     *  topo,fm_sizec1,fm_sizec2,
     *  proc_mode,i_ang_bip,r_bip,fr_bip,
     *  coef_fd,coef_fr,coef_lk_ang,np_air,
     *  fd_near,fr_near,fd_far,fr_far,
     *  r_cntr,fd_cntr,fr_cntr,pbw_near,
     *  fd_low_near,fd_high_near,pbw_far,fd_low_far,
     *  fd_high_far,pbw_cntr,fd_low_cntr,fd_high_cntr,
     *  c1_flm,c2_flm,v_sc_mag,c1_cell,
     *  c2_cell,c1size_blk,c2size_blk,n_c1_blk,
     *  n_c2_blk,c1_g,c2_g,r_g,
     *  fd_g,fd_bip,r_low,r_high,
     *  alpha1,alpha2,alpha3,c1_frm,
     *  c2_frm,l1_ct_pro,l2_ct_pro,lat_frm,
     *  lon_frm,az_peak_coef,p_x,p_y,p_z,
     *  dfddth,pre_phase,coef_fd_0,avg_terrain,coef_inc,rlocal_mean)
        ta1 =rtc()
        write(6,*)'proc_parameter elapsed = ',ta1-tb1

c          call  fd_profile(
c    *  r_low,r_high,dr,r_bip,coef_lk_ang,prf,dfddth,
c    *  nbeams,burst_end,ire_sim,gain_rec,ns,np,df,fs,
c    *  dk_burst,file_spc_raw,data_cond,tbl,blk_gain,snr,
c    *  chirp_rate,fd_bip,nfft_az,r_1st,coef_fd,coef_fr,
c    *  istatus,file_raw,nr,sim_eam_nsp,np_v,dk_rep,gain_rep,file_rep,
c    *  aux_wdp_chg,aux_wdp_np,aux_agc_np,aux_wdp_cnt,aux_agc_chg,aux_agc_cnt,
c    *  coef_fd_0,v_sc_mag,x_sc,y_sc,z_sc,v_x_sc,v_y_sc,v_z_sc,rng_bm_ang,
c    *  beam_no,beam_id,az_peak_coef,roll,yaw,pitch,look_ang,dem_ellip,
c    *  dlon_topo,dlat_topo,topo,i_ang_bip,fr_bip,np_air,fd_near,fr_near,fd_far,
c    *  fr_far,r_cntr,fd_cntr,fr_cntr,avg_terrain,coef_inc,
c    *  burst_start_p,burst_end_p,mytask,kids,allgrp,beam_mode)
c       stop


c	call MP_SET_NUMTHREADS (num_threads)
  
        if(iquick.eq.1) then
	
c       call quick_process(
c    *  ire_sim,burst_start,burst_end,burst_good,c1_pxl,
c    *  c2_pxl,fm_sizec1,fm_sizec2,image_beam_mode,
c    *  beam_no,file_pj_image_low,v_filler,
c    *  fs,file_spc_raw,ns,
c    *  chirp_rate,fd_bip,nfft_az,r_1st,
c    *  dr,r_bip,coef_fd,prf,
c    *  coef_fr,r_g,c1_g,c2_g,
c    *  nr,v_sc_mag,coef_lk_ang,look_ang,
c    *  roll_o,roll,ant_patn_r,beam_id,
c    *  ant_patn_az,v_noise,n_c2_blk,fd_g,
c    *  c1_frm,c2_frm,c1size_blk,c2size_blk,
c    *  df,istatus,plen,dk_burst,file_raw)
c       if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return


	else

c	     call process			!SAR signal processing
        tb1 =rtc()
         call process(
     *  burst_start_p,burst_end_p,plen,nr,np,beam_no,
     *  image_beam_mode,burst_no_dump,stage_dump,
     *  fs,file_spc_raw,ns,
     *  ire_sim,gain_rec,dk_burst,data_cond,
     *  blk_gain,chirp_rate,fd_bip,nfft_az,
     *  r_1st,dr,r_bip,coef_fd,
     *  i_ang_bip,prf,coef_fr,pbw_far,pbw_near,
     *  r_high,r_low,v_sc_mag,coef_lk_ang,
     *  look_ang,roll_o,roll,df,
     *  ant_patn_az,beam_id,ant_patn_r,radio_comp_mode,
     *  v_noise,c1size_blk,c2size_blk,c1_cell,
     *  c2_cell,c1_g,c2_g,n_c1_blk,
     *  n_c2_blk,r_g,fd_g,c1_pxl,
     *  c2_pxl,sam_ac2,c1_frm,c2_frm,
     *  file_image_dump,norm_look_mode,v_filler,l1_ct_pro,
     *  l2_ct_pro,fm_sizec2_in,tbl,snr,burst_good,istatus,
     *  prod_type,file_pvs_beam,file_framelet,ct_profile,ct_prof_len,
     *  kids,mytask,ipage,ipage_start,file_raw,file_mask,file_data,variance,
     *  file_status,nbeams,burst_pvs,file_pvs_look,
     *  hist_i_avg,hist_q_avg,n_single_look,
     *  sim_eam_nsp,np_air,np_v,dk_rep,gain_rep,file_rep,buff_spec,nburst_spec,
     *  aux_wdp_chg,aux_wdp_np,aux_agc_np,aux_wdp_cnt,aux_agc_chg,aux_agc_cnt,
     *  hist_i,hist_q,maxgain,proc_gain,dfddth,coef_inc,daz,fm_sizec2,c2_flm,
     *  rlocal_mean)
        if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
        ta1 =rtc()
        write(6,*)'process elapsed = ',ta1-tb1
	
ccc          call combine !coarse grain mosaicking

        tb1=rtc()
         write(6,*)'QdN after process, before correlator_to_disk:',
     *             ' ipage=',ipage,' ipage_start=',ipage_start
         call correlator_to_disk(
     *   c1_frm,c2_frm,
     *   sam_ac2,ct_prof_len,l1_ct_pro,l2_ct_pro,
     *   v_filler,
     *   ct_profile,
     *   alpha1,
     *   alpha2,alpha3,
     *   ipage,ipage_start,file_corr_to_post,variance,istatus,rlocal_mean)
        if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return

        call dump_corr_to_ceos(file_corr_to_ceos,
     *  prf,np,np_air,burst_start,burst_end,chirp_rate,fs,
     *  x_sc,y_sc,z_sc,v_x_sc,v_y_sc,v_z_sc,
     *  c1_flm,c2_flm,fd_low_near,fd_low_far,fr_near,fr_far,i_ang_bip,
     *  look_ang,r_low,r_high,chirp_bw,hist_i_avg,hist_q_avg,
     *  yaw,roll,pitch,yaw_o,roll_o,pitch_o,pvs_node,
     *  yaw_o_rate,roll_o_rate,pitch_o_rate,p_x,p_y,p_z,
     *  window_start_time,t_burst,c1_frm,c2_frm,alpha1,alpha2,alpha3,
     *  np_v,
     *  c1_g,c2_g,c2_cell,c1size_blk,n_c2_blk,buff_spec,r_bip,
     *  coef_fd,ns,
     *  aux_replica_agc,aux_lna_temp,aux_subsystem_temp,aux_protector_temp,
     *  aux_rx_agc,istatus,aux_calibration_temp,aux_repl_pow,bit_error_rate,
     *  gain_rec,r_cntr,aux_yaw_valid,aux_roll_valid,aux_pitch_valid,t_b)
        if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return

        ta1=rtc()
        write(6,*)'correlator proc elpased = ',ta1-tb1



	end if
        e_t=etime_(dummyt)
        t_t=e_t-b_t
        ta =rtc()
        write(6,*)'total cpu     = ',t_t
        write(6,*)'total elpased = ',ta-tb
        if (mytask.eq.0) idummy=printfstat(file_status,3,100.0)

	return
	end

       subroutine proc_parameter(
     *  burst_start,burst_end,burst_cnt,nr,
     *  roll_o,roll,yaw_o,yaw,
     *  pitch_o,pitch,x_sc,y_sc,
     *  z_sc,v_x_sc,v_y_sc,v_z_sc,
     *  beam_id,rng_bm_ang,look_ang,ire_sim,
     *  np,ns,prf,np_proc,
     *  dr,r_1st,max_look,beam_no,
     *  fm_sizec2_in,nfft_az,dem_ellip,c1_pxl,
     *  c2_pxl,sam_ac2,nbeams,dlon_topo,dlat_topo,
     *  topo,fm_sizec1,fm_sizec2,
     *  proc_mode,i_ang_bip,r_bip,fr_bip,
     *  coef_fd,coef_fr,coef_lk_ang,np_air,
     *  fd_near,fr_near,fd_far,fr_far,
     *  r_cntr,fd_cntr,fr_cntr,pbw_near,
     *  fd_low_near,fd_high_near,pbw_far,fd_low_far,
     *  fd_high_far,pbw_cntr,fd_low_cntr,fd_high_cntr,
     *  c1_flm,c2_flm,v_sc_mag,c1_cell,
     *  c2_cell,c1size_blk,c2size_blk,n_c1_blk,
     *  n_c2_blk,c1_g,c2_g,r_g,
     *  fd_g,fd_bip,r_low,r_high,
     *  alpha1,alpha2,alpha3,c1_frm,
     *  c2_frm,l1_ct_pro,l2_ct_pro,lat_frm,
     *  lon_frm,az_peak_coef,p_x,p_y,p_z,
     *  dfddth,pre_phase,coef_fd_0,avg_terrain,coef_inc,rlocal_mean)
	implicit real*8 (a-h, o-z)
c	include 'key_const.inc'
c	include 'key_pp.inc'
C*****WAYNE******
       include 'ssp2_const.inc'
       real*8        avg_terrain
       real*8          az_peak_coef(3,8)
       integer*4     burst_start
       integer*4     burst_end
       integer*4     nr
       integer     proc_mode
       integer*4     burst_cnt(burst_max)
       real*8     roll_o(burst_max)
       real*8     roll(burst_max)
       real*8     yaw_o(burst_max)
       real*8     yaw(burst_max)
       real*8     pitch_o(burst_max)
       real*8     pitch(burst_max)
       real*8     x_sc(burst_max)
       real*8     y_sc(burst_max)
       real*8     z_sc(burst_max)
       real*8     v_x_sc(burst_max)
       real*8     v_y_sc(burst_max)
       real*8     v_z_sc(burst_max)
       integer*4     beam_id(burst_max)
       real*8     rng_bm_ang(4)
       real*8     look_ang(burst_max)
       integer     ire_sim
       integer*4     np(burst_max)
       integer*4     ns(burst_max)
       real*8     prf(burst_max)
       integer     np_proc
       real*8     dr(burst_max)
       real*8     r_1st(burst_max)
       integer*4     max_look
       integer*4     beam_no(burst_max)
       real*8     fm_sizec2_in
       integer*4     nfft_az
       integer     dem_ellip
       real*8     c1_pxl
       real*8     c2_pxl
       integer*4  sam_ac2
       integer*4     nbeams
       real*8     i_ang_bip(burst_max)
       real*8     r_bip(burst_max)
       real*8     fr_bip(burst_max)
       real*8     coef_fd(4,burst_max)
       real*8     coef_fr(4,burst_max)
       real*8     coef_lk_ang(4,burst_max)
       integer*4     np_air(burst_max)
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
       real*8     dlon_topo
       real*8     dlat_topo
       integer*2     topo(ns_ew,ns_ns)
       real*8     alpha1
       real*8     alpha2
       real*8     alpha3
       real*8     fm_sizec1
       real*8     fm_sizec2
       real*8     c1_frm(2,2)
       real*8     c2_frm(2,2)
       integer     l1_ct_pro
       integer     l2_ct_pro
       real*8     lat_frm(2,2)
       real*8     lon_frm(2,2)
       real*8    p_x(burst_max),p_y(burst_max),p_z(burst_max)
       real*8    dfddth
       integer   pre_phase
       real*8     coef_fd_0(4,burst_max)
       real*8     coef_inc(4,burst_max)
       real*8     rlocal_mean
C*****WAYNE******
       integer    k
c** set processing mode ******************************************
	proc_mode = 1	 	!(0=pre process, 1=main process)

	do k = burst_start, burst_end	!loop through all bursts
        call burst_pp(
     *  k,burst_cnt,roll_o,roll,nr,
     *  yaw_o,yaw,pitch_o,pitch,
     *  x_sc,y_sc,z_sc,v_x_sc,
     *  v_y_sc,v_z_sc,beam_id,rng_bm_ang,
     *  look_ang,ire_sim,np,ns,
     *  prf,np_proc,dr,r_1st,
     *  max_look,proc_mode,beam_no,fm_sizec2_in,
     *  nfft_az,dem_ellip,c1_pxl,c2_pxl,
     *  nbeams,dlon_topo,dlat_topo,topo,
     *  i_ang_bip,r_bip,fr_bip,coef_fd,
     *  coef_fr,coef_lk_ang,np_air,fd_near,
     *  fr_near,fd_far,fr_far,r_cntr,
     *  fd_cntr,fr_cntr,pbw_near,fd_low_near,
     *  fd_high_near,pbw_far,fd_low_far,fd_high_far,
     *  pbw_cntr,fd_low_cntr,fd_high_cntr,c1_flm,
     *  c2_flm,v_sc_mag,c1_cell,c2_cell,
     *  c1size_blk,c2size_blk,n_c1_blk,n_c2_blk,
     *  c1_g,c2_g,r_g,fd_g,
     *  fd_bip,r_low,r_high,alpha1,
     *  alpha2,alpha3,az_peak_coef,p_x,p_y,p_z,
     *  dfddth,pre_phase,coef_fd_0,avg_terrain,coef_inc,rlocal_mean)
	end do

        call frame_pp(
     *  beam_no,c1_flm,c2_flm,nbeams,
     *  burst_start,burst_end,fm_sizec1,fm_sizec2,
     *  c1_g,c1_pxl,c2_pxl,c1_frm,c2_frm,l1_ct_pro,l2_ct_pro,
     *  lat_frm,lon_frm,alpha1,alpha2,
     *  alpha3,rlocal_mean       )

c       call c2_to_lkang(c1_flm,c2_flm,c2_frm,c2_pxl,nbeams,fm_sizec2,
c    *                         sam_ac2,x_sc,y_sc,z_sc,r_bip,coef_lk_ang,
c    *                         avg_terrain,dlon_topo,dlat_topo,topo,dem_ellip,
c    *                         rlocal_mean,alpha1,alpha2,alpha3)

	return
	end

c*************************************************************************

        subroutine c2_to_lkang(c1_flm,c2_flm,c2_frm,c2_pxl,nbeams,fm_sizec2,
     *                         sam_ac2,x_sc,y_sc,z_sc,r_bip,coef_lk_ang,
     *                         avg_terrain,dlon_topo,dlat_topo,topo,dem_ellip,
     *                         rlocal_mean,alpha1,alpha2,alpha3)
C***************************************
        implicit real*8 (a-h, o-z)
        include 'ssp2_const.inc'
       real*8     rlocal_mean
       real*8     alpha1,alpha2,alpha3
       real*8     c1_flm(2,2,burst_max)
       real*8     c2_flm(2,2,burst_max)
       real*8     c2_frm(2,2)
       real*8     c2_pxl
       integer    nbeams
       real*8     fm_sizec2
       integer     sam_ac2
       real*8     x_sc(burst_max)
       real*8     y_sc(burst_max)
       real*8     z_sc(burst_max)
       real*8     r_bip(burst_max)
       real*8     coef_lk_ang(4,burst_max)
       real*8     avg_terrain
       real*8     dlon_topo
       real*8     dlat_topo
       integer*2     topo(ns_ew,ns_ns)
       integer     dem_ellip

        real*8 lat,lat_d,lon
        integer c2lkang(11000), c2_stn(5)

c Local variables
        integer    ipt, istat
        integer    k,m, n_truncate,kb1,i,kbeam
        real*8     c1_temp, c2_temp
        real*8     x_g,y_g,z_g,h,vxt,vyt,vzt
        real*8     vlook_ang
        real*8     dis, rst


        m = 4

        n_truncate = 200
        kb1 = 1
        c2_stn(1) = (c2_flm(1,1,kb1)-c2_frm(1,1))/c2_pxl+n_truncate
        do i = 1, nbeams-1
c       c2_stn(i+1)=(c2_flm(1,2,kb1+i-1)*3./4.+c2_flm(1,1,kb1+i)/4.
c    1                  -c2_frm(1,1))/c2_pxl
c 9/22/96 
        c2_stn(i+1)=(c2_flm(1,2,kb1+i-1)*0.5+c2_flm(1,1,kb1+i)*0.5
     1                  -c2_frm(1,1))/c2_pxl
        end do
        c2_stn(nbeams+1) = fm_sizec2/c2_pxl

        c1_temp = c1_flm(1,1,1)
        do k = 1, sam_ac2
        c2_temp = c2_frm(1,1) + (k-1)* c2_pxl
           kbeam = 1
           do i = 2,4
           if(k.ge.c2_stn(i)) kbeam = i !determine beam number
           end do
        call ac_to_ebf(c1_temp,c2_temp,x_g,y_g,z_g,lat,lat_d,lon,rlocal_mean)
        call elevation(lat,lon,h,avg_terrain,
     *  dem_ellip,dlon_topo,dlat_topo,topo )
        call get_tv(lat,lon,lat_d,h,x_g,y_g,z_g,vxt,vyt,vzt)
        rst = dis(x_sc(1),y_sc(1),z_sc(1),x_g,y_g,z_g)
        call v_poly(rst,r_bip(kbeam),coef_lk_ang(1,kbeam),m,vlook_ang)
        c2lkang(k) = nint((vlook_ang-look_ang_cnt-ele_ang_ref)/dang_rg)+1
D       if(k.eq.k/20*20) write(6,*) 'c2_to_lkang', k,c2lkang(k)
        end do

c       ipt = 0
c       call write_disk('/home/tmpdisk/c2_to_lk.dat  ',
c    *                        c2lkang,sam_ac2*4,ipt,istat)

        return
        end






	



c*********************************************************************
	subroutine rotate_x(x,y,z,theta0,sign)

C	--------------------------------
C 	Abstract:
C		Performs coordinate rotation about the X axis
C		by an angle of theta0.
C	--------------------------------
	implicit none

        character*128 SccsId_ONE
        data SccsId_ONE
     +  /'@(#)PPONE.f:2.41'/


c	----------
c	PASSING INPUT/OUTPUT PARAMETERS
c	----------
	real*8 x, y, z		

c	----------
c	PASSING INPUT PARAMETERS
c	----------
	real*8 theta0
	real*4 sign

c	----------
c	LOCAL VARIABLES
c	----------
	real*8 theta
	real*8 z1, y1

	theta=theta0*sign
	y1=y*cosd(theta)+z*sind(theta)
	z1=-1*y*sind(theta)+z*cosd(theta)

c	x=x

	z=z1
	y=y1

D	write(6,*)' rotate_X alpha:',theta

	return
	end

c*********************************************************************
c*********************************************************************
c*********************************************************************
c****************************************************************
c*********************************************************************
c*********************************************************************



        



	subroutine frame_pp(
     *  beam_no,c1_flm,c2_flm,nbeams,
     *  burst_start,burst_end,fm_sizec1,fm_sizec2,
     *  c1_g,c1_pxl,c2_pxl,c1_frm,c2_frm,l1_ct_pro,l2_ct_pro,
     *  lat_frm,lon_frm,alpha1,alpha2,
     *  alpha3,rlocal_mean       )
	implicit real*8 (a-h, o-z)
C*****WAYNE******
       include 'ssp2_const.inc'
       real*8       rlocal_mean
       integer*4     beam_no(burst_max)
       real*8     c1_flm(2,2,burst_max)
       real*8     c2_flm(2,2,burst_max)
       integer*4     nbeams
       integer*4     burst_start
       integer*4     burst_end
       real*8     fm_sizec1
       real*8     fm_sizec2
       real*8     c1_g(3,3,n_az,n_rg,burst_max)
       real*8     c1_pxl
       real*8     c2_pxl
       real*8     c1_frm(2,2)
       real*8     c2_frm(2,2)
       integer     l1_ct_pro
       integer     l2_ct_pro
       real*8     lat_frm(2,2)
       real*8     lon_frm(2,2)
       real*8     alpha1
       real*8     alpha2
       real*8     alpha3
C*****WAYNE******
c	include 'key_const.inc'
c	include 'key_pp.inc'
	real*8 c1_min_frm,c2_min_frm,lat_d

c***************** START PROCESSING PARAMETER FOR IMAGE FRAME *********
     	kb1 = 1
	if(beam_no(kb1).ne.1) kb1 = kb1+5-beam_no(kb1)
        if(nbeams.ge.3) then
        c1_min_frm = min(c1_flm(1,1,kb1),c1_flm(1,2,kb1),c1_flm(1,1,kb1+1),
     *  c1_flm(1,2,kb1+1),
     *  c1_flm(1,1,kb1+2),c1_flm(1,2,kb1+2),c1_flm(1,1,kb1+nbeams-1),
     *  c1_flm(1,2,kb1+nbeams-1))
        else
        c1_min_frm = min(c1_flm(1,1,kb1),c1_flm(1,2,kb1),c1_flm(1,1,kb1+1),
     *  c1_flm(1,2,kb1+1))
        end if

	kb2 = burst_end
	if(beam_no(kb2).ne.1) kb2 = kb2+1-beam_no(kb2)
	c2_min_frm = min(c2_flm(1,1,kb1),c2_flm(1,1,kb2))
D	write(6,*)'c2', c2_flm(1,1,kb1), c2_flm(1,1,kb2)
D	write(6,*)'kb1,kb2', kb1, kb2

        n_truncate = 200

c       if (int(c1_pxl).eq.50) then
c        c1_min_frm = nint(c1_min_frm/space_50-4)*space_50      !frame minimum c1
c        c2_min_frm = nint(c2_min_frm/space_50-4+n_truncate)*space_50   !frame minimum c2
c       else
c        c1_min_frm = nint(c1_min_frm/space_100-4)*space_100    !frame minimum c1
c        c2_min_frm = nint(c2_min_frm/space_100-4+n_truncate)*space_100 !frame minimum c2
c       endif
        c1_min_frm = nint(c1_min_frm/c1_pxl-4)*c1_pxl                           !frame mini
        c2_min_frm = nint(c2_min_frm/c2_pxl-4+100./c2_pxl*n_truncate)*c2_pxl    !frame mini


c	c1_min_frm = nint(c1_min_frm/space_100-4)*space_100	!frame minimum c1
c	c2_min_frm = nint(c2_min_frm/space_100-4+n_truncate)*space_100	!frame minimum c2

	c1_frm(1,1) = c1_min_frm				!frame corner c1
	c1_frm(2,1) = c1_min_frm + fm_sizec1-c1_pxl		!frame corner c1
	c1_frm(1,2) = c1_min_frm 				!frame corner c1
	c1_frm(2,2) = c1_min_frm + fm_sizec1-c1_pxl		!frame corner c1

	c2_frm(1,1) = c2_min_frm				!frame corner c2
	c2_frm(2,1) = c2_min_frm 				!frame corner c2
	c2_frm(1,2) = c2_min_frm + fm_sizec2-c2_pxl		!frame corner c2
	c2_frm(2,2) = c2_min_frm + fm_sizec2-c2_pxl		!frame corner c2

D	write(6,*) 'c1_flm', c1_flm(1,1,kb1)
D	write(6,*) 'c1_flm', c1_flm(1,2,kb1)
D	write(6,*) 'c1_flm', c1_flm(1,1,kb1+3)
D	write(6,*) 'c1_flm', c1_flm(1,2,kb1+3)
D	write(6,*) 'c1_g', c1_g(1,1,1,1,kb1)
D	write(6,*) 'c1_g', c1_g(1,1,1,1,kb1+3)


	l1_ct_pro = (max(c1_flm(1,1,kb1),c1_flm(1,2,kb1),	!start line # for ct_p
     *	 c1_flm(1,1,kb1+nbeams-1),c1_flm(1,2,kb1+nbeams-1))
     *	 -c1_frm(1,1))/c1_pxl+1+4

	l2_ct_pro = (min(c1_flm(2,1,kb2),c1_flm(2,2,kb2),	!end line # for ct_p
     *	 c1_flm(2,1,kb2+nbeams-1),c1_flm(2,2,kb2+nbeams-1))
     *	 -c1_frm(1,1))/c1_pxl+1-4

D	write(6,*)'l1, l2', l1_ct_pro,l2_ct_pro

	do k1 = 1, 2
	do k2 = 1, 2
	call ac_to_ebf(c1_frm(k1,k2),c2_frm(k1,k2),x,y,z,
     *		lat_frm(k1,k2),lat_d,lon_frm(k1,k2),rlocal_mean,
     *          alpha1,alpha2,alpha3)	    !frame corner lat_lon
	end do
	end do

D	write(6,*) 'c12_frm(1,1)', c1_frm(1,1), c2_frm(1,1)
D	write(6,*) 'end of pp generation'

	return
	end


       subroutine process(
     *  burst_start,burst_end,plen,nr,np,beam_no,
     *  image_beam_mode,burst_no_dump,stage_dump,
     *  fs,file_spc_raw,ns,
     *  ire_sim,gain_rec,dk_burst,data_cond,
     *  blk_gain,chirp_rate,fd_bip,nfft_az,
     *  r_1st,dr,r_bip,coef_fd,
     *  i_ang_bip,prf,coef_fr,pbw_far,pbw_near,
     *  r_high,r_low,v_sc_mag,coef_lk_ang,
     *  look_ang,roll_o,roll,df,
     *  ant_patn_az,beam_id,ant_patn_r,radio_comp_mode,
     *  v_noise,c1size_blk,c2size_blk,c1_cell,
     *  c2_cell,c1_g,c2_g,n_c1_blk,
     *  n_c2_blk,r_g,fd_g,c1_pxl,
     *  c2_pxl,sam_ac2,c1_frm,c2_frm,
     *  file_image_dump,norm_look_mode,v_filler,l1_ct_pro,
     *  l2_ct_pro,fm_sizec2_in,tbl,snr,burst_good,istatus,prod_type,
     *  file_pvs_beam,file_framelet,ct_profile,ct_prof_len,kids,mytask,
     *  ipage,ipage_start,file_raw,file_mask,file_data,variance,file_status,
     *  nbeams,burst_pvs,file_pvs_look,hist_i_avg,hist_q_avg,n_single_look,
     *  sim_eam_nsp,np_air,np_v,dk_rep,gain_rep,file_rep,buff_spec,nburst_spec,
     *  aux_wdp_chg,aux_wdp_np,aux_agc_np,aux_wdp_cnt,aux_agc_chg,aux_agc_cnt,
     *  hist_i,hist_q,maxgain,proc_gain,dfddth,coef_inc,daz,fm_sizec2,c2_flm,
     *  rlocal_mean)

c
        implicit real*8 (a-h, o-z)
C*****WAYNE******
       include 'ssp2_const.inc'
       include 'ssp2_dim.inc'
C*****WAYNE******
       real*8     rlocal_mean
       real*8     c2_flm(2,2,burst_max)
       integer  sim_eam_nsp(4)
       real*8     fm_sizec2
       integer*4       np_air(burst_max)
       integer*4       np_v(burst_max)
       integer*4       dk_rep(burst_max)
       real*8          gain_rep(burst_max)
C*****WAYNE******
       complex buff_rep(ns_rep_buff,1)
       real*8        plen
       integer*4     nr
       integer*4     burst_start
       integer*4     burst_end
       integer*4     np(burst_max)
       integer*4     beam_no(burst_max)
       integer     image_beam_mode
       integer     burst_no_dump
       integer     stage_dump
       integer     np_proc
       integer     ipinpon
       integer     disk_ptr
       real*8    buff6(sam_ac,sam_ac1_2page)
       byte     buff7(sam_ac,sam_ac1_2page)
       real*8     ct_profile(sam_ac)
       integer    ct_prof_len
       real*8     fs
       character*60     file_spc_raw
       character*60     file_rep
       byte     buff0(2*sam_raw,line_raw)
       integer*4     istatus
       integer*4     ns(burst_max)
       complex     buff1(sam_raw,line_raw)
       integer     ire_sim
       real*8     gain_rec(burst_max)
       integer*4     dk_burst(burst_max)
       integer     data_cond
       complex     tbl(-128:127)
       integer*4     blk_gain(iq_blk,burst_max)
       real*8     chirp_rate
       real*8     fd_bip(burst_max)
       integer*4     nfft_az
       real*8     r_1st(burst_max)
       real*8     dr(burst_max)
       real*8     r_bip(burst_max)
       real*8     coef_fd(4,burst_max)
       real*8     prf(burst_max)
       real*8         i_ang_bip(burst_max)
       complex     buff2(line_raw,sam_raw)
       real*8     coef_fr(4,burst_max)
       real*8     pbw_far(burst_max)
       real*8     pbw_near(burst_max)
       real*8     r_high(burst_max)
       real*8     r_low(burst_max)
       real*8     v_sc_mag(burst_max)
       real*8     coef_lk_ang(4,burst_max)
       real*8     look_ang(burst_max)
       real*8     roll_o(burst_max)
       real*8     roll(burst_max)
       real*8     df(burst_max)
       real     ant_patn_az(ns_ant_az)
       integer*4     beam_id(burst_max)
       real     ant_patn_r(ns_ant_rg,7)
       integer     radio_comp_mode
       integer*4     v_noise
       complex     buff2a(np_geo,sam_raw)
       real*8     c1size_blk(burst_max)
       real*8     c2size_blk(burst_max)
       real*8     c1_cell(burst_max)
       real*8     c2_cell(burst_max)
       real*8     c1_g(3,3,n_az,n_rg,burst_max)
       real*8     c2_g(3,3,n_az,n_rg,burst_max)
       integer*4     n_c1_blk(burst_max)
       integer*4     n_c2_blk(burst_max)
       real*8     r_g(3,3,n_az,n_rg,burst_max)
       real*8     fd_g(3,3,n_az,n_rg,burst_max)
       real     buff3(sam_raw,np_geo)
       real*8     c1_pxl
       real*8     c2_pxl
       real     buff4(sam_post,np_geo)
       byte     buff5(sam_post,np_geo)
       integer*4     sam_ac2
       real*8     c1_frm(2,2)
       real*8     c2_frm(2,2)
       character*60     file_image_dump
       integer     norm_look_mode
       integer*4       burst_good(1600)
       integer*4     v_filler
       integer     l1_ct_pro
       integer     l2_ct_pro
       real*8     fm_sizec2_in
       integer*4     prod_type
       character*60     file_pvs_beam
       character*60     file_framelet(4)
        integer   kids,mytask
       integer  ipage,ipage_start
       real*8 ta,tb,rtc,ta1,tb1,ta2,tb2
       integer nbuf(4),dontcare,allmsg,nulltask,allgrp
       character*60 file_raw,file_mask,file_data
       character*10  str_buff1,str_buff2
       character*(*)  file_status
       integer*4    nbeams
        integer  burst_pvs
       character*60 file_pvs_look
       real*4 maxgain
       integer hist_i(256),hist_q(256),hist_i_avg(256),hist_q_avg(256)
       integer n_single_look
       complex buff_spec(8192)
       integer nburst_spec
        real*8        aux_wdp_chg(max_agc,1600)
        integer*4     aux_wdp_np(max_agc,1600)
        integer*4     aux_agc_np(max_agc,1600)
        integer*4     aux_wdp_cnt(1600)
        real*8        aux_agc_chg(max_agc,1600)
        integer*4     aux_agc_cnt(1600)
       real*8          proc_gain
       real*8          dfddth
       real*8         coef_inc(4,burst_max)
       real daz(ns_ant_rg,6)



C*****WAYNE******
c	include 'key_const.inc'
c	include 'key_pp.inc'
	integer grid_mode,image_combine
        real*8   variance
        integer  ic_var
        real*4   a_percent
c************NEW ARRAY***************
        integer b_x,b_y,x_len,y_len
        integer ival,nbytes
        byte buff_b_xy(sam_raw,line_raw)
        real*4  buff_r4_xy(sam_raw,line_raw)
        byte buff_b_yx(line_raw,sam_raw)
        real*4  buff_r4_yx(line_raw,sam_raw)
        byte buff_b_rad(np_geo,sam_raw)
        real*4  buff_r4_rad(np_geo,sam_raw),val
        byte buff_b_geo(sam_raw,np_geo)
        character*60 rg_file
        character*60 eh_file
        integer ch_fd,ccreate,cwrite
c************NEW ARRAY***************

        igetout=0
           idummy=int_to_str(burst_start,str_buff1,10)
           idummy=int_to_str(burst_end  ,str_buff2,10)
           idummy=printflog(3,'processing burst# '//str_buff1//' to '//str_buff2//'&')
        allgrp   = MPI_COMM_WORLD
        call mpi_barrier(MPI_COMM_WORLD,ierr)
        ta=rtc()
        write(6,*)'processing burst# ',burst_start,' to ',burst_end
c       idummy=map_alloc(sam_ac*512*3*4,ptr_image,sam_ac*512*3,ptr_mask)
c       lptr_image=ptr_image
c       lptr_mask =ptr_mask
c       do j = 1, 512*3
c          do i = 1, sam_ac
c          wayne_image (i,j)=0.0
c          wayne_mask  (i,j)=0
c          enddo
c       enddo
      

        call init_spec(buff_spec)


	ijk = 0		!9 test overlay by bypass fore-process
	iim_mode = 0	!1 insert image to test overlay
	grid_mode = 0

c***************** start main processing ********************************

        call clear_frame(
     *  sam_ac2,buff6,buff7,ct_profile )

	ipinpon = 1
	disk_ptr = 0
        ipage=0
        variance=0.
        ic_var  =0
	
        do 1234 k = burst_start, burst_end	!loop through all bursts
        if (mytask.eq.0) then
         a_percent=(k*1.0)/burst_end*100.0
         idummy=printfstat(file_status,3,a_percent)
        endif

c	write(6,*) image_beam_mode
 	write(6,*) beam_no(k), ',  burst no = ', k

	if (burst_good(k) .eq. yes ) then

	if(ijk.ne.0) go to 1212

	if(image_beam_mode.ne.0.and.		!determine process or skip
     *	beam_no(k).ne.image_beam_mode) go to 1234

        do while (nfft_az .lt. np_v(k))       ! AC 10/3/95
           nfft_az=nfft_az*2
        enddo

        if (ire_sim.eq.0) then
        np_proc = np(k) - 9
        call get_burst_sim_hist(
     *  k,plen,np_proc,fs,file_spc_raw,
     *  ns,buff0,istatus,buff1,dk_burst,file_raw,
     *  hist_i,hist_q,sim_eam_nsp)
        else
        np_proc= np_v(k)
        call get_burst_hist(
     *  k,plen,ire_sim,gain_rec,ns,
     *  np,dk_burst,file_spc_raw,data_cond,
     *  np_proc,fs,tbl,blk_gain,snr,
     *  buff0,buff1,istatus,file_raw,nr,
     *  prod_type,maxgain,hist_i,hist_q,np_v,
     *  aux_wdp_chg,aux_wdp_np,aux_agc_np,aux_wdp_cnt,aux_agc_chg,aux_agc_cnt)
D       write(6,*)'out get_burst'
c       call get_rep(k,
c    *       nr,dk_rep,gain_rep,file_rep,istatus,tbl,buff_rep)
        endif
        if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) then
          goto 1999
        endif 

c       if (k .eq. 1 ) then
c       do i=2377,2387
c             write(6,*)'raw burst 1:',i,buff0(i,1),buff1(i,1)
c       enddo
c       endif


c       write(6,*)'HEY buff_rep',buff_rep(1,1),buff_rep(8,1),buff_rep(16,1)
c       write(6,*)'chirp_rate', chirp_rate
c       write(6,*)'nr',nr
c       write(6,*)'fd_bip ',fd_bip(k)
c       write(6,*)'nfft_az ',nfft_az
c       write(6,*)'np_proc ',np_proc
c       write(6,*)'ns ',ns(k)
c       write(6,*)'r_1st ',r_1st(k)
c       write(6,*)'dr ',dr(k)
c       write(6,*)'r_bip ',r_bip(k)
c       write(6,*)'coef_fd ',coef_fd(1,k),coef_fd(2,k),coef_fd(3,k),coef_fd(4,k)
c       write(6,*)'prf ',prf(k)
       
        if (mytask.eq.0) then
         if ((k.le.nburst_spec).and.(k.le.burst_end)) then
          call get_spec(k,buff1,ns,np,np_v,buff_spec)
         endif
         if (k.eq.nburst_spec) then
          do i=1,128
           write(6,*)'BUFF_SPEC',buff_spec(i),cabs(buff_spec(i))
          enddo
         endif
        endif

c      if(k.eq.1) then
c          b_x=2*sam_raw
c          b_y=line_raw
c          x_len= ns(k)
c          y_len=np_v(k)
c          eh_file='/spdata/people/jwtun/update19_port/eh_b1.dat'
c          call  out_byte(buff0,x_len,y_len,b_x,b_y,eh_file)
c       end if

 

c       write(6,*)'before rg-cmp:',buff1(1,1),buff1(1024,1),buff1(2048,1)
        call range_comp(
     *  k,nr,fs,chirp_rate,fd_bip,
     *  nfft_az,np_proc,ns,r_1st,
     *  dr,r_bip,coef_fd,prf,
     *  buff1,np_v,buff_rep,ire_sim,
     *  dk_rep,gain_rep,file_rep,istatus,tbl)
        if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) then
          goto 1999
        endif 
c       write(6,*)'after rg-cmp:',buff1(1,1),buff1(1024,1),buff1(2048,1)
D 	write(6,*) 'end of range compression'
C**********************************************
c      if(k.eq.1) then
c          b_x=sam_raw
c          b_y=line_raw
c          x_len= ns(k)
c          y_len=np_v(k)
c          rg_file='/spdata/people/jwtun/update19_port/rg_b1.dat'
c          call  out_cmplx2(buff1,buff_r4_xy,buff_b_xy,x_len,
c    *                   y_len,b_x,b_y,rg_file)
c       end if
C**********************************************

        call corner_turn(
     *  k,nr,ns,np_proc,nfft_az,
     *  buff1,buff2       )
D 	write(6,*) 'end of corner turn'

c       write(6,*)'TEST1',r_bip(k)
c       write(6,*)'TEST2',coef_fd(1,k),coef_fd(2,k)
c       write(6,*)'TEST3',coef_fr(1,k),coef_fr(2,k)
c       write(6,*)'TEST3',buff2(1,1)
        call az_comp(
     *  k,nr,np_proc,nfft_az,ns,
     *  r_1st,dr,r_bip,coef_fd,
     *  coef_fr,prf,buff2  )
c       write(6,*)'after az comp',buff2(1,1)
D 	write(6,*) 'end of azimuth compression'
C**********************************************
c       if(k.eq.1) then
c          b_y=sam_raw
c          b_x=line_raw
c          y_len= ns(k) 
c          x_len=np_v(k)
c          rg_file='/spdata/people/jwtun/update16_port/az_b1.dat'
c          call  out_cmplx2(buff2,buff_r4_yx,buff_b_yx,x_len,
c    *                   y_len,b_x,b_y,rg_file)
c       end if
C**********************************************


        call rdmtrc_comp(
     *  k,nr,ire_sim,nfft_az,pbw_far,
     *  pbw_near,r_high,r_low,v_sc_mag,
     *  ns,dr,r_bip,coef_fd,
     *  coef_lk_ang,look_ang,roll_o,roll,
     *  df,ant_patn_az,beam_id,ant_patn_r,
     *  radio_comp_mode,v_noise,prf,buff2,buff2a,dfddth,
     *  daz,beam_no,coef_inc)
c       write(6,*)'after rdmtrc_comp',buff2a(1,1)
D 	write(6,*) 'end of radiometric compensation'
c       if(k.eq.1) then
c          b_y=sam_raw
c          b_x=np_geo
c          write(6,*)'what is np_geo', np_geo
c          y_len= ns(k)
c          x_len=nfft_az+np_geo/8
c          rg_file='/spdata/people/jwtun/update16_port/rad_b1.dat'
c            call  out_cmplx2(buff2a,buff_r4_rad,buff_b_rad,x_len,
c    *                   y_len,b_x,b_y,rg_file)
c       end if


1212	if(grid_mode.eq.1) then
c	   call grid(buff2a) !generate test pattern
c 	   write(6,*) 'end of grid pattern gen'
	end if

c      write(6,*)'before geo_rectif on burst # ',k
        call geo_rectif(
     *  k,c1size_blk,c2size_blk,c1_cell,
     *  c2_cell,c1_g,c2_g,n_c1_blk,
     *  n_c2_blk,r_g,fd_g,buff2a,
     *  r_1st,dr,r_bip,coef_fd,
     *  nfft_az,df,buff3)
c       write(6,*)'after geo_rectif on burst # ',k
D 	write(6,*) 'end of geometric rectification'
c       if(k.eq.1) then
c         do  b_y=1,np_geo
c         do  b_x=1,sam_raw
c            val = sqrt(buff3(b_x,b_y))
c             ival = val
c             if (ival.gt.255) ival= 255
c             buff_b_geo(b_x,b_y) = ival
c         enddo
c         enddo
c         nbytes = np_geo*sam_raw
c          rg_file='/spdata/people/jwtun/update16_port/geo_b1.dat'
c          ch_fd=ccreate(rg_file)
c          n_bytes = cwrite(ch_fd,buff_b_geo,nbytes,0)
c       end if



c       write(6,*)'i_ang_bip ',i_ang_bip(k)
        call spatial_avg(
     *  k,c1_cell,c2_cell,c1_pxl,
     *  c2_pxl,r_bip,c1_g,c2_g,
     *  n_c2_blk,i_ang_bip,dr,prf,np_proc,buff3,buff4,buff5,c1size_blk       )
c       write(6,*)'after spatial',buff4(1,1),buff5(1,1)
D 	write(6,*) 'end of spatial average'

c	call put_pvs_4b(k,
c    *  prod_type,beam_mode,buff4,file_pvs_beam,
c    *  istatus        )
        if ((prod_type.eq.prod_pvs_3).or.(prod_type.eq.prod_pvs_2)) then
         call put_pvs_4b(k,
     *   prod_type,nbeams,buff4,file_pvs_look,
     *   istatus,burst_pvs,n_single_look,proc_gain)
         if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) then 
          goto 1999
         endif
        endif


c	if(iim_mode.eq.1)
c    *  call test_image(kburst,
c    *  c1_g,c2_g,n_c2_blk,c1_pxl,
c    *  c2_pxl,c1_frm,c2_frm,buff4,buff5)


        call overlay(
     *  k,disk_ptr,sam_ac2,c1_g,
     *  c2_g,n_c2_blk,c1_pxl,c2_pxl,
     *  c1_cell,c2_cell,c1_frm,c2_frm,
     *  ipinpon,buff4,buff5,burst_start,burst_end,
     *  file_image_dump,norm_look_mode,v_filler,l1_ct_pro,
     *  l2_ct_pro,fm_sizec2_in,buff6,buff7,ct_prof_len,ct_profile,istatus,
     *  ipage,ipage_start,file_mask,file_data,variance,ic_var,c1size_blk,
     *  beam_no,nbeams,fm_sizec2,c2_flm)
        if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) then
          goto 1999
        endif
D 	write(6,*) 'end of overlay'
c       write(6,*)'after overlay',buff6(1,1),buff7(1,1)

c       if (mytask.eq.0) then
c        if (k.eq.1) then
c         call put_frame(buff6,buff7,v_filler,sam_ac2)
c         stop
c         igetout=1
c        endif
c       endif
c       call mpi_bcast(igetout,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
c       write(6,*)'what is getout',igetout
c       if (igetout.eq.1) return
c
c	if(k.eq.burst_no_dump) then
c       call put_framelet(stage_dump,
c    *  buff1,buff2a,buff3,buff4,
c    *  buff5,file_framelet,istatus)
c 	   write(6,*) 'end of dumping framelet'
c       if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) goto 1999
c	end if

	else
	   write(6,*)' main proc: bad burst # skip at:',k,burst_good(k)
           if(k.eq.burst_start) then
           call overlay(
     *     k,disk_ptr,sam_ac2,c1_g,
     *     c2_g,n_c2_blk,c1_pxl,c2_pxl,
     *     c1_cell,c2_cell,c1_frm,c2_frm,
     *     ipinpon,buff4,buff5,burst_start,burst_end,
     *     file_image_dump,norm_look_mode,v_filler,l1_ct_pro,
     *     l2_ct_pro,fm_sizec2_in,buff6,buff7,ct_prof_len,ct_profile,istatus,
     *     ipage,ipage_start,file_mask,file_data,variance,ic_var,c1size_blk,
     *     beam_no,nbeams,fm_sizec2,c2_flm)
           if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) then
             goto 1999
           endif
           endif

	endif
1234	continue


	write(6,*) 'end of saving frame image data'
        write(6,*) 'QdN after processing ipage_start=',ipage_start
        tb=rtc()
        write(6,*)'partial process elapsed 2=',tb-ta
c***************** END OF MAIN PROCESSING ********************************
1999    continue
        call mpi_barrier(MPI_COMM_WORLD,ierr)
 
        if (mytask.eq.0) then
          iistatus= istatus
          do i=1,kids-1
            call mpi_recv(istatus,1,MPI_INTEGER,i,0,MPI_COMM_WORLD,isuccess,ierr)
            if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) iistatus=istatus
          enddo
        else
          call mpi_send(istatus,1,MPI_INTEGER,0,0,MPI_COMM_WORLD,ierr)
        endif
        call mpi_bcast(iistatus,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
        istatus=iistatus
        if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
C***************************************************
        variance= variance/ic_var

        if (mytask.eq.0) then
          do i= 1,256
           hist_i_avg(i)    =hist_i(i)
           hist_q_avg(i)    =hist_q(i)
          enddo
          do i=1,kids-1
           call mpi_recv(hist_i,256,MPI_INTEGER,i,1,MPI_COMM_WORLD,isuccess,ierr)
           call mpi_recv(hist_q,256,MPI_INTEGER,i,2,MPI_COMM_WORLD,isuccess,ierr)
           do ii= 1,256
           hist_i_avg(ii)    =hist_i(ii)+ hist_i_avg(ii)
           hist_q_avg(ii)    =hist_q(ii)+ hist_q_avg(ii)
           enddo
          enddo
        else
           call mpi_send(hist_i,256,MPI_INTEGER,0,1,MPI_COMM_WORLD,ierr)
           call mpi_send(hist_q,256,MPI_INTEGER,0,2,MPI_COMM_WORLD,ierr)
        endif

        call mpi_bcast(hist_i_avg,256,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
        call mpi_bcast(hist_q_avg,256,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
        call mpi_bcast(buff_spec,8192,MPI_COMPLEX,0,MPI_COMM_WORLD,ierr)

        


	return
	end


       subroutine clear_frame(
     *  sam_ac2,buff6,buff7,ct_profile )
C*****WAYNE******
       include 'ssp2_const.inc'
       integer*4     sam_ac2 
       real*8     buff6(sam_ac,sam_ac1_2page)
       byte     buff7(sam_ac,sam_ac1_2page)
       real*8     ct_profile(sam_ac)
C*****WAYNE******
c	include 'key_const.inc'
c	include 'key_pp.inc'

	do i = 1, sam_ac2
	do j = 1, sam_ac1_2page
	buff6(i,j) = 0.
	buff7(i,j) = 0
	end do
	end do

	do i = 1, sam_ac/2
	ct_profile(i) = 0
	end do

	return
	end





c*******************************************************************
c***********************************************************************
c	Jet Propulsion Laboratory
c	Section:	334, Radar Science and Engineering
c	Project:	RadarSAT
c	Task:		ScanSAR Data Processor
c	Code:           Fortran 77, Unix DEC 5000/133
c-----------------------------------------------------------------------
c	Module Name:	radio_comp.f
c
c		Perform the Radiometrci Compensation
c		for the RadarSAT ScanSAR Data.
c
c	Input:	1). 
c
c	Output: 
c
c	Function:    
c		The azimuth processing include are
c
c		1) Azimuth Compreesion:
c		   The deramp-FFT method is chosen because of its 
c		   computional efficiency.
c
c	*** Algorithms designed and original coded by: Mike Jin ***
c----------------------------------------------------------------------------
c			   for RadarSAT ScanSAR
c	07/12/94 Michael JIn Initial code.
c****************************************************************************
        subroutine rdmtrc_comp(
     *  kburst,nr,ire_sim,nfft_az,pbw_far,
     *  pbw_near,r_high,r_low,v_sc_mag,
     *  ns,dr,r_bip,coef_fd,
     *  coef_lk_ang,look_ang,roll_o,roll,
     *  df,ant_patn_az,beam_id,ant_patn_r,
     *  radio_comp_mode,v_noise,prf,buff_in,buff_out,dfddth,
     *  daz,beam_no,coef_inc)
	implicit none
C*****WAYNE******
       include 'ssp2_const.inc'
       integer     ire_sim
       integer     kburst
       integer*4   nr
       integer*4     nfft_az
       real*8     pbw_far(burst_max)
       real*8     pbw_near(burst_max)
       real*8     r_high(burst_max)
       real*8     r_low(burst_max)
       real*8     v_sc_mag(burst_max)
       integer*4     ns(burst_max)
       real*8     dr(burst_max)
       real*8     r_bip(burst_max)
       real*8     coef_fd(4,burst_max)
       real*8     coef_lk_ang(4,burst_max)
       real*8     look_ang(burst_max)
       real*8     roll_o(burst_max)
       real*8     roll(burst_max)
       real*8     df(burst_max)
       real     ant_patn_az(ns_ant_az)
       integer*4     beam_id(burst_max)
       real     ant_patn_r(ns_ant_rg,7)
       integer     radio_comp_mode
       integer*4     v_noise
       complex     buff_in(line_raw,sam_raw)
       complex     buff_out(np_geo,sam_raw)
       real*8         prf(burst_max)
       real*8 coef_inc(4,burst_max)
       real daz(ns_ant_rg,6)
       integer*4       beam_no(burst_max)
C*****WAYNE******
c	include 'key_const.inc'
c	include 'key_pp.inc'

c	---------------------
c	PASSING IN PARAMETERS
c	---------------------
c	complex buff_in(line_raw,sam_raw)	!Input buffer,range compressed data
c	equivalence (buff_in,buff2)
c	integer kburst			!burst index

c	-----------------
c	OUTPUT PARAMETERS
c	-----------------
c	complex buff_out(np_geo,sam_raw)	!Input buffer,range compressed data
c	equivalence (buff_out,buff2a)

c	-------------------
c	LOCAL VARIABLES
c	-------------------
	integer i,j,jj,n_cell,m,n1,n2,k1,k2
        integer k1_c(sam_raw)
        real*8  r_c(sam_raw)
        real*8  sc_inc(sam_raw)
        real*8  dfddth

	real c_scale,comp_factor,pbw_curr,pbw_slope,del_f
	real*8 r,f,fdc,vlook_ang,hori_ang,ant_gain,dh_ang,ainc

   	c_scale = 1. 				!when use E. Chu's FFT
cSIM data    c_scale = 120./nfft_az
c  	c_scale = 120./(nfft_az*2048*2048)  				!when use E. Chu's FFT
clooking for targ   	c_scale = 120./(2048.*2048.*nfft_az)		!when use SGI FFT
        c_scale = c_scale*1500./sqrt(prf(kburst))
	m = 4

c$doacross local(j,i)
	do j = 1, sam_raw
 	do i = 1, np_geo
c	do i = 1+np_geo/8, nfft_az+np_geo/8
	buff_out(i,j) = cmplx(0.,0.)
	end do
	end do

	pbw_slope = (pbw_far(kburst)-pbw_near(kburst))/
     *		    (r_high(kburst)-r_low(kburst)) 
	n_cell = 20

C* extract

	do i = 1, ns(kburst)-nr+1
	if(i-n_cell*int((i-1)/n_cell).eq.1) then
	  r = r_low(kburst)+(i-1+n_cell/2)*dr(kburst)
 	  call v_poly(r,r_bip(kburst),coef_inc(1,kburst),m,ainc)
	  call v_poly(r,r_bip(kburst),coef_lk_ang(1,kburst),m,vlook_ang)

	  if(ire_sim.eq.0) then
	    k1 = nint((vlook_ang-look_ang(kburst))/dang_rg)+ns_ant_rg/2+1		
	  else
ccc !AC	    k1 = nint((vlook_ang-roll_o(kburst)-roll(kburst)-ele_ang_ref)
            k1 = nint((vlook_ang-look_ang_cnt-ele_ang_ref)
     1		 /dang_rg)+1
	  end if
	end if
           k1_c(i)= k1
           sc_inc(i)= sqrt(sind(ainc))
           r_c(i) = r
        end do


C* extract

c$doacross local(j,i,pbw_curr,n1,n2,hori_ang,
c$&              dh_ang,k2,ant_gain,comp_factor)

	do i = 1, ns(kburst)-nr+1

	pbw_curr = pbw_near(kburst)+(r_c(i)-r_low(kburst))*pbw_slope 
	n1 = 1+(1.-pbw_curr/prf(kburst))/2*nfft_az
	n2 = 1+(1.+pbw_curr/prf(kburst))/2*nfft_az

c	hori_ang = (n1-nfft_az/2-1)*df(kburst)/dfddth-
c    1                  daz(k1_c(i),beam_no(kburst))
 	hori_ang = (n1-nfft_az/2-1)*df(kburst)/dfddth
	dh_ang = df(kburst)/dfddth

c       write(6,*)'WWW',df(kburst),dfddth,dh_ang,hori_ang


	do j = n1, n2
	hori_ang = hori_ang + dh_ang
c       write(6,*)'NEW ANT VAR ',hori_ang,dh_ang
c       write(6,*)'NEW ANT VAR ',i,j
	k2 = nint(0.9*abs(hori_ang)/dang_az)+1
	ant_gain = ant_patn_r(k1_c(i),beam_id(kburst))*ant_patn_az(k2)

	if(radio_comp_mode.eq.1) then     !(1=normal, 2=no ant,3=no sr,4=none)
        comp_factor = sc_inc(i)*(r_c(i)/r_bip(1))**1.5/ant_gain
        else
	if(radio_comp_mode.eq.2) then  	
        comp_factor = sc_inc(i)*(r_c(i)/r_bip(1))**1.5  
        else
	if(radio_comp_mode.eq.3) then
        comp_factor = sc_inc(i)*1./ant_gain
	else
        comp_factor = sc_inc(i)*1.
	end if	
	end if	
	end if	
c        write(6,*)'ANT VAR ',ant_patn_r(k1_c(i),beam_id(kburst)),beam_id(kburst)
c        write(6,*)'ANT VAR ',ant_patn_az(k2),k2,k1_c(i)
c        write(6,*)'ANT VAR ',k2,hori_ang,dang_az
c        write(6,*)'COMP_FACTOR',comp_factor

        if (ire_sim .eq. 1 ) then
           buff_out(j+np_geo/8,i+np_geo/8)= buff_in(j,i)*
     *          comp_factor*c_scale
        else
 	   buff_out(j+np_geo/8,i+np_geo/8)= buff_in(j,i)*comp_factor
     *			*c_scale + cmplx(v_noise,0.)		!for testing
     *			*0. + cmplx(v_noise*r_low(1)/r_c(i),0.)	!for testing
        endif

	
  	end do
	
  	end do	

	return
	end
c********************  end of radiometric compensation **************************

c*******************************************************************
c	Jet Propulsion Laboratory
c	Section:	334, Radar Sience and Engineering
c	Project:	RadarSAT
c	Task:		ScanSAR Data Processing Subsystem.
c	Code:		Fortran-77, Unix DEC 5000/133
c-------------------------------------------------------------------
c	Module: geo_rectif.f	
c	
c		Perform a geometric rectification function resampling 
c		a range doppler image into c1/c2 projection domain.
c
c	Input:	1). A burst range-doppler image data 
c		2). A associated processing parameters file.
c
c	Ouput: 	A single look image in the C1/C2 projection.
c
c	Function:
c		Pixels in the raw image framelet are indexed by
c		slant range and Doppler frequency. In order to get
c		projection image, three functions, framelet
c		truncation, c1 (or Doppler) resampling, and c2 (or range)
c		resampling are required to perform.
c		1). Framelet truncation:
c	            The framelet truncation shall retain all pixels whose
c		    centers lie on or within the perimeter defined by this 
c		    framelet and set pixels value to be zero for pixels 
c		    outside the perimeter.
c		2). c1 (or Doppler) Resampling:
c		    Perform a 1-D four-point Doppler interpolation to obtain 
c		    pixels along iso-range lines.  
c		3). c2 (or range) Resampling:
c		    Perform a 1-D four-point range interpolation to obtain 
c		    pixels along is-c1 lines.  
c
c	Algorithms designed by Mike Jin.
c-------------------------------------------------------------------
c
c	06/30/94 M. Jin Modified for resampling in iso-r & iso-c1 lines
c	01/10/93 Anhua Chu Modified for RadarSAT ScanSAR
c	12/21/92 Anhua Chu Initial code.
c*******************************************************************


       subroutine geo_rectif(
     *  kburst,c1size_blk,c2size_blk,c1_cell,
     *  c2_cell,c1_g,c2_g,n_c1_blk,
     *  n_c2_blk,r_g,fd_g,buff2a,
     *  r_1st,dr,r_bip,coef_fd,
     *  nfft_az,df,flm_geo)
	implicit none	

C*****WAYNE******
       include 'ssp2_const.inc'
       integer     kburst
       real*8     c1size_blk(burst_max)
       real*8     c2size_blk(burst_max)
       real*8     c1_cell(burst_max)
       real*8     c2_cell(burst_max)
       real*8     c1_g(3,3,n_az,n_rg,burst_max)
       real*8     c2_g(3,3,n_az,n_rg,burst_max)
       integer*4     n_c1_blk(burst_max)
       integer*4     n_c2_blk(burst_max)
       real*8     r_g(3,3,n_az,n_rg,burst_max)
       real*8     fd_g(3,3,n_az,n_rg,burst_max)
       real     flm_geo(sam_raw,np_geo)
       real*4     W1(16+1)
       real*4     W2(16+2)
       real*4     W3(16+3)
       real*4     W4(16+4)
       complex     buff2a(np_geo,sam_raw)
       real*8     r_1st(burst_max)
       real*8     dr(burst_max)
       real*8     r_bip(burst_max)
       real*8     coef_fd(4,burst_max)
       integer*4     nfft_az
       real*8     df(burst_max)
C*****WAYNE******
c	include 'key_const.inc'
c	include 'key_pp.inc'
	
c	------------------
c	PASSING IN PARAMETERS	
c	------------------
c	integer kburst

c	------------------
c	PASSING OUT DATA
c	------------------
c	real flm_geo(sam_raw,np_geo)
c	equivalence (flm_geo,buff3)

c	------------------
c	LOCAL VARIABLES
c	------------------
	complex data(513,513)
	real*8  r_t(3,3),fd_t(3,3),c1_t(3,3),c2_t(3,3)
	real*8  c1min_flm,c2min_flm,c1max_flm
	integer i,j,k1,k2,k1_off,k2_off,c1_len,c2_len
 	real*4	sum,dp,sinc
c	real*4	W1(16),W2(16),W3(16),W4(16),sum,dp,sinc
c	common/c_weight/w1,w2,w3,w4

c	------------
c	RESAMPLING COEFFICIENTS
c	------------

	do i = 1, 16
	dp = (i-1)/16.
c	w1(i) = sinc(dp+1.,pi)
c	w2(i) = sinc(dp,pi)
c	w3(i) = sinc(1.-dp,pi)
c	w4(i) = sinc(2.-dp,pi)
c	sum = w1(i)+w2(i)+w3(i)+w4(i)
c	w1(i)=w1(i)/sum
c	w2(i)=w2(i)/sum
c	w3(i)=w3(i)/sum
c	w4(i)=w4(i)/sum
           W1(I)=-(1.+dp)**3 + 5.*(1.+dp)**2 - 8.*(1.+dp) +4.
           W2(I)=      dp**3 -      2.*dp**2               +1.
           W3(I)= (1.-dp)**3 - 2.*(1.-dp)**2               +1.
           W4(I)=-(2.-dp)**3 + 5.*(2.-dp)**2 - 8.*(2.-dp) +4.
	end do

c	------------
c	CLEAR BUFFER
c	------------

c$doacross local(i,j)
	do i = 1, sam_raw
	do j = 1, np_geo
	flm_geo(i,j) = cmplx(0.,0.)
	end do
	end do

	c1_len = nint(c1size_blk(kburst)/c1_cell(kburst))
	c2_len = nint(c2size_blk(kburst)/c2_cell(kburst))
c	write(6,*) 'c12len', c1_len,c2_len
c	write(6,*) 'c12cell', c1_cell(kburst),c2_cell(kburst)
c	write(6,*) 'rfell', dr(kburst),df(kburst)

	c1min_flm = min(c1_g(1,1,1,1,kburst),     
     1			c1_g(1,1,1,n_c2_blk(kburst),kburst))
        c1max_flm = max(c1_g(3,1,1,1,kburst),
     1                  c1_g(3,1,1,n_c2_blk(kburst),kburst))
	c2min_flm = c2_g(1,1,1,1,kburst)
c	------------
c	RESAMPLING PROCESSING
c	------------
	do i=1,n_c1_blk(kburst)
	do j=1,n_c2_blk(kburst)

c	   ------------
c	   PICKUP 3x3 GROUND REFENCE CELLS,EACH TIME
c	   ------------
	   do k1=1,3
	   do k2=1,3

	      r_t(k1,k2)  = r_g(k1,k2,i,j,kburst)
	      fd_t(k1,k2) = fd_g(k1,k2,i,j,kburst)
	      c2_t(k1,k2) = c2_g(k1,k2,i,j,kburst)
	      c1_t(k1,k2) = c1_g(k1,k2,i,j,kburst)
c	   write(6,*) 'i,j,k1,k2', i,j,k1,k2
c	   write(6,*) 'c1,c2', c1_t(k1,k2),c2_t(k1,k2)
c	   write(6,*) 'r,fd', r_t(k1,k2),fd_t(k1,k2)
           enddo
	   enddo

           call resampling(r_t,fd_t,c1_t,c2_t,
     *                     data,kburst,c1_len,c2_len,
     *                     buff2a,r_1st,dr,r_bip,
     *                     coef_fd,c1_cell,nfft_az,df,
     *                     c2_cell,W1,W2,W3,W4)

	k1_off = nint((c1_t(1,1)-c1min_flm)/c1_cell(kburst))+np_geo/16
	k2_off = nint((c2_t(1,1)-c2min_flm)/c2_cell(kburst))+np_geo/16
c	write(6,*) 'k12_off', k1_off,k2_off
c	write(6,*) 'c12', c1_t(1,1),c2_t(1,1)
c	   ------------
c	   OUTPUT TO FRAME BUFFER
c	   ------------
c$doacross local(k1,k2)
	   do k1=1,c1_len
	   do k2=1,c2_len
	   flm_geo(k2+k2_off,k1+k1_off) = real(data(k2,k1))
           enddo
           enddo

	enddo
	enddo
	
	return
	end


c***************************************************************
c	SUBROUTINES
c***************************************************************
	subroutine resampling(r_t,fd_t,c1_t,c2_t,
     *	                      data,kburst,c1_len,c2_len,
     *  		      buff_in,r_1st,dr,r_bip,
     *   		      coef_fd,c1_cell,nfft_az,df,
     *  		      c2_cell,W1,W2,W3,W4)
	implicit none	

c	include 'key_const.inc'
c	include 'key_pp.inc'
C*****WAYNE******
       include 'ssp2_const.inc'
       complex     buff_in(np_geo,sam_raw)
       real*8     r_1st(burst_max)
       real*8     dr(burst_max)
       real*8     r_bip(burst_max)
       real*8     coef_fd(4,burst_max)
       real*8     c1_cell(burst_max)
       integer*4     nfft_az
       real*8     df(burst_max)
       real*8     c2_cell(burst_max)
       real*4     W1(16+1)
       real*4     W2(16+2)
       real*4     W3(16+3)
       real*4     W4(16+4)
C*****WAYNE******
c	------------------
c	PASSING IN PARAMETERS	
c	------------------
	real*8  r_t(3,3),fd_t(3,3),c1_t(3,3),c2_t(3,3)
c 	complex	buff_in(np_geo,sam_raw)	 !input buffer 68=64+4pt
c	equivalence (buff_in,buff2a)
	integer kburst

c	real*4	W1(16),W2(16),W3(16),W4(16)
c	common/c_weight/w1,w2,w3,w4

c	------------------
c	PASSING OUT PARAMTERS
c	------------------
	complex  data(513,513)	!output data buffer
	complex  temp

c	------------------
c	LOCAL VARIABLES
c	------------------
	complex array(512)
	real*8	mu1(3,3),mu2(3,3)
	real*8  r_min,r_max,sr,fdc
	real 	e_coef(3)
	real    dsr,dc1,dc2,restm,festm
	real    r_indx,f_indx,f_indx0
	integer k,i,il,ip,idp,j,k1,k2,c1_len,c2_len,isize,m

	m=4
c	------------------------
c	COMPUTE 9-COEF. FOR AZ & RANGE RESAMPLING
c	------------------------
	call rsmplcf(c1_t,c2_t,r_t,fd_t,mu1,mu2)

	r_min = min(r_t(1,1),r_t(1,3),r_t(3,1),r_t(3,3))-4*dr(kburst) !AC
	r_max = max(r_t(1,1),r_t(1,3),r_t(3,1),r_t(3,3))+4*dr(kburst) !AC
	r_min = int((r_min-r_1st(kburst))/dr(kburst))*dr(kburst)+
     *			r_1st(kburst)
	r_max = int((r_max-r_1st(kburst))/dr(kburst)+1)*dr(kburst)
     *			+r_1st(kburst)

c	------------
c	CLEAR BUFFER
c	------------
	isize = max(c1_len,c2_len,nint((r_max-r_min)/dr(kburst))+4)
	isize = isize + 2

c$doacross local(i,j)
	do i = 1, isize
	do j = 1, isize
	data(i,j) = cmplx(0.,0.)
	end do
	end do

c	------------------------
c	INTERPOLATION PROCESSING
c	------------------------
	do i = 1, nint((r_max-r_min)/dr(kburst))+4

	sr = (i-3)*dr(kburst) + r_min	
	dsr = sr - r_t(2,2)
	call v_poly(sr,r_bip(kburst),coef_fd(1,kburst),m,fdc)
	il = nint((sr-r_1st(kburst))/dr(kburst)) + 1+np_geo/8

c	  ----------------
C	  DOPPLER RESAMPLING 
c	  ----------------

	  do k=1,3
             e_coef(k)=(mu1(k,3)*dsr+mu1(k,2))*dsr+mu1(k,1)
	  enddo

	     dc1 = (1-2)*c1_cell(kburst) + c1_t(1,1) - c1_t(2,2)
             f_indx0 = np_geo/8+nfft_az/2+1
cAC 10/30/95         f_indx0 = -fdc/df(kburst)+np_geo/8+nfft_az/2+1
	  do j = 1, c1_len

	     dc1 = dc1 + c1_cell(kburst) 

	     festm=(e_coef(3)*dc1+e_coef(2))*dc1+e_coef(1)+fd_t(2,2)
             f_indx = (festm-fdc)/df(kburst)+f_indx0
cAC 10/30/95         f_indx = festm/df(kburst)+f_indx0
	       ip=int(f_indx)
	       idp=int((f_indx-ip)*16)+1
               if(buff_in(ip-1,il).eq.0..or.buff_in(ip,il).eq.0.
     *	       .or.buff_in(ip+1,il).eq.0..or.buff_in(ip+2,il).eq.0.) then
	       array(j)= cmplx(0.,0.)
	       else
	       data(j,i) = buff_in(ip-1,il)*w1(idp)
     *	                  + buff_in(ip,il)*w2(idp)
     *	                  + buff_in(ip+1,il)*w3(idp)
     *	                  + buff_in(ip+2,il)*w4(idp)
	       end if
	 end do 
	 end do 

c	  -------------------------
C	  END OF DOPPLER RESAMPLING 
c	  -------------------------

c	  -----------------
C	  CORNER TURNING
c	  -----------------

c	  call c_turn(data,isize)
        do i = 2, isize
        do j = 1, i-1
        temp = data(j,i)
        data(j,i) = data(i,j)
        data(i,j) = temp
        end do
        end do


c	  ---------------------
C	  END OF CORNER TUENING
c	  ---------------------

c	  ----------------
C	  RANGE RESAMPLING 
c	  ----------------

	  do i = 1, c1_len

	     dc1 = (i-1)*c1_cell(kburst) + c1_t(1,1) - c1_t(2,2)

	  do k=1,3
             e_coef(k)=(mu2(k,3)*dc1+mu2(k,2))*dc1+mu2(k,1)
	  enddo

	     dc2 = (1-2)*c2_cell(kburst) + c2_t(1,1) - c2_t(2,2)
	  do j = 1, c2_len

	     dc2 = dc2 + c2_cell(kburst) 
  
	     restm=(e_coef(3)*dc2+e_coef(2))*dc2+e_coef(1)+r_t(2,2)

             r_indx=(restm - r_min)/dr(kburst) + 3   !extra 2 is for 4pt interp 

	       ip=int(r_indx)
	       idp=int((r_indx-ip)*16)+1
               if(data(ip-1,i).eq.0..or.data(ip,i).eq.0.
     *	          .or.data(ip+1,i).eq.0..or.data(ip+2,i).eq.0.) then
	       array(j)= cmplx(0.,0.)
	       else
	       array(j)= data(ip-1,i)*w1(idp) 
     *                 + data(ip,i)*w2(idp) 
     *                 + data(ip+1,i)*w3(idp)
     *                 + data(ip+2,i)*w4(idp)
	       end if
	  end do
	  
c$doacross local(j)
	  do j = 1, c2_len
               data(j,i) = array(j)*conjg(array(j))
          end do	
  
          end do	

c	  ------------------------
C	  END OF RANGE RESAMPLING 
c	  ------------------------

	return
	end


	subroutine rsmplcf(c1in,c2in,rin,fin,mu1,mu2)
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
c$doacross local(k1,k2)
	do k1 = 1, 3
	do k2 = 1, 3
	a(k2+k1*3-3,1) = 1
	a(k2+k1*3-3,2) = r(k1,k2)
	a(k2+k1*3-3,3) = r(k1,k2)**2
	a(k2+k1*3-3,4) = c1(k1,k2)
	a(k2+k1*3-3,5) = c1(k1,k2)*r(k1,k2)
	a(k2+k1*3-3,6) = c1(k1,k2)*r(k1,k2)**2
	a(k2+k1*3-3,7) = c1(k1,k2)**2
	a(k2+k1*3-3,8) = c1(k1,k2)**2*r(k1,k2)
	a(k2+k1*3-3,9) = c1(k1,k2)**2*r(k1,k2)**2
	a(k2+k1*3-3,10) = f(k1,k2)
	end do
	end do
	call mats(a,x,n)
	do k1 = 1, 3
	do k2 = 1, 3
	mu1(k1,k2) = x(k2+k1*3-3)
	end do
	end do

c$doacross local(k1,k2)
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
	mu2(k1,k2) = x(k2+k1*3-3)
	end do
	end do

	return
	end


        subroutine c_turn(data,isize)

        complex data(513,513),temp
        integer isize

        do i = 2, isize
        do j = 1, i-1
        temp = data(j,i)
        data(j,i) = data(i,j)
        data(i,j) = temp
        end do
        end do

        return
        end


c**************************************************************
       subroutine spatial_avg(
     *  kburst,c1_cell,c2_cell,c1_pxl,
     *  c2_pxl,r_bip,c1_g,c2_g,
     *  n_c2_blk,i_ang_bip,dr,prf,np_proc,buff_in,buff_out,mask_out,c1size_blk)
C*****WAYNE******
       include 'ssp2_const.inc'
       real*8     c1size_blk(burst_max)
       integer     kburst
       integer    np_proc
       real*8     c1_cell(burst_max)
       real*8     c2_cell(burst_max)
       real*8     c1_pxl
       real*8     c2_pxl
       real*8     r_bip(burst_max)
       real*8     c1_g(3,3,n_az,n_rg,burst_max)
       real*8     c2_g(3,3,n_az,n_rg,burst_max)
       integer*4     n_c2_blk(burst_max)
       real     buff_in(sam_raw,np_geo)
       real     buff_out(sam_post,np_geo)
       byte     mask_out(sam_post,np_geo)
       real     w_factor(16,64,8,4)
       real*8   prf(burst_max)
       real*8          dr(burst_max)
       real*8         i_ang_bip(burst_max)
C*****WAYNE******
c	include 'key_const.inc'
c	include 'key_pp.inc'
c	real buff_in(sam_raw,np_geo)
c	real buff_out(sam_post,np_geo)
c	byte mask_out(sam_post,np_geo)
c	equivalence (buff_in,buff3)
c	equivalence (buff_out,buff4)
c	equivalence (mask_out,buff5)
	real temp,wght,d1,d2
	real*8 c1,c2,aic1,aic2,c1_ref_flm,c2_ref_flm
	real rvalue,amalue,c1_res,c2_res
	integer ic1,ic2,d10,d20

c	The following four lines changed on 12/13/94, bug

	prr1 = 1.5*c1_pxl/c1_cell(kburst) 	!pixel reduction ratio in c1
	prr2 = 1.5*c2_pxl/c2_cell(kburst) 	!pixel reduction ratio in c2

	m1 = 3 * prr1				!fileter box size in azimuth
	m2 = 3 * prr2				!fileter box size in range

c	write(6,*) 'm12', m1,m2,c1_pxl,c2_pxl,c1_cell(kburst),c2_cell(kburst)
c       write(6,*)'before post_filter on burst # ',kburst
	call post_filter(c1_pxl,c2_pxl,dr(kburst),prf(kburst),
     1	np_proc,lambda,i_ang_bip(kburst),nfltr_c1,nfltr_c2,
     1	c1_cell(kburst),c2_cell(kburst),r_bip(kburst),w_factor)
     	nfltr1 = nfltr_c1/2
     	nfltr2 = nfltr_c2/2
c       write(6,*)'out post_filter on burst # ',kburst

	c1min_flm = min(c1_g(1,1,1,1,kburst),     
     1			c1_g(1,1,1,n_c2_blk(kburst),kburst))
        c1max_flm = max(c1_g(3,1,1,1,kburst),
     1                  c1_g(3,1,1,n_c2_blk(kburst),kburst))
	c2min_flm = c2_g(1,1,1,1,kburst)
        c2max_flm = c2_g(1,3,1,n_c2_blk(kburst),kburst)


	c1_ref_flm = nint(c1min_flm/c1_pxl)*c1_pxl
	c2_ref_flm = int(c2min_flm/c2_pxl)*c2_pxl

ccc !AC n1 = min(np_geo,int(np_geo*c1_cell(kburst)/c1_pxl+1))
        n1 = (c1max_flm-c1min_flm)/c1_pxl+5
        if(n1.gt.np_geo) write(6,*) '!!!!!  !!! np_geo must be increased !!!!!!! !!!!'
        n2 = (c2max_flm-c2min_flm)/c2_pxl+5

c 	write(6,*) 'n12', n1,n2

c$doacross local(k1,k2,c1,c2,aic1,aic2,ic1,ic2,i,j,d10,d20
c$& ),mp_schedtype=interleave

        do k1 = 1, n1           !AC
	c1 = (k1-1)*c1_pxl + c1_ref_flm
	aic1 = (c1 - c1min_flm)/c1_cell(kburst)+1+np_geo/16 !AC
	ic1 = int(aic1)

        do k2 = 1, n2           !AC
	buff_out(k2,k1) = 0.
	mask_out(k2,k1) = 0
	
	c2 = (k2-1)*c2_pxl + c2_ref_flm
	aic2 = (c2 - c2min_flm)/c2_cell(kburst)+1+np_geo/16 !AC
	ic2 = int(aic2)

c         write(6,*) ic1,ic2,m1,m2
c         if (((ic2+m2/2).gt.sam_raw).or.((ic1+m1/2).gt.np_geo).or.
c    *        ((ic2-m2/2).le.0      ).or.((ic1-m1/2).le.0     )) go to 1111
	  if(abs(buff_in(ic2-m2/2,ic1-m1/2)).eq.0.) go to 1111
	  if(abs(buff_in(ic2-m2/2,ic1+m1/2)).eq.0.) go to 1111
	  if(abs(buff_in(ic2+m2/2,ic1-m1/2)).eq.0.) go to 1111
	  if(abs(buff_in(ic2+m2/2,ic1+m1/2)).eq.0.) go to 1111

	  d10 =int((aic1-ic1)*8)+1
	  d20 =int((aic2-ic2)*4)+1
c         write(6,*) d10,d20

	  do i = -m1/2, m1/2
	  do j = -m2/2, m2/2
c         if (((ic2+j).gt.sam_raw).or.((ic1+i).gt.np_geo).or.
c    *        ((ic2+j).le.0      ).or.((ic1+i).le.0     )) go to 1110
	  buff_out(k2,k1) = buff_out(k2,k1)+
c    *	    buff_in(ic2+j,ic1+i)*wght(i+nfltr1,j+nfltr2,d10,d20)
     *	    buff_in(ic2+j,ic1+i)*w_factor(i+nfltr1,j+nfltr2,d10,d20)
1110      continue
	  end do
	  end do
	  buff_out(k2,k1) = sqrt(buff_out(k2,k1))
	  mask_out(k2,k1) = 1

1111	continue
	end do
	end do
c******* 8/3/95 add the following lines ************    !AC
          do k2 = 1, sam_post
            do k1 = n1+1, np_geo
              buff_out(k2,k1) = 0.
              mask_out(k2,k1) = 0
            end do
            end do
 
           do k2 = n2+1, sam_post
            do k1 = 1, np_geo
              buff_out(k2,k1) = 0.
              mask_out(k2,k1) = 0
            end do
            end do
c******* 8/3/95 add the above lines ************


	return
	end


c******************************************************************
	subroutine post_filter(c1_pxl,c2_pxl,dr,prf,np,lambda,i_ang_bip,
     1		nfltr_c1,nfltr_c2,c1_cell,c2_cell,r_bip,w_factor)
C*****WAYNE******
       real     w_factor(16,64,8,4)
C*****WAYNE******
	complex buff(512)
c	real w_factor(16,64,8,4),factor
c	common /avg_fltr/w_factor
	real*8 c1_pxl,c2_pxl,dr,prf,lambda,c1_cell,c2_cell,r_bip,i_ang_bip

	daz = (prf/np)*(r_bip*lambda/(2*7500))  !Vs = 7440
	c2_res_pre = 0.88 * dr /sind(i_ang_bip)
	c1_res_pre = 0.88 * 1.227 * daz

	c1_res_post = c1_pxl * 1.4
	c2_res_post = c2_pxl * 1.4

	c1_ratio = c1_res_post/c1_res_pre
	c2_ratio = c2_res_post/c2_res_pre

c	write(6,*) 'pre c12 res = ', c1_res_pre,c2_res_pre

	c1_3db_fltr =(.74+1.008*(c1_ratio-1.047)-
     1		     .61*exp(-(c1_ratio-1.047)/.14))*c1_res_pre*0.87
	c2_3db_fltr =(.74+1.008*(c2_ratio-1.047)-
     1		     .61*exp(-(c2_ratio-1.047)/.14))*c2_res_pre*0.87


        if(c1_3db_fltr.lt.c1_pxl) c1_3db_fltr = c1_pxl
        if(c2_3db_fltr.lt.c2_pxl) c2_3db_fltr = c2_pxl
c	write(6,*) 'c12-3db-fltr', c1_3db_fltr,c2_3db_fltr

	alpha = 3.4
	nt = 512
	call kaiser(buff,nt,alpha)    !at alfa=3.4, peak-to-3db = 64 bins
c	write(6,*) 'kaiser at 64', buff(64), buff(255)

	np_c1 = 8
	np_c2 = 4
	nfltr_c1 = 16
	nfltr_c2 = 64

	do k1 = 1, np_c1
	do k2 = 1, np_c2
	
	  do i0 = 1, nfltr_c1
	  i = i0 - nfltr_c1/2
	  d1 = -(k1-1)/8.*c1_cell+i*c1_cell
	  ip1 = abs(d1/(c1_3db_fltr/2.)*64)+1
	 
	  do j0 = 1, nfltr_c2 
	  j = j0 - nfltr_c2/2
	  d2 = -(k2-1)/4.*c2_cell+j*c2_cell
	  ip2 = abs(d2/(c2_3db_fltr/2.)*64)+1
	  if(ip1.gt.256.or.ip2.gt.256) then
	  w_factor(i0,j0,k1,k2) = 0.
	  else
	  w_factor(i0,j0,k1,k2) = (real(buff(ip1))*real(buff(ip2)))**2
	  end if
c	  write(6,*) k1,k2,i0,j0,w_factor(i0,j0,k1,k2)
	  end do
	  end do
	
	end do
	end do

	do k1 = 1, np_c1
	do k2 = 1, np_c2
	sum = 0.	
	  do i0 = 1, nfltr_c1
	  do j0 = 1, nfltr_c2
	  sum = sum + w_factor(i0,j0,k1,k2)
	  end do
	  end do
          if(sum.eq.0) go to 1011
	  do i0 = 1, nfltr_c1
	  do j0 = 1, nfltr_c2
	  w_factor(i0,j0,k1,k2) = w_factor(i0,j0,k1,k2)/sum
	  end do
	  end do
1011    continue
	end do
	end do
	
	return
	end

c*******************************************************************
 	function wght(i,j,k1,k2)
 	real wght
 	integer i,j,k1,k2
 	real w_factor(16,64,8,4)
 	common /avg_fltr/w_factor

 	wght = w_factor(i,j,k1,k2)

 	return
 	end


c**************************************************************
       subroutine overlay(
     *  kburst,disk_ptr,sam_ac2,c1_g,
     *  c2_g,n_c2_blk,c1_pxl,c2_pxl,
     *  c1_cell,c2_cell,c1_frm,c2_frm,
     *  ipinpon,buff4,buff5,burst_start,burst_end,
     *  file_image_dump,norm_look_mode,v_filler,l1_ct_pro,
     *  l2_ct_pro,fm_sizec2_in,buff6,buff7,ct_prof_len,ct_profile,istatus,
     *  ipage,ipage_start,file_mask,file_data,variance,ic_var,c1size_blk,
     *  beam_no,nbeams,fm_sizec2,c2_flm)
C*****WAYNE******
       include 'ssp2_const.inc'
       real*8     fm_sizec2
       integer*4     beam_no(burst_max)
       integer    nbeams
       real*8     c1size_blk(burst_max)
       integer     kburst
       integer     disk_ptr
       integer*4     sam_ac2
       real*8     c1_g(3,3,n_az,n_rg,burst_max)
       real*8     c2_g(3,3,n_az,n_rg,burst_max)
       integer*4     n_c2_blk(burst_max)
       real*8     c1_pxl
       real*8     c2_pxl
       real*8     c1_cell(burst_max)
       real*8     c2_cell(burst_max)
       real*8     c2_flm(2,2,burst_max)
       real*8     c1_frm(2,2)
       real*8     c2_frm(2,2)
       integer     ipinpon
       real     buff4(sam_post,np_geo)
       byte     buff5(sam_post,np_geo)
       integer*4     burst_end
       integer*4     burst_start
       real*8     buff6(sam_ac,sam_ac1_2page)
       byte     buff7(sam_ac,sam_ac1_2page)
       character*60     file_image_dump,file_mask,file_data
       integer     norm_look_mode
       integer*4     v_filler
       integer     l1_ct_pro
       integer     l2_ct_pro
       real*8     fm_sizec2_in
       real*8     ct_profile(sam_ac)
       integer    ct_prof_len
       integer*4     istatus
       integer ipage,ipage_start,ipage_end
       real*8     variance
       integer    ic_var
       real       value_real
c      integer  ptr_image,ptr_mask
C*****WAYNE******
c	include 'key_const.inc'
c	include 'key_pp.inc'
	integer dk_ptr,bytes
        integer c2_stn(5)
        integer iloc
        real  c1min_flm_R(4),c1max_flm_R(4),c1_ref_flm_R(4)
        integer i_off_R(4),i_off_min,i_off_max,n1_R(4)
       
        if ((kburst.eq.burst_start).or.(kburst.eq.burst_end)) then
         kb1 = (kburst-1)/nbeams*nbeams+1
         do i = 1, nbeams
          c1min_flm_R (i) = min(c1_g(1,1,1,1,kb1+i-1),
     1                  c1_g(1,1,1,n_c2_blk(kb1+i-1),kb1+i-1))
          c1max_flm_R (i) = max(c1_g(1,1,1,1,kb1+i-1),
     1                  c1_g(1,1,1,n_c2_blk(kb1+i-1),kb1+i-1))
	  c1_ref_flm_R(i) = nint(c1min_flm_R(i)/c1_pxl)*c1_pxl
          i_off_R     (i) = nint(c1_ref_flm_R(i) - c1_frm(1,1))/c1_pxl
          n1_R        (i) = (c1max_flm_R(i)-c1min_flm_R(i))/c1_pxl+5
         end do
        endif

 	bytes = 512*sam_ac2*8 	!4 byte, 1/2 buffer dump

	c1min_flm = min(c1_g(1,1,1,1,kburst),     
     1			c1_g(1,1,1,n_c2_blk(kburst),kburst))
        c1max_flm = max(c1_g(3,1,1,1,kburst),
     1                  c1_g(3,1,1,n_c2_blk(kburst),kburst))
	c2min_flm = c2_g(1,1,1,1,kburst)
        c2max_flm = c2_g(1,3,1,n_c2_blk(kburst),kburst)

	c1_ref_flm = nint(c1min_flm/c1_pxl)*c1_pxl
	c2_ref_flm = int(c2min_flm/c2_pxl)*c2_pxl

        n1 = (c1max_flm-c1min_flm)/c1_pxl+5
        if(n1.gt.np_geo) write(6,*) '!!!!!  !!! np_geo must be increased !!!!!!! !!!!'
        n2 = (c2max_flm-c2min_flm)/c2_pxl+5
        if(n2.gt.sam_post) write(6,*) '!!!!!  !!! sam_post must be increased !!!!!!!'
c       write(6,*)'NEW1',c1max_flm,c1min_flm,c1_pxl
c       write(6,*)'NEW2',c2max_flm,c2min_flm,c2_pxl


        i_off = nint(c1_ref_flm - c1_frm(1,1))/c1_pxl !+(kburst-1)/nbeams*40
        j_off = nint(c2_ref_flm - c2_frm(1,1))/c2_pxl !+(beam_no(kburst)-1)*200

        n_truncate = 200
        kb1 = (kburst-1)/nbeams*nbeams+1
        c2_stn(1) = (c2_flm(1,1,kb1)-c2_frm(1,1))/c2_pxl+100./c2_pxl*n_truncate
        do i = 1, nbeams-1
        c2_stn(i+1)=(c2_flm(1,2,kb1+i-1)*0.5+c2_flm(1,1,kb1+i)*0.5
     1                  -c2_frm(1,1))/c2_pxl
        end do
        c2_stn(nbeams+1) = fm_sizec2/c2_pxl
D       do i = 1, nbeams+1
D       write(6,*) ' ******  c2_stn = ', i, c2_stn(i)
D       end do



c
D       write(6,*)'WAYNE BURST #',kburst,i_off,j_off,n1,n2
        if (kburst.eq.burst_start)then
c          ipage_start = int((1+i_off-1)*1.0/(512))
           i_off_min   = i_off_R(1)
           do i=1,nbeams
            if (i_off_R(i).lt.i_off_min)  i_off_min=i_off_R(i)
           enddo
           iloc        = mod(1+i_off_min-1,1024)+1+(1+i_off_min-1)/1024*1024
           ipage_start = iloc/512
c qdn 8/21/97 check the overlay
           write(6,*)'QdN at kburst',kburst,' burst_start=',burst_start,
     *                  ' i_off=',i_off,' ipage_start=',ipage_start,'(',
     *                  int((1+i_off-1)*1.0/512),')'
           if (mod(iloc,512).eq.0)  ipage_start=ipage_start-1
           iswitch= mod(ipage_start+1,2)
           if (iswitch.eq.0) then
            ipinpon=2
           else
            ipinpon=1
           endif
        end if
c
C**********************************************************
        do i = 1, n1
        iadd = mod(i+i_off-1,1024)+1
           if(iadd.eq.(1024-10).and.ipinpon.eq.1) then
            write(6,*)'disk_ptr',disk_ptr
            call image_dump(ipinpon,ipage,
     *                     disk_ptr,sam_ac2,file_image_dump,norm_look_mode,
     *                     v_filler,l1_ct_pro,l2_ct_pro,fm_sizec2_in,
     *                     c2_pxl, buff7,buff6,ct_prof_len,ct_profile,istatus,
     *                     file_mask,file_data,variance,ic_var)
            if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
            disk_ptr=disk_ptr+bytes
            ipinpon = 2-ipinpon+1
           end if
           if(iadd.eq.(512-10).and.ipinpon.eq.2) then
            write(6,*)'disk_ptr',disk_ptr
            call image_dump(ipinpon,ipage,
     *                     disk_ptr,sam_ac2,file_image_dump,norm_look_mode,
     *                     v_filler,l1_ct_pro,l2_ct_pro,fm_sizec2_in,
     *                     c2_pxl, buff7,buff6,ct_prof_len,ct_profile,istatus,
     *                     file_mask,file_data,variance,ic_var)
            if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
            disk_ptr=disk_ptr+bytes
            ipinpon = 2-ipinpon+1
           end if
c qdn 7/31/97 modified to check the index to be within range
        do j = 1, n2
        if(j+j_off.lt.c2_stn(beam_no(kburst)).or.
     1          j+j_off.gt.c2_stn(beam_no(kburst)+1).or.
     1          j+j_off.lt.1 .or.
     1          j+j_off.gt.sam_ac ) then
        else
c        value_real          = buff4(j,i)**2
         buff6(j+j_off,iadd) = buff6(j+j_off,iadd)+buff4(j,i)**2
         buff7(j+j_off,iadd) = buff7(j+j_off,iadd)+buff5(j,i)
        endif
1111    continue
        end do
        end do

        if(kburst.eq.burst_end) then
c         ipage_end = int((n1+i_off-1)*1.0/(512))
           i_off_max   = n1_R(1)+i_off_R(1)-1
           do i=1,nbeams
            if ((n1_R(i)+i_off_R(i)-1).gt.i_off_max)  i_off_max=n1_R(i)+i_off_R(i)-1
           enddo
          iloc        = mod(i_off_max,1024)+1+(i_off_max)/1024*1024
          ipage_end = iloc/512
          if (mod(iloc,512).eq.0)  ipage_end=ipage_end-1
          iadd        = i_off_max
          if (ipage_end.eq.ipage_start) then
                 call image_dump(ipinpon,ipage,
     *                           disk_ptr,sam_ac2,file_image_dump,norm_look_mode,
     *                           v_filler,l1_ct_pro,l2_ct_pro,fm_sizec2_in,
     *                           c2_pxl, buff7,buff6,ct_prof_len,ct_profile,istatus,
     *                           file_mask,file_data,variance,ic_var)
                 if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
          else
             if ((iadd.ge.(1024-10)  .and.iadd.le.1024).or.
     &           (iadd.ge.(512-10 )  .and.iadd.le.512 )) then
                      call image_dump(ipinpon,ipage,
     *                                disk_ptr,sam_ac2,file_image_dump,norm_look_mode,
     *                                v_filler,l1_ct_pro,l2_ct_pro,fm_sizec2_in,
     *                                c2_pxl, buff7,buff6,ct_prof_len,ct_profile,istatus,
     *                                file_mask,file_data,variance,ic_var)
                      if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
              else
                      call image_dump(ipinpon,ipage,
     *                                disk_ptr,sam_ac2,file_image_dump,norm_look_mode,
     *                                v_filler,l1_ct_pro,l2_ct_pro,fm_sizec2_in,
     *                                c2_pxl, buff7,buff6,ct_prof_len,ct_profile,istatus,
     *                                file_mask,file_data,variance,ic_var)
                      if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
                      disk_ptr=disk_ptr+bytes
                      ipinpon = 2-ipinpon+1
                      call image_dump(ipinpon,ipage,
     *                                disk_ptr,sam_ac2,file_image_dump,norm_look_mode,
     *                                v_filler,l1_ct_pro,l2_ct_pro,fm_sizec2_in,
     *                                c2_pxl, buff7,buff6,ct_prof_len,ct_profile,istatus,
     *                                file_mask,file_data,variance,ic_var)
                      if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
              endif
          endif
        endif




	return
	end
c*************************************************************************
 
        subroutine image_dump(iarea,ipage,
     *  disk_ptr,sam_ac2,file_image_dump,norm_look_mode,
     *  v_filler,l1_ct_pro,l2_ct_pro,fm_sizec2_in,
     *  c2_pxl, buff7,buff6,ct_prof_len,ct_profile,istatus,
     *  file_mask,file_data,variance,ic_var)
C*****WAYNE******
       include 'ssp2_const.inc'
       integer     disk_ptr
       integer*4     sam_ac2
       character*60     file_image_dump,file_mask,file_data
       byte     buff7(sam_ac,sam_ac1_2page)
       real*8     buff6(sam_ac,sam_ac1_2page)
       integer     norm_look_mode
       integer*4     v_filler
       integer     l1_ct_pro
       integer     l2_ct_pro
       real*8     fm_sizec2_in
       real*8     c2_pxl
       integer    ct_prof_len
       real*8     ct_profile(sam_ac)
       integer*4     istatus
       integer    ipage
       real*8     variance
       integer    ic_var
C*****WAYNE******
        integer bytes_data,bytes_mask,dk_ptr0,dk_ptr_data,dk_ptr_mask,iarea
c       include 'key_const.inc'
c       include 'key_pp.inc'

c       lptr_image=ptr_image
c       lptr_mask =ptr_mask

        dk_ptr0 = disk_ptr
        write(6,*) 'image dump, dk_ptr0= ', dk_ptr0

        iadd = 1
        if(iarea.eq.2) iadd = 512+1

        call look_normalize(iadd,
     *  buff7,sam_ac2,norm_look_mode,v_filler,
     *  disk_ptr,l1_ct_pro,l2_ct_pro,fm_sizec2_in,
     *  c2_pxl,buff6,ct_prof_len,ct_profile,variance,ic_var       )

        bytes_data = sam_ac2*8
        bytes_mask = sam_ac2
        do k = 1, 512
           dk_ptr_data = dk_ptr0  +(k-1)*bytes_data
           dk_ptr_mask = dk_ptr0/8+(k-1)*bytes_mask
           call write_disk(file_data,buff6(1,iadd+k-1),bytes_data,dk_ptr_data,istatus)
           if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
           call write_disk(file_mask,buff7(1,iadd+k-1),bytes_mask,dk_ptr_mask,istatus)
           if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
           do i = 1, sam_ac2
            buff6(i,iadd+k-1) = 0.
            buff7(i,iadd+k-1) = 0
           end do
        end do

1111    continue
        ipage=ipage+1

        return
        end



c*************************************************************************

	subroutine look_normalize(iadd,
     *  buff_mask,sam_ac2,norm_look_mode,v_filler,
     *  disk_ptr,l1_ct_pro,l2_ct_pro,fm_sizec2_in,
     *  c2_pxl,buff_data,ct_prof_len,ct_profile,variance,ic_var       )
C*****WAYNE******
       include 'ssp2_const.inc'
       byte     buff_mask(sam_ac,sam_ac1_2page)
       integer*4     sam_ac2
       integer     norm_look_mode
       integer*4     v_filler
       integer     disk_ptr
       integer     l1_ct_pro
       integer     l2_ct_pro
       real*8     buff_data(sam_ac,sam_ac1_2page)
       real*8     fm_sizec2_in
       real*8     c2_pxl
       integer    ct_prof_len
       real*8     ct_profile(sam_ac)
       real*8     variance
       integer    ic_var
C*****WAYNE******

 	do j = iadd, iadd+512-1
	do i = 1, sam_ac2
	nlooks = buff_mask(i,j)
	  if(nlooks.ne.0) then
	    if(norm_look_mode.eq.1) then	      !(1=process, 0=skip)
 	    buff_data(i,j) = buff_data(i,j)    !normalize by nlooks
c	    buff_data(i,j) = sqrt(buff_data(i,j)/nlooks)    !normalize by nlooks
c	    buff_data(i,j) = buff_data(i,j)/nlooks    !normalize by nlooks
	    else
 	    buff_data(i,j) = buff_data(i,j)         !divide by 2
c	    buff_data(i,j) = sqrt(buff_data(i,j)/2)         !divide by 2
c	    buff_data(i,j) = buff_data(i,j)/2         !divide by 2
	    end if
	  else
 	    buff_data(i,j) = buff_data(i,j)		  !use v_filler for invalid pixel
c	    buff_data(i,j) = v_filler		  !use v_filler for invalid pixel
	  end if
	end do
	iline = disk_ptr/(8*sam_ac2)+j-1
c	if(iline.gt.l1_ct_pro.and.iline.lt.l2_ct_pro)
c    *     call alng_avg(j,
c    *  buff_mask,sam_ac2,fm_sizec2_in,c2_pxl,
c    *  buff_data,ct_prof_len,ct_profile,variance,ic_var)
        end do

	return
	end

	
	subroutine alng_avg(j,
     *  b_mask,sam_ac2,fm_sizec2_in,c2_pxl,
     *  b_data,ct_prof_len,ct_profile,variance,ic_var)
C*****WAYNE******
       include 'ssp2_const.inc'
       byte     b_mask(sam_ac,sam_ac1_2page)
       integer*4     sam_ac2
       real*8     fm_sizec2_in
       real*8     c2_pxl
       real*8     b_data(sam_ac,sam_ac1_2page)
       real*8     ct_profile(sam_ac)
       integer   ct_prof_len
       real*8    variance
       integer ic_var
C*****WAYNE******
	integer ist,iend

	ist = 0
	
	do k = 1, sam_ac2/8			!search 1st point
	   nlooks = b_mask(k,j)
	   if(nlooks.ne.0) then
	   ist = k
	   go to 1111
	   end if
	end do
1111	iend = ist+fm_sizec2_in/c2_pxl-2

	if(ist.ne.0) then
	  if(c1_pxl.eq.100) then
	   ct_prof_len = iend-ist-1
	   do i = 1, ct_prof_len
	   ct_profile(i)=ct_profile(i)+b_data(i+ist,j)
	   end do
C**add it to calculate variance which resulted in a scale
           do k=ist, sam_ac2
           if (ct_profile(k-ist+1).ne.0) then
            variance= variance + (b_data(k,j)/ct_profile(k-ist+1))**2
            ic_var  = ic_var+1
           endif
           enddo
C**add it to calculate variance which resulted in a scale
	  else
	   ct_prof_len = (iend-ist)/2-2
	   do i = 1, ct_prof_len
	   ct_profile(i)=ct_profile(i)+b_data(i*2+ist,j)
	   end do
C**add it to calculate variance which resulted in a scale
           do k=ist, sam_ac2
           if (ct_profile(k-ist).ne.0) then
            variance= variance + (b_data(k,j)/ct_profile((k-ist)/2+1))**2
            ic_var  = ic_var+1
           endif
           enddo
C**add it to calculate variance which resulted in a scale
	  end if
	end if

c	write(6,*) 'j,ctlen', j,ct_prof_len
c	do k = 1, ct_prof_len,100
c	write(6,*) 'k,ct_prof', k,ct_profile(k)
c	end do

	
	return
	end
c************************************************************


c	subroutine quick_process(
c    *  ire_sim,burst_start,burst_end,burst_good,c1_pxl,
c    *  c2_pxl,fm_sizec1,fm_sizec2,image_beam_mode,
c    *  beam_no,file_pj_image_low,v_filler,
c    *  fs,file_spc_raw,ns,
c    *  chirp_rate,fd_bip,nfft_az,r_1st,
c    *  dr,r_bip,coef_fd,prf,
c    *  coef_fr,r_g,c1_g,c2_g,
c    *  nr,v_sc_mag,coef_lk_ang,look_ang,
c    *  roll_o,roll,ant_patn_r,beam_id,
c    *  ant_patn_az,v_noise,n_c2_blk,fd_g,
c    *  c1_frm,c2_frm,c1size_blk,c2size_blk,
c    *  df,istatus,plen,dk_burst,file_raw)
c	implicit real*8 (a-h, o-z)
c	include 'key_const.inc'
c	include 'key_pp.inc'
C*****WAYNE******
c      include 'ssp2_const.inc'
c      integer     ire_sim
c      integer*4     burst_start
c      integer*4     burst_end
c      integer*4     burst_good(1600)
c      real*8     c1_pxl
c      real*8     c2_pxl
c      real*8     fm_sizec1
c      real*8     fm_sizec2
c      integer     image_beam_mode
c      integer*4     beam_no(burst_max)
c      character*60     file_pj_image_low
c      integer*4     v_filler
c      integer     np_proc
c      real*8     fs
c      character*60     file_spc_raw
c      byte     buff0(2*sam_raw,line_raw)
c      integer*4     istatus
c      integer*4     ns(burst_max)
c      complex     buff1(sam_raw,line_raw)
c      real*8     chirp_rate
c      real*8     fd_bip(burst_max)
c      integer*4     nfft_az
c      real*8     r_1st(burst_max)
c      real*8     dr(burst_max)
c      real*8     r_bip(burst_max)
c      real*8     coef_fd(4,burst_max)
c      real*8     prf(burst_max)
c      complex     buff2(line_raw,sam_raw)
c      real*8     coef_fr(4,burst_max)
c      real*8     r_g(3,3,n_az,n_rg,burst_max)
c      real*8     c1_g(3,3,n_az,n_rg,burst_max)
c      real*8     c2_g(3,3,n_az,n_rg,burst_max)
c      integer*4     nr
c      real*8     v_sc_mag(burst_max)
c      real*8     coef_lk_ang(4,burst_max)
c      real*8     look_ang(burst_max)
c      real*8     roll_o(burst_max)
c      real*8     roll(burst_max)
c      real     ant_patn_r(ns_ant_rg,7)
c      integer*4     beam_id(burst_max)
c      real     ant_patn_az(ns_ant_az)
c      integer*4     v_noise
c      integer*4     n_c2_blk(burst_max)
c      real*8     fd_g(3,3,n_az,n_rg,burst_max)
c      real*8     c1_frm(2,2)
c      real*8     c2_frm(2,2)
c      real*8     c1size_blk(burst_max)
c      real*8     c2size_blk(burst_max)
c      real*8     df(burst_max)
c      real     qk_image(sam_ac/8,sam_ac/8)
c      byte     qk_mask(sam_ac/8,sam_ac/8)
c      real*8          plen
c      integer*4     dk_burst(burst_max)
c      character*60 file_raw
C*****WAYNE******



c******* clear image buffer ***********************************
	
c	do i = 1, nint(fm_sizec1/c1_pxl)
c	do j = 1, nint(fm_sizec2/c2_pxl)
c	qk_image(i,j) = 0.
c	qk_mask(i,j) = 0
c	end do
c	end do
	
c	do k = burst_start, burst_end	!loop through all bursts

c	if (burst_good(k) .eq. yes ) then

c	write(6,*) image_beam_mode
c	write(6,*) beam_no(k), ',  burst no = ', k

c	np_proc = 8

c       call get_burst_sim(
c    *  k,plen,np_proc,fs,file_spc_raw,
c    *  ns,buff0,istatus,buff1,dk_burst,file_raw       )
c	write(6,*) 'end of get burst'

c       call range_comp(
c    *  k,nr,fs,chirp_rate,fd_bip,
c    *  nfft_az,np_proc,ns,r_1st,
c    *  dr,r_bip,coef_fd,prf,
c    *  buff1        )
c	write(6,*) 'end of range compression'

c       call corner_turn(
c    *  k,nr,ns,np_proc,nfft_az,
c    *  buff1,buff2       )
c	write(6,*) 'end of corner turn'

c       call az_comp(
c    *  k,nr,np_proc,nfft_az,ns,
c    *  r_1st,dr,r_bip,coef_fd,
c    *  coef_fr,prf,buff2  )
c	write(6,*) 'end of azimuth compression'

c	call qk_radio_geo(k,ire_sim,
c    *  r_g,c1_g,c2_g,c1_pxl,
c    *  c2_pxl,dr,nfft_az,ns,
c    *  nr,v_sc_mag,r_1st,r_bip,
c    *  coef_lk_ang,look_ang,roll_o,roll,
c    *  prf,ant_patn_r,beam_id,ant_patn_az,
c    *  v_noise,n_c2_blk,fd_g,c1_frm,
c    *  c2_frm,c1size_blk,c2size_blk,coef_fd,
c    *  df,buff2,qk_image,qk_mask       )
c	else
c	   write(6,*)' quick proc: bad burst # skip at:',k,burst_good(k)
c	endif

c	end do
	

c******* perform look normalization & output to disk ***************
	
c	do i = 1, nint(fm_sizec1/c1_pxl)
c	do j = 1, nint(fm_sizec2/c2_pxl)
c	if(qk_mask(i,j).ne.0) then
c	qk_image(i,j) = sqrt(qk_image(i,j) / qk_mask(i,j))
c	else
c	qk_image(i,j) = v_filler
c	end if
c	end do
c	end do

c	ipt = 0
c	nbyte = nint(fm_sizec1/c1_pxl)*nint(fm_sizec2/c2_pxl)*4
c	call write_disk(file_pj_image_low,qk_image,nbyte,ipt,istatus) 

c	return
c	end
	subroutine qk_radio_geo(kburst,ire_sim,
     *  r_g,c1_g,c2_g,c1_pxl,
     *  c2_pxl,dr,nfft_az,ns,
     *  nr,v_sc_mag,r_1st,r_bip,
     *  coef_lk_ang,look_ang,roll_o,roll,
     *  prf,ant_patn_r,beam_id,ant_patn_az,
     *  v_noise,n_c2_blk,fd_g,c1_frm,
     *  c2_frm,c1size_blk,c2size_blk,coef_fd,
     *  df,buff2r,qk_image,qk_mask       )
	implicit real*8 (a-h,o-z)
c	include 'key_const.inc'
c	include 'key_pp.inc'
C*****WAYNE******
       include 'ssp2_const.inc'
       integer     ire_sim
       real*8     r_g(3,3,n_az,n_rg,burst_max)
       real*8     c1_g(3,3,n_az,n_rg,burst_max)
       real*8     c2_g(3,3,n_az,n_rg,burst_max)
       real*8     c1_pxl
       real*8     c2_pxl
       real*8     dr(burst_max)
       integer*4     nfft_az
       integer*4     ns(burst_max)
       integer*4     nr
       real*8     v_sc_mag(burst_max)
       real*8     r_1st(burst_max)
       real*8     r_bip(burst_max)
       real*8     coef_lk_ang(4,burst_max)
       real*8     look_ang(burst_max)
       real*8     roll_o(burst_max)
       real*8     roll(burst_max)
       real*8     prf(burst_max)
       real     ant_patn_r(ns_ant_rg,7)
       integer*4     beam_id(burst_max)
       real     ant_patn_az(ns_ant_az)
       integer*4     v_noise
       integer*4     n_c2_blk(burst_max)
       real*8     fd_g(3,3,n_az,n_rg,burst_max)
       real*8     c1_frm(2,2)
       real*8     c2_frm(2,2)
       real*8     c1size_blk(burst_max)
       real*8     c2size_blk(burst_max)
       real*8     coef_fd(4,burst_max)
       real*8     df(burst_max)
       complex     buff2r(line_raw,sam_raw)
       real     qk_image(sam_ac/8,sam_ac/8)
       byte     qk_mask(sam_ac/8,sam_ac/8)
C*****WAYNE******
c******* N to one average in range *****************************

	dr_dc2 =(r_g(1,2,1,1,kburst)-r_g(1,1,1,1,kburst))/
     1		 (c2_g(1,2,1,1,kburst)-c2_g(1,1,1,1,kburst)) 
	n_r_avg = dr_dc2*c2_pxl/dr(kburst)+1
     	del_r = n_r_avg*dr(kburst)

	do i = 1, nfft_az
	do j = 1, ns(kburst)-nr ,n_r_avg 
	j1 = j/n_r_avg+1
 	buff2r(i,j1) = 0.
	do k = 1, n_r_avg
c 	buff2r(i,j1)=buff2r(i,j1)+(buff2(i,j+k-1)*conjg(buff2(i,j+k-1)))
 	buff2r(i,j1)=buff2r(i,j1)+(buff2r(i,j+k-1)*conjg(buff2r(i,j+k-1)))
	end do
 	buff2r(i,j1) = sqrt(buff2r(i,j1)/n_r_avg)
	end do
	end do

c****** radiometric compensation process **********************

	m = 4
	dfddth = 2*v_sc_mag(kburst)/lambda *pi/180.

	do j = 1, (ns(kburst)-nr)/n_r_avg 
	r = (j-1)*del_r+r_1st(kburst)+.5*del_r
	  call v_poly(r,r_bip(kburst),coef_lk_ang(1,kburst),m,vlook_ang)
	  if(ire_sim.eq.0) then
	    k1 = nint((vlook_ang-look_ang(kburst))/dang_rg)+ns_ant_rg/2+1		
	  else
	    k1 = nint((vlook_ang-roll_o(kburst)-roll(kburst)-ele_ang_ref)
     1		 /dang_rg)+1
	  end if
	do i = 1, nfft_az
	hori_ang = ((i-nfft_az/2-1)*prf(kburst)/nfft_az)/dfddth
	k2 = nint(abs(hori_ang)/dang_az)+1
	ant_gain = ant_patn_r(k1,beam_id(kburst))*ant_patn_az(k2)
c	write(6,*) k1,k2,ant_patn_r(k1,beam_id(kburst)),ant_patn_az(k2)

	buff2r(i,j) = 300*(buff2r(i,j)/ant_gain)*(r/r_bip(1))**2+v_noise

	end do
	end do

	write(6,*) 'end of radiometric compensation'

c****** geometric rectification process ***********************

	do k = 1, n_c2_blk(kburst)
	c10 = (nint(c1_g(1,1,1,k,kburst)/c1_pxl)+1)*c1_pxl

	dr_dc1 = (r_g(2,1,1,k,kburst)-r_g(1,1,1,k,kburst))/
     1		 (c1_g(2,1,1,k,kburst)-c1_g(1,1,1,k,kburst)) 
	dfd_dc1 =(fd_g(2,1,1,k,kburst)-fd_g(1,1,1,k,kburst))/
     1		 (c1_g(2,1,1,k,kburst)-c1_g(1,1,1,k,kburst)) 
	dr_dc2 =(r_g(1,2,1,k,kburst)-r_g(1,1,1,k,kburst))/
     1		 (c2_g(1,2,1,k,kburst)-c2_g(1,1,1,k,kburst)) 

	iout0 = (c10 - c1_frm(1,1))/c1_pxl + 1
	jout0 = (c2_g(1,1,1,k,kburst) - c2_frm(1,1))/c2_pxl + 1
c	write(6,*) 'fdc=', fdc

	do j = 1, nint(c2size_blk(kburst)/c2_pxl)
	jout = jout0+j-1

	dc2 = (j-1)*c2_pxl
	c2 = c2_g(1,1,1,k,kburst) + dc2
	r = r_g(1,1,1,k,kburst) + dc2*dr_dc2 
	aj_in0 = (r-r_1st(kburst)-.5*del_r)/del_r
	call v_poly(r,r_bip(kburst),coef_fd(1,kburst),m,fdc)

	do i = 1, nint(c1size_blk(kburst)/c1_pxl)
	iout = iout0+i-1

	c1 = c10 + (i-1)*c1_pxl
	dc1 = c1 - c1_g(1,1,1,k,kburst)
	fd = fd_g(1,1,1,k,kburst) + dc1*dfd_dc1 
	ai_in = (fd - fdc)/df(kburst)+nfft_az/2+1
	i_in = ai_in
	d1 = ai_in - i_in

	aj_in = aj_in0 + dc1*dr_dc1/del_r
	j_in = aj_in
	d2 = aj_in - j_in
c	write(6,*) 'i_in,j_in,d1,d2,iout,jout', i_in,j_in,d1,d2,iout,jout	

        iout=iout+1
        jout=jout+1
        i_in=i_in+1
        j_in=j_in+1

	value = buff2r(i_in,j_in)*(1-d1)*(1-d2)+buff2r(i_in,j_in+1)*(1-d1)*d2+
     1		buff2r(i_in+1,j_in)*d1*(1-d2)+buff2r(i_in+1,j_in+1)*d1*d2
	qk_image(iout,jout) =  value**2 + qk_image(iout,jout)
	qk_mask(iout,jout) = qk_mask(iout,jout) + 1
	end do
	end do
	end do

	write(6,*) 'end of geometric compensation'

	return
	end
        subroutine put_frame(buff6,buff7,v_filler,sam_ac2)
        include 'ssp2_const.inc'
        real*8   buff6(sam_ac,sam_ac1_2page)
        byte     buff7(sam_ac,sam_ac1_2page)
        integer bytes,dk_ptr
        integer sam_ac2,ifd,ccreate
        integer v_filler
        byte buffb(sam_ac1_2page,sam_ac)
        character*60 file_name

        do i = 1, sam_ac2
        do j = 1, sam_ac1_2page
        itemp = buff6(i,j)
          if(buff7(i,j).eq.0) then
            buffb(j,i) = v_filler
          else
            itemp = sqrt(buff6(i,j)/buff7(i,j))*0.8
            if(itemp.gt.255) itemp = 255
            buffb(j,i) = itemp
          end if
        end do
        end do

        bytes = sam_ac2*sam_ac1_2page
        dk_ptr = 0
        file_name= '/home/tmpdisk/check.dat'
        ifd           = ccreate(file_name)
        call write_disk(file_name,buffb(1,1),bytes,dk_ptr,istatus)
        if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return

        return
        end



	subroutine put_framelet(istage,
     *  buff1,buff2a,buff3,buff4,
     *  buff5,file_framelet,istatus)
C*****WAYNE******
       include 'ssp2_const.inc'
       complex     buff1(sam_raw,line_raw)
       complex     buff2a(np_geo,sam_raw)
       real     buff3(sam_raw,np_geo)
       real     buff4(sam_post,np_geo)
       byte     buff5(sam_post,np_geo)
       character*60     file_framelet(4)
       integer*4     istatus
C*****WAYNE******
	integer bytes,dk_ptr
c 	include 'key_const.inc'
c	include 'key_pp.inc'
	byte b2(np_geo,sam_raw)
	byte b4(np_geo,sam_post)

	ipxl = np_geo
	iline = sam_raw
	if(istage.eq.1) ipxl = line_raw
	if(istage.eq.4) iline = sam_post
	do i = 1, iline
	do j = 1, ipxl
	if(istage.eq.1) then
	val =  30.*real(buff1(i,j))+128.
	ival = val
	b2(j,i) = ival	
        val =  30.*aimag(buff1(i,j))+128.
        ival = val
        b2(j+128,i) = ival
	else
	if(istage.eq.2) then
	val =  abs(buff2a(j,i))*0.5
	ival = val
	b2(j,i) = ival	
	else
	if(istage.eq.3) then
	val = 2.*sqrt(buff3(i,j))
	ival = val
	b2(j,i) = ival
	else
	val =  abs(buff4(i,j))
	ival = val
c	b4(j,i) = buff5(i,j) 	!output mask
	b4(j,i) = ival		!output data
	end if
	end if
	end if
	end do
	end do


	dk_ptr = 0
	bytes = np_geo*iline

	if(istage.ne.4) then
	call write_disk(file_framelet(1),b2,bytes,dk_ptr,istatus)
        if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
	else
	call write_disk(file_framelet(1),b4,bytes,dk_ptr,istatus)
        if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
	end if

	return
	end

	subroutine put_pvs_4b(kburst,
     *  prod_type,nbeams,buff4,file_pvs_look,
     *  istatus,burst_pvs,n_single_look,proc_gain)
C*****WAYNE******
       include 'ssp2_const.inc'
       integer*4     prod_type
       integer*4     nbeams
       real     buff4(sam_post,np_geo)
       integer*4     istatus
       character*60      file_pvs_look
       integer       burst_pvs
       integer       n_single_look
C*****WAYNE******
       integer       ok
       integer       b_ctr
       real*8          proc_gain
C*****************
          ok=mod(burst_pvs,nbeams)


          if (nbeams .eq. 4) then
             if (ok .eq. 1) then
                 b_ctr=burst_pvs
             else if (ok .eq. 2) then
                 b_ctr=burst_pvs-1
             else if (ok .eq. 3) then
                 b_ctr=burst_pvs-2
             else if (ok .eq. 0) then
                 b_ctr=burst_pvs-3
             endif
          else  if (nbeams .eq. 3) then
             if (ok .eq. 1) then
                 b_ctr=burst_pvs
             else if (ok .eq. 2) then
                 b_ctr=burst_pvs-1
             else if (ok .eq. 0) then
                 b_ctr=burst_pvs-2
             endif
          else  if (nbeams .eq. 2) then
             if (ok .eq. 1) then
                 b_ctr=burst_pvs
             else if (ok .eq. 0) then
                 b_ctr=burst_pvs-1
             endif
          endif
         
        do i=1,nbeams
         if (kburst.eq.(b_ctr+i-1)) call pvs_beam(file_pvs_look,buff4,i-1,istatus,proc_gain)
         if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
        enddo




	return
	end




	subroutine pvs_beam(
     *  file_pvs_look,buff4,iptr,istatus,proc_gain)
C*****WAYNE******
       include 'ssp2_const.inc'
       real     buff4(sam_post,np_geo)
       character*60     file_pvs_look
       integer*4     istatus
       integer*4     iptr
       real*8          proc_gain
C*****WAYNE******
	integer bytes,dk_ptr
c	include 'key_const.inc'
c	include 'key_pp.inc'
	byte b_data(sam_post,np_geo)

	ipxl = np_geo
	iline = sam_post
	
	imax=0.	
	do i = 1, iline
	do j = 1, ipxl
	   ival =  abs(buff4(i,j))
	   if (ival .gt. imax) imax=ival
	   b_data(i,j) = ival		!output data
	end do
	end do

        if (imax.ne.0) then
	do i = 1, iline
	do j = 1, ipxl
           b_data(i,j) = b_data(i,j)*proc_gain
c	   b_data(i,j) = b_data(i,j)*(255/imax)
	end do
	end do
        endif

	bytes = ipxl*iline
   	dk_ptr = iptr*bytes
        write(6,*)'DUMPING FRAMELET ',bytes,dk_ptr
	call write_disk(file_pvs_look,b_data,bytes,dk_ptr,istatus)
        if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return

	return
	end
                               

c*************************************************************
	subroutine test_image(kburst,
     *  c1_g,c2_g,n_c2_blk,c1_pxl,
     *  c2_pxl,c1_frm,c2_frm,buff_out,mask_out)
C*****WAYNE******
       include 'ssp2_const.inc'
       real*8     c1_g(3,3,n_az,n_rg,burst_max)
       real*8     c2_g(3,3,n_az,n_rg,burst_max)
       integer*4     n_c2_blk(burst_max)
       real*8     c1_pxl
       real*8     c2_pxl
       real*8     c1_frm(2,2)
       real*8     c2_frm(2,2)
       real     buff_out(sam_post,np_geo)
       byte     mask_out(sam_post,np_geo)
C*****WAYNE******
c	include 'key_const.inc'
c	include 'key_pp.inc'
c	real buff_out(sam_post,np_geo)
c	byte mask_out(sam_post,np_geo)
c 	equivalence (buff_out,buff4)
c	equivalence (mask_out,buff5)
	real*8 c1,c2,aic1,aic2,c1_ref_flm,c2_ref_flm

	c1min_flm = min(c1_g(1,1,1,1,kburst),     
     1			c1_g(1,1,1,n_c2_blk(kburst),kburst))
	c2min_flm = c2_g(1,1,1,1,kburst)

        c1_ref_flm = nint(c1min_flm/c1_pxl)*c1_pxl
	c2_ref_flm = int(c2min_flm/c2_pxl)*c2_pxl
	i_off = nint(c1_ref_flm - c1_frm(1,1))/c1_pxl 
	j_off = nint(c2_ref_flm - c2_frm(1,1))/c2_pxl 

	do i = 1, np_geo
	do j = 1, sam_post
	buff_out(j,i) = 0.
	mask_out(j,i) = 0
	end do
	end do

	do i = 1+np_geo/4, np_geo*3/4
	do j = 32, sam_post-32
	i0 = i+i_off
	j0 = j+j_off
	buff_out(j,i) = 0.
	if(i0/100*100.eq.i0.and.j0/100*100.eq.j0) buff_out(j,i) = 180.
	mask_out(j,i) = 1
	end do
	end do

	return
	end

	subroutine grid(
     *  buff_in)
C*****WAYNE******
       include 'ssp2_const.inc'
       complex     buff_in(np_geo,sam_raw)
C*****WAYNE******
	implicit none
c	include 'key_const.inc'
c	include 'key_pp.inc'
c	complex buff_in(np_geo,sam_raw)	!Input buffer,range compressed data
c	equivalence (buff_in,buff2a)
	integer i,j
	real value

	do i = 1, np_geo
	do j = 1, sam_raw
	buff_in(i,j) = cmplx(0.01,0.01)
	end do
	end do

	do i = 1,np_geo,10
	do j = 1, sam_raw
	buff_in(i,j) = cmplx(100.,0.)
	end do
	end do

	do i = 1, np_geo
	do j = 1, sam_raw, 100
	buff_in(i,j) = cmplx(100.,0.)
	end do
	end do

	return
	end
c-------------------------------------------------------------
        subroutine  out_byte(buffb,x_len,y_len,b_x,b_y,out_fname)
         integer b_x,b_y
         integer x_len,y_len
         byte buffb(b_x,b_y)
         character*60  out_fname
         integer dk_ptr,ch_fd,n_bytes,bytes
         integer ccreate,cwrite

           dk_ptr = 0
           bytes = b_x*b_y
           ch_fd=ccreate(out_fname)
           write(6,*)' Rg Dump size:',b_x,b_y,out_fname
           n_bytes = cwrite(ch_fd,buffb,bytes,dk_ptr)
           if ( n_bytes .ne. bytes) then
           write(6,*)' write ERR 8192*128',bytes,n_bytes,out_fname
           endif
           call cclose(ch_fd)

        return
        end
        subroutine out_cmplx2(buff_x2,buff_r4,buff_b,
     *                 x_len,y_len,b_x,b_y,out_fname)

        implicit none

c       ------------------
c       INPUT PARAMETERS
c       ------------------
        integer         b_x,b_y                 !buff size
        complex         buff_x2(b_x,b_y)
        real            buff_r4(b_x,b_y)
        byte            buff_b(b_x,b_y)
        integer         x_len,y_len             !real data size
        character*60    out_fname

c       ------------------
c       OUTPUT PARAMETERS
c       ------------------
c       Disk file

c       ------------------
c       LOCAL VARIABLES
c       ------------------
        integer         i,j
        real            max_r4,val
        integer         ival
        integer         dk_ptr,bytes,ch_fd,n_bytes
        integer         ccreate,cwrite


           do i=1,y_len
           do j=1,x_len
               buff_r4(j,i)= sqrt(buff_x2(j,i)*conjg(buff_x2(j,i)))
           end do
           end do
           max_r4=0.0
           do i=1,y_len
           do j=1,x_len
               if (buff_r4(j,i) .gt. max_r4) max_r4=buff_r4(j,i)
           end do
           end do

          write(6,*)' max r4:',max_r4,y_len,x_len
           do i=1,y_len
           do j=1,x_len

             val=buff_r4(j,i)*255.0/max_r4
             if (val .gt. 255.) then
                 val=255.0
                 write(6,*)'gt 255:',val
             endif
             ival=int(val)
             buff_b(j,i)=ival
           end do
           end do

           dk_ptr = 0
           bytes = b_x*b_y
           ch_fd=ccreate(out_fname)
           write(6,*)' Rg Dump size:',b_x,b_y,out_fname
           n_bytes = cwrite(ch_fd,buff_b,bytes,dk_ptr)
           if ( n_bytes .ne. bytes) then
           write(6,*)' write ERR 8192*128',bytes,n_bytes,out_fname
           endif
           call cclose(ch_fd)

        return
        end

c--------------------------------------------------------
