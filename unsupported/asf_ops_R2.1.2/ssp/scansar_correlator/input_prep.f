c SccsId = @(#)input_prep.f	2.41 3/24/98
       subroutine input_prep(i_file_type,file_config,istatus,kids,mytask)
       include 'ssp2_const.inc'
       integer     i_file_type
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
       character*60     file_image_dump
       character*60     file_mask_dump
       character*60     file_pj_image
       character*60     file_pj_image_low
       character*60     file_frame
       character*60     file_framelet(4)
       real*8     p1_pxl
       real*8     p2_pxl
       character*60     file_ac_image
       integer    kids,mytask
       character*60 file_raw,file_dk,file_dk_temp,file_corr_to_post,file_mask,file_data,file_aux
       character*60 file_pre_to_corr
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
       character*256    echo_file,aux_file,burst_loc_file,topo_file
       integer*4       job_mode,ifd,isuccess
       character*256    file_config
       real*8     fm_sizec1
       real*8     fm_sizec2
       real*8     fm_sizec1_in
       real*8     fm_sizec2_in
       integer*4     nbeams
       real*8     bm_id(4)
       real*8     rng_bm_ang(4)
       real*8     peak_bm_ang(4)
       real*8     ele_ang_eq(4)
       integer    istatus,get_node
       character*256 file_status
       real*8          az_peak_coef(3,8)
       integer proc_mode
C************************
       integer*4 utm_zone
       real*8    lamb_lat_s
       real*8    lamb_lat_n
       real*8    ps_lat,ps_lon
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
       integer*4       sim_eam_nsp(4),burst_start,burst_end
       real*8          sv_x_pos
       real*8          sv_y_pos
       real*8          sv_z_pos
       real*8          sv_x_vel
       real*8          sv_y_vel
       real*8          sv_z_vel
       real*8          fs
       integer*4       proc_id
       character*60       product_id
       integer*4       sv_type
       character*60    file_pmf_template
       integer*4       ter_correct
       integer         nburst_spec
       integer         idummy
       character*60    sitename
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
        character*60    file_pmflet(4)
        integer         ant_flag
        integer         subframe_id
        integer         prf_num
        character*60    file_az
       
    
 




        job_mode=0

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
        if (mytask.eq.0) idummy=printfstat(file_status,1,0.0)

c ceos_leader_file,image_file,pmf_file,cali_file  are outputs

        istatus= get_node(i_file_type,echo_file,aux_file,rep_file,eph_file,
     *            burst_loc_file,topo_file,cali_file,scan_results_file,
     *            file_raw,file_aux,file_rep,file_eph,file_dk,file_dk_temp,file_topo,
     *            file_cali,file_scan_results,
     *            file_status,file_config,ire_sim,kids,mytask)
        if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
c       if (mytask.eq.0) idummy=printfstat(file_status,1,100.0)


        return
        end
