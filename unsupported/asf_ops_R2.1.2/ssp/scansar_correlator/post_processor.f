c SccsId = @(#)post_processor.f	2.41 3/24/98
        subroutine post_processor(file_config,istatus,kids,mytask)
C*****WAYNE******
       include 'ssp2_const.inc'
       integer*4       prod_type
       integer     ire_sim
       integer     proj
       real*8     c1_frm(2,2)
       real*8     c2_frm(2,2)
       character*60     file_pj_image
       integer     res_prod
       integer*4     sam_ac2
       integer     ct_prof_len
       integer     l1_ct_pro
       integer     l2_ct_pro
       real*8     c1_pxl
       real*8     c2_pxl
       real*8     ct_profile(sam_ac)
       character*60     file_image_dump
       character*60     file_mask_dump
       integer*4     v_filler
       integer*4     istatus
       real*8     proc_gain
       real*8     alpha1
       real*8     alpha2
       real*8     alpha3
       character*60     file_pj_image_low
       real*8     p1_pxl
       real*8     p2_pxl
       integer     grid_ptn_flag 
       character*60 file_ac_image
       integer      ipage,ipage_start
       integer kids,mytask
       real*8     overlay_image(sam_ac,sam_ac1_1page)
       byte     overlay_mask(sam_ac,sam_ac1_1page)
       real*8     buff6(sam_ac,sam_ac1_1page)
       byte     buff7(sam_ac,sam_ac1_1page)
       character*60 file_corr_to_post,file_mask,file_data
       character*60 file_pre_to_corr
       character*256 file_config
       character*256 file_status
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
        real*8 ta,tb,rtc
        integer nbuf(4),dontcare,allmsg,nulltask,allgrp
        integer total_page
        integer proc_mode
C*****WAYNE******
        integer job_mode
        integer iquick
        integer n_times_pre
        integer data_cond
        integer     pta_refn_mode
        integer     rgc_est_mode
        integer     fdc_est_mode
        integer     prf_est_mode
        integer     ct_profile_mode
        integer     norm_look_mode
        integer     dem_ellip
        integer     radio_comp_mode
        integer     image_beam_mode
        integer     burst_no_dump
        integer     stage_dump
        integer*4     beam_mode
        character*60     file_im_sirc
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
        character*60     file_frame
        character*60     file_framelet(4)
        character*60     file_raw,file_aux,file_dk,file_dk_temp
        real*8          snr(burst_max)
        integer*4       burst_good(1600)
        real*8          sv_ref_time
        real*8          gain_rec(burst_max)
        integer*4       blk_gain(iq_blk,burst_max)
        real*8     topo_spacing
        real*8          plen
        integer*4       nr
        character*60     file_pvs_beam
C*****WAYNE******
        character*256    echo_file,aux_file,burst_loc_file,topo_file
        integer         ipage_position,istart,max_page
        integer         ipage_position_last,ipage_last
        integer         save_ipage,save_ipage_start,icheck_dump 
        real            scale_im
        real*8          variance
        byte     ac_image(sam_ac,sam_ac)
        integer nbytes,bytes
        real*8         variance_sum
        real*8     ct_profile_sum(sam_ac)
        character*10 str_buff1,str_buff2
        real  a_percent
c**************
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
       integer         nlines,nsamples
       character*60  file_ceos_template
       real*8        p1_fm(2,2),p2_fm(2,2)
       integer*4       sim_eam_nsp(4),burst_start,burst_end
       real*8          sv_x_pos
       real*8          sv_y_pos
       real*8          sv_z_pos
       real*8          sv_x_vel
       real*8          sv_y_vel
       real*8          sv_z_vel
       real*8          fs
       integer         proc_id
       character*60         product_id
       integer*4       sv_type
       character*60    file_pmf_template
       integer*4       ter_correct
        real*8  plat1,plat2
        real*8 lat_orig,long_orig
        real*8     slat
        real*8     along0
        integer     zone
        integer    nburst_spec
        integer    itrouble
        character*60  sitename
        integer       idummy
        integer     overlay_f(100)
        integer     iptr_image,iptr_mask
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
        integer    curr_ipage,curr_ipage_start
        character*60    file_pmflet(4)
        integer   ant_flag
        integer   subframe_id
        integer   prf_num
        character*60 file_az
        real*8       rlocal_mean




      


         ta = rtc()


        allgrp   = MPI_COMM_WORLD
C*******************
        job_mode =3
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
        if (mytask.eq.0)idummy=printfstat(file_status,4,0.0)

C*******************


        call disk_to_post_proc(
     *  c1_frm,c2_frm,
     *  sam_ac2,ct_prof_len,l1_ct_pro,l2_ct_pro,
     *  v_filler,
     *  ct_profile,
     *  alpha1,
     *  alpha2,alpha3,
     *  ipage,ipage_start,file_corr_to_post,variance,istatus,rlocal_mean)
        if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return

          do i = 1, sam_ac/2
          ct_profile(i) = 0
          enddo
C***********************************************************
        if (mytask.eq.0) then
           ipage_last=0
           do ii=0,kids-1
             if (ii.eq.0) then
              curr_ipage       = ipage 
              curr_ipage_start=  ipage_start
             else
              call mpi_bcast(ii,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
              call receive_page_info(ii,curr_ipage,curr_ipage_start)
             endif
             if ((curr_ipage_start+curr_ipage-1).gt.ipage_last)
     *         ipage_last=curr_ipage_start+curr_ipage-1
           enddo
        else
            do ii=1, kids-1
             call mpi_bcast(iii,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
             if (iii.eq.mytask) then
              call send_page_info(mytask,ipage,ipage_start)
             endif
            enddo
        endif
        call mpi_bcast(ipage_last,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
        total_page= ipage_last + 1
	ipage_first=0
C*******OVERLAY
           itrouble = 0
           do iii=ipage_first, ipage_last
              call mpi_bcast(itrouble,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
              if (itrouble.eq.1) goto  8888
              if (mytask.eq.0) then
                  do inode=0, kids-1
                     if (inode.eq.0)   inew_page=1
                     if (inode.eq.0) then
                       curr_ipage       = ipage 
                       curr_ipage_start=  ipage_start
                     else
                       call mpi_bcast(inode,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
                       call receive_page_info(inode,curr_ipage,curr_ipage_start)
                     endif
                     call inode_has_page(curr_ipage,curr_ipage_start,iii,ipage_from_disk)
                     write(6,*)'looking at page ',ipage_from_disk,' from ',inode
                     if (ipage_from_disk.ne. -1) then
                       call load_receive(inode,buff6,buff7,file_mask,file_data,ipage_from_disk,sam_ac2)
                       call image_combine(overlay_image,overlay_mask,buff6,buff7,sam_ac2,inew_page)
                     endif
                  enddo 
                  call image_combine_dump(overlay_image,overlay_mask,sam_ac2,file_image_dump,
     *                            iii,norm_look_mode,v_filler,
     *                            ct_profile,c1_pxl,c2_pxl,
     *                            fm_sizec2_in,ct_prof_len,l1_ct_pro,
     *                            l2_ct_pro,istatus,file_mask_dump)
                  write(6,*)'DONE PAGE # ',iii
                  if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) then
                   itrouble= 1
                  endif
              else
                 do inode=1, kids-1
                     call mpi_bcast(ik,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
                     if (ik.eq.mytask) then
                       call send_page_info(mytask,ipage,ipage_start)
                       call inode_has_page(ipage,ipage_start,iii,ipage_from_disk)
                       if (ipage_from_disk.ne. -1) then
                         call load_send(buff6,buff7,file_mask,file_data,ipage_from_disk,sam_ac2)
                       endif
                     endif
                 enddo
              endif
                 
                  

           enddo
C*******OVERLAY
8888    continue
        call mpi_bcast(istatus,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
        if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return

        call mpi_bcast(ct_prof_len,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
        call mpi_bcast(ct_profile ,sam_ac,MPI_REAL8,0,MPI_COMM_WORLD,ierr)


        if (ire_sim.eq.1) then
          if(ct_profile_mode.eq.1) then
            call get_ct_prof(
     *          ct_prof_len,l1_ct_pro,l2_ct_pro,c1_pxl,
     *          ct_profile        )
          else
             call flat_ct_prof(ct_profile,sam_ac2)
          end if

        endif



        if (ire_sim.eq.1.and.prod_type.eq.prod_1) then 
c          if (mytask.eq.0) then
c           call ct_inten_norm(scale_im,file_image_dump,v_filler,c1_pxl,ct_profile,istatus)
c          endif
c          call mpi_bcast(istatus,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
c          if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
c          call mpi_bcast(scale_im,1,MPI_REAL,0,MPI_COMM_WORLD,ierr)
           scale_im= .8
C********************
        else
            scale_im= .8
        endif
C********************
        if (mytask.eq.0) then
         call read_ac_image(scale_im,
     *  file_image_dump,proc_gain,ac_image,sam_ac2,total_page,istatus)
        endif
        call mpi_bcast(istatus,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
        if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
        call mpi_bcast(ac_image,sam_ac*sam_ac,MPI_BYTE,0,MPI_COMM_WORLD,ierr)



         tb = rtc()
         write(6,*)'setup & overlay elapsed time', tb-ta

        call post_proc(
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
        if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return


        call dump_post_to_ceos(file_post_to_ceos,
     *  nlines,nsamples,p1_fm,p2_fm,
     *  zone,slat,along0,plat1,plat2,lat_orig,long_orig,istatus)
        if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return


        if (mytask.eq.0)idummy=printfstat(file_status,4,100.0)
         

        return

        END
