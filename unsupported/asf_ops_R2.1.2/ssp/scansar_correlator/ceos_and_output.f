c SccsId = @(#)ceos_and_output.f	2.41 3/24/98
       subroutine ceos_and_output(file_config,istatus,kids,mytask,subsystem_name)
       implicit none

        character*128 SccsId_ceos_and_output
        data SccsId_ceos_and_output
     +  /'@(#)PPceos_and_output.f:2.41'/

c************************************************
        include 'ssp2_const.inc'
        character*256 file_config
        character*256 file_status
        integer      kids,mytask,istatus
c************************************************
       real*8     c1_pxl
       real*8     c2_pxl
       character*60     file_pj_image
       character*60     file_framelet(4)
       character*60     file_pmflet(4)
       real*8          gain_rec(burst_max)
       real*8          plen
       real*8          proc_gain
       integer*4     nbeams
       integer*4     beam_mode
C************************
       character*60  subsystem_name
       character*60  file_ceos_leader,file_image,file_pmf,file_cali
       character*60  file_scan_results
       character*60  file_pvs_look
       character*60  file_framelet_leader(4)
       integer       n_single_look
       real*8        avg_terrain
       real*8          gha_time
       real*8          gha
       integer*4       orbit_num
c****************************
       real*8     prf(burst_max)
       integer*4     np(burst_max)
       integer*4     np_air(burst_max)
       integer*4     burst_start
       integer*4     burst_end
       real*8     chirp_rate
       real*8     fs
       real*8     x_sc(burst_max)
       real*8     y_sc(burst_max)
       real*8     z_sc(burst_max)
       real*8     v_x_sc(burst_max)
       real*8     v_y_sc(burst_max)
       real*8     v_z_sc(burst_max)
       real*8     c1_flm(2,2,burst_max)
       real*8     c2_flm(2,2,burst_max)
       real*8     fr_near(burst_max)
       real*8     fr_far(burst_max)
       real*8     fd_low_far(burst_max)
       real*8     fd_low_near(burst_max)
       real*8     i_ang_bip(burst_max)
       real*8     look_ang(burst_max)
       real*8     r_low(burst_max)
       real*8     r_high(burst_max)
       real*8     chirp_bw
       integer    hist_i(256),hist_q(256)
       real*8     roll_o(burst_max)
       real*8     yaw_o(burst_max)
       real*8     roll(burst_max)
       real*8     yaw(burst_max)
       real*8     pitch_o(burst_max)
       real*8     pitch(burst_max)
       real*8     yaw_rate(burst_max)
       real*8     roll_rate(burst_max)
       real*8     pitch_rate(burst_max)
       integer    pvs_node
       integer    inp_frame_id
       character*40  acq_time
       real*8     p_x(burst_max)
       real*8     p_y(burst_max)
       real*8     p_z(burst_max)
       real*8     t_burst(burst_max)
c
       integer nbuf(4),dontcare,allmsg,nulltask,allgrp
       integer*4       prod_type
       character*60  file_ceos_template
       integer*4    idummy,printfstat
c
       integer    proj
       real*8     p1_fm(2,2)
       real*8     p2_fm(2,2)
       real*8     p1_pxl
       real*8     p2_pxl
       real*8     c1_frm(2,2)
       real*8     c2_frm(2,2)
       integer    zone
       real*8     slat,along0,plat1,plat2,lat_orig,long_orig
       character*60 file_pmf_template
       real*8     alpha1,alpha2,alpha3

      integer np_v(burst_max),sv_type
      real*8 peak_bm_ang(4)

      integer ter_correct
      real*8 sv_x_pos,sv_y_pos,sv_z_pos

      real*8 sv_ref_time
      integer ldr_rec_cnt
      integer ldr_max_rec_len
      real*8 pixel_spacing
      character*60 calib_fname
      integer min,nlines,nsamples,i,orbit_no,nbins
      integer npix,linec
      real*8 c1_g(3,3,n_az,n_rg,burst_max)               ! 12-24-95 JMS
      real*8 c2_g(3,3,n_az,n_rg,burst_max)               ! 12-24-95 JMS
      real*8 c2_cell(burst_max),c1size_blk(burst_max)    ! 12-24-95 JMS
      integer n_c2_blk(burst_max)                        ! 12-24-95 JMS
       integer nburst_spec        ! Added 1-4-96 JMS for range spectra
       complex buff_spec(8192)    ! Added 1-4-96 JMS for range spectra
       real*8  noise_flr_def
       real*8  r_bip(burst_max)
       real*8  coef_fd(4,burst_max)
       real*8          sv_x_vel
       real*8          sv_y_vel
       real*8          sv_z_vel
       integer         rgc_est_mode
       integer         fdc_est_mode
       integer         res_prod
       integer ns(burst_max)                       ! JMS 2-8-96
       real*8 ps_lat,ps_lon,lamb_lat_n,lamb_lat_s      ! JMS 2-8-96
c  Note window_start_time merely redimensioned
       real*8 window_start_time(1600),replica_agc(1600)  ! JMS 2-14-96
       real*8 lna_temp(1600),subsystem_temp(1600)   
       real*8 protector_temp(1600),rx_agc(1600)
       character*64 sitename

       real*8 cal_sys_temp(1600),repl_pow                       ! from auxiliary data
       character*64 sw_ver                                      ! internally stored
       character*60    RDS_ver
       character*64 media_id,media_type                         ! in frame request
       character*64 media_loc,prod_id                           ! in frame request
       character*60 file_topo                                   ! already read in in get_config
       character*256     file_out_ldr_flm(4)                     ! 4-4-96
       character*256     file_out_img_flm(4)
       real*4           ber
       integer          subframe_id
C***NEW GUYS
       integer utm_zone               ! already in get_config
       character*10 aux_yaw_valid(1600)                   ! 5-10-96
       character*10 aux_roll_valid(1600)                  ! 5-10-96
       character*10 aux_pitch_valid(1600)                 ! 5-10-96
       real*8 r_cntr(burst_max)
       real*8 topo_spacing            ! already in get_config
       integer dem_ellip              ! already in get_config
       real*8 t_b(burst_max)                              ! 5-15-96
       character*256 cali_file,scan_results_file          ! 9-10-96
C***NEW GUYS



c  Internal values
       real*4     value(256)
       integer    year,doy,hr
       integer    dk_ptr
       integer    pro_hist(256)
       real*4     minfreq,maxfreq,meansamp,stdvsamp,meanfreq,stdvfreq
       real*4     minfreq_i,maxfreq_i,meansamp_i,stdvsamp_i
       real*4     meanfreq_i,stdvfreq_i
       real*4     minfreq_q,maxfreq_q,meansamp_q,stdvsamp_q
       real*4     meanfreq_q,stdvfreq_q
       integer    maxlines
       parameter  (maxlines=15554)
       integer    left_fill_cnt(maxlines)
       integer    image_pix_cnt(maxlines)
       integer    right_fill_cnt(maxlines)
       real*4     slr_first(maxlines)
       real*4     slr_mid(maxlines)
       real*4     slr_last(maxlines)
       real*4     lat_first(maxlines)
       real*4     lat_mid(maxlines)
       real*4     lat_last(maxlines)
       real*4     long_first(maxlines)
       real*4     long_mid(maxlines)
       real*4     long_last(maxlines)
       byte       byte_data(sam_post,np_geo,4)
       character*60  ldr_fname
       character*60  out_pname
       real*8     fsec
       integer    msecs
       byte       look_no
c
       integer    proc_id
       integer    c_transfer_back,isuccess
       character*256 pmf_file,ceos_leader_file,image_file
       integer    nbytes
       integer    ierr
c
       integer act_lines
       integer act_pix
       character*16 ceos_fname
       real*4 sc_head      !  changed to real*4 12-24-95 JMS ***************************
**
       integer kburst

       include 'scan.inc'
       integer job_id
       character*64 platform,sensor
      integer rev,sequence,strlen
      character*64 act_id,media_id_dum,media_type_dum
      character*64 media_loc_dum
      character*64 recorder_id,station_id
      character*64 frame_mode,site_name
      integer seg_id,start_addr,end_addr
      integer status
      integer npix_max      ! JMS 2-6-96
      real*8 pre_cal1,pre_cal2,post_cal1,post_cal2
      character*64 sv_prec

      real *8 snr,radio_acc                         ! JMS 3-17-96
      character*60 pmf_fname                        ! JMS 3-18-96
      character*256   leader_flm                     ! JMS 4-4-96
      character*256   image_flm                      ! JMS 4-4-96
      integer ant_flag
C**NEW GUYS*
      integer*2 topo(ns_ew,ns_ns)
      real*8 dlon_topo,dlat_topo
      integer nbyte,ipt
      real*8 rlocal_mean                             !  JMS 6-10-96
C**NEW GUYS*
      character*60 scan_vers                         !  JMS 9-10-96
      real*8 p1size_frm2,p2size_frm2                 !  JMS 9-26-96



      write (*,*) ' HELLO in ceos_and_output'
      write (*,*) ' subsystem_name = ',subsystem_name

        allgrp = MPI_COMM_WORLD

       call  load_ceos_parameters(file_config,istatus,kids,mytask,
     * file_status,
     * file_pmf,file_scan_results,file_cali,
     * file_pvs_look,file_pj_image,
     * file_image,file_ceos_leader,
     * file_framelet,file_framelet_leader,
     * nlines,nsamples,acq_time,inp_frame_id,prf,
     * np,np_air,burst_start,burst_end,window_start_time,
     * roll_rate,yaw_rate,pitch_rate,chirp_rate,fs,plen,gain_rec,
     * avg_terrain,proc_gain,orbit_num,x_sc,y_sc,z_sc,v_x_sc,
     * v_y_sc,v_z_sc,
     * pitch,yaw,roll,pitch_o,yaw_o,roll_o,nbeams,c1_pxl,c2_pxl,
     * c1_flm,c2_flm,fd_low_near,fd_low_far,fr_near,fr_far,i_ang_bip,
     * look_ang,beam_mode,gha,gha_time,r_low,r_high,chirp_bw,
     * hist_i,hist_q,
     * n_single_look,pvs_node,p_x,p_y,p_z,t_burst,prod_type,
     * file_ceos_template,proc_id,
     * pmf_file,ceos_leader_file,image_file,
     * p1_fm,p2_fm,p1_pxl,p2_pxl,c1_frm,c2_frm,
     * proj,zone,slat,along0,
     * plat1,plat2,lat_orig,long_orig,
     * np_v,
     * peak_bm_ang,
     * sv_type,ter_correct,
     * sv_x_pos,sv_y_pos,sv_z_pos,
     * sv_ref_time,
     * file_pmf_template,
     * alpha1,alpha2,alpha3,c1_g,c2_g,c2_cell,c1size_blk,
     * n_c2_blk,nburst_spec,buff_spec,     ! JMS 1-4-96 added last two values
     * noise_flr_def,r_bip,coef_fd,
     * sv_x_vel,sv_y_vel,sv_z_vel,fdc_est_mode,rgc_est_mode,res_prod,
     * ns,ps_lat,ps_lon,lamb_lat_n,lamb_lat_s,
     * replica_agc,lna_temp,subsystem_temp,
     * protector_temp,rx_agc,sitename,
C*******NEW GUYS
C*******  Cal_sys_temp and repl_pow are in auxiliary data
C*******  sw_ver is software version wherever you keep it
C*******  all other new guys are in frame request
     * cal_sys_temp,repl_pow,sw_ver,prod_id,media_id,media_type,media_loc,
     * job_id,platform,sensor,rev,sequence,act_id,recorder_id,
     * station_id,frame_mode,sv_prec,file_pmflet,
     * file_out_ldr_flm,file_out_img_flm,ber,subframe_id,ant_flag,
     * utm_zone,aux_yaw_valid,aux_roll_valid,aux_pitch_valid,
     * r_cntr,topo_spacing,dem_ellip,t_b,RDS_ver,cali_file,
     * scan_results_file)
C*******NEW GUYS

       write (*,*) ' HELLO back from load_ceos_parameters'
       write (*,*) 'along0 = ',along0
       write (*,*) 'p1_fm(1,1),p2_fm(1,1) = ',p1_fm(1,1),p2_fm(1,1)
       write (*,*) 'p1_fm(1,2),p2_fm(1,2) = ',p1_fm(1,2),p2_fm(1,2)
       write (*,*) 'p1_fm(2,1),p2_fm(2,1) = ',p1_fm(2,1),p2_fm(2,1)
       write (*,*) 'p1_fm(2,2),p2_fm(2,2) = ',p1_fm(2,2),p2_fm(2,2)
       write (*,*) 'PROJ = ',proj
       if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return

c***   JEFFS CODE HERE
      nbytes=np_geo*sam_post*nbeams
      dk_ptr=0

      if ((prod_type.eq.prod_pvs_3).or.(prod_type.eq.prod_pvs_2)) then
       if (pvs_node.eq.mytask) then
        call read_disk(file_pvs_look,byte_data,nbytes,dk_ptr,istatus)
       endif
       call mpi_bcast(istatus,1,MPI_INTEGER,pvs_node,MPI_COMM_WORLD,ierr)
       if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
      endif

      nbytes=np_geo*sam_post*4
      call mpi_bcast(byte_data,nbytes,MPI_BYTE,pvs_node,MPI_COMM_WORLD,ierr)


      IF (mytask.eq.0)  THEN
c***   JEFFS CODE HERE

      write (*,*) 'JEFFS in main'
      write (*,*) ' alpha1 = ',alpha1
      write (*,*) ' alpha2 = ',alpha2
      write (*,*) ' alpha3 = ',alpha3
      write (*,*) 'file_cali = ',file_cali
      write (*,*) 'file_image = ',file_image
      write (*,*) 'image_file = ',image_file
      write (*,*) 'ceos_leader_file = ',ceos_leader_file
      write (*,*) 'file_pmf_template = ',file_pmf_template
      write (*,*) 'rev = ',rev
      write (*,*) 'sequence = ',sequence
c  Main image

      write (*,*) 'hello 2'

      strlen=64
      call get_frame_id(image_file,inp_frame_id,status)

c  JMS new 5-13-96
      if (dem_ellip.eq.dem) then
        ipt=0
        nbyte=ns_ew*ns_ns
        write (*,*) 'read topo'
        write (*,*) 'file_topo = ',file_topo
        call read_disk(file_topo,topo,nbyte,ipt,istatus)
      endif
      if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return

      write (*,*) 'hello 2.5'
      call getscandata(inp_frame_id,file_scan_results,status,
     *seg_id,styr,
     *stdoy,sthr,stmin,stsec,stusec,edyr,eddoy,edhr,edmin,edsec,
     *edusec,start_addr,end_addr,
     *ascdes,pre_cal1,pre_cal2,
     *post_cal1,post_cal2,scan_vers,istatus)
      if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return

      write (*,*) 'hello 3'
c  JMS new 6-10-96
      rlocal_mean=sqrt((x_sc(1)**2+y_sc(1)**2+z_sc(1)**2)/
     *((x_sc(1)**2+y_sc(1)**2)/re**2+(z_sc(1)**2)/rp**2))
      write (*,*) 'x_sc(1) = ',x_sc(1)
      write (*,*) 'y_sc(1) = ',y_sc(1)
      write (*,*) 'z_sc(1) = ',z_sc(1)
      write (*,*) 'rlocal_mean = ',rlocal_mean

      dlat_topo=topo_spacing
      dlon_topo=topo_spacing

c  10-1-96
c      along0=45.0d0

      call ceos_image(file_pj_image,file_image,image_file,nlines,
     *nsamples,left_fill_cnt,image_pix_cnt,right_fill_cnt,
     *pro_hist,minfreq,maxfreq,meansamp,stdvsamp,meanfreq,
     *stdvfreq,linec,sc_head,act_lines,res_prod,p1_fm,p2_fm,
     *c1_frm,c2_frm,proj,prod_type,ceos_fname,
     *zone,slat,along0,plat1,plat2,lat_orig,long_orig,
     *alpha1,alpha2,alpha3,ter_correct,burst_end,nbeams,gha,
     *sv_ref_time,sv_x_pos,sv_y_pos,sv_z_pos,sv_x_vel,sv_y_vel,
     *sv_z_vel,t_burst,r_bip,coef_fd,gha_time,dem_ellip,
     *dlon_topo,dlat_topo,topo,avg_terrain,rlocal_mean,istatus)
      if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return

      write (*,*) 'hello 4'

      npix_max=0
      do 47 i=1,nlines
      if (npix_max.lt.image_pix_cnt(i)) npix_max=image_pix_cnt(i)
   47 continue
      call ld_im_hist(act_lines,npix_max,minfreq,maxfreq,
     *  meansamp,stdvsamp,meanfreq,stdvfreq,pro_hist)

      nbins=256
      do 20 i=1,nbins
      value(i)=float(i-129)
   20 continue

      write (*,*) 'hello 5'
      call hist_stat(nbins,hist_i,value,minfreq_i,maxfreq_i,
     *meansamp_i,stdvsamp_i,meanfreq_i,stdvfreq_i)

      write (*,*) 'hello 6'
      call hist_stat(nbins,hist_q,value,minfreq_q,maxfreq_q,
     *meansamp_q,stdvsamp_q,meanfreq_q,stdvfreq_q)

      write (*,*) 'hello 7'


      call ld_iq_hist(minfreq_i,maxfreq_i,meansamp_i,stdvsamp_i,
     *meanfreq_i,stdvfreq_i,hist_i,minfreq_q,maxfreq_q,meansamp_q,
     *stdvsamp_q,meanfreq_q,stdvfreq_q,hist_q,np_v,burst_start,
     *burst_end,ns)

      write (*,*) 'hello 8'
c  JMS 1-4-96
      call ld_r_spec(nburst_spec,buff_spec,fs)

      write (*,*) 'hello 9'
      write (*,*) 'file_ceos_template = ',file_ceos_template
      write (*,*) 'proc_gain = ',proc_gain

      call get_ldr(nlines,nsamples,left_fill_cnt,image_pix_cnt,
     *right_fill_cnt,file_pj_image,file_scan_results,inp_frame_id,
     *file_ceos_leader,sc_head,
     *ceos_fname,orbit_no,act_lines,linec,file_cali,
     *file_pmf,ldr_rec_cnt,ldr_max_rec_len,chirp_rate,fs,plen,
     *burst_start,burst_end,prf,gain_rec,nbeams,np,np_v,
     *avg_terrain,proc_gain,p1_pxl,p2_pxl,peak_bm_ang, gha,
     *sv_type,c1_frm,c2_frm,ter_correct,x_sc,y_sc,z_sc,
     *v_x_sc,v_y_sc,v_z_sc,sv_x_pos,sv_y_pos,sv_z_pos,t_burst,
     *window_start_time,np_air,roll_rate,yaw_rate,pitch_rate,
     *pitch_o,yaw_o,roll_o,pitch,yaw,roll,c1_pxl,c2_pxl,c1_flm,
     *c2_flm,fd_low_near,fd_low_far,fr_near,fr_far,i_ang_bip,
     *look_ang,sv_ref_time,r_low,r_high,p_x,p_y,p_z,chirp_bw,
     *beam_mode,file_ceos_template,file_pmf_template,alpha1,alpha2,
     *alpha3,noise_flr_def,meansamp,stdvsamp,r_bip,coef_fd,
     *sv_x_vel,sv_y_vel,sv_z_vel,fdc_est_mode,rgc_est_mode,
     *res_prod,proj,ps_lat,ps_lon,lamb_lat_n,lamb_lat_s,
     *replica_agc,lna_temp,subsystem_temp,
     *protector_temp,rx_agc,sitename,cal_sys_temp,repl_pow,
     *sw_ver,image_file,ceos_leader_file,prod_id,media_id,
     *media_type,media_loc,job_id,platform,sensor,rev,sequence,
     *act_id,recorder_id,station_id,frame_mode,
     *sv_prec,prod_type,snr,radio_acc,subframe_id,ber,ant_flag,
     *gha_time,utm_zone,aux_yaw_valid,aux_roll_valid,
     *aux_pitch_valid,r_cntr,dem_ellip,dlon_topo,dlat_topo,
     *topo,t_b,rlocal_mean,plat1,plat2,slat,along0,RDS_ver,
     *cali_file,scan_results_file,subsystem_name,p1_fm,p2_fm,
     *zone,lat_orig,long_orig,istatus)
      if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return

      write (*,*) 'hello 10'

      IF ((prod_type.eq.prod_pvs_2).or.(prod_type.eq.prod_pvs_3)) 
     * THEN
C**************************PVS RELATED ONLY**
c  Framelet images ******************************************

      nlines=256
      npix=3072
      nbytes=nlines*npix
      do 100 i=1,nbeams
      out_pname=file_framelet(i)
      ldr_fname=file_framelet_leader(i)
      pmf_fname=file_pmflet(i)
      leader_flm=file_out_ldr_flm(i)
      image_flm=file_out_img_flm(i)
      dk_ptr=(i-1)*nbytes

      write (*,*) 'hello 11'
      write (*,*) 'hello 10'
      call framelet_image(i,byte_data,out_pname,image_flm,nlines,
     *npix,year,doy,msecs,pro_hist,
     *minfreq,maxfreq,meansamp,stdvsamp,meanfreq,stdvfreq,
     *ceos_fname,act_lines,act_pix,istatus)
      if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return

      write (*,*) 'hello 12'
      call ld_im_hist(act_lines,act_pix,minfreq,maxfreq,
     *  meansamp,stdvsamp,meanfreq,stdvfreq,pro_hist)

      write (*,*) 'hello 13'
      kburst=i


      call get_ldr_flm(nlines,kburst,
     *file_scan_results,inp_frame_id,ldr_fname,
     *sc_head,
     *ceos_fname,orbit_no,act_lines,act_pix,chirp_rate,
     *fs,plen,burst_start,burst_end,
     *prf,gain_rec,nbeams,np,np_v,avg_terrain,proc_gain,
     *peak_bm_ang,gha,sv_type,c1_frm,c2_frm,ter_correct,
     *x_sc,y_sc,z_sc,v_x_sc,v_y_sc,v_z_sc,
     *sv_x_pos,sv_y_pos,sv_z_pos,t_burst,window_start_time,
     *np_air,roll_rate,yaw_rate,pitch_rate,pitch_o,
     *yaw_o,roll_o,pitch,yaw,roll,
     *c1_pxl,c2_pxl,c1_flm,c2_flm,fd_low_near,
     *fd_low_far,fr_near,fr_far,i_ang_bip,look_ang,
     *sv_ref_time,r_low,r_high,p_x,p_y,p_z,
     *chirp_bw,beam_mode,
     *file_ceos_template,alpha1,alpha2,alpha3,
     *c1_g,c2_g,c2_cell,c1size_blk,n_c2_blk,r_bip,coef_fd,
     *sv_x_vel,sv_y_vel,sv_z_vel,fdc_est_mode,rgc_est_mode,
     *res_prod,replica_agc,lna_temp,
     *subsystem_temp,protector_temp,rx_agc,cal_sys_temp,
     *repl_pow,pmf_fname,file_pmf_template,sitename,sw_ver,
     *image_flm,leader_flm,proj,prod_id,media_id,
     *media_type,media_loc,job_id,platform,sensor,rev,sequence,
     *act_id,recorder_id,station_id,frame_mode,sv_prec,
     *prod_type,npix,snr,radio_acc,subframe_id,ber,
     *gha_time,aux_yaw_valid,aux_roll_valid,aux_pitch_valid,
     *r_cntr,dem_ellip,dlon_topo,dlat_topo,topo,t_b,rlocal_mean,
     *istatus)
      if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return

  100 continue



C**************************PVS RELATED ONLY**
      ENDIF
      isuccess= c_transfer_back(pmf_file,ceos_leader_file,image_file,
     *                file_pmf,file_ceos_leader,file_image,
     *                file_framelet_leader,file_framelet,
     *                prod_type,file_config,nbeams,proc_id,file_pmflet)

      ENDIF
 1000  format (i4,1x,i3,1x,i2,1x,i2,1x,f6.3)
      if (mytask.eq.0) idummy=printfstat(file_status,5,100.0)



       return
       end
       subroutine  load_ceos_parameters(file_config,istatus,kids,mytask,
     * file_status,
     * file_pmf,file_scan_results,file_cali,
     * file_pvs_look,file_final_image,
     * file_image,file_ceos_leader,
     * file_framelet,file_framelet_leader,
     * nlines,nsamples,acq_time,inp_frame_id,prf,
     * np,np_air,burst_start,burst_end,window_start_time,
     * roll_rate,yaw_rate,pitch_rate,chirp_rate,fs,plen,gain_rec,
     * avg_terrain,proc_gain,orbit_num,x_sc,y_sc,z_sc,v_x_sc,
     * v_y_sc,v_z_sc,
     * pitch,yaw,roll,pitch_o,yaw_o,roll_o,nbeams,c1_pxl,c2_pxl,
     * c1_flm,c2_flm,fd_low_near,fd_low_far,fr_near,fr_far,i_ang_bip,
     * look_ang,beam_mode,gha,gha_time,r_low,r_high,chirp_bw,
     * hist_i_avg,hist_q_avg,
     * n_single_look,pvs_node,p_x,p_y,p_z,t_burst,prod_type,
     * file_ceos_template,proc_id,
     * pmf_file,ceos_leader_file,image_file,
     * p1_fm,p2_fm,p1_pxl,p2_pxl,c1_frm,c2_frm,
     * proj,zone,slat,along0,
     * plat1,plat2,lat_orig,long_orig,
     * np_v,
     * peak_bm_ang,
     * sv_type,ter_correct,
     * sv_x_pos,sv_y_pos,sv_z_pos,
     * sv_ref_time,
     * file_pmf_template,
     * alpha1,alpha2,alpha3,c1_g,c2_g,c2_cell,c1size_blk,
     * n_c2_blk,nburst_spec,buff_spec,
     * noise_flr_def,r_bip,coef_fd,
     * sv_x_vel,sv_y_vel,sv_z_vel,fdc_est_mode,rgc_est_mode,res_prod,
     * ns,ps_lat,ps_lon,lamb_lat_n,lamb_lat_s,                               ! JMS 2-8-96
     * replica_agc,lna_temp,subsystem_temp,
     * protector_temp,rx_agc,sitename,                                       ! JMS 2-14-96
C*******NEW GUYS
     * cal_sys_temp,repl_pow,sw_ver,                                         ! JMS 2-24-96
     * prod_id,media_id,media_type,media_loc,                                ! JMS 2-26-96
     * job_id,platform,sensor,rev,sequence,act_id,recorder_id,               ! JMS 2-28-96
     * station_id,frame_mode,sv_prec,file_pmflet,
     * framelet_leader_file,framelet_file,ber,subframe_id,ant_flag,
     * utm_zone,aux_yaw_valid,aux_roll_valid,aux_pitch_valid,
     * r_cntr,topo_spacing,dem_ellip,t_b,RDS_ver,
     * cali_file,scan_results_file)                    ! these two added JMS 9-10-96
C*******NEW GUYS

     
       include 'ssp2_const.inc'
       character*60 file_w1_dyn,file_w2_dyn,file_w3_dyn,file_s5_dyn,file_s6_dyn,file_s7_dyn,file_az_dyn
       integer          ant_flag,subframe_id
       real*8           bit_error_rate
       real*4           ber
       character*256   framelet_file(4),framelet_leader_file(4)
       integer    proj
       real*8     p1_fm(2,2)
       real*8     p2_fm(2,2)
       real*8     p1_pxl
       real*8     p2_pxl
       real*8     c1_frm(2,2)
       real*8     c2_frm(2,2)
       integer    zone
       real*8     slat,along0,plat1,plat2,lat_orig,long_orig
       character*60 file_pmf_template
       real*8     alpha1,alpha2,alpha3
      integer np_v(burst_max),sv_type
      integer ter_correct
      real*8 c1_g(3,3,n_az,n_rg,burst_max)               ! 12-24-95 JMS
      real*8 c2_g(3,3,n_az,n_rg,burst_max)               ! 12-24-95 JMS
      real*8 c2_cell(burst_max),c1size_blk(burst_max)    ! 12-24-95 JMS
      integer n_c2_blk(burst_max)                        ! 12-24-95 JMS
      integer nburst_spec        ! Added 1-4-96 JMS for range spectra
      complex buff_spec(8192)    ! Added 1-4-96 JMS for range spectra
      real*8  noise_flr_def
      real*8     r_bip(burst_max)
      real*8     coef_fd(4,burst_max)
       integer ns(burst_max)            ! JMS 2-8-96
       real*8 window_start_time(1600),replica_agc(1600)  ! JMS 2-14-96
       real*8 lna_temp(1600),subsystem_temp(1600)
       real*8 protector_temp(1600),rx_agc(1600)
       character*64 sitename
C***NEW GUYS
       real*8 cal_sys_temp(1600),repl_pow                ! JMS 2-24-96
       character*64 sw_ver
       character*64 prod_id,media_id                     ! JMS 2-26-96
       character*64 media_type,media_loc
       character*64 platform,sensor,act_id,recorder_id               ! JMS 2-28-96
       character*64 station_id,frame_mode
       character*64 site_name,sv_prec
       integer rev,sequence
C***NEW GUYS

       integer     proc_id
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
       character*60     file_topo
       character*60     file_image_dump
       character*60     file_mask_dump
       character*60     file_pj_image
       character*60     file_pj_image_low
       character*60     file_frame
       character*60     file_framelet(4)
       character*60     file_ceos_template
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
       character*(*)    file_config
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
       character*(*) file_status
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
c****************************
       real*8     prf(burst_max)
       integer*4     np(burst_max)
       integer*4     np_air(burst_max)
       real*8     chirp_rate
       real*8     fs
       real*8     x_sc(burst_max)
       real*8     y_sc(burst_max)
       real*8     z_sc(burst_max)
       real*8     v_x_sc(burst_max)
       real*8     v_y_sc(burst_max)
       real*8     v_z_sc(burst_max)
       real*8     c1_flm(2,2,burst_max)
       real*8     c2_flm(2,2,burst_max)
       real*8     fr_near(burst_max)
       real*8     fr_far(burst_max)
       real*8     fd_low_far(burst_max)
       real*8     fd_low_near(burst_max)
       real*8     i_ang_bip(burst_max)
       real*8     look_ang(burst_max)
       real*8     r_low(burst_max)
       real*8     r_high(burst_max)
       real*8     chirp_bw
       integer hist_i_avg(256),hist_q_avg(256)
       real*8     roll_o(burst_max)
       real*8     yaw_o(burst_max)
       real*8     roll(burst_max)
       real*8     yaw(burst_max)
       real*8     pitch_o(burst_max)
       real*8     pitch(burst_max)
       real*8     yaw_rate(burst_max)
       real*8     roll_rate(burst_max)
       real*8     pitch_rate(burst_max)
       real*8     yaw_o_rate(burst_max)
       real*8     roll_o_rate(burst_max)
       real*8     pitch_o_rate(burst_max)
       integer    pvs_node
       integer    inp_frame_id
       character*40  acq_time
       real*8     p_x(burst_max)
       real*8     p_y(burst_max)
       real*8     p_z(burst_max)
       real*8     t_burst(burst_max)
       integer*4       sim_eam_nsp(4),burst_start,burst_end
       real*8          sv_x_pos
       real*8          sv_y_pos
       real*8          sv_z_pos
       real*8          sv_x_vel
       real*8          sv_y_vel
       real*8          sv_z_vel
       character*60    file_final_image
       integer*4    idummy,printfstat,gen_frameletnames
       character*60    product_id
       character*60     file_pmflet(4)
       integer         prf_num
       character*60    file_az
       real*8          r_cntr(burst_max)
       character*10    aux_yaw_valid(burst_max)
       character*10    aux_roll_valid(burst_max)
       character*10    aux_pitch_valid(burst_max)
       real*8          t_b(burst_max)
       integer         read_1st_auxline
       character*60    RDS_ver



       noise_flr_def= -20.0d0
       sw_ver= 'VERS2.41'



        job_mode=4

       write(6,*)'MY TASK IS ',mytask
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
     *  file_aux,file_dk,file_dk_temp,file_mask_dump,file_ceos_leader,
     *  file_image,
     *  file_pmf,file_cali,file_eph,file_rep,file_scan_results,
     *  snr,burst_good,sv_ref_time,gain_rec,blk_gain,topo_spacing,
     *  plen,nr,prod_type,file_pvs_beam,grid_ptn_flag,proc_gain,
     *  job_mode,echo_file,aux_file,rep_file,eph_file,cali_file,pmf_file,
     *  ceos_leader_file,image_file,scan_results_file,burst_loc_file,
     *  topo_file,
     *  fm_sizec1,fm_sizec2,fm_sizec1_in,fm_sizec2_in,
     *  nbeams,bm_id,rng_bm_ang,peak_bm_ang,
     *  ele_ang_eq,az_peak_coef,proc_mode,lamb_lat_n,lamb_lat_s,
     *  ps_lat,ps_lon,utm_zone,file_pvs_look,file_framelet_leader,
     * n_single_look,
     *  avg_terrain,gha,gha_time,orbit_num,
     *  file_pre_to_ceos,file_corr_to_ceos,file_post_to_ceos,
     *  file_ceos_template,
     *  burst_start,burst_end,sim_eam_nsp,
     *  sv_x_pos,sv_y_pos,sv_z_pos,sv_x_vel,sv_y_vel,sv_z_vel,fs,
     *  file_status,proc_id,
     *  product_id,sv_type,file_pmf_template,ter_correct,nburst_spec,
     *  sitename,
     *  platform,sensor,rev,sequence,recorder_id,act_id,station_id,
     *  frame_mode,sv_prec,media_id,media_type,media_loc,file_pmflet,
     *  file_w1_dyn,file_w2_dyn,file_w3_dyn,file_s5_dyn,file_s6_dyn,file_s7_dyn,
     *  file_az_dyn,ant_flag,subframe_id,prf_num,file_az)

        write (*,*) 'HELLO back from get_config'


        job_id =proc_id
        prod_id=product_id

        write(6,*)'STATUS',istatus
        if (res_prod.eq.600) then
         file_final_image=file_pj_image_low
        else
         file_final_image=file_pj_image
        endif

        if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
        istatus=read_1st_auxline(file_aux,RDS_ver)
        if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
        write (*,*) 'HELLO back from get_config'
        if (mytask.eq.0) idummy=printfstat(file_status,5,0.0)
        write (*,*) 'HELLO back from printfstat'

        call load_corr_to_ceos(file_corr_to_ceos,
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
     *  replica_agc,lna_temp,subsystem_temp,protector_temp,
     *  rx_agc,istatus,cal_sys_temp,repl_pow,bit_error_rate,gain_rec,
     *  r_cntr,aux_yaw_valid,aux_roll_valid,aux_pitch_valid,t_b)
        ber=bit_error_rate

        write (*,*) 'HELLO back from load_corr_to_ceos'
        if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
        write (*,*) 'HELLO back from load_corr_to_ceos'
        


        
        call load_post_to_ceos(file_post_to_ceos,
     *  nlines,nsamples,p1_fm,p2_fm,zone,slat,along0,plat1,
c     *  nlines,nsamples,p1_fm,p2_fm,zone,slat,along,plat1,
     *  plat2,lat_orig,long_orig,istatus)
        write (*,*) 'HELLO back from load_post_to_ceos'
        if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
        write (*,*) 'HELLO back from load_post_to_ceos'
        write (*,*) 'along0 = ',along0

        
       do i=1,burst_max
         yaw_rate(i)=yaw_o_rate(i)
         roll_rate(i)=roll_o_rate(i)
         pitch_rate(i)=pitch_o_rate(i)
       enddo

       idummy=gen_frameletnames(nbeams,framelet_file,framelet_leader_file,image_file,
     *                          ceos_leader_file)
        write (*,*) 'HELLO back from gen_frameletnames'


        return
        end
