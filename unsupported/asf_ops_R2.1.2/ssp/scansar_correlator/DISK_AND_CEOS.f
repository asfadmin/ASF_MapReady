c SccsId = @(#)DISK_AND_CEOS.f	2.41 3/24/98
	subroutine dump_corr_to_ceos(file_corr_to_ceos,
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

C*****WAYNE******
       include 'ssp2_const.inc'
        real*8  r_cntr(burst_max)
        character*10  aux_yaw_valid(burst_max)
        character*10  aux_roll_valid(burst_max)
        character*10  aux_pitch_valid(burst_max)
        real*8  aux_calibration_temp(1600)
        real*8  aux_repl_pow
        real*8  bit_error_rate
        real*8  aux_lna_temp(1600)
        real*8  aux_subsystem_temp(1600)
        real*8  aux_protector_temp(1600)
        real*8           aux_rx_agc(1600)
        real*8           aux_replica_agc(1600)
       character*60     file_corr_to_ceos
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
       integer hist_i_avg(256),hist_q_avg(256)
       real*8     roll_o(burst_max)
       real*8     yaw_o(burst_max)
       real*8     roll(burst_max)
       real*8     yaw(burst_max)
       real*8     pitch_o(burst_max)
       real*8     pitch(burst_max)
       integer    pvs_node
       real*8          yaw_o_rate(burst_max)
       real*8          roll_o_rate(burst_max)
       real*8          pitch_o_rate(burst_max)
       real*8    p_x(burst_max),p_y(burst_max),p_z(burst_max)
       real*8          window_start_time(burst_max)
       real*8     t_burst(burst_max)
       real*8     c1_frm(2,2)
       real*8     c2_frm(2,2)
       real*8     alpha1
       real*8     alpha2
       real*8     alpha3
       integer*4       np_v(burst_max)
       real*8     c1_g(3,3,n_az,n_rg,burst_max)
       real*8     c2_g(3,3,n_az,n_rg,burst_max)
       real*8     c2_cell(burst_max)
       real*8     c1size_blk(burst_max)
       integer*4     n_c2_blk(burst_max)
       complex buff_spec(8192)
       real*8     r_bip(burst_max)
       real*8     coef_fd(4,burst_max)
       integer*4     ns(burst_max)
       real*8     gain_rec(burst_max)
       real*8     t_b(burst_max)







C*****WAYNE******
       integer              ifd,bytes,copen_w,cwrite_seq

       ifd            =copen_w(file_corr_to_ceos)
       if (ifd .le. 0) then
        istatus=ierr_2
        idummy=printflog(3,'Failed to open '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8*burst_max
       nbyte          =cwrite_seq(ifd,prf        ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =4*burst_max
       nbyte          =cwrite_seq(ifd,np        ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =4*burst_max
       nbyte          =cwrite_seq(ifd,np_air        ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =4
       nbyte          =cwrite_seq(ifd,burst_start        ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =4
       nbyte          =cwrite_seq(ifd,burst_end        ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =8
       nbyte          =cwrite_seq(ifd,chirp_rate        ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =8
       nbyte          =cwrite_seq(ifd,fs        ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =8*burst_max
       nbyte          =cwrite_seq(ifd,x_sc        ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =8*burst_max
       nbyte          =cwrite_seq(ifd,y_sc        ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =8*burst_max
       nbyte          =cwrite_seq(ifd,z_sc        ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =8*burst_max
       nbyte          =cwrite_seq(ifd,v_x_sc        ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =8*burst_max
       nbyte          =cwrite_seq(ifd,v_y_sc        ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =8*burst_max
       nbyte          =cwrite_seq(ifd,v_z_sc        ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =2*2*8*burst_max
       nbyte          =cwrite_seq(ifd,c1_flm        ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =2*2*8*burst_max
       nbyte          =cwrite_seq(ifd,c2_flm        ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =8*burst_max
       nbyte          =cwrite_seq(ifd,fr_near        ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =8*burst_max
       nbyte          =cwrite_seq(ifd,fr_far        ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =8*burst_max
       nbyte          =cwrite_seq(ifd,fd_low_far        ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =8*burst_max
       nbyte          =cwrite_seq(ifd,fd_low_near        ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =8*burst_max
       nbyte          =cwrite_seq(ifd,i_ang_bip        ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =8*burst_max
       nbyte          =cwrite_seq(ifd,look_ang        ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =8*burst_max
       nbyte          =cwrite_seq(ifd,r_low        ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =8*burst_max
       nbyte          =cwrite_seq(ifd,r_high        ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =8
       nbyte          =cwrite_seq(ifd,chirp_bw        ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =4*256
       nbyte          =cwrite_seq(ifd,hist_i_avg        ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =4*256
       nbyte          =cwrite_seq(ifd,hist_q_avg        ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =8*burst_max
       nbyte          =cwrite_seq(ifd,yaw        ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =8*burst_max
       nbyte          =cwrite_seq(ifd,roll        ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =8*burst_max
       nbyte          =cwrite_seq(ifd,pitch        ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =8*burst_max
       nbyte          =cwrite_seq(ifd,yaw_o       ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =8*burst_max
       nbyte          =cwrite_seq(ifd,roll_o       ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =8*burst_max
       nbyte          =cwrite_seq(ifd,pitch_o      ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =4
       nbyte          =cwrite_seq(ifd,pvs_node      ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =8*burst_max
       nbyte          =cwrite_seq(ifd,yaw_o_rate      ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =8*burst_max
       nbyte          =cwrite_seq(ifd,roll_o_rate      ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =8*burst_max
       nbyte          =cwrite_seq(ifd,pitch_o_rate      ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =8*burst_max
       nbyte          =cwrite_seq(ifd,p_x      ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =8*burst_max
       nbyte          =cwrite_seq(ifd,p_y      ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =8*burst_max
       nbyte          =cwrite_seq(ifd,p_z      ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =8*burst_max
       nbyte          =cwrite_seq(ifd,window_start_time     ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =8*burst_max
       nbyte          =cwrite_seq(ifd,t_burst     ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =8*2*2
       nbyte          =cwrite_seq(ifd,c1_frm     ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =8*2*2
       nbyte          =cwrite_seq(ifd,c2_frm     ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =8
       nbyte          =cwrite_seq(ifd,alpha1     ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =8
       nbyte          =cwrite_seq(ifd,alpha2     ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =8
       nbyte          =cwrite_seq(ifd,alpha3     ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =4*burst_max
       nbyte          =cwrite_seq(ifd,np_v    ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =8*3*3*n_az*n_rg*burst_max
       nbyte          =cwrite_seq(ifd,c1_g    ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =8*3*3*n_az*n_rg*burst_max
       nbyte          =cwrite_seq(ifd,c2_g    ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =8*burst_max
       nbyte          =cwrite_seq(ifd,c2_cell    ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =8*burst_max
       nbyte          =cwrite_seq(ifd,c1size_blk    ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =4*burst_max
       nbyte          =cwrite_seq(ifd,n_c2_blk    ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =8*8192
       nbyte          =cwrite_seq(ifd,buff_spec    ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =8*burst_max
       nbyte          =cwrite_seq(ifd,r_bip    ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =8*burst_max*4
       nbyte          =cwrite_seq(ifd,coef_fd    ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =4*burst_max
       nbyte          =cwrite_seq(ifd,ns    ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =8*burst_max
       nbyte          =cwrite_seq(ifd,aux_lna_temp    ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =8*burst_max
       nbyte          =cwrite_seq(ifd,aux_subsystem_temp    ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =8*burst_max
       nbyte          =cwrite_seq(ifd,aux_protector_temp    ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =8*burst_max
       nbyte          =cwrite_seq(ifd,aux_rx_agc    ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =8*burst_max
       nbyte          =cwrite_seq(ifd,aux_replica_agc    ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =8*burst_max
       nbyte          =cwrite_seq(ifd, aux_calibration_temp    ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif

       bytes          =8
       nbyte          =cwrite_seq(ifd, aux_repl_pow    ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =8
       nbyte          =cwrite_seq(ifd, bit_error_rate    ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =8*burst_max
       nbyte          =cwrite_seq(ifd, gain_rec    ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =8*burst_max
       nbyte          =cwrite_seq(ifd, r_cntr    ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =10*burst_max
       nbyte          =cwrite_seq(ifd, aux_yaw_valid    ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =10*burst_max
       nbyte          =cwrite_seq(ifd, aux_roll_valid    ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =10*burst_max
       nbyte          =cwrite_seq(ifd, aux_pitch_valid    ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif
       bytes          =8*burst_max
       nbyte          =cwrite_seq(ifd, t_b    ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_ceos//'&')
           idummy=printerr (file_corr_to_ceos)
           return
       endif

       return
       end

	subroutine load_corr_to_ceos(file_corr_to_ceos,
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

C*****WAYNE******
       include 'ssp2_const.inc'
        real*8 r_cntr
        character*10  aux_yaw_valid(burst_max)
        character*10  aux_roll_valid(burst_max)
        character*10  aux_pitch_valid(burst_max)
        real*8  aux_lna_temp(1600)
        real*8  aux_subsystem_temp(1600)
        real*8  aux_protector_temp(1600)
        real*8  aux_calibration_temp(1600)
        real*8  aux_repl_pow
        real*8  bit_error_rate
        real*8           aux_rx_agc(1600)
        real*8           aux_replica_agc(1600)
       character*60     file_corr_to_ceos
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
       integer hist_i_avg(256),hist_q_avg(256)
       real*8     roll_o(burst_max)
       real*8     yaw_o(burst_max)
       real*8     roll(burst_max)
       real*8     yaw(burst_max)
       real*8     pitch_o(burst_max)
       real*8     pitch(burst_max)
       integer    pvs_node
       real*8          yaw_o_rate(burst_max)
       real*8          roll_o_rate(burst_max)
       real*8          pitch_o_rate(burst_max)
       real*8    p_x(burst_max),p_y(burst_max),p_z(burst_max)
       real*8          window_start_time(burst_max)
       real*8     t_burst(burst_max)
       real*8     c1_frm(2,2)
       real*8     c2_frm(2,2)
       real*8     alpha1
       real*8     alpha2
       real*8     alpha3
       integer*4       np_v(burst_max)
       real*8     c1_g(3,3,n_az,n_rg,burst_max)
       real*8     c2_g(3,3,n_az,n_rg,burst_max)
       real*8     c2_cell(burst_max)
       real*8     c1size_blk(burst_max)
       integer*4     n_c2_blk(burst_max)
       complex      buff_spec(8192)
       real*8     r_bip(burst_max)
       real*8     coef_fd(4,burst_max)
       integer*4     ns(burst_max)
       real*8     gain_rec(burst_max)
       real*8     t_b(burst_max)
C*****WAYNE******
       integer              ifd,bytes,copen_r,cread_seq

       ifd            =copen_r(file_corr_to_ceos)
       if(ifd .le. 0)then
        istatus=ierr_2
        idummy=printflog(3,'Failed to open '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8*burst_max
       nbyte          =cread_seq(ifd,prf        ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =4*burst_max
       nbyte          =cread_seq(ifd,np        ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =4*burst_max
       nbyte          =cread_seq(ifd,np_air        ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =4
       nbyte          =cread_seq(ifd,burst_start        ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =4
       nbyte          =cread_seq(ifd,burst_end        ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8
       nbyte          =cread_seq(ifd,chirp_rate        ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8
       nbyte          =cread_seq(ifd,fs        ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8*burst_max
       nbyte          =cread_seq(ifd,x_sc        ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8*burst_max
       nbyte          =cread_seq(ifd,y_sc        ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8*burst_max
       nbyte          =cread_seq(ifd,z_sc        ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8*burst_max
       nbyte          =cread_seq(ifd,v_x_sc        ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8*burst_max
       nbyte          =cread_seq(ifd,v_y_sc        ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8*burst_max
       nbyte          =cread_seq(ifd,v_z_sc        ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =2*2*8*burst_max
       nbyte          =cread_seq(ifd,c1_flm        ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =2*2*8*burst_max
       nbyte          =cread_seq(ifd,c2_flm        ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8*burst_max
       nbyte          =cread_seq(ifd,fr_near        ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8*burst_max
       nbyte          =cread_seq(ifd,fr_far        ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8*burst_max
       nbyte          =cread_seq(ifd,fd_low_far        ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8*burst_max
       nbyte          =cread_seq(ifd,fd_low_near        ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8*burst_max
       nbyte          =cread_seq(ifd,i_ang_bip        ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8*burst_max
       nbyte          =cread_seq(ifd,look_ang        ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8*burst_max
       nbyte          =cread_seq(ifd,r_low        ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8*burst_max
       nbyte          =cread_seq(ifd,r_high        ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8
       nbyte          =cread_seq(ifd,chirp_bw        ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =4*256
       nbyte          =cread_seq(ifd,hist_i_avg        ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =4*256
       nbyte          =cread_seq(ifd,hist_q_avg        ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8*burst_max
       nbyte          =cread_seq(ifd,yaw        ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8*burst_max
       nbyte          =cread_seq(ifd,roll        ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8*burst_max
       nbyte          =cread_seq(ifd,pitch        ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8*burst_max
       nbyte          =cread_seq(ifd,yaw_o       ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8*burst_max
       nbyte          =cread_seq(ifd,roll_o       ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8*burst_max
       nbyte          =cread_seq(ifd,pitch_o      ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =4
       nbyte          =cread_seq(ifd,pvs_node      ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        return
       endif
       bytes          =8*burst_max
       nbyte          =cread_seq(ifd,yaw_o_rate      ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8*burst_max
       nbyte          =cread_seq(ifd,roll_o_rate      ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8*burst_max
       nbyte          =cread_seq(ifd,pitch_o_rate      ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8*burst_max
       nbyte          =cread_seq(ifd,p_x      ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8*burst_max
       nbyte          =cread_seq(ifd,p_y      ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8*burst_max
       nbyte          =cread_seq(ifd,p_z      ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8*burst_max
       nbyte          =cread_seq(ifd,window_start_time     ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8*burst_max
       nbyte          =cread_seq(ifd,t_burst     ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8*2*2
       nbyte          =cread_seq(ifd,c1_frm     ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8*2*2
       nbyte          =cread_seq(ifd,c2_frm     ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8
       nbyte          =cread_seq(ifd,alpha1     ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8
       nbyte          =cread_seq(ifd,alpha2     ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8
       nbyte          =cread_seq(ifd,alpha3     ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =4*burst_max
       nbyte          =cread_seq(ifd,np_v    ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8*3*3*n_az*n_rg*burst_max
       nbyte          =cread_seq(ifd,c1_g    ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8*3*3*n_az*n_rg*burst_max
       nbyte          =cread_seq(ifd,c2_g    ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8*burst_max
       nbyte          =cread_seq(ifd,c2_cell    ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8*burst_max
       nbyte          =cread_seq(ifd,c1size_blk    ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =4*burst_max
       nbyte          =cread_seq(ifd,n_c2_blk    ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8*8192
       nbyte          =cread_seq(ifd,buff_spec    ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8*burst_max
       nbyte          =cread_seq(ifd,r_bip    ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8*burst_max*4
       nbyte          =cread_seq(ifd,coef_fd    ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =4*burst_max
       nbyte          =cread_seq(ifd,ns    ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8*burst_max
       nbyte          =cread_seq(ifd,aux_lna_temp    ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8*burst_max
       nbyte          =cread_seq(ifd,aux_subsystem_temp    ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8*burst_max
       nbyte          =cread_seq(ifd,aux_protector_temp    ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8*burst_max
       nbyte          =cread_seq(ifd,aux_rx_agc    ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8*burst_max
       nbyte          =cread_seq(ifd,aux_replica_agc    ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8*burst_max
       nbyte          =cread_seq(ifd,aux_calibration_temp    ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8
       nbyte          =cread_seq(ifd,aux_repl_pow    ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8
       nbyte          =cread_seq(ifd,bit_error_rate    ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8*burst_max
       nbyte          =cread_seq(ifd,gain_rec    ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8*burst_max
       nbyte          =cread_seq(ifd,r_cntr    ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =10*burst_max
       nbyte          =cread_seq(ifd,aux_yaw_valid    ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =10*burst_max
       nbyte          =cread_seq(ifd,aux_roll_valid    ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =10*burst_max
       nbyte          =cread_seq(ifd,aux_pitch_valid    ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif
       bytes          =8*burst_max
       nbyte          =cread_seq(ifd,t_b    ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_ceos//'&')
        idummy=printerr (file_corr_to_ceos)
        return
       endif







       return
       end
	subroutine dump_pre_to_ceos(file_pre_to_ceos,istatus)
C*****WAYNE******
       include 'ssp2_const.inc'
       character*60     file_pre_to_ceos
C*****WAYNE******
       integer              ifd,bytes,copen_w,cwrite_seq

       ifd            =copen_w(file_pre_to_ceos)
       if (ifd .le. 0) then
        istatus=ierr_2
        idummy=printflog(3,'Failed to open '//file_pre_to_ceos//'&')
        idummy=printerr (file_pre_to_ceos)
        return
       endif
       bytes          =4
       nbyte          =cwrite_seq(ifd,ipage        ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_pre_to_ceos//'&')
           idummy=printerr (file_pre_to_ceos)
           return
       endif
       return
       end
	subroutine load_pre_to_ceos(file_pre_to_ceos,istatus)
C*****WAYNE******
       include 'ssp2_const.inc'
       character*60     file_pre_to_ceos
C*****WAYNE******
       integer              ifd,bytes,copen_r,cread_seq

       ifd            =copen_r(file_pre_to_ceos)
       if(ifd .le. 0)then
        istatus=ierr_2
        idummy=printflog(3,'Failed to open '//file_pre_to_ceos//'&')
        idummy=printerr (file_pre_to_ceos)
        return
       endif
       bytes          =4
       nbyte          =cread_seq(ifd,ipage        ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_pre_to_ceos//'&')
        idummy=printerr (file_pre_to_ceos)
        return
       endif
       return
       end
	subroutine dump_post_to_ceos(file_post_to_ceos,nlines,nsamples,p1_fm,p2_fm,zone,slat,
     *   along,plat1,plat2,lat_orig,long_orig,istatus)
C*****WAYNE******
       include 'ssp2_const.inc'
       character*60     file_post_to_ceos
       integer          nlines,nsamples
       real*8           p1_fm(2,2),p2_fm(2,2)
       real*8  plat1,plat2
       real*8 lat_orig,long_orig
       real*8     slat
       real*8     along0
       integer     zone
C*****WAYNE******
       integer              ifd,bytes,copen_w,cwrite_seq

       ifd            =copen_w(file_post_to_ceos)
       if (ifd .le. 0) then
        istatus=ierr_2
        idummy=printflog(3,'Failed to open '//file_post_to_ceos//'&')
        idummy=printerr (file_post_to_ceos)
        return
       endif
       bytes          =4
       nbyte          =cwrite_seq(ifd,nlines        ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_post_to_ceos//'&')
           idummy=printerr (file_post_to_ceos)
           return
       endif
       bytes          =4
       nbyte          =cwrite_seq(ifd,nsamples        ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_post_to_ceos//'&')
           idummy=printerr (file_post_to_ceos)
           return
       endif
       bytes          =8*2*2
       nbyte          =cwrite_seq(ifd,p1_fm        ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_post_to_ceos//'&')
           idummy=printerr (file_post_to_ceos)
           return
       endif
       bytes          =8*2*2
       nbyte          =cwrite_seq(ifd,p2_fm        ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_post_to_ceos//'&')
           idummy=printerr (file_post_to_ceos)
           return
       endif
       bytes          =8
       nbyte          =cwrite_seq(ifd,plat1        ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_post_to_ceos//'&')
           idummy=printerr (file_post_to_ceos)
           return
       endif
       bytes          =8
       nbyte          =cwrite_seq(ifd,plat2        ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_post_to_ceos//'&')
           idummy=printerr (file_post_to_ceos)
           return
       endif
       bytes          =8
       nbyte          =cwrite_seq(ifd,lat_orig        ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_post_to_ceos//'&')
           idummy=printerr (file_post_to_ceos)
           return
       endif
       bytes          =8
       nbyte          =cwrite_seq(ifd,long_orig        ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_post_to_ceos//'&')
           idummy=printerr (file_post_to_ceos)
           return
       endif
       bytes          =8
       nbyte          =cwrite_seq(ifd,slat        ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_post_to_ceos//'&')
           idummy=printerr (file_post_to_ceos)
           return
       endif
       bytes          =8
       nbyte          =cwrite_seq(ifd,along0        ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_post_to_ceos//'&')
           idummy=printerr (file_post_to_ceos)
           return
       endif
       bytes          =4
       nbyte          =cwrite_seq(ifd,zone        ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_post_to_ceos//'&')
           idummy=printerr (file_post_to_ceos)
           return
       endif


       return
       end
	subroutine load_post_to_ceos(file_post_to_ceos,nlines,nsamples,p1_fm,p2_fm,zone,slat,
     *   along,plat1,plat2,lat_orig,long_orig,istatus)
C*****WAYNE******
       include 'ssp2_const.inc'
       character*60     file_post_to_ceos
       integer          nlines,nsamples
       real*8           p1_fm(2,2),p2_fm(2,2)
       real*8  plat1,plat2
       real*8 lat_orig,long_orig
       real*8     slat
       real*8     along0
       integer     zone
C*****WAYNE******
       integer              ifd,bytes,copen_r,cread_seq

       ifd            =copen_r(file_post_to_ceos)
       if(ifd .le. 0)then
        istatus=ierr_2
        idummy=printflog(3,'Failed to open '//file_post_to_ceos//'&')
        idummy=printerr (file_post_to_ceos)
        return
       endif
       bytes          =4
       nbyte          =cread_seq(ifd,nlines        ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_post_to_ceos//'&')
        idummy=printerr (file_post_to_ceos)
        return
       endif
       bytes          =4
       nbyte          =cread_seq(ifd,nsamples        ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_post_to_ceos//'&')
        idummy=printerr (file_post_to_ceos)
        return
       endif
       bytes          =8*2*2
       nbyte          =cread_seq(ifd,p1_fm        ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_post_to_ceos//'&')
        idummy=printerr (file_post_to_ceos)
        return
       endif
       bytes          =8*2*2
       nbyte          =cread_seq(ifd,p2_fm        ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_post_to_ceos//'&')
        idummy=printerr (file_post_to_ceos)
        return
       endif
       bytes          =8
       nbyte          =cread_seq(ifd,plat1        ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_post_to_ceos//'&')
        idummy=printerr (file_post_to_ceos)
        return
       endif
       bytes          =8
       nbyte          =cread_seq(ifd,plat2        ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_post_to_ceos//'&')
        idummy=printerr (file_post_to_ceos)
        return
       endif
       bytes          =8
       nbyte          =cread_seq(ifd,lat_orig        ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_post_to_ceos//'&')
        idummy=printerr (file_post_to_ceos)
        return
       endif
       bytes          =8
       nbyte          =cread_seq(ifd,long_orig        ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_post_to_ceos//'&')
        idummy=printerr (file_post_to_ceos)
        return
       endif
       bytes          =8
       nbyte          =cread_seq(ifd,slat        ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_post_to_ceos//'&')
        idummy=printerr (file_post_to_ceos)
        return
       endif
       bytes          =8
       nbyte          =cread_seq(ifd,along0        ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_post_to_ceos//'&')
        idummy=printerr (file_post_to_ceos)
        return
       endif
       bytes          =4
       nbyte          =cread_seq(ifd,zone        ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_post_to_ceos//'&')
        idummy=printerr (file_post_to_ceos)
        return
       endif


       return
       end
