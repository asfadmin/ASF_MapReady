c SccsId = @(#)fix_fd_b2.f	2.41 3/24/98

        subroutine fix_fd_B2(bestp,nprf1,
     *  ire_sim,gain_rec,ns,
     *  np,dr,df,fs,dk_burst,file_spc_raw,data_cond,
     *  tbl,blk_gain,snr,buff0,buff1,buff2,chirp_rate,fd_bip,
     *  nfft_az,r_low,r_1st,r_bip,r_high,coef_fd,coef_fr,prf,
     *  nbeams,burst1,burst2,w1,w2,w3,w4,contrast,s_power,
     *  istatus,file_raw,nr,
     *  sim_eam_nsp,np_v,dk_rep,gain_rep,file_rep,
     *  aux_wdp_chg,aux_wdp_np,aux_agc_np,aux_wdp_cnt,aux_agc_chg,aux_agc_cnt,
     *  dfddth,pre_phase,coef_fd_0,thi,tp,v_sc_mag,x_sc,y_sc,z_sc,
     *  v_x_sc,v_y_sc,v_z_sc,rng_bm_ang,beam_no,beam_id,az_peak_coef,
     *  az_pk_coef,roll,yaw,pitch,look_ang,dem_ellip,dlon_topo,dlat_topo,
     *  topo,i_ang_bip,fr_bip,coef_lk_ang,np_air,fd_near,fr_near,
     *  fd_far,fr_far,r_cntr,fd_cntr,fr_cntr,avg_terrain,coef_inc,
     *  hufd)

c       implicit real*8 (a-h,o-z)
        implicit none

        character*128 SccsId_fix_fd_b2
        data SccsId_fix_fd_b2
     +  /'@(#)PPfix_fd_b2.f:2.41'/


	include 'ssp2_const.inc'
        include 'ssp2_dim.inc'
c	include 'key_const.inc'
c	include 'key_pp.inc'


        integer bestp,nprf1

        real*8   avg_terrain,coef_inc(4,burst_max)
        integer  sim_eam_nsp(4)
        integer  np_v(burst_max)
        integer  dk_rep(burst_max)
        real*8   gain_rep(burst_max)
        character*60         file_rep
       complex buff_rep(ns_rep_buff,1)
        real*8        aux_wdp_chg(max_agc,1600)
        integer*4     aux_wdp_np(max_agc,1600)
        integer*4     aux_agc_np(max_agc,1600)
        integer*4     aux_wdp_cnt(1600)
        real*8        aux_agc_chg(max_agc,1600)
        integer*4     aux_agc_cnt(1600)
C***********************************
        integer ire_sim
        real*8 gain_rec(burst_max),dr(burst_max)
        real*8 df(burst_max)
        integer ns(burst_max),np(burst_max)
        integer dk_burst(burst_max)
        character*60 file_spc_raw
        integer data_cond
        complex tbl(-128:127)
        integer blk_gain(iq_blk,burst_max)
        real*8 snr(burst_max)
        byte buff0(2*sam_raw,line_raw)
        complex buff1(sam_raw,line_raw)
        complex buff2(line_raw,sam_raw)
        real*8 chirp_rate
        real*8 fd_bip(burst_max)
        real*8 coef_fd(4,burst_max)
        real*8 coef_fr(4,burst_max)
        real*8 r_low(burst_max)
        real*8 r_1st(burst_max)
        real*8 r_bip(burst_max)
        real*8 r_high(burst_max)
        integer nfft_az,nbeams
        real*8 prf(burst_max)
        real*8 fs
        integer nr
        character*60 file_raw
        real w1(64),w2(64),w3(64),w4(64)
        real hufd(60,200,4)
        complex burst1(line_raw,sam_raw),burst2(line_raw,sam_raw)

       real*8     dfddth
       integer    pre_phase
       real*8     coef_fd_0(4,burst_max)
       real*8     thi(5)
       real*8     tp
       real*8          v_sc_mag(burst_max)
       real*8 x_sc(burst_max)
       real*8 y_sc(burst_max)
       real*8 z_sc(burst_max)
       real*8 v_x_sc(burst_max)
       real*8 v_y_sc(burst_max)
       real*8 v_z_sc(burst_max)
       real*8 rng_bm_ang(4)
       integer beam_no(burst_max)
       integer beam_id(burst_max)
       real*8 az_peak_coef(3,8)
       real*8     az_pk_coef(3)
        real*8 roll(burst_max)
        real*8 yaw(burst_max)
        real*8 pitch(burst_max)
        real*8 look_ang(burst_max)
        integer dem_ellip
        real*8 dlon_topo,dlat_topo
        integer*2 topo(ns_ew,ns_ns)
        real*8          i_ang_bip(burst_max)
        real*8          fr_bip(burst_max)
        real*8          coef_lk_ang(4,burst_max)
        integer         np_air(burst_max)
        real*8          fd_near(burst_max)
        real*8          fr_near(burst_max)
        real*8          fd_far(burst_max)
        real*8          fr_far(burst_max)
        real*8          r_cntr(burst_max)
        real*8          fd_cntr(burst_max)
        real*8          fr_cntr(burst_max)
        real contrast,s_power


c Local variables
        integer n_pair,nr1_1,nr2_1,nr1_2,nr2_2,n_ovlp,nprf2
        integer k,kburst1,kburst2,istatus
        integer k1,k2,m
        real*8  pos1,pos2,prf1,prf2
        real*8  pos,f_diff
        real*8  r_low_in,r_high_in
        real*8  r_cnt,angloook,az_pk_ang1,az_pk_ang2,df_azoff
        real*8  pos1_ref,pos2_ref
        real*8  f1,f2,x_nfft_az

        prf1 = prf(bestp)
        prf2 = prf(bestp+1)

c       n_pair = 16
        n_pair = 16
        pos1 = 0
        pos2 = 0

c       write(6,*)' QDN in fix_fd_b2, before call burst_pp_0',
c    *                'at burst=',2,(coef_fd(k,2),k=1,4)
c       call burst_pp_0(dfddth,pre_phase,thi,tp,coef_fd_0,
c    *                  bestp,v_sc_mag,v_x_sc,v_y_sc,v_z_sc,
c    *                  rng_bm_ang,beam_no,az_pk_coef,az_peak_coef,
c    *                  beam_id,x_sc,y_sc,z_sc,roll,yaw,pitch,
c    *                  look_ang,ire_sim,dem_ellip,dlon_topo,dlat_topo,
c    *                  topo,i_ang_bip,r_bip,fr_bip,fd_bip,coef_fd,
c    *                  coef_fr,coef_lk_ang,nbeams,np,prf,np_air,
c    *                  r_low,r_1st,fd_near,fr_near,r_high,ns,nr,dr,
c    *                  fd_far,fr_far,r_cntr,fd_cntr,fr_cntr,
c    *                  avg_terrain,coef_inc)
c       write(6,*)' QDN in fix_fd_b2, after call burst_pp_0',
c    *                'at burst=',2,(coef_fd(k,2),k=1,4)

c       call burst_pp_0(dfddth,pre_phase,thi,tp,coef_fd_0,
c    *                  bestp+1,v_sc_mag,v_x_sc,v_y_sc,v_z_sc,
c    *                  rng_bm_ang,beam_no,az_pk_coef,az_peak_coef,
c    *                  beam_id,x_sc,y_sc,z_sc,roll,yaw,pitch,
c    *                  look_ang,ire_sim,dem_ellip,dlon_topo,dlat_topo,
c    *                  topo,i_ang_bip,r_bip,fr_bip,fd_bip,coef_fd,
c    *                  coef_fr,coef_lk_ang,nbeams,np,prf,np_air,
c    *                  r_low,r_1st,fd_near,fr_near,r_high,ns,nr,dr,
c    *                  fd_far,fr_far,r_cntr,fd_cntr,fr_cntr,
c    *                  avg_terrain,coef_inc)

c       write(6,*)' QDN in fix_fd_b2, after call 2nd burst_pp_0',
c    *                'at burst=',2,(coef_fd(k,2),k=1,4)

	r_low_in = max(r_low(bestp),r_low(bestp+1))
	r_high_in = min(r_high(bestp),r_high(bestp+1))
        n_ovlp = int((r_high_in-r_low_in)/dr(bestp))
        nr1_1 = nint((r_low_in-r_low(bestp))/dr(bestp))+3
        nr2_1 = nr1_1 + n_ovlp - 6
        nr1_2 = 3
        nr2_2 = n_ovlp - 6


        do k=1,n_pair
           kburst1 = (k-1)*nbeams+bestp
           call clutter_prf_amb(kburst1,ire_sim,gain_rec,ns,
     *  np,dr,df,fs,dk_burst,file_spc_raw,data_cond,
     *  tbl,blk_gain,snr,buff0,buff1,buff2,chirp_rate,fd_bip,
     *  nfft_az,r_low,r_1st,r_bip,r_high,coef_fd,coef_fr,prf,
     *  nbeams,burst1,burst2,w1,w2,w3,w4,contrast,s_power,
     *  istatus,file_raw,nr,
     *  sim_eam_nsp,np_v,dk_rep,gain_rep,file_rep,
     *  aux_wdp_chg,aux_wdp_np,aux_agc_np,aux_wdp_cnt,aux_agc_chg,aux_agc_cnt,
     *  dfddth,pre_phase,coef_fd_0,thi,tp,v_sc_mag,x_sc,y_sc,z_sc,
     *  v_x_sc,v_y_sc,v_z_sc,rng_bm_ang,beam_no,beam_id,az_peak_coef,
     *  az_pk_coef,roll,yaw,pitch,look_ang,dem_ellip,dlon_topo,dlat_topo,
     *  topo,i_ang_bip,fr_bip,coef_lk_ang,np_air,fd_near,fr_near,
     *  fd_far,fr_far,r_cntr,fd_cntr,fr_cntr,avg_terrain,coef_inc,
     *  hufd,nr1_1,nr2_1,pos)

           if(k.eq.1) pos1_ref=pos
           pos = pos-nfft_az*nint((pos-pos1_ref)/nfft_az)
           pos1 = pos1 + pos


           kburst2 = kburst1 + 1
           call clutter_prf_amb(kburst2,ire_sim,gain_rec,ns,
     *  np,dr,df,fs,dk_burst,file_spc_raw,data_cond,
     *  tbl,blk_gain,snr,buff0,buff1,buff2,chirp_rate,fd_bip,
     *  nfft_az,r_low,r_1st,r_bip,r_high,coef_fd,coef_fr,prf,
     *  nbeams,burst1,burst2,w1,w2,w3,w4,contrast,s_power,
     *  istatus,file_raw,nr,
     *  sim_eam_nsp,np_v,dk_rep,gain_rep,file_rep,
     *  aux_wdp_chg,aux_wdp_np,aux_agc_np,aux_wdp_cnt,aux_agc_chg,aux_agc_cnt,
     *  dfddth,pre_phase,coef_fd_0,thi,tp,v_sc_mag,x_sc,y_sc,z_sc,
     *  v_x_sc,v_y_sc,v_z_sc,rng_bm_ang,beam_no,beam_id,az_peak_coef,
     *  az_pk_coef,roll,yaw,pitch,look_ang,dem_ellip,dlon_topo,dlat_topo,
     *  topo,i_ang_bip,fr_bip,coef_lk_ang,np_air,fd_near,fr_near,
     *  fd_far,fr_far,r_cntr,fd_cntr,fr_cntr,avg_terrain,coef_inc,
     *  hufd,nr1_2,nr2_2,pos)

           if(k.eq.1) pos2_ref=pos
           pos = pos-nfft_az*nint((pos - pos2_ref)/nfft_az)
           pos2 = pos2 + pos

        end do

        pos1 = pos1/n_pair
        pos2 = pos2/n_pair


        m = 4
        k1 = bestp
        k2 = k1+1
        r_cnt = r_low(k1) + (nr1_1+nr2_1)/2*dr(k1)
        call v_poly(r_cnt,r_bip(k1),coef_lk_ang(1,k1),m,angloook)
        az_pk_coef(1) = az_peak_coef(1,beam_id(k1))
        az_pk_coef(2) = az_peak_coef(2,beam_id(k1))
        az_pk_coef(3) = az_peak_coef(3,beam_id(k1))
        az_pk_ang1 = az_pk_coef(1)+az_pk_coef(2)*angloook+
     1              az_pk_coef(3)*angloook**2
        az_pk_coef(1) = az_peak_coef(1,beam_id(k2))
        az_pk_coef(2) = az_peak_coef(2,beam_id(k2))
        az_pk_coef(3) = az_peak_coef(3,beam_id(k2))
        az_pk_ang2= az_pk_coef(1)+az_pk_coef(2)*angloook+
     1              az_pk_coef(3)*angloook**2
        df_azoff = (az_pk_ang1 -az_pk_ang2)*dfddth

    
        write(6,*)' dfddth=',dfddth
        write(6,*)' az_pk_ang1,az_pk_ang2=',az_pk_ang1,az_pk_ang2
        write(6,*) ' df_azoff=',df_azoff
        write(6,*) ' pos1,pos2=',pos1,pos2
        write(6,*) ' (pos1-pos2)*prf(bestp)/nfft_az=',
     *               (pos1-pos2)*prf(bestp)/nfft_az

        x_nfft_az = nfft_az
        f1 = (mod(pos1+100.5*nfft_az,x_nfft_az) 
     *             - 0.5*nfft_az - 1.) * prf(bestp)/nfft_az

        f2 = (mod(pos2+100.5*nfft_az,x_nfft_az) 
     *             - 0.5*nfft_az - 1.) *  prf(bestp+1)/nfft_az


c       f_diff =  f1 - f2 - nint((f1-f2)/prf(bestp+1))*prf(bestp+1) 
c qdn 8/19/97 This offset, 4.55, will make the correct PRF amb.
        f_diff =  f1 - f2  - 4.55 - nint((f1-f2)/prf(bestp+1))*prf(bestp+1) 

        write(6,*)' f_diff=',f_diff


        call n_prf_both(prf1,prf2,f_diff,nprf1,nprf2) !match by PRF diff table

        write(6,*)' nprf1,nprf2=',nprf1,nprf2

        return
        end

