c SccsId = @(#)clutter_prf_amb.f	2.41 3/24/98




        subroutine clutter_prf_amb(kburst,ire_sim,gain_rec,ns,
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
     *  hufd,nr1,nr2,pos)
     


        implicit real*8 (a-h,o-z)
	include 'ssp2_const.inc'
	include 'ssp2_dim.inc'
c	include 'key_const.inc'
c	include 'key_pp.inc'
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
c WAYNE
	complex temp1,temp2,intp
	real acu(line_raw),ai1,ai2,aj1,aj2
	real*8 rr,dfdc(60,4),fdc(60),angloook(60,4),cc(4,5)
	integer pmod_i
c******************************************************
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
        real contrast,s_power,variance

c qdn 6/7/97
        real*8        run_avg, x_fdc(60)
        integer*4     inum_avg, iorder_smooth
      
        character*30 filename
        integer      ifd,initdk,idum

c qdn 7/25/97
        real*8        coef_fd_save(4)




c******************************************************



        call weight(w1,w2,w3,w4,64)
        kbeam = kburst - (kburst-1)/nbeams*nbeams

        do i=1,2*sam_raw
        do j=1,line_raw
           buff0(i,j)=0.0
        enddo
        enddo
        do i=1,sam_raw
        do j=1,line_raw
         buff1(i,j)=0.0
        enddo
        enddo
        do i=1,line_raw
        do j=1,sam_raw
         buff2(i,j)=0.0
        enddo
        enddo

c qdn 8/18/97 save the measured fd coefficients
        do i=1,4
           coef_fd_save(i)   = coef_fd(i,kburst)
        end do

c       write(6,*)' QDN CLUTTER coef_fd at i=',2,(coef_fd(i,2),i=1,4)
        call burst_pp_0(dfddth,pre_phase,thi,tp,coef_fd_0,
     *                        kburst,v_sc_mag,v_x_sc,v_y_sc,v_z_sc,
     *                        rng_bm_ang,beam_no,az_pk_coef,az_peak_coef,
     *                        beam_id,x_sc,y_sc,z_sc,roll,yaw,pitch,
     *                        look_ang,ire_sim,dem_ellip,dlon_topo,dlat_topo,
     *                        topo,i_ang_bip,r_bip,fr_bip,fd_bip,coef_fd,
     *                        coef_fr,coef_lk_ang,nbeams,np,prf,np_air,
     *                        r_low,r_1st,fd_near,fr_near,r_high,ns,nr,dr,
     *                        fd_far,fr_far,r_cntr,fd_cntr,fr_cntr,avg_terrain,coef_inc)
c       if (ire_sim.eq.0) then
c        np_proc = np(kburst) -9
c        call get_burst_sim(
c    *   kburst,plen,np_proc,fs,file_spc_raw,
c    *   ns,buff0,istatus,buff1,dk_burst,file_raw,
c    *   sim_eam_nsp)
c       else
         np_proc = np_v(kburst)
         call get_burst(
     *   kburst,plen,ire_sim,gain_rec,ns,
     *   np,dk_burst,file_spc_raw,data_cond,
     *   np_proc,fs,tbl,blk_gain,snr,
     *   buff0,buff1,istatus,file_raw,nr,np_v,
     *   aux_wdp_chg,aux_wdp_np,aux_agc_np,aux_wdp_cnt,aux_agc_chg,aux_agc_cnt)
         write(6,*)'CLUTTER IO',istatus, kburst
         if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
c       endif
c       write(6,*)'WHY',buff0(1,1),buff1(1,1)
c       write(6,*)'WHY',nr,fs,np_proc
        call range_comp(kburst,nr,fs,chirp_rate,fd_bip,
     *  nfft_az,np_proc,ns,r_1st,dr,r_bip,coef_fd,
     *  prf,buff1,np_v,buff_rep,ire_sim,   
     *  dk_rep,gain_rep,file_rep,istatus,tbl)
        if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
c       write(6,*)'WHY',buff1(1,1)
        call corner_turn(kburst,nr,ns,np_proc,nfft_az,
     *  buff1,buff2)                                           !data corner turn
c       write(6,*)'WHY',buff2(1,1)
c       write(6,*)'qdn in clutter_prf_amb : at burst',kburst
c       write(6,*)'qdn in clutter_prf_amb : coef_fd=',
c    *                           (coef_fd(i,kburst),i=1,4)
        do i=1,4
           coef_fd(i,kburst) = 0.0
        end do
        call az_comp(kburst,nr,np_proc,nfft_az,ns,
     *  r_1st,dr,r_bip,coef_fd,coef_fr,prf,buff2)  
        do i=1,4
           coef_fd(i,kburst) = coef_fd_save(i)
        end do
c       write(6,*)'WHY',buff2(1,1)
c       write(6,*)'WHY',kburst,nr,np_proc,nfft_az
c       write(6,*)'WHY',ns(kburst),r_1st(kburst),dr(kburst)
c       write(6,*)'qdn WHY',r_bip(kburst),(coef_fd(i,kburst),i=1,4)
c       write(6,*)'WHY',coef_fr(1,kburst),prf(kburst)

	do i = 1, ns(kburst)
	do j = 1, nfft_az
	burst1(j,i) = buff2(j,i)
	end do
	end do

c       write(6,*)' QDN CLUTTER coef_fd at i=',2,(coef_fd(i,2),i=1,4)


	s_power = 0.
	variance = 0.
	do i = 1, ns(kburst)
	do j = 1, nfft_az
	s_power = s_power + abs(burst1(j,i))
	variance = variance + abs(burst1(j,i))**2
	end do
	end do
	s_power = s_power / (nfft_az*ns(kburst))
	variance = variance / (nfft_az*ns(kburst))
D	write (*,*) 'here 60',kburst,nfft_az
D	write (*,*) 'here 61',s_power,variance
	contrast = (variance-s_power**2)/s_power**2

        do i=1,2*sam_raw
        do j=1,line_raw
           buff0(i,j)=0.0
        enddo
        enddo
        do i=1,sam_raw
        do j=1,line_raw
         buff1(i,j)=0.0
        enddo
        enddo
        do i=1,line_raw
        do j=1,sam_raw
         buff2(i,j)=0.0
        enddo
        enddo

c qdn 8/18/97 save the measured fd
         do i=1,4
            coef_fd_save(i) = coef_fd(i,kburst+nbeams)
         end do

c       write(6,*)' QDN CLUTTER coef_fd at i=',2,(coef_fd(i,2),i=1,4)
        call burst_pp_0(dfddth,pre_phase,thi,tp,coef_fd_0,
     *                        kburst+nbeams,v_sc_mag,v_x_sc,v_y_sc,v_z_sc,
     *                        rng_bm_ang,beam_no,az_pk_coef,az_peak_coef,
     *                        beam_id,x_sc,y_sc,z_sc,roll,yaw,pitch,
     *                        look_ang,ire_sim,dem_ellip,dlon_topo,dlat_topo,
     *                        topo,i_ang_bip,r_bip,fr_bip,fd_bip,coef_fd,
     *                        coef_fr,coef_lk_ang,nbeams,np,prf,np_air,
     *                        r_low,r_1st,fd_near,fr_near,r_high,ns,nr,dr,
     *                        fd_far,fr_far,r_cntr,fd_cntr,fr_cntr,avg_terrain,coef_inc)
c       if (ire_sim.eq.0) then
c        call get_burst_sim(
c    *   (kburst+nbeams),plen,np_proc,fs,file_spc_raw,
c    *   ns,buff0,istatus,buff1,dk_burst,file_raw,
c    *   sim_eam_nsp)
c       else
         call get_burst(
     *   (kburst+nbeams),plen,ire_sim,gain_rec,ns,
     *   np,dk_burst,file_spc_raw,data_cond,
     *   np_proc,fs,tbl,blk_gain,snr,
     *   buff0,buff1,istatus,file_raw,nr,np_v,
     *   aux_wdp_chg,aux_wdp_np,aux_agc_np,aux_wdp_cnt,aux_agc_chg,aux_agc_cnt)
         write(6,*)'CLUTTER IO',istatus, kburst+nbeams
         if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
c       endif
         call range_comp((kburst+nbeams),nr,fs,chirp_rate,fd_bip,
     *   nfft_az,np_proc,ns,r_1st,dr,r_bip,coef_fd,
     *   prf,buff1,np_v,buff_rep,ire_sim,  
     *   dk_rep,gain_rep,file_rep,istatus,tbl)
         if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
         call corner_turn((kburst+nbeams),nr,ns,np_proc,nfft_az,
     *   buff1,buff2) 
        write(6,*)'qdn in clutter_prf_amb : at burst',kburst+nbeams
        write(6,*)'qdn in clutter_prf_amb : coef_fd=',
     *                           (coef_fd(i,kburst+nbeams),i=1,4)
         do i=1,4
            coef_fd(i,kburst+nbeams)=0.0
         end do
         call az_comp((kburst+nbeams),nr,np_proc,nfft_az,ns,
     *   r_1st,dr,r_bip,coef_fd,coef_fr,prf,buff2) !azimuth comopression
         do i=1,4
            coef_fd(i,kburst+nbeams) = coef_fd_save(i) 
         end do

D	write (*,*) 'two burst done'
c       write(6,*)'qdn WHY',r_bip(kburst),(coef_fd(i,kburst),i=1,4)



	do i = 1, ns(kburst+nbeams)
	do j = 1, nfft_az
	burst2(j,i) = buff2(j,i)
	end do
	end do


c       write(6,*)' QDN CLUTTER coef_fd at i=',2,(coef_fd(i,2),i=1,4)

c******** input parameters *******************
	m = 4
	margin = 20

	tp = 0.
	do j = kburst,kburst+nbeams-1
	 tp = np(j)/prf(j)+tp
	end do

c******** initialize buffer ******************	

	r_low_in =  max(r_low(kburst),r_low(kburst+nbeams)) 
	r_high_in =  min(r_high(kburst),r_high(kburst+nbeams))

c       write(6,*)'WHAT',r_low_in,r_low(kburst),r_low(kburst+nbeams)

	do j = 1, nfft_az
	acu(j) = 0.
	end do


        write(6,*)' nr1,nr2=',nr1,nr2
        do i1 = nr1,nr2
c********** set index parameters *****************
C
	r = r_low_in + (i1-1)*dr(kburst)
	call v_poly(r,r_bip(kburst),coef_fd(1,kburst),m,fd_cnt)
	call v_poly(r,r_bip(kburst),coef_fr(1,kburst),m,fr)
        ai1 = ((r-r_low(kburst))/dr(kburst))+1
c       write(6,*)'Z1',r,r_low(kburst),dr(kburst),i1,r_low_in
c       write(6,*)'Z1',ai1
        ai20 =((r-r_low(kburst+nbeams))/dr(kburst+nbeams))+1
        f_off = abs(fr)*tp
       

c********** take log of the ratio ****************

        do j = 1, nfft_az
        fd = fd_cnt+(j-nfft_az/2-1)*df(kburst)
        dr_migra = lambda/2*(fd+fr/2*tp)*tp/dr(kburst)
	ai2 = ai20-dr_migra
	aj2 = j
	temp2 = intp(burst2,nfft_az,line_raw,sam_raw,aj2,ai2,2,
     *               w1,w2,w3,w4)
c       write(6,*)'Z2',aj2,ai2,temp2
	aj1 = j+f_off/df(kburst)
	temp1 = intp(burst1,nfft_az,line_raw,sam_raw,aj1,ai1,1,
     *               w1,w2,w3,w4)
c       write(6,*)'Z1',aj1,ai1,temp1
c       acu(j) = acu(j)+ log(abs(temp1/temp2))
        if(abs(temp2).ne.0.0 .and. abs(temp1).ne.0.0)
     *               acu(j) = acu(j)+ log(abs(temp1/temp2))
	end do
	end do




c************* zero cross detection *****************
        rr = r_low_in + (nr1+nr2)/2*dr(kburst)
	call v_poly(rr,r_bip(kburst),coef_fd(1,kburst),m,fd)
	call v_poly(rr,r_bip(kburst),coef_fr(1,kburst),m,fr)
	f_off = abs(fr) * tp
	ratio = prf(kburst)/(2*7500.d0/az_ant_len)
	call zero_cross_fd(acu,nfft_az,f_off,df(kburst),pos,ratio)

c       if(pos.eq.1.0) then
c         write(filename,'(a21,i4.4)') '/home/tmpdisk/acu.dat',kburst
c         write(6,*)'QDN Generate the file ',filename(1:25),
c    *              '-with',nfft_az
c         ifd = initdk(idum,filename(1:25)//char(0))
c         call iowrit(ifd,acu,nfft_az*4)
c         call closedk(idum,ifd)
c       end if

c       write(6,*)'ZZZ',f_off,pos,acu,nfft_az,df(kburst),ratio

 	write (*,*) 'fd_pred,pos= ', fd,pos


	
	return	
	end
