c SccsId = @(#)pta.f	2.41 3/24/98

c***************** pointing refinement ***************************


	subroutine pta_refine(n_times_pre,burst_total,nbeams,
     *  ire_sim,gain_rec,ns,np,dr,df,fs,dk_burst,file_spc_raw,
     *  data_cond,tbl,blk_gain,snr,chirp_rate,nfft_az,prf,plen,
     *  prf_est_mode,np_proc,beam_no,az_peak_coef,beam_id,
     *  x_sc,y_sc,z_sc,v_x_sc,v_y_sc,v_z_sc,peak_bm_ang,
c Beginning of block of variables which can be removed
     *  r_1st,
     *  np_air,
c    *  i_ang_bip,r_bip,fd_bip,fr_bip,coef_fd,coef_fr,coef_lk_ang,
c    *  r_low,r_1st,r_cntr,r_high,fd_low_near,fd_high_near,
c    *  fd_near,fr_near,pbw_near,fd_low_cntr,fd_high_cntr,fd_cntr,
c    *  fr_cntr,pbw_cntr,fd_low_far,fd_high_far,fd_far,fr_far,
c    *  pbw_far,r_g,fd_g,c1_g,c2_g,n_c1_blk,n_c2_blk,c1_flm,c2_flm,
c    *  c1_cell,c2_cell,c1size_blk,c2size_blk,np_air,v_sc_mag,
c End of block of variables which can be removed
c Also, can remove buff0,buff,and buff2
     *  rgc_est_mode,burst_cnt,roll_o,yaw_o,pitch_o,roll,yaw,
     *  pitch,nr,rng_bm_ang,look_ang,max_look,
     *  proc_mode,fm_sizec2_in,c1_pxl,c2_pxl,ele_ang_eq,alpha1,
     *  alpha2,alpha3,file_raw,dem_ellip,dlon_topo,dlat_topo,topo,
     *  kids,mytask,burst_start_p,burst_end_p,
     *  sim_eam_nsp,np_v,dk_rep,gain_rep,file_rep,
     *  aux_wdp_chg,aux_wdp_np,aux_agc_np,aux_wdp_cnt,aux_agc_chg,aux_agc_cnt,
     *  pre_phase,prf_num,ant_patn_r,beam_mode,avg_terrain,istatus,burst_good)

	implicit real*8 (a-h, o-z)
	include 'ssp2_const.inc'
	include 'ssp2_dim.inc'
c	include 'key_const.inc'
c	include 'key_pp.inc'
        integer*4       burst_good(1600)
        integer  kids,mytask,istatus
        integer  burst_start_p,burst_end_p
        integer*4       sim_eam_nsp(4)
        real*8          gain_rep(burst_max)
        real*8          avg_terrain
        integer*4       np_v(burst_max)
        integer*4       dk_rep(burst_max)
        character*60    file_rep
        real*8        aux_wdp_chg(max_agc,1600)
        integer*4     aux_wdp_np(max_agc,1600)
        integer*4     aux_agc_np(max_agc,1600)
        integer*4     aux_wdp_cnt(1600)
        real*8        aux_agc_chg(max_agc,1600)
        integer*4     aux_agc_cnt(1600)
        integer*4     prf_num

c qdn 6/18/97
        integer*4     beam_mode


c  ARGUMENTS
	integer n_times_pre
        integer burst_total
        integer nbeams
        integer ire_sim
        real*8 gain_rec(burst_max)
        integer ns(burst_max)
        integer np(burst_max)
        real*8 dr(burst_max)
        real*8 df(burst_max)
        integer dk_burst(burst_max)
        character*60 file_spc_raw
        character*60 file_raw
        integer data_cond
        complex tbl(-128:127)
        integer blk_gain(iq_blk,burst_max)
        real*8 snr(burst_max)
        real*8 chirp_rate,fs,plen
        integer nfft_az
        real*8 prf(burst_max)
        integer prf_est_mode
        integer np_proc
        integer beam_no(burst_max)
        integer beam_id(burst_max)
        real*8 az_peak_coef(3,8)
        real*8 dlk_ang(burst_max)
        real*8 x_sc(burst_max)
        real*8 y_sc(burst_max)
        real*8 z_sc(burst_max)
        real*8 v_x_sc(burst_max)
        real*8 v_y_sc(burst_max)
        real*8 v_z_sc(burst_max)
        real*8 peak_bm_ang(4)
        integer rgc_est_mode
        integer burst_cnt(burst_max)
        real*8 roll_o(burst_max)
        real*8 yaw_o(burst_max)
        real*8 pitch_o(burst_max)
        real*8 roll(burst_max)
        real*8 yaw(burst_max)
        real*8 pitch(burst_max)
        byte buff0(2*sam_raw,line_raw)
        complex buff1(sam_raw,line_raw)
        complex buff2(line_raw,sam_raw)
        integer nr
        real*8 rng_bm_ang(4)
        real*8 look_ang(burst_max)
        integer max_look
        integer proc_mode
        real*8 fm_sizec2_in
        real*8 c1_pxl
        real*8 c2_pxl
        real*8 ele_ang_eq(4)
        real*8 alpha1,alpha2,alpha3
        integer dem_ellip
        real*8 dlon_topo,dlat_topo
        integer*2 topo(ns_ew,ns_ns)

	real*8		i_ang_bip(burst_max)	
	real*8		r_bip(burst_max)
	real*8		fd_bip(burst_max)	
	real*8		fr_bip(burst_max)
	real*8		coef_fd(4,burst_max)	
	real*8		coef_fr(4,burst_max)
	real*8		coef_lk_ang(4,burst_max)
	real*8		r_low(burst_max)
	real*8		r_1st(burst_max)	
	real*8		r_cntr(burst_max)
	real*8		r_high(burst_max)	
	real*8		fd_low_near(burst_max)
	real*8		fd_high_near(burst_max)	
	real*8		fd_near(burst_max)
	real*8		fr_near(burst_max)	
	real*8		pbw_near(burst_max)
	real*8		fd_low_cntr(burst_max)	
	real*8		fd_high_cntr(burst_max)
	real*8		fd_cntr(burst_max)
	real*8		fr_cntr(burst_max)	
	real*8		pbw_cntr(burst_max)
	real*8		fd_low_far(burst_max)	
	real*8		fd_high_far(burst_max)
	real*8		fd_far(burst_max)
	real*8		fr_far(burst_max)	
	real*8		pbw_far(burst_max)
	real*8		r_g(3,3,n_az,n_rg,burst_max)
	real*8		fd_g(3,3,n_az,n_rg,burst_max)
	real*8		c1_g(3,3,n_az,n_rg,burst_max)
	real*8		c2_g(3,3,n_az,n_rg,burst_max)
	integer*4	n_c1_blk(burst_max)         
	integer*4	n_c2_blk(burst_max) 	   
	real*8	        c1_flm(2,2,burst_max) 
	real*8	        c2_flm(2,2,burst_max)
	real*8          c1_cell(burst_max)  
	real*8          c2_cell(burst_max) 
	real*8          c1size_blk(burst_max) 
	real*8          c2size_blk(burst_max)
        integer         np_air(burst_max)
        real*8          v_sc_mag(burst_max)
	integer burst_total_trun,btotal,burst_k(800),bestk,bestp
	real contrast(800),s_power(800)
        real w1(64),w2(64),w3(64),w4(64)
	complex burst1(line_raw,sam_raw),burst2(line_raw,sam_raw)
        real*8    p_x(burst_max),p_y(burst_max),p_z(burst_max)
C WAYNE
        real*8   ta,tb
        integer  nburst_cpu
        integer  burst_count_start,burst_count_end
        integer  icollect_node,max_num_burst
        integer  iflag_cont(0:15),iflag_cont_d
        real     contrast_d,s_power_d
        real*8   coef_fd_d
        integer nbuf(4),dontcare,allmsg,nulltask,allgrp 
C WAYNE
       real*8     dfddth
       integer    pre_phase
       real*8     coef_fd_0(4,burst_max)
       real*8     coef_inc(4,burst_max)
       real*8     thi(5)
       real*8     tp
       real*8     az_pk_coef(3)
       real hufd(60,200,4)
       integer burst_check(1600)
       integer burst_k_count

c qdn 6/18/97
       real*8     offset_beam
       integer*4  iselect_beam
       
c qdn 7/25/97
        real*8    r_mid, fd_bestk_base, fd_bestk
        integer   no_prf_fix



        allgrp   = MPI_COMM_WORLD
	call init_fft(2048)
        do k = 1, burst_total
        roll(k) = 0.
        yaw(k) = 0.
        pitch(k) = 0.
        end do
c ********************** Start Phase 1 Preprocessing ***************
       





        pre_phase = 1
	n_times = n_times_pre 
c	btotal = (burst_total/nbeams)/n_times*n_times*nbeams
c       btotal = (burst_total-2*nbeams)/((n_times-1)*nbeams)*n_times*nbeams
        btotal = (burst_total-2*nbeams)/(n_times*nbeams)*(n_times*nbeams)

       ta=rtc()
       write(6,*)'PHASE 1: TOTAL OF ',btotal, 'bursts',burst_total
       write(6,*)'processing node ',burst_start_p,burst_end_p

c WAYNE
        itrouble = 0
        burst_count_start= -1
        burst_count_end  = -1
        nburst_cpu=0
	icount = 0
        do i =1, n_times
c        kg = 1 + (i-1)*(btotal/n_times)
         kg = 1 + (i-1)*(btotal/n_times/(nbeams*2)*(nbeams*2))
         do j = 1, nbeams
           icount=icount+1
           burst_k(icount) = kg+j-1
           IF ((burst_k(icount).ge.burst_start_p).and.(burst_k(icount).le.burst_end_p)) then
c
            if (.not.(((burst_k(icount)+nbeams).ge.burst_start_p).and.
     *              ((burst_k(icount)+nbeams).le.burst_end_p  ))   )then
             write(6,*)  'adjacent beam data not available in the node # ',mytask
             write(6,*)  'adjacent beam probleam, burst #',burst_k(icount)
             itrouble = -1
             goto  1111
            endif
            if (nburst_cpu.eq.0) then
             burst_count_start=icount
             burst_count_end  =burst_count_start
            else
             burst_count_end  =burst_count_end+1
            endif
            nburst_cpu=nburst_cpu+1
           ENDIF
         enddo
        enddo
1111    continue
        if (mytask.eq.0) then
          iitrouble = itrouble
          do i=1,kids-1
            call mpi_recv(itrouble,1,MPI_INTEGER,i,0,MPI_COMM_WORLD,isuccess,ierr)
            if (itrouble.eq.-1) iitrouble = itrouble
          enddo
        else
          call mpi_send(itrouble,1,MPI_INTEGER,0,0,MPI_COMM_WORLD,ierr)
        endif
        call mpi_bcast(iitrouble,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
        if (iitrouble.eq.-1) then
          istatus = ierr_18
          return
        endif
        burst_k_count=icount
       

        call init_burst_check(burst_k_count,burst_k,burst_good,isuccess,nbeams,
     *                        burst_start_p,burst_end_p,burst_count_start,burst_count_end,
     *                        mytask,kids)
c       if (isuccess.eq.0) then
c          istatus= ierr_20
c          return
c       endif



c WAYNE
        write(6,*)nburst_cpu,' COUNTS (phase1) FROM ',burst_count_start,' TO ',burst_count_end

        if (burst_count_start .ne. -1) then
 	do i = burst_count_start,burst_count_end
           call burst_pp_0(dfddth,pre_phase,thi,tp,coef_fd_0,
     *                        burst_k(i),v_sc_mag,v_x_sc,v_y_sc,v_z_sc,
     *                        rng_bm_ang,beam_no,az_pk_coef,az_peak_coef,
     *                        beam_id,x_sc,y_sc,z_sc,roll,yaw,pitch,
     *                        look_ang,ire_sim,dem_ellip,dlon_topo,dlat_topo,
     *                        topo,i_ang_bip,r_bip,fr_bip,fd_bip,coef_fd,
     *                        coef_fr,coef_lk_ang,nbeams,np,prf,np_air,
     *                        r_low,r_1st,fd_near,fr_near,r_high,ns,nr,dr,
     *                        fd_far,fr_far,r_cntr,fd_cntr,fr_cntr,
     *                        avg_terrain,coef_inc)
	end do
        endif
        call mpi_barrier(MPI_COMM_WORLD,ierr)

c       write(6,*) 'ARE WE HERE'

        call collect_coef_fd0(
     *     coef_fd_0,
     *     burst_count_start,burst_count_end,burst_k,
     *     mytask,kids,allgrp)

c       write(6,*) 'MADE IT collect'


c ********************** Start Phase 2 Preprocessing ***************************
        pre_phase = 2
        n_times = 3 !5
	n_times = n_times_pre ! qdn 6/8/97
c       btotal = (burst_total-2*nbeams)/((n_times-1)*nbeams)*n_times*nbeams
        btotal = (burst_total-2*nbeams)/(n_times*nbeams)*(n_times*nbeams)
        write(6,*)'PHASE 2: TOTAL OF ',btotal, 'bursts',burst_total
        write(6,*)'processing node ',burst_start_p,burst_end_p

c WAYNE
        itrouble = 0
        burst_count_start= -1
        burst_count_end  = -1
        nburst_cpu=0
        icount = 0
        do i =1, n_times
c        kg = 1 + (i-1)*btotal/n_times
         kg = 1 + (i-1)*(btotal/n_times/(nbeams*2)*(nbeams*2))
         do j = 1, nbeams
           icount=icount+1
           burst_k(icount) = kg+j-1
           IF ((burst_k(icount).ge.burst_start_p).and.(burst_k(icount).le.burst_end_p)) then
c
            if (.not.(((burst_k(icount)+nbeams).ge.burst_start_p).and.
     *              ((burst_k(icount)+nbeams).le.burst_end_p  ))   )then
             write(6,*)  'adjacent beam data not available in node # ',mytask
             write(6,*)  'adjacent beam probleam, burst #',burst_k(icount)
             itrouble = -1
             goto 2222
             return
            endif
            if (nburst_cpu.eq.0) then
             burst_count_start=icount
             burst_count_end  =burst_count_start
            else
             burst_count_end  =burst_count_end+1
            endif
            nburst_cpu=nburst_cpu+1
           ENDIF
         enddo
        enddo
2222    continue
        if (mytask.eq.0)  then
          iitrouble = itrouble
          do i=1,kids-1
            call mpi_recv(itrouble,1,MPI_INTEGER,i,0,MPI_COMM_WORLD,isuccess,ierr)
            if (itrouble.eq.-1) iitrouble = itrouble
          enddo
        else
          call mpi_send(itrouble,1,MPI_INTEGER,0,0,MPI_COMM_WORLD,ierr)
        endif
        call mpi_bcast(iitrouble,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
        if (iitrouble.eq.-1) then
          istatus = ierr_18
          return
        endif
        burst_k_count=icount


c WAYNE
        call init_burst_check(burst_k_count,burst_k,burst_good,isuccess,nbeams,
     *                        burst_start_p,burst_end_p,burst_count_start,burst_count_end,
     *                        mytask,kids)
c       if (isuccess.eq.0) then
c          istatus= ierr_20
c          return
c       endif

        write(6,*)nburst_cpu,' COUNTS (phase2) FROM ',burst_count_start,' TO ',burst_count_end

        if (burst_count_start .ne. -1) then
 	do i = burst_count_start,burst_count_end

          write(6,*)'WORKING ',burst_k(i)
          call clutter(burst_k(i),ire_sim,gain_rec,ns,
     *    np,dr,df,fs,dk_burst,file_spc_raw,data_cond,
     *    tbl,blk_gain,snr,buff0,buff1,buff2,chirp_rate,fd_bip,
     *    nfft_az,r_low,r_1st,r_bip,r_high,coef_fd,coef_fr,prf,
     *    nbeams,burst1,burst2,w1,w2,w3,w4,contrast(i),s_power(i),
     *    istatus,file_raw,nr,
     *    sim_eam_nsp,np_v,dk_rep,gain_rep,file_rep,
     *    aux_wdp_chg,aux_wdp_np,aux_agc_np,aux_wdp_cnt,aux_agc_chg,aux_agc_cnt,
     *    dfddth,pre_phase,coef_fd_0,thi,tp,v_sc_mag,x_sc,y_sc,z_sc,
     *    v_x_sc,v_y_sc,v_z_sc,rng_bm_ang,beam_no,beam_id,az_peak_coef,
     *    az_pk_coef,roll,yaw,pitch,look_ang,dem_ellip,dlon_topo,dlat_topo,
     *    topo,i_ang_bip,fr_bip,coef_lk_ang,np_air,fd_near,fr_near,
     *    fd_far,fr_far,r_cntr,fd_cntr,fr_cntr,avg_terrain,coef_inc,
     *    hufd)
          if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) go to 2223

        enddo 
        endif
2223    continue
        call mpi_barrier(MPI_COMM_WORLD,ierr)
        if (mytask.eq.0)  then
          do i=1,kids-1
            call mpi_recv(itrouble,1,MPI_INTEGER,i,0,MPI_COMM_WORLD,isuccess,ierr)
            if (itrouble.ne.0) istatus= itrouble
          enddo
        else
          call mpi_send(istatus,1,MPI_INTEGER,0,0,MPI_COMM_WORLD,ierr)
        endif
        call mpi_bcast(istatus,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
        if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return




C***
          write(6,*)'SSS',coef_fd(1,1)
          write(6,*)'SSS',coef_fd(2,1)
          write(6,*)'SSS',coef_fd(3,1)
          write(6,*)'SSS',coef_fd(4,1)
          write(6,*)'TTT2',coef_fd(1,2)
          write(6,*)'TTT2',coef_fd(2,2)
          write(6,*)'TTT2',coef_fd(3,2)
          write(6,*)'TTT2',coef_fd(4,2)
           call collect_var_clutter(
     *     r_bip,fd_bip,coef_fd,coef_fr,coef_lk_ang,r_low,r_high,
     *     contrast,s_power,dfddth,
     *     burst_count_start,burst_count_end,burst_k,nbeams,
     *     mytask,kids,allgrp,
     *     roll,yaw,pitch)
          write(6,*)'SSS',coef_fd(1,1)
          write(6,*)'SSS',coef_fd(2,1)
          write(6,*)'SSS',coef_fd(3,1)
          write(6,*)'SSS',coef_fd(4,1)

          write(6,*)'TTT2',coef_fd(1,2)
          write(6,*)'TTT2',coef_fd(2,2)
          write(6,*)'TTT2',coef_fd(3,2)
          write(6,*)'TTT2',coef_fd(4,2)




	 if(prf_est_mode.eq.1) then

           write (*,*) ' About to call best_burst'
C         write(6,*)'SSS',coef_fd(1,1)
C         write(6,*)'SSS',coef_fd(2,1)
C         write(6,*)'SSS',coef_fd(3,1)
C         write(6,*)'SSS',coef_fd(4,1)
c	    call best_burst(burst_k,nbeams,n_times,
c    *                      contrast,s_power,bestk) !best burst
c qdn 7/2/97 select 
c           call select_beam(nbeams,prf,iselect_beam)
c qdn 8/5/97 select beam  based on the mode
            if(beam_mode .eq. 1) then   ! SWA
              iselect_beam = 1 
            else if(beam_mode.eq.2) then ! SWB
              iselect_beam = 1
            else if(beam_mode.eq.3) then ! SNA
              iselect_beam = 0 
            else if(beam_mode.eq.4) then ! SNB
              iselect_beam = 0 
            else
              write(6,*)' ERROR in beam_mode',beam_mode
            end if
            write(6,*)' QDN_FD select the beam=',iselect_beam
            write(6,*)'   prf=',(prf(i),i=1,nbeams)

 	    call best_burst2(burst_k,nbeams,n_times,iselect_beam,
     *                      contrast,s_power,bestk) !best burst

          write(6,*)'SSS',coef_fd(1,bestk)
          write(6,*)'SSS',coef_fd(2,bestk)
          write(6,*)'SSS',coef_fd(3,bestk)
          write(6,*)'SSS',coef_fd(4,bestk)
            write(6,*)  ' best burst no. = ', bestk
c           IF ((bestk.ge.burst_start_p).and.(bestk.le.burst_end_p)) then
            IF ((bestk.ge.burst_start_p).and.
     *                    (bestk+16*nbeams.le.burst_end_p)) then
              if(prf_num.ne.9999) then
                no_prf = prf_num                        !use input prf number
              else
c qdn 7/1/97 modified to call fix_fd_B
c select using 3 for beam 3 and beam4
                write(6,*)' QDN_FD est_mode at=',bestk,' to ',bestk+15
c               call fix_fd_B(bestk,prf,r_low,
c    *            r_bip,r_high,coef_fd,no_prf,coef_lk_ang,
c    *            az_peak_coef,beam_id,dfddth)

c qdn 7/25/97 modified by MJ 

                r_mid = (r_high(bestk)+r_low(bestk+1))/2.
                call v_poly(r_mid,r_bip(bestk),coef_fd(1,bestk),4,fd_bestk)

                fd_bestk_base=mod(fd_bestk+100.5*prf(bestk),prf(bestk))
     *                          - prf(bestk)/2.

                write(6,*)'TTT3 coef_fd',bestk,(coef_fd(i,bestk),i=1,4)
                call  fix_fd_B2(bestk,no_prf,
     *            ire_sim,gain_rec,ns,
     *            np,dr,df,fs,dk_burst,file_spc_raw,data_cond,
     *            tbl,blk_gain,snr,buff0,buff1,buff2,chirp_rate,fd_bip,
     *            nfft_az,r_low,r_1st,r_bip,r_high,coef_fd,coef_fr,prf,
     *            nbeams,burst1,burst2,w1,w2,w3,w4,contrast,s_power,
     *            istatus,file_raw,nr,
     *            sim_eam_nsp,np_v,dk_rep,gain_rep,file_rep,
     *            aux_wdp_chg,aux_wdp_np,aux_agc_np,aux_wdp_cnt,
     *            aux_agc_chg,aux_agc_cnt,
     *            dfddth,pre_phase,coef_fd_0,thi,tp,v_sc_mag,
     *            x_sc,y_sc,z_sc,v_x_sc,v_y_sc,v_z_sc,
     *            rng_bm_ang,beam_no,beam_id,az_peak_coef,
     *            az_pk_coef,roll,yaw,pitch,look_ang,dem_ellip,
     *            dlon_topo,dlat_topo,
     *            topo,i_ang_bip,fr_bip,coef_lk_ang,np_air,fd_near,fr_near,
     *            fd_far,fr_far,r_cntr,fd_cntr,fr_cntr,avg_terrain,coef_inc,
     *            hufd)

 
c               call n_prf_rx(bestk,ire_sim,gain_rec,ns,
c    *            np,dr,df,fs,dk_burst,file_spc_raw,data_cond,np_proc,
c    *            tbl,blk_gain,snr,buff0,buff1,buff2,chirp_rate,fd_bip,
c    *            nfft_az,r_low,r_1st,r_bip,coef_fd,coef_fr,prf,nbeams,
c    *            burst1,burst2,w1,w2,w3,w4,no_prf,istatus,file_raw,nr, 
c    *            sim_eam_nsp,np_v,dk_rep,gain_rep,file_rep,
c    *            aux_wdp_chg,aux_wdp_np,aux_agc_np,aux_wdp_cnt,
c    *            aux_agc_chg,aux_agc_cnt)
                  if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
          write(6,*)'TTT',coef_fd(1,bestk)
          write(6,*)'TTT',coef_fd(2,bestk)
          write(6,*)'TTT',coef_fd(3,bestk)
          write(6,*)'TTT',coef_fd(4,bestk)
              endif
 
              no_prf_fix=no_prf-nint((fd_bestk-fd_bestk_base)/prf(bestk))

c             write(6,*)'PRF number #', no_prf
              write(6,*)' fd_bestk=',fd_bestk,' fd_bestk_base=',fd_bestk_base
              write(6,*)' the ratio of (fd_bestk-fd_bestk_base)/prf=',
     *                                 (fd_bestk-fd_bestk_base)/prf(bestk)
              write(6,*)' nint((fd_bestk-fdbestk_base)/prf(bestk)=',
     *                     nint((fd_bestk-fd_bestk_base)/prf(bestk))
              write(6,*)'absolute PRF #',no_prf,' PRF # change',no_prf_fix

              call fix_fd_A(bestk,n_times,btotal,no_prf_fix,r_low,r_bip,
     *                    r_high,prf,beam_no,coef_fd,nbeams,
     *                    burst_k_count,burst_k)
          write(6,*)'UUU',coef_fd(1,bestk)
          write(6,*)'UUU',coef_fd(2,bestk)
          write(6,*)'UUU',coef_fd(3,bestk)
          write(6,*)'UUU',coef_fd(4,bestk)
            ENDIF
            call return_node(burst_total,nbeams,kids,bestk,inode)
            call mpi_bcast(coef_fd     ,8*burst_max,MPI_REAL8,inode,MPI_COMM_WORLD,ierr)
 	 end if

c       do i = 1, n_times
c        kg = 1 + (i-1)*btotal/n_times
c        kg = 1 + (i-1)*(btotal/n_times/(nbeams*2)*(nbeams*2))
c        do j = 1, 1
c         kburst = kg+j-1
        do i = 1, burst_k_count
          kburst = burst_k(i)
          if (beam_no(kburst).eq.1) then
D         write(6,*)'WAIT',r_low(kburst)
D         write(6,*)'WAIT',r_bip(kburst)
D         write(6,*)'WAIT',r_high(kburst)
D         write(6,*)'WAIT',coef_fd(1,kburst)
D         write(6,*)'WAIT',coef_fd(2,kburst)
D         write(6,*)'WAIT',coef_fd(3,kburst)
D         write(6,*)'WAIT',coef_fd(4,kburst)
D         write(6,*)'WAIT',coef_lk_ang(1,kburst)
D         write(6,*)'WAIT',x_sc(kburst)
D         write(6,*)'WAIT',y_sc(kburst)
D         write(6,*)'WAIT',z_sc(kburst)
D         write(6,*)'WAIT',v_x_sc(kburst)
D         write(6,*)'WAIT',v_y_sc(kburst)
D         write(6,*)'WAIT',v_z_sc(kburst)
D         write(6,*)'WAIT','******************'
          call yaw_roll_pitch(kburst,az_peak_coef,beam_id,
     *    r_low,r_bip,r_high,coef_fd,coef_lk_ang,
     *    x_sc,y_sc,z_sc,v_x_sc,v_y_sc,v_z_sc,peak_bm_ang,
     *    beam_no,dem_ellip,dlon_topo,dlat_topo,topo,dfddth,
     *    yaw,roll,pitch,nbeams,avg_terrain)
          endif
c        enddo
        enddo



        call update_ryp(n_times,nbeams,btotal,burst_total,
     *  roll,yaw,pitch,burst_k,burst_k_count,beam_no)

c qdn 6/17/97 for testing only: add 0.3 to the roll angle
c       write(6,*)' QDN ADD 0.6 TO THE ROLL ARRAY for ',burst_total
c       do i=1,burst_total
c          roll(i) = roll(i) + 0.6
c       end do

c ********************** Start Phase 3 Preprocessing ***************************

        pre_phase = 3
        n_times = n_times_pre
c       btotal = (burst_total-2*nbeams)/((n_times-1)*nbeams)*n_times*nbeams
        btotal = (burst_total-2*nbeams)/(n_times*nbeams)*(n_times*nbeams)
        write(6,*)'PHASE 3: TOTAL OF ',btotal, 'bursts',burst_total
        write(6,*)'processing node ',burst_start_p,burst_end_p


c WAYNE
        itrouble = 0
        burst_count_start= -1
        burst_count_end  = -1
        nburst_cpu=0
        icount = 0
        do i =1, n_times
c        kg = 1 + (i-1)*btotal/n_times
         kg = 1 + (i-1)*(btotal/n_times/(nbeams*2)*(nbeams*2))
         do j = 1, nbeams
           icount=icount+1
           burst_k(icount) = kg+j-1
           IF ((burst_k(icount).ge.burst_start_p).and.(burst_k(icount).le.burst_end_p)) then
c
            if (.not.(((burst_k(icount)+nbeams).ge.burst_start_p).and.
     *              ((burst_k(icount)+nbeams).le.burst_end_p  ))   )then
             write(6,*)  'adjacent beam data not available in node # ',mytask
             write(6,*)  'adjacent beam probleam, burst #',burst_k(icount)
             itrouble = -1
             goto 3333
             return
            endif
            if (nburst_cpu.eq.0) then
             burst_count_start=icount
             burst_count_end  =burst_count_start
            else
             burst_count_end  =burst_count_end+1
            endif
            nburst_cpu=nburst_cpu+1
           ENDIF
         enddo
        enddo
3333    continue
        if (mytask.eq.0)  then
          iitrouble = itrouble
          do i=1,kids-1
            call mpi_recv(itrouble,1,MPI_INTEGER,i,0,MPI_COMM_WORLD,isuccess,ierr)
            if (itrouble.eq.-1) iitrouble = itrouble
          enddo
        else
          call mpi_send(itrouble,1,MPI_INTEGER,0,0,MPI_COMM_WORLD,ierr)
        endif
        call mpi_bcast(iitrouble,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
        if (iitrouble.eq.-1) then
          istatus = ierr_18
          return
        endif
        burst_k_count=icount


c WAYNE
        call init_burst_check(burst_k_count,burst_k,burst_good,isuccess,nbeams,
     *                        burst_start_p,burst_end_p,burst_count_start,burst_count_end,
     *                        mytask,kids)
c       if (isuccess.eq.0) then
c          istatus= ierr_20
c          return
c       endif
        write(6,*)nburst_cpu,' COUNTS (phase3) FROM ',burst_count_start,' TO ',burst_count_end

        if (burst_count_start .ne. -1) then
        do i = burst_count_start,burst_count_end


          write(6,*)'DOING BURST ',i,burst_k(i)
          call clutter(burst_k(i),ire_sim,gain_rec,ns,
     *    np,dr,df,fs,dk_burst,file_spc_raw,data_cond,
     *    tbl,blk_gain,snr,buff0,buff1,buff2,chirp_rate,fd_bip,
     *    nfft_az,r_low,r_1st,r_bip,r_high,coef_fd,coef_fr,prf,
     *    nbeams,burst1,burst2,w1,w2,w3,w4,contrast(i),s_power(i),
     *    istatus,file_raw,nr,
     *    sim_eam_nsp,np_v,dk_rep,gain_rep,file_rep,
     *    aux_wdp_chg,aux_wdp_np,aux_agc_np,aux_wdp_cnt,aux_agc_chg,aux_agc_cnt,
     *    dfddth,pre_phase,coef_fd_0,thi,tp,v_sc_mag,x_sc,y_sc,z_sc,
     *    v_x_sc,v_y_sc,v_z_sc,rng_bm_ang,beam_no,beam_id,az_peak_coef,
     *    az_pk_coef,roll,yaw,pitch,look_ang,dem_ellip,dlon_topo,dlat_topo,
     *    topo,i_ang_bip,fr_bip,coef_lk_ang,np_air,fd_near,fr_near,
     *    fd_far,fr_far,r_cntr,fd_cntr,fr_cntr,avg_terrain,coef_inc,
     *    hufd)
          if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) go to 3334

        enddo
        endif
3334    continue
        call mpi_barrier(MPI_COMM_WORLD,ierr)
        if (mytask.eq.0)  then
          do i=1,kids-1
            call mpi_recv(itrouble,1,MPI_INTEGER,i,0,MPI_COMM_WORLD,isuccess,ier)
            if (itrouble.ne.0) istatus= itrouble
          enddo
        else
          call mpi_send(istatus,1,MPI_INTEGER,0,0,MPI_COMM_WORLD,ierr)
        endif
        call mpi_bcast(istatus,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
        if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return



           call collect_var_clutter(
     *     r_bip,fd_bip,coef_fd,coef_fr,coef_lk_ang,r_low,r_high,
     *     contrast,s_power,dfddth,
     *     burst_count_start,burst_count_end,burst_k,nbeams,
     *     mytask,kids,allgrp,
     *     roll,yaw,pitch)
c         call collect_roll_yaw_pitch(
c    *     roll,yaw,pitch,
c    *     burst_count_start,burst_count_end,burst_k,nbeams,
c    *     mytask,kids,allgrp)

        if(rgc_est_mode.eq.1) then
          if (burst_count_start .ne. -1) then
          do i = burst_count_start,burst_count_end
            kburst = burst_k(i)
            i_at_beam=mod(kburst,nbeams)
c qdn 5/23/97
            write(6,*)' at kburst=',kburst,' i=',i,' burst_k(i)=',burst_k(i)
            write(6,*)' i_at_beam=',i_at_beam,' nbeams=',nbeams
            IF (i_at_beam.ne.0) then
              call rng_cnt(kburst,ire_sim,gain_rec,ns,
     *          np,dr,df,fs,plen,dk_burst,file_spc_raw,data_cond,np_proc,
     *          tbl,blk_gain,snr,buff0,buff1,buff2,chirp_rate,fd_bip,
     *          nfft_az,r_low,r_1st,r_bip,r_high,coef_fd,coef_fr,
     *          coef_lk_ang,prf,nbeams,burst1,burst2,beam_no,beam_id,
     *          ele_ang_eq,dlk_ang,istatus,file_raw,nr,
     *          sim_eam_nsp,np_v,dk_rep,gain_rep,file_rep,
     *          aux_wdp_chg,aux_wdp_np,aux_agc_np,aux_wdp_cnt,aux_agc_chg,
     *          aux_agc_cnt,dfddth,pre_phase,thi,tp,coef_fd_0,v_sc_mag,
     *          v_x_sc,v_y_sc,v_z_sc,rng_bm_ang,x_sc,y_sc,z_sc,
     *          az_peak_coef,az_pk_coef,roll,yaw,
     *          pitch,look_ang,dem_ellip,dlon_topo,dlat_topo,topo,
     *          i_ang_bip,fr_bip,np_air,fd_near,fr_near,fd_far,fr_far,
     *          r_cntr,fd_cntr,fr_cntr,buff_rep,ant_patn_r,avg_terrain,coef_inc)
                if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
            ELSE
c             dlk_ang(kburst+1) = dlk_ang(kburst)
c             write(6,*)' overide the dlk_ang=',dlk_ang(kburst+1),
c    *                  dlk_ang(kburst)
c qdn 6/3/97
              roll(kburst+1) = roll(kburst)
              write(6,*)'QDN at kburst=',kburst,
     *                  ' overide the roll=',roll(kburst+1),' by',roll(kburst)

            ENDIF
          end do
          endif
          call mpi_barrier(MPI_COMM_WORLD,ierr)
c         call collect_var_rng_cnt(
c    *     r_bip,fd_bip,coef_fd,coef_fr,coef_lk_ang,r_low,r_high,
c    *     dlk_ang,
c    *     burst_count_start,burst_count_end,burst_k,nbeams,
c    *     mytask,kids,allgrp)
c qdn 6/16/97
          call collect_var_rng_cnt2(
     *     r_bip,fd_bip,coef_fd,coef_fr,coef_lk_ang,r_low,r_high,
     *     dlk_ang,roll,
     *     burst_count_start,burst_count_end,burst_k,nbeams,
     *     mytask,kids,allgrp)

c qdn  6/18/97
c Update the roll vector based on the beam_mode
c 
           if(beam_mode .eq. 1) then   ! SWA
              iselect_beam = 0 
              offset_beam  = 0.0 
           else if(beam_mode.eq.2) then ! SWB
              iselect_beam = 2
              offset_beam  = 0.0
c             iselect_beam = 1
c             offset_beam  = 0.3
           else if(beam_mode.eq.3) then ! SNA
              iselect_beam = 0 
              offset_beam  = 0.0
           else if(beam_mode.eq.4) then ! SNB
              iselect_beam = 1 
              offset_beam  = 0.3 
           else
              write(6,*)' ERROR in beam_mode',beam_mode
           end if
           write(6,*)' QDN update the roll based on the mode',beam_mode,
     *               'isel=',iselect_beam,'offset_val=',offset_beam,
     *               'nbeams=',nbeams
           do i=1,burst_total
              if(mod(i,nbeams).eq.1) then
                 roll(i) = roll(i+iselect_beam) + offset_beam 
              end if
           end do

	end if

        call update_ryp(n_times,nbeams,btotal,burst_total,
     *  roll,yaw,pitch,burst_k,burst_k_count,beam_no)

        if (mytask.eq.0) then
         do i=1,burst_total
          write(6,*)'ROLL,YAW,PITCH',roll(i),yaw(i),pitch(i)
         enddo
        endif


     



	return
	end


	subroutine yaw_roll_pitch(kburst,az_peak_coef,beam_id,
     *  r_low,r_bip,r_high,coef_fd,coef_lk_ang,
     *  x_sc,y_sc,z_sc,v_x_sc,v_y_sc,v_z_sc,peak_bm_ang,
     *  beam_no,dem_ellip,dlon_topo,dlat_topo,topo,dfddth,
     *  yaw,roll,pitch,nbeams,avg_terrain)
	implicit real*8 (a-h,o-z)
	include 'ssp2_const.inc'
	real*8 p1(3),p2(3),n(3),az_pk_coef(3),n4
        real*8 az_peak_coef(3,8)
        integer beam_id(burst_max),beam_no(burst_max)
        real*8 r_low(burst_max),r_bip(burst_max),r_high(burst_max)
        real*8 coef_fd(4,burst_max)
        real*8 coef_lk_ang(4,burst_max)
        real*8 x_sc(burst_max)
        real*8 y_sc(burst_max)
        real*8 z_sc(burst_max)
        real*8 v_x_sc(burst_max)
        real*8 v_y_sc(burst_max)
        real*8 v_z_sc(burst_max)
        real*8 peak_bm_ang(4)
        integer dem_ellip
        real*8 dlon_topo
        real*8 dlat_topo
        integer*2 topo(ns_ew,ns_ns)
        real*8  p_x(burst_max),p_y(burst_max),p_z(burst_max)
        real*8 dfddth                                !  JMS 5-96
        real*8 pc(3),pbip(3)                         !  5-96 
        real*8 yaw(burst_max),roll(burst_max),pitch(burst_max)
        real*8 avg_terrain

        h = avg_terrain
	m = 4
	k1 = kburst
	az_pk_coef(1) = az_peak_coef(1,beam_id(k1))
	az_pk_coef(2) = az_peak_coef(2,beam_id(k1))
	az_pk_coef(3) = az_peak_coef(3,beam_id(k1))

	call v_poly(r_low(k1),r_bip(k1),coef_fd(1,k1),m,fd10)
	call v_poly(r_low(k1),r_bip(k1),coef_lk_ang(1,k1),m,anglk_low)
	az_pk_ang = az_pk_coef(1)+az_pk_coef(2)*anglk_low+
     1		    az_pk_coef(3)*anglk_low**2
	fd1 = fd10 - dfddth*az_pk_ang
        write(6,*)'YRP',kburst,fd1,h
        write(6,*)'YRP',dfddth,r_low(k1)
        write(6,*)'YRP',x_sc(k1),y_sc(k1),z_sc(k1)
        write(6,*)'YRP',v_x_sc(k1),v_y_sc(k1),v_z_sc(k1)
	call gttarg1(r_low(k1),fd1,x_sc(k1),y_sc(k1),z_sc(k1),
     *	v_x_sc(k1),v_y_sc(k1),v_z_sc(k1),h,
     *  p_x(k1),p_y(k1),p_z(k1),
     *	xt,yt,zt,vxt,vyt,vzt,alkang,yaw1,
     *  dem_ellip,dlon_topo,dlat_topo,topo)     
	call pt_a2b(x_sc(k1),y_sc(k1),z_sc(k1),xt,yt,zt,p1)
        write(6,*)'FIRST',xt,yt,zt
        write(6,*)'FIRST',p1(1),p1(2),p1(3)

	koff = nbeams-1
	az_pk_coef(1) = az_peak_coef(1,beam_id(k1+koff))
	az_pk_coef(2) = az_peak_coef(2,beam_id(k1+koff))
	az_pk_coef(3) = az_peak_coef(3,beam_id(k1+koff))

	call v_poly(r_high(k1+koff),r_bip(k1+koff),coef_fd(1,k1+koff),m,fd20)
	call v_poly(r_high(k1+koff),r_bip(k1+koff),coef_lk_ang(1,k1+koff),
     *    m,anglk_high)
	az_pk_ang = az_pk_coef(1)+az_pk_coef(2)*anglk_high+
     1		    az_pk_coef(3)*anglk_high**2
	fd2 = fd20 - dfddth*az_pk_ang
	call gttarg1(r_high(k1+koff),fd2,x_sc(k1),y_sc(k1),z_sc(k1),
     *	v_x_sc(k1),v_y_sc(k1),v_z_sc(k1),h,
     *  p_x(k1),p_y(k1),p_z(k1),
     *	xt,yt,zt,vxt,vyt,vzt,alkang,yaw1,
     *  dem_ellip,dlon_topo,dlat_topo,topo) 
	call pt_a2b(x_sc(k1),y_sc(k1),z_sc(k1),xt,yt,zt,p2)
        write(6,*)'FIRST2',xt,yt,zt
        write(6,*)'FIRST2',p2(1),p2(2),p2(3)


	call cross(p2(1),p2(2),p2(3),p1(1),p1(2),p1(3),pc(1),pc(2),pc(3))
	call get_n(pc(1),pc(2),pc(3),x_sc(k1),y_sc(k1),z_sc(k1),
     1		v_x_sc(k1),v_y_sc(k1),v_z_sc(k1),n(1),n(2),n(3))

	anglk_bip = coef_lk_ang(1,k1)
	az_pk_coef(1) = az_peak_coef(1,beam_id(k1))
	az_pk_coef(2) = az_peak_coef(2,beam_id(k1))
	az_pk_coef(3) = az_peak_coef(3,beam_id(k1))
	az_pk_ang = az_pk_coef(1)+az_pk_coef(2)*anglk_bip+
     1		    az_pk_coef(3)*anglk_bip**2
	fdbip = coef_fd(1,k1) - dfddth*az_pk_ang
	call gttarg1(r_bip(k1),fdbip,x_sc(k1),y_sc(k1),z_sc(k1),
     *	v_x_sc(k1),v_y_sc(k1),v_z_sc(k1),h,
     *	p_x(k1),p_y(k1),p_z(k1),	!AC 11/09/95
     *	xt,yt,zt,vxt,vyt,vzt,alkang,yaw1,     
     *  dem_ellip,dlon_topo,dlat_topo,topo)  
	call pt_a2b(x_sc(k1),y_sc(k1),z_sc(k1),xt,yt,zt,pbip)
	n4=-1.0d0*(pbip(1)*x_sc(k1)+pbip(2)*y_sc(k1)+pbip(3)*z_sc(k1))
     1		/sqrt(x_sc(k1)**2+y_sc(k1)**2+z_sc(k1)**2)

 	write(6,*) 'fd10,fd20,fdbip0', fd10,fd20,coef_fd(1,k1)
 	write(6,*) 'r_low,r_bip,r_hig,fd1,fdbip,fd2',r_low(k1),r_bip(k1),r_high(k1),fd1,fdbip,fd2
 	write(6,*) 'anglw,anghi', anglk_low,anglk_high
 	write(6,*) 'n(1),n(2),n(3),n4', n(1),n(2),n(3),n4
	call solve_yrp(n(1),n(2),n(3),n4,anglk_bip,yawp,rollp,pitchp)

        write (*,*) 'yawp,rollp,pitchp',yawp,rollp,pitchp
        yaw(kburst)=yawp
        roll(kburst)=rollp
        pitch(kburst)=pitchp

	return
	end

        subroutine get_n(pc1,pc2,pc3,xsc,ysc,zsc,vxsc,vysc,vzsc,n1,n2,n3)

        real*8 pc1,pc2,pc3,xsc,ysc,zsc,vxsc,vysc,vzsc,n1,n2,n3,c1,c2,c3
        real*8 d1,d2,d3,rs,vs

        rs = sqrt(xsc**2+ysc**2+zsc**2)
        vs = sqrt(vxsc**2+vysc**2+vzsc**2)
        call cross(vxsc,vysc,vzsc,xsc,ysc,zsc,c1,c2,c3)
        call cross(xsc,ysc,zsc,c1,c2,c3,d1,d2,d3)
        n1 = pc1*d1+pc2*d2+pc3*d3
        n3 = -(pc1*xsc+pc2*ysc+pc3*zsc)/rs
        n2 = pc1*c1+pc2*c2+pc3*c3

        return
        end



	subroutine pt_a2b(x1,y1,z1,x2,y2,z2,p)
	implicit real*8 (a-h,o-z)
	real*8 p(3)

	dx = x2-x1
	dy = y2-y1
	dz = z2-z1
	dis = sqrt(dx**2+dy**2+dz**2)
	p(1) = dx/dis
	p(2) = dy/dis
	p(3) = dz/dis

	return
	end


	subroutine solve_yrp(n1,n2,n3,n4,alk,yaw,roll,pitch)
	
	implicit real*8 (a-h,o-z)
	real*8 n1,n2,n3,n4

        roll = 0.
        yaw = 0.
        pitch = 0.
        c1 = cosd(alk)
        c2 = sind(alk)

        d = 0.
        e = 0.
        do k = 1, 10
        sy = -n2/cosd(roll)
        sp = (n3-cosd(pitch)*sind(roll)*sind(yaw))/cosd(yaw)
        yaw = asind(sy)
        pitch = asind(sp)
        sr = -((n4-c1*cosd(roll)*cosd(pitch))/c2-sy*sp)/(cosd(p1)*cosd(y1))
        roll = asind(sr)
c        type *, 'k,yaw,roll,pitch=', k,yaw,roll,pitch
        end do





	end


	subroutine rng_cnt(kburst,ire_sim,gain_rec,ns,
     *  np,dr,df,fs,plen,dk_burst,file_spc_raw,data_cond,np_proc,
     *  tbl,blk_gain,snr,buff0,buff1,buff2,chirp_rate,fd_bip,
     *  nfft_az,r_low,r_1st,r_bip,r_high,coef_fd,coef_fr,
     *  coef_lk_ang,prf,nbeams,burst1,burst2,beam_no,beam_id,
     *  ele_ang_eq,dlk_ang,istatus,file_raw,nr,
     *  sim_eam_nsp,np_v,dk_rep,gain_rep,file_rep,
     *  aux_wdp_chg,aux_wdp_np,aux_agc_np,aux_wdp_cnt,aux_agc_chg,
     *  aux_agc_cnt,dfddth,pre_phase,thi,tp,coef_fd_0,v_sc_mag,
     *  v_x_sc,v_y_sc,v_z_sc,rng_bm_ang,x_sc,y_sc,z_sc,
     *  az_peak_coef,az_pk_coef,roll,yaw,
     *  pitch,look_ang,dem_ellip,dlon_topo,dlat_topo,topo,
     *  i_ang_bip,fr_bip,np_air,fd_near,fr_near,fd_far,fr_far,
     *  r_cntr,fd_cntr,fr_cntr,buff_rep,ant_patn_r,avg_terrain,coef_inc)

c       implicit real*8 (a-h,o-z)
        implicit  none

        character*128 SccsId_pta
        data SccsId_pta
     +  /'@(#)PPpta.f:2.41'/

	include 'ssp2_const.inc'
	include 'ssp2_dim.inc'
        integer  kburst
	integer pmod_i
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
c**************************************
        integer ire_sim
        real*8 gain_rec(burst_max),dr(burst_max)
        real*8 df(burst_max)
        real*8  fs,plen
        integer ns(burst_max),np(burst_max)
        integer dk_burst(burst_max)
        integer beam_no(burst_max)
        integer beam_id(burst_max)
        character*60 file_spc_raw
        integer data_cond,np_proc
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
        real*8 coef_lk_ang(4,burst_max)
        real*8 r_low(burst_max)
        real*8 r_1st(burst_max)
        real*8 r_bip(burst_max)
        real*8 r_high(burst_max)
        integer nfft_az,nbeams
        real*8 prf(burst_max)
        real*8 dlk_ang(burst_max)
        real*8 ele_ang_eq(4)
        real ant_patn_r(ns_ant_rg,7)
        character*60 file_raw
        integer nr
        real w1(64),w2(64),w3(64),w4(64)

        real*8 dfddth
        integer pre_phase
        real*8 thi(5)
        real*8 tp
        real*8 coef_fd_0(4,burst_max)
        real*8 v_sc_mag(burst_max)
        real*8 v_x_sc(burst_max)
        real*8 v_y_sc(burst_max)
        real*8 v_z_sc(burst_max)
        real*8 x_sc(burst_max)
        real*8 y_sc(burst_max)
        real*8 z_sc(burst_max)
        real*8 rng_bm_ang(4)
        real*8 az_peak_coef(3,8)
        real*8 az_pk_coef(3)
        real*8 roll(burst_max)
        real*8 yaw(burst_max)
        real*8 pitch(burst_max)
        real*8 look_ang(burst_max)
        integer dem_ellip
        real*8 dlon_topo,dlat_topo
        integer topo(ns_ew,ns_ns)
        real*8 i_ang_bip(burst_max),fr_bip(burst_max)
        integer np_air(burst_max)
        real*8 fd_near(burst_max),fr_near(burst_max)
        real*8 fd_far(burst_max),fr_far(burst_max)
        real*8 r_cntr(burst_max),fd_cntr(burst_max),fr_cntr(burst_max)

        integer istatus

c Local variables
c qdn 5/23/97
        real*8 droll

c qdn 5/27/97
        complex burst1(line_raw,sam_raw),burst2(line_raw,sam_raw)
        complex temp1,temp2,intp
        real*8  dr_migra,pos,r_zero,fd,fr,ai20,f_off,ang_p
        real*8  ratio_db,ratio,sum,r_low_in,r_high_in,fd_cnt,r

        real    acu(4096),ai1,ai2,aj1,aj2

        integer i,j,k,kcnt,keq,nk,n_ovlap,lines_b,m,margin,nset

c qdn 5/27/97
        write(*,*)' Welcome to rng_cnt routine,roll=',roll(kburst)
        call weight(w1,w2,w3,w4,64)
        do i=kburst,kburst+3
        call burst_pp_0(dfddth,pre_phase,thi,tp,coef_fd_0,
     *                        i,v_sc_mag,v_x_sc,v_y_sc,v_z_sc,
     *                        rng_bm_ang,beam_no,az_pk_coef,az_peak_coef,
     *                        beam_id,x_sc,y_sc,z_sc,roll,yaw,pitch,
     *                        look_ang,ire_sim,dem_ellip,dlon_topo,dlat_topo,
     *                        topo,i_ang_bip,r_bip,fr_bip,fd_bip,coef_fd,
     *                        coef_fr,coef_lk_ang,nbeams,np,prf,np_air,
     *                        r_low,r_1st,fd_near,fr_near,r_high,ns,nr,dr,
     *                        fd_far,fr_far,r_cntr,fd_cntr,fr_cntr,avg_terrain,coef_inc)
        write(6,*) 'qdn after calling burst_pp_0',i
        write(6,*) 'qdn dr(i)=',dr(i)
        write(6,*) 'qdn r_low(i)=',r_low(i)
        write(6,*) 'qdn r_high(i)=',r_high(i)
        write(6,*) 'qdn roll(i)=',roll(i)
        end do


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



        write(6,*) ' before calling burst_pp_0',kburst
        call burst_pp_0(dfddth,pre_phase,thi,tp,coef_fd_0,
     *                        kburst,v_sc_mag,v_x_sc,v_y_sc,v_z_sc,
     *                        rng_bm_ang,beam_no,az_pk_coef,az_peak_coef,
     *                        beam_id,x_sc,y_sc,z_sc,roll,yaw,pitch,
     *                        look_ang,ire_sim,dem_ellip,dlon_topo,dlat_topo,
     *                        topo,i_ang_bip,r_bip,fr_bip,fd_bip,coef_fd,
     *                        coef_fr,coef_lk_ang,nbeams,np,prf,np_air,
     *                        r_low,r_1st,fd_near,fr_near,r_high,ns,nr,dr,
     *                        fd_far,fr_far,r_cntr,fd_cntr,fr_cntr,avg_terrain,coef_inc)
        write(6,*) ' after calling burst_pp_0',kburst
        write(6,*) 'dr(kburst)=',dr(kburst)
        write(6,*) 'r_low_in=',r_low_in,' r_high_in=',r_high_in
        write(6,*) 'r_low(kburst)=',r_low(kburst)
        write(6,*) 'r_low(kburst+1)=',r_low(kburst+1)
        write(6,*) 'r_high(kburst)=',r_high(kburst)
        write(6,*) 'r_high(kburst+1)=',r_high(kburst+1)
        write(6,*) 'roll(kburst)=',roll(kburst)
        write(6,*) 'roll(kburst+1)=',roll(kburst+1)

c       if (ire_sim.eq.0) then
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
         if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
c       endif
        call range_comp(kburst,nr,fs,chirp_rate,fd_bip,
     *  nfft_az,np_proc,ns,r_1st,dr,r_bip,coef_fd,
     *  prf,buff1,np_v,buff_rep,ire_sim,   
     *  dk_rep,gain_rep,file_rep,istatus,tbl)
        if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
        call corner_turn(kburst,nr,ns,np_proc,nfft_az,
     *  buff1,buff2)                                           !data corner turn
        call az_comp(kburst,nr,np_proc,nfft_az,ns,
     *  r_1st,dr,r_bip,coef_fd,coef_fr,prf,buff2)  

c qdn 5/27/97
        write(6,*)' line_raw=',line_raw,' sam_raw=',sam_raw
        write(6,*)' ns(kburst)=',ns(kburst),' nfft_az=',nfft_az
	do i = 1, ns(kburst)
	do j = 1, nfft_az
	burst1(j,i) = buff2(j,i)
	end do
	end do


        write(6,*)' before calling burst_pp_0',kburst+1
        call burst_pp_0(dfddth,pre_phase,thi,tp,coef_fd_0,
     *                        (kburst+1),v_sc_mag,v_x_sc,v_y_sc,v_z_sc,
     *                        rng_bm_ang,beam_no,az_pk_coef,az_peak_coef,
     *                        beam_id,x_sc,y_sc,z_sc,roll,yaw,pitch,
     *                        look_ang,ire_sim,dem_ellip,dlon_topo,dlat_topo,
     *                        topo,i_ang_bip,r_bip,fr_bip,fd_bip,coef_fd,
     *                        coef_fr,coef_lk_ang,nbeams,np,prf,np_air,
     *                        r_low,r_1st,fd_near,fr_near,r_high,ns,nr,dr,
     *                        fd_far,fr_far,r_cntr,fd_cntr,fr_cntr,avg_terrain,coef_inc)
        write(6,*)' after calling burst_pp_0',kburst+1
        write(6,*) 'dr(kburst)=',dr(kburst)
        write(6,*) 'r_low_in=',r_low_in,' r_high_in=',r_high_in
        write(6,*) 'r_low(kburst)=',r_low(kburst)
        write(6,*) 'r_low(kburst+1)=',r_low(kburst+1)
        write(6,*) 'r_high(kburst)=',r_high(kburst)
        write(6,*) 'r_high(kburst+1)=',r_high(kburst+1)
c       if (ire_sim.eq.0) then
c        call get_burst_sim(
c    *   (kburst+1),plen,np_proc,fs,file_spc_raw,
c    *   ns,buff0,istatus,buff1,dk_burst,file_raw,
c    *   sim_eam_nsp)
c       else
         call get_burst(
     *   (kburst+1),plen,ire_sim,gain_rec,ns,
     *   np,dk_burst,file_spc_raw,data_cond,
     *   np_proc,fs,tbl,blk_gain,snr,
     *   buff0,buff1,istatus,file_raw,nr,np_v,
     *   aux_wdp_chg,aux_wdp_np,aux_agc_np,aux_wdp_cnt,
     *   aux_agc_chg,aux_agc_cnt)
         if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
c       endif
         call range_comp((kburst+1),nr,fs,chirp_rate,fd_bip,
     *   nfft_az,np_proc,ns,r_1st,dr,r_bip,coef_fd,
     *   prf,buff1,np_v,buff_rep,ire_sim,  
     *   dk_rep,gain_rep,file_rep,istatus,tbl)
         if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
         call corner_turn((kburst+1),nr,ns,np_proc,nfft_az,
     *   buff1,buff2) 
         call az_comp((kburst+1),nr,np_proc,nfft_az,ns,
     *   r_1st,dr,r_bip,coef_fd,coef_fr,prf,buff2)                    !azimuth comopression

c qdn 5/27/97
        write(6,*)' ns(kburst+1)=',ns(kburst+1),' nfft_az=',nfft_az
	do i = 1, ns(kburst+1)
	do j = 1, nfft_az
	burst2(j,i) = buff2(j,i)
	end do
	end do

c******** input parameters *******************
	m = 4
	nset = 1				!no of blocks			
	margin = 20
	tp = (np(kburst)/2)/prf(kburst)+
     *       (np(kburst+1)/2)/prf(kburst+1)

c qdn 5/28/97
c       do i=kburst,kburst+3
c       call burst_pp_0(dfddth,pre_phase,thi,tp,coef_fd_0,
c    *                        i,v_sc_mag,v_x_sc,v_y_sc,v_z_sc,
c    *                        rng_bm_ang,beam_no,az_pk_coef,az_peak_coef,
c    *                        beam_id,x_sc,y_sc,z_sc,roll,yaw,pitch,
c    *                        look_ang,ire_sim,dem_ellip,dlon_topo,dlat_topo,
c    *                        topo,i_ang_bip,r_bip,fr_bip,fd_bip,coef_fd,
c    *                        coef_fr,coef_lk_ang,nbeams,np,prf,np_air,
c    *                        r_low,r_1st,fd_near,fr_near,r_high,ns,nr,dr,
c    *                        fd_far,fr_far,r_cntr,fd_cntr,fr_cntr,avg_terrain,coef_inc)
c       write(6,*) 'qdn_2ndtime after calling burst_pp_0',i
c       write(6,*) 'qdn_2ndtime dr(i)=',dr(i)
c       write(6,*) 'qdn_2ndtime r_low(i)=',r_low(i)
c       write(6,*) 'qdn_2ndtime r_high(i)=',r_high(i)
c       write(6,*) 'qdn_2ndtime dfddth',dfddth,' pre_phase=',pre_phase
c       write(6,*) 'qdn_2ndtime tp=',tp
c       end do

c******** initialize buffer ******************	

	r_low_in = max(r_low(kburst),r_low(kburst+1))
	r_high_in = min(r_high(kburst),r_high(kburst+1))
	lines_b = int(((r_high_in-r_low_in)/dr(kburst)-margin)/nset) 

	n_ovlap = nint((r_high_in-r_low_in)/dr(kburst))-1
        write(6,*) 'novlap',n_ovlap,' kburst=',kburst
        write(6,*) 'dr(kburst)=',dr(kburst)
        write(6,*) 'r_low_in=',r_low_in,' r_high_in=',r_high_in
        write(6,*) 'r_low(kburst)=',r_low(kburst)
        write(6,*) 'r_low(kburst+1)=',r_low(kburst+1)
        write(6,*) 'r_high(kburst)=',r_high(kburst)
        write(6,*) 'r_high(kburst+1)=',r_high(kburst+1)

	
	do i = 1, n_ovlap
	r = r_low_in + (i-1)*dr(kburst)
	call v_poly(r,r_bip(kburst),coef_fd(1,kburst),m,fd_cnt)
	call v_poly(r,r_bip(kburst),coef_fr(1,kburst),m,fr)
	ai1 = ((r-r_low(kburst))/dr(kburst))+1
	ai20 = ((r-r_low(kburst+1))/dr(kburst+1))+1
	f_off = abs(fr)*tp

c********** take log of the ratio ****************
	acu(i) = 0.0
	do j = 1, nfft_az-f_off/df(kburst)-1
	fd = fd_cnt+(j-nfft_az/2-1)*df(kburst+1)+f_off
        dr_migra = lambda/2.0d0*(fd+fr/2*tp)*tp/dr(kburst)
        ai2 = ai20-dr_migra
	aj2 = j
	temp2 = intp(burst2,nfft_az,line_raw,sam_raw,aj2,ai2,2,
     *               w1,w2,w3,w4)
	aj1 = j+f_off/df(kburst)
	temp1 = intp(burst1,nfft_az,line_raw,sam_raw,aj1,ai1,1,
     *               w1,w2,w3,w4)
c	if(j/10*10.eq.j.and.i/20*20.eq.i)
c    *   write(6,*) 'temp12,i,j,ai1,aj1,ai2,aj2',
c    *               temp1,temp2,i,j,ai1,aj1,ai2,aj2
        acu(i) = acu(i)+ log(abs(temp1/temp2))
c       if(abs(temp2).ne.0.0 .and. abs(temp1).ne.0.0)
c    *               acu(i) = acu(i)+ log(abs(temp1/temp2))
	end do
	acu(i) = 2*acu(i)/nint(nfft_az-f_off/df(kburst))
c	write(6,*) 'acu(i)= ', acu(i)
	end do

c qdn 5/28/97
c       write(6,*)' within the loop kburst=',kburst
c       do j=kburst,kburst+3
c       write(6,*) i,'qdn_3rdtime after calling burst_pp_0',j
c       call burst_pp_0(dfddth,pre_phase,thi,tp,coef_fd_0,
c    *                        j,v_sc_mag,v_x_sc,v_y_sc,v_z_sc,
c    *                        rng_bm_ang,beam_no,az_pk_coef,az_peak_coef,
c    *                        beam_id,x_sc,y_sc,z_sc,roll,yaw,pitch,
c    *                        look_ang,ire_sim,dem_ellip,dlon_topo,dlat_topo,
c    *                        topo,i_ang_bip,r_bip,fr_bip,fd_bip,coef_fd,
c    *                        coef_fr,coef_lk_ang,nbeams,np,prf,np_air,
c    *                        r_low,r_1st,fd_near,fr_near,r_high,ns,nr,dr,
c    *                        fd_far,fr_far,r_cntr,fd_cntr,fr_cntr,avg_terrain,coef_inc)
c       write(6,*) i,'qdn_3rdtime dr(j)=',dr(j)
c       write(6,*) i,'qdn_3rdtime r_low(j)=',r_low(j)
c       write(6,*) i,'qdn_3rdtime r_high(j)=',r_high(j)
c       write(6,*) i,'qdn_3rdtime dfddth',dfddth,' pre_phase=',pre_phase
c       write(6,*) i,'qdn_3rdtime tp=',tp
c       end do
c qdn 5/28/97


c************* zero cross detection *****************
	call zero_cross_rng(acu,n_ovlap,pos)

c qdn 5/28/97
c       do i=kburst,kburst+3
c       call burst_pp_0(dfddth,pre_phase,thi,tp,coef_fd_0,
c    *                        i,v_sc_mag,v_x_sc,v_y_sc,v_z_sc,
c    *                        rng_bm_ang,beam_no,az_pk_coef,az_peak_coef,
c    *                        beam_id,x_sc,y_sc,z_sc,roll,yaw,pitch,
c    *                        look_ang,ire_sim,dem_ellip,dlon_topo,dlat_topo,
c    *                        topo,i_ang_bip,r_bip,fr_bip,fd_bip,coef_fd,
c    *                        coef_fr,coef_lk_ang,nbeams,np,prf,np_air,
c    *                        r_low,r_1st,fd_near,fr_near,r_high,ns,nr,dr,
c    *                        fd_far,fr_far,r_cntr,fd_cntr,fr_cntr,avg_terrain,coef_inc)
c       write(6,*) 'qdn_4thtime after calling burst_pp_0',i
c       write(6,*) 'qdn_4thtime dr(i)=',dr(i)
c       write(6,*) 'qdn_4thtime r_low(i)=',r_low(i)
c       write(6,*) 'qdn_4thtime r_high(i)=',r_high(i)
c       write(6,*) 'qdn_4thtime dfddth',dfddth,' pre_phase=',pre_phase
c       write(6,*) 'qdn_4thtime tp=',tp
c       end do

        write(6,*) '***************************pos=', pos
cif(pos.ne.9999) then
c qdn 5/27/97
        if(int(pos).ne.9999) then
        write(6,*) '***************************pos=', pos
        write(6,*) 'pos=', pos,(r_high_in-r_low_in)/2.
c       r_zero = r_high_in+(pos-1.0d0)*dr(kburst)
c qdn 6/20/97
        r_zero = r_low_in+(pos-1.0d0)*dr(kburst)
        write(6,*) 'r_zero=',r_zero
        call v_poly(r_zero,r_bip(kburst),coef_lk_ang(1,kburst),m,ang_p)
        write(6,*) 'ang_p=',ang_p
        kcnt = nint((ang_p -look_ang_cnt-ele_ang_ref)/dang_rg) + 1
        write(6,*) 'kcnt=',kcnt,' keq=',keq,' before ant_eq_bin'
        call ant_eq_bin(kcnt,kburst,beam_no,beam_id,
     *                        ant_patn_r,keq)
        write(6,*) 'keq=',keq
c qdn 6/23/97 change the sign
c       droll = (kcnt - keq)*dang_rg
        droll = (keq - kcnt)*dang_rg
        write(6,*) 'droll=',droll,' dang_rg=',dang_rg

        else
        write(6,*) ' else part of pos.ne.9999,pos=',pos
        write(6,*)' n_ovlap=',n_ovlap
        nk = min(100,n_ovlap)/2*2
        write(6,*)'nk=',nk
        write(6,*)' k is from=',n_ovlap/2-nk/2+1, ' to',n_ovlap/2+nk/2
        sum = 0.0
        do k = n_ovlap/2-nk/2+1,n_ovlap/2+nk/2
        sum = sum+acu(k)
        end do
        ratio_db = sum/nk
        ratio =  2.718281829d0**ratio_db
        write(6,*)' sum=',sum,' ratio_db=',ratio_db,' ratio=',ratio
        call get_ratio_ang(kburst,beam_no,beam_id,
     *                     r_low,r_high,r_bip,coef_lk_ang,
     *                     ant_patn_r,ratio,droll)
        end if


c qdn 5/28/97
c       do i=kburst,kburst+3
c       call burst_pp_0(dfddth,pre_phase,thi,tp,coef_fd_0,
c    *                        i,v_sc_mag,v_x_sc,v_y_sc,v_z_sc,
c    *                        rng_bm_ang,beam_no,az_pk_coef,az_peak_coef,
c    *                        beam_id,x_sc,y_sc,z_sc,roll,yaw,pitch,
c    *                        look_ang,ire_sim,dem_ellip,dlon_topo,dlat_topo,
c    *                        topo,i_ang_bip,r_bip,fr_bip,fd_bip,coef_fd,
c    *                        coef_fr,coef_lk_ang,nbeams,np,prf,np_air,
c    *                        r_low,r_1st,fd_near,fr_near,r_high,ns,nr,dr,
c    *                        fd_far,fr_far,r_cntr,fd_cntr,fr_cntr,avg_terrain,coef_inc)
c       write(6,*) 'qdn_5thtime after calling burst_pp_0',i
c       write(6,*) 'qdn_5thtime dr(i)=',dr(i)
c       write(6,*) 'qdn_5thtime r_low(i)=',r_low(i)
c       write(6,*) 'qdn_5thtime r_high(i)=',r_high(i)
c       write(6,*) 'qdn_5thtime dfddth',dfddth,' pre_phase=',pre_phase
c       write(6,*) 'qdn_5thtime tp=',tp
c       end do

c qdn 5/23/97
        write(6,*)' QDN1 DROLL=',droll,'kburst=',kburst,
     *            ' roll(kburst)=',roll(kburst),
     *            ' newvalue=',roll(kburst)+droll
c       roll(kburst) = roll(kburst) + droll
c qdn 6/24/97
        roll(kburst) = roll(kburst) - droll

c qdn 5/28/97
c       do i=kburst,kburst+3
c       call burst_pp_0(dfddth,pre_phase,thi,tp,coef_fd_0,
c    *                        i,v_sc_mag,v_x_sc,v_y_sc,v_z_sc,
c    *                        rng_bm_ang,beam_no,az_pk_coef,az_peak_coef,
c    *                        beam_id,x_sc,y_sc,z_sc,roll,yaw,pitch,
c    *                        look_ang,ire_sim,dem_ellip,dlon_topo,dlat_topo,
c    *                        topo,i_ang_bip,r_bip,fr_bip,fd_bip,coef_fd,
c    *                        coef_fr,coef_lk_ang,nbeams,np,prf,np_air,
c    *                        r_low,r_1st,fd_near,fr_near,r_high,ns,nr,dr,
c    *                        fd_far,fr_far,r_cntr,fd_cntr,fr_cntr,avg_terrain,coef_inc)
c       write(6,*) 'qdn_6thtime after calling burst_pp_0',i
c       write(6,*) 'qdn_6thtime dr(i)=',dr(i)
c       write(6,*) 'qdn_6thtime r_low(i)=',r_low(i)
c       write(6,*) 'qdn_6thtime r_high(i)=',r_high(i)
c       write(6,*) 'qdn_6thtime dfddth',dfddth,' pre_phase=',pre_phase
c       write(6,*) 'qdn_6thtime tp=',tp
c       end do




	return	
	end






	subroutine zero_cross_rng(acu,n,pos)

	implicit real*8 (a-h,o-z)
	real acu(*),b(4096)

	nt = 50
	
	do i = 1, n
	b(i) = nt*acu(i)
	end do
	do i = nt/2+1,n-nt/2
	b(i) = 0.
	do k = -nt/2,nt/2
	b(i) = b(i)+acu(i+k)
	end do
	end do

	pos = 9999
	val_min = 1.*10**20.

	do j = nt/2+3,n-nt/2-2
	if(abs(b(j)).lt.val_min.and.b(j+2).lt.0.and.b(j+3).lt.0
     *		.and.b(j-2).gt.0.and.b(j-3).gt.0) then
	pos = j
	val_min = b(j)
	end if
	end do

	j = pos
        if(j.ne.9999) then
	  if(b(j)*b(j+1).lt.0.) 
     1			pos = pos+abs(b(j))/(abs(b(j))+abs(b(j+1)))
	  if(b(j)*b(j-1).lt.0.)
     1			pos = pos-abs(b(j))/(abs(b(j))+abs(b(j-1)))
        end if

	return
	end

c qdn 5/23/97
        subroutine ant_eq_bin(kcnt,kburst,beam_no,beam_id,
     *                        ant_patn_r,keq)
c       implicit real*8 (a-h,o-z)
        implicit none

        include 'ssp2_const.inc'
c       include 'key_const.inc'
c       include 'key_pp.inc'
        integer  kcnt,kburst,keq
        integer  beam_no(burst_max)
        integer  beam_id(burst_max)
        real     ant_patn_r(ns_ant_rg,7)

c Local variables
        integer beam1, beam2
        integer k, k_start,k_end, ipos
        real*8  vmin,ratio

        beam1 = beam_id(kburst)
        beam2 = beam_id(kburst+1)

        k_start = kcnt - 30
        k_end = kcnt + 30

        vmin = 10**30.

c       write(6,*)' Welcome to the ant_eq_bin'
c       write(6,*)' search from k_start=',k_start,' to k_end=',k_end
c       write(6,*)' beam1=',beam1,' beam2=',beam2
        do k = k_start, k_end
           ratio = (ant_patn_r(k,beam1)/ant_patn_r(k,beam2))**2
c          write(6,*)' k=',k,' beam1=',ant_patn_r(k,beam1),
c    *                 ' beam2=',ant_patn_r(k,beam2),ratio,vmin
           if(abs(ratio-1).lt.vmin) then
              ipos = k
              vmin = abs(ratio-1)
           end if
        end do

        keq = ipos
        write(6,*)' Left the ant_eq_bin with keq=',keq,ipos

        return
        end




      subroutine get_ratio_ang(kburst,beam_no,beam_id,
     *  r_low,r_high,r_bip,coef_lk_ang,
     *  ant_patn_r,ratio,d_roll)
c     implicit real*8 (a-h,o-z)
      implicit none
      include 'ssp2_const.inc'
c       include 'key_const.inc'
c       include 'key_pp.inc'
      integer kburst
      integer beam_no(burst_max)
      integer beam_id(burst_max)
      real*8  r_low(burst_max)
      real*8  r_high(burst_max)
      real*8  r_bip(burst_max)
      real*8  coef_lk_ang(4,burst_max)
      real    ant_patn_r(ns_ant_rg,7)
      real*8  ratio
      real*8  d_roll

c Local variables
      real*8   r_low_in, r_high_in, r_mean
      integer  beam1,beam2
      real*8   vlook_ang1,vlook_ang2
      real*8   vmin,ratio1
      integer  m, k, k_start, k_end, k1, k2, ipos

      m = 4

      beam1 = beam_id(kburst)
      beam2 = beam_id(kburst+1)

      r_low_in = max(r_low(kburst),r_low(kburst+1))
      r_high_in = min(r_high(kburst),r_high(kburst+1))
      r_mean = (r_low_in+r_high_in)/2.

      call v_poly(r_mean,r_bip(kburst),coef_lk_ang(1,kburst),m,vlook_ang1)
      k1 = nint((vlook_ang1-look_ang_cnt-ele_ang_ref) /dang_rg)+1
      call v_poly(r_mean,r_bip(kburst+1),coef_lk_ang(1,kburst+1),m,vlook_ang2)
      k2 = nint((vlook_ang2-look_ang_cnt-ele_ang_ref) /dang_rg)+1


      k_start = - 30
      k_end =  + 30

      vmin = 10**30.
c     write(6,*)' beam1=',beam1,' beam2=',beam2
c     write(6,*)' k1=',k1,' k2=',k2,' k_start=',k_start
c     write(6,*)' k_start+k1',k_start+k1,' k_start+k2=',k_start+k2
      do k = k_start, k_end
         ratio1 = (ant_patn_r(k1+k,beam1)/ant_patn_r(k2+k,beam2))**2
         if(abs(ratio-ratio1).lt.vmin) then
            ipos = k
            vmin = abs(ratio-ratio1)
         end if
      end do
      write(6,*)' ipos=',ipos,' k=',k

c qdn  6/2/97
c     d_roll = k * dang_rg
      d_roll = ipos * dang_rg
      write(6,*)' d_roll=',d_roll,' ipos*dang_rg=',ipos*dang_rg


      return
      end




        subroutine n_prf_rx(kburst,ire_sim,gain_rec,ns,
     *  np,dr,df,fs,dk_burst,file_spc_raw,data_cond,np_proc,
     *  tbl,blk_gain,snr,buff0,buff1,buff2,chirp_rate,fd_bip,
     *  nfft_az,r_low,r_1st,r_bip,coef_fd,coef_fr,prf,nbeams,
     *  burst1,burst2,w1,w2,w3,w4,n_prf,istatus,file_raw,nr, 
     *  sim_eam_nsp,np_v,dk_rep,gain_rep,file_rep,
     *  aux_wdp_chg,aux_wdp_np,aux_agc_np,aux_wdp_cnt,aux_agc_chg,aux_agc_cnt)

        implicit real*8 (a-h,o-z)
	include 'ssp2_const.inc'
	include 'ssp2_dim.inc'
c	include 'key_const.inc'
c	include 'key_pp.inc'
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
c************************************************
        integer ire_sim
        real*8 gain_rec(burst_max),dr(burst_max)
        real*8 df(burst_max)
        integer ns(burst_max),np(burst_max)
        integer dk_burst(burst_max)
        character*60 file_spc_raw
        integer data_cond,np_proc
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
        integer nfft_az,nbeams
        real*8 prf(burst_max)
        character*60 file_raw
        integer nr
        real w1(64),w2(64),w3(64),w4(64)
	complex burst1(line_raw,sam_raw),burst2(line_raw,sam_raw)

        complex temp1,temp2,intp
	real*8 xprod(11)
        real ai1,ai2,aj1,aj2
        real arra1(128,10240),arra2(128,10240)
        integer pmod_i

        call weight(w1,w2,w3,w4,64)
        call copyfdfr_coef(kburst, kburst+nbeams, coef_fd, coef_fr)

c       if (ire_sim.eq.0) then
c       np_proc = np(kburst) -9
c       call get_burst_sim(
c    *  kburst,plen,np_proc,fs,file_spc_raw,
c    *  ns,buff0,istatus,buff1,dk_burst,file_raw,
c    *  sim_eam_nsp)
c       else
        np_proc = np_v(kburst)
        call get_burst(
     *  kburst,plen,ire_sim,gain_rec,ns,
     *  np,dk_burst,file_spc_raw,data_cond,
     *  np_proc,fs,tbl,blk_gain,snr,
     *  buff0,buff1,istatus,file_raw,nr,np_v,
     *  aux_wdp_chg,aux_wdp_np,aux_agc_np,aux_wdp_cnt,aux_agc_chg,aux_agc_cnt)
        if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
c       endif
        call range_comp(kburst,nr,fs,chirp_rate,fd_bip,
     *  nfft_az,np_proc,ns,r_1st,dr,r_bip,coef_fd,
     *  prf,buff1,np_v,buff_rep,ire_sim,
     *  dk_rep,gain_rep,file_rep,istatus,tbl)
        if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
        call corner_turn(kburst,nr,ns,np_proc,nfft_az,
     *  buff1,buff2)                                           !data corner turn
        call az_comp(kburst,nr,np_proc,nfft_az,ns,
     *  r_1st,dr,r_bip,coef_fd,coef_fr,prf,buff2)                    !azimuth comopression

        do i = 1, ns(kburst)
        do j = 1, nfft_az
        burst1(j,i) = buff2(j,i)
        end do
        end do

c       if (ire_sim.eq.0) then
c       np_proc = np(kburst+nbeams) -9
c       call get_burst_sim(
c    *  (kburst+nbeams),plen,np_proc,fs,file_spc_raw,
c    *  ns,buff0,istatus,buff1,dk_burst,file_raw,
c    *  sim_eam_nsp)
c       else
        np_proc = np_v(kburst+nbeams)
        call get_burst(
     *  (kburst+nbeams),plen,ire_sim,gain_rec,ns,
     *  np,dk_burst,file_spc_raw,data_cond,
     *  np_proc,fs,tbl,blk_gain,snr,
     *  buff0,buff1,istatus,file_raw,nr,np_v,
     *  aux_wdp_chg,aux_wdp_np,aux_agc_np,aux_wdp_cnt,aux_agc_chg,aux_agc_cnt)
        if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
c       endif
        call range_comp((kburst+nbeams),nr,fs,chirp_rate,fd_bip,
     *  nfft_az,np_proc,ns,r_1st,dr,r_bip,coef_fd,
     *  prf,buff1,np_v,buff_rep,ire_sim,
     *  dk_rep,gain_rep,file_rep,istatus,tbl)
        if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
        call corner_turn((kburst+nbeams),nr,ns,np_proc,nfft_az,
     *  buff1,buff2)                !data corner turn
        call az_comp((kburst+nbeams),nr,np_proc,nfft_az,ns,
     *  r_1st,dr,r_bip,coef_fd,coef_fr,prf,buff2)

        do i = 1, ns(kburst+nbeams)
        do j = 1, nfft_az
        burst2(j,i) = buff2(j,i)
        end do
        end do

	tp = 0.
	do j = 1, nbeams
	tp = np(j)/prf(j)+tp
	end do
	m = 4
c******** initialize buffer ******************

        margin = 20
        r_low_in =  max(r_low(kburst),r_low(kburst+nbeams))
        write(6,*)'r_low12', r_low(kburst), r_low(kburst+nbeams)


        nset = 11    !must be an odd number
	do ijk = 1, nset
        xprod(ijk) = 0.

        do i1 = margin/2, ns(kburst)-nr
        r = r_low_in + (i1-1)*dr(kburst)
	call v_poly(r,r_bip(kburst),coef_fd(1,kburst),m,fd_cnt0)
	call v_poly(r,r_bip(kburst),coef_fr(1,kburst),m,fr)
        fd_cnt = fd_cnt0 +(ijk-nset/2-1)*prf(kburst)
        ai1 = ((r-r_low(kburst))/dr(kburst))+1
        ai20 = ((r-r_low(kburst+nbeams))/dr(kburst+nbeams))+1
        f_off = abs(fr)*tp

        do j = 1, nfft_az*(prf(kburst+nbeams)-f_off)/prf(kburst+nbeams)
        fd = fd_cnt+(j-nfft_az/2-1)*df(kburst+nbeams)
        dr_migra = lambda/2*(fd+fr/2*tp)*tp/dr(kburst)
        ai2 = ai20-dr_migra
        aj2 = j
        temp2 = intp(burst2,nfft_az,line_raw,sam_raw,aj2,ai2,2,w1,w2,w3,w4)
        aj1 = j+(f_off+prf(kburst+nbeams)/2.-prf(kburst)/2.)/df(kburst)
        temp1 = intp(burst1,nfft_az,line_raw,sam_raw,aj1,ai1,1,w1,w2,w3,w4)
        xprod(ijk) = xprod(ijk) + abs(temp1)*abs(temp2)
        if(ijk.eq.nset/2+1) then
        arra1(j,i1) = abs(temp1)
        arra2(j,i1) = abs(temp2)
        end if
        end do
        end do

        end do

        amax = 0.
        do ijk = 1, nset
D       write (*,*) ijk, xprod(ijk)
	if(xprod(ijk).gt.amax) then
        amax = real(xprod(ijk))
        n_prf = ijk - nset/2-1
        end if
        end do

        write(6,*) 'n_prf = ', n_prf
c       call write_disk('filea',arra1,4*128*10240,0,istatus)  !for debug use
c       call write_disk('fileb',arra2,4*128*10240,0,istatus)  !for debug use


        return
        end
        subroutine copyfdfr_coef(k1, k2, coef_fd, coef_fr)
        implicit real*8 (a-h,o-z)
        include 'ssp2_const.inc'
c       include 'key_const.inc'
c       include 'key_pp.inc'
        real*8 coef_fd(4,burst_max)
        real*8 coef_fr(4,burst_max)


        do k = 1, 4
        coef_fd(k,k2) = coef_fd(k,k1)
        coef_fr(k,k2) = coef_fr(k,k1)
        end do

        return
        end




        subroutine clutter(kburst,ire_sim,gain_rec,ns,
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
	real acu(line_raw,60),ai1,ai2,aj1,aj2
	real*8 rr(60),dfdc(60,4),fdc(60),angloook(60,4),cc(4,5)
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
        call az_comp(kburst,nr,np_proc,nfft_az,ns,
     *  r_1st,dr,r_bip,coef_fd,coef_fr,prf,buff2)  
c       write(6,*)'WHY',buff2(1,1)
        write(6,*)'WHY kburst,nr',kburst,nr,np_proc,nfft_az
        write(6,*)'WHY nr,r_1st,dr',ns(kburst),r_1st(kburst),dr(kburst)
        write(6,*)'WHY r_bip,coef_fd',r_bip(kburst),
     *                 coef_fd(1,kburst),coef_fd(2,kburst)
        write(6,*)'WHY coef_fr,prf',coef_fr(1,kburst),prf(kburst)

	do i = 1, ns(kburst)
	do j = 1, nfft_az
	burst1(j,i) = buff2(j,i)
	end do
	end do


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
         call az_comp((kburst+nbeams),nr,np_proc,nfft_az,ns,
     *   r_1st,dr,r_bip,coef_fd,coef_fr,prf,buff2)                    !azimuth comopression

D	write (*,*) 'two burst done'
        write(6,*)'WHY kburst,nr,np_proc',kburst+nbeams,nr,np_proc,nfft_az
        write(6,*)'WHY ns,r_1st,dr',ns(kburst+nbeams),r_1st(kburst+nbeams),
     *                  dr(kburst+nbeams)
        write(6,*)'WHY r_bip,coef_fd',r_bip(kburst+nbeams),
     *                  coef_fd(1,kburst+nbeams),coef_fd(2,kburst+nbeams)
        write(6,*)'WHY coef_fr,prf',coef_fr(1,kburst+nbeams),prf(kburst+nbeams)

	do i = 1, ns(kburst+nbeams)
	do j = 1, nfft_az
	burst2(j,i) = buff2(j,i)
	end do
	end do



c******** input parameters *******************
	m = 4
	nset = 60
	margin = 20

	tp = 0.
	do j = kburst,kburst+nbeams-1
	 tp = np(j)/prf(j)+tp
	end do

c******** initialize buffer ******************	

	r_low_in =  max(r_low(kburst),r_low(kburst+nbeams)) 
	r_high_in =  min(r_high(kburst),r_high(kburst+nbeams))
	lines_b = int(((r_high_in-r_low_in)/dr(kburst)-margin)/nset) 

c       write(6,*)'WHAT',r_low_in,r_low(kburst),r_low(kburst+nbeams)

	do i = 1, nset
	do j = 1, nfft_az
	acu(j,i) = 0.
	end do
	end do


	do i = 1, nset
	i_st = margin/2+(i-1)*lines_b
	i_end = margin/2+i*lines_b
D	write (*,*) 'set= ', i,i_st,i_end

	do i1 = i_st, i_end
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
	acu(j,i) = acu(j,i)+ log(abs(temp1/temp2))
	end do
	end do




c************* zero cross detection *****************
	rr(i) = ((i-1)*lines_b+(margin+lines_b)/2)*dr(kburst)+r_low_in
	call v_poly(rr(i),r_bip(kburst),coef_fd(1,kburst),m,fd)
	call v_poly(rr(i),r_bip(kburst),coef_fr(1,kburst),m,fr)
	f_off = abs(fr) * tp
	ratio = prf(kburst)/(2*7500.d0/az_ant_len)
	call zero_cross_fd(acu(1,i),nfft_az,f_off,df(kburst),pos,ratio)
c       write(6,*)'ZZZ',f_off,pos,acu(1,i),nfft_az,df(kburst),ratio
        dfd = pos*df(kburst)-(prf(kburst)-f_off)/2.
	if(dfd.gt.prf(kburst)/2.) dfd = dfd - prf(kburst)
	if(dfd.lt.-prf(kburst)/2.) dfd = dfd + prf(kburst)
        fdc(i) = fd + dfd  			!update Doppler centroid
 	write (*,*) 'fd_pred,pos,dfd= ', fd,pos,dfd,fdc(i)

	end do

       
c*********** get a contiguous profile of the Doppler centroid *********
	do i = 1,nset
        call v_poly(rr(i),r_bip(kburst),coef_lk_ang(1,kburst),m,angloook(i,kbeam))
c       write(6,*) 'lookang= ', angloook(i,kbeam)
	end do

c qdn 6/7/97 get a contiguous profile 
c       run_avg = fdc(1)
c       inum_avg = 1
c       do i=1,nset
c          if(i.ne.1) fdc(i) =
c    *         fdc(i) - prf(kburst)*nint((fdc(i)-run_avg)/prf(kburst))
c          run_avg = (run_avg*inum_avg+fdc(i))/(inum_avg+1)
c          inum_avg = inum_avg + 1
c          x_fdc(i) = i
c       end do

c qdn 8/19/97
        x_fdc(1) = 1
        do i=2,nset
           run_avg = 0.0
           inum_avg = 0
           do  j=i-1,max(1,i-10),-1
               run_avg = run_avg + fdc(j)
               inum_avg = inum_avg + 1
           end do
           run_avg = run_avg / inum_avg
           if(nint((fdc(i)-run_avg)/prf(kburst)).ne.0) then
               write(6,*)'BEWARE at i=',i,'fdc(i)=',fdc(i),'run_avg=',run_avg
           end if
           fdc(i) =
     *         fdc(i) - prf(kburst)*nint((fdc(i)-run_avg)/prf(kburst))
           x_fdc(i) = i
        end do
c qdn 6/7/97 smooth out the fdc vector
        iorder_smooth = 3
        call smooth_out(x_fdc,fdc,nset,iorder_smooth)

        iorder_smooth = 3
        call smooth_out(x_fdc,fdc,nset,iorder_smooth)


        do i = 1,nset
c qdn 6/7/97 modified to compare with running sum
c        if(i.ne.1) fdc(i) =
c    1    fdc(i)-prf(kburst)*nint((fdc(i)-fdc(i-1))/prf(kburst))
         write(6,*) ' new fd= ', fdc(i)
         call v_poly(rr(i),r_bip(kburst),coef_fd(1,kburst),m,fd)
         if(kburst.le.nbeams*200) hufd(i,(kburst-1)/nbeams+1,kbeam) = fdc(i) - fd
         call v_poly(rr(i),r_bip(kburst),coef_fd_0(1,kburst),m,fd)
         az_pk_coef(1) = az_peak_coef(1,beam_id(kburst))
         az_pk_coef(2) = az_peak_coef(2,beam_id(kburst))
         az_pk_coef(3) = az_peak_coef(3,beam_id(kburst))
         az_pk_ang = az_pk_coef(1)+az_pk_coef(2)*angloook(i,kbeam)+
     1              az_pk_coef(3)*angloook(i,kbeam)**2
         dfdc(i,kbeam) = fdc(i) - fd - dfddth*az_pk_ang
c        write(6,*)'i=',i,angloook(i,kbeam),' az_ang=',
c    *            (fdc(i) -fd)/dfddth, az_pk_ang
        end do

        if(pre_phase.eq.1.or.pre_phase.eq.2) then
           do i = 1, nset
           rr(i) = rr(i) - r_bip(kburst)
           end do

           call lsf(rr(1),fdc(1),nset,coef_fd(1,kburst),m,cc)
 
           do i = 1, nset
           rr(i) = rr(i) + r_bip(kburst)
           call v_poly(rr(i),r_bip(kburst),coef_fd(1,kburst),m,fd)
           write(6,*) 'fd from new coef= ', fd,' rr=',rr(i)
           end do

        else
        if(pre_phase.eq.3.and.kbeam.eq.nbeams) then
          call yaw_pitch(kburst,dfdc,angloook,
     *                       beam_id,nbeams,dfddth,yaw,pitch)
        end if
        end if



	
	return	
	end

c QDN 7/24/97

        subroutine yaw_pitch(kburst,dfdc,angloook,
     *                       beam_id,nbeams,dfddth,yaw,pitch)

        implicit real*8 (a-h,o-z)
c****************************************
        include 'ssp2_const.inc'
c       include 'key_const.inc'
c       include 'key_pp.inc'
        real*8  dfddth
        integer nbeams
        integer beam_id(burst_max)
        real*8 yaw(burst_max)
        real*8 pitch(burst_max)
        real*8 dfdc(60,4),angloook(60,4),cc(2,3)
        real*8 dx(240),ct(240),coef(2)
c****************************************
        real*8 dthita


        m = 4
        nset = 60

c       do kbeam = 1, nbeams
c       do i = 1, nset
c       dthita = dfdc(i,kbeam) / dfddth
c       r = 1.d0 / cosd(angloook(i,kbeam))
c       ii = (kbeam-1)*nset+i
c       dx(ii) = r * dthita * pi/180.d0
c       ct(ii) = tand(angloook(i,kbeam))
c       end do
c       end do

        icount  = 0
        do kbeam=1,nbeams
           nset = 60
           if(beam_id(kbeam).eq.5) nset=30
           do i=1,nset
              icount = icount+1
              i1 = i
              if(beam_id(kbeam).eq.5) i1=i+30
              dthita = dfdc(i1,kbeam)/dfddth
              r = 1.0d0/cosd(angloook(i1,kbeam))
              dx(icount) = r * dthita * pi/180.d0
              ct(icount) = tand(angloook(i1,kbeam))
           end do
        end do

        call lsf(ct,dx,icount,coef,2,cc)

c       call lsf(ct,dx,nbeams*nset,coef,2,cc)

        do kbeam = 1, nbeams
        kb = kburst - nbeams + kbeam
        yaw(kb) = atand(coef(2))
        pitch(kb) = -asind(coef(1))
        write(6,*) 'yaw,pitch refinement =', yaw(kb),pitch(kb)
        end do

        return
        end



        function intp(burst,n1,nx,n2,a1,a2,id,
     *                w1,w2,w3,w4)
        complex burst(nx,n2),intp
        integer pmod_i,id,n1,n2
        real*8 pi
        real sinc,a1,a2,w1(64),w2(64),w3(64),w4(64)
        

        pi = 4.*atan(1.)
        intp = cmplx(0.,0.)
        if(id.eq.1) then        !interp in the 1st dimension

        j = a2
c       j = nint(a2)
        j = pmod_i(j,n2)
        ii = a1
c       ii = nint(a1)
        di = a1 - ii
        kk = di*64+1
        i0 = pmod_i(ii,n1)
        im1 = pmod_i(ii-1,n1)
        i1 = pmod_i(ii+1,n1)
        i2 = pmod_i(ii+2,n1)
        intp = burst(im1,j)*w1(kk)+burst(i0,j)*w2(kk)+
     1          burst(i1,j)*w3(kk)+burst(i2,j)*w4(kk)
        else                    !interp in the 2nd dimension


        i = a1
c       i = nint(a1)
        i = pmod_i(i,n1)
        jj = a2
c       jj = nint(a2)
        dj = a2 - jj
        kk = dj*64+1
        j0 = pmod_i(jj,n2)
        jm1 = pmod_i(jj-1,n2)
        j1 = pmod_i(jj+1,n2)
        j2 = pmod_i(jj+2,n2)
        intp = burst(i,jm1)*w1(kk)+burst(i,j0)*w2(kk)+
     1          burst(i,j1)*w3(kk)+burst(i,j2)*w4(kk)

        end if

        return
        end






	subroutine zero_cross_fd(acu,nfft_az,f_off,df,pos,ratio)

	implicit real*8 (a-h,o-z)
	real acu(128),ant_w(128),val_min
	complex buffer1(128),buffer2(128)

	call ant_weight(nfft_az,f_off,df,ant_w,ratio)
	do k = 1, nfft_az
	buffer1(k) = cmplx(acu(k),0.)
	buffer2(k) = cmplx(ant_w(k),0.)
	end do
	call cfft(buffer1,1,nfft_az,1)
	call cfft(buffer2,1,nfft_az,1)
	do k = 1, nfft_az
	buffer1(k) = buffer1(k)/nfft_az
	buffer2(k) = buffer2(k)/nfft_az
	end do
	do k = 1, nfft_az
	buffer1(k) = buffer1(k)*conjg(buffer2(k))
	end do
	call cfft(buffer1,1,nfft_az,-1)
	do k = 1, nfft_az
	acu(k) = real(buffer1(k))
	end do

	call z_cross(acu,nfft_az,pos)
	
	return
	end


	subroutine z_cross(acu,n,pos)
	implicit real*8 (a-h,o-z)
	real acu(n)
	integer pmod_i

c       write(6,*)'Welcome to z_cross n=',n
	pos = 1
	val_min = 1.*10**10.
c qdn 6/3/97
c       val_max = 0.
        val_max = -1.0
c       write(6,*)' before the loop max=',val_max,val_min
	do j = 1, n
	if(acu(j).gt.val_max) then
	val_max = acu(j)
	jmax = j
	end if 
	if(acu(j).lt.val_min) then
	val_min = acu(j)
	jmin = j
	end if
	end do
c       write(6,*)' in z_cross jmax=',jmax,val_max,' jmin=',jmin,val_min

	val_min = 1.*10**10.
	if(jmin.gt.jmax) then
	   do j = jmax, jmin
	   if(abs(acu(j)).lt.val_min) then
	   pos = j
	   val_min = abs(acu(j))
	   end if
	   end do
	else
	   do j = jmax, n
	   if(abs(acu(j)).lt.val_min) then
	   pos = j
	   val_min = abs(acu(j))
	   end if
	   end do
	   do j = 1, jmin
	   if(abs(acu(j)).lt.val_min) then
	   pos = j
	   val_min = abs(acu(j))
	   end if
	   end do
	end if

	j = pos
	jp1 = j+1
	jm1 = j-1
	jp1 = pmod_i(jp1,n)
	jm1 = pmod_i(jm1,n)
	if(acu(j)*acu(jp1).lt.0.) 
     1	pos = pos+abs(acu(j))/(abs(acu(j))+abs(acu(jp1)))
	if(acu(j)*acu(jm1).lt.0.)
     1	pos = pos-abs(acu(j))/(abs(acu(j))+abs(acu(jm1)))

	return
	end


	subroutine ant_weight(nfft_az,f_off,df,ant_r_dot,ratio)
	implicit real*8 (a-h,o-z)
	real ant(128),ant_r(128),ant_r_dot(128)
	real*8 ratio

	n = nfft_az
	call ant_fun(ant,n,ratio)
	call ant_rt_dot(ant,ant_r,ant_r_dot,n,f_off,df)

c	do i=1, n
c	type *, i, ant(i)
c	end do

c	do i=1, n
c	type *, i, ant_r(i)
c	end do

c	do i=1, n
c	type *, i, ant_r_dot(i)
c	end do
	return
	end

	subroutine ant_fun(ant,n,ratio)
	implicit real*8 (a-h,o-z)
	real ant(n)
	
	an = ratio*n
	twopi = 8. * atan(1.)
	do i = 1, n
	ant(i) = 0.
	do k = -3,3
	x = 1.e-5+(k*n+i-n/2)/an * twopi
	ant(i) = ant(i)+(sin(x)/x)**4	
	end do
	end do

	return
	end


	subroutine ant_rt_dot(ant,ant_r,ant_r_dot,n,f_off,df)
	implicit real*8 (a-h,o-z)
	real ant(n),ant_r(n),ant_r_dot(n)
	
	sam_off = f_off/df
	sam_eq = (n-sam_off)/2.
	n2 = sam_eq
	ds2 = sam_eq - n2
	n1 = sam_eq + sam_off
	ds1 = sam_eq + sam_off - n1

	do i = 1, n
	ant_r(i) = 0.
	ant_r_dot(i) = 0.
	end do

	do i = 1, n2
	v2 = ant(n2+i-1)*(1-ds2)+ant(n2+i)*ds2
	v1 = ant(n1+i-1)*(1-ds1)+ant(n1+i)*ds1
	ant_r(n/2+i) = log(v1/v2) 
	end do
	do i = 1, n2
	ant_r(n/2-i+1) = -1*ant_r(n/2+i+1)
	end do

	do i = 1, n2*.75
	ant_r_dot(i) = -1*(ant_r(n/2+i+1)-ant_r(n/2+i-1))
	end do
	do i = 1, n2*.75-1
	ant_r_dot(n-i+1) =  ant_r_dot(i)
	end do

	return
	end

	function pmod_i(k,m)
	integer k,m,pmod_i

	k1 = mod(100*m+k-1,m)+1
	pmod_i = k1

	return
	end



c	subroutine fdr_reg(r_2_r1st,fd,n,fdmean,fdd)	
c
c	implicit none
c	real*8 r_2_r1st(512), fd(512)
c	real*8 fdmean,rmean,accu,fdd,sum,sigma,dfd
c	integer i,n
c	
c	fdmean = 0
c	rmean = 0
c	do i = 1, n
c	rmean = rmean + r_2_r1st(i)
c	fdmean = fdmean + fd(i)
c	end do
c	rmean = rmean/n
c	fdmean = fdmean/n
c
c	fdd = 0
c	accu = 0
c	do i = 1, n
c	fdd = fdd + fd(i)*(r_2_r1st(i)-rmean)
c	accu = accu + (r_2_r1st(i)-rmean)**2
c	end do
c	fdd = fdd/accu
c
c	sum = 0
c	do i = 1, n
c	dfd = fd(i)-(fdmean+fdd*(r_2_r1st(i)-rmean))
c	write (*,*) 'dfd = ', dfd
c	sum = sum+dfd**2
c	end do
c	sigma = sqrt(sum/n)
c	write (*,*) 'sigma fd = ', sigma
c
c	return
c	end


 
	subroutine weight(w1,w2,w3,w4,n)
	real*8 pi
	real w1(n),w2(n),w3(n),w4(n)
	real sinc

        pi = 4.*atan(1.)
        do k = 1, n
        dp = (k-1.)/n
           W1(k)=-(1.+dp)**3 + 5.*(1.+dp)**2 - 8.*(1.+dp) +4.
           W2(k)=      dp**3 -      2.*dp**2               +1.
           W3(k)= (1.-dp)**3 - 2.*(1.-dp)**2               +1.
           W4(k)=-(2.-dp)**3 + 5.*(2.-dp)**2 - 8.*(2.-dp) +4.
        end do



	return
	end



	subroutine best_pair(burst_k,n_times,nbeams,beam_no,
     *  s_power,bestp)
	implicit real*8 (a-h,o-z)
	include 'ssp2_const.inc'
c	include 'key_const.inc'
c	include 'key_pp.inc'
        integer beam_no(burst_max)
	integer burst_k(800),bestp,bkk
	real s_power(800),s_power2(800)

	do k = 1, nbeams*n_times
        bkk=burst_k(k)
	if(beam_no(burst_k(k)).ne.nbeams) then
        s_power2(bkk) = (s_power(bkk)*s_power(bkk+1))/
     *				(s_power(bkk)+s_power(bkk+1))
	else
	s_power2(burst_k(k)) = s_power(burst_k(k))
	end if
	end do

	v_max = 0.
	do k = 1, nbeams*n_times
	if(s_power2(burst_k(k)).gt.v_max) then
	v_max = s_power2(burst_k(k))
	bestp = burst_k(k)
	end if
	end do
	
	return
	end


        subroutine best_burst(burst_k,nbeams,n_times,
     *  contrast,s_power,bestk)
	implicit real*8 (a-h,o-z)
	integer burst_k(800),bestk,big5(5)
	real contrast(800),s_power(800),s_power2(800)

c***************************************************************
c	Select 5 sets with the highest s_power, then
c	select one (among the 5 sets) with the best contrast
c***************************************************************

	do k = 1, n_times*nbeams
	s_power2(k) = s_power(k)
	end do

	do i = 1, 5
	 v_max = 0.
	  do k = 1, n_times*nbeams
	    if(s_power2(k).gt.v_max) then
	     v_max = s_power2(k)
	     big5(i) = k
            end if
	  end do
	 s_power2(big5(i)) = 0
D	write (*,*) 'val = ', i,big5(i),v_max
	end do
	

	v_max = 0.
	do k = 1, 5
D	write (*,*) big5(k)
	if(contrast(big5(k)).gt.v_max) then
	v_max = contrast(big5(k))
	bestk = burst_k(big5(k))
	end if
	end do
D	write (*,*) 'val = ', bestk,v_max

	return
	end

c qdn 7/2/97: modified to select for SWB select st5 and st6
c               
        subroutine best_burst2(burst_k,nbeams,n_times,iselect_beam,
     *  contrast,s_power,bestk)
	implicit real*8 (a-h,o-z)
        integer iselect_beam
	integer burst_k(800),bestk,big5(5)
	real contrast(800),s_power(800),s_power2(800)

c***************************************************************
c	Select 5 sets with the highest s_power, then
c	select one (among the 5 sets) with the best contrast
c***************************************************************

        write(6,*)' Inside best_burst2, iselect_beam=',iselect_beam
        write(6,*)' Inside best_burst2, n_times=',n_times,' nbeams=',nbeams
        do k = 1, n_times
	s_power2(k) = s_power((k-1)*nbeams+iselect_beam+1)
	end do
        write(6,*)' ntimes=',n_times
        write(6,*)' print out s_power2=',(s_power2(k),k=1,n_times)

c qdn 7/9/97 for fix_fd_B, it requires to find only maximum power
        v_max = s_power2(1) 
        bestk = burst_k(iselect_beam+1)
c       do k=1,n_times
        do k=1,1
          if(s_power2(k).gt.v_max) then
             v_max = s_power2(k)
             bestk = burst_k((k-1)*nbeams+iselect_beam+1)
          end if
        end do

 	write (6,*) 'best_k = ', bestk, v_max

	return
	end

c qdn 7/2/97: select the beam offset for maximinize the prf difference
c iselect_beam will have the value between 0 and nbeams - 1
      subroutine select_beam(nbeams,prf,iselect_beam)
      implicit   none
      integer    nbeams,iselect_beam
      real*8     prf(nbeams)

c Local variables
      real*8     diff_prf(3)
      real*8     diff_max
      integer    i

      do i=1,nbeams-1
         diff_prf(i) = abs(prf(i+1) - prf(i))
      end do

      diff_max = diff_prf(1)
      iselect_beam = 1
      do i=2,nbeams-1
         if(diff_max.lt.diff_prf(i))then
            diff_max = diff_prf(i)
            iselect_beam = i
         end if
      end do
      iselect_beam = iselect_beam - 1

c QDN hardcode for testing 8/5/97
      iselect_beam = 1

      write(6,*)'For  PRF amb., iselect_beam=',iselect_beam,' for',nbeams
      write(6,*)'For  PRF amb., prf=',(prf(i),i=1,nbeams)

      return
      end


        subroutine fix_fd_A(bestk,n_times,btotal,nprf,r_low,r_bip,
     *  r_high,prf,beam_no,coef_fd,nbeams,burst_k_count,burst_k)
c**********************************************
        implicit real*8 (a-h,o-z)
        integer burst_k(800),burst_k_count
	integer bestk,c_prf,btotal
	include 'ssp2_const.inc'
c	include 'key_const.inc'
c	include 'key_pp.inc'
        real*8 coef_fd(4,burst_max)
        real*8 prf(burst_max)
        real*8 r_low(burst_max)
        real*8 r_bip(burst_max)
        real*8 r_high(burst_max)
        integer beam_no(burst_max)
        integer k1,k2,m
        integer interval

        write(6,*)' Inside fix_fd_A routine, bestk=',bestk
        write(6,*)' Inside fix_fd_A routine, PRF amb num=',nprf

        interval=(btotal/n_times)/(nbeams*2)*(nbeams*2)

	m = 4
	coef_fd(1,bestk) = coef_fd(1,bestk)+nprf*prf(bestk)

c qdn 8/15 adding for debug
c       write(6,*)' QDN_CHECK, burst_k_count=',burst_k_count
c       write(6,*)' QDN_CHECK, nbeams=',nbeams,' bestk=',bestk
c       do i=1,burst_k_count
c          k1_qdn = burst_k(i)
c          if(mod(k1_qdn,nbeams).lt.nbeams-1) then
cc            call qdn_check_df(k1_qdn,k1_qdn+1,coef_fd,r_low,r_bip,
cc   *                       r_high,prf)
c             call qdn_check_df(k1_qdn+1,k1_qdn,coef_fd,r_low,r_bip,
c    *                       r_high,prf)
c          end if
c       end do


	if(beam_no(bestk).gt.1) then
           do k = bestk, bestk-beam_no(bestk)+2, -1
           call fix_2nd_burst(k,k-1,coef_fd,r_low,r_bip,r_high,prf)   
	   end do
	end if

        do i=1,burst_k_count
          if (burst_k(i).eq.bestk) then
            icount_bestk= i
            icount_bestk= icount_bestk-beam_no(bestk)+1
          endif
        enddo

        do k=icount_bestk,nbeams+1,-1
          if (beam_no(burst_k(k)).eq.1) then
   	   call fix_2nd_burst(burst_k(k),burst_k(k-nbeams),coef_fd,r_low,r_bip,r_high,prf)   
          endif
        enddo

c       do k=bestk-beam_no(bestk)+1,interval+1,-interval
c  	 call fix_2nd_burst(k,k-interval,coef_fd,r_low,r_bip,r_high,prf)   
c	end do

        do k=1,burst_k_count-beam_no(burst_k_count)+1-nbeams
          if (beam_no(burst_k(k)).eq.1) then
   	   call fix_2nd_burst(burst_k(k),burst_k(k+nbeams),coef_fd,r_low,r_bip,r_high,prf)   
          endif
        enddo

c	do k=1,btotal-interval,interval
c        call fix_2nd_burst(k,k+interval,coef_fd,r_low,r_bip,r_high,prf)   
c       end do


        do k=1,burst_k_count
          if (beam_no(burst_k(k)).eq.1) then
            do j=1,nbeams-1
              call fix_2nd_burst(burst_k(k+j-1),burst_k(k+j),coef_fd,r_low,r_bip,r_high,prf)   
            enddo
          endif
        enddo
c       do k=1,btotal,interval
c       do j=1,nbeams-1
c         call fix_2nd_burst(k+j-1,k+j,coef_fd,r_low,r_bip,r_high,prf)   
c	end do
c	end do

	return
	end


	subroutine fix_2nd_burst(k1,k2,coef_fd,r_low,r_bip,
     *  r_high,prf)   
                                                       	!make fd contiguous in 
							!two adjacent bursts
	implicit real*8 (a-h,o-z)
	include 'ssp2_const.inc'
c	include 'key_const.inc'
c	include 'key_pp.inc'
        real*8 coef_fd(4,burst_max)
        real*8 prf(burst_max)
        real*8 r_low(burst_max)
        real*8 r_bip(burst_max)
        real*8 r_high(burst_max)

        real*8 sum_change

        write(6,*)'FIXING bursts ',k1,k2
	m = 4
	r_low_in = max(r_low(k1),r_low(k2))
	r_high_in = min(r_high(k1),r_high(k2))
	r_cnt = (r_low_in+r_high_in)/2.0d0
        call v_poly(r_cnt,r_bip(k1),coef_fd(1,k1),m,fd1)
        call v_poly(r_cnt,r_bip(k2),coef_fd(1,k2),m,fd2)
        write (*,*) 'fd1,fd2,r_cnt=',fd1,fd2,r_cnt
        write (*,*) 'fd1-fd2=',fd1-fd2,' prf=',prf(k2),
     *              ' c_prf=',(fd1-fd2)/prf(k2)
        write (*,*) '  nint(c_prf)=',nint((fd1-fd2)/prf(k2))
        write (*,*) 'coef_fd(1,k1)=',(coef_fd(i,k1),i=1,4)
        write (*,*) 'coef_fd(1,k2)=',(coef_fd(i,k2),i=1,4)
        c_prf = nint((fd1-fd2)/prf(k2))
      
	coef_fd(1,k2) = coef_fd(1,k2) + c_prf*prf(k2)
        write (*,*) 'kburst = ',k1,k2,c_prf

	return
	end


        subroutine qdn_check_df(k1,k2,coef_fd,r_low,r_bip,r_high,prf)
        implicit real*8 (a-h,o-z)
        include 'ssp2_const.inc'
        real*8 coef_fd(4,burst_max)
        real*8 prf(burst_max)
        real*8 r_low(burst_max)
        real*8 r_bip(burst_max)
        real*8 r_high(burst_max)

        write(6,*)' qdn_check Comparing the burst k1=',k1,'k2=',k2
        m = 4
        r_low_in = max(r_low(k1),r_low(k2))
        r_high_in = min(r_high(k1),r_high(k2))
        r_cnt = (r_low_in+r_high_in)/2.0d0
        call v_poly(r_cnt,r_bip(k1),coef_fd(1,k1),m,fd1)
        call v_poly(r_cnt,r_bip(k2),coef_fd(1,k2),m,fd2)
        write (*,*) 'qdn_check coef_fd of k1=',(coef_fd(i,k1),i=1,4)
        write (6,*) 'qdn_check r_low   of k1=',r_low(k1)
        write (6,*) 'qdn_check r_high  of k1=',r_high(k1)
        write (6,*) 'qdn_check r_bip   of k1=',r_bip(k1)
        write (*,*) 'qdn_check coef_fd of k2=',(coef_fd(i,k2),i=1,4)
        write (6,*) 'qdn_check r_low   of k2=',r_low(k2)
        write (6,*) 'qdn_check r_high  of k2=',r_high(k2)
        write (6,*) 'qdn_check r_bip   of k2=',r_bip(k2)
        write (6,*) 'qdn_check r_low_in =',r_low_in,' r_high_in=',r_high_in
        write (6,*) 'qdn_check r_cnt=',r_cnt
        write (*,*) 'qdn_check fd1,fd2',fd1,fd2
        write (*,*) 'qdn_check fd1-fd2=',fd1-fd2,' prf(k2)=',prf(k2)
        write (*,*) 'qdn_check c_prf=',(fd1-fd2)/prf(k2)
        write (*,*) 'qdn_check nint(c_prf)=',nint((fd1-fd2)/prf(k2))

        return
        end


	subroutine fix_fd_B(bestp,prf,r_low,
     *  r_bip,r_high,coef_fd,nprf1,coef_lk_ang,az_peak_coef,beam_id,dfddth)

	implicit real*8 (a-h,o-z)
	include 'ssp2_const.inc'
c	include 'key_const.inc'
c	include 'key_pp.inc'
        real*8 coef_fd(4,burst_max)
        real*8 prf(burst_max)
        real*8 r_low(burst_max)
        real*8 r_bip(burst_max)
        real*8 r_high(burst_max)
	integer bestp
        real*8 az_pk_coef(3)
        real*8          coef_lk_ang(4,burst_max)
        real*8 dfddth
        integer beam_id(burst_max)
        real*8 az_peak_coef(3,8)


	m = 4
	k1 = bestp
	k2 = bestp+1
	prf1 = prf(k1)
	prf2 = prf(k2)
	r_low_in = max(r_low(k1),r_low(k2))
	r_high_in = min(r_high(k1),r_high(k2))
	r_cnt = (r_low_in+r_high_in)/2.0d0
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
        call v_poly(r_cnt,r_bip(k1),coef_fd(1,k1),m,fd1)
        call v_poly(r_cnt,r_bip(k2),coef_fd(1,k2),m,fd2)
c       f_diff = fd1 - fd2 - df_azoff
        f_diff = fd1 - fd2 


        write (6,*) 'df_azoff=',df_azoff
        write (6,*) 'fd1=',fd1
        write (6,*) 'fd2=',fd2
        write (6,*) 'fd1-fd2=',fd1-fd2
        write (6,*) 'f_diff = ', f_diff

	call n_prf_both(prf1,prf2,f_diff,nprf1,nprf2) !match by PRF diff Table
 	write (6,*) 'nprf1,nprf2', nprf1,nprf2

	return
	end



	subroutine n_prf_both(prf1,prf2,f_diff,n1,n2)
	implicit real*8 (a-h,o-z)
	real df(31,31)

        write(6,*)'Inside n_prf_both input: prf1=',prf1,' prf2=',prf2
        write(6,*)'                         f_diff=',f_diff
	do i = 1,31
	do j = 1,31
	df(i,j) = (i-16)*prf1-(j-16)*prf2
	end do
	end do

	amin = 1.e10
	do i = 1,31
	do j = 1,31
	if(abs(f_diff-df(i,j)).lt.amin) then
	n1 = i-16
	n2 = j-16
	amin = abs(f_diff-df(i,j))
	end if 
	end do
	end do

        n1 = - n1
        n2 = - n2

        write(6,*)' Found the min at=',n1,n2,' amin=',amin
	return
	end


	subroutine update_ryp(n_times,nbeams,btotal,burst_total,
     *  roll,yaw,pitch,burst_k,burst_k_count,beam_no)
	implicit real*8 (a-h,o-z)
	include 'ssp2_const.inc'
	real*8 x(200),y(200),cc(20,21),coef(20)
	integer btotal
c JMS 10-95
	integer burst_total
        real*8 roll(burst_max),yaw(burst_max),pitch(burst_max)
        integer burst_k(800),icount_last
        integer beam_no(burst_max)
        integer burst_k_count

c qdn 6/20/97
        integer  iorder_smooth, m_qdn, n_pt
        real*8   xroll,qdn_zero
        real*8   qx(200),qy(200),qmean,qsquare,qstd,qthreshold


        if (burst_total.le.70) then
          m=2
        else
	  m = 2
        endif



	ng = (btotal/n_times)/(nbeams*2)*(nbeams*2)
c	x_cntr = btotal/2
c	x_cntr = burst_k(burst_k_count)/2
 	x_cntr = burst_total/2
c	x_cntr = btotal/2
        write(6,*)'VALUE ',x_cntr,burst_k(burst_k_count)/2,burst_total/2
 

        icount=1
c	do i = 1, n_times
c	do k = 1,1
        do i=1,burst_k_count
         if (beam_no(burst_k(i)).eq.1) then
	  kburst = burst_k(i)
	  x(icount) = kburst-x_cntr
	  y(icount) = yaw(kburst)
	  icount = icount +1
          endif
        enddo
c	end do	
c	end do	
c**skipping bad bursts
c       icount_last=icount-1
c       icount=1
c       do i = 1, n_times
c	do k = 1,1
c	kburst = (i-1)*ng+k
c       if (burst_check(kburst).ne.1) then
c        if (icount.eq.1) then
c            y(icount)= y(icount+1)
c        else  if (icount.eq.icount_last) then
c            y(icount)= y(icount-1)
c        else
c            y(icount)= y(icount-1)
c        endif
c       endif
c 	icount = icount +1
c	end do	
c	end do	
c**skipping bad bursts

         qmean = 0.0
         qsquare = 0.0
         do i=1,n_times
            qmean = qmean+y(i)
            qsquare = qsquare + y(i)*y(i)
         end do
         qmean = qmean/n_times
         qsquare = qsquare/n_times
         qstd  = sqrt(qsquare - qmean*qmean)
         qthreshold = 1.5*qstd

         write(6,*)' the mean=',qmean,' qstd=',qstd,
     *             ' qthreshold=',qthreshold
         n_pt = 0
         do i=1,n_times
            if(abs(y(i)-qmean).le.qthreshold) then
               n_pt = n_pt + 1
               qy(n_pt) = y(i)
               qx(n_pt) = x(i)
            else
               write(6,*)'rejext the point',i,' y(i)',y(i),abs(y(i)-qmean)
            end if
         end do
     

         write(6,*)'QDN VALUE YAW AFTER SMOOTH out, n_pt=',n_pt
         write(6,*)' (qx,qy)=',('(',i,qx(i),qy(i),')',i=1,n_pt)
       
c qdn 6/23/97
         do i=1,m
            coef(i) = 0.0
         end do
         call lsf(qx,qy,n_pt,coef,m,cc)
         write(6,*)'QDN VALUE ROLL coeff=',(coef(i),i=1,m)

c qdn 7/29/97 replace the original fit
c        call lsf(x,y,n_times,coef,m,cc)
c qdn 6/5/97 print out the value
         write(6,*)'QDN VALUE YAW n_times=',n_times,' m=',m
         write(6,*)' (x,y)=',('(',i,x(i),y(i),')',i=1,n_times)
         write(6,*)' coef=',(coef(i),i=1,m)


	do k = 1, burst_total
	xk = k 
	call v_poly(xk,x_cntr,coef,m,yaw(k))
	end do
C******************************************

        icount=1
c	do i = 1, n_times
c	do k = 1,1
c	kburst = (i-1)*ng+k
        do i=1,burst_k_count
         if (beam_no(burst_k(i)).eq.1) then
          kburst = burst_k(i)
	  x(icount) = kburst-x_cntr                 !  JMS platform-dependent type conversion?
	  y(icount) = roll(kburst)
	  icount = icount +1
         endif
        enddo
c	end do	
c	end do	
c**skipping bad bursts
c       icount_last=icount-1
c       icount=1
c       do i = 1, n_times
c	do k = 1,1
c	kburst = (i-1)*ng+k
c       if (burst_check(kburst).ne.1) then
c        if (icount.eq.1) then
c            y(icount)= y(icount+1)
c        else  if (icount.eq.icount_last) then
c            y(icount)= y(icount-1)
c        else
c            y(icount)= y(icount-1)
c        endif
c       endif
c	icount = icount +1
c	end do	
c	end do	
c**skipping bad bursts

c qdn 6/5/97 print out the value
         write(6,*)'QDN VALUE ROLL n_times=',n_times,' m=',m
         write(6,*)' (x,y)=',('(',i,x(i),y(i),')',i=1,n_times)

         qmean = 0.0
         qsquare = 0.0
         do i=1,n_times
            qmean = qmean+y(i)
            qsquare = qsquare + y(i)*y(i)
         end do
         qmean = qmean/n_times
         qsquare = qsquare/n_times
         qstd  = sqrt(qsquare - qmean*qmean)
         qthreshold = 1.5*qstd

         write(6,*)' the mean=',qmean,' qstd=',qstd,
     *             ' qthreshold=',qthreshold
         n_pt = 0
         do i=1,n_times
            if(abs(y(i)-qmean).le.qthreshold) then
               n_pt = n_pt + 1
               qy(n_pt) = y(i)
               qx(n_pt) = x(i)
            else
               write(6,*)'rejext the point',i,' y(i)',y(i),abs(y(i)-qmean)
            end if
         end do
     

         write(6,*)'QDN VALUE ROLL AFTER SMOOTH out, n_pt=',n_pt
         write(6,*)' (qx,qy)=',('(',i,qx(i),qy(i),')',i=1,n_pt)
       

c        call lsf(x,y,n_times,coef,m,cc)
c        write(6,*)'QDN VALUE ROLL coeff=',(coef(i),i=1,m)

c qdn 6/23/97
         do i=1,m
            coef(i) = 0.0
         end do
         m_qdn = 1
         call lsf(qx,qy,n_pt,coef,m_qdn,cc)
         write(6,*)'QDN VALUE ROLL coeff=',(coef(i),i=1,m_qdn)

         qdn_zero = x_cntr
         do k=1,n_times
            call v_poly(x(k),qdn_zero,coef,m_qdn,xroll)
            write(6,*)'QDN ROLL at x=',x(k),' roll=',xroll
         end do

	do k = 1, burst_total
	xk = k 
c	call v_poly(xk,x_cntr,coef,m,roll(k))
	call v_poly(xk,x_cntr,coef,m_qdn,roll(k))
	end do

c qdn 6/24/97, 7/29/97 
c       do k=1,burst_total
c          roll(k) = roll(((k-1)/nbeams)*nbeams+1)
c       end do           

C******************************************

        icount=1
c	do i = 1, n_times
c	do k = 1,1
c	kburst = (i-1)*ng+k
        do i=1,burst_k_count
         if (beam_no(burst_k(i)).eq.1) then
          kburst = burst_k(i)
	  x(icount) = kburst-x_cntr
	  y(icount) = pitch(kburst)
	  icount = icount +1
         endif
        enddo
c	end do	
c	end do	
c**skipping bad bursts
c       icount_last=icount-1
c       icount=1
c	do i = 1, n_times
c	do k = 1,1
c	kburst = (i-1)*ng+k
c       if (burst_check(kburst).ne.1) then
c        if (icount.eq.1) then
c            y(icount)= y(icount+1)
c        else  if (icount.eq.icount_last) then
c            y(icount)= y(icount-1)
c        else
c            y(icount)= y(icount-1)
c        endif
c       endif
c	icount = icount +1
c	end do	
c	end do	
c**skipping bad bursts

	call lsf(x,y,n_times,coef,m,cc)
c qdn 6/5/97 print out the value
         write(6,*)'QDN VALUE PITCH n_times=',n_times,' m=',m
         write(6,*)' (x,y)=',('(',i,x(i),y(i),')',i=1,n_times)
         write(6,*)' coef=',(coef(i),i=1,m)


	do k = 1, burst_total
	xk = k 
	call v_poly(xk,x_cntr,coef,m,pitch(k))
D       write (6,*) 'roll,yaw,pitch=',k,roll(k),yaw(k),pitch(k)
	end do

C******************************************
	return
	end


c***************** end of pointing refinement ***************************

        subroutine return_node(burst_total,nbeams,kids,kburst,inode)
        integer burst_total,nbeams,kids,kburst,inode
        integer nset,burst_left,nset_cpu(16),nset_left
        integer iset_b(16),iset_e(16)

        nset=burst_total/(nbeams*2)
        burst_left=MOD(burst_total,(nbeams*2))
        do i=1,kids
          nset_cpu(i)=nset/kids
        enddo
        nset_left=MOD(nset,kids)
        if (nset_left.ne.0) then
         do i=1, nset_left
          nset_cpu(i)=nset_cpu(i)+1
         enddo
        endif


        iset_b(1)=1
        iset_e(1)=nset_cpu(1)
        do i=2 , kids
          iset_b(i)=iset_e(i-1)+1
          iset_e(i)=iset_b(i)+nset_cpu(i)-1
        enddo

        do i=1,kids
          iset_b(i) =   (iset_b(i)-1) *(nbeams*2)+1
          iset_e(i) =    iset_e(i)    *(nbeams*2)
          if (i.eq.kids) iset_e(i)= iset_e(i)+burst_left
          if ((kburst.ge.iset_b(i)).and.(kburst.le.iset_e(i))) then
            inode=i-1
          endif
        enddo
        return
        end 


        subroutine init_burst_check(burst_k_count,burst_k,burst_good,iflag,nbeams,
     *                        burst_start_p,burst_end_p,burst_count_start,burst_count_end,
     *                        mytask,kids)
	include 'ssp2_const.inc'
        integer burst_good(1600),burst_k(800)
        integer burst_k_count,isuccess,nbeams,ibegin,kburst,kburst_sub
        integer burst_start_p,burst_end_p
        integer burst_count_start,burst_count_end
        integer mytask,kids


        igood_area=1

        if (burst_count_start .ne. -1) then
          do i = burst_count_start,burst_count_end
            kburst=burst_k(i)
            ibegin=mod(kburst, nbeams)
            if (ibegin.eq.1) then
              call check_area(igood_area,nbeams,burst_good,kburst)
              if (igood_area.eq.0) then
                if (igood_area.eq.0) then
                 kburst_sub=kburst+nbeams*2
                 if ((kburst_sub+nbeams*2-1).le.burst_end_p) then
                  call check_area(igood_area,nbeams,burst_good,kburst_sub)
                  if (igood_area.eq.1) then
                   write(6,*)'BAD SPOT; SWITCHING TO RIGHT',kburst,' TO ',kburst_sub
                   call replace_area(kburst_sub,burst_k,i,nbeams)
                  endif
                 else
                  igood_area=0
                 endif
                endif
                if (igood_area.eq.0) then
                 kburst_sub=kburst-nbeams*2
                 if ((kburst_sub).ge.burst_start_p) then
                  call check_area(igood_area,nbeams,burst_good,kburst_sub)
                  if (igood_area.eq.1) then
                   write(6,*)'BAD SPOT; SWITCHING TO LEFT',kburst,' TO ',kburst_sub
                   call replace_area(kburst_sub,burst_k,i,nbeams)
                  endif
                 else
                  igood_area=0
                 endif
                endif
              endif
              if (igood_area.eq.0) goto  7878
             endif
          enddo
        endif

7878    continue
C******************************************************

        call mpi_barrier(MPI_COMM_WORLD,ierr)
        iflag=1
        if (mytask.eq.0) then
         if (igood_area.eq.0) iflag= 0 
         do iii=0,kids-1
          if (iii.eq.0) then
          else
            call mpi_bcast(iii,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
            call mpi_recv(igood_area,1,MPI_INTEGER,iii,0,MPI_COMM_WORLD,isuccess,ierr)
            if (igood_area.eq.0) iflag= 0
          endif
         enddo
        else
         do ii=1, kids-1
          call mpi_bcast(iii,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
          if (iii.eq.mytask) then
            call mpi_send(igood_area,1,MPI_INTEGER,0,0,MPI_COMM_WORLD,ierr)
          endif
         enddo
        endif
        call mpi_bcast(iflag     ,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
        if (iflag.eq.0) return

C******************************************************

        if (mytask.eq.0) then
            do iii=0,kids-1
             if (iii.eq.0) then
              else
               call mpi_bcast(iii,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
               call mpi_recv(num_c,1,MPI_INTEGER,iii,0,MPI_COMM_WORLD,isuccess,ierr)
               if (num_c .gt. 1) then
                call mpi_recv(istart,1,MPI_INTEGER,iii,1,MPI_COMM_WORLD,isuccess,ierr)
                call mpi_recv(iend  ,1,MPI_INTEGER,iii,1,MPI_COMM_WORLD,isuccess,ierr)
                do i = istart,iend
                  call mpi_recv(iburst,1,MPI_INTEGER,iii,1,MPI_COMM_WORLD,isuccess,ierr)
                  burst_k(i)=iburst
                enddo
               endif
             endif
            enddo
        else
         do ii=1, kids-1
          call mpi_bcast(iii,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
          if (iii.eq.mytask) then
            idummy= burst_count_end-burst_count_start+1
            call mpi_send(idummy,1,MPI_INTEGER,0,0,MPI_COMM_WORLD,ierr)
            if (idummy .gt. 1) then
              call mpi_send(burst_count_start,1,MPI_INTEGER,0,1,MPI_COMM_WORLD,ierr)
              call mpi_send(burst_count_end  ,1,MPI_INTEGER,0,1,MPI_COMM_WORLD,ierr)
              do i = burst_count_start,burst_count_end
               call mpi_send(burst_k(i),1,MPI_INTEGER,0,1,MPI_COMM_WORLD,ierr)
              enddo
            endif
          endif
         enddo
       endif


         
       call mpi_bcast(burst_k     ,800,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

        

        return
        end
C*********************************************************************************** 
        subroutine check_area(igood_area,nbeams,burst_good,kburst)
          integer igood_area
          integer nbeams,burst_good(1600),kburst

              igood_area=1
              do j=1,2*nbeams
               if (burst_good(kburst+j-1).ne.1) then
                igood_area=0
               endif
              enddo
      
        return
        end

        subroutine replace_area(kburst,burst_k,i,nbeams)
         integer kburst,burst_k(800),i,nbeams
         do j=1,nbeams
           burst_k(i+j-1)=kburst+j-1
         enddo
        return
        end
