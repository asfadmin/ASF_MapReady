c SccsId = @(#)COMMON_ZERO_ONE.f	2.41 3/24/98

	subroutine set_process(
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



c	include 'key_const.inc'
c	include 'key_pp.inc'
c	include 'proj_const.inc'
C*****WAYNE******
       include 'ssp2_const.inc'
       integer*4  ant_flag
       character*60,file_w1_dyn,file_w2_dyn,file_w3_dyn,file_s5_dyn,file_s6_dyn,file_s7_dyn,file_az_dyn
       character*60     file_topo
       real*8     topo_spacing
       integer     ire_sim
       character*60     file_w1_rg
       character*60     file_w2_rg
       character*60     file_w3_rg
       character*60     file_s5_rg
       character*60     file_s6_rg
       character*60     file_s7_rg
       character*60     file_az
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
       real*8     plen
       integer*2     topo(ns_ew,ns_ns)
       real*8     dlat_topo
       real*8     dlon_topo
       real     ant_patn_r(ns_ant_rg,7)
       real     ant_patn_az(ns_ant_az)
       real daz(ns_ant_rg,6)
       integer*4     v_filler
       integer*4     v_noise
       integer*4     sam_ac2
       integer*4     burst_start
       integer*4     burst_end
       integer*4     burst_total
       integer*4     status 
       integer*4  dk_burst(burst_max)
       integer   kids,mytask,istatus
       integer*4     burst_start_p
       integer*4     burst_end_p
       character*60  file_dk
       integer nset,burst_left,nset_cpu(16),nset_left
       integer iset_b(16),iset_e(16)
       integer burst_pvs,pvs_node
       integer nbuf(4),dontcare,allmsg,nulltask,allgrp
C*****WAYNE******
       real sinc,ang_ratio
       integer btotal
       integer burst_total_adjusted

c***************************************************************
        allgrp   = MPI_COMM_WORLD


        write(6,*)'kids = ',kids,' mytask= ',mytask

c        burst_start = 1
c        burst_end =1472 !1472
         burst_total = burst_end
         burst_pvs=(burst_end-burst_start+1)/2+1

        nset=burst_total/(nbeams*2)
        burst_left=MOD(burst_total,(nbeams*2))
        do i=1, kids
         nset_cpu(i)=nset/kids
        enddo
        nset_left=MOD(nset,kids)

        if (nset_left.ne.0) then
         do i=1, nset_left
          nset_cpu(i)=nset_cpu(i)+1
         enddo
         write(6,*)'NONE-SYMMETRIC PARALLEL JOBS'
         idummy=printflog(3,'A NONE-SYMMETRIC PARALLEL JOB with uneven bursts processed'//'&')
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
          if (i.eq.kids) iset_e(i)= iset_e(i) + burst_left
          if ((burst_pvs.ge.iset_b(i)).and.(burst_pvs.le.iset_e(i))) then
            pvs_node=i-1
          endif
        enddo
        if (iprocessor.eq.1) then
         n_times = n_times_pre
         btotal = (burst_end-2*nbeams)/((n_times-1)*nbeams)*n_times*nbeams
         kg = 1 + (n_times-1)*(btotal/n_times/(nbeams*2)*(nbeams*2))
         burst_end=kg+nbeams-1+nbeams
         iset_e(kids)=burst_end
        endif
        burst_start_p = iset_b(mytask+1)
        burst_end_p =   iset_e(mytask+1)
        call init_dk_burst(burst_start_p,burst_end_p,dk_burst,file_dk,nbeams,mytask,kids)


c***************************************************************

c** set range decode look up table *******************************

	call raw_tbl(tbl)
        write(6,*)'tbl(119) ',tbl(119)

c** set range pulse parameters ***********************************

	chirp_bw = bw_3

	dtau = 1./fs
	chirp_rate = -chirp_bw/plen	!one can adjust chirp sign
        write(6,*)chirp_rate,plen

c***************************************************************

c* bandwidth fixed for the white lines ***
        nfft_az = 64
        max_look = 2
        if(nbeams.ne.4) then
c       max_look = 2
        nfft_az = 128
        end if
c	if(iquick.eq.1) nfft_az=8




c********* read topo model ***************************************

        if (dem_ellip .eq. dem) then
	ipt = 0
	nbyte = ns_ew * ns_ns *2
	write (6,*) 'read topo'
        write (6,*) 'file_topo = ',file_topo
	call read_disk(file_topo,topo,nbyte,ipt,istatus)
        endif
        if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
	dlat_topo = topo_spacing
	dlon_topo = topo_spacing

c********* read range antenna patterns ***************************************

	if(ire_sim.eq.1) then
	   ipt = 0
	   nbyte = 4* ns_ant_rg
           if (mytask.eq.0) then
            if (ant_flag.eq.0) then
	      call read_disk(file_w1_rg,ant_patn_r(1,1),nbyte,ipt,istatus)
              if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) goto 6789
	      call read_disk(file_w2_rg,ant_patn_r(1,2),nbyte,ipt,istatus)
              if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) goto 6789
	      call read_disk(file_w3_rg,ant_patn_r(1,3),nbyte,ipt,istatus)
              if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) goto 6789
	      call read_disk(file_s5_rg,ant_patn_r(1,5),nbyte,ipt,istatus)
              if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) goto 6789
	      call read_disk(file_s6_rg,ant_patn_r(1,6),nbyte,ipt,istatus)
              if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) goto 6789
	      call read_disk(file_s7_rg,ant_patn_r(1,7),nbyte,ipt,istatus)
              if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) goto 6789
            else
	      call read_disk(file_w1_dyn,ant_patn_r(1,1),nbyte,ipt,istatus)
              if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) goto 6789
	      call read_disk(file_w2_dyn,ant_patn_r(1,2),nbyte,ipt,istatus)
              if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) goto 6789
	      call read_disk(file_w3_dyn,ant_patn_r(1,3),nbyte,ipt,istatus)
              if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) goto 6789
	      call read_disk(file_s5_dyn,ant_patn_r(1,5),nbyte,ipt,istatus)
              if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) goto 6789
	      call read_disk(file_s6_dyn,ant_patn_r(1,6),nbyte,ipt,istatus)
              if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) goto 6789
	      call read_disk(file_s7_dyn,ant_patn_r(1,7),nbyte,ipt,istatus)
              if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) goto 6789
            endif
c           call read_disk(file_az,daz,700*6*4,0,istatus)
c           if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) goto 6789
           endif
6789       continue
           call mpi_bcast(istatus,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
           if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
           call mpi_bcast(ant_patn_r,ns_ant_rg*7,MPI_REAL,0,MPI_COMM_WORLD,ierr)
c          call mpi_bcast(daz,ns_ant_rg*6,MPI_REAL,0,MPI_COMM_WORLD,ierr)


C*** DIMMING DOWN
c          do k = 1,ns_ant_rg
c	    ant_patn_r(k,5) = ant_patn_r(k,5) - 0.25
c          enddo
c          do k = 1,ns_ant_rg/6
cc          v_scale = 0.10 + (1.0 -0.10) /(ns_ant_rg/6)*k
c           v_scale = 1.0 + (2.0 -1.0) /(ns_ant_rg/6)*k
c           ant_patn_r(k,6) = ant_patn_r(k,6) - v_scale
c          enddo
C*** DIMMING DOWN
           

	   do j = 1, 7
	   do k = 1, ns_ant_rg
           ant_patn_r(k,j) = 10.**((ant_patn_r(k,j)-42.3368)/10.)  !dB convert to linear
c          ant_patn_r(k,j) = 10.**(ant_patn_r(k,j)/10.)  !dB convert to linear
	   end do
	   end do
           write(6,*)'scansar ant_r(637/679,7)',ant_patn_r(637,7),
     *                 ant_patn_r(679,7)


	else

	   do j = 1, 4
	   do k = 1, ns_ant_rg
	   rg_ang = (k-ns_ant_rg/2-1)* dang_rg
	   ang_ratio = rg_ang/rng_bm_ang(j)
	   ant_patn_r(k,j) = sinc(ang_ratio,pi)**2	  !**2 is 1-way gain
	   end do
	   end do
	end if

c********* generate azimuth antenna patterns ********************

D       write(6,*)'DELTA AZ',dang_az
	do k = 1, ns_ant_az
	az_ang = (k-1)* dang_az
	ang_ratio = az_ang/(az_bm_ang)
	ant_patn_az(k) = sinc(ang_ratio,pi)**2		  !**2 is 1-way gain
D       write(6,*)'k,ant_patn_az',k,ant_patn_az(k)
	end do

c       if (ant_flag.eq.1) then
c        ipt = 0
c	 nbyte = 4* ns_ant_az
c	 call read_disk(file_az_dyn,ant_patn_az,nbyte,ipt,istatus)
c        if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
c       endif

c** set image filler pixel value *********************************

        if (ire_sim .eq. 0) then
           v_filler =25         !sim
        else
           v_filler =0          !real
        endif
	v_noise = 0

        sam_ac2 = nint(fm_sizec2/c2_pxl) 	!# of samples in ac_image
        write(6,*)'WHAT IS *************',sam_ac2

	if(c1_pxl.eq.100) djump = 200
	if(c1_pxl.eq.50) djump =400

	return
	end
      	subroutine raw_tbl(tbl)

	implicit none

        character*128 SccsId_COMMON_ZERO_ONE
        data SccsId_COMMON_ZERO_ONE
     +  /'@(#)PPCOMMON_ZERO_ONE.f:2.41'/


c	------------
c	OUTOUT PARAMETERS
c	------------
      	complex tbl( -128:127 )

c	byte  b_data
c	------------
c	LOCAL VARIABLES
c	------------
 	real*4 wt(0:15)/  .5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 
     *                  -7.5,-6.5,-5.5,-4.5,-3.5,-2.5,-1.5,-0.5/
        integer i
      	integer di, dq



	do i = -128, 127

c	   di = iand( ishft( i, -4 ), '0000000f'x )
c	   dq = iand( i, '0000000f'x )
 	   di = iand( ishft( i, -4 ), 15 )
 	   dq = iand( i, 15 )
	   tbl( i ) = cmplx( wt( di ), wt( dq ) )

	end do
	return
      end
	subroutine get_burst_aux(k,jpulse,plen,nr,
     *  ire_sim,proj,bm_id,
     *  nbeams,sv_ref_time,nfft_az,fs,
     *  istatus,beam_no,beam_id,t_burst,
     *  burst_cnt,prf,ns,np,
     *  look_ang,np_air,x_sc,y_sc,
     *  z_sc,v_x_sc,v_y_sc,v_z_sc,
     *  prf_s,df,dr,r_1st,
     *  roll_o,yaw_o,yaw,pitch_o,
     *  pitch,roll,file_aux,file_spc_raw,
     *  sim_eam_nsp,peak_bm_ang,
     *  sv_x_pos,sv_y_pos,sv_z_pos,sv_x_vel,sv_y_vel,sv_z_vel,
     *  t_b,gha,rg_gate_delay,np_v,gha_time)

c	IMPLICIT REAL*8 (A-H,O-Z)
 	IMPLICIT none
c	include 'key_const.inc'
c	include 'key_pp.inc'
       include 'ssp2_const.inc'
        real*8  gha_time
        real*8          sv_x_pos
        real*8          sv_y_pos
        real*8          sv_z_pos
        real*8          sv_x_vel
        real*8          sv_y_vel
        real*8          sv_z_vel
        real*8          gha
        real*8          t_b(burst_max)
        real*8          rg_gate_delay(burst_max)
        real*8          peak_bm_ang(4)
        integer         sim_eam_nsp(4)
        integer*4       np_v(burst_max)
C*****WAYNE******
       real*8      plen
       integer*4   nr
       integer     ire_sim
       integer     proj
       real*8     bm_id(4)
       integer*4     nbeams
       real*8     sv_ref_time
       integer*4     nfft_az
       real*8     fs
       integer*4    istatus 
       integer*4     beam_no(burst_max)
       integer*4     beam_id(burst_max)
       real*8     t_burst(burst_max)
       integer*4     burst_cnt(burst_max)
       real*8     prf(burst_max)
       integer*4     ns(burst_max)
       integer*4     np(burst_max)
       real*8     look_ang(burst_max)
       integer*4     np_air(burst_max)
       real*8     x_sc(burst_max)
       real*8     y_sc(burst_max)
       real*8     z_sc(burst_max)
       real*8     v_x_sc(burst_max)
       real*8     v_y_sc(burst_max)
       real*8     v_z_sc(burst_max)
       real*8     prf_s(burst_max)
       real*8     df(burst_max)
       real*8     dr(burst_max)
       real*8     r_1st(burst_max)
       real*8     roll_o(burst_max)
       real*8     yaw_o(burst_max)
       real*8     yaw(burst_max)
       real*8     pitch_o(burst_max)
       real*8     pitch(burst_max)
       real*8     roll(burst_max)  
       character*60     file_aux,file_spc_raw
C*****WAYNE******
       integer icyc_skip,nb,ns4,dk_ptr0,i,jpulse,k
       real*8  x_ini,y_ini,z_ini,xs0_eme,ys0_eme,zs0_eme
       real*8  vx_ini,vy_ini,vz_ini,xv0_eme,yv0_eme,zv0_eme
       real*8  xs1_eme,ys1_eme,zs1_eme,xv1_eme,yv1_eme,zv1_eme
       real*8  sec_t1,delta_t,rst
       real*8  sc_t_offset,r_sc
C*****WAYNE******
	integer nppb
	parameter (nppb = 65)
	integer nsp(4),itemp,dk_ptr,bytes
c	byte a(30000,nppb)
	real*8 auxiliary(32)
c	common /aux/auxiliary
        real*8  r_x2,r_y2,r_z2,v_x2,v_y2,v_z2


c************************************************
        IF (ire_sim.eq.0) THEN
         nr=542
         nbeams = 4
         nsp(1) = sim_eam_nsp(1)
         nsp(2) = sim_eam_nsp(2)
         nsp(3) = sim_eam_nsp(3)
         nsp(4) = sim_eam_nsp(4)
        dk_ptr0= (k-1)*nppb*32*8


	bytes = 32*8
c 	dk_ptr = dk_ptr0+(jpulse-1)*nsp(nb)*2
       dk_ptr = dk_ptr0+(jpulse-1)*32*8
	call read_disk(file_aux,auxiliary,bytes,dk_ptr,istatus)
c 	call read_disk(file_spc_raw,auxiliary,bytes,dk_ptr,istatus)
        if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
c       write(6,*)'are we here'


           beam_no(k) = k - 4*int((k-1)/4) 
           beam_id(k) = bm_id(beam_no(k))
	   t_burst(k) = auxiliary(1)
           burst_cnt(k) = (auxiliary(2)-1)*nbeams+auxiliary(3)
	   prf(k) = auxiliary(6)
           ns(k) = auxiliary(5)-nr
           np(k) = 65
           look_ang(k) = auxiliary(7)
c          look_ang(k) = -roll_o(k)-roll(k)+peak_bm_ang(beam_no(k))
	   np_air(k) = auxiliary(8)
           x_sc(k)   = auxiliary(9)
           y_sc(k)   = auxiliary(10)
           z_sc(k)   = auxiliary(11)
         v_x_sc(k)   = auxiliary(12)
         v_y_sc(k)   = auxiliary(13)
         v_z_sc(k)   = auxiliary(14)
         r_1st (k)   = (np_air(k)/prf(k)+plen)*cspeed/2.d0
     *                 -2.934d0*0.5d0*cspeed/fs
           itemp = prf(k)*100.d0
           prf_s(k) = itemp
           prf_s(k) = prf_s(k)/100.d0
           df(k) = prf_s(k)/nfft_az
           dr(k) = 0.5d0*cspeed/fs

c       write(6,*)'are we here'
	   roll(k) = roll_o(k)
	   yaw(k)  = yaw_o(k)
	   pitch(k)= pitch_o(k)
      ELSE IF (ire_sim.eq.1)   THEN

          look_ang(k) = look_ang_cnt+peak_bm_ang(beam_no(k))
c          roll_o from aux roll from pta_refine
c          look_ang(k) = -roll_o(k)-roll(k)+peak_bm_ang(beam_no(k))


          if (k .eq. 1) then    !1st burst
            r_sc=sqrt(sv_x_pos**2+sv_y_pos**2+sv_z_pos**2)
D            write(6,*)' R xyz:',k,r_sc,sv_x_pos,sv_y_pos,sv_z_pos
D            write(6,*)'*statevec vel', sv_x_vel,sv_y_vel,sv_z_vel


             sec_t1 = sv_ref_time  !in second from get_config.f
             sc_t_offset=t_b(1) - sec_t1 !sc ref time in leading edge
             delta_t=(np(1)-np_v(1)/2)/prf(1)+sc_t_offset
             t_burst(1)=delta_t+sec_t1  !Center time of the burst
D            write(6,*)'B1:',t_b(1),np(1), np_v(1),prf(1),sc_t_offset
D            write(6,*)'B1:t0,delta bt1 gha',sec_t1,delta_t,t_burst(1),gha


          else
              delta_t = (np_v(k-1)/2.)/prf(k-1)
     *                + (np(k)-np_v(k)/2.)/prf(k)
              t_burst(k)=delta_t+t_burst(k-1)  !Center time of the burst
              delta_t = t_burst(k) - sv_ref_time
              sec_t1= sv_ref_time  !Center time of the burst
          endif

              call propagation(sv_x_pos,sv_y_pos,sv_z_pos,
     *                   sv_x_vel,sv_y_vel,sv_z_vel,
     *                         sec_t1,delta_t,
     *                         xs1_eme,ys1_eme,zs1_eme,
     *                         xv1_eme,yv1_eme,zv1_eme)
D             write(6,*)'LAST',k
D             write(6,*)'LAST',sv_x_pos,sv_y_pos
D             write(6,*)'LAST',sv_z_pos,sv_x_vel
D             write(6,*)'LAST',sv_y_vel,sv_z_vel
D             write(6,*)'LAST',sec_t1,delta_t
D             write(6,*)'LAST',xs1_eme,ys1_eme,zs1_eme
D             write(6,*)'LAST',xv1_eme,yv1_eme,zv1_eme

c             call propa_simp(sv_x_pos,sv_y_pos,sv_z_pos,
c    *                   sv_x_vel,sv_y_vel,sv_z_vel,
c    *                         sec_t1,delta_t,
c    *                         r_x2,r_y2,r_z2,v_x2,v_y2,v_z2)
c
cD               write(6,*) 'error of propg', xs1_eme-r_x2,ys1_eme-r_y2,zs1_eme-r_z2,
cD    *                                    xv1_eme-v_x2,yv1_eme-v_y2,zv1_eme-v_z2


D             write(6,*)'OUT PROPOGATION'
D             write(6,*)'HEY2',gha,gha_time
D             write(6,*)'HEY2',t_burst(k)
D             write(6,*)'HEY2',xs1_eme,ys1_eme
D             write(6,*)'HEY2',zs1_eme
c             stop

              call EME_to_EBF(gha,gha_time,t_burst(k),
     *                         xs1_eme,ys1_eme,zs1_eme,
     *                         x_sc(k),y_sc(k),z_sc(k))
D             write(6,*)'OUT EME_TO_EBF for post'
D              write(6,*)'HEY',gha,gha_time,t_burst(k)
D              write(6,*)'HEY',xs1_eme,ys1_eme,zs1_eme
D              write(6,*)'HEY',x_sc(k),y_sc(k),z_sc(k)
c             if (k.eq.4) stop



              call EME_to_EBF(gha,gha_time,t_burst(k),
     *                         xv1_eme,yv1_eme,zv1_eme,
     *                         v_x_sc(k),v_y_sc(k),v_z_sc(k))
D             write(6,*)'OUT EME_TO_EBF for vel'



         itemp = prf(k)*100.d0
         prf_s(k) = itemp
         prf_s(k) = prf_s(k)/100.d0
         df(k) = prf_s(k)/nfft_az
         dr(k) =  0.5d0*cspeed/fs

         roll(k) =roll_o(k)
         yaw(k) = yaw_o(k)
         pitch(k) = pitch_o(k)

c qdn  6/2/97
c        roll(k) = roll_o(k) - 0.5   !qdn testing
c        write(6,*)'0.5roll,yaw,pitch',roll(k),roll_o(k),yaw(k),pitch(k)


c        write(6,*)'roll,yaw,pitch',roll(k),yaw(k),pitch(k)

        call get_np_air(look_ang_cnt,peak_bm_ang(beam_no(k)),
     *                        roll(k),
     *                        x_sc(k),y_sc(k),z_sc(k),Re,Rp,
     *                        plen,prf(k),rg_gate_delay(k),
     *                        cspeed,rst,np_air(k))
c        write(6,*)'are we back',np_air(k),prf(k),rg_gate_delay(k),dr(k)

c       r_1st(k) = (np_air(k)/prf(k)+rg_gate_delay(k))*cspeed/2.d0
c    *                 -2.934d0*dr(k)
c qdn 4/10/97 modified the slant range by 220 meters back
c qdn 4/11/97 modified the slant range by 140 meter back
c qdn 4/18/97 modified the slant range by 0 meter back
c qdn 4/21/97 modified the slant range by 30 meter back
        r_1st(k) = (np_air(k)/prf(k)+rg_gate_delay(k))*cspeed/2.d0
     *                 -2.934d0*dr(k)  - 30.0

c        write(6,*) r_1st(k) ,k, r_1st(k)
c        1 sample after and .934 is the fraction of plen
        if (k .le.8) then
c         write(6,*)'look_ang:',look_ang_cnt+peak_bm_ang(k)-roll(k)
c        write(6,*)'np_delay,rst :',np_air(k),rst
c        write(6,*)'rggate prf bean:',rg_gate_delay(k),prf(k),beam_no(k)
        endif
c       write(6,*)'**R_1st np_air:',k,r_1st(k),np_air(k)





  
        ENDIF

	return
	end

        subroutine propa_simp(r_x1,r_y1,r_z1,v_x1,v_y1,v_z1,
     *                         start_time,delta_t,
     *                         r_x2,r_y2,r_z2,v_x2,v_y2,v_z2)
        implicit real*8 (a-h,o-z)

        gmass = 3.9860045e14
        dt = 1.e-3
        loop = int(delta_t/dt)+1

        r_x2 = r_x1
        r_y2 = r_y1
        r_z2 = r_z1
        v_x2 = v_x1
        v_y2 = v_y1
        v_z2 = v_z1

        do i = 1, loop
        if(i.eq.loop) dt = delta_t - (loop-1)*dt
        rcub = (r_x2**2+r_y2**2+r_z2**2)**1.5
        ax = - gmass/rcub * r_x2
        ay = - gmass/rcub * r_y2
        az = - gmass/rcub * r_z2
        v_x2 = v_x2 + ax * dt
        v_y2 = v_y2 + ay * dt
        v_z2 = v_z2 + az * dt
        r_x2 = r_x2 + v_x2 * dt
        r_y2 = r_y2 + v_y2 * dt
        r_z2 = r_z2 + v_z2 * dt
        end do

        return
        end

	subroutine alpha123 ( x,y,z,vx,vy,vz,re,alpha1,alpha2,alpha3)

c       ------------------------------------------------
C	This subroutine generates alpha1,alpha2, alpha3 
c	angles of the new projection coordinate system.
c       ------------------------------------------------
	implicit none
c	include 'proj_const.inc'
C*****WAYNE******
c       include 'ssp2_const.inc'
        real*8     alpha1
        real*8     alpha2
        real*8     alpha3
C*****WAYNE******

c	----------------
C	PASSING IN PARAMETERS
c	----------------
	real*8	x,y,z		!Sensor position
	real*8	vx,vy,vz	!Sensor velocity
	real*8	re		!Earth radius
	real*8	incl_ang	!Inclination angle

c	----------------
C	PASSING OUT PARAMETERS
c	----------------
c	real*8	alpha1,alpha2,alpha3   !defined in proj_const.inc

c	----------------
C	LOCAL VARIABLES
c	----------------
	real*8 z_orbit(3)	!Orbit normal vector 
	real*8 y_axis(3)	!Y axis
	real*8 a(3)		!Sensor position vector A at the mostly
				!north point, namely unit vector of the
				!X-axis of the new coordinate system. 
	real*8 theta		!Angle between vector A and sensor position
				!of the 1st burst 
	real*8 r_mag
	real*8 n_12_x,n_12_y,n_12_z
	real*8 a_12_x,a_12_y,a_12_z
	real*8 n_a_x,n_a_y,n_a_z
	real*8 alpha3_sign
	real*8 nd1,nd2,nd3

	real*8 pi

	pi=atan(1.d0)*4.d0

c	------------
c	GET UNIT VECTOR A, AS NEW X-AXIS COORDINATE
c	------------
        call cross(x,y,z,vx,vy,vz,
     *             z_orbit(1),z_orbit(2),z_orbit(3))

        call cross(z_orbit(1),z_orbit(2),z_orbit(3),
     *             0.d0,0.d0,1.d0,
     *             y_axis(1),y_axis(2),y_axis(3))

        call cross(y_axis(1),y_axis(2),y_axis(3),
     *             z_orbit(1),z_orbit(2),z_orbit(3),
     *             a(1),a(2),a(3))

c	------------
c	GET ROTATE ANGLES,alpha 1 & 2, TO TRANSFER COORDINATE TO BE 
c	AS NEW S/C ORBIT EQUATOR IN X-Y PLANE
c	------------
D	write(6,*)'ALPHA123 new x-vector:',a(1),a(2),a(3)
        if(abs(a(1)).lt.1.e-10.and.abs(a(2)).lt.1.e-10) then
	alpha1=0.d0
	else
	alpha1=atan2d(a(2),a(1))
        end if
	alpha2=-1.d0*asind(a(3))
	if (z_orbit(3) .lt. 0.d0) then
	   alpha1 = 180.0d0 + alpha1
	   alpha2 = -1.*(180.0d0 - abs(alpha2))
        endif
D	   write(6,*)'alpha1 alpha2',alpha1,alpha2

c	------------
c	GET the 3rd ANGLE for
c	TRANSFERING THE 1ST BURST POSITION AS A REFERENCE POINT IN 
c	NEW EQUATOR X-Y PLANE 
c	------------
        call cross(a(1),a(2),a(3),x,y,z,nd1,nd2,nd3)

	alpha3_sign=nd1*z_orbit(1)+nd2*z_orbit(2)+nd3*z_orbit(3)
	r_mag = sqrt(x**2+y**2+z**2)

	alpha3 = acosd((a(1)*x+a(2)*y+a(3)*z)/r_mag)
	if(alpha3_sign .lt. 0.d0) alpha3 = - alpha3
D	   write(6,*)'alpha3',alpha3


	return
	end




c------------------ end of raw_decode.f ---------------------------



       subroutine read_aux_data(
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

	implicit real*8 (a-h, o-z)
c	include 'key_const.inc'
c	include 'key_pp.inc'
       include 'ssp2_const.inc'
        real*8   gha_time
        real*8          sv_x_pos
        real*8          sv_y_pos
        real*8          sv_z_pos
        real*8          sv_x_vel
        real*8          sv_y_vel
        real*8          sv_z_vel
        real*8          gha
        real*8          t_b(burst_max)
        real*8          rg_gate_delay(burst_max)
        real*8          peak_bm_ang(4)
        integer         sim_eam_nsp(4)
        integer*4       np_v(burst_max)
C*****WAYNE******
       real*8       alpha1,alpha2,alpha3
       integer*4     burst_start
       integer*4     burst_end
       integer*4     np(burst_max)
       integer*4     istatus
       integer     ire_sim
       integer     proj
       real*8     bm_id(4)
       integer*4     nbeams
       real*8     sv_ref_time
       integer*4     nfft_az
       real*8     fs
       integer*4     beam_no(burst_max)
       integer*4     beam_id(burst_max)
       real*8     t_burst(burst_max)
       integer*4     burst_cnt(burst_max)
       real*8     prf(burst_max)
       integer*4     ns(burst_max)
       real*8     look_ang(burst_max)
       integer*4     np_air(burst_max)
       real*8     x_sc(burst_max)
       real*8     y_sc(burst_max)
       real*8     z_sc(burst_max)
       real*8     v_x_sc(burst_max)
       real*8     v_y_sc(burst_max)
       real*8     v_z_sc(burst_max)
       real*8     prf_s(burst_max)
       real*8     df(burst_max)
       real*8     dr(burst_max)
       real*8     r_1st(burst_max)
       real*8     roll_o(burst_max)
       real*8     yaw_o(burst_max)
       real*8     yaw(burst_max)
       real*8     pitch_o(burst_max)
       real*8     pitch(burst_max)
       real*8         roll(burst_max)  
       real*8     plen
       integer*4  nr
       character*60 file_aux,file_spc_raw
C*****WAYNE******

        write(6,*)'before first get_burst_aux'
        call get_burst_aux(1,1,plen,nr,
     *  ire_sim,proj,bm_id,
     *  nbeams,sv_ref_time,nfft_az,fs,
     *  istatus,beam_no,beam_id,t_burst,
     *  burst_cnt,prf,ns,np,
     *  look_ang,np_air,x_sc,y_sc,
     *  z_sc,v_x_sc,v_y_sc,v_z_sc,
     *  prf_s,df,dr,r_1st,
     *  roll_o,yaw_o,yaw,pitch_o,
     *  pitch,roll,file_aux,file_spc_raw,
     *  sim_eam_nsp,peak_bm_ang,
     *  sv_x_pos,sv_y_pos,sv_z_pos,sv_x_vel,sv_y_vel,sv_z_vel,
     *  t_b,gha,rg_gate_delay,np_v,gha_time)

        write(6,*)'after first get_burst_aux'

        call alpha123(x_sc(1),y_sc(1),z_sc(1),
     *  v_x_sc(1),v_y_sc(1),v_z_sc(1),re,alpha1,alpha2,alpha3)
        write(6,*)'ALPHA123',x_sc(1),y_sc(1),z_sc(1)
        write(6,*)'ALPHA123',v_x_sc(1),v_y_sc(1),v_z_sc(1)
        write(6,*)'ALPHA123',alpha1,alpha2,alpha3
        if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return



	do k = burst_start, burst_end
        call get_burst_aux(k,9+(np(1)-9)/2,plen,nr,
     *  ire_sim,proj,bm_id,
     *  nbeams,sv_ref_time,nfft_az,fs,
     *  istatus,beam_no,beam_id,t_burst,
     *  burst_cnt,prf,ns,np,
     *  look_ang,np_air,x_sc,y_sc,
     *  z_sc,v_x_sc,v_y_sc,v_z_sc,
     *  prf_s,df,dr,r_1st,
     *  roll_o,yaw_o,yaw,pitch_o,
     *  pitch,roll,file_aux,file_spc_raw,
     *  sim_eam_nsp,peak_bm_ang,
     *  sv_x_pos,sv_y_pos,sv_z_pos,sv_x_vel,sv_y_vel,sv_z_vel,
     *  t_b,gha,rg_gate_delay,np_v,gha_time)
D       write(6,*)'DONE LOAD AUX BURST # ',k

        if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
	end do

	return
	end

        subroutine init_dk_burst(burst_start_p,burst_end_p,dk_burst,file_dk,nbeams,mytask,kids)
        include 'ssp2_const.inc'
        character*60  file_dk
        integer*4 ifd,bytes,nbyte,burst_start_p,burst_end_p,copen_r
        integer*4 dk_burst(burst_max),cread_seq,ptr
        integer nbeams
        integer burst_end_p_adjust,mytask,kids

        if (mytask.eq.kids) then
         burst_end_p_adjust=burst_end_p
        else
         burst_end_p_adjust= burst_end_p+nbeams
        endif
 


        ifd            =copen_r(file_dk)
        bytes          =4
        do 100 i=1, burst_max
c        if ((i.ge.burst_start_p).and.(i.le.burst_end_p_adjust)) then
         if ((i.ge.burst_start_p).and.(i.le.burst_end_p)) then
         nbyte          =cread_seq(ifd,ptr,bytes)
         dk_burst(i)=    ptr
         else
          dk_burst(i)= -9999
         endif
100     continue

        return
        end
        subroutine init_fft(n_fft)
        implicit none

        integer n_fft,isign
        complex a(4096)
        real*8  aux1(30000),aux2(30000),aux3(30000),aux4(30000)
        common /twid/aux1,aux2,aux3,aux4

        isign=1
        call scft(1,a,1,n_fft,a,1,n_fft
     *  ,n_fft,1,isign,1.0,aux1,30000,aux2,30000)
c    *  ,n_fft,1,1,1.0,aux1,30000,aux2,30000)

        call scft(1,a,1,n_fft,a,1,n_fft
     *  ,n_fft,1,-1,1.0,aux3,30000,aux4,30000)
        return
        end


c*************************************************************************
        subroutine cfft(a,icr,n_fft,iflag)
        implicit none

        complex a(4096)
        integer n_fft,icr,iflag,isign
        real*8  aux1(30000),aux2(30000),aux3(30000),aux4(30000)
        common /twid/aux1,aux2,aux3,aux4

        isign = iflag

        if (isign .eq. 1)then
        call scft(0,a,1,n_fft,a,1,n_fft
     *  ,n_fft,1,1,1.0,aux1,30000,aux2,30000)
        else
        call scft(0,a,1,n_fft,a,1,n_fft
c    *  ,n_fft,1,1,1.0,aux1,30000,aux2,30000)
     *  ,n_fft,1,isign,1.0,aux3,30000,aux4,30000)
        end if

        return
        end
       subroutine corner_turn(
     *  kburst,nr,ns,np_proc,nfft_az,
     *  buff1,buff2       )
C*****WAYNE******
       include 'ssp2_const.inc'
       integer     kburst
       integer*4   nr
       integer*4     ns(burst_max)
       integer     np_proc
       integer*4     nfft_az
       complex     buff1(sam_raw,line_raw)
       complex     buff2(line_raw,sam_raw)
C*****WAYNE******
c	include 'key_const.inc'
c	include 'key_pp.inc'

c$doacross local(i,j)
	do i = 1, ns(kburst)-nr
	do j = 1, np_proc
	buff2(j,i) = buff1(i,j)
	end do
	do j = np_proc+1, nfft_az
	buff2(j,i) = cmplx(0.,0.)
	end do
	end do

	return
	end
       subroutine get_burst_sim(
     *  kburst,plen,np_proc,fs,file_spc_raw,
     *  ns,buff0,istatus,buff1,dk_burst,file_raw,
     *  sim_eam_nsp)
	IMPLICIT REAL*8 (A-H,O-Z)
C*****WAYNE******
       include 'ssp2_const.inc'
       integer     kburst
       integer     np_proc
       real*8     fs
       character*60     file_spc_raw
       byte     buff0(2*sam_raw,line_raw)
       integer*4     istatus
       integer*4     ns(burst_max)
       complex     buff1(sam_raw,line_raw)
       real*8      plen
       integer*4     dk_burst(burst_max)
       character*60     file_raw
C*****WAYNE******
        integer         sim_eam_nsp(4)
C*****WAYNE******
	integer nppb
	parameter (nppb = 65)
	integer bytes,dk_ptr0,dk_ptr
	real*8 auxiliary(32)
	integer nsp(4)
c	include 'key_const.inc'
c	include 'key_pp.inc'

	nbeam = 4
	icyc_skip = (kburst-1)/nbeam
	nb = kburst- icyc_skip*nbeam
c	write(6,*)'icycle,nbeam=', icyc_skip+1,nb
	nsp(1) =sim_eam_nsp(1)
	nsp(2) =sim_eam_nsp(2)
	nsp(3) =sim_eam_nsp(3)
	nsp(4) =sim_eam_nsp(4)
	ns4 = nsp(1)+nsp(2)+nsp(3)+nsp(4)

	dk_ptr0 = icyc_skip*ns4*nppb*2	

	do k =1, nb-1
	dk_ptr0 = dk_ptr0+nsp(k)*nppb*2
	end do 

	bytes = 2*(nsp(nb)-plen*fs)

	do j = 1, np_proc
c	dk_ptr = dk_ptr0+int(plen*fs)*2+(j+9-1)*nsp(nb)*2
 	dk_ptr = dk_burst(kburst)+int(plen*fs)*2+(j+9-1)*nsp(nb)*2
c       call read_disk(file_spc_raw,buff0(1,j),bytes,dk_ptr,istatus)
        call read_disk(file_raw,buff0(1,j),bytes,dk_ptr,istatus)
        if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
	end do

        call decode_sim(kburst,plen,
     *  np_proc,ns,fs,buff0,
     *  buff1        )


	return
	end
       subroutine get_burst_sim_hist(
     *  kburst,plen,np_proc,fs,file_spc_raw,
     *  ns,buff0,istatus,buff1,dk_burst,file_raw,
     *  hist_i,hist_q,sim_eam_nsp)
	IMPLICIT REAL*8 (A-H,O-Z)
C*****WAYNE******
       include 'ssp2_const.inc'
       integer     kburst
       integer     np_proc
       real*8     fs
       character*60     file_spc_raw
       byte     buff0(2*sam_raw,line_raw)
       integer*4     istatus
       integer*4     ns(burst_max)
       complex     buff1(sam_raw,line_raw)
       real*8      plen
       integer*4     dk_burst(burst_max)
       character*60     file_raw
       integer hist_i(256),hist_q(256)
       integer     sim_eam_nsp(4)
C*****WAYNE******
	integer nppb
	parameter (nppb = 65)
	integer bytes,dk_ptr0,dk_ptr
	real*8 auxiliary(32)
	integer nsp(4)
c	include 'key_const.inc'
c	include 'key_pp.inc'

	nbeam = 4
	icyc_skip = (kburst-1)/nbeam
	nb = kburst- icyc_skip*nbeam
c	write(6,*)'icycle,nbeam=', icyc_skip+1,nb
	nsp(1) =sim_eam_nsp(1)
	nsp(2) =sim_eam_nsp(2)
	nsp(3) =sim_eam_nsp(3)
	nsp(4) =sim_eam_nsp(4)
	ns4 = nsp(1)+nsp(2)+nsp(3)+nsp(4)

	dk_ptr0 = icyc_skip*ns4*nppb*2	

	do k =1, nb-1
	dk_ptr0 = dk_ptr0+nsp(k)*nppb*2
	end do 

	bytes = 2*(nsp(nb)-plen*fs)

	do j = 1, np_proc
c	dk_ptr = dk_ptr0+int(plen*fs)*2+(j+9-1)*nsp(nb)*2
 	dk_ptr = dk_burst(kburst)+int(plen*fs)*2+(j+9-1)*nsp(nb)*2
c       call read_disk(file_spc_raw,buff0(1,j),bytes,dk_ptr,istatus)
        call read_disk(file_raw,buff0(1,j),bytes,dk_ptr,istatus)
        if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
	end do

        call decode_sim(kburst,plen,
     *  np_proc,ns,fs,buff0,
     *  buff1        )
        
        do i=1,np_proc
        do j=1,(ns(kburst)-plen*fs)
        iv1=int(real(buff1(j,i)))+129
        iv2=int(aimag(buff1(j,i)))+129
        if(iv1.gt.256.or.iv2.gt.256.or.iv1.le.0.or.iv2.le.0) then
          write (*,*) '***** Boundary error in raw data histogram****'
          return
        endif
        hist_i(iv1)=hist_i(iv1)+1
        hist_q(iv2)=hist_q(iv2)+1
        end do
        end do



	return
	end
       subroutine decode_sim(kburst,plen,
     *  np_proc,ns,fs,buff0,
     *  buff1        )
C*****WAYNE******
       include 'ssp2_const.inc'
       integer     np_proc
       integer*4     ns(burst_max)
       real*8     fs
       integer  kburst
       byte     buff0(2*sam_raw,line_raw)
       complex     buff1(sam_raw,line_raw)
       real*8    plen
C*****WAYNE******
c	include 'key_const.inc'
c	include 'key_pp.inc'
	integer btoi

	do i = 1, np_proc
	do j = 1, ns(kburst)-plen*fs
	iv1 = btoi(buff0(j*2-1,i))-128
	iv2 = btoi(buff0(j*2,i))-128
	v1 = iv1
	v2 = iv2
	buff1(j,i) = cmplx(v1,v2)
	end do	
	end do	

	do i = 1, np_proc
	do j = ns(kburst)-plen*fs+1, sam_raw
	buff1(j,i) = cmplx(0.,0.)
	end do	
	end do	


	return

	end
      	subroutine get_burst(
     *  cur_burst,plen,ire_sim,gain_rec,ns,
     *  np,dk_burst,file_spc_raw,data_cond,
     *  np_proc,fs,tbl,blk_gain,snr,
     *  buff0,buff1,istatus,file_raw,nr,np_v,
     *  aux_wdp_chg,aux_wdp_np,aux_agc_np,aux_wdp_cnt,aux_agc_chg,aux_agc_cnt)

	implicit none

c	include 'key_const.inc'
c	include 'key_pp.inc'
C*****WAYNE******
       include 'ssp2_const.inc'
       include 'ssp2_dim.inc'
       integer*4       np_v(burst_max)
       real*8      plen
       integer     cur_burst
       integer     ire_sim
       real*8     gain_rec(burst_max)
       integer*4     ns(burst_max)
       integer*4     np(burst_max)
       integer*4     dk_burst(burst_max)
       character*60     file_spc_raw
       integer     data_cond
       integer     np_proc
       real*8     fs
       complex     tbl(-128:127)
       byte     buff0(2*sam_raw,line_raw)
       complex     buff1(sam_raw,line_raw)
       integer*4     istatus
       integer*4     blk_gain(iq_blk,burst_max)
       real*8     snr(burst_max)
       character*60  file_raw
       integer*4   nr
C*****WAYNE******
c	--------------
c	INPUT PARAMETERS
c	--------------
c      	integer*4 	cur_burst	!current burst number

c	--------------
c	LOCAL VARIABLES
c	--------------
        integer samples,pulses,i,j,m,n,jstart,istart,jend,inp,jj
        integer bytes,dk_ptr,n_2048,offset
	real*4	gain,ns_cycle,pw_2,wdp_dt
        integer*4 ns_buff_len
        integer wdp_nps(128)
        real*8        aux_wdp_chg(max_agc,1600)
        integer*4     aux_wdp_np(max_agc,1600)
        integer*4     aux_agc_np(max_agc,1600)
        integer*4     aux_wdp_cnt(1600)
        real*8        aux_agc_chg(max_agc,1600)
        integer*4     aux_agc_cnt(1600)


	
c	--------------
c	4I/4Q DATA
c	--------------
c	write(6,*)' decode SAR, cur burst:',cur_burst
c	write(6,*)'burst loc ',cur_burst,dk_ptr
c	write(6,*)'key_pp ns np',ns(cur_burst),np(cur_burst)
 	write(6,*)'DK_BURST KBURST',dk_burst(cur_burst),cur_burst
c       gain=1.0
 	gain=gain_rec(cur_burst)

        
        if (aux_wdp_cnt(cur_burst) .le. 1 ) then 
    
        istart = np(cur_burst)-np_v(cur_burst) + 1

c          write(6,*)'CLUTTER',np(cur_burst),np_v(cur_burst),istart
        do i=istart,np(cur_burst)
           dk_ptr=dk_burst(cur_burst)+ns(cur_burst)*(i-1)
           call read_disk(file_raw,buff0(1,i-istart+1),ns(cur_burst),dk_ptr,istatus)
c          write(6,*)'CLUTTER',i,dk_ptr,ns(cur_burst),cur_burst
c          write(6,*)'CLUTTER',istatus
           if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
        enddo

        endif

        if (aux_wdp_cnt(cur_burst) .gt. 1 ) then 

          do i=1,np(cur_burst)
             wdp_nps(i)=0
          enddo
          aux_wdp_np(aux_wdp_cnt(cur_burst)+1,cur_burst)=np(cur_burst) + aux_wdp_np(1,cur_burst)
          do inp=2,aux_wdp_cnt(cur_burst)
              wdp_dt=aux_wdp_chg(inp,cur_burst)-aux_wdp_chg(1,cur_burst)
              do i= aux_wdp_np(inp,cur_burst)-aux_wdp_np(1,cur_burst)+1,
     *              aux_wdp_np(inp+1,cur_burst)-aux_wdp_np(1,cur_burst)
                 wdp_nps(i)=nint(wdp_dt*fs)
              enddo
          enddo

c         Read and align the data

          i=0
          j=0
          do inp=np(cur_burst)-np_v(cur_burst)+1,np(cur_burst)  !raw data address start at 0
           j=j+1
           i=inp-(np(cur_burst)-np_v(cur_burst))

           if (wdp_nps(i) .lt. 0) then
                do m=ns(cur_burst),ns(cur_burst)-abs(wdp_nps(i))+1, -1
                   buff0(m,j)=0
                enddo
                bytes=ns(cur_burst)-abs(wdp_nps(i))
                dk_ptr=dk_burst(cur_burst)+ns(cur_burst)*(inp-1)+abs(wdp_nps(i))
                call read_disk(file_raw,buff0(1,j),bytes,dk_ptr,istatus)
                if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
           else if (wdp_nps(i) .gt. 0) then
                offset=wdp_nps(i)
                do m=1, offset
                  buff0(m,j)=0
                enddo
                bytes=ns(cur_burst)-offset
                dk_ptr=dk_burst(cur_burst)+ns(cur_burst)*(inp-1)
                call read_disk(file_raw,buff0(offset+1,j),bytes,dk_ptr,istatus)
                if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
           else if (wdp_nps(i) .eq. 0) then
                bytes=ns(cur_burst)
                dk_ptr=dk_burst(cur_burst)+ns(cur_burst)*(inp-1)
                call read_disk(file_raw,buff0(1,j),bytes,dk_ptr,istatus)
                if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
           endif


          enddo

        endif

          call decode_4iq(cur_burst,buff0,np_v(cur_burst),ns(cur_burst),gain,tbl,buff1,
     *  aux_agc_cnt,aux_agc_np,aux_agc_chg)


           do j=1,np_v(cur_burst)
           do i=ns(cur_burst)+1,sam_raw
              buff1(i,j)=cmplx(0.,0.)
           enddo
           enddo




           






        if(data_cond.eq.1) then
c	   write(6,*)'Data Conditioning ON !'
           call iq_conditioning(cur_burst,
     *  ns,np_proc,blk_gain,snr,buff1)
        end if

c	write(6,*)' ns pw_2:',samples,pw_2
c	write(6,*)' rcv gain:',gain,gain_rec(cur_burst)

	return
	end
      	subroutine get_burst_hist(
     *  cur_burst,plen,ire_sim,gain_rec,ns,
     *  np,dk_burst,file_spc_raw,data_cond,
     *  np_proc,fs,tbl,blk_gain,snr,
     *  buff0,buff1,istatus,file_raw,nr,
     *  prod_type,maxgain,hist_i,hist_q,np_v,
     *  aux_wdp_chg,aux_wdp_np,aux_agc_np,aux_wdp_cnt,aux_agc_chg,aux_agc_cnt)

	implicit none

c	include 'key_const.inc'
c	include 'key_pp.inc'
C*****WAYNE******
       include 'ssp2_const.inc'
       include 'ssp2_dim.inc'
       real*8      plen
       integer     cur_burst
       integer     ire_sim
       real*8     gain_rec(burst_max)
       integer*4     ns(burst_max)
       integer*4     np(burst_max)
       integer*4     dk_burst(burst_max)
       character*60     file_spc_raw
       integer     data_cond
       integer     np_proc
       real*8     fs
       complex     tbl(-128:127)
       byte     buff0(2*sam_raw,line_raw)
       complex     buff1(sam_raw,line_raw)
       integer*4     istatus
       integer*4     blk_gain(iq_blk,burst_max)
       real*8     snr(burst_max)
       character*60  file_raw
       integer*4   nr
       integer    prod_type
       real*4     maxgain
       integer hist_i(256),hist_q(256)
       integer*4       np_v(burst_max)
        real*8        aux_wdp_chg(max_agc,1600)
        integer*4     aux_wdp_np(max_agc,1600)
        integer*4     aux_agc_np(max_agc,1600)
        integer*4     aux_wdp_cnt(1600)
        real*8        aux_agc_chg(max_agc,1600)
        integer*4     aux_agc_cnt(1600)
C*****WAYNE******
c	--------------
c	INPUT PARAMETERS
c	--------------
c      	integer*4 	cur_burst	!current burst number

c	--------------
c	LOCAL VARIABLES
c	--------------
        integer samples,pulses,i,j,m,n,jstart,istart,jend,inp,jj
        integer bytes,dk_ptr,n_2048,offset
        real*4  gain,ns_cycle,pw_2,wdp_dt
        integer*4 ns_buff_len
        integer wdp_nps(128)


	
c	--------------
c	4I/4Q DATA
c	--------------
D	write(6,*)' decode SAR, cur burst:',cur_burst
D	write(6,*)'burst loc ',cur_burst,dk_burst(cur_burst)
D	write(6,*)'key_pp ns np',ns(cur_burst),np(cur_burst)
D	write(6,*)'file_raw,np_v ',file_raw,np_v(cur_burst)
c       gain=1.0
        gain=gain_rec(cur_burst)

        if (aux_wdp_cnt(cur_burst) .le. 1 ) then

        istart = np(cur_burst)-np_v(cur_burst) + 1

        do i=istart,np(cur_burst)
           dk_ptr=dk_burst(cur_burst)+ns(cur_burst)*(i-1)
           call read_disk(file_raw,buff0(1,i-istart+1),ns(cur_burst),dk_ptr,istatus)
           if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
        enddo

        endif

        if (aux_wdp_cnt(cur_burst) .gt. 1 ) then

          do i=1,np(cur_burst)
             wdp_nps(i)=0
          enddo
          aux_wdp_np(aux_wdp_cnt(cur_burst)+1,cur_burst)=np(cur_burst) + aux_wdp_np(1,cur_burst)
          do inp=2,aux_wdp_cnt(cur_burst)
              wdp_dt=aux_wdp_chg(inp,cur_burst)-aux_wdp_chg(1,cur_burst)
              do i= aux_wdp_np(inp,cur_burst)-aux_wdp_np(1,cur_burst)+1,
     *              aux_wdp_np(inp+1,cur_burst)-aux_wdp_np(1,cur_burst)
                 wdp_nps(i)=nint(wdp_dt*fs)
              enddo
          enddo
c         Read and align the data

          i=0
          j=0
          do inp=np(cur_burst)-np_v(cur_burst)+1,np(cur_burst)  !raw data address start at 0
           j=j+1
           i=inp-(np(cur_burst)-np_v(cur_burst))

           if (wdp_nps(i) .lt. 0) then
                do m=ns(cur_burst),ns(cur_burst)-abs(wdp_nps(i))+1, -1
                   buff0(m,j)=0
                enddo
                bytes=ns(cur_burst)-abs(wdp_nps(i))
                dk_ptr=dk_burst(cur_burst)+ns(cur_burst)*(inp-1)+abs(wdp_nps(i))
                call read_disk(file_raw,buff0(1,j),bytes,dk_ptr,istatus)
                if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
           else if (wdp_nps(i) .gt. 0) then
                offset=wdp_nps(i)
                do m=1, offset
                  buff0(m,j)=0
                enddo
                bytes=ns(cur_burst)-offset
                dk_ptr=dk_burst(cur_burst)+ns(cur_burst)*(inp-1)
                call read_disk(file_raw,buff0(offset+1,j),bytes,dk_ptr,istatus)
                if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
           else if (wdp_nps(i) .eq. 0) then
                bytes=ns(cur_burst)
                dk_ptr=dk_burst(cur_burst)+ns(cur_burst)*(inp-1)
                call read_disk(file_raw,buff0(1,j),bytes,dk_ptr,istatus)
                if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
           endif

          enddo
         endif



          
c       if (prod_type .eq. prod_1 ) then
          call decode_hist(cur_burst,buff0,np_v(cur_burst),ns(cur_burst),gain,
     *                maxgain,aux_agc_cnt,aux_agc_np,aux_agc_chg,tbl,hist_i,hist_q)
c       endif

          call decode_4iq(cur_burst,buff0,np_v(cur_burst),ns(cur_burst),gain,
     *  tbl,buff1,
     *  aux_agc_cnt,aux_agc_np,aux_agc_chg)

        do j=1,np_v(cur_burst)
           do i=ns(cur_burst)+1,sam_raw
              buff1(i,j)=cmplx(0.,0.)
           enddo
        enddo


        if(data_cond.eq.1) then
c	   write(6,*)'Data Conditioning ON !'
           call iq_conditioning(cur_burst,
     *  ns,np_proc,blk_gain,snr,buff1)
        end if

c	write(6,*)' ns pw_2:',samples,pw_2
c	write(6,*)' rcv gain:',gain,gain_rec(cur_burst)

	return
	end
c******************************************************************

      	subroutine decode_4iq_repl(in,pulses,samples,gain,tbl,out)

	implicit none

c	--------------
c	INPUT PARAMETERS
c	--------------
        integer samples		!Samples per pulse
        byte 	in(2*10240,128)	!4I/4Q Raw data in 
        integer pulses		!number of pulses
        real*4  gain		!??? apply to whole burst ??Receiver gain
        complex tbl( -128:127 )	!Lookup table

c	--------------
c	OUTPUT PARAMETERS
c	--------------
        complex out(10240,128 )	!32I/32Q Raw out

c	--------------
c	LOCAL VARIABLES
c	--------------
	integer k, l, m, t,i

	do i = 1, pulses

c	   -------------
c	   FOR EACH PULSE
c	   -------------
	   do k = 1, samples
	      m = in( k,i )
	      out( k,i ) = tbl( m ) * gain	


c	      write'(''4I4/q data:'',i6,''='',z2.2 )',k,in(k,i) 
c	      write(6,*)' out:',k,out(k,i)
	   end do

	end do
        

	return
      end


c***************************************************************
c******************************************************************

      	subroutine decode_4iq(k_b,in,pulses,samples,gain,tbl,out,
     *  aux_agc_cnt,aux_agc_np,aux_agc_chg)

	implicit none

c	--------------
c	INPUT PARAMETERS
c	--------------
        include 'ssp2_dim.inc'
        integer*4     aux_agc_np(max_agc,1600)
        real*8        aux_agc_chg(max_agc,1600)
        integer*4     aux_agc_cnt(1600)
        integer k_b
        integer samples		!Samples per pulse
        byte 	in(2*10240,128)	!4I/4Q Raw data in 
        integer pulses		!number of pulses
        real*4  gain		!??? apply to whole burst ??Receiver gain
        complex tbl( -128:127 )	!Lookup table

c	--------------
c	OUTPUT PARAMETERS
c	--------------
        complex out(10240,128 )	!32I/32Q Raw out

c	--------------
c	LOCAL VARIABLES
c	--------------
	integer k, l, m, t,i,j,n,jstart,jend
        real*4  gain_c(128)

        if (aux_agc_cnt(k_b) .le. 1 ) then
           if (gain_flag .eq. 1 ) gain = 1.0
           if (gain_flag .eq. 3 ) gain = 1.0/gain
           if (gain_flag .eq. 9 ) gain = gain

           do i = 1, pulses
           do k = 1, samples
              m = in( k,i )
              out( k,i ) = tbl( m ) * gain
           end do
           end do


        else            !agc change

           n = aux_agc_cnt(k_b)

           aux_agc_np(n+1,k_b) = pulses + aux_agc_np(1,k_b)
           do i = 1, n
              jstart = aux_agc_np(i,k_b) - aux_agc_np(1,k_b) + 1
              jend = aux_agc_np(i+1,k_b) - aux_agc_np(1,k_b)

              do j = jstart, jend
                 gain_c(j) =sqrt(10**(aux_agc_chg(i,k_b)/10))
cc                 write(6,*)'DECODE Chg:',j,gain_c(j),aux_agc_chg(i,k_b),i
              enddo
           enddo


          if (gain_flag .eq. 1 ) then
              do i = 1, pulses
              do k = 1, samples
                 m = in( k,i )
                 out( k,i ) = tbl( m )
              end do
              end do
           else if (gain_flag .eq. 3 ) then
              do i = 1, pulses
              do k = 1, samples
                 m = in( k,i )
                 out( k,i ) = tbl( m ) / gain_c(i)
              end do
              end do
           else if (gain_flag .eq. 9 ) then
              do i = 1, pulses
              do k = 1, samples
                 m = in( k,i )
                 out( k,i ) = tbl( m ) * gain_c(i)
              end do
              end do
           endif



        endif



        

	return
      end


c***************************************************************
	subroutine iq_conditioning(kburst,
     *  ns,np_proc,blk_gain,snr,buff_in)

c	--------------	
c	ABSTRACT:
c	        1).Remove DC bias.
c	        2).Remove IQ imbalance
c	        3).Correct for IQ phase lag
c	--------------	

	implicit none
c	include 'key_const.inc'
c	include 'key_pp.inc'
C*****WAYNE******
       include 'ssp2_const.inc'
       integer*4     ns(burst_max)
       integer     np_proc
       integer*4     blk_gain(iq_blk,burst_max)
       real*8     snr(burst_max)
       complex     buff_in(sam_raw,line_raw)
C*****WAYNE******
c	--------------
c	PASSING IN PARAMETRERS
c	--------------
	integer kburst		!Burst index

c	--------------
c	BUFFER IN/OUT
c	--------------
c	complex buff_in(sam_raw,line_raw)
c	equivalence (buff_in,buff1)

c	--------------
c	LOCAL VARIABLES
c	--------------
	integer	k,i,j
	integer	n_sub_blk,is_addr,npulse,isample
	real	mean_i,mean_q,std_i,std_q
	real	sum_i,sum2_i,sum_q,sum2_q,value_i,value_q
	real	q_ratio,zho,value2_i,value2_q
	real	value_iq,phi
	real	c_snr,gain_min,gain_max,scale

	n_sub_blk=ns(kburst)/iq_sub_len
	if(n_sub_blk*iq_sub_len.lt.ns(kburst)) n_sub_blk = n_sub_blk+1

c	------------
c	DO-LOOP for SUB-BLOCK within ONE AZIMUTH LINE
c	------------

	npulse = np_proc

	gain_min = 1.e10
	gain_max = 0.
	do k = 1, n_sub_blk
	if(blk_gain(k,kburst).gt.gain_max) gain_max = blk_gain(k,kburst)
	if(blk_gain(k,kburst).lt.gain_min) gain_min = blk_gain(k,kburst)
	end do
	c_snr = .65*(gain_max/gain_min)

	do k = 1, n_sub_blk

           sum_i=0.
	   sum2_i=0.	
           sum_q=0.
	   sum2_q=0.	
	   value_iq=0.	
	   isample = iq_sub_len
	   if(k*iq_sub_len.gt.ns(kburst)) isample=ns(kburst)-(k-1)*iq_sub_len

	   do i=1,isample
	   do j=1,npulse

	      is_addr= i+(k-1)*iq_sub_len

	      value_i  = real(buff_in(is_addr,j))
	      sum_i    = sum_i + value_i 
	      sum2_i   = sum2_i + value_i*value_i 

	      value_q  = aimag(buff_in(is_addr,j))
	      sum_q    = sum_q + value_q 
	      sum2_q   = sum2_q + value_q*value_q 

	      value_iq = value_iq + value_i*value_q
	     
	   enddo
	   enddo

c	   ------------
c	   COMPUTE MEAN & STANDARD DEVIATION
c	   ------------
	   mean_i = sum_i / iq_sub_len
	   std_i  = sqrt(sum2_i / iq_sub_len - mean_i*mean_i)

	   mean_q = sum_q / iq_sub_len
	   std_q  = sqrt(sum2_q / iq_sub_len - mean_q*mean_q)

d	   write(6,*)'I mean / std:',mean_i,std_i
d	   write(6,*)'Q mean / std:',mean_q,std_q

c	   ------------
c	   COMPUTE IQ IMBALANCE
c	   ------------
	   if (std_i .ne. 0) then
	      q_ratio= std_q/std_i
	   else 
	      write(6,*)' I std is zero (from data conditioning)!'
	      return
	   endif

c	   ------------
c	   COMPUTE IQ PHASE LAG
c	   ------------
	   zho=(value_iq-mean_i*mean_q)/(std_i*std_q)
	   scale = blk_gain(k,kburst)/gain_min/c_snr
	   phi= asind(-zho*(1+1/(snr(kburst)*scale)))
	   
c	   ------------
c	   DO-LOOP for SAMPLES in A SUB-BLOCK 
c	   ------------
	   do i=1,iq_sub_len
	   do j=1,npulse

	    is_addr= i+ (k-1)*iq_sub_len
            value_i  = real(buff_in(is_addr,j))- mean_i
            value_q  = (aimag(buff_in(is_addr,j))-mean_q)/q_ratio
            value_q  = (value_q+value_i*sin(phi))/cos(phi)

	    buff_in(is_addr,j)=cmplx(value_i,value_q)

	   enddo
	   enddo

	enddo

	
	return
	end
c*******************************************************************
	SUBROUTINE GTTARG1(RST0,FD0,XS,YS,ZS,
     1                  VXS,VYS,VZS,H,
     1                  PX,PY,PZ,XT,YT,ZT,VXT,VYT,VZT,ALKANG,yaw,
     1                  dem_ellip,dlon_topo,dlat_topo,topo)
     
	IMPLICIT REAL*8 (A-H,O-Z)
c	include 'key_const.inc'
C*****WAYNE******
       include 'ssp2_const.inc'
       integer     dem_ellip
       real*8     dlon_topo
       real*8     dlat_topo
       integer*2     topo(ns_ew,ns_ns)
C*****WAYNE******
	real*8 lat,lat_d,lon

	ALKANG = 30.d0
	yaw = 0.d0
c	h = 0.d0
        n_iter=0
1111    continue
	CALL POINTING(XS,YS,ZS,VXS,VYS,VZS,ALKANG,yaw,PX,PY,PZ)
	CALL SEETARG(h,XS,YS,ZS,PX,PY,PZ,XT,YT,ZT)
	call ebf_to_ll(xt,yt,zt,lat,lat_d,lon)
	call get_tv(lat,lon,lat_d,h,xt,yt,zt,vxt,vyt,vzt)
        n_iter = n_iter + 1
        if (n_iter.gt.n_iteration) then         !2
/17/95
           return
        else
c          if(h-h_update.gt.1.e-1) then
c          h = h_update
c          GO TO 1111
c          end if
	CALL RNGDOP(XS,YS,ZS,VXS,VYS,VZS,XT,YT,ZT,RST,FD)
	CALL INC_ANG(ALKANG,XS,YS,ZS,XT,YT,ZT,THI_INC)
	VST = sqrt((VXS-VXT)**2+(VYS-VYT)**2+(VZS-VZT)**2)
	ALKANG = ALKANG + atand((RST0-RST)/(TAND(THI_INC)*RST))
	yaw = yaw - asind(lambda*(FD0-FD)/(2.d0*VST))
        IF(ABS(FD-FD0).GT.0.1.OR.ABS(RST-RST0).GT.0.1) THEN
c            h = h_update
             GO TO 1111
           endif
        endif



	RETURN
	END

        SUBROUTINE POINTING(FXS,FYS,FZS,FVXS,FVYS,FVZS,ALK,YAW,PX,PY,PZ)
        IMPLICIT REAL*8 (A-H,O-Z)

        CALL CROSS(FVXS,FVYS,FVZS,FXS,FYS,FZS,C1,C2,C3)   !right looking
        call cross(fxs,fys,fzs,c1,c2,c3,a1,a2,a3)
        R=(FXS**2+FYS**2+FZS**2)**.5d0
        V1=-1*FXS/R
        V2=-1*FYS/R
        V3=-1*FZS/R
        PGX=C1*SIND(ALK)
        PGY=C2*SIND(ALK)
        PGZ=C3*SIND(ALK)
        PX=V1*COSD(ALK)+PGX*COSD(YAW)-A1*SIND(YAW)*SIND(ALK)
        PY=V2*COSD(ALK)+PGY*COSD(YAW)-A2*SIND(YAW)*SIND(ALK)
        PZ=V3*COSD(ALK)+PGZ*COSD(YAW)-A3*SIND(YAW)*SIND(ALK)
        PP = SQRT(PX**2+PY**2+PZ**2)
        PX = PX/PP
        PY = PY/PP
        PZ = PZ/PP


        RETURN
        END



	SUBROUTINE RNGDOP(XS,YS,ZS,VXS,VYS,VZS,XT,YT,ZT,
     1			RST,FD)
	IMPLICIT REAL*8 (A-H,O-Z)
c	include 'key_const.inc'
C*****WAYNE******
       include 'ssp2_const.inc'
C*****WAYNE******

	RST = sqrt((XS-XT)**2+(YS-YT)**2+(ZS-ZT)**2)
	RT = SQRT(XT**2+YT**2)
	VT = omega_e * RT

	CALL CROSS(0.D0,0.D0,1.D0,XT,YT,ZT,U1,U2,U3)
	VXT = VT * U1
	VYT = VT * U2
	VZT = VT * U3

	DRX = XT - XS
	DRY = YT - YS
	DRZ = ZT - ZS

	DVX = VXT - VXS
	DVY = VYT - VYS
	DVZ = VZT - VZS

	VDOTR= DVX*DRX +DVY*DRY +DVZ*DRZ
	REL_R = SQRT ( DRX**2 + DRY**2 + DRZ**2 )

	FD = -2.d0 * VDOTR /(lambda * REL_R)

	RETURN
	END

c****************************************************************
	SUBROUTINE INC_ANG(ALKANG,XS,YS,ZS,XT,YT,ZT,THI_INC)
	IMPLICIT REAL*8 (A-H,O-Z)
	REAL*8 LAT,LAT_D
c	include 'key_const.inc'
C*****WAYNE******
       include 'ssp2_const.inc'
C*****WAYNE******
	
	RS = SQRT(XS**2+YS**2+ZS**2)
	RT = SQRT(XT**2+YT**2+ZT**2)
	THI_INC = ASIND((RS/RT)*SIND(ALKANG))
	LAT = ATAND(ZT/SQRT(XT**2+YT**2))
        LAT_D = ATAND(TAND(LAT)/(1-ecc_e**2))   !AC
c mar23 LAT_D = ATAND((1-ecc_e**2)*TAND(LAT))
	THI_INC = THI_INC -(LAT_D-LAT)

	RETURN
	END
	SUBROUTINE SEETARG(h,XS,YS,ZS,PX,PY,PZ,XT,YT,ZT)
	IMPLICIT REAL*8 (A-H,O-Z)
C	include 'key_const.inc'
C*****WAYNE******
       include 'ssp2_const.inc'
C*****WAYNE******
	real*8 lat, lat_d
	
ccc     lat = atan2d(sqrt(xs**2+ys**2),zs)      !AC
        lat = atan2d(zs,sqrt(xs**2+ys**2))
	lat_d = atand(tand(lat)/(1-ecc_e**2))
	R = 1.d0/sqrt(cosd(lat)**2/RE**2+sind(lat)**2/RP**2)
	ratio = (R+h/cosd(lat-lat_d))/R
	RE1 = RE*ratio
	RP1 = RP*ratio
	A = (px**2+py**2)/RE1**2+pz**2/RP1**2
	B = (2.d0*px*XS+2.d0*py*YS)/RE1**2+2.d0*pz*ZS/RP1**2
	CC = (XS**2+YS**2)/RE1**2+ZS**2/RP1**2-1
	con =B**2-4.d0*A*CC
	if(con.lt.0) write(6,*) 'cannot see a target'
	RST=(-B-sqrt(con))/(2.d0*A)
	XT=XS+PX*RST
	YT=YS+PY*RST
	ZT=ZS+PZ*RST
	
	RETURN
	END


	subroutine get_tv(lat,lon,lat_d,h,
     1			xt,yt,zt,vxt,vyt,vzt)
	implicit real*8 (a-h,o-z)
c	include 'key_const.inc'
C*****WAYNE******
       include 'ssp2_const.inc'
C*****WAYNE******
	real*8 lat,lon,lat_d,h

	Rn = Re/sqrt(1-(ecc_e*sind(lat_d))**2)
	xt = (Rn+h)*cosd(lon)*cosd(lat_d)
	yt = (Rn+h)*sind(lon)*cosd(lat_d)
	zt = (Rn*(1-ecc_e**2)+h)*sind(lat_d)
	rxyt = sqrt(xt**2+yt**2)
	call cross(0.d0,0.d0,1.d0,xt,yt,zt,u1,u2,u3)
	vxt = omega_e*rxyt*u1
	vyt = omega_e*rxyt*u2
	vzt = omega_e*rxyt*u3
	
	return
	end
        subroutine burst_pp_0(dfddth,pre_phase,thi,tp,coef_fd_0,
     *                        kburst,v_sc_mag,v_x_sc,v_y_sc,v_z_sc,
     *                        rng_bm_ang,beam_no,az_pk_coef,az_peak_coef,
     *                        beam_id,x_sc,y_sc,z_sc,roll,yaw,pitch,
     *                        look_ang,ire_sim,dem_ellip,dlon_topo,dlat_topo,
     *                        topo,i_ang_bip,r_bip,fr_bip,fd_bip,coef_fd,
     *                        coef_fr,coef_lk_ang,nbeams,np,prf,np_air,
     *                        r_low,r_1st,fd_near,fr_near,r_high,ns,nr,dr,
     *                        fd_far,fr_far,r_cntr,fd_cntr,fr_cntr,
     *                        avg_terrain,coef_inc)
        include 'ssp2_const.inc'
       integer    kburst
       real*8     v_sc_mag(burst_max)
       real*8     v_x_sc(burst_max)
       real*8     v_y_sc(burst_max)
       real*8     v_z_sc(burst_max)
       real*8     dfddth
       real*8     rng_bm_ang(4)
       integer*4     beam_no(burst_max)
       integer    pre_phase
       real*8 az_pk_coef(3)
       real*8          az_peak_coef(3,8)
       integer*4     beam_id(burst_max)
       real*8     x_sc(burst_max)
       real*8     y_sc(burst_max)
       real*8     z_sc(burst_max)
       real*8     roll(burst_max)
       real*8     yaw(burst_max)
       real*8     pitch(burst_max)
       real*8     look_ang(burst_max)
       integer    ire_sim
       integer     dem_ellip
       real*8     dlon_topo
       real*8     dlat_topo
       integer*2     topo(ns_ew,ns_ns)
       real*8     i_ang_bip(burst_max)
       real*8     r_bip(burst_max)
       real*8     fr_bip(burst_max)
       real*8     fd_bip(burst_max)
       real*8    thi(5)
       real*8     coef_fd(4,burst_max)
       real*8     coef_fd_0(4,burst_max)
       real*8     coef_fr(4,burst_max)
       real*8     coef_lk_ang(4,burst_max)
       integer*4     nbeams
       real*8          tp 
       integer*4     np(burst_max)
       real*8     prf(burst_max)
       integer*4     np_air(burst_max)
       real*8     r_low(burst_max)
       real*8     r_1st(burst_max)
       real*8     fd_near(burst_max)
       real*8     fr_near(burst_max)
       real*8     r_high(burst_max)
       integer*4     ns(burst_max)
       integer*4    nr
       real*8     dr(burst_max)
       real*8     fd_far(burst_max)
       real*8     fr_far(burst_max)
       real*8     r_cntr(burst_max)
       real*8     fd_cntr(burst_max)
       real*8     fr_cntr(burst_max)
       real*8     coef_inc(4,burst_max)
C******************************************************************
       real*8  coef_r_fd(4),coef_r_fr(4),coef_r_ang(4)
       real*8  coef_r_inc(4)
       real*8 r_t(5),fd_t(5),fr_t(5)
       integer mod_k
       real*8  rng_bm
       real*8  alkang_near
       real*8  ang_hh
       real*8  avg_terrain

        m = 4
        v_sc_mag(kburst)=sqrt(v_x_sc(kburst)**2+v_y_sc(kburst)**2+
     1                          v_z_sc(kburst)**2)
c       dfddth = 2*v_sc_mag(1)/lambda *pi/180.
        dfddth = 2*sqrt(v_x_sc(1)**2+v_y_sc(1)**2+
     1                          v_z_sc(1)**2)/lambda*pi/180.
	rng_bm = rng_bm_ang(beam_no(kburst))

        if(pre_phase.eq.1) then
         az_pk_coef(1) = 0.
         az_pk_coef(2) = 0.
         az_pk_coef(3) = 0.
        else
         az_pk_coef(1) = az_peak_coef(1,beam_id(kburst))         !AC
         az_pk_coef(2) = az_peak_coef(2,beam_id(kburst))
         az_pk_coef(3) = az_peak_coef(3,beam_id(kburst))
        endif
D	write(6,*) 'x ',x_sc(kburst),y_sc(kburst),z_sc(kburst)
D	write(6,*) 'v ',v_x_sc(kburst),v_y_sc(kburst),v_z_sc(kburst)
D	write(6,*) 'az_pk_coef ',az_pk_coef(1),az_pk_coef(2),az_pk_coef(3)
D	write(6,*) 'rng_bm,lookang',rng_bm,look_ang(kburst)
D       write(6,*)'B ryp',roll(kburst),yaw(kburst),pitch(kburst)
C
	call prime_coefs(x_sc(kburst),y_sc(kburst),z_sc(kburst),
     c	v_x_sc(kburst),v_y_sc(kburst),v_z_sc(kburst),roll(kburst),
     c  yaw(kburst),pitch(kburst),az_pk_coef,
     c	r_t,fd_t,fr_t,thi,coef_r_fd,coef_r_fr,coef_r_ang,
     c	coef_r_inc,rng_bm,look_ang(kburst),ire_sim,
     *  dem_ellip,dlon_topo,dlat_topo,topo,avg_terrain)

	i_ang_bip(kburst) = thi(3)
	r_bip(kburst) = r_t(3)
	fd_bip(kburst) = fd_t(3)
	fr_bip(kburst) = fr_t(3)
	do i = 1, 4
	coef_fd(i,kburst) = coef_r_fd(i)
        if(pre_phase.eq.1) coef_fd_0(i,kburst) = coef_r_fd(i)
	coef_fr(i,kburst) = coef_r_fr(i)
	coef_lk_ang(i,kburst) = coef_r_ang(i)
        coef_inc(i,kburst) = coef_r_inc(i)
	end do

D	write(6,*) 'r_bip, fd_bip', r_bip(kburst),fd_bip(kburst)
D	write(6,*) 'coef_fd(1,kburst), coef_r_fd(1)',coef_fd(1,kburst),coef_r_fd(1)
c12. Get the delay time of the first range sample by FDD 4.4.1.10.1. Eq.

C 	Tp = np(1)/prf(1)+np(2)/prf(2)+np(3)/prf(3)+np(4)/prf(4)

        mod_k=mod(kburst,nbeams)
        if (mod_k .eq. 1) then
           Tp = 0.     
           do j = kburst, kburst+nbeams-1
              Tp = np(j)/prf(j)+Tp
           end do
        endif

	np_air(kburst) = 2*r_bip(kburst)/cspeed*prf(kburst)

D	write(6,*) 'Tp:',Tp
D	write(6,*) 'r_t(1),np_air', r_t(1), np_air(kburst)
c13. Get the range of the first sample according to 4.4.1.10.2. Eq.

	rw = abs(fd_bip(kburst)*(np(kburst)-np_air(kburst))
     *          /prf(kburst)*lambda/2.0d0)                          !AC
ccc     1               /prf(kburst)*lambda/2.0d0)/dr(kburst)
    	r_low(kburst) = r_1st(kburst) + rw/2.d0
	call v_poly(r_low(kburst),r_bip(kburst),coef_r_fd,m,fd_near(kburst))
	call v_poly(r_low(kburst),r_bip(kburst),coef_r_fr,m,fr_near(kburst))
        call v_poly(r_low(kburst),r_bip(kburst),coef_r_ang,m,alkang_near)
D	write(6,*) 'rw,low,fd,fr',rw,r_low(kburst),fd_near(kburst),fr_near(kburst)
c14. Get the range of the last sample according to 4.4.1.10.3. Eq.

   	r_high(kburst) = r_low(kburst)+(ns(kburst)-nr+1)*dr(kburst)-rw
	call v_poly(r_high(kburst),r_bip(kburst),coef_r_fd,m,fd_far(kburst))
	call v_poly(r_high(kburst),r_bip(kburst),coef_r_fr,m,fr_far(kburst))
        call v_poly(r_high(kburst),r_bip(kburst),coef_r_ang,m,ang_hh)

D       write(6,*)'IMP',r_low(kburst),ns(kburst)
D       write(6,*)'IMP',nr,dr(kburst),rw
D	write(6,*) 'r_high,fd,fr',r_high(kburst),fd_far(kburst),fr_far(kburst)
  
 	r_cntr(kburst) = (r_low(kburst)+r_high(kburst))/2.d0
	call v_poly(r_cntr(kburst),r_bip(kburst),coef_r_fd,m,fd_cntr(kburst))
	call v_poly(r_cntr(kburst),r_bip(kburst),coef_r_fr,m,fr_cntr(kburst))
D	write(6,*) 'r_cntr,fd,fr',r_cntr(kburst),fd_cntr(kburst),fr_cntr(kburst)
        return
        end

	subroutine burst_pp(
     *  kburst,burst_cnt,roll_o,roll,nr,
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
	implicit real*8 (a-h, o-z)
c	include 'key_const.inc'
c	include 'key_pp.inc'
C*****WAYNE******
       include 'ssp2_const.inc'
       real*8    avg_terrain
       real*8    coef_inc(4,burst_max)
       real*8          az_peak_coef(3,8)
       real*8 az_pk_coef(3)
       integer     kburst
       integer*4     burst_cnt(burst_max)
       integer*4    nr
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
       integer     proc_mode
       integer*4     beam_no(burst_max)
       real*8     fm_sizec2_in
       integer*4     nfft_az
       integer     dem_ellip
       real*8     c1_pxl
       real*8     c2_pxl
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
       real*8     dfddth
       integer    pre_phase
       real*8     coef_fd_0(4,burst_max)
C*****WAYNE******
       real*8     dummy_c1,dummy_c2
	real*8 coef_r_fd(4),coef_r_fr(4),coef_r_ang(4)
	real*8 r_t(5),fd_t(5),fr_t(5),thi(5)
	real*8 r_corner(2,2),fd_corner(2,2),lat_corner(2,2),lon_corner(2,2)
	real*8 c1_g_min(4,50),c2_g_min(4,50)
	real*8 r_g_t(3,3),fd_g_t(3,3),c1_g_t(3,3),c2_g_t(3,3)
	real*8 coef_c1(3,3),coef_c2(3,3),dis,fd,lat,lon,lat_d
	real gain_cmp(600,64)
        integer mod_k
        real*8  tp
        real*8    p_x(burst_max),p_y(burst_max),p_z(burst_max) 
        real*8    rst0,fd0,xt,yt,zt,vxt,vyt,vzt,alkang,yaw1
        real*8    c1_span,c1_min_near,c2_min_near
        real*8    c1_min,c1_max,pbw_slope,x_g,y_g,z_g
        real*8    h,fd_cnt,pbw_curr,chd_fd,chd_c1
        real*8    rlocal_mean

D       write(6,*)'BURST_PP BURST # ',kburst
D	write(6,*) 'kburst, burst_cnt', kburst, burst_cnt(kburst)

c15. Get the near range Doppler bounds according to 4.4.1.11 Eq. 
C***************************************************************************
C***************************************************************************
        rlocal_mean = sqrt((x_sc(1)**2+y_sc(1)**2+z_sc(1)**2)/
     *               ((x_sc(1)**2+y_sc(1)**2)/re**2+(z_sc(1)**2)/rp**2))


        call burst_pp_0(dfddth,pre_phase,thi,tp,coef_fd_0,
     *                        kburst,v_sc_mag,v_x_sc,v_y_sc,v_z_sc,
     *                        rng_bm_ang,beam_no,az_pk_coef,az_peak_coef,
     *                        beam_id,x_sc,y_sc,z_sc,roll,yaw,pitch,
     *                        look_ang,ire_sim,dem_ellip,dlon_topo,dlat_topo,
     *                        topo,i_ang_bip,r_bip,fr_bip,fd_bip,coef_fd,
     *                        coef_fr,coef_lk_ang,nbeams,np,prf,np_air,
     *                        r_low,r_1st,fd_near,fr_near,r_high,ns,nr,dr,
     *                        fd_far,fr_far,r_cntr,fd_cntr,fr_cntr,
     *                        avg_terrain,coef_inc)
        m = 4

        pbw_near(kburst) = 1.35d0*max_look*Tp*abs(fr_near(kburst))
        if (ire_sim.eq.0) then
         pbw_near(kburst) = 0.7d0*max_look*Tp*abs(fr_near(kburst))
        endif
	fd_low_near(kburst)= fd_near(kburst) - pbw_near(kburst)/2
	fd_high_near(kburst) = fd_near(kburst) + pbw_near(kburst)/2 

c16. Get the far range Doppler bounds according to 4.4.1.12 Eq.

    	pbw_far(kburst) = 1.35d0*max_look*Tp*abs(fr_far(kburst))
        if (ire_sim.eq.0) then
         pbw_far(kburst) = 0.7d0*max_look*Tp*abs(fr_far(kburst))
        endif
	fd_low_far(kburst) = fd_far(kburst) - pbw_far(kburst)/2.d0
	fd_high_far(kburst) = fd_far(kburst) + pbw_far(kburst)/2.d0

c16.1 Get the center range Doppler bounds according to 4.4.1.12 Eq.

        pbw_cntr(kburst) = 1.35d0*max_look*Tp*abs(fr_cntr(kburst))
        if (ire_sim.eq.0) then
         pbw_cntr(kburst) = 0.7d0*max_look*Tp*abs(fr_cntr(kburst))
        endif
	fd_low_cntr(kburst) = fd_cntr(kburst) - pbw_cntr(kburst)/2.d0
	fd_high_cntr(kburst) = fd_cntr(kburst) + pbw_cntr(kburst)/2.d0

D       write(6,*)'pbw_n,pbw_c,pbw_f',
D    *		pbw_near(kburst),pbw_cntr(kburst),pbw_far(kburst)

c17. Based on 13~16, the range_Doppler of the four corner points of a 
c    framelet is available.

    	r_corner(1,1) = r_low(kburst)
    	r_corner(1,2) = r_low(kburst)
    	r_corner(2,1) = r_high(kburst)
    	r_corner(2,2) = r_high(kburst)
    	fd_corner(1,1) = fd_low_near(kburst)
    	fd_corner(1,2) = fd_high_near(kburst)
    	fd_corner(2,1) = fd_low_far(kburst)
    	fd_corner(2,2) = fd_high_far(kburst)

c18. Get the position of the famelet corner points based on its range-Doppler 
c    and the ellipsoidal surface model.
c18.1 Get the lat and lon of the Corner points
c19. Get the projection of the Corner points

        h= avg_terrain
	do i = 1,2
	do j = 1,2
	rst0 = r_corner(i,j)
	fd0 = fd_corner(i,j)
	call GTTARG1(RST0,FD0,x_sc(kburst),y_sc(kburst),z_sc(kburst),
     1			v_x_sc(kburst),v_y_sc(kburst),v_z_sc(kburst),
     2                  h,p_x(kburst),p_y(kburst),p_z(kburst),
     2 			XT,YT,ZT,VXT,VYT,VZT,ALKANG,yaw1,
     3                  dem_ellip,dlon_topo,dlat_topo,topo)
c	call ebf_to_ac(xt,yt,zt,re,c1_flm(j,i,kburst),c2_flm(j,i,kburst),
c    *  alpha1,alpha2,alpha3       )
        call ebf_to_ac(xt,yt,zt,rlocal_mean,dummy_c1,dummy_c2,
     *  alpha1,alpha2,alpha3       )
D       write(6,*)'alpha ',alpha1,alpha2,alpha3
        c1_flm(j,i,kburst)= dummy_c1
        c2_flm(j,i,kburst)= dummy_c2
D	write(6,*) 'rst, fd', rst0, fd0
D       write(6,*) 'xt,yt,zt', xt,yt,zt
D	write(6,*) 'c12_flm', c1_flm(j,i,kburst), c2_flm(j,i,kburst)
D	write(6,*) 'WAYNE1', x_sc(kburst),y_sc(kburst),z_sc(kburst)
D	write(6,*) 'WAYNE2', v_x_sc(kburst),v_y_sc(kburst),v_z_sc(kburst)
	end do
	end do

	if(beam_no(kburst).eq.nbeams) then
	if(abs(c2_flm(1,2,kburst)-c2_flm(1,1,kburst-(nbeams-1))).gt.
     1	fm_sizec2_in)
     1	c2_flm(1,2,kburst) = c2_flm(1,1,kburst-(nbeams-1))+fm_sizec2_in 
	end if
	c1_span = max(c1_flm(2,1,kburst)-c1_flm(1,1,kburst),
     1		      c1_flm(2,2,kburst)-c1_flm(1,2,kburst)) 
D	write(6,*) 'c1_span', c1_span

c20. Get the projection of the center point of each ground block

c    	Get the size of the ground block in along and cross tracks
c	(use the topo resolution to determine)

	c1_cell(kburst) = (prf(kburst)/nfft_az)/(dfddth*180./pi)*r_1st(kburst)
	c2_cell(kburst) = dr(kburst)/sind(thi(5))

        if(dem_ellip.eq.dem) then       !AC
        c1size_blk(kburst) = (int(c1_span/c1_cell(kburst))+2)/2.d0*
     1                          2.d0*c1_cell(kburst)
        c2size_blk(kburst) = (nint(6000.d0/c2_cell(kburst)))/2.d0*
     1                          2.d0*c2_cell(kburst)
        else
        c1size_blk(kburst) = (int(c1_span/c1_cell(kburst))+2)/2.d0*
     1                          2.d0*c1_cell(kburst)
        c2size_blk(kburst) = (nint(6000.d0/c2_cell(kburst)))/2.d0*
     1                          2.d0*c2_cell(kburst)
        end if


D	write(6,*) 'c12cell', c1_cell(kburst),c2_cell(kburst)
D	write(6,*) 'c12size', c1size_blk(kburst),c2size_blk(kburst)

c    Get the boundary values for a burst frame
c    Take the minimum and maximum of the corner points of the framelet.

	c1_min_near = min(c1_flm(1,1,kburst),c1_flm(1,2,kburst))
	c2_min_near = min(c2_flm(1,1,kburst),c2_flm(2,1,kburst))
        c1_min = min(c1_flm(1,1,kburst),c1_flm(1,2,kburst),
     1               c1_flm(2,1,kburst),c1_flm(2,2,kburst))
        c1_max = max(c1_flm(1,1,kburst),c1_flm(1,2,kburst),
     1               c1_flm(2,1,kburst),c1_flm(2,2,kburst))

D       write(6,*)'corner',c1_min_near,c2_min_near,c1_min,c1_max

c     Determine number of blocks in along- and cross-track 

     	n_c1_blk(kburst)=abs(c1_flm(1,1,kburst)-c1_flm(2,1,kburst))
     1				/c1size_blk(kburst) + 1

     	n_c2_blk(kburst)=abs(c2_flm(1,1,kburst)-c2_flm(1,2,kburst))
     1				/c2size_blk(kburst) + 1

c    	if(n_c2_blk(kburst)*c2size_blk(kburst).gt.174000.)
c    1	 n_c2_blk(kburst)=174000.d0/c2size_blk(kburst)
         if(n_c2_blk(kburst)*c2size_blk(kburst).gt.200000.)
     1   n_c2_blk(kburst)=200000./c2size_blk(kburst)


D	write(6,*) '# of blocks', n_c1_blk(kburst),n_c2_blk(kburst)
c     Compute the projection of the minmum points of ground blocks by
c     linear equation
c20.1. Get the projection of the ground block 9 points.
c21. Get the position of the ground block corner points.
c22. Get the range/Doopler of the ground block corner points.
c23. Get the resampling coefficients of the ground blocks.
c24. Get the start and end values of C1 and C2 for resampling

        pbw_slope = (pbw_far(kburst)-pbw_near(kburst))/
     *              (r_high(kburst)-r_low(kburst))
 
	do i = 1, n_c1_blk(kburst)
     	do j = 1, n_c2_blk(kburst)

        c1_g_min(i,j)=(c1_min+c1_max)/2.+(i-1)*c1size_blk(kburst)
        c2_g_min(i,j)=c2_min_near+(j-1)*c2size_blk(kburst)
        c2_g_min(i,j) = nint(c2_g_min(i,j)/c2_cell(kburst))*c2_cell(kburst)

     	do k1 = 1, 3
     	do k2 = 1, 3

     	c1_g(k1,k2,i,j,kburst) =c1_g_min(i,j) + (k1-1)* c1size_blk(kburst)/2.d0
     	c2_g(k1,k2,i,j,kburst) =c2_g_min(i,j) + (k2-1)* c2size_blk(kburst)/2.d0
	call ac_to_ebf(c1_g(k1,k2,i,j,kburst),c2_g(k1,k2,i,j,kburst),
     *   		x_g,y_g,z_g,lat,lat_d,lon,rlocal_mean,
     *  		alpha1,alpha2,alpha3)
        call elevation(lat,lon,h,avg_terrain,
     *  dem_ellip,dlon_topo,dlat_topo,topo )
	call get_tv(lat,lon,lat_d,h,x_g,y_g,z_g,vxt,vyt,vzt)

        r_g(k1,k2,i,j,kburst)=
     1	dis(x_sc(kburst),y_sc(kburst),z_sc(kburst),x_g,y_g,z_g)
        fd_g(k1,k2,i,j,kburst) = fd(lambda,x_sc(kburst),y_sc(kburst),
     1		z_sc(kburst),v_x_sc(kburst),v_y_sc(kburst),v_z_sc(kburst),
     2		x_g,y_g,z_g,vxt,vyt,vzt)

	c1_g_t(k1,k2) = c1_g(k1,k2,i,j,kburst)
	c2_g_t(k1,k2) = c2_g(k1,k2,i,j,kburst)

	r_g_t(k1,k2) = r_g(k1,k2,i,j,kburst)
	fd_g_t(k1,k2) = fd_g(k1,k2,i,j,kburst)

        if(k1.eq.1.and.k2.eq.1) then
        call v_poly(r_g_t(k1,k2),r_bip(kburst),coef_fd(1,kburst),m,fd_cnt)
        pbw_curr = pbw_near(kburst)+(r_g_t(k1,k2)-r_low(kburst))*pbw_slope
        chd_fd = (fd_cnt - pbw_curr/2.) -  fd_g_t(k1,k2)
        chd_c1 = chd_fd /(dfddth*180./pi) * r_g_t(k1,k2)
        chd_c1 = nint(chd_c1/c1_cell(kburst))*c1_cell(kburst)
        end if

        c1_g_t(k1,k2) = c1_g_t(k1,k2) + chd_c1 - i*c1_cell(kburst)

        call ac_to_ebf(c1_g_t(k1,k2),c2_g_t(k1,k2),
     *                  x_g,y_g,z_g,lat,lat_d,lon,rlocal_mean,
     *                  alpha1,alpha2,alpha3)
        call elevation(lat,lon,h,avg_terrain,
     *  dem_ellip,dlon_topo,dlat_topo,topo )
        call get_tv(lat,lon,lat_d,h,x_g,y_g,z_g,vxt,vyt,vzt)

        r_g(k1,k2,i,j,kburst)=
     1  dis(x_sc(kburst),y_sc(kburst),z_sc(kburst),x_g,y_g,z_g)
        fd_g(k1,k2,i,j,kburst) = fd(lambda,x_sc(kburst),y_sc(kburst),
     1  z_sc(kburst),v_x_sc(kburst),v_y_sc(kburst),v_z_sc(kburst),
     2  x_g,y_g,z_g,vxt,vyt,vzt)

        c1_g(k1,k2,i,j,kburst) = c1_g_t(k1,k2)
        r_g_t(k1,k2) = r_g(k1,k2,i,j,kburst)
        fd_g_t(k1,k2) = fd_g(k1,k2,i,j,kburst)




	end do
     	end do
c       write(6,*)'Burst PP lat lat_d lon:',lat,lat_d,lon

c	call rsmplcf(c1_g_t,c2_g_t,r_g_t,fd_g_t,coef_c1,coef_c2)

c     	do k1 = 1, 3
c     	do k2 = 1, 3
c	write(6,*) i,j,k1,k2,c1_g(k1,k2,i,j,kburst)
c	write(6,*) i,j,k1,k2,c2_g(k1,k2,i,j,kburst)
c	write(6,*) i,j,k1,k2,r_g(k1,k2,i,j,kburst)
c	write(6,*) i,j,k1,k2,fd_g(k1,k2,i,j,kburst)
c	end do
c     	end do

c************** test coefficients ***********************************
 	do k1 = 1, 3
 	do k2 = 1, 3
 	call v_poly2(r_g_t(k1,k2),c1_g_t(k1,k2),fdout,
     1			r_g_t(2,2),c1_g_t(2,2),fd_g_t(2,2),coef_c1)
 	call v_poly2(c1_g_t(k1,k2),c2_g_t(k1,k2),rout,
     1			c1_g_t(2,2),c2_g_t(2,2),r_g_t(2,2),coef_c2)

c	write(6,*) 'k1,k2,r,f***', k1,k2,rout,fdout
c	write(6,*) 'k1,k2,r,f%%%', k1,k2,r_g_t(k1,k2),fd_g_t(k1,k2)
     	end do
      	end do
c************** end of test coefficients ****************************

	end do
     	end do

        if (kburst .eq. 1) then
        write(6,*)' burst-pp :',kburst
        j=1
        i=1
        do k1 = 1, 3
        do k2 = 1, 3
        write(6,*)'c1_g:', i,j,k1,k2,c1_g(k1,k2,i,1,kburst)
        write(6,*)'c2_g:', i,j,k1,k2,c2_g(k1,k2,i,1,kburst)
        write(6,*)'r_g_g:', i,j,k1,k2,r_g(k1,k2,i,1,kburst)
        write(6,*)'fd_g_g:', i,j,k1,k2,fd_g(k1,k2,1,j,kburst)
        end do
        end do
        j=17
        i=1
        do k1 = 1, 3
        do k2 = 1, 3
        write(6,*)'c1_g:', i,j,k1,k2,c1_g(k1,k2,i,17,kburst)
        write(6,*)'c2_g:', i,j,k1,k2,c2_g(k1,k2,i,17,kburst)
        write(6,*)'r_g_g:', i,j,k1,k2,r_g(k1,k2,i,17,kburst)
c        write(6,*)'fd_g_g:', i,j,k1,k2,fd_g(k1,k2,17,j,kburst)
        end do
        end do
        endif

        return


	end



	subroutine prime_coefs(xs,ys,zs,vxs,vys,vzs,roll,yaw,pitch,
     c	az_pk_coef,r_t,fd_t,fr_t,thi,coef_r_fd,coef_r_fr,coef_r_ang,
     c	coef_r_inc,rng_bm,look_ang,ire_sim,
     *  dem_ellip,dlon_topo,dlat_topo,topo,avg_terrain)
	implicit real*8 (a-h, o-z)
c	include 'key_const.inc'
C*****WAYNE******
       include 'ssp2_const.inc'
       integer     dem_ellip
       real*8     dlon_topo
       real*8     dlat_topo
       integer*2     topo(ns_ew,ns_ns)
C*****WAYNE******
	real*8 coef_r_fd(4),coef_r_fr(4),coef_r_ang(4)
        real*8 coef_r_inc(4)
	real*8 vlook_ang(5),r_t(5),r_t1(5),fd_t(5),fr_t(5),thi(5),cc(4,5)
	real*8 xs,ys,zs,vxs,vys,vzs,roll,yaw,pitch,lat,lon,look_ang
        real*8 az_pk_coef(3),az_pk_ang
        real*8          avg_terrain

	n1 = 5
	m = 4		!polynomial order+1
	do i = 1, n1
	vlook_ang(i) = (i-n1/2-1)*rng_bm/4.d0+look_ang 
        az_pk_ang = az_pk_coef(1)+az_pk_coef(2)*vlook_ang(i)+
     1              az_pk_coef(3)*vlook_ang(i)**2
D       write(6,*) 'az coef, beam', az_pk_coef(1),az_pk_coef(2),az_pk_coef(3),rng_bm
D       write(6,*) 'vlook_ang(i) ', vlook_ang(i)
	p1= 0.
	p2= 0.
	p3= 1.
        call roll_rot(p1,p2,p3,vlook_ang(i))
	height = avg_terrain
c       write(6,*)'height',height
	call ptv_to_gip(p1,p2,p3,roll,yaw,pitch,az_pk_ang,height,
     c	xs,ys,zs,vxs,vys,vzs,lat,lon,xt,yt,zt,vxt,vyt,vzt,thi(i),
     *  dem_ellip,dlon_topo,dlat_topo,topo)
c	write(6,*) 'xs,vs', xs,ys,zs,vxs,vys,vzs
c	write(6,*) 'xt,vt', xt,yt,zt,vxt,vyt,vzt
c	write(6,*) 'az_pk_ang',az_pk_ang
c	write(6,*) 'thi',thi(i)
	call bip_par(lambda,gmass,xt,yt,zt,vxt,vyt,vzt,
     1                    xs,ys,zs,vxs,vys,vzs,r_t(i),fd_t(i),fr_t(i))
D	write(6,*) 'r_t,fd_t,fr_t',r_t(i),fd_t(i),fr_t(i)
	end do

	do i = 1, n1
	r_t1(i) = r_t(i)-r_t(n1/2+1)
D	write(6,*) 'r_t---' ,r_t1(i)
D	write(6,*) 'fd_t---',fd_t(i)
	end do

	call lsf(r_t1,fd_t,n1,coef_r_fd,m,cc)
D       write(6,*)'cc---',cc
D       write(6,*)'coef_r_fd1----',coef_r_fd(1)
D       write(6,*)'coef_r_fd1----',coef_r_fd(2)
D       write(6,*)'coef_r_fd1----',coef_r_fd(3)
D       write(6,*)'coef_r_fd1----',coef_r_fd(4)
	call lsf(r_t1,fr_t,n1,coef_r_fr,m,cc)
	call lsf(r_t1,vlook_ang,n1,coef_r_ang,m,cc)
        call lsf(r_t1,thi,n1,coef_r_inc,m,cc)



	return
	end


        SUBROUTINE AZ_ROT(p1,p2,p3,AZANG)
        IMPLICIT REAL*8 (A-H,O-Z)

        p1 = p1 + tand(AZANG)
        pmag = sqrt(p1**2+p2**2+p3**2)
        p1 = p1/pmag
        p2 = p2/pmag
        p3 = p3/pmag

        RETURN
        END


	subroutine ptv_to_gip(p1,p2,p3,roll,yaw,pitch,az_pk_ang,h,
     c	xs,ys,zs,vxs,vys,vzs,lat,lon,xt,yt,zt,vxt,vyt,vzt,thi,
     *  dem_ellip,dlon_topo,dlat_topo,topo)
        implicit real*8 (a-h, o-z)
c	include 'key_const.inc'
C*****WAYNE******
       include 'ssp2_const.inc'
       integer     dem_ellip
       real*8     dlon_topo
       real*8     dlat_topo
       integer*2     topo(ns_ew,ns_ns)
C*****WAYNE******
	real*8 lat,lat_d,lon

	call yaw_rot(p1,p2,p3,yaw)      !AC         	
	call roll_rot(p1,p2,p3,roll)    !AC	
	call pitch_rot(p1,p2,p3,pitch)	
        call az_rot(p1,p2,p3,az_pk_ang)
	call spc_to_EBF(p1,p2,p3,xs,ys,zs,vxs,vys,vzs)	
1111	CALL SEETARG(h,XS,YS,ZS,P1,P2,P3,XT,YT,ZT)
	call ebf_to_ll(xt,yt,zt,lat,lat_d,lon)
	call get_tv(lat,lon,lat_d,h,xt,yt,zt,vxt,vyt,vzt)
	call look_ang(p1,p2,p3,xs,ys,zs,alkang)
	call inc_ang(alkang,xs,ys,zs,xt,yt,zt,thi)

	return
	end
	subroutine look_ang(px,py,pz,xs,ys,zs,thl)
	implicit real*8 (a-h, o-z)

	rs = sqrt(xs**2+ys**2+zs**2)
	p_dot_s = px*xs+py*ys+pz*zs
	thl = acosd(-1*p_dot_s/rs)

	return
	end
	subroutine bip_par(lambda,gmass,xt,yt,zt,vxt,vyt,vzt,
     1                    xs,ys,zs,vxs,vys,vzs,r,fd,fr)
	implicit real*8 (a-h, o-z)
	real*8 lambda

        dvx = vxt - vxs
	dvy = vyt - vys
	dvz = vzt - vzs
	dx = xt - xs
	dy = yt - ys
	dz = zt - zs
        rs = sqrt(xs**2+ys**2+zs**2)
	dis_dx = sqrt(dx**2+dy**2+dz**2)
	dis_dv = sqrt(dvx**2+dvy**2+dvz**2)
        a0 = gmass/rs**3
	r = dis_dx
	fd =-2.d0*(dvx*dx+dvy*dy+dvz*dz)/(lambda*dis_dx) 
	fr =-2.d0*(dis_dv**2+(dx*xs+dy*ys+dz*zs)*a0)/(lambda*dis_dx)-
     1		2.d0*(dvx*dx+dvy*dy+dvz*dz)**2/(lambda*dis_dx**3) 

	return
	end
        SUBROUTINE ROLL_ROT(p1,p2,p3,roll)
        IMPLICIT REAL*8 (A-H,O-Z)
	call rotate_xyz(p1,p2,p3,roll,0.d0,0.d0)

	RETURN
	END



	SUBROUTINE ROTATE_XYZ(X,Y,Z,THIX,THIY,THIZ)

	IMPLICIT REAL*8 (A-H,O-Z)

	X1=X
	Y1=COSD(THIX)*Y+SIND(THIX)*Z
	Z1=-SIND(THIX)*Y+COSD(THIX)*Z
	Y2=Y1
	Z2=COSD(THIY)*Z1+SIND(THIY)*X1
	X2=-SIND(THIY)*Z1+COSD(THIY)*X1
	Z=Z2
	X=COSD(THIZ)*X2+SIND(THIZ)*Y2
	Y=-SIND(THIZ)*X2+COSD(THIZ)*Y2

	RETURN
	END


	SUBROUTINE YAW_ROT(p1,p2,p3,yaw)
	IMPLICIT REAL*8 (A-H,O-Z)

	call rotate_xyz(p1,p2,p3,0.d0,0.d0,yaw)

	RETURN
	END

	
	SUBROUTINE PITCH_ROT(p1,p2,p3,pitch)
	IMPLICIT REAL*8 (A-H,O-Z)

	call rotate_xyz(p1,p2,p3,0.d0,pitch,0.d0)
	
	RETURN
	END


	subroutine spc_to_EBF(p1,p2,p3,xs,ys,zs,vxs,vys,vzs)	
	implicit real*8 (a-h, o-z)

	call cross(vxs,vys,vzs,xs,ys,zs,c1,c2,c3)
	call cross(xs,ys,zs,c1,c2,c3,d1,d2,d3)
	call cross(d1,d2,d3,c1,c2,c3,z1,z2,z3)
	px = p1*d1+p2*c1+p3*z1
	py = p1*d2+p2*c2+p3*z2
	pz = p1*d3+p2*c3+p3*z3
	r = sqrt(px**2+py**2+pz**2)
	p1 = px/r
	p2 = py/r
	p3 = pz/r

	return
	end




        subroutine v_poly2(xin,yin,zout,xref,yref,zref,mu)
        implicit real*8 (a-h,o-z)
        real*8 mu(3,3),coef(3)

        m = 3
        x = xin-xref
        y = yin-yref
        z = 0.d0

        do k1 = 1, 3
        coef(k1) = mu(k1,m)
        do k2 = 1, m-1
        coef(k1) = coef(k1)*x+mu(k1,m-k2)
        end do
        end do

        z = coef(m)
        do k1 = 1, m-1
        z = z*y+coef(m-k1)
        end do
        zout = zref + z

        return
        end



	subroutine qsph_to_ebf(qlat,qlon,
     *              rlocal,x,y,z,lat,lat_d,lon,alpha1,alpha2,alpha3)
c	-------------------------------------------
C 	Abstract:
C	   Converts the spcacraft position from the quasi
C	   spherical coordinates into the earth body fixed coordinates.
c	-------------------------------------------
        implicit none
c	include 'proj_const.inc'
C*****WAYNE******
       real*8     alpha1
       real*8     alpha2
       real*8     alpha3
C*****WAYNE******
c	------------------
C	PASSING IN PARAMETERS
c	------------------
	real*8 qlat, qlon		!Quasi-spherical angles lat.lon.
	real*8 rlocal

c	------------------
C	PASSING OUT PARAMETERS
c	------------------
	real*8	x,y,z			!Sensor position	
	real*8 lat,lat_d,lon			!spherical angles lat.lon.

c	------------------
C	LOCAL VARIABLES
c	------------------

	x=rlocal*cosd(qlat)*cosd(qlon)
	y=rlocal*cosd(qlat)*sind(qlon)
	z=rlocal*sind(qlat)

	call rotate_z(x,y,z,alpha3,-1.)
        call rotate_y(x,y,z,alpha2,-1.)
        call rotate_z(x,y,z,alpha1,-1.)

	call ebf_to_ll(x,y,z,lat,lat_d,lon)	!These x,y,z are not real ebf
     	call ll_to_ebf(lat,lon,x,y,z)


	return
	end
c***********************************************************************
c	Jet Propulsion Laboratory
c	Section:	334, Radar Science and Engineering
c	Project:	RadarSAT
c	Task:		ScanSAR Data Processor
c	Code:           Fortran 77, Unix DEC 5000/133
c-----------------------------------------------------------------------
c	Module Name:	az_comp.f
c
c		Perform the Azimuth compression
c		for the RadarSAT ScanSAR Data.
c
c	Input:	1). A transposed range compressed data.
c		2). A associated Processing Parameters file.
c		    1.npulse  :# of cleaned pulses (last beam removed)	  
c		    2.ns      :# of samples.
c		    3.prf     :pulse repetition frequency in hz.
c		    4.fd_bip  :mid-range doppler freqency in hz.
c		    5.fr_bip  :mid-range doppler rate in hz per sec.
c
c	Output: A azimuth compressed data with complex form in freq. domain
c
c	Function:    
c		The azimuth processing include are
c
c		1) Azimuth Compreesion:
c		   The deramp-FFT method is chosen because of its 
c		   computional efficiency.
c
c		2).Impulse Response Shaping:
c		   Using a Kaiser weight in time domain to reduce the 
c		   integrated side lobe ratio (ISLR).
c
c		3).Perform a circular shift for each azimuth line:
c	          A circular shift is performed for each azimuth line to place
c		  the Mid-pulse of the burst at the beginning of the FFT buff 
c		  in time domain.
c
c	*** Algorithms designed and original coded by: Mike Jin ***
c----------------------------------------------------------------------------
c
c	05/23/94 Anhua Chu Added a flag of deramp 1st_order term
c			   for RadarSAT ScanSAR
c	02/14/93 Anhua Chu Initial code.
c****************************************************************************
       subroutine az_comp(
     *  kburst,nr,np_proc,nfft_az,ns,
     *  r_1st,dr,r_bip,coef_fd,
     *  coef_fr,prf,buff_in  )
	implicit none
C*****WAYNE******
       include 'ssp2_const.inc'
       integer     kburst
       integer*4   nr
       integer     np_proc
       integer*4     nfft_az
       integer*4     ns(burst_max)
       real*8     r_1st(burst_max)
       real*8     dr(burst_max)
       real*8     r_bip(burst_max)
       real*8     coef_fd(4,burst_max)
       real*8     coef_fr(4,burst_max)
       real*8     prf(burst_max)
       complex     buff_in(line_raw,sam_raw)
C*****WAYNE******
c	include 'key_const.inc'
c	include 'key_pp.inc'

c	---------------------
c	PASSING IN PARAMETERS
c	---------------------
c	complex buff_in(line_raw,sam_raw)  !Input buffer,range compressed data
c	equivalence (buff_in,buff2)
c	integer kburst			!burst index

c	-------------------
c	LOCAL VARIABLES
c	-------------------
	complex buffa(256)
	complex buffb(256)
	complex kw(256),deramp(256)
	real	alpha
	real*8 	r,taz,dtaz,fdc,frc,phi
	integer i,j,k,npulse,m,n_cell
        real*8  frc_c(sam_raw),fdc_c(sam_raw),frc_dummy,fdc_dummy



	npulse = np_proc
c	---------------
c	1. SETUP FFT
c	---------------

	call init_fft(nfft_az)		! initial cfft
	m=4

c	---------------
C	GENERATE KAISER WEIGHTS ACCORDING TO AZ LINES
c	pi*alpha=2.9 -> alpha=0.923 in time domain
c	---------------
	alpha = 0.923
	call kaiser( kw, npulse, alpha ) 

	n_cell = 10	
c	---------------
C	AZIMUTH COMPRESSION DO-LOOP
c	---------------
c*extract
c       do i = 1 ,ns(kburst) - nr

c          r = r_1st(kburst) + dr(kburst)*(i-1)
C          if(i/n_cell*n_cell.eq.i) then
c          call v_poly(r,r_bip(kburst),coef_fd(1,kburst),m,fdc_dummy)
c          call v_poly(r,r_bip(kburst),coef_fr(1,kburst),m,frc_dummy)
C          end if
c          fdc_c(i)= fdc_dummy
c          frc_c(i)= frc_dummy
c       enddo
c*extract
c$doacross local(i,j,taz,dtaz,phi,deramp,buffa,buffb)
	do i = 1 ,ns(kburst) - nr

           r = r_1st(kburst) + dr(kburst)*(i-1)
           call v_poly(r,r_bip(kburst),coef_fd(1,kburst),m,fdc_c(i))
           call v_poly(r,r_bip(kburst),coef_fr(1,kburst),m,frc_c(i))
C	   r = r_1st(kburst) + dr(kburst)*(i-1)
C	   if(i/n_cell*n_cell.eq.i) then
C	   call v_poly(r,r_bip(kburst),coef_fd(1,kburst),m,fdc)
C	   call v_poly(r,r_bip(kburst),coef_fr(1,kburst),m,frc)
C	   end if

	      taz = ( -npulse /2 - 1 ) / prf(kburst) 
	      dtaz = 1 / prf(kburst) 
	   do j=1,npulse
	      taz = taz + dtaz
	      phi=2.d0*pi*(-fdc_c(i)+prf(kburst)/2.d0-frc_c(i)/2.d0*taz)*taz
	      deramp(j)=cmplx(cos(phi),sin(phi))	!generate deramp
	      buffa(j) = buff_in(j,i)*deramp(j)		!deramp process
c             write(6,*)phi,taz,prf(kburst)
c             write(6,*)fdc_c(i),frc_c(i)
c             write(6,*)deramp(j)
c             write(6,*)buffa(j),buff_in(j,i)
	   enddo 

c	   ---------------
C	   PLACE THE MID-ECHO OF THE BURST AT THE BEGINNING OF FFT BUFFER
c	   & PERFORM KAISER WEIGHT 
c	   ---------------
	   do j=1,npulse/2
	      buffb(j)=buffa( npulse/2+j)*kw(j)
	      buffb(nfft_az-npulse/2+j)=buffa(j)*kw(npulse/2+j)	
	   end do

	   do j=npulse/2+1, nfft_az-npulse/2
	      buffb(j) = cmplx(0.,0.)  
	   end do

c	   ------------
C	   TRANSFORM TO FREQUENCY DOMAIN
c	   ------------
	   call cfft(buffb,1,nfft_az,1) !forward cfft

	   do j=1,nfft_az
c            buff_in(j,i)=buffb(j)
             buff_in(j,i)=buffb(j)/float(nfft_az)
	   enddo 

	end do

	return
	end
c*********************************************************************
	SUBROUTINE KAISER(A,N,ALPHA)

	COMPLEX A(N)
	PI=3.1415926

	PIALPA=PI*ALPHA
	CALL BESSEL(PIALPA,BSLI0)
C
	NW=N
	NW2=NW/2

	DO I=1,NW2
	   CI=(1.-((I-.5)/NW*2)**2)**.5*PIALPA
	   CALL BESSEL(CI,BSLI)
	   VRAL=BSLI/BSLI0
	   A(I)=CMPLX(VRAL,0.0)
	   A(NW-I+1)=A(I)
	END DO

	END	
c************  BESSEL  ************************************************

        subroutine BESSEL(AI,AO)
	AO=1.
	AT=1.
	DO K=1,20
	  AT=AT*AI/2/K
	  AO=AO+AT**2
	END DO
	RETURN
	END

c*******************************************************************
c	Jet Propulsion Laboratory
c	Section:	334, Radar Science and Engineering
c	Preoject:	RadarSat
c	Task:		ScanSAR
c	Code:		Fortran 77 
c-------------------------------------------------------------------
c	Module Name: range_comp.f
c
c	             Perform the Range compression
c		     for the RadarSAT Data.
c
c	Input:	1).32I / 32Q SAR BAQ Data.
c		2).A associated Processing Parameters file.
c		   1. np    : # of pulses ( echoes ).
c		   2. fd    : doppler freqency in hz.
c		   3. tau    : sample time in sec within a echo.
c	           4. Chirp_rate: range chirp rate in hz.
c	           5. plen  : range chirp duration in sec.
c		   6. nw    : range walk across the burst in terms of 
c			      range cell.
c	           7. n_fft: range FFT size.
c	           8. nfft_az: azimuth FFT size.
c
c	Output: A range compressed data stored in common buffer.
c
c	Function:    
c		Three functions to be performed by range processing are 
c		1) Range Doppler Phase Compensation:
c		   The nominal Doppler frequency is chosen for compensation,
c		   which is defined as the one associated with the boresight intercept
c		   point(BIP) at the Mid_Time of the burst reference time.
c
c		2).Pulse Compression:
c		   The range spectrum is weighted with a range weight function to
c		   achieve range resolution.
c
c		3).Range Walk Correction:
c		   The nominal range walk for each segment is determined by the nominal
c		   Doppler frequencyis accomplished by a phase reference function.
c
c     	***Algorithms designed & original coded by:  Mike Jin ***
c----------------------------------------------------------------------------
c
c	05/21/94	Anhua Chu Modified for RadarSAT ScanSAR,
c	                   processing 2048 range bin of each run
c	12/28/92	Anhua Chu Initial code.
c****************************************************************************
       subroutine range_comp(
     *  kburst,nr,fs,chirp_rate,fd_bip,
     *  nfft_az,np_proc,ns,r_1st,
     *  dr,r_bip,coef_fd,prf,
     *  buff_rg,np_v,buff_rep,ire_sim,
     *  dk_rep,gain_rep,file_rep,istatus,tbl)
	implicit none
C*****WAYNE******
       include 'ssp2_const.inc'
       character*60 file_rep
       integer      istatus
       integer      dk_rep(burst_max)
       real*8       gain_rep(burst_max)
       complex      tbl(-128:127) 
       integer*4       np_v(burst_max)
       complex buff_rep(2048,1)
       real*8     fs
       integer*4  nr
       real*8     chirp_rate
       real*8     fd_bip(burst_max)
       integer*4     nfft_az
       integer     np_proc
       integer*4     ns(burst_max)
       real*8     r_1st(burst_max)
       real*8     dr(burst_max)
       real*8     r_bip(burst_max)
       real*8     coef_fd(4,burst_max)
       real*8     prf(burst_max)
       complex buff_rg(sam_raw,line_raw)
       integer ire_sim
C*****WAYNE******
C	include 'key_const.inc'
C	include 'key_pp.inc'

c	-------------------
c	BUFFER PASSING IN
c	ONE BURST 32I/32Q
c	-------------------
c	complex buff_rg(sam_raw,line_raw)
c	equivalence (buff_rg,buff1)

c	-------------------
c	LOCAL VAVIABLES
c	-------------------
	integer n_2048,npulse,kburst
	parameter (n_2048=2048)
	complex buff_in(n_2048)
	complex r_ref(n_2048),kw(n_2048)
	integer np_no,ns_no,ns_start
	real*8 d_r_cell,r,fd,r_walk
	integer i,j,k,m
	
C	----------------------
c	1. CLEAR RANGE REF. BUFFER 
c	2. SETUP FFT 
C       3. GENERATE KAISER WEIGHTS ACCORDING TO AZ LINES
c          pi*alpha=2.8 -> alpha=0.89
c	4. GENERATE RANGE REFERENCE FUNTION SPECTRUM
C	----------------------

        call kaiser(kw,n_2048,0.8913 )    	!shape factor=.8913

        if (ire_sim .eq. 1) then
c         call get_rep(kburst,
c    *       nr,dk_rep,gain_rep,file_rep,istatus,tbl,buff_rep)
c         if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
c         do i=1,n_2048
c            r_ref(i)=buff_rep(i,1)
c         enddo
c qdn 4/9/97 switch to new reference function
	 call chirp_ref(chirp_rate,fs,fd_bip(kburst),nr,pi,r_ref,n_2048)
        else
	 call chirp_ref(chirp_rate,fs,fd_bip(kburst),nr,pi,r_ref,n_2048)
        endif

	call init_fft(n_2048)			!Initial cfft
	call cfft(r_ref,1,n_2048,1)	       	!perform cfft for chirp ref

 	do i=1,n_2048				!for SGI scaling
            r_ref(i)=r_ref(i)/float(n_2048*n_2048)
        enddo
        if (ire_sim.eq.1) then
D           write(6,*)'r_ref spec 1 2:',r_ref(1),r_ref(2)
D           write(6,*)'r_ref 2047 2048:',r_ref(2047),r_ref(2048)
D           write(6,*)'raw 2377/9:',buff_rg(2377,1),buff_rg(2379,1)
c qdn 4/9/97 switch to new reference function
c        r_ref(2046)= r_ref(2045)
c        r_ref(2047)= r_ref(2045)
c        r_ref(2048)= r_ref(2045)
         r_ref(1)=cmplx(0.,0.)
        else
         r_ref(1)=cmplx(0.,0.)
        endif

	m = 4

c	npulse = np_v(kburst)
 	npulse = np_proc

D       write(6,*)'r_1st dr ns_st',r_1st(kburst),dr(kburst),ns_start
D       write(6,*)' np_proc fd  prf lambda',npulse,fd_bip(kburst),prf(kburst),lambda
D       write(6,*)'np r coef_fd',npulse,r_bip(kburst),coef_fd(1,kburst)


C	----------------------
C	RANGE COMPRESSION DO-LOOP 
C	----------------------

c$doacross local(np_no,ns_no,ns_start,i,buff_in,r,r_walk,d_r_cell,fd) 
	do np_no=1, npulse
	   
	   do ns_no=1,(ns(kburst)-nr)/(n_2048-nr)+ 1	
	      ns_start=(ns_no-1)*(n_2048-nr) 

c	      do i=1,n_2048
c	          buff_in(i)=buff_rg(ns_start+i,np_no)
c	      enddo

              if(ns_start+n_2048.gt.ns(kburst)) then        !AC 10/3/95
              do i= ns_start+1, ns(kburst)                  !AC 10/3/95
                  buff_in(i-ns_start)=buff_rg(i,np_no)      !AC 10/3/95
              enddo                                         !AC 10/3/95
              do i= ns(kburst)+1, ns_start+n_2048           !AC 10/3/95
                  buff_in(i-ns_start)=cmplx(0.,0.)          !AC 10/3/95
              enddo                                         !AC 10/3/95
              else                                          !AC 10/3/95
              do i= ns_start+1, ns_start+n_2048             !AC 10/3/95
                  buff_in(i-ns_start)=buff_rg(i,np_no)      !AC 10/3/95
              enddo                                         !AC 10/3/95
              end if

            if (ns_no .eq. 2 .and. np_no .eq. 1 ) then
                do i=1,5
D               write(6,*)'raw rg_cmp:',np_no,i+905,buff_in(i+904)
                enddo
              endif

	      r = r_1st(kburst) + dr(kburst) * ns_start
	      call v_poly(r,r_bip(kburst),coef_fd(1,kburst),m,fd)

	      r_walk = -fd * npulse/prf(kburst) * (lambda/2.d0) / dr(kburst)

              d_r_cell= r_walk * (np_no-npulse/2-1)/(npulse -1.)
	      
              call rng_corr(buff_in,kw,r_ref,n_2048,d_r_cell)

              do i=1,min(n_2048,ns(kburst)-ns_start)-nr     !AC 10/3/95
c	      do i=1,n_2048-nr
	         buff_rg(ns_start+i,np_no)=buff_in(i)
	      enddo
	   enddo

	end do					!end of range compression do-loop

D       write(6,*)'buff_rg(1,1) ',buff_rg(1,1)

	return
	end


c************************* end of range_comp.f *****************************

	subroutine chirp_ref(chirp_rate,fs,fd,nr,pi,r_ref,n_fft)
	
c	----------------------------------------------------
c	Abstract: Generate a range chirp reference spectrum   
c	----------------------------------------------------
	
	implicit none
c	-----------------
c	Input Passing Parameter
c	-----------------
	real*8	chirp_rate	!range chirp rate
	real*8	fs		!sampling frequency
	real*8	fd		!Nominal Doppler centroid frequency
	real*8	pi		!3.14159
	integer nr		!# of chip for chirp 
	integer n_fft		!FFT size

c	-----------------
c	Output Passing Parameter
c	-----------------
	complex r_ref(n_fft)	!chirp reference function

c	-----------------
c	Local Variables
c	-----------------
	real*8	phi		
	integer i

	do i=1,nr 
	   phi= 2.d0*pi*(.5d0*chirp_rate*((i-nr/2)/fs)+fd)*(i-nr/2)/fs	
c          r_ref(i)=cmplx(cos(phi), sin(phi)) 
c qdn 4/9/97 switch to new function
	   r_ref(i)=85.*cmplx(cos(phi), sin(phi)) 
	end do
	do i=nr+1,n_fft 
	   r_ref(i)=cmplx(0.,0.)
	end do
	
	return
	end

c************************* end of chirp_ref.f *****************************

	subroutine rng_corr(buff,kw,r_ref,n_fft,d_r_cell)
	
	implicit none

c	-------------------
c	ARGUMENTS PASSING 
c	-------------------
	integer n_fft			!Length of fft 
	complex r_ref(n_fft)		!Range reference function
	complex kw(n_fft)		!Kaiser weighting function
	integer np_no			!Index for # of pulse

c	-------------------
c	BUFFER PASSING IN/OUT
c	-------------------
	complex buff(2048)		!Buffer contains 32I/32Q SAR raw data

c	-------------------
c	LOCAL VAVIABLES
c	-------------------
	integer forward,inverse
	parameter (forward=1,inverse=-1)
	integer i,j,k,increaments
	real*8 	pi,d_r_cell,phi1,phi2,dphase

C	PERFORM CFFT FOR EACH ECHO
	increaments=1
	call cfft(buff,increaments,n_fft,forward) 

	pi = 4.d0 * atan(1.)

	dphase = (2.d0 * pi * d_r_cell) / float(n_fft) 
	  
	do i=1,n_fft/2

c	   MULTIPLY WITH RANGE WEIGHT SPECTRUM

	   buff(i)=buff(i) *conjg(r_ref(i))

	   buff(n_fft+1 -i)=buff(n_fft+1 -i)
     * 	                    * conjg(r_ref(n_fft+1 -i))	

C	   MULTIPLY WITH RANGE WALK SPECTRUM AND KAISER WEIGHT

	   phi1 = dphase * (i-1)
	   phi2 = dphase * (-1. * i) 

	   buff(i)=buff(i) * cmplx(cos(phi1),sin(phi1)) *kw(i)

	   buff(n_fft+1 -i)=buff(n_fft+1 -i) 
     *                  *cmplx(cos(phi2),sin(phi2))*kw(n_fft+1-i)

	end do

c	-------------------
C	BACK TO TIME DOMAIN 
c	-------------------
	call cfft(buff,increaments,n_fft,inverse)	

	return
	end




        subroutine EBF_to_EME(omega0_e,second0,second,x,y,z,x1,y1,z1)
        implicit real*8 (a-h,o-z)
        include 'ssp2_const.inc'
cc      parameter (omega_e = 360.9856296d0 / (24.0d0 * 3600.0d0 ))

        omega_e_day =-(omega0_e+omega_e*(second-second0)*180.d0/pi)
        x1=cosd(omega_e_day)*x+sind(omega_e_day)*y
        y1=-1.*sind(omega_e_day)*x+cosd(omega_e_day)*y
        z1 = z

        return
        end

        subroutine get_np_air(ref_look_ang,peak_look_ang,roll,
     *                        x_sc,y_sc,z_sc,Re,Rp,
     *                        plen,prf,rg_gate_delay,cspeed,
     *                        rst,np_delay)

        implicit none

c       --------------
c       INPUT PARAMETER
c       --------------
        real*4  ref_look_ang    !for radarsat 29.8
        real*8  peak_look_ang   ! w1: -7.1
        real*8  rng_bm_ang      ! w1: 11.6 3dB width
        real*8  roll            !roll angle
        real*8  x_sc,y_sc,z_sc  !state vector
        real*8  Re,Rp           !earth radius
        real*8  plen            !pulse width
        real*8  prf             !PRF
        real*8  rg_gate_delay   !range gate delay time
        real*8  cspeed          !light speed

c       --------------
c       OUTPUT PARAMETER
c       --------------
        integer np_delay        !delay in terms of  pulses ,in the air
        integer rst             !slant range

c       --------------
c       LOCAL VARIABLES
c       --------------
        real*8  theda,phi,r_sc,look_ang

        look_ang=ref_look_ang+peak_look_ang-roll
        r_sc = sqrt(x_sc**2+y_sc**2+z_sc**2)
        theda=dasind(dsind(look_ang)*r_sc/Re)
        phi=theda-look_ang
        rst=dsind(phi)*r_sc/dsind(theda)
        np_delay=2*rst/cspeed * prf

D       write(6,*)'r_sc  rggate:',r_sc,rg_gate_delay,plen
D       write(6,*)'lk ref_lk phi:',look_ang,ref_look_ang,phi
D       write(6,*)'np_delay rst prf:',np_delay,rst,prf
        return
        end


        subroutine get_rep(cur_burst,
     *       nr,dk_rep,gain_rep,file_rep,istatus,tbl,buff_rep)

        implicit none

c        include 'key_const.inc'
c        include 'key_pp.inc'
         include 'ssp2_const.inc'

 
        integer*4       nr
        integer*4       dk_rep(burst_max)
        real*8          gain_rep(burst_max)
        character*60    file_rep
        integer         istatus
        complex     tbl(-128:127)
        complex buff_rep(2048,1)

c       --------------
c       INPUT PARAMETERS
c       --------------
        integer*4       cur_burst       !current burst number

c       --------------
c       OUTPUT PARAMETERS
c       --------------
ccc     complex buff_rep(2048,1) !move to key_pp.inc

c       --------------
c       LOCAL VARIABLES
c       --------------
        byte    buff_in(2048,1)
        integer samples,pulses,i
        integer bytes,dk_ptr,pw_2
        real*4  gain


        samples=nr
        pulses=1
        dk_ptr=dk_rep(cur_burst)
        gain=gain_rep(cur_burst)

D       write(6,*)' chirp # of chip:',samples
D       write(6,*)' rcplica gain:',gain,dk_ptr
        call read_disk(file_rep,buff_in,samples,dk_ptr,istatus)
        if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return

        call decode_4iq_repl(buff_in,pulses,samples,gain,tbl,buff_rep)

        do i=samples+1, ns_rep_buff     !2048 in ey_pp.inc
           buff_rep(i,1)=cmplx(0.,0.)
        enddo
c       if (cur_burst .eq. 1) then
c          do i=1,nr
c            type *,' replica:',cur_burst,i,buff_rep(i,1)
c           enddo
c       endif

        return
        end
C****************************************************************************
C****************************************************************************


        subroutine get_y(y,kbeam,prf,nprf,nset,hufd)
        real*8 y(60), yin(60), prf
        real hufd(60,200,4)

        nsize = 192000


        do k = 1, 60
        do i = 2, nset
        hufd(k,i,kbeam) = hufd(k,i,kbeam)-prf*nint((hufd(k,i,kbeam)-hufd(k,i-1,kbeam))/prf)
        end do
        end do

        do k = 1, 60
        y(k) = 0.
        do i = 1, nset
        yin(k) = hufd(k,i,kbeam)
        y(k) = y(k)+yin(k)
        end do
        y(k) = y(k)/nset+nprf*prf
        end do

        return
        end

        subroutine refine_azpk_coef(
     *  r_low,r_high,dr,r_bip,coef_lk_ang,prf,dfddth,hufd,pre_phase,
     *  beam_id,az_peak_coef,nbeams,beam_mode)
        include 'ssp2_const.inc'
        real*8     r_low(burst_max)
        real*8     r_high(burst_max)
        real*8     dr(burst_max)
        real*8     r_bip(burst_max)
        real*8     coef_lk_ang(4,burst_max)
        real*8     prf(burst_max)
        real*8     dfddth
        real hufd(60,200,4)
        real*8 rr(60),x(60),y(60),coef(7),c(3,4)
        integer pre_phase
        integer beam_mode
        integer beam_id(burst_max)
        real*8 az_peak_coef(3,8)

        m = 4
        ngroup = 200
        nset = 60

        do kbeam = 1, nbeams

        kburst =  kbeam+nbeams*ngroup/2
        margin = 20
        r_low_in =  max(r_low(kburst),r_low(kburst+nbeams))
        r_high_in =  min(r_high(kburst),r_high(kburst+nbeams))
        lines_b = int(((r_high_in-r_low_in)/dr(kburst)-margin)/nset)
        do i = 1, 60
        rr(i) = ((i-1)*lines_b+(margin+lines_b)/2)*dr(kburst)+r_low_in
        m = 4
        call v_poly(rr(i),r_bip(kburst),coef_lk_ang(1,kburst),m,x(i))
        end do

        call get_y(y,kbeam,prf(kbeam),0,ngroup,hufd)

        do i = 1, 60
        y(i) = y(i)/dfddth
        write(6,*)'Kbeam',kbeam,'set',i,x(i),'dang',y(i)
        end do

        m = 3
        call lsf(x,y,60,coef,m,c)
c qdn 7/23/97 check to see if SWB or SNB and update the beam 3
c       if(kbeam.eq.3 .and. beam_mode.eq.2 .or.
c    *     kbeam.eq.2 .and. beam_mode.eq.4 ) then
        if(beam_id(kbeam).eq.5) then
           print*,' QDN COEF (before) coef=',(coef(i),i=1,m)
           print*,' QDN COEF (before) coef+az_peak=',
     *            (coef(i)+az_peak_coef(i,beam_id(kbeam)),i=1,m)
           call lsf(x(31),y(31),30,coef,m,c) 
           print*,' QDN COEF SWB (half)for kbeam=',kbeam,
     *            'coef=',(coef(i),i=1,m)
           print*,' QDN COEF SWB (half) coef+az_peak=',
     *            (coef(i)+az_peak_coef(i,beam_id(kbeam)),i=1,m)
        end if

        if(pre_phase.eq.1) then
         write(6,*) 'Beam', kbeam, '       coef1= ', coef(1)
         write(6,*) 'Beam', kbeam, '       coef2= ', coef(2)
         write(6,*) 'Beam', kbeam, '       coef3= ', coef(3)
        else
         write(6,*) 'Beam', kbeam, '       coef1= ', coef(1)+az_peak_coef(1,beam_id(kbeam))
         write(6,*) 'Beam', kbeam, '       coef2= ', coef(2)+az_peak_coef(2,beam_id(kbeam))
         write(6,*) 'Beam', kbeam, '       coef3= ', coef(3)+az_peak_coef(3,beam_id(kbeam))
         write(6,*) '    '
        end if
         write(6,*) 'Beam', kbeam, '      dcoef1= ', coef(1)
         write(6,*) 'Beam', kbeam, '      dcoef2= ', coef(2)
         write(6,*) 'Beam', kbeam, '      dcoef3= ', coef(3)
         write(6,*) '    '

        end do

        return
        end

        subroutine fd_profile(
     *  r_low,r_high,dr,r_bip,coef_lk_ang,prf,dfddth,
     *  nbeams,burst_end,ire_sim,gain_rec,ns,np,df,fs,
     *  dk_burst,file_spc_raw,data_cond,tbl,blk_gain,snr,
     *  chirp_rate,fd_bip,nfft_az,r_1st,coef_fd,coef_fr,
     *  istatus,file_raw,nr,sim_eam_nsp,np_v,dk_rep,gain_rep,file_rep,
     *  aux_wdp_chg,aux_wdp_np,aux_agc_np,aux_wdp_cnt,aux_agc_chg,aux_agc_cnt,
     *  coef_fd_0,v_sc_mag,x_sc,y_sc,z_sc,v_x_sc,v_y_sc,v_z_sc,rng_bm_ang,
     *  beam_no,beam_id,az_peak_coef,roll,yaw,pitch,look_ang,dem_ellip,
     *  dlon_topo,dlat_topo,topo,i_ang_bip,fr_bip,np_air,fd_near,fr_near,fd_far,
     *  fr_far,r_cntr,fd_cntr,fr_cntr,avg_terrain,coef_inc,
     *  burst_start_p,burst_end_p,mytask,kids,allgrp,beam_mode )

        include 'ssp2_const.inc'
        include 'ssp2_dim.inc'
        implicit real*8 (a-h,o-z)
        integer burst_start_p,burst_end_p,mytask,kids,allgrp
        real*8     r_low(burst_max)
        real*8     r_high(burst_max)
        real*8     dr(burst_max)
        real*8     r_bip(burst_max)
        real*8     coef_lk_ang(4,burst_max)
        real*8     prf(burst_max)
        real*8     dfddth
        integer    nbeams,burst_end,ire_sim
        real*8 gain_rec(burst_max)
        integer ns(burst_max)
        integer np(burst_max)
        real*8 df(burst_max)
        real*8 fs
        integer dk_burst(burst_max)
        character*60 file_spc_raw
        integer data_cond
        complex tbl(-128:127)
        integer blk_gain(iq_blk,burst_max)
        real*8 snr(burst_max)
        real*8 chirp_rate
        real*8          fd_bip(burst_max)
        integer     nfft_az
        real*8          r_1st(burst_max)
        real*8          coef_fd(4,burst_max)
        real*8          coef_fr(4,burst_max)
        integer     istatus
        character*60 file_raw
        integer      nr
        integer*4       sim_eam_nsp(4)
        integer*4       np_v(burst_max)
        integer*4       dk_rep(burst_max)
        real*8          gain_rep(burst_max)
        character*60    file_rep
        real*8        aux_wdp_chg(max_agc,1600)
        integer*4     aux_wdp_np(max_agc,1600)
        integer*4     aux_agc_np(max_agc,1600)
        integer*4     aux_wdp_cnt(1600)
        real*8        aux_agc_chg(max_agc,1600)
        integer*4     aux_agc_cnt(1600)
       real*8     coef_fd_0(4,burst_max)
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
        real*8 roll(burst_max)
        real*8 yaw(burst_max)
        real*8 pitch(burst_max)
        real*8 look_ang(burst_max)
        integer dem_ellip
        real*8 dlon_topo,dlat_topo
        integer*2 topo(ns_ew,ns_ns)
        real*8          i_ang_bip(burst_max)
        real*8          fr_bip(burst_max)
        integer         np_air(burst_max)
        real*8          fd_near(burst_max)
        real*8          fr_near(burst_max)
        real*8          fd_far(burst_max)
        real*8          fr_far(burst_max)
        real*8          r_cntr(burst_max)
        real*8          fd_cntr(burst_max)
        real*8          fr_cntr(burst_max)
        real*8          avg_terrain
        real*8     coef_inc(4,burst_max)
        integer    beam_mode













c****LOCAL
        real hufd(60,200,4)
        byte buff0(2*sam_raw,line_raw)
        complex buff1(sam_raw,line_raw)
        complex buff2(line_raw,sam_raw)
        complex burst1(line_raw,sam_raw),burst2(line_raw,sam_raw)
        real     w1(64),w2(64),w3(64),w4(64)
        real        contrast,s_power
        integer     pre_phase
        real*8     az_pk_coef(3)
       real*8     thi(5)
       real*8     tp
       integer burst_count_start,burst_count_end,burst_k(800)

       integer  ifd,initdk,idum


        ngroup = 200
        ntt = nbeams*ngroup
        jump = ntt/ngroup

        pre_phase = 4   !or 1

        if (pre_phase .eq.1) then
        do k = 1, burst_end
        roll(k) = 0.
        yaw(k) = 0.
        pitch(k) = 0.
        end do
        end if

        pre_phase = 4
C**** SETTING UP BURSTS
c WAYNE
        itrouble = 0
        burst_count_start= -1
        burst_count_end  = -1
        nburst_cpu=0
        icount = 0
        do k = 1, ngroup
        do kbeam = 1, nbeams
           kburst = (k-1)*jump + kbeam
           icount=icount+1
           burst_k(icount) = kburst
c          write(6,*)'LOOKING ',kburst,k
           IF ((burst_k(icount).ge.burst_start_p).and.(burst_k(icount).le.burst_end_p)) then
c          write(6,*)'WORKING ',kburst,k
c
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
C**** SETTING UP BURSTS

        write(6,*)'QDN_PROFILE from',burst_k(burst_count_start),
     *                         ' to',burst_k(burst_count_end)

c       do k = 1, ngroup
c       do kbeam = 1, nbeams
        if (burst_count_start .ne. -1) then
        do i = burst_count_start,burst_count_end
        kburst = burst_k(i)
          write(6,*)'WORKING ',kburst
c       call clutter(kburst,contrast,s_power)
          call clutter(kburst,ire_sim,gain_rec,ns,
     *    np,dr,df,fs,dk_burst,file_spc_raw,data_cond,
     *    tbl,blk_gain,snr,buff0,buff1,buff2,chirp_rate,fd_bip,
     *    nfft_az,r_low,r_1st,r_bip,r_high,coef_fd,coef_fr,prf,
     *    nbeams,burst1,burst2,w1,w2,w3,w4,contrast,s_power,
     *    istatus,file_raw,nr,
     *    sim_eam_nsp,np_v,dk_rep,gain_rep,file_rep,
     *    aux_wdp_chg,aux_wdp_np,aux_agc_np,aux_wdp_cnt,aux_agc_chg,aux_agc_cnt,
     *    dfddth,pre_phase,coef_fd_0,thi,tp,v_sc_mag,x_sc,y_sc,z_sc,
     *    v_x_sc,v_y_sc,v_z_sc,rng_bm_ang,beam_no,beam_id,az_peak_coef,
     *    az_pk_coef,roll,yaw,pitch,look_ang,dem_ellip,dlon_topo,dlat_topo,
     *    topo,i_ang_bip,fr_bip,coef_lk_ang,np_air,fd_near,fr_near,
     *    fd_far,fr_far,r_cntr,fd_cntr,fr_cntr,avg_terrain,coef_inc,
     *    hufd)
          if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) then
            write(6,*)'data not available',kburst
            return
          endif

        end do
        endif
c       if(k.eq.k/20*20) call write_disk('hugefd.dat',hufd,192000,0)
c       end do

        ifd = initdk(idum,'/home/tmpdisk/hugefd.dat' //char(0))
        call iowrit(ifd,hufd,192000)
        call closedk(idum,ifd)

c       call write_disk('/home/tmpdisk/hugefd.dat ',hufd,192000,0)

           call collect_fd_clutter(
     *     r_bip,fd_bip,coef_fd,coef_fr,coef_lk_ang,r_low,r_high,
     *     dfddth,
     *     burst_count_start,burst_count_end,burst_k,nbeams,
     *     mytask,kids,allgrp,
     *     roll,yaw,pitch,hufd)

        write(6,*)'FINISHED fd_clutter'
        


        call refine_azpk_coef(
     *  r_low,r_high,dr,r_bip,coef_lk_ang,prf,dfddth,hufd,pre_phase,
     *  beam_id,az_peak_coef,nbeams,beam_mode)


        return
        end

