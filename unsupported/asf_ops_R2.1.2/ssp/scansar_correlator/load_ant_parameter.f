c SccsId = @(#)load_ant_parameter.f	2.41 3/24/98
         subroutine load_ant_parameter(file_w1,file_w2,file_w3,file_s5,
     *          file_s6,file_s7,file_az,file_name,istatus,az_peak_coef)
         include 'ssp2_const.inc'
c        real    ele_ang_ref,dang_rg,look_ang_cnt
c        parameter       (look_ang_cnt = 29.8)
c        parameter       (ele_ang_ref = -20.08)
c        parameter       (dang_rg = .0575)
         real*8 w1_elev(ns_ant_rg),w2_elev(ns_ant_rg),w3_elev(ns_ant_rg)
         real*8 s5_elev(ns_ant_rg),s6_elev(ns_ant_rg),s7_elev(ns_ant_rg)
         real*8 w1_gain(ns_ant_rg),w2_gain(ns_ant_rg),w3_gain(ns_ant_rg)
         real*8 s5_gain(ns_ant_rg),s6_gain(ns_ant_rg),s7_gain(ns_ant_rg)
         integer w1_num,w2_num,w3_num,s5_num,s6_num,s7_num
         real*8 w1_1st_elev,w2_1st_elev,w3_1st_elev,s5_1st_elev,s6_1st_elev,s7_1st_elev
         real*8 w1_elev_inc,w2_elev_inc,w3_elev_inc,s5_elev_inc,s6_elev_inc,s7_elev_inc
         real*8 w1_beamctr,w2_beamctr,w3_beamctr,s5_beamctr,s6_beamctr,s7_beamctr
         integer azm_num
         real*8 azm_inc
         real*8 azm_gain(ns_ant_rg)
         integer       istatus,ssp_get_ant
         real     ang_act
         real  ant_pat_gain(ns_ant_rg,7)
         real  ant_pat_az_gain(ns_ant_az)
         integer imode
         character*60  file_name
         character*60  file_w1,file_w2,file_w3,file_s5,file_s6,file_s7,file_az
         character*60  file_w1_p,file_w2_p,file_w3_p,file_s5_p,file_s6_p,file_s7_p,file_az_p
         integer     copen_w,fd
         real*8   az_peak_coef1(8),az_peak_coef2(8),az_peak_coef3(8)
         real*8   az_peak_coef(3,8)
         real*8   new_1st_elev,new_elev_inc
         real *8  az_1st_elev,az_elev_inc 

         do i=1,8
          az_peak_coef1(i)=0.0
          az_peak_coef2(i)=0.0
          az_peak_coef3(i)=0.0
         enddo
         file_w1_p='w1.dat.p'
         file_w2_p='w2.dat.p'
         file_w3_p='w3.dat.p'
         file_s5_p='s5.dat.p'
         file_s6_p='s6.dat.p'
         file_s7_p='s7.dat.p'
         file_az_p='az.dat.p'

         istatus= ssp_get_ant(file_name,
     *   w1_elev,w2_elev,w3_elev,s5_elev,s6_elev,s7_elev,
     *   w1_gain,w2_gain,w3_gain,s5_gain,s6_gain,s7_gain,
     *   w1_num,w2_num,w3_num,s5_num,s6_num,s7_num,
     *   w1_1st_elev,w2_1st_elev,w3_1st_elev,s5_1st_elev,s6_1st_elev,s7_1st_elev,
     *   w1_elev_inc,w2_elev_inc,w3_elev_inc,s5_elev_inc,s6_elev_inc,s7_elev_inc,
     *   w1_beamctr,w2_beamctr,w3_beamctr,s5_beamctr,s6_beamctr,s7_beamctr,
     *   azm_num,azm_inc,azm_gain,ns_ant_rg,az_peak_coef1,az_peak_coef2,az_peak_coef3)
         write(6,*)'istatus = ', istatus
         if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return

c        write(6,*)w1_num,w2_num,w3_num,s5_num,s6_num,s7_num
c        write(6,*)w1_1st_elev,w2_1st_elev,w3_1st_elev,s5_1st_elev,s6_1st_elev,s7_1st_elev
c        write(6,*)w1_elev_inc,w2_elev_inc,w3_elev_inc,s5_elev_inc,s6_elev_inc,s7_elev_inc
         
c        write(6,*)w1_beamctr,w2_beamctr,w3_beamctr,s5_beamctr,s6_beamctr,s7_beamctr
c        write(6,*)azm_num,azm_inc,azm_gain

         do i=1,8
          az_peak_coef(1,i)=az_peak_coef1(i)
          az_peak_coef(2,i)=az_peak_coef2(i)
          az_peak_coef(3,i)=az_peak_coef3(i)
         enddo
          

         
         new_1st_elev=  look_ang_cnt + ele_ang_ref
         new_elev_inc=  dang_rg
         
         imode= 1
         if (w1_num.eq.0) then
            call set_patn_zero(ant_pat_gain,imode)
         else
            call do_patn_inter(ant_pat_gain,new_1st_elev,new_elev_inc,imode,
     *         w1_1st_elev,w1_elev_inc,w1_gain,w1_num,file_w1_p)
         endif
         imode= 2
         if (w2_num.eq.0) then
            call set_patn_zero(ant_pat_gain,imode)
         else
            call do_patn_inter(ant_pat_gain,new_1st_elev,new_elev_inc,imode,
     *         w2_1st_elev,w2_elev_inc,w2_gain,w2_num,file_w2_p)
         endif
         imode= 3
         if (w3_num.eq.0) then
            call set_patn_zero(ant_pat_gain,imode)
         else
            call do_patn_inter(ant_pat_gain,new_1st_elev,new_elev_inc,imode,
     *         w3_1st_elev,w3_elev_inc,w3_gain,w3_num,file_w3_p)
         endif
         imode= 5
         if (s5_num.eq.0) then
            call set_patn_zero(ant_pat_gain,imode)
         else
            call do_patn_inter(ant_pat_gain,new_1st_elev,new_elev_inc,imode,
     *         s5_1st_elev,s5_elev_inc,s5_gain,s5_num,file_s5_p)
         endif
         imode= 6
         if (s6_num.eq.0) then
            call set_patn_zero(ant_pat_gain,imode)
         else
            call do_patn_inter(ant_pat_gain,new_1st_elev,new_elev_inc,imode,
     *         s6_1st_elev,s6_elev_inc,s6_gain,s6_num,file_s6_p)
         endif
         imode= 7
         if (s7_num.eq.0) then
            call set_patn_zero(ant_pat_gain,imode)
         else
            call do_patn_inter(ant_pat_gain,new_1st_elev,new_elev_inc,imode,
     *         s7_1st_elev,s7_elev_inc,s7_gain,s7_num,file_s7_p)
         endif

         idummy=ccreate(file_w1)
         fd=copen_w(file_w1)
         idummy=cwrite(fd,ant_pat_gain(1,1),700*4,0)
         idummy=ccreate(file_w2)
         fd=copen_w(file_w2)
         idummy=cwrite(fd,ant_pat_gain(1,2),700*4,0)
         idummy=ccreate(file_w3)
         fd=copen_w(file_w3)
         idummy=cwrite(fd,ant_pat_gain(1,3),700*4,0)
         idummy=ccreate(file_s5)
         fd=copen_w(file_s5)
         idummy=cwrite(fd,ant_pat_gain(1,5),700*4,0)
         idummy=ccreate(file_s6)
         fd=copen_w(file_s6)
         idummy=cwrite(fd,ant_pat_gain(1,6),700*4,0)
         idummy=ccreate(file_s7)
         fd=copen_w(file_s7)
         idummy=cwrite(fd,ant_pat_gain(1,7),700*4,0)

C* interpolate azimuth ?
c old X,Y:missing X azm_num,azm_inc,azm_gain
c new X,Y:missing X ant_pat_az_gain(ns_ant_az)
c
c   ?    new_1st_elev=  look_ang_cnt + ele_ang_ref
c   ?    new_elev_inc=  dang_rg
c   ?    az_1st_elev =
c   ?    az_elev_inc = azm_inc
c
c
c        call do_patn_az_inter(ant_pat_az_gain,new_1st_elev,new_elev_inc,
c    *         az_1st_elev,az_elev_inc,azm_gain,azm_num,file_az_p)
c
c   outcome: ant_pat_az_gain
c        fd=copen_w(file_az)
c        idummy=cwrite(fd,ant_pat_az_gain,200*4,0)
c


         return

         end
