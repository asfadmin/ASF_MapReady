c SccsId = @(#)pta_support.f	2.41 3/24/98
       subroutine collect_var_clutter(
     *     r_bip,fd_bip,coef_fd,coef_fr,coef_lk_ang,r_low,r_high,
     *     contrast,s_power,dfddth,
     *     burst_count_start,burst_count_end,burst_k,nbeams,
     *     mytask,kids,allgrp,
     *     roll,yaw,pitch)
       include 'ssp2_const.inc'
       real*8          r_bip(burst_max)
       real*8          fd_bip(burst_max)
       real*8          coef_fd(4,burst_max)
       real*8          coef_fr(4,burst_max)
       real*8          coef_lk_ang(4,burst_max)
       real*8          r_low(burst_max)
       real*8          r_high(burst_max)
       real contrast(800),s_power(800)
       integer   burst_count_start,burst_count_end,burst_k(800)
       integer   mytask,kids,allgrp,nbeams
       integer   idummy
       real*8    dfddth
       real*8     roll(burst_max)
       real*8     yaw(burst_max)
       real*8     pitch(burst_max)


        write(6,*)'COLLECTING VAR FROM CLUTTER'
        
        if (mytask.eq.0) then
            do iii=0,kids-1
             write(6,*)'clutter collecting data from node # ',iii
             if (iii.eq.0) then
              else
               call mpi_bcast(iii,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
               call mpi_recv(num_c,1,MPI_INTEGER,iii,0,MPI_COMM_WORLD,isuccess,ierr)
               if (num_c .gt. 1) then
                do i = 1,num_c
                 call mpi_recv(icc,1,MPI_INTEGER,iii,1,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(contrast(icc),1,MPI_REAL,iii,2,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(s_power(icc),1,MPI_REAL,iii,2,MPI_COMM_WORLD,isuccess,ierr)
                 iburst=burst_k(icc)
                   call mpi_recv(roll(iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                   call mpi_recv(yaw(iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                   call mpi_recv(pitch(iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(r_bip(iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(fd_bip(iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(r_low(iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(r_high(iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_fd(1,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_fd(2,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_fd(3,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_fd(4,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_fr(1,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_fr(2,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_fr(3,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_fr(4,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_lk_ang(1,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_lk_ang(2,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_lk_ang(3,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_lk_ang(4,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 iburst=burst_k(icc)+nbeams
                   call mpi_recv(roll(iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                   call mpi_recv(yaw(iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                   call mpi_recv(pitch(iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(r_bip(iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(fd_bip(iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(r_low(iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(r_high(iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_fd(1,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_fd(2,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_fd(3,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_fd(4,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_fr(1,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_fr(2,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_fr(3,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_fr(4,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_lk_ang(1,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_lk_ang(2,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_lk_ang(3,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_lk_ang(4,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                enddo
               endif
             endif
            enddo
        else
         do ii=1, kids-1
          call mpi_bcast(iii,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
          if (iii.eq.mytask) then
            write(6,*)'clutter sending data from node # ',iii
            idummy= burst_count_end-burst_count_start+1
            call mpi_send(idummy,1,MPI_INTEGER,0,0,MPI_COMM_WORLD,ierr)
            if (idummy.gt.1) then
             do i = burst_count_start,burst_count_end
              call mpi_send(i,1,MPI_INTEGER,0,1,MPI_COMM_WORLD,ierr)
              call mpi_send(contrast(i),1,MPI_REAL,0,2,MPI_COMM_WORLD,ierr)
              call mpi_send(s_power(i),1,MPI_REAL,0,2,MPI_COMM_WORLD,ierr)
              iburst=burst_k(i)
               call mpi_send(roll(iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
               call mpi_send(yaw(iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
               call mpi_send(pitch(iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(r_bip(iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(fd_bip(iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(r_low(iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(r_high(iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_fd(1,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_fd(2,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_fd(3,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_fd(4,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_fr(1,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_fr(2,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_fr(3,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_fr(4,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_lk_ang(1,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_lk_ang(2,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_lk_ang(3,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_lk_ang(4,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              iburst=burst_k(i)+nbeams
               call mpi_send(roll(iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
               call mpi_send(yaw(iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
               call mpi_send(pitch(iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(r_bip(iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(fd_bip(iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(r_low(iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(r_high(iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_fd(1,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_fd(2,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_fd(3,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_fd(4,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_fr(1,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_fr(2,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_fr(3,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_fr(4,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_lk_ang(1,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_lk_ang(2,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_lk_ang(3,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_lk_ang(4,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
             enddo
            endif
          endif
         enddo
        endif
C***
C***
C***
C***

       call mpi_bcast(r_bip       ,burst_max,MPI_REAL8,0,MPI_COMM_WORLD,ierr)
       call mpi_bcast(fd_bip      ,burst_max,MPI_REAL8,0,MPI_COMM_WORLD,ierr)
       call mpi_bcast(coef_fd     ,4*burst_max,MPI_REAL8,0,MPI_COMM_WORLD,ierr)
       call mpi_bcast(coef_fr     ,4*burst_max,MPI_REAL8,0,MPI_COMM_WORLD,ierr)
       call mpi_bcast(coef_lk_ang ,4*burst_max,MPI_REAL8,0,MPI_COMM_WORLD,ierr)
       call mpi_bcast(r_low       ,burst_max,MPI_REAL8,0,MPI_COMM_WORLD,ierr)
       call mpi_bcast(r_high      ,burst_max,MPI_REAL8,0,MPI_COMM_WORLD,ierr)
       call mpi_bcast(contrast    ,800,MPI_REAL,0,MPI_COMM_WORLD,ierr)
       call mpi_bcast(s_power     ,800,MPI_REAL,0,MPI_COMM_WORLD,ierr)
       call mpi_bcast(dfddth      ,1,MPI_REAL8,0,MPI_COMM_WORLD,ierr)
       call mpi_bcast(roll       ,burst_max,MPI_REAL8,0,MPI_COMM_WORLD,ierr)
       call mpi_bcast(yaw        ,burst_max,MPI_REAL8,0,MPI_COMM_WORLD,ierr)
       call mpi_bcast(pitch      ,burst_max,MPI_REAL8,0,MPI_COMM_WORLD,ierr)





       return
       end
C**
       subroutine collect_var_rng_cnt(
     *     r_bip,fd_bip,coef_fd,coef_fr,coef_lk_ang,r_low,r_high,
     *     dlk_ang,
     *     burst_count_start,burst_count_end,burst_k,nbeams,
     *     mytask,kids,allgrp)
       include 'ssp2_const.inc'
       real*8          r_bip(burst_max)
       real*8          fd_bip(burst_max)
       real*8          coef_fd(4,burst_max)
       real*8          coef_fr(4,burst_max)
       real*8          coef_lk_ang(4,burst_max)
       real*8          r_low(burst_max)
       real*8          r_high(burst_max)
       real*8 dlk_ang(burst_max)
       integer   burst_count_start,burst_count_end,burst_k(800)
       integer   mytask,kids,allgrp,nbeams
       integer   idummy

        
        if (mytask.eq.0) then
            do iii=0,kids-1
             write(6,*)'collecting data from node # ',iii
             if (iii.eq.0) then
              else
               call mpi_bcast(iii,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
               call mpi_recv(num_c,1,MPI_INTEGER,iii,0,MPI_COMM_WORLD,isuccess,ierr)
               if (num_c .gt. 1) then
                do i = 1,num_c
                 call mpi_recv(icc,1,MPI_INTEGER,iii,1,MPI_COMM_WORLD,isuccess,ierr)
                 iburst=burst_k(icc)
                 call mpi_recv(dlk_ang(iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(r_bip(iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(fd_bip(iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(r_low(iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(r_high(iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_fd(1,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_fd(2,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_fd(3,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_fd(4,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_fr(1,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_fr(2,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_fr(3,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_fr(4,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_lk_ang(1,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_lk_ang(2,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_lk_ang(3,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_lk_ang(4,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
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
            if (idummy.gt.1) then
             do i = burst_count_start,burst_count_end
              call mpi_send(i,1,MPI_INTEGER,0,1,MPI_COMM_WORLD,ierr)
              iburst=burst_k(i)
              call mpi_send(dlk_ang(iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(r_bip(iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(fd_bip(iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(r_low(iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(r_high(iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_fd(1,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_fd(2,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_fd(3,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_fd(4,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_fr(1,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_fr(2,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_fr(3,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_fr(4,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_lk_ang(1,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_lk_ang(2,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_lk_ang(3,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_lk_ang(4,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
             enddo
            endif
          endif
         enddo
        endif
C***


       call mpi_bcast(r_bip        ,burst_max,MPI_REAL8,0,MPI_COMM_WORLD,ierr)
       call mpi_bcast(fd_bip       ,burst_max,MPI_REAL8,0,MPI_COMM_WORLD,ierr)
       call mpi_bcast(coef_fd      ,4*burst_max,MPI_REAL8,0,MPI_COMM_WORLD,ierr)
       call mpi_bcast(coef_fr      ,4*burst_max,MPI_REAL8,0,MPI_COMM_WORLD,ierr)
       call mpi_bcast(coef_lk_ang  ,4*burst_max,MPI_REAL8,0,MPI_COMM_WORLD,ierr)
       call mpi_bcast(r_low        ,burst_max,MPI_REAL8,0,MPI_COMM_WORLD,ierr)
       call mpi_bcast(r_high       ,burst_max,MPI_REAL8,0,MPI_COMM_WORLD,ierr)
       call mpi_bcast(dlk_ang      ,burst_max,MPI_REAL8,0,MPI_COMM_WORLD,ierr)



       return
       end

c qdn 6/3/97
       subroutine collect_var_rng_cnt2(
     *     r_bip,fd_bip,coef_fd,coef_fr,coef_lk_ang,r_low,r_high,
     *     dlk_ang,roll,
     *     burst_count_start,burst_count_end,burst_k,nbeams,
     *     mytask,kids,allgrp)
       include 'ssp2_const.inc'
       real*8          r_bip(burst_max)
       real*8          fd_bip(burst_max)
       real*8          coef_fd(4,burst_max)
       real*8          coef_fr(4,burst_max)
       real*8          coef_lk_ang(4,burst_max)
       real*8          r_low(burst_max)
       real*8          r_high(burst_max)
       real*8          dlk_ang(burst_max)
       real*8          roll(burst_max)
       integer   burst_count_start,burst_count_end,burst_k(800)
       integer   mytask,kids,allgrp,nbeams
       integer   idummy


        if (mytask.eq.0) then
            do iii=0,kids-1
             write(6,*)'collecting data from node # ',iii
             if (iii.eq.0) then
              else
               call mpi_bcast(iii,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
               call mpi_recv(num_c,1,MPI_INTEGER,iii,0,MPI_COMM_WORLD,isuccess,ierr)
               if (num_c .gt. 1) then
                do i = 1,num_c
                 call mpi_recv(icc,1,MPI_INTEGER,iii,1,MPI_COMM_WORLD,isuccess,ierr)
                 iburst=burst_k(icc)
                 call mpi_recv(dlk_ang(iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
c qdn 6/3/97
                 call mpi_recv(roll(iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(r_bip(iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(fd_bip(iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(r_low(iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(r_high(iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_fd(1,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_fd(2,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_fd(3,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_fd(4,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_fr(1,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_fr(2,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_fr(3,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_fr(4,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_lk_ang(1,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_lk_ang(2,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_lk_ang(3,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_lk_ang(4,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
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
            if (idummy.gt.1) then
             do i = burst_count_start,burst_count_end
              call mpi_send(i,1,MPI_INTEGER,0,1,MPI_COMM_WORLD,ierr)
              iburst=burst_k(i)
              call mpi_send(dlk_ang(iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
c qdn 6/3/97
              call mpi_send(roll(iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(r_bip(iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(fd_bip(iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(r_low(iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(r_high(iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_fd(1,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_fd(2,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_fd(3,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_fd(4,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_fr(1,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_fr(2,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_fr(3,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_fr(4,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_lk_ang(1,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_lk_ang(2,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_lk_ang(3,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_lk_ang(4,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
             enddo
            endif
          endif
         enddo
        endif
C***


       call mpi_bcast(r_bip        ,burst_max,MPI_REAL8,0,MPI_COMM_WORLD,ierr)
       call mpi_bcast(fd_bip       ,burst_max,MPI_REAL8,0,MPI_COMM_WORLD,ierr)
       call mpi_bcast(coef_fd      ,4*burst_max,MPI_REAL8,0,MPI_COMM_WORLD,ierr)
       call mpi_bcast(coef_fr      ,4*burst_max,MPI_REAL8,0,MPI_COMM_WORLD,ierr)
       call mpi_bcast(coef_lk_ang  ,4*burst_max,MPI_REAL8,0,MPI_COMM_WORLD,ierr)
       call mpi_bcast(r_low        ,burst_max,MPI_REAL8,0,MPI_COMM_WORLD,ierr)
       call mpi_bcast(r_high       ,burst_max,MPI_REAL8,0,MPI_COMM_WORLD,ierr)
       call mpi_bcast(dlk_ang      ,burst_max,MPI_REAL8,0,MPI_COMM_WORLD,ierr)
c qdn 6/3/97
       call mpi_bcast(roll         ,burst_max,MPI_REAL8,0,MPI_COMM_WORLD,ierr)



       return
       end

       subroutine   collect_roll_yaw_pitch(
     *     roll,yaw,pitch,
     *     burst_count_start,burst_count_end,burst_k,nbeams,
     *     mytask,kids,allgrp)
       include 'ssp2_const.inc'
       real*8     roll(burst_max)
       real*8     yaw(burst_max)
       real*8     pitch(burst_max)
       integer   burst_count_start,burst_count_end,burst_k(800)
       integer   mytask,kids,allgrp
       integer   idummy
        if (mytask.eq.0) then
            do iii=0,kids-1
             write(6,*)'collecting data from node # ',iii
             if (iii.eq.0) then
              else
               call mpi_bcast(iii,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
               call mpi_recv(num_c,1,MPI_INTEGER,iii,0,MPI_COMM_WORLD,isuccess,ierr)
               if (num_c .gt. 1) then
                do i = 1,num_c
                  call mpi_recv(icc,1,MPI_INTEGER,iii,1,MPI_COMM_WORLD,isuccess,ierr)
                  do index= 0, nbeams-1
                   iburst=burst_k(icc)+index
                   call mpi_recv(roll(iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                   call mpi_recv(yaw(iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                   call mpi_recv(pitch(iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                  enddo
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
             do i = burst_count_start,burst_count_end
              call mpi_send(i,1,MPI_INTEGER,0,1,MPI_COMM_WORLD,ierr)
              do index= 0, nbeams-1
               iburst=burst_k(i)+index
               call mpi_send(roll(iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
               call mpi_send(yaw(iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
               call mpi_send(pitch(iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              enddo
             enddo
            endif
          endif
         enddo
        endif

       call mpi_bcast(roll       ,burst_max,MPI_REAL8,0,MPI_COMM_WORLD,ierr)
       call mpi_bcast(yaw        ,burst_max,MPI_REAL8,0,MPI_COMM_WORLD,ierr)
       call mpi_bcast(pitch      ,burst_max,MPI_REAL8,0,MPI_COMM_WORLD,ierr)

       return
       end

       subroutine collect_coef_fd0(
     *     coef_fd_0,
     *     burst_count_start,burst_count_end,burst_k,
     *     mytask,kids,allgrp)
       include 'ssp2_const.inc'
       real*8     coef_fd_0(4,burst_max)
       integer   burst_count_start,burst_count_end,burst_k(800)
       integer   mytask,kids,allgrp
       integer   idummy

        
        if (mytask.eq.0) then
            do iii=0,kids-1
             write(6,*)'collecting data from node # ',iii
             if (iii.eq.0) then
              else
               call mpi_bcast(iii,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
               call mpi_recv(num_c,1,MPI_INTEGER,iii,0,MPI_COMM_WORLD,isuccess,ierr)
               if (num_c .gt. 1) then
                do i = 1,num_c
                  call mpi_recv(icc,1,MPI_INTEGER,iii,1,MPI_COMM_WORLD,isuccess,ierr)
                  iburst=burst_k(icc)
                  call mpi_recv(coef_fd_0(1,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                  call mpi_recv(coef_fd_0(2,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                  call mpi_recv(coef_fd_0(3,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                  call mpi_recv(coef_fd_0(4,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
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
             do i = burst_count_start,burst_count_end
              call mpi_send(i,1,MPI_INTEGER,0,1,MPI_COMM_WORLD,ierr)
              iburst=burst_k(i)
              call mpi_send(coef_fd_0(1,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_fd_0(2,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_fd_0(3,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_fd_0(4,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
             enddo
            endif
          endif
         enddo
        endif
C***

      call mpi_bcast(coef_fd_0      ,4*burst_max,MPI_REAL8,0,MPI_COMM_WORLD,ierr)



       return
       end
       subroutine collect_fd_clutter(
     *     r_bip,fd_bip,coef_fd,coef_fr,coef_lk_ang,r_low,r_high,
     *     dfddth,
     *     burst_count_start,burst_count_end,burst_k,nbeams,
     *     mytask,kids,allgrp,
     *     roll,yaw,pitch,hufd)
       include 'ssp2_const.inc'
       real*8          r_bip(burst_max)
       real*8          fd_bip(burst_max)
       real*8          coef_fd(4,burst_max)
       real*8          coef_fr(4,burst_max)
       real*8          coef_lk_ang(4,burst_max)
       real*8          r_low(burst_max)
       real*8          r_high(burst_max)
       integer   burst_count_start,burst_count_end,burst_k(800)
       integer   mytask,kids,allgrp,nbeams
       integer   idummy
       real*8    dfddth
       real*8     roll(burst_max)
       real*8     yaw(burst_max)
       real*8     pitch(burst_max)
       real hufd(60,200,4)
       real hufd_dummy(60)


        write(6,*)'COLLECTING VAR FROM CLUTTER'
        
        if (mytask.eq.0) then
            do iii=0,kids-1
             write(6,*)'clutter collecting data from node # ',iii
             if (iii.eq.0) then
              else
               call mpi_bcast(iii,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
               call mpi_recv(num_c,1,MPI_INTEGER,iii,0,MPI_COMM_WORLD,isuccess,ierr)
               if (num_c .gt. 1) then
                do i = 1,num_c
                 call mpi_recv(icc,1,MPI_INTEGER,iii,1,MPI_COMM_WORLD,isuccess,ierr)
C**********
                 call unpack_and_receive(hufd,hufd_dummy,icc,nbeams,iii)
C**********
                 iburst=burst_k(icc)
                   call mpi_recv(roll(iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                   call mpi_recv(yaw(iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                   call mpi_recv(pitch(iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(r_bip(iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(fd_bip(iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(r_low(iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(r_high(iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_fd(1,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_fd(2,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_fd(3,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_fd(4,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_fr(1,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_fr(2,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_fr(3,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_fr(4,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_lk_ang(1,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_lk_ang(2,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_lk_ang(3,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_lk_ang(4,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 iburst=burst_k(icc)+nbeams
                   call mpi_recv(roll(iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                   call mpi_recv(yaw(iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                   call mpi_recv(pitch(iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(r_bip(iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(fd_bip(iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(r_low(iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(r_high(iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_fd(1,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_fd(2,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_fd(3,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_fd(4,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_fr(1,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_fr(2,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_fr(3,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_fr(4,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_lk_ang(1,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_lk_ang(2,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_lk_ang(3,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                 call mpi_recv(coef_lk_ang(4,iburst),1,MPI_REAL8,iii,3,MPI_COMM_WORLD,isuccess,ierr)
                enddo
               endif
             endif
            enddo
        else
         do ii=1, kids-1
          call mpi_bcast(iii,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
          if (iii.eq.mytask) then
            write(6,*)'clutter sending data from node # ',iii
            idummy= burst_count_end-burst_count_start+1
            call mpi_send(idummy,1,MPI_INTEGER,0,0,MPI_COMM_WORLD,ierr)
            if (idummy.gt.1) then
             do i = burst_count_start,burst_count_end
              call mpi_send(i,1,MPI_INTEGER,0,1,MPI_COMM_WORLD,ierr)
C***
              call pack_and_send(hufd,hufd_dummy,i,nbeams,0)
C***
              iburst=burst_k(i)
               call mpi_send(roll(iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
               call mpi_send(yaw(iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
               call mpi_send(pitch(iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(r_bip(iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(fd_bip(iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(r_low(iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(r_high(iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_fd(1,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_fd(2,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_fd(3,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_fd(4,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_fr(1,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_fr(2,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_fr(3,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_fr(4,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_lk_ang(1,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_lk_ang(2,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_lk_ang(3,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_lk_ang(4,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              iburst=burst_k(i)+nbeams
               call mpi_send(roll(iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
               call mpi_send(yaw(iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
               call mpi_send(pitch(iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(r_bip(iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(fd_bip(iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(r_low(iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(r_high(iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_fd(1,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_fd(2,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_fd(3,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_fd(4,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_fr(1,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_fr(2,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_fr(3,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_fr(4,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_lk_ang(1,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_lk_ang(2,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_lk_ang(3,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
              call mpi_send(coef_lk_ang(4,iburst),1,MPI_REAL8,0,3,MPI_COMM_WORLD,ierr)
             enddo
            endif
          endif
         enddo
        endif
C***
C***
C***
C***

       call mpi_bcast(r_bip       ,burst_max,MPI_REAL8,0,MPI_COMM_WORLD,ierr)
       call mpi_bcast(fd_bip      ,burst_max,MPI_REAL8,0,MPI_COMM_WORLD,ierr)
       call mpi_bcast(coef_fd     ,4*burst_max,MPI_REAL8,0,MPI_COMM_WORLD,ierr)
       call mpi_bcast(coef_fr     ,4*burst_max,MPI_REAL8,0,MPI_COMM_WORLD,ierr)
       call mpi_bcast(coef_lk_ang ,4*burst_max,MPI_REAL8,0,MPI_COMM_WORLD,ierr)
       call mpi_bcast(r_low       ,burst_max,MPI_REAL8,0,MPI_COMM_WORLD,ierr)
       call mpi_bcast(r_high      ,burst_max,MPI_REAL8,0,MPI_COMM_WORLD,ierr)
       call mpi_bcast(dfddth      ,1,MPI_REAL8,0,MPI_COMM_WORLD,ierr)
       call mpi_bcast(roll       ,burst_max,MPI_REAL8,0,MPI_COMM_WORLD,ierr)
       call mpi_bcast(yaw        ,burst_max,MPI_REAL8,0,MPI_COMM_WORLD,ierr)
       call mpi_bcast(pitch      ,burst_max,MPI_REAL8,0,MPI_COMM_WORLD,ierr)
       call mpi_bcast(hufd       ,60*200*4,MPI_REAL,0,MPI_COMM_WORLD,ierr)





       return
       end
       subroutine pack_and_send(hufd,hufd_dummy,icount,nbeams,inode)
       include 'ssp2_const.inc'
       real hufd(60,200,4)
       real hufd_dummy(60)
       iremain=mod(icount,nbeams)
       if (iremain.eq.0) then
         index_x= icount/nbeams
         index_y= nbeams
       else
         index_x= icount/nbeams+1
         index_y= iremain
       endif
       do i=1,60
         hufd_dummy(i)=hufd(i,index_x,index_y)
       enddo
       call mpi_send(hufd_dummy,60,MPI_REAL,inode,3,MPI_COMM_WORLD,ierr)


       

   
       return
       end

       subroutine unpack_and_receive(hufd,hufd_dummy,icount,nbeams,inode)
       include 'ssp2_const.inc'
       real hufd(60,200,4)
       real hufd_dummy(60)
       call mpi_recv(hufd_dummy,60,MPI_REAL,inode,3,MPI_COMM_WORLD,isuccess,ierr)

       iremain=mod(icount,nbeams)
       if (iremain.eq.0) then
         index_x= icount/nbeams
         index_y= nbeams
       else
         index_x= icount/nbeams+1
         index_y= iremain
       endif
       do i=1,60
         hufd(i,index_x,index_y)=hufd_dummy(i)
       enddo

       return
       end
