c SccsId = @(#)ZERO_TO_ONE.f	2.41 3/24/98
	subroutine pre_to_disk(yaw,pitch,roll,pre_phase,file_pre_to_corr,istatus)
C*****WAYNE******
       include 'ssp2_const.inc'
       real*8     roll(burst_max)
       real*8     yaw(burst_max)
       real*8     pitch(burst_max)
       integer    pre_phase
       character*60     file_pre_to_corr
       integer    istatus
C*****WAYNE******
       integer              ifd,bytes,ccreate,copen_w,cwrite_seq

       ifd            =copen_w(file_pre_to_corr)
       if (ifd .le. 0) then
         istatus=ierr_2
         idummy=printflog(3,'Failed to open '//file_pre_to_corr//'&')
         idummy=printerr(file_pre_to_corr)
         return
       endif 
       bytes          =8*burst_max
       nbyte          =cwrite_seq(ifd,roll        ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_pre_to_corr//'&')
           idummy=printerr(file_pre_to_corr)
           return
       endif
       nbyte          =cwrite_seq(ifd,yaw         ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_pre_to_corr//'&')
           idummy=printerr(file_pre_to_corr)
           return
       endif
       nbyte          =cwrite_seq(ifd,pitch       ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_pre_to_corr//'&')
           idummy=printerr(file_pre_to_corr)
           return
       endif
       bytes          =4
       nbyte          =cwrite_seq(ifd,pre_phase       ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_pre_to_corr//'&')
           idummy=printerr(file_pre_to_corr)
           return
       endif


       return
       end

	subroutine disk_to_corr(yaw,pitch,roll,pre_phase,file_pre_to_corr,istatus)
C*****WAYNE******
       include 'ssp2_const.inc'
       real*8     roll(burst_max)
       real*8     yaw(burst_max)
       real*8     pitch(burst_max)
       integer    pre_phase
       character*60 file_pre_to_corr
       integer   istatus
C*****WAYNE******
       integer              ifd,bytes,copen_r,cread_seq

       ifd            =copen_r(file_pre_to_corr)
       if(ifd .le. 0)then
        istatus=ierr_2
        idummy=printflog(3,'Failed to open '//file_pre_to_corr//'&')
        idummy=printerr(file_pre_to_corr)
        return
       endif
       bytes          =8*burst_max
       nbyte          =cread_seq(ifd,roll        ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_pre_to_corr//'&')
        idummy=printerr(file_pre_to_corr)
        return
       endif
       nbyte          =cread_seq(ifd,yaw         ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_pre_to_corr//'&')
        idummy=printerr(file_pre_to_corr)
        return
       endif
       nbyte          =cread_seq(ifd,pitch       ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_pre_to_corr//'&')
        idummy=printerr(file_pre_to_corr)
        return
       endif
       bytes          =4
       nbyte          =cread_seq(ifd,pre_phase       ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_pre_to_corr//'&')
        idummy=printerr(file_pre_to_corr)
        return
       endif

       return
       end
