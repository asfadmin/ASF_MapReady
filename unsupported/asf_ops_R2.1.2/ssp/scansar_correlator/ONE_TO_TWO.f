c SccsId = @(#)ONE_TO_TWO.f	2.41 3/24/98
	subroutine correlator_to_disk(
     *  c1_frm,c2_frm,
     *  sam_ac2,ct_prof_len,l1_ct_pro,l2_ct_pro,
     *  v_filler,
     *  ct_profile,
     *  alpha1,
     *  alpha2,alpha3,
     *  ipage,ipage_start,file_corr_to_post,variance,istatus,rlocal_mean)
C*****WAYNE******
       include 'ssp2_const.inc'
       real*8       rlocal_mean
       integer*4     sam_ac2
       integer     ct_prof_len
       integer     l1_ct_pro
       integer     l2_ct_pro
       integer*4     v_filler
       real*8     alpha1
       real*8     alpha2
       real*8     alpha3
       real*8     c1_frm(2,2)
       real*8     c2_frm(2,2)
       real*8     ct_profile(sam_ac)
       integer    ipage,ipage_start
       real*8     variance
C*****WAYNE******
       character*60     file_corr_to_post
       integer              ifd,bytes,ccreate,copen_w,cwrite_seq

       ifd            =copen_w(file_corr_to_post)
       if (ifd .le. 0) then
         istatus=ierr_2
         idummy=printflog(3,'Failed to open '//file_corr_to_post//'&')
         idummy=printerr(file_corr_to_post)
         return
       endif 
       bytes          =4
       nbyte          =cwrite_seq(ifd,ipage        ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_post//'&')
           idummy=printerr(file_corr_to_post)
           return
       endif
       nbyte          =cwrite_seq(ifd,ipage_start  ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_post//'&')
           idummy=printerr(file_corr_to_post)
           return
       endif
       nbyte          =cwrite_seq(ifd,sam_ac2      ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_post//'&')
           idummy=printerr(file_corr_to_post)
           return
       endif
       nbyte          =cwrite_seq(ifd,ct_prof_len  ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_post//'&')
           idummy=printerr(file_corr_to_post)
           return
       endif
       nbyte          =cwrite_seq(ifd,l1_ct_pro    ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_post//'&')
           idummy=printerr(file_corr_to_post)
           return
       endif
       nbyte          =cwrite_seq(ifd,l2_ct_pro    ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_post//'&')
           idummy=printerr(file_corr_to_post)
           return
       endif
       nbyte          =cwrite_seq(ifd,v_filler     ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_post//'&')
           idummy=printerr(file_corr_to_post)
           return
       endif
       bytes          =8
       nbyte          =cwrite_seq(ifd,alpha1       ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_post//'&')
           idummy=printerr(file_corr_to_post)
           return
       endif
       nbyte          =cwrite_seq(ifd,alpha2       ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_post//'&')
           idummy=printerr(file_corr_to_post)
           return
       endif
       nbyte          =cwrite_seq(ifd,alpha3       ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_post//'&')
           idummy=printerr(file_corr_to_post)
           return
       endif
       nbyte          =cwrite_seq(ifd,variance     ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_post//'&')
           idummy=printerr(file_corr_to_post)
           return
       endif
       bytes          =2*2*8
       nbyte          =cwrite_seq(ifd,c1_frm       ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_post//'&')
           idummy=printerr(file_corr_to_post)
           return
       endif
       nbyte          =cwrite_seq(ifd,c2_frm       ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_post//'&')
           idummy=printerr(file_corr_to_post)
           return
       endif
       bytes          =sam_ac*8
       nbyte          =cwrite_seq(ifd,ct_profile   ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_post//'&')
           idummy=printerr(file_corr_to_post)
           return
       endif
       bytes          =8
       nbyte          =cwrite_seq(ifd,rlocal_mean   ,bytes)
       if ( nbyte .ne. bytes) then
           istatus=ierr_3
           idummy=printflog(3,'Failed to write to '//file_corr_to_post//'&')
           idummy=printerr(file_corr_to_post)
           return
       endif

       return
       end

	subroutine disk_to_post_proc(
     *  c1_frm,c2_frm,
     *  sam_ac2,ct_prof_len,l1_ct_pro,l2_ct_pro,
     *  v_filler,
     *  ct_profile,
     *  alpha1,
     *  alpha2,alpha3,
     *  ipage,ipage_start,file_corr_to_post,variance,istatus,rlocal_mean)
C*****WAYNE******
       include 'ssp2_const.inc'
       integer*4     sam_ac2
       integer     ct_prof_len
       integer     l1_ct_pro
       integer     l2_ct_pro
       integer*4     v_filler
       real*8     alpha1
       real*8     alpha2
       real*8     alpha3
       real*8     c1_frm(2,2)
       real*8     c2_frm(2,2)
       real*8     ct_profile(sam_ac)
       integer    ipage,ipage_start
       character*60 file_corr_to_post
       real*8     variance
       real*8     rlocal_mean
C*****WAYNE******
       integer              ifd,bytes,copen_r,cread_seq

       ifd            =copen_r(file_corr_to_post)
       if(ifd .le. 0)then
        istatus=ierr_2
        idummy=printflog(3,'Failed to open '//file_corr_to_post//'&')
        idummy=printerr(file_corr_to_post)
        return
       endif
       bytes          =4
       nbyte          =cread_seq(ifd,ipage        ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_post//'&')
        idummy=printerr(file_corr_to_post)
        return
       endif
       nbyte          =cread_seq(ifd,ipage_start  ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_post//'&')
        idummy=printerr(file_corr_to_post)
        return
       endif
       nbyte          =cread_seq(ifd,sam_ac2      ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_post//'&')
        idummy=printerr(file_corr_to_post)
        return
       endif
       nbyte          =cread_seq(ifd,ct_prof_len  ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_post//'&')
        idummy=printerr(file_corr_to_post)
        return
       endif
       nbyte          =cread_seq(ifd,l1_ct_pro    ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_post//'&')
        idummy=printerr(file_corr_to_post)
        return
       endif
       nbyte          =cread_seq(ifd,l2_ct_pro    ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_post//'&')
        idummy=printerr(file_corr_to_post)
        return
       endif
       nbyte          =cread_seq(ifd,v_filler     ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_post//'&')
        idummy=printerr(file_corr_to_post)
        return
       endif
       bytes          =8
       nbyte          =cread_seq(ifd,alpha1       ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_post//'&')
        idummy=printerr(file_corr_to_post)
        return
       endif
       nbyte          =cread_seq(ifd,alpha2       ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_post//'&')
        idummy=printerr(file_corr_to_post)
        return
       endif
       nbyte          =cread_seq(ifd,alpha3       ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_post//'&')
        idummy=printerr(file_corr_to_post)
        return
       endif
       nbyte          =cread_seq(ifd,variance     ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_post//'&')
        idummy=printerr(file_corr_to_post)
        return
       endif
       bytes          =2*2*8
       nbyte          =cread_seq(ifd,c1_frm       ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_post//'&')
        idummy=printerr(file_corr_to_post)
        return
       endif
       nbyte          =cread_seq(ifd,c2_frm       ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_post//'&')
        idummy=printerr(file_corr_to_post)
        return
       endif
       bytes          =sam_ac*8
       nbyte          =cread_seq(ifd,ct_profile   ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_post//'&')
        idummy=printerr(file_corr_to_post)
        return
       endif
       bytes          =8
       nbyte          =cread_seq(ifd,rlocal_mean   ,bytes)
       if (nbyte .ne. bytes) then
        istatus=ierr_4
        idummy=printflog(3,'Failed to read from '//file_corr_to_post//'&')
        idummy=printerr(file_corr_to_post)
        return
       endif

       return
       end
