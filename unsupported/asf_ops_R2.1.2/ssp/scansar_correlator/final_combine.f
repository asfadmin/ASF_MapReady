c SccsId = @(#)final_combine.f	2.41 3/24/98
        subroutine inode_has_page(curr_ipage,curr_ipage_start,iii,ipage_from_disk)
        integer curr_ipage,curr_ipage_start,iii,i,ipage_from_disk

        ipage_from_disk= -1
        do i=curr_ipage_start,curr_ipage_start+curr_ipage-1
           if (i.eq.iii) then
             ipage_from_disk=i-curr_ipage_start+1
           endif
        enddo
        return
        end
        subroutine image_combine(overlay_image,overlay_mask,buff6,buff7,sam_ac2,inew_page)
         include 'ssp2_const.inc'
         real*8    overlay_image(sam_ac,sam_ac1_1page)
         byte     overlay_mask(sam_ac,sam_ac1_1page)
         real*8    buff6(sam_ac,sam_ac1_1page)
         byte     buff7(sam_ac,sam_ac1_1page)
         integer  sam_ac2,inew_page

           if (inew_page.eq.1) then
              inew_page=0
              do i=1,sam_ac1_1page
              do j=1,sam_ac2
                overlay_mask (j,i)=  buff7(j,i)
                overlay_image(j,i)=  buff6(j,i)
              enddo
              enddo
           else
              do i=1,sam_ac1_1page
              do j=1,sam_ac2
                overlay_mask (j,i)=  overlay_mask(j,i) +buff7(j,i)
                overlay_image(j,i)=  overlay_image(j,i)+buff6(j,i)
              enddo
              enddo
           endif

        return
        end

        subroutine image_combine_dump(overlay_image,overlay_mask,sam_ac2,file_image_dump,
     *                            iii,norm_look_mode,v_filler,
     *                            ct_profile,c1_pxl,c2_pxl,
     *                            fm_sizec2_in,ct_prof_len,l1_ct_pro,
     *                            l2_ct_pro,istatus,file_mask_dump)
         include 'ssp2_const.inc'
         integer     ct_prof_len
         integer     l1_ct_pro
         integer     l2_ct_pro
         real*8     c1_pxl
         real*8     c2_pxl
         real*8     ct_profile(sam_ac)
         real*8   overlay_image(sam_ac,sam_ac1_1page)
         byte     overlay_mask(sam_ac,sam_ac1_1page)
         character*60     file_image_dump,file_mask_dump
         integer sam_ac2,iii,norm_look_mode,v_filler,nlooks
         real*8     fm_sizec2_in
         integer  ipage,dk_ptr,dk_ptr0_data,dk_ptr0_mask,bytes_data,bytes_mask
         real     overlay_image_temp(sam_ac,sam_ac1_1page)

         ipage = iii
         dk_ptr0_data= ipage*512*sam_ac2*4
         dk_ptr0_mask= ipage*512*sam_ac2
         bytes_data  = sam_ac2*4
         bytes_mask  = sam_ac2

         write(6,*)'v_filler ', v_filler
         write(6,*)'norm_look_mode ', norm_look_mode

          

         do i=1,sam_ac1_1page
         do j=1,sam_ac2
            nlooks= overlay_mask(j,i)
            if (nlooks.ne.0) then
              if (norm_look_mode.eq.1) then
c               overlay_image_temp(j,i)=  overlay_image(j,i)/(1.0*nlooks)
c               overlay_image_temp(j,i)=  sqrt(overlay_image_temp(j,i))
                overlay_image_temp(j,i)=  sqrt(overlay_image(j,i)/nlooks)
              else
                overlay_image_temp(j,i)=  overlay_image(j,i)/2.0
                overlay_image_temp(j,i)=  sqrt(overlay_image_temp(j,i))
              endif 
            else
c             overlay_image_temp(j,i) = v_filler
              overlay_image_temp(j,i) = v_filler
            endif
         enddo    
         iline = (ipage*512)+i
         if(iline.gt.l1_ct_pro.and.iline.lt.l2_ct_pro)
     *     call  gen_ct_profile(i,
     *                          overlay_mask,sam_ac2,fm_sizec2_in,c2_pxl,
     *                          overlay_image_temp,ct_prof_len,ct_profile)
c        DUMPING IMAGE
         dk_ptr = dk_ptr0_data+(i-1)*sam_ac2*4
         call write_disk(file_image_dump,overlay_image_temp(1,i),bytes_data,dk_ptr,istatus)
         if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
c        dk_ptr = dk_ptr0_mask+(i-1)*sam_ac2
c        call write_disk(file_mask_dump,overlay_mask(1,i),bytes_mask,dk_ptr,istatus)
c        if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return

         enddo    

          
         
        
         
        return
        end


        subroutine gen_ct_profile(j,
     *  b_mask,sam_ac2,fm_sizec2_in,c2_pxl,
     *  b_data,ct_prof_len,ct_profile)
C*****WAYNE******
       include 'ssp2_const.inc'
       byte     b_mask(sam_ac,sam_ac1_1page)
       integer*4     sam_ac2
       real*8     fm_sizec2_in
       real*8     c2_pxl
       real    b_data(sam_ac,sam_ac1_1page)
       real*8     ct_profile(sam_ac)
       integer   ct_prof_len
C*****WAYNE******
        integer ist,iend

        ist = 0

        do k = 1, sam_ac2/8                     !search 1st point
           nlooks = b_mask(k,j)
           if(nlooks.ne.0) then
           ist = k
           go to 1111
           end if
        end do
1111    iend = ist+fm_sizec2_in/c2_pxl-2

        if(ist.ne.0) then
          if(c1_pxl.eq.100) then
           ct_prof_len = iend-ist-1
           do i = 1, ct_prof_len
           ct_profile(i)=ct_profile(i)+b_data(i+ist,j)
           end do
          else
           ct_prof_len = (iend-ist)/2-2
           do i = 1, ct_prof_len
           ct_profile(i)=ct_profile(i)+b_data(i*2+ist,j)
           end do
          end if
        end if



        return
        end


