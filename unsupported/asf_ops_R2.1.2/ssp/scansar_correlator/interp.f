c SccsId = @(#)interp.f	2.41 3/24/98
         subroutine set_patn_zero(ant_pat_gain,imode)
         include 'ssp2_const.inc'
            real ant_pat_gain(ns_ant_rg,7)
            integer imode
            write(6,*)'SETTING MODE # ',imode,' TO 0s'
            do i=1,ns_ant_rg
             ant_pat_gain(i,imode)= 0.0
            enddo
           
         return
         end
         subroutine do_patn_inter(ant_pat_gain,new_elev_1st,new_elev_inc,imode,
     *                            elev_1st,elev_inc,gain,num,file_out)
         include 'ssp2_const.inc'
            real ant_pat_gain(ns_ant_rg,7)
            real*8 gain(ns_ant_rg)
            real*8 elev_1st,elev_inc
            real*8 new_elev_1st,new_elev_inc
            real ang_act
            integer imode,num
            integer index,ilower,iupper
            real  delta
            character*60 file_out
            real x,y
            integer iunit
            integer itotal_interp
            real min_gain,max_gain
            integer index_min,index_max
            iunit= 8
c           open(unit=iunit,file=file_out,status='OLD')
c
            write(6,*)'PERFORM INTERPOLATION ON MODE # ',imode
            min_gain= gain(1)
            index_min= 1
            max_gain= gain(1)
            index_max= 1
            do i=1,num
             if (gain(i).le.min_gain) then
                min_gain= gain(i) 
                index_min= i
             endif
             if (gain(i).ge.max_gain) then
                max_gain= gain(i) 
                index_max= i
             endif
            enddo
c           write(6,*)index_min,index_max
            write(6,*)'min ',gain(index_min), elev_1st+(index_min-1)*elev_inc
            write(6,*)'max ',gain(index_max), elev_1st+(index_max-1)*elev_inc
            

            itotal_interp=0
            do i=1,ns_ant_rg
             x= elev_1st+(i-1)*elev_inc
             y= gain(i)
             ang_act = new_elev_1st + (i-1)*new_elev_inc
             index = int ((ang_act - elev_1st) / elev_inc )
             delta = ( (ang_act -elev_1st)  - index*elev_inc ) / elev_inc
             if ((index.lt.0).or.(index.gt.(num-1))) then
c              ant_pat_gain(i,imode)=0.0
               ant_pat_gain(i,imode)=min_gain
             else
              ilower= index+1
              iupper= ilower+1
              if (delta.ne.0) then
                if (ilower.eq.num) then
c                ant_pat_gain(i,imode)=0.0
                 ant_pat_gain(i,imode)=min_gain
                else
                 ant_pat_gain(i,imode)=(gain(iupper)-gain(ilower))*delta+ gain(ilower)
                 itotal_interp=itotal_interp+1
                endif
              else
                ant_pat_gain(i,imode)= gain(ilower)
                itotal_interp=itotal_interp+1
              endif
             endif
c               write(iunit,*)ang_act, ant_pat_gain(i,imode)
c               write(iunit,*)x,y
            enddo
            write(6,*)'before interplation points ',num,' after interplation points ',itotal_interp
         return
         end
         subroutine do_patn_az_inter(ant_pat_az_gain,new_elev_1st,new_elev_inc,
     *                            elev_1st,elev_inc,gain,num,file_out)
         include 'ssp2_const.inc'
            real ant_pat_az_gain(ns_ant_az)
            real*8 gain(ns_ant_rg)
            real*8 elev_1st,elev_inc
            real*8 new_elev_1st,new_elev_inc
            real ang_act
            integer num
            integer index,ilower,iupper
            real  delta
            character*60 file_out
            real x,y
            integer iunit
            integer itotal_interp
            real min_gain,max_gain
            integer index_min,index_max
            iunit= 8
c           open(unit=iunit,file=file_out,status='OLD')
c
            write(6,*)'PERFORM INTERPOLATION ON MODE AZ '
            min_gain= gain(1)
            index_min= 1
            max_gain= gain(1)
            index_max= 1
            do i=1,num
             if (gain(i).le.min_gain) then
                min_gain= gain(i) 
                index_min= i
             endif
             if (gain(i).ge.max_gain) then
                max_gain= gain(i) 
                index_max= i
             endif
            enddo
c           write(6,*)index_min,index_max
            write(6,*)'min ',gain(index_min), elev_1st+(index_min-1)*elev_inc
            write(6,*)'max ',gain(index_max), elev_1st+(index_max-1)*elev_inc
            

            itotal_interp=0
            do i=1,ns_ant_rg
             x= elev_1st+(i-1)*elev_inc
             y= gain(i)
             ang_act = new_elev_1st + (i-1)*new_elev_inc
             index = int ((ang_act - elev_1st) / elev_inc )
             delta = ( (ang_act -elev_1st)  - index*elev_inc ) / elev_inc
             if ((index.lt.0).or.(index.gt.(num-1))) then
c              ant_pat_az_gain(i)=0.0
               ant_pat_az_gain(i)=min_gain
             else
              ilower= index+1
              iupper= ilower+1
              if (delta.ne.0) then
                if (ilower.eq.num) then
c                ant_pat_az_gain(i)=0.0
                 ant_pat_az_gain(i)=min_gain
                else
                 ant_pat_az_gain(i)=(gain(iupper)-gain(ilower))*delta+ gain(ilower)
                 itotal_interp=itotal_interp+1
                endif
              else
                ant_pat_az_gain(i)= gain(ilower)
                itotal_interp=itotal_interp+1
              endif
             endif
c               write(iunit,*)ang_act, ant_pat_az_gain(i)
c               write(iunit,*)x,y
            enddo
            write(6,*)'before interplation points ',num,' after interplation points ',itotal_interp
         return
         end
