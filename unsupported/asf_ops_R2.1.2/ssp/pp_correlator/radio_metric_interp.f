c SccsId = @(#)radio_metric_interp.f	2.41 3/24/98
      subroutine radio_metric_interp(ant_pat_gain, 
     +   look_ang_cnt, ele_ang_ref, dang_rg, 
     +   elev_1st, elev_inc, gain, npt)

      implicit none

        character*128 SccsId_radio_metric_interp
        data SccsId_radio_metric_interp
     +  /'@(#)PPradio_metric_interp.f:2.41'/


c output
      real*4 ant_pat_gain(700)

c input
      integer npt
      real*8 look_ang_cnt, ele_ang_ref, dang_rg
      real*8 elev_1st, elev_inc
      real*8 gain(npt)

c local variables
      real*8 new_elev_1st, new_elev_inc
      real*8 new_elev
      integer index,lower,upper,i
      real*8  delta
      real*8 min_gain

      do i = 1, npt
        gain(i) = 10**(gain(i)/10)
      end do

      new_elev_1st = look_ang_cnt + ele_ang_ref
      new_elev_inc = dang_rg

      min_gain= gain(1)
      do i=1,npt
         if (gain(i).lt.min_gain) then
            min_gain= gain(i) 
         endif
      enddo

      do i = 1, 700
         ant_pat_gain(i)=min_gain
      end do

      do i=1, 700
         new_elev = new_elev_1st+(i-1)*new_elev_inc
         index = int((new_elev-elev_1st)/elev_inc)
         delta = ((new_elev-elev_1st)-index*elev_inc)/elev_inc

         lower= index+1
         upper= index+2

         if (lower.ge.1 .and. upper.le.npt) then
            ant_pat_gain(i)=(gain(upper)-gain(lower))*delta+gain(lower)
         endif
      enddo

      return
      end


