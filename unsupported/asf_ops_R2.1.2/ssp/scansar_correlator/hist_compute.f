c SccsId = @(#)hist_compute.f	2.41 3/24/98
      	subroutine decode_hist(in,pulses,samples,gain,maxgain,
     *                         tbl,hist_i,hist_q)

	implicit none

c	--------------
c	INPUT PARAMETERS
c	--------------
        integer samples		!Samples per pulse
        byte 	in(2*10240,128)	!4I/4Q Raw data in 
        integer pulses		!number of pulses
        real*4  gain		!??? apply to whole burst ??Receiver gain
        real*4  maxgain	
        complex tbl( -128:127 )	!Lookup table

c	--------------
c	OUTPUT PARAMETERS
c	--------------
        integer hist_i(256),hist_q(256)

c	--------------
c	LOCAL VARIABLES
c	--------------
	integer k, l, m, t,i,ihi,ihq
        integer i_tbl(-128:127)
        integer q_tbl(-128:127)
        real*4 ti,tq

c  Produce table of 8I-8Q AGC adjusted values
        do i = -128,127
          ti=real(tbl(i))
          tq=aimag(tbl(i))
          i_tbl(i)=int(ti*(gain*127.0)/(maxgain*7.5))
          if (i_tbl(i).gt.127) i_tbl(i)=127
          if (i_tbl(i).lt.-128) i_tbl(i)=-128
          q_tbl(i)=int(tq*(gain*127.0)/(maxgain*7.5))
          if (q_tbl(i).gt.127) q_tbl(i)=127
          if (q_tbl(i).lt.-128) q_tbl(i)=-128
        end do

	do i = 1, pulses

	   write (*,*) 'np',i
c	   -------------
c	   FOR EACH PULSE
c	   -------------
	   do k = 1, samples
	      m = in( k,i )
              ihi=i_tbl(m)+129         ! histogram index from 1 to 256
              ihq=q_tbl(m)+129         ! histogram index from 1 to 256
              hist_i(ihi)=hist_i(ihi)+1
              hist_q(ihq)=hist_q(ihq)+1
	   end do

	end do

	return
      end



c  This subroutine initializes the raw data histogram arrays
c  and the maximum receiver gain.

      subroutine rhist_setup(gain_rec,burst_start,burst_end,
     *           hist_i,hist_q,maxgain)

      integer burst_start,burst_end
      include 'ssp2_const.inc'
      real*8 gain_rec(burst_max)
      real*4 gain,maxgain
      integer hist_i(256),hist_q(256)

      do 100 i=1,256
      hist_i(i)=0
      hist_q(i)=0
  100 continue

      maxgain=0.0

      do 200 i=burst_start,burst_end
      gain=gain_rec(i)
      if (abs(maxgain).lt.abs(gain)) maxgain=gain
  200 continue

      return
      end

