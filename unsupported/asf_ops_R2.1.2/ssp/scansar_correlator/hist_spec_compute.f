c SccsId = @(#)hist_spec_compute.f	2.41 3/24/98
c------------------------------------------
c  This subroutine initializes the raw data histogram arrays
c  and the maximum receiver gain.
c------------------------------------------



      subroutine rhist_setup(gain_rec,burst_start,burst_end,
     *aux_agc_cnt,aux_agc_np,aux_agc_chg,np_v,hist_value,
     *hist_i,hist_q,maxgain)

      implicit none
      include 'ssp2_dim.inc'
      integer burst_start,burst_end
      include 'ssp2_const.inc'
      real*8 gain_rec(burst_max)
      integer np_v(burst_max)
      real*4 gain,maxgain
      integer hist_i(256),hist_q(256)
      real*4 hist_value(256)
      integer kb,i,j,jstart,jend,n
      integer aux_agc_cnt(1600)
      integer aux_agc_np(max_agc,1600)
      real*8 aux_agc_chg(max_agc,1600)
      real*4 gain_c
      integer pulses

      do 100 i=1,256
      hist_i(i)=0
      hist_q(i)=0
      hist_value(i)=float(i-129)    ! JMS 4-3-96 changed 1 to 129
  100 continue

      maxgain=0.0
c  No gain change during burst

      do 200 i=burst_start,burst_end
      gain=gain_rec(i)
      if (abs(maxgain).lt.abs(gain)) maxgain=gain
  200 continue

c  Gain change during burst
      do kb=burst_start,burst_end
      if (aux_agc_cnt(kb).gt.1) then                ! JMS 3-29-96
        pulses=np_v(kb)
        n=aux_agc_cnt(kb)
        do i=(aux_agc_np(n,kb)-aux_agc_np(1,kb)+1),pulses
          gain_c=sqrt(10.0**(aux_agc_chg(n,kb)/10.0))
          if (abs(gain_c).gt.abs(maxgain)) maxgain=gain_c
        enddo
        do i=1,(n-1)
          jstart=(aux_agc_np(i,kb)-aux_agc_np(1,kb))+1
          jend=aux_agc_np((i+1),kb)-aux_agc_np(1,kb)
          do j=jstart,jend
            gain_c=sqrt(10.0**(aux_agc_chg(i,kb)/10.0))
          if (abs(gain_c).gt.abs(maxgain)) maxgain=gain_c
          enddo
        enddo
      endif
      enddo

      return
      end


c------------------------------------------
c	Generate Histogram for Raw Echo data 
c	in 8I/8Q form
c------------------------------------------

        subroutine decode_hist(kb,in,pulses,samples,gain,maxgain,
     *  aux_agc_cnt,aux_agc_np,aux_agc_chg,tbl,hist_i,hist_q)

        implicit none

c       --------------
c       INPUT PARAMETERS
c       --------------
        include 'ssp2_dim.inc'

        integer samples         !Samples per pulse
        byte    in(2*10240,128) !4I/4Q Raw data in
        integer pulses          !number of pulses
        real*4  gain            !??? apply to whole burst ??Receiver
        real*4  maxgain
        complex tbl( -128:127 ) !Lookup table
        integer kb
        integer aux_agc_cnt(1600)
        integer aux_agc_np(max_agc,1600)
        real*8 aux_agc_chg(max_agc,1600)


c       --------------
c       OUTPUT PARAMETERS
c       --------------
        integer hist_i(256),hist_q(256)

c       --------------
c       LOCAL VARIABLES
c       --------------
        integer k, l, m, t,i,ihi,ihq
        integer i_tbl(-128:127)
        integer q_tbl(-128:127)
        real*4 ti,tq,gain_c(128)
        integer n,jstart,jend,j

        if (aux_agc_cnt(kb).le.1) then
c  No gain change during burst

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

c          -------------
c          FOR EACH PULSE
c          -------------
           do k = 1, samples
              m = in( k,i )
              ihi=i_tbl(m)+129         ! histogram index from 1 to 256
              ihq=q_tbl(m)+129         ! histogram index from 1 to 256
              hist_i(ihi)=hist_i(ihi)+1
              hist_q(ihq)=hist_q(ihq)+1
           end do

        end do

        else
c  Gain change during burst

        n=aux_agc_cnt(kb)
        do i=(aux_agc_np(n,kb)-aux_agc_np(1,kb)+1),pulses
          gain_c(i)=sqrt(10.0**(aux_agc_chg(n,kb)/10.0))
        enddo
        do i=1,(n-1)
          jstart=(aux_agc_np(i,kb)-aux_agc_np(1,kb))+1
          jend=aux_agc_np((i+1),kb)-aux_agc_np(1,kb)
          do j=jstart,jend
            gain_c(j)=sqrt(10.0**(aux_agc_chg(i,kb)/10.0))
          enddo
        enddo

        do i = 1, pulses

c  Produce table of 8I-8Q AGC adjusted values
        do j = -128,127
          ti=real(tbl(j))
          tq=aimag(tbl(j))
          i_tbl(j)=int(ti*(gain_c(i)*127.0)/(maxgain*7.5))
          if (i_tbl(j).gt.127) i_tbl(j)=127
          if (i_tbl(j).lt.-128) i_tbl(j)=-128
          q_tbl(j)=int(tq*(gain_c(i)*127.0)/(maxgain*7.5))
          if (q_tbl(j).gt.127) q_tbl(j)=127
          if (q_tbl(j).lt.-128) q_tbl(j)=-128
        end do


c          -------------
c          FOR EACH PULSE
c          -------------
           do k = 1, samples
              m = in( k,i )
              ihi=i_tbl(m)+129         ! histogram index from 1 to 256
              ihq=q_tbl(m)+129         ! histogram index from 1 to 256
              hist_i(ihi)=hist_i(ihi)+1
              hist_q(ihq)=hist_q(ihq)+1
           end do

        end do

        endif

        return
      end



	subroutine get_spec(kburst,buff1,ns,np,np_v,buff_spec)

	implicit none

	include 'ssp2_const.inc'
c	----------------
c	INPUT PARAMETERS
c	----------------
	complex buff1(sam_raw,line_raw)
	integer*4	n_fft
	integer*4	nvals
	integer*4	np(burst_max)
	integer*4	ns(burst_max)
	integer*4       np_v(burst_max)
	integer*4       kburst

c	----------------
c	OUTPUT PARAMETERS
c	----------------
	complex  	buff_spec(max_fft_size)

c	----------------
c	LOCAL VARIABLES
c	----------------
	complex		buff_t(max_fft_size)
	integer*4	i
	integer*4	np_index

        n_fft=4096
	call init_fft(n_fft)

c	CLEAR BUFFERS

        do i = 1, n_fft
           buff_t(i) = cmplx(0., 0.)
        enddo

c	TAKE CFFT & ACCUMULATION THEM

        do np_index =(np(kburst)-np_v(kburst)+1),np(kburst)
           nvals=ns(kburst)
           if (nvals.gt.n_fft) nvals=n_fft
           do i = 1, nvals
              buff_t(i) = buff1(i, np_index)
           enddo
           call cfft(buff_t,1,n_fft,1)
           do i = 1, n_fft
              buff_spec(i) = (buff_t(i)/float(n_fft))+buff_spec(i)
           enddo
        enddo


	return
	end


        subroutine init_spec(buff_spec)
        implicit none
	include 'ssp2_const.inc'
        complex buff_spec(max_fft_size)
        integer i

        do 100 i=1,max_fft_size
        buff_spec(i)=cmplx(0.0,0.0)
  100   continue

        return
        end

