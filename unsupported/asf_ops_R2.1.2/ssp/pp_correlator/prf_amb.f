c SccsId = @(#)prf_amb.f	2.41 3/24/98


	subroutine prf_amb(start_sample,start_line,raw_file_fd,offset,
     +  fac_reduce,
     +  window_pos_changes, window_pos_values, agc_changes, agc_values,
     +  rng_in,az_in,nadd,n_max,pi,c,pxl_unit,
     +  ers,jers,rsat_s12,rsat_s37,rsat_ws,rsat_fr,rsat_eh,rsat_el,
     +  buff_in,array,buff_in_rd,
     +  mode,
     +  raw_mode,i_dop_mode,nband,nlines,line_offset,
     +  ns,nr_header,ns_header,i_squint,i_pt_sim,i_data_type,nlook,
     +  nfr,nfx,icomplex,i_preproc,i_iqqi,nx_offset,i_dummy,
     +  lambda,fc,bw,dmu,fs,plen,k_chirp,dr,
     +  prf,prf_in,pbw,az_ang,az_res,df,
     +  t_1st_echo,t_eph,t_gha,gha,xs,ys,zs,rsc,vxs,vys,vzs,vs,
     +  re_mean,h,
     +  rs_ref,r0_base,fd0_base,fr0_base,fd0,fr0,fdd,frd,
     +  fd_const,r_ref,r_1st,
     +  pxl_spacing,nx_spacing,
     +  t_start_frm,t_end_frm,
     +  lat1,lon1,rs_crn1,rg_crn1,lookang1,
     +  lat2,lon2,rs_crn2,rg_crn2,lookang2,
     +  lat3,lon3,rs_crn3,rg_crn3,lookang3,
     +  lat4,lon4,rs_crn4,rg_crn4,lookang4,
     +  lat5,lon5,rs_crn5,rg_crn5,lookang5,
     +  raw_file,image_file,
     +  n_prf,fd_pred)


        implicit none

        character*128 SccsId_decode
        data SccsId_decode
     +  /'@(#)PPprf_amb.f:2.41'/

	include '/usr/lpp/ppe.poe/include/mpif.h'

        integer ierror

        real*8  fd_pred             !The fd_predict value
        integer n_prf               !The ambiguity prf number

        integer fac_reduce          !Factor reduce in range spectrum
        integer rng_in              !# rng samples procesed each block
	integer az_in               !# az samples procesed each block
	integer nadd                !extra az spectr sampls for fd drift 
	integer n_max               !n_max = max(rng_in,az_in)
	real*8 pi		    !constant pai
	real*8 c                    !speed of light
	real*8 pxl_unit		    !pxl_unit = 6.25 meter
	integer ers
	integer jers
	integer rsat_s12
	integer rsat_s37
	integer rsat_ws
	integer rsat_fr
	integer rsat_eh
	integer rsat_el
	complex buff_in(rng_in,az_in+nadd),array(n_max+nadd)
	complex buff_in_rd(rng_in,az_in)
	integer mode                !airsar (1) or spaceborne sar (2) modes
	integer raw_mode            !raw data in real(1) or complex(2)
	integer i_dop_mode	    !process (1) zero dop,(2)const,(3) centroid
	integer nband		    !# of frequency samples processed
	integer nlines              !total # of az. lines to be procsed
	integer line_offset	    !processing start at line_offset+1-th line
	integer ns                  !total # of complex samples per echo
	integer nr_header	    !# of samples in header of each echo pulse
	integer ns_header           !total # of complex samples with header
	integer i_squint	    !1 = squint mode, 0 = none squint
	integer i_pt_sim	    !point target simulation
	integer i_data_type	    !processing data type
	integer nlook               !total # of azimuth looks
	integer nfr		    !image frame size in range
	integer nfx		    !image frame size inazimuth
	integer icomplex	    !1 = output complex image, 0 = real image
	integer i_preproc    	    !1 = preprocessing on going
	integer i_iqqi		    !0 = i first, 1 = q first
	integer nx_offset	    !azimuth offset to adjust 1st image pixel
	integer i_dummy
	real*8 lambda               !wave length 
	real*8 fc                   !radar center frequency (carrier)
	real*8 bw                   !range bandwidth
	real*8 dmu                  !range freq. sample spacing in spectrum
	real*8 fs                   !range sampling frequency
	real*8 plen                 !time interval of radar pulse
	real*8 k_chirp              !radar pulse frequency rate
	real*8 dr                   !range sampling spacing
	real*8 prf                  !pulse repetition frequency 
	real*8 prf_in               !pulse repetition frequency input
	real*8 pbw                  !the processing bandwidth
	real*8 az_ang               !azimuth beam angle
	real*8 az_res               !image azimuth resolution
	real*8 df                   !doppler sample spacing
	real*8 t_1st_echo 	    !time associated with first echo pulse
	real*8 t_eph		    !time associated with given ephemeris
	real*8 t_gha		    !time associated with gha
	real*8 gha		    !longitude reference
	real*8 xs		    !sensor position, x-dimension	
	real*8 ys		    !sensor position, y-dimension	
	real*8 zs		    !sensor position, z-dimension	
	real*8 rsc		    !sensor to earth center distance
	real*8 vxs                  !spacecraft speed in x
	real*8 vys                  !spacecraft speed in y
	real*8 vzs                  !spacecraft speed in z
	real*8 vs                   !spacecraft speed
	real*8 re_mean		    !mean earth radius at local
	real*8 h		    !local mean elevation above ellipsoid surface
	real*8 rs_ref               !slant rng of the 1st range sample in frame
	real*8 r0_base              !slant rng of the 1st input range sample
	real*8 fd0_base             !doppler centroid of the 1st rng sample
	real*8 fr0_base             !doppler rate of the 1st rng sample
	real*8 fd0                  !dop cent of 1st rng sample in block
	real*8 fr0                  !dop rate of 1st rng sample in block
	real*8 fdd                  !fd change per meter in cross-track
	real*8 frd                  !fr change per meter in cross-track
	real*8 fd_const
	real*8 r_ref                !range reference for chirp scaling
	real*8 r_1st                !slant rng of 1st input range sample in block
	real*8 pxl_spacing          !image pixel spacing for ground projection
	real*8 nx_spacing	    !nature spacing in azimuth
	real*8 t_start_frm 	    !time associated with first image frame line
	real*8 t_end_frm 	    !time associated with last image frame line
	real*8 lat1		    !lat of frame corner 1, near range 1st image line
	real*8 lon1		    !lat of frame corner 1, near range 1st image line
	real*8 rs_crn1		    !slant range of frame corner 1, near range 1st image line
	real*8 rg_crn1		    !ground range of frame corner 1, near range 1st image line
	real*8 lookang1		    !look angle of frame corner 1, near range 1st image line
	real*8 lat2		    !lat of frame corner 2, far range 1st image line
	real*8 lon2		    !lat of frame corner 2, far range 1st image line
	real*8 rs_crn2		    !slant range of frame corner 2, near range 1st image line
	real*8 rg_crn2		    !ground range of frame corner 2, near range 1st image line
	real*8 lookang2		    !look angle of frame corner 2, near range 1st image line
	real*8 lat3		    !lat of frame corner 3, near range last image line
	real*8 lon3		    !lat of frame corner 3, near range last image line
	real*8 rs_crn3		    !slant range of frame corner 3, near range last image line
	real*8 rg_crn3		    !ground range of frame corner 3, near range last image line
	real*8 lookang3		    !look angle of frame corner 3, near range last image line
	real*8 lat4		    !lat of frame corner 4, far range last image line
	real*8 lon4		    !lat of frame corner 4, far range last image line
	real*8 rs_crn4		    !slant range of frame corner 4, near range last image line
	real*8 rg_crn4		    !ground range of frame corner 4, near range last image line
	real*8 lookang4		    !look angle of frame corner 4, near range last image line
	real*8 lat5		    !lat of frame center
	real*8 lon5		    !lat of frame center
	real*8 rs_crn5		    !slant range of frame center
	real*8 rg_crn5		    !ground range of frame center
	real*8 lookang5		    !look angle of frame center
	character*64 raw_file	    !raw data file name
	character*64 image_file	    !image data file name

	integer	start_line	! starting azimuth line
	integer	start_sample	! starting range sample
	integer offset, window_pos_changes(2), agc_changes(nlines+1)
	real*8 window_pos_values(2), agc_values(nlines+1)

	byte 	b_data(16384)
	complex	fun1,fun2,temp,fun5

	real*4	r_value
	integer raw_file_fd
	integer	n_byte,r_byte,disk_ptr
	integer	nfft,samples,lines
	integer	i,j,k,iflag,fac,factor

        integer nside,noffset
	integer jump, jump1, jump2, counter
	real*8  agc_factor

c qdn 2/6/98
        integer nref, nr_total, nblock, nr_blk,  nskip
        integer m, kp, k_p_p, ist, iend
        integer nfft_real
        integer nst1,nst2
        integer spect_fd,idum,initdk
        integer nonzero1,nonzero2


        real*8  xdmu,xfc,xbw
        real*8  mu,phase,sum1,sum2
        real*8  fd_mean, fd_diff, fd_cnt

        real*8  ang1(256),ang2(256)
        real*8  fd1_raw(256),fd2_raw(256)

        complex c_data(16384),d_data(16384),rngref(16384)
        complex a(2,8192)
        complex*16 acc1(256),acc2(256)
        complex*16 temp1,temp2

        real*8     mspect(16384)

c qdn 3/3/98
        real*8 q_mean,q_diff,q_pred,q_nprf,q_fd
        real*8 q_sum_mean,q_sum_diff

	if(rng_in/2 .gt. 8192) then
	   print *,"decode: rng_in/2 > 8192. increase c_data,b_data size"
	   call mpi_abort(mpi_comm_world,22,ierror)
           call exit(22)	   
	endif
   
	
	jump = 0
	jump1 = 0
	jump2 = 0

c Generate the range reference function (without 2nd compression)
   
        nref = plen*fs       ! number of samples in range reference
        nr_total = ns - nref ! numbe of total range cells processed
        nblock = 32          ! 16,32
        nr_blk =  (nr_total/2)/nblock

        nfft = 8192
        nskip = nfft/2
       

        xbw = 0.8*fs
        xfc = c / lambda
        xdmu = fs/nfft
        do i=1,nfft
           mu = (i-1)*xdmu
           if(mu.ge.fs/2) mu = mu-fs
           phase = pi/k_chirp*mu**2 + 2*pi*mu*plen/2.
           rngref(i) = cmplx(cos(phase),sin(phase))  ! generate range reference
        end do
        rngref(1) = cmplx(0.0,0.0)
        print*,'xfc=',xfc,' bw=',bw,' xdmu=',xdmu
        print*,'fs=',fs,'pi=',pi,'k_chirp=',k_chirp
        print*,'The rngref=',(rngref(i),i=1,5)

c Accumulate the range spectrum
        do i=1,nfft
           mspect(i)   =  0.0
        end do

c qdn 3/5/98: using only 0.8 of the spectrum
        nonzero1 = (nfft/2)*0.8
        nonzero2 = nfft - nonzero1 
        do i=nonzero1+1,nonzero2
          rngref(i)=cmplx(0.0,0.0)
        end do
           
c ns which is number of samples in each pulse
        samples = ns
	if(start_sample+samples .gt. ns) then
	   samples = ns-start_sample
	end if
        print*,' samples=',samples

c nlines is number of pulses
	lines = 8192   ! for PP it should be 8192 
	if(start_line+lines .gt. nlines) then
	   lines = nlines-start_line
	end if

	if(i_data_type.eq.jers.or.i_data_type.eq.ers) then
	   factor = 2
	else
	   factor = 1
	endif

	n_byte = factor*samples  

c*********** window position changes ***************************
	if(window_pos_changes(2) <= 0) then
	   !no window position change
	   jump1 = 0
	   jump2 = 0
	else
	   !there is a change in window position
	   if(window_pos_values(2) > window_pos_values(1)) then
	      jump1 = int(abs(window_pos_values(2) -
     +	                      window_pos_values(1))*fs)
	      jump2 = 0
	   else
	      jump1 = 0
	      jump2 = int(abs(window_pos_values(2) -
     +	                      window_pos_values(1))*fs)
	   end if
	end if
c*********************************************************

	counter = 1

c Initialize the buffer to zero
        do m=1,nblock
           acc1(m) = cmplx(0.0,0.0)
           acc2(m) = cmplx(0.0,0.0)
        end do
        k_p_p = 1

c Start the do loop for each range line

	do j = 1, lines

c*********** compute the buffer pointers ********************
           if(k_p_p.eq.1) then
               k_p_p = 2
               kp = 1
           else
               k_p_p = 1
               kp = 2
           end if 

c*********** window position changes ***************************
	   if(start_line+j-1 .lt. window_pos_changes(2)) then
	      jump = jump1
	   else
	      jump = jump2
	   end if

c*****************AGC change for radarsat******************
 30	   continue
	   if(start_line+j-1 .ge. agc_changes(counter) .and.
     +        start_line+j-1 .lt. agc_changes(counter+1)) then
	        agc_factor = sqrt(10**(agc_values(counter)/10.0))
	   else
	      if(counter .ge. nlines) then
		 counter = 1
		 goto 40
	      else
		 counter = counter + 1
		 goto 30
	      end if
	   end if
 40	   continue

c***********************************************************
c offset is counted from the beginning of echo file to where echo data starts 
	   disk_ptr=factor*((start_line+j-2)*ns +
     +	      start_sample-1+jump) + offset

	   ! if disk_ptr+n_byte > raw data file size in bytes, we read less
	   ! bytes
	   if(disk_ptr+n_byte .gt. factor*nlines*ns+offset) then
	      n_byte = factor*nlines*ns+offset - disk_ptr
	   end if

	   call ioseek(raw_file_fd,disk_ptr)
	   call ioread(raw_file_fd,b_data,n_byte)

c          print*,' read at =',disk_ptr,' number of bytes=',n_byte

	   if(raw_mode.eq.2)  then !input is real(1) or i/q(2)

	      if(i_iqqi.eq.0) then
		 if(factor.eq.2) then
		    do i=1,samples
                       c_data(i) = fun2(b_data(2*i-1),b_data(2*i))
		    enddo
		 else
		    do i=1,samples
                       c_data(i) = fun5(b_data(i))*agc_factor
		    enddo
		 endif
	      else
		 if(factor.eq.2) then
		    do i=1,samples
                       c_data(i) = fun2(b_data(2*i),b_data(2*i-1))
		    enddo
		 else
		    do i=1,samples
                       c_data(i) = fun5(b_data(i))*agc_factor
		    enddo
		 endif
	      end if
	      
	   else             ! input is real signal, need convert to complex

	      do i=1,n_byte
		 c_data(i)=fun1(b_data(i)) !fun1 is a function
	      enddo
	      do i= n_byte+1, 2*rng_in !append zero to each line
		 c_data(i)=cmplx(0.0,0.0)
	      enddo
	      
	      nfft_real = 2*nfft
	      call init_fft(nfft_real) !convert real to i/q
	      iflag = 1
	      call cfft(c_data,1,nfft_real,iflag)
	      do k = 1, nfft/2
		 temp = c_data(k+nfft)
		 c_data(k) = c_data(k+3*nfft/2)
		 c_data(k+nfft/2) = temp
	      end do
	      call init_fft(nfft) 
	      iflag = -1
	      call cfft(c_data,1,nfft,iflag) ! take inverse FFT
	      
	   end if

           do k=samples+1,nfft
              c_data(k) = cmplx(0.0,0.0)
           end do
c          print*,' the raw data=',(c_data(k),k=1,5)
c Take forward FFT
           call init_fft(nfft)
           call cfft(c_data,1,nfft,1)
           c_data(1) = cmplx(0.0,0.0)

c Average the spectrum
           do i=1,nfft
              mspect(i) = mspect(i) + abs(c_data(i))**2.0
           end do

c Multiply with range reference function
           do k=1,nfft
               c_data(k) = c_data(k)*rngref(k)
           end do

c Copy half spectrum into d_data array
           do k=1,nskip
               d_data(k) = c_data(nskip+k)
           end do

c Circular shift
c          nst1 = bw/fs*nskip
c          nst2 = nskip - nst1
           nst1 = 0.8*nskip
           nst2 = nskip - nst1
           call crcsht(c_data,nskip,nst1)
           call crcsht(d_data,nskip,nst2)

c Take inverse FFT
           call init_fft(nskip)
           call cfft(c_data,1,nskip,-1)
           call cfft(d_data,1,nskip,-1)

           do k=1,nskip
              a(kp,k) = d_data(k)
              a(kp,k+nskip) = c_data(k)
           end do

c Compute average over two buffers

c          print*,'kp=',kp,'k_p_p=',k_p_p
c          print*,' a(kp,i)=',(a(kp,m),m=1,3)
c          print*,' a(kp_p_p,i)=',(a(kp,m),m=1,3)
c          print*,' nr_blk=',nr_blk,' nblock=',nblock

           if(j.gt.1) then
           do m=1,nblock
              ist = (m-1)*nr_blk + 1
              iend = m*nr_blk
              do i=ist,iend
                 temp1 = a(kp,i)*conjg(a(k_p_p,i))
                 acc1(m) = acc1(m) + temp1
                 temp2 = a(kp,i+nskip)*conjg(a(k_p_p,i+nskip))
                 acc2(m) = acc2(m) + temp2
               end do
           end do
c          print*,' acc1=',acc1(1),acc2(1)
           end if

        end do

c Dump out the spectrum
        do i=1,nfft
           mspect(i) = mspect(i)/nfft
        end do
c       spect_fd = initdk(idum,'/home/tmpdisk/spectrum.dat'//char(0))
c       call iowrit(spect_fd,mspect,8*nfft)
c       call closedk(idum,spect_fd)

c Computing the Doppler information
        print*,' k, ang1, ang2'
        do k=1,nblock
           ang1(k) = atan2d(aimag(acc1(k)),real(acc1(k)))
           ang2(k) = atan2d(aimag(acc2(k)),real(acc2(k)))
           fd1_raw(k) = ang1(k)*prf/360.0
           fd2_raw(k) = ang2(k)*prf/360.0
           print*,'k=',k,' ang1=',ang1(k),' ang2=',ang2(k)
        end do
        print*,' k, fd1, fd2'

        do k=1,nblock
           print*,'k=',k,'fd1=',fd1_raw(k),' fd2=',fd2_raw(k)
        end do

c Check the doppler accross prf values
        fd2_raw(1) = fd2_raw(1)-nint((fd2_raw(1)-fd1_raw(1))/prf)*prf
        do k=2,nblock
           fd1_raw(k) = fd1_raw(k) 
     *                  - nint((fd1_raw(k)-fd1_raw(k-1))/prf)*prf
           fd2_raw(k) = fd2_raw(k)
     *                  - nint((fd2_raw(k)-fd2_raw(k-1))/prf)*prf
        end do

c Printout the predict Doppler based on fd1 and fd2
        print*,'QDN k q_mean q_diff q_pred q_nprf nint(q_nprf) q_fd'
        q_sum_diff = 0.0
        q_sum_mean = 0.0
        do k=1,nblock
           q_mean = (fd2_raw(k) + fd1_raw(k))/2.0
           q_diff = (fd2_raw(k) - fd1_raw(k))
           q_pred = q_diff*2.0*xfc/xbw
           q_nprf = (q_pred-q_mean)/prf
           q_fd   = q_mean+nint(q_nprf)*prf
           q_sum_mean = q_sum_mean + q_mean
           q_sum_diff = q_sum_diff + q_diff
           print*,k,q_mean,q_diff,q_pred,q_nprf,nint(q_nprf),q_fd
        end do
        q_sum_mean = q_sum_mean/nblock
        q_sum_diff = q_sum_diff/nblock
        print*,' Average of q_diff =',q_sum_diff
        print*,' Average of q_mean =',q_sum_mean
        print*,' end of printout 2.0*xfc/xbw=',2.0*xfc/xbw


c Calculate the Doppler centroid at the mid point 
        sum1 = 0
        sum2 = 0
        do k=1,nblock
           print*,'k=',k,'fd1_raw=',fd1_raw(k),'fd2_raw=',fd2_raw(k)
           sum1 = sum1 + fd1_raw(k)
           sum2 = sum2 + fd2_raw(k)
        end do

        fd_mean = (sum1+sum2)/nblock*.5
        fd_diff = (sum2-sum1)/nblock
c       fd_pred = fd_diff*2.0*xfc/bw
c qdn 2/17/98
        fd_pred = fd_diff*2.0*xfc/xbw

        print*,'fd_mean,fd_diff=',fd_mean,fd_diff
        print*,'fd_pred=',fd_pred
        print*,'xbw/fc*.5 and its inverse=',xbw/xfc*.5,2*xfc/xbw

c Compute the n_prf  and correct the doppler centroid at the mid point
        n_prf = nint((fd_pred-fd_mean)/prf)
        fd_cnt = fd_mean+n_prf*prf

        print*,'n_prf=',n_prf, ' a_prf=',(fd_pred-fd_mean)/prf,
     *         'orig_prf=',(fd_pred-fd_mean)/prf
        print*,'Doppler centroid estimate=',fd_cnt

c Compute the slope of Doppler centroid across range dimension
        sum1 = 0.0
        sum2 = 0.0
        do k=1,nblock
           sum1 = sum1 + (fd1_raw(k)+fd2_raw(k))/2.*(k-nblock/2-0.5)
           sum2 = sum2 + (k-nblock/2-0.5)**2
        end do
        fdd = sum1/sum2

c Compute the fd_pred at the 1st range point
        fd_pred = fd_pred - fdd*(nblock/2.+.5)

c Compute the doppler information
        fd0 = fd_cnt - fdd*(nblock/2.+.5)
        fdd = fdd/(nr_blk*dr*2.0)       ! Doppler change per meters

        print*,'fd_pred=',fd_pred,' fd0=',fd0,' fdd=',fdd

	
	return
	end

        subroutine crcsht(data,nt,nst)
        implicit none
        complex  data(4096)
        integer  nt,nst

c Local variables
        complex  buf(4096)
        integer  i,i1
        
        do i=nst,nst+nt-1
        i1=i
        if(i.gt.nt) i1=i-nt
        buf(i-nst+1) = data(i1)
        end do
        do i=1,nt
           data(i) = buf(i)
        end do
       
        return
        end
	
