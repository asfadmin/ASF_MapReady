c SccsId = @(#)decode2.f	2.41 3/24/98


	subroutine decode2(start_sample,start_line,raw_file_fd,offset,
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
     +  raw_file,image_file)


        implicit none

        character*128 SccsId_decode
        data SccsId_decode
     +  /'@(#)PPdecode2.f:2.41'/

	include '/usr/lpp/ppe.poe/include/mpif.h'

        integer ierror

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
	complex	c_data(16384),fun1,fun2,temp,fun5
        complex c_temp(16384),xarray(16384)

	real*4	r_value
	integer raw_file_fd
	integer	n_byte,r_byte,disk_ptr
	integer	nfft,samples,lines
	integer	i,j,k,iflag,fac,factor

        integer nside,noffset
	integer jump, jump1, jump2, counter
	real*8 agc_factor

c qdn 1/12/98
        real*8  xdmu,k1,fd_ref,fr_ref,mu,mu0,phase,dphase
        real*4  alpha
        complex*8 kw(16384)

	if(rng_in/2 .gt. 8192) then
	   print *,"decode: rng_in/2 > 8192. increase c_data,b_data size"
	   call mpi_abort(mpi_comm_world,22,ierror)
           call exit(22)	   
	endif
   
	
	jump = 0
	jump1 = 0
	jump2 = 0

c Generate the reference function
        nfft = rng_in * fac_reduce
        xdmu = fs/nfft

        alpha = 2.85 / pi
        call kaiser(kw,nfft,alpha)

        call dop_ref(i_dop_mode,fd0,fdd,r_ref,fd_const,r_1st,fd_ref)
        fr_ref = fr0_base + (r_ref - r_1st) * frd

        k1 = 1./(1./k_chirp + (fd_ref/fc)**2/fr_ref)
        dphase = pi/4
        if(k1.gt.0) dphase = -1 * dphase

        do i = 1, nfft
           mu = (i-1)*xdmu
           mu0 = mu
           if(mu.ge.fs/2+fd_ref) mu = mu-fs
           if(mu0.ge.fs/2) mu0 = mu0-fs

           phase = pi/k1*(mu-fd_ref)**2+2*pi*mu0*plen/2.+dphase

           if(mu.gt.bw/2..or.mu.lt.-bw/2.) then
              xarray(i) = cmplx(0.,0.)
           else
              xarray(i) = cmplx(cos(phase),sin(phase))*kw(i)
           end if
        end do

c initialize buffer
	do i = 1, rng_in
	   do j = 1, az_in
	      buff_in_rd(i,j) = 0.0
	   end do
	end do

c ns which is number of samples in each pulse
	samples = rng_in * fac_reduce
	if(start_sample+samples .gt. ns) then
	   samples = ns-start_sample
	end if

c nlines is number of pulses
	lines = az_in
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

	do j = 1, lines

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

	   if(raw_mode.eq.2)  then !input is real(1) or i/q(2)

	      if(i_iqqi.eq.0) then
		 if(factor.eq.2) then
		    do i=1,samples
		       buff_in_rd(i,j)=fun2(b_data(2*i-1),b_data(2*i))
                       c_temp(i) = fun2(b_data(2*i-1),b_data(2*i))
		    enddo
		 else
		    do i=1,samples
		       buff_in_rd(i,j)=fun5(b_data(i))*agc_factor
                       c_temp(i) = fun5(b_data(i))*agc_factor
		    enddo
		 endif
	      else
		 if(factor.eq.2) then
		    do i=1,samples
		       buff_in_rd(i,j)=fun2(b_data(2*i),b_data(2*i-1))
                       c_temp(i) = fun2(b_data(2*i),b_data(2*i-1))
		    enddo
		 else
		    do i=1,samples
		       buff_in_rd(i,j)=fun5(b_data(i))*agc_factor
                       c_temp(i) = fun5(b_data(i))*agc_factor
		    enddo
		 endif
	      end if
c
c Reduce range the spectrum by fac_reduce qdn 1/12/98
c
              nfft = rng_in * fac_reduce
              do i=samples+1,nfft     ! append zero to each range line
                 c_temp(i) = cmplx(0.0,0.0)
              end do
              call init_fft(nfft)     
              iflag = 1
              call cfft(c_temp,1,nfft,iflag) ! take forward FFT
        
              do i=1,rng_in                  ! clean out the spectrum
                 c_data(i) = cmplx(0.0,0.0)
              end do
              nside = (rng_in/2) * 0.85
              do i=1,nside                   ! copy the lower spectrum
                 c_data(i) = c_temp(i) * xarray(i)
              end do
              do i=0,nside-1                 ! copy the higher spectrum
                 c_data(rng_in - i) = c_temp(nfft-i) * xarray(nfft-i)
              end do
              c_data(1) = cmplx(0.0,0.0)     ! set dc term to be zero
              call init_fft(rng_in)          ! for inverse size
              iflag = -1
              call cfft(c_data,1,rng_in,iflag) ! take inverse FFT 
              do i=1,rng_in - plen*fs/fac_reduce
                 buff_in_rd(i,j) = c_data(i)
              end do
              do i=rng_in - plen*fs/fac_reduce+1,rng_in
                 buff_in_rd(i,j) = cmplx(0.0,0.0) 
              end do
              
	      
	   else

	      do i=1,n_byte
		 c_temp(i)=fun1(b_data(i)) !fun1 is a function
	      enddo
	      do i= n_byte+1, 2*rng_in !append zero to each line
		 c_temp(i)=cmplx(0.0,0.0)
	      enddo
	      
	      nfft = 2*rng_in*fac_reduce
	      call init_fft(nfft) !convert real to i/q
	      iflag = 1
	      call cfft(c_temp,1,nfft,iflag)
	      do k = 1, rng_in/2
		 temp = c_data(k+rng_in)
		 c_data(k) = c_data(k+3*rng_in/2)
		 c_data(k+rng_in/2) = temp
	      end do
              do i=1,rng_in
                 c_data(i) = cmplx(0.0,0.0)
              end do
              noffset = nfft/2 - rng_in/2 
              do i=1,rng_in/2           !rotate and reduce the spectrum
                 c_data(i) = c_temp(i+noffset)*xarray(i)
                 c_data(i+rng_in/2) = c_temp(i)*xarray(i)
              end do
	      call init_fft(rng_in) 
	      iflag = -1
	      call cfft(c_data,1,rng_in,iflag) ! take inverse FFT
	      do i = 1, rng_in	!write into 2-d block
		 buff_in_rd(i,j) = c_data(i)
	      end do
	      
	   end if
	end do

	if(lines.gt.1) then
	   do j=lines+1, az_in	!append zero line to block
	      do i=1,rng_in	
		 buff_in_rd(i,j)=cmplx(0.0,0.0)
	      enddo
	   enddo
	end if
	
	return
	end


	
