c SccsId = @(#)wayne_f.f	2.41 3/24/98
c  Subroutine:  ceos_image
c
c  Purpose:  Read in image file in byte format and write out image records
c            in CEOS format
c
c  Inputs:   in_fname -- name of input image file
c            out_pname -- name of path to write out image in CEOS format
c            nlines --  number of lines in image file, including fill
c            line1 -- beginning line number for line heading calculation
c            line2 -- ending line number for line heading calculation
c            linec -- line number of scene center
c            npix -- number of pixels per line, including fill
c            year -- acquisition year (integer)
c            doy -- acquisition day of year (integer)
c            msecs -- acquisition milliseconds of day
c            left_fill_cnt -- array of number of left fill pixels in each line
c            image_pix_cnt -- array of number of image pixels in each line
c            right_fill_cnt -- array of number of right fill pixels in each line
c            slr_first -- array of slant range to first image pixel in each line
c            slr_mid -- array of slant range to mid image pixel in each line
c            slr_last -- array of slant range to last image pixel in each line
c            lat_first -- array of latitude of first image pixel in each line
c            lat_mid -- array of latitude of mid image pixel in each line
c            lat_last -- array of latitude of last image pixel in each line
c            long_first -- array of longitude of first image pixel in each line
c            long_mid -- array of longitude of mid image pixel in each line
c            long_last -- array of longitude of last image pixel in each line
c
c  Outputs:  pro_hist -- table of processed data histogram values
c            minfreq -- minimum histogram table value
c            maxfreq -- maximum histogram table value
c            meansamp -- histogram mean sample value
c            stdvsamp -- histogram standard deviation of sample value
c            meanfreq -- histogram mean table value
c            stdvfreq -- histogram standard deviation of table value
c            sc_head  -- heading of scene center line
c            act_lines -- actual image lines
c            act_pix -- maximum number of pixels in an image line
c 
c            the CEOS formatted image file.
c            
c
c  Processing:  The following steps are taken:
c
c  1.  Set all line buffer bytes to zero.
c
c  2.  Set all line header invariant bytes to correct values.
c
c  3.  Call line_hdng to calculate line headings.
c
c  4.  Strip path from file name.
c
c  5.  Call subroutine img_desc to write out the image options file
c      descriptor record.
c
c  6.  Loop through all lines in the image.  For each line, do the
c      following:
c
c    a.  Set remaining line header bytes to correct values.
c
c    b.  Read image line from input file and copy to CEOS image line
c        buffer pixel region.
c
c    c.  Update processed data histogram with these pixel values.
c
c    d.  Write formatted buffer line to CEOS output file.
c
c  7.  Compute histogram statistics.
c
c  Warnings:  Currently no validity checking is being performed on the
c             values of nlines or npix, with unpredictable results if
c             they don't match the actual dimensions of the image.


      subroutine ceos_image(in_fname,internal_out_fname,out_pname,
     *nlines,npix,left_fill_cnt,image_pix_cnt,right_fill_cnt,
     *pro_hist,minfreq,maxfreq,meansamp,stdvsamp,meanfreq,
     *stdvfreq,linec,sc_head,act_lines,res_prod,p1_fm,p2_fm,
     *c1_frm,c2_frm,proj_id,prod_type,ceos_fname,
     *zone,slat,along0,plat1,plat2,lat_orig,long_orig,
     *alpha1,alpha2,alpha3,terr_corr_flag,burst_end,nbeams,gha,
     *sv_ref_time,sv_x_pos,sv_y_pos,sv_z_pos,sv_x_vel,sv_y_vel,
     *sv_z_vel,t_burst,r_bip,coef_fd,gha_time,dem_ellip,
     *dlon_topo,dlat_topo,topo,avg_terrain,rlocal_mean,istatus)

c  Miscellaneous declarations, including subroutine arguments
      implicit none

        character*128 SccsId_wayne_f
        data SccsId_wayne_f
     +  /'@(#)PPwayne_f.f:2.41'/

      include 'ssp2_const.inc'
      integer maxlines
      parameter (maxlines=15554)
      integer maxpix
      parameter (maxpix=15554)
      integer left_fill_cnt(maxlines)
      integer image_pix_cnt(maxlines)
      integer right_fill_cnt(maxlines)
      real*4 line_head(maxlines)
      integer dk_ptr_in,dk_ptr_out
      character*60 in_fname,data_fname
      character*60 ldr_fname
      character*256 out_pname,fname1
      character*60 internal_out_fname
      character*16 ceos_fname,fname2
      character*20 scen_id
      integer year
      integer doy
      integer i,j,k
      integer ccreate,cclose,ch_fd
      byte ch
      real*4 sc_head
      integer act_lines,act_pix
      real*8 p1_fm(2,2)
      real*8 p2_fm(2,2)
      real*8 p1_pxl
      real*8 p2_pxl
      real*8 p1_begin,p2_begin
      real*8 p1_first,p1_mid,p1_last
      real*8 del_p1
      real*8 del_p2_first,del_p2_mid,del_p2_last
      real*8 p2_first,p2_mid,p2_last
      real*8 plat,plat_d,plon
      real*8 c1_begin,c2_begin
      real*8 c1_first,c1_mid,c1_last
      real*8 del_c1
      real*8 del_c2_first,del_c2_mid,del_c2_last
      real*8 c2_first,c2_mid,c2_last
      real*8 clat,clat_d,clon
      real*4 lat_first
      real*4 lat_mid
      real*4 lat_last
      real*4 long_first
      real*4 long_mid
      real*4 long_last
      integer lineval(maxlines)
      integer nlines,npix,msecs,nchars
      integer jstart,jend,jval,line1,line2,linec
      real*8 c1_frm(2,2),c2_frm(2,2),c1_pxl,c2_pxl
      integer proj_id
      integer prod_type
      character*8 dummy_str

      integer printflog
      integer zone,istatus,idummy
      real*8 slat,along0,plat1,plat2,lat_orig,long_orig
      real*8 alpha1,alpha2,alpha3

      integer scyear,scdoy,schr,scmin,scsec,scusec
      integer scmon,scdom                       ! JMS 9-7-96
      real*8 nslat,nslon,nelat,nelon
      real*8 fslat,fslon,felat,felon,sclat,sclon
      real*8 sc_time_sec
      integer burst_end,nbeams,terr_corr_flag
      real*8 gha,sv_ref_time,sv_x_pos,sv_y_pos,sv_z_pos
      real*8 sv_x_vel,sv_y_vel,sv_z_vel,t_burst(burst_max)
      real*8 r_bip(burst_max),coef_fd(4,burst_max)
      integer res_prod
      real*8 ns_h,ne_h,fs_h,fe_h               ! JMS 5-8-96
      real*8 gha_time                          ! JMS 5-9-96
      real*8 dlon_topo                         ! 5-10-96
      real*8 dlat_topo                         ! 5-10-96
      integer dem_ellip                        ! 5-10-96
      integer*2 topo(ns_ew,ns_ns)              ! 5-10-96
      real*8 avg_terrain                       ! 5-15-96
      real*8 rlocal_mean                       ! 6-10-96

      character*1 hem
      real*8 tmp_latc,tmp_latc_d,tmp_lonc
      integer k1,k2

c  Declarations for processed data histogram items
      integer nbins
      parameter (nbins=256)
      integer pro_hist(256)
      real*4 value(256)
      real*4 minfreq,maxfreq,meansamp,stdvsamp,meanfreq,stdvfreq

c  Declarations of input and output line buffers
      byte pix_buf(maxpix)
      byte line_buf(maxpix+192)

c  Declarations for variables to strip path information from output
c  file name
      integer fbuf1len,fbuf2len
      parameter (fbuf1len=256)
      parameter (fbuf2len=20)          
      character*1 fbuf1(fbuf1len),fbuf2(fbuf2len),xc

c  Declarations for image line header items
      integer rec_seq,rec_num,length,line_num
      byte rec_sub1,rec_type,rec_sub2,rec_sub3
      real*4 slrf
      real*4 slrm
      real*4 slrl
      real*4 latf
      real*4 latm
      real*4 latl
      real*4 longf
      real*4 longm
      real*4 longl
      real*4 lineh
      integer lfc,ipc,rfc,sensor_updf,acq_year,acq_day
      integer acq_msec,geo_updf
      integer*2 sar_chan_ind,sar_chan_code

c  Declarations to place line header data items at correct byte locations
c  in output image line
      equivalence(line_buf(1),rec_seq)
      equivalence(line_buf(5),rec_sub1)
      equivalence(line_buf(6),rec_type)
      equivalence(line_buf(7),rec_sub2)
      equivalence(line_buf(8),rec_sub3)
      equivalence(line_buf(9),length)
      equivalence(line_buf(13),line_num)
      equivalence(line_buf(17),rec_num)
      equivalence(line_buf(21),lfc)
      equivalence(line_buf(25),ipc)
      equivalence(line_buf(29),rfc)
      equivalence(line_buf(33),sensor_updf)
      equivalence(line_buf(37),acq_year)
      equivalence(line_buf(41),acq_day)
      equivalence(line_buf(45),acq_msec)
      equivalence(line_buf(49),sar_chan_ind)
      equivalence(line_buf(51),sar_chan_code)
      equivalence(line_buf(129),geo_updf)
      equivalence(line_buf(65),slrf)
      equivalence(line_buf(69),slrm)
      equivalence(line_buf(73),slrl)
      equivalence(line_buf(133),latf)
      equivalence(line_buf(137),latm)
      equivalence(line_buf(141),latl)
      equivalence(line_buf(145),longf)
      equivalence(line_buf(149),longm)
      equivalence(line_buf(153),longl)
      equivalence(line_buf(181),lineh)

c  Declaration to place input pixel data at correct place in output
c  image line
      equivalence(line_buf(193),pix_buf(1))

c  Declarations for stripping path information from output file name
      equivalence(fbuf1(1),fname1)
      equivalence(fbuf2(1),fname2)

      istatus=iok

      write (*,*) ' In ceos_image JEFF 1'
c  Set line buffer bytes to zero
      do 20 i=1,(maxpix+192)
      line_buf(i)=char(0)
   20 continue

      write (*,*) ' In ceos_image JEFF 1A'



c  Strip path information from output file name
c  Begin by searching backwards from end of string for a "/"
      j=fbuf1len
      fname1=out_pname
      do 40 i=1,fbuf1len
      xc=fbuf1(j)
      if (xc.eq."/") go to 50
      j=j-1
   40 continue
   50 continue
      write (*,*) ' In ceos_image JEFF 1C'

c  Now, beginning with the first non-pathname character, copy file name
c  from buffer 1 to buffer 2.  File name is 16 byte.
      nchars=16
      do 60 i=1,nchars
      k=j+i
      fbuf2(i)=fbuf1(k)
   60 continue
      fbuf2(17)=' '
      fbuf2(18)=' '
      ceos_fname=fname2
      write (*,*) ' In ceos_image JEFF 2'


c  Get acquisition time which the same as the scene center time
      write (*,*) 'c1_frm(1,1) = ',c1_frm(1,1)
      write (*,*) 'c1_frm(1,2) = ',c1_frm(1,2)
      write (*,*) 'c1_frm(2,1) = ',c1_frm(2,1)
      write (*,*) 'c1_frm(2,2) = ',c1_frm(2,2)
      write (*,*) 'c2_frm(1,1) = ',c2_frm(1,1)
      write (*,*) 'c2_frm(1,2) = ',c2_frm(1,2)
      write (*,*) 'c2_frm(2,1) = ',c2_frm(2,1)
      write (*,*) 'c2_frm(2,2) = ',c2_frm(2,2)
      call corn_cent(c1_frm,c2_frm,terr_corr_flag,
     *burst_end,nbeams,gha,sv_ref_time,sv_x_pos,sv_y_pos,
     *sv_z_pos,sv_x_vel,sv_y_vel,sv_z_vel,t_burst,alpha1,
     *alpha2,alpha3,r_bip,coef_fd,
     *sc_time_sec,sclat,sclon,scyear,scdoy,schr,scmin,
     *scsec,scusec,nslat,nslon,nelat,nelon,fslat,
     *fslon,felat,felon,ns_h,ne_h,fs_h,fe_h,gha_time,
     *dem_ellip,dlon_topo,dlat_topo,topo,avg_terrain,
     *rlocal_mean,scmon,scdom)
      write (*,*) 'scsec = ',scsec
      write (*,*) 'scusec = ',scusec
      msecs=1000*(3600*schr+60*scmin+scsec)+int(dble(scusec)/1000.0d0)


c  Fill in invariant line header items
      rec_sub1=char(50)
      rec_type=char(11)
      rec_sub2=char(18)
      rec_sub3=char(20)
      length=npix+192
      rec_num=1
      sensor_updf=1
      sar_chan_ind=1
      sar_chan_code=2
      geo_updf=1
      acq_year=scyear
      acq_day=scdoy
      acq_msec=msecs
      slrf=0.0
      slrm=0.0
      slrl=0.0


c  Create and fill in file descriptor record
      call img_desc(internal_out_fname,ceos_fname,nlines,npix,
     *istatus)
      if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return
      write (*,*) ' In ceos_image JEFF 3'

c  Set up histogram params
      do 90 i=1,nbins
      pro_hist(i)=0
      value(i)=float(i-1)
   90 continue
      write (*,*) ' In ceos_image JEFF 4'

      act_lines=nlines
c  Loop through image lines
      do 200 i=1,nlines
      if (mod(i,10).eq.0) then
        write (*,*) ' In image loop  ',i
      endif

c  Read in pixel data.
      dk_ptr_in=(i-1)*npix
      call read_disk(in_fname,pix_buf,npix,dk_ptr_in,istatus)
      if (istatus.eq.ierr_2) then
        idummy=printflog(3,'Cannot open unformatted multilook 
     *  image file&')
        return
      elseif (istatus.eq.ierr_4) then
        idummy=printflog(3,'Cannot read from  unformatted 
     *  multilook image file&') 
        write (unit=dummy_str,fmt=1234) i
        idummy=printflog(3,'Line number '//dummy_str//'&')
        return
      elseif (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) then
        return
      endif
c  Loop through pixels for line header
      lineval(i)=0
      do 100 j=1,npix
      ch=pix_buf(j)
      if (int(ch).ne.0) lineval(i)=1     ! line contains image
  100 continue
 
      if (lineval(i).eq.0) then
        left_fill_cnt(i)=npix
        image_pix_cnt(i)=0
        right_fill_cnt(i)=0
        lat_first=0.0
        lat_mid=0.0
        lat_last=0.0
        long_first=0.0
        long_mid=0.0
        long_last=0.0
        line_head(i)=0.0
        act_lines=act_lines-1
      else
c  Find first nonzero pixel
        do 110 j=1,npix
        jstart=j
        ch=pix_buf(j)
        if (int(ch).ne.0) go to 115
  110   continue
  115   continue
c  Find last nonzero pixel
        do 120 j=1,npix
        jend=npix+1-j
        ch=pix_buf(jend)
        if (int(ch).ne.0) go to 125
  120   continue
  125   continue
        left_fill_cnt(i)=jstart-1
        right_fill_cnt(i)=npix-jend
        image_pix_cnt(i)=jend+1-jstart

        if (res_prod.eq.75) then
          c1_pxl=50.0d0
          c2_pxl=50.0d0
          p1_pxl=50.0d0
          p2_pxl=50.0d0
        else if (res_prod.eq.150) then
          c1_pxl=100.0d0
          c2_pxl=100.0d0
          p1_pxl=100.0d0
          p2_pxl=100.0d0
        else if (res_prod.eq.600) then
          c1_pxl=400.0d0
          c2_pxl=400.0d0
          p1_pxl=400.0d0
          p2_pxl=400.0d0
        endif

c ************************************************************
	  if (proj_id.eq.ps) then
	   hem = 'n'
	   do k1=1,2
	      do k2=1,2
		 call ac_to_ll(c1_frm(k1,k2),c2_frm(k1,k2),
     .		 tmp_latc,tmp_latc_d,tmp_lonc,
     .		 alpha1,alpha2,alpha3,rlocal_mean)
		 if(tmp_latc_d.lt.0) hem = 's'
	      end do
	   end do
	  end if
c ************************************************************
        if (prod_type.eq.1.and.proj_id.ne.atct) then
          p1_begin=p1_fm(1,1)
          p2_begin=p2_fm(1,1)
          del_p1=dble(i-1)*p1_pxl
          p1_first=p1_begin+del_p1
          p1_mid=p1_begin+del_p1
          p1_last=p1_begin+del_p1
          del_p2_first=dble(jstart-1)*p2_pxl
          del_p2_last=dble(jend-1)*p2_pxl
          del_p2_mid=(del_p2_first+del_p2_last)/2.0d0
          p2_first=p2_begin+del_p2_first
          p2_mid=p2_begin+del_p2_mid
          p2_last=p2_begin+del_p2_last
          call proj_to_ll(p1_first,p2_first,plat,plat_d,plon,
     *    proj_id,zone,slat,along0,hem,plat1,plat2,lat_orig,
     *    long_orig)
          lat_first=plat_d
          long_first=plon
          call proj_to_ll(p1_mid,p2_mid,plat,plat_d,plon,
     *    proj_id,zone,slat,along0,hem,plat1,plat2,lat_orig,
     *    long_orig)
          lat_mid=plat_d
          long_mid=plon
          call proj_to_ll(p1_last,p2_last,plat,plat_d,plon,
     *    proj_id,zone,slat,along0,hem,plat1,plat2,lat_orig,
     *    long_orig)
          lat_last=plat_d
          long_last=plon
          call line_hdng(lat_first,lat_last,long_first,
     *    long_last,line_head(i))
        else
          c1_begin=c1_frm(1,1)
          c2_begin=c2_frm(1,1)
          del_c1=dble(i-1)*c1_pxl
          c1_first=c1_begin+del_c1
          c1_mid=c1_begin+del_c1
          c1_last=c1_begin+del_c1
          del_c2_first=dble(jstart-1)*c2_pxl
          del_c2_last=dble(jend-1)*c2_pxl
          del_c2_mid=(del_c2_first+del_c2_last)/2.0d0
          c2_first=c2_begin+del_c2_first
          c2_mid=c2_begin+del_c2_mid
          c2_last=c2_begin+del_c2_last
          call ac_to_ll(c1_first,c2_first,clat,clat_d,clon,
     *      alpha1,alpha2,alpha3,rlocal_mean)
          lat_first=clat_d
          long_first=clon
          call ac_to_ll(c1_mid,c2_mid,clat,clat_d,clon,
     *      alpha1,alpha2,alpha3,rlocal_mean)
          lat_mid=clat_d
          long_mid=clon
          call ac_to_ll(c1_last,c2_last,clat,clat_d,clon,
     *      alpha1,alpha2,alpha3,rlocal_mean)
          lat_last=clat_d
          long_last=clon
          call line_hdng(lat_first,lat_last,long_first,
     *    long_last,line_head(i))
        endif
      endif

c  Fill in line header data that varies with each line
      if (prod_type.eq.1.and.proj_id.ne.atct) then
        rec_seq=i+1
        line_num=i
      else
        rec_seq=nlines+2-i
        line_num=nlines+1-i
      endif
      lfc=left_fill_cnt(i)
      ipc=image_pix_cnt(i)
      rfc=right_fill_cnt(i)
      latf=lat_first
      latm=lat_mid
      latl=lat_last
      longf=long_first
      longm=long_mid
      longl=long_last
      lineh=line_head(i)
c  Write the record.  Note that the output
c  index is offset by one record relative to the input, to make
c  room for the file descriptor record
c  Changed 10-25-96 to flip ps as well as atct image
c  Changed 11-01-96 to flip all images
c      if (prod_type.eq.1.and.proj_id.ne.atct.and.proj_id.ne.ps) then
c        dk_ptr_out=i*(npix+192)
c      else
        dk_ptr_out=(nlines+1-i)*(npix+192)     ! JMS 5-8-96 for atct or ps flip top to bottom
c      endif
      jstart=left_fill_cnt(i)+1
      jend=npix-right_fill_cnt(i)
c  Loop through pixels for histogram
      if (lineval(i).ne.0) then
        do 180 j=jstart,jend
        ch=pix_buf(j)
        jval=int(ch)
        if (jval.lt.0) jval=jval+nbins
        jval=jval+1
        if(jval.gt.nbins) jval=nbins
        pro_hist(jval)=pro_hist(jval)+1
  180   continue
      endif
      call write_disk(internal_out_fname,line_buf,(npix+192),
     *dk_ptr_out,istatus)
      if (istatus.eq.ierr_2) then
        idummy=printflog(3,'Cannot open CEOS multilook image file&')
        return
      elseif (istatus.eq.ierr_3) then
        idummy=printflog(3,'Cannot write to CEOS multilook image file&') 
        write (unit=dummy_str,fmt=1234) i
        idummy=printflog(3,'Line number '//dummy_str//'&')
        return
      elseif (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) then
        return
      endif
  200 continue
      write (*,*) ' In ceos_image JEFF 5'

c  Now do histogram statistics
      call hist_stat(nbins,pro_hist,value,minfreq,maxfreq,
     *meansamp,stdvsamp,meanfreq,stdvfreq)

c  Choose line at center of image and fill in image center
c  line heading

c  Find first image line
      do 250 i=1,nlines
      line1=i
      if (lineval(i).ne.0) go to 255
  250 continue
  255 continue

c  Find last image line
      do 300 i=1,nlines
      line2=nlines+1-i
      if (lineval(i).ne.0) go to 305
  300 continue
  305 continue

c  Find heading of center image line
c  Changed 10-25-96 to flip ps as well as atct image
c  Changed 11-01-96 to flip all images
c      if (prod_type.eq.1.and.proj_id.ne.atct.and.proj_id.ne.ps) then
c        linec=(line1+line2)/2
c      else
        linec=nlines+1-(line1+line2)/2
c      endif
      sc_head=line_head(linec)

      write (*,*) ' Exiting ceos_image JEFF2'
      return
 1234 format (i8)
      end

c  Subroutine:  img_desc
c
c  Purpose:  Create image options file descriptor record for image file.
c
c  Inputs:   fname1 -- name of output image file with complete path
c            fname2 -- name of output file without path information
c            nlines --  number of lines in image file, including fill
c            npix -- number of pixels per line, including fill
c
c  Outputs:  no arguments or common variables are output.  The only output is
c            the file descriptor record in the CEOS formatted image file.
c            
c
c  Processing:  The following steps are taken:
c
c  1.  Set all line buffer bytes to ASCII blank.
c
c  2.  Set all line header bytes to correct values.
c
c  3.  Write formatted buffer line to CEOS output file.


      subroutine img_desc(fname1,fname2,nlines,npix,istatus)

	implicit none
      include 'error.inc'
      integer maxpix
      parameter (maxpix=15554)
      character*60 fname1
      character*16 fname2
      byte line_buf(maxpix+192)
      integer dk_ptr,ch_fd,ccreate
      integer idummy,printflog,istatus

c  Declarations for file descriptor record data items
      integer rec_seq
      byte rec_sub1
      byte rec_type
      byte rec_sub2
      byte rec_sub3
      integer length
      integer nlines
      integer npix
      integer i
      integer line_bytes

      character*2  ascii_flag
      character*12  format_doc
      character*2  format_rev
      character*2  design_rev
      character*12  software_id
      character*4  file_num
      character*16  file_name
      character*4  rec_seq_flg
      character*8  seq_loc
      character*4  seq_len
      character*4  rec_code
      character*8  code_loc
      character*4  code_len
      character*4  rec_len
      character*8  rlen_loc
      character*4  rlen_len
      character*6  n_dataset
      character*6  l_dataset
      character*4  nbit
      character*4  nsamp
      character*4  nbyte
      character*4  justify
      character*4  nchn
      character*8  n_lin
      character*4  nleft
      character*8  ngrp
      character*4  nright
      character*4  ntop
      character*4  nbox1
      character*4  intleav
      character*2  nrec_lin
      character*2  nrec_chn
      character*4  n_prefix
      character*8  n_sar
      character*4  n_suffix
      character*8  lin_loc
      character*8  chn_loc
      character*8  time_loc
      character*8  left_loc
      character*8  right_loc
      character*4  pad_ind
      character*8  qual_loc
      character*8  cali_loc
      character*8  gain_loc
      character*8  bias_loc
      character*28  type_id
      character*4  type_code
      character*4  left_fill
      character*4  right_fill
      character*8  pix_mg

c  Declarations to place data items in correct locations in buffer
      equivalence(line_buf(1),rec_seq)
      equivalence(line_buf(5),rec_sub1)
      equivalence(line_buf(6),rec_type)
      equivalence(line_buf(7),rec_sub2)
      equivalence(line_buf(8),rec_sub3)
      equivalence(line_buf(9),length)
      equivalence(line_buf(13),ascii_flag)
      equivalence(line_buf(17),format_doc)
      equivalence(line_buf(29),format_rev)
      equivalence(line_buf(31),design_rev)
      equivalence(line_buf(33),software_id)
      equivalence(line_buf(45),file_num)
      equivalence(line_buf(49),file_name)
      equivalence(line_buf(65),rec_seq_flg)
      equivalence(line_buf(69),seq_loc)
      equivalence(line_buf(77),seq_len)
      equivalence(line_buf(81),rec_code)
      equivalence(line_buf(85),code_loc)
      equivalence(line_buf(93),code_len)
      equivalence(line_buf(97),rec_len)
      equivalence(line_buf(101),rlen_loc)
      equivalence(line_buf(109),rlen_len)
      equivalence(line_buf(181),n_dataset)
      equivalence(line_buf(187),l_dataset)
      equivalence(line_buf(217),nbit)
      equivalence(line_buf(221),nsamp)
      equivalence(line_buf(225),nbyte)
      equivalence(line_buf(229),justify)
      equivalence(line_buf(233),nchn)
      equivalence(line_buf(237),n_lin)
      equivalence(line_buf(245),nleft)
      equivalence(line_buf(249),ngrp)
      equivalence(line_buf(257),nright)
      equivalence(line_buf(261),ntop)
      equivalence(line_buf(265),nbox1)
      equivalence(line_buf(269),intleav)
      equivalence(line_buf(273),nrec_lin)
      equivalence(line_buf(275),nrec_chn)
      equivalence(line_buf(277),n_prefix)
      equivalence(line_buf(281),n_sar)
      equivalence(line_buf(289),n_suffix)
      equivalence(line_buf(297),lin_loc)
      equivalence(line_buf(305),chn_loc)
      equivalence(line_buf(313),time_loc)
      equivalence(line_buf(321),left_loc)
      equivalence(line_buf(329),right_loc)
      equivalence(line_buf(337),pad_ind)
      equivalence(line_buf(369),qual_loc)
      equivalence(line_buf(377),cali_loc)
      equivalence(line_buf(385),gain_loc)
      equivalence(line_buf(393),bias_loc)
      equivalence(line_buf(401),type_id)
      equivalence(line_buf(429),type_code)
      equivalence(line_buf(433),left_fill)
      equivalence(line_buf(437),right_fill)
      equivalence(line_buf(441),pix_mg)


c  Initially fill line buffer with blanks
      do 50 i=1,maxpix+192
      line_buf(i)=" "
   50 continue

c  Fill in data items
      rec_seq=1
      rec_sub1=char(63)
      rec_type=char(-64)
      rec_sub2=char(18)
      rec_sub3=char(18)
      length=npix+192
      line_bytes=npix+192
      call puti4(npix,8,n_sar)
      call puti4(nlines,6,n_dataset)
      call puti4(line_bytes,6,l_dataset)
      call puti4(nlines,8,n_lin)
      call puti4(npix,8,ngrp)
      ascii_flag="A "
      format_doc="CEOS-SAR-CCT"
      format_rev=" B"
      design_rev=" B"
      software_id="SSP-1 V1.0  "
      file_num="   2"
      file_name=fname2
      rec_seq_flg="FSEQ"
      seq_loc="       1"
      seq_len="   4"
      rec_code="FTYP"
      code_loc="       5"
      code_len="   4"
      rec_len="FLGT"
      rlen_loc="       9"
      rlen_len="   4"
      nbit="   8"
      nsamp="   1"
      nbyte="   1"
      justify="    "
      nchn="   1"
      nleft="   0"
      nright="   0"
      ntop="   0"
      nbox1="   0"
      intleav="BSQ "
      nrec_lin=" 1"
      nrec_chn=" 1"
      n_prefix=" 192"
      n_suffix="   0"
      lin_loc="  1354PB"
      chn_loc="  4952PB"
      time_loc="  4554PB"
      left_loc="  21 4PB"
      right_loc="  29 4PB"
      pad_ind="    "
      qual_loc="        "
      cali_loc="        "
      gain_loc="        "
      bias_loc="        "
      type_id="UNSIGNED INTEGER1           "
      type_code="IU1 "
      left_fill="   0"
      right_fill="   0"
      pix_mg="     255"

c  Write the record
      dk_ptr=0
      ch_fd=ccreate(fname1)
      if (ch_fd .le. 0) then
        idummy=printflog(3,'Cannot create CEOS multilook image file'
     *  //fname1//'&')
      endif
      call cclose(ch_fd)

      call write_disk(fname1,line_buf,(npix+192),dk_ptr,istatus)
      if (istatus.eq.ierr_2) then
        idummy=printflog(3,'Cannot open CEOS multilook image file&')
        return
      elseif (istatus.eq.ierr_3) then
        idummy=printflog(3,'Cannot write image descriptor record
     *  to multilook image file&') 
        return
      elseif (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) then
        return
      endif

      return
      end

      subroutine  puti4(ival,nchars,str)
      byte str(8)
      integer idigit(8)

      maxval=(10**nchars)-1
      if (ival.gt.maxval) ival=maxval
      jval=ival
      do 100 i=1,nchars
      j=nchars+1-i
      idigit(j)=mod(jval,10)
      jval=(jval-idigit(j))/10
  100 continue
      iblank=1
      do 200 i=1,nchars
      if (idigit(i).eq.0.and.iblank.eq.1) then
        str(i)=32
      else
        iblank=0
        str(i)=idigit(i)+48
      endif
  200 continue
      return
      end



c hist_stat(nbins,hstgrm,value,minfreq,maxfreq,meansamp, -------
c	     stdvsamp,meanfreq,stdvfreq)
c
c compute some statistics for the histogram.
c
c inputs:	nbins 	-- number of bins in the histogram
c		hstgrm 	-- the histogram 
c		value	-- the values of each bin 
c output:	minfreq	-- the minimum histogram table value
c		maxfreq	-- the maximum histogram table value
c		meansamp - the mean sample value
c		stdvsamp - the std dev. of sample value
c		meanfreq - the mean histogram table value
c		stdvfreq - the std dev of histogram table value
c



      subroutine hist_stat(nbins,hstgrm,value,minfreq,maxfreq,
     *meansamp,stdvsamp,meanfreq,stdvfreq)

      integer hstgrm(256)
      real*4 value(256)
      real*4 meansamp,stdvsamp,meanfreq,stdvfreq,minfreq,maxfreq
      integer temp
      real*4 ftemp,total,total2,sum,sum2,val

      if (nbins.le.0) return

      mintemp = 9999999
      maxtemp = 0

      total=0. 
      total2=0.
      sum=0.
      sum2=0.

      do 100 i=1,nbins
      temp = hstgrm(i)
      ftemp= float(temp)
c      val= float(i-1)
      val= value(i)
      total=total+ftemp
      total2=total2+ftemp*ftemp
      sum=sum+ftemp*val
      sum2=sum2+ftemp*val*val
      if (mintemp.gt.temp) mintemp=temp
      if (maxtemp.lt.temp) maxtemp=temp
  100 continue

      if (total.gt.0.) then
      meansamp = sum/total
      stdvsamp=sqrt((sum2/total)-(meansamp*meansamp))
      endif
      meanfreq = total/nbins
      stdvfreq=sqrt((total2/nbins)-(meanfreq*meanfreq))
      minfreq  = float(mintemp)
      maxfreq  = float(maxtemp)
c      write (*,*) 'JEFF in hist_stat'
c      write (*,*) 'total,meansamp,meanfreq = ',total,meansamp,meanfreq
c      write (*,*) 'stdvsamp,stdvfreq = ',stdvsamp,stdvfreq
      return
      end



c  Subroutine:  line_hdng
c
c  Purpose:  Compute line heading from first and last pixel latitude and
c            longitude of each image line.
c
c  Inputs:   line1 --  line number of first image line
c            line2 --  line number of last image line
c            lat_first -- array of latitude of first image pixel in each line
c            lat_last -- array of latitude of last image pixel in each line
c            long_first -- array of longitude of first image pixel in each line
c            long_last -- array of longitude of last image pixel in each line
c            asc_flag -- set to 1 if the satellite is ascending, 0 otherwise
c
c  Outputs:  line_head -- array of heading of each line of image
c
c            
c
c  Processing:  The following steps are taken:
c
c  1.  Compute delta latitude and longitude between first and last pixels
c      of image line.
c
c  2.  Multiply delta longitude by the cosine of the latitude to adjust
c      for projection on Earth surface at image location.
c
c  3.  Compute direction of image line, relative to East (for ascending)
c      or West (for descending).
c
c  4.  Compute direction of perpendicular to this line.
c
c  5.  If the line goes through a pole, the heading is 180 deg (due south) 
c      for the northern hemisphere and 0 deg (north) for the southern 
c      hemisphere.
c
c  Assumptions:
c
c  1.  The relations used here for computing the heading are approximate;
c      we are assuming they are close enough for this purpose.  If more
c      rigorous relations are required, the code will have to be changed.
c
c  2.  For measuring longitude, east is the positive direction.
c

      subroutine line_hdng(lat_first,lat_last,long_first,
     *long_last,line_head)

c  Miscellaneous declarations, including subroutine arguments
      real*4 lat_first
      real*4 lat_last
      real*4 long_first
      real*4 long_last
      real*4 line_head
      real*8 pie4,dtr,lat1,lat2,long1,long2,coslat,dlat,dlonc,theta

      pie4=datan(1.0d0)
      dtr=pie4/45.0d0

      lat1=lat_first
      lat2=lat_last
      long1=long_first
      long2=long_last
c  Take cosine of average line latitude
      coslat=dcos(dtr*(lat1+lat2)/2.0d0)
c  Compute delta latitude and cosine corrected delta longitude
      dlat=lat2-lat1
      dlonc=coslat*(long2-long1)
c  Compute line angle
      theta=(datan2(dlat,dlonc))/dtr
c  Due to definition of heading, the following simple expression
c  suffices to compute the perpendicular
      line_head=-theta
c  Take care of special case where image straddles a pole
      if ((dabs(long2-long1)).gt.150.0d0) then
        if (lat1.gt.0.0d0) then  ! North pole
          line_head=180.0     ! Heading is south
        else                     ! South pole
          line_head=0.0       ! Heading is north
        endif
      endif

      return
      end






c  Subroutine:  framelet_image
c
c  Purpose:  Read in image file in byte format and write out image records
c            in CEOS format
c
c  Inputs:   byte_data -- array of pixel values of framelet
c            out_fname -- name of file to write out image in CEOS format
c            nlines --  number of lines in image file, including fill
c            line1 -- beginning line number for line heading calculation
c            line2 -- ending line number for line heading calculation
c            npix -- number of pixels per line, including fill
c            year -- acquisition year (integer)
c            doy -- acquisition day of year (integer)
c            msecs -- acquisition milliseconds of day
c            left_fill_cnt -- array of number of left fill pixels in each line
c            image_pix_cnt -- array of number of image pixels in each line
c            right_fill_cnt -- array of number of right fill pixels in each line
c            slr_first -- array of slant range to first image pixel in each line
c            slr_mid -- array of slant range to mid image pixel in each line
c            slr_last -- array of slant range to last image pixel in each line
c            lat_first -- array of latitude of first image pixel in each line
c            lat_mid -- array of latitude of mid image pixel in each line
c            lat_last -- array of latitude of last image pixel in each line
c            long_first -- array of longitude of first image pixel in each line
c            long_mid -- array of longitude of mid image pixel in each line
c            long_last -- array of longitude of last image pixel in each line
c            linec -- line number at scene center
c
c  Outputs:  pro_hist -- table of processed data histogram values
c            minfreq -- minimum histogram table value
c            maxfreq -- maximum histogram table value
c            meansamp -- histogram mean sample value
c            stdvsamp -- histogram standard deviation of sample value
c            meanfreq -- histogram mean table value
c            stdvfreq -- histogram standard deviation of table value
c            sc_head  -- scene center line heading
c 
c            the CEOS formatted image file.
c            
c
c  Processing:  The following steps are taken:
c
c  1.  Set all line buffer bytes to zero.
c
c  2.  Set all line header invariant bytes to correct values.
c
c  3.  Call line_hdng to calculate line headings.
c
c  4.  Call subroutine img_desc to write out the image options file
c      descriptor record.
c
c  5.  Loop through all lines in the image.  For each line, do the
c      following:
c
c    a.  Set remaining line header bytes to correct values.
c
c    b.  Read image line from input array and copy to CEOS image line
c        buffer pixel region.
c
c    c.  Update processed data histogram with these pixel values.
c
c    d.  Write formatted buffer line to CEOS output file.
c
c  7.  Compute histogram statistics.
c
c  Warnings:  Currently no validity checking is being performed on the
c             values of nlines or npix, with unpredictable results if
c             they don't match the actual dimensions of the image.


      subroutine framelet_image(index3,byte_data,out_fname,image_file,
     *nlines,npix,year,doy,msecs,pro_hist,minfreq,maxfreq,meansamp,
     *stdvsamp,meanfreq,stdvfreq,ceos_fname,act_lines,act_pix,
     *istatus)

c  Miscellaneous declarations, including subroutine arguments
      implicit none
      include 'ssp2_const.inc'
c      integer sam_post
c      parameter (sam_post=3072)
c      integer np_geo
c      parameter (np_geo=256)
      integer dk_ptr_in,dk_ptr_out
      character*60 in_fname,data_fname
      character*60 ldr_fname
      character*256 image_file
      character*60 out_fname
      character*256 fname1
      character*16 ceos_fname
      character*18 fname2
      character*20 scen_id
      integer year
      integer doy
      integer act_lines,act_pix
      integer ccreate,cclose,ch_fd
      byte ch
c      byte byte_data(np_geo,sam_post)
      byte byte_data(sam_post,np_geo,4)
      integer nlines,npix,msecs,i,j,k
      integer nchars,jval,jend
      integer lineval(np_geo),index3
      integer istatus,idummy,printflog
      character*8 dummy_str
      integer nmax                                !  JMS 5-8-96

c  Declarations for processed data histogram items
      integer nbins
      parameter (nbins=256)
      integer pro_hist(nbins)
      real*4 value(nbins)
      real*4 minfreq,maxfreq,meansamp,stdvsamp,meanfreq,stdvfreq

c  Declarations of input and output line buffers
      byte pix_buf(sam_post)
      byte line_buf(sam_post+192)

c  Declarations for variables to strip path information from output
c  file name
      integer fbuf1len,fbuf2len
      parameter (fbuf1len=256)
      parameter (fbuf2len=18)
      character*1 fbuf1(fbuf1len),fbuf2(fbuf2len),xc

c  Declarations for image line header items
      integer rec_seq,rec_num,length,line_num
      byte rec_sub1,rec_type,rec_sub2,rec_sub3
      real*4 slrf
      real*4 slrm
      real*4 slrl
      real*4 latf
      real*4 latm
      real*4 latl
      real*4 longf
      real*4 longm
      real*4 longl
      real*4 lineh
      integer lfc,ipc,rfc,sensor_updf,acq_year,acq_day
      integer acq_msec,geo_updf
      integer*2 sar_chan_ind,sar_chan_code

c  Declarations to place line header data items at correct byte locations
c  in output image line
      equivalence(line_buf(1),rec_seq)
      equivalence(line_buf(5),rec_sub1)
      equivalence(line_buf(6),rec_type)
      equivalence(line_buf(7),rec_sub2)
      equivalence(line_buf(8),rec_sub3)
      equivalence(line_buf(9),length)
      equivalence(line_buf(13),line_num)
      equivalence(line_buf(17),rec_num)
      equivalence(line_buf(21),lfc)
      equivalence(line_buf(25),ipc)
      equivalence(line_buf(29),rfc)
      equivalence(line_buf(33),sensor_updf)
      equivalence(line_buf(37),acq_year)
      equivalence(line_buf(41),acq_day)
      equivalence(line_buf(45),acq_msec)
      equivalence(line_buf(49),sar_chan_ind)
      equivalence(line_buf(51),sar_chan_code)
      equivalence(line_buf(129),geo_updf)
      equivalence(line_buf(65),slrf)
      equivalence(line_buf(69),slrm)
      equivalence(line_buf(73),slrl)
      equivalence(line_buf(133),latf)
      equivalence(line_buf(137),latm)
      equivalence(line_buf(141),latl)
      equivalence(line_buf(145),longf)
      equivalence(line_buf(149),longm)
      equivalence(line_buf(153),longl)
      equivalence(line_buf(181),lineh)

c  Declaration to place input pixel data at correct place in output
c  image line
      equivalence(line_buf(193),pix_buf(1))

c  Declarations for stripping path information from output file name
      equivalence(fbuf1(1),fname1)
      equivalence(fbuf2(1),fname2)

      istatus=iok

c  Set line buffer bytes to zero
      do 20 i=1,(sam_post+192)
      line_buf(i)=char(0)
   20 continue


c  Strip path information from output file name
c  Begin by searching backwards from end of string for a "/"
      j=fbuf1len
      fname1=image_file
      do 40 i=1,fbuf1len
      xc=fbuf1(j)
      if (xc.eq."/") go to 50
      j=j-1
   40 continue
   50 continue

c  Now, beginning with the first non-pathname character, copy file name
c  from buffer 1 to buffer 2.  File name is 16 byte.
      nchars=16
      do 60 i=1,nchars
      k=j+i
      fbuf2(i)=fbuf1(k)
   60 continue
      fbuf2(17)=' '
      fbuf2(18)=' '
      ceos_fname=fname2


c  Fill in invariant line header items
      rec_sub1=char(50)
      rec_type=char(11)
      rec_sub2=char(18)
      rec_sub3=char(20)
      length=npix+192
      rec_num=1
      sensor_updf=1
      sar_chan_ind=1
      sar_chan_code=2
      geo_updf=1
      acq_year=year
      acq_day=doy
      acq_msec=msecs
      lfc=0
      ipc=npix
      rfc=0
      slrf=0.0
      slrm=0.0
      slrl=0.0
      latf=0.0
      latm=0.0
      latl=0.0
      latf=0.0
      longf=0.0
      longm=0.0
      longl=0.0
      lineh=0.0

c  Create and fill in file descriptor record
      call img_desc(out_fname,ceos_fname,nlines,npix,istatus)
      if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return

c  Set up histogram params
      do 90 i=1,nbins
      pro_hist(i)=0
      value(i)=float(i-1)
   90 continue

c  Loop through image lines
      do 200 i=1,nlines
      if (mod(i,10).eq.0) then
        write (*,*) ' In image loop  ',i
      endif
c  Fill in line header data that varies with each line
      rec_seq=i+1
      line_num=i

c  Read in pixel data and write the record.  Note that the output
c  index is offset by one record relative to the input, to make
c  room for the file descriptor record
      dk_ptr_in=(i-1)*npix
      dk_ptr_out=i*(npix+192)
c  Load pix_buf from pixel array
      lineval(i)=0
      do 100 j=1,npix
      ch=byte_data(j,i,index3)
      pix_buf(j)=ch
      if (int(ch).ne.0) lineval(i)=1
  100 continue

c  Loop through pixels for histogram
      do 150 j=1,npix
      ch=byte_data(j,i,index3)
      jval=int(ch)
      if (jval.lt.0) jval=jval+nbins
      jval=jval+1
      if(jval.gt.nbins) jval=nbins
      pro_hist(jval)=pro_hist(jval)+1
  150 continue
      call write_disk(out_fname,line_buf,(npix+192),dk_ptr_out,
     *istatus)
      if (istatus.eq.ierr_2) then
        idummy=printflog(3,'Cannot open framelet image file&')
        return
      elseif (istatus.eq.ierr_3) then
        idummy=printflog(3,'Cannot write to framelet image file&')
        write (unit=dummy_str,fmt=1234) i
        idummy=printflog(3,'Line number '//dummy_str//'&')
        return
      elseif (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) then
        return
      endif
  200 continue

c  Now do histogram statistics
      call hist_stat(nbins,pro_hist,value,minfreq,maxfreq,
     *meansamp,stdvsamp,meanfreq,stdvfreq)

c  Determine number of valid lines and pixels
      do 300 i=1,nlines
      j=nlines+1-i
      if (lineval(j).ne.0) go to 310
  300 continue
  310 continue
      act_lines=j
c  Find last nonzero pixel
      nmax=0
      do 360 i=1,nlines
      do 350 j=1,npix
      jend=npix+1-j
      ch=byte_data(jend,i,index3)
      if (int(ch).ne.0) go to 355
  350 continue
  355 continue
      if (jend.gt.nmax) nmax=jend
  360 continue
      act_pix=nmax

      return
 1234 format (i8)
      end

      subroutine get_ldr(nlines,nsamples,
     *left_fill_cnt,image_pix_cnt,
     *right_fill_cnt,in_fname,scan_fname,inp_frame_id,
     *ldr_fname,sc_head,
     *ceos_fname,orbit_no,act_lines,linec,calib_fname,
     *pmf_file,ldr_rec_cnt,ldr_max_rec_len,chirp_rate,fs,plen,
     *burst_start,burst_end,prf,gain_rec,nbeams,np,np_v,
     *avg_terrain,proc_gain,p1_pxl,p2_pxl,peak_bm_ang, gha,
     *sv_type,c1_frm,c2_frm,terr_corr_flag,x_sc,y_sc,z_sc,
     *v_x_sc,v_y_sc,v_z_sc,sv_x_pos,sv_y_pos,sv_z_pos,t_burst,
     *window_start_time,np_air,roll_rate,yaw_rate,pitch_rate,
     *pitch_o,yaw_o,roll_o,pitch,yaw,roll,c1_pxl,c2_pxl,c1_flm,
     *c2_flm,fd_low_near,fd_low_far,fr_near,fr_far,i_ang_bip,
     *look_ang,sv_ref_time,r_low,r_high,p_x,p_y,p_z,chirp_bw,
     *beam_mode,ceos_temp_file,pmf_temp_file,alpha1,alpha2,
     *alpha3,noise_flr_def,meansamp,stdvsamp,r_bip,coef_fd,
     *sv_x_vel,sv_y_vel,sv_z_vel,fdc_est_mode,rgc_est_mode,
     *res_prod,proj,ps_lat,ps_lon,lamb_lat_n,lamb_lat_s,
     *replica_agc,lna_temp,subsystem_temp,
     *protector_temp,rx_agc,sitename,cal_sys_temp,repl_pow,
     *sw_ver,image_path,leader_path,prod_id,frq_media_id,
     *frq_media_type,frq_media_loc,job_id,platform,sensor,
     *rev,sequence,act_id,recorder_id,station_id,frame_mode,
     *sv_prec,product_type,s_n_r,rel_radio_acc,subframe_id,
     *bit_error_rate,ant_flag,gha_time,utm_zone,aux_yaw_valid,
     *aux_roll_valid,aux_pitch_valid,r_cntr,dem_ellip,
     *dlon_topo,dlat_topo,topo,t_b,rlocal_mean,plat1,plat2,
     *slat,along0,rds_vers,cali_file,scan_file,subsystem_name,
     *p1_fm,p2_fm,zone,lat_orig,long_orig,istatus)

      implicit none
      integer maxlines
      parameter (maxlines=15554)
      integer left_fill_cnt(maxlines)
      integer right_fill_cnt(maxlines)
      integer image_pix_cnt(maxlines)
      integer act_lines
      include 'ssp2_const.inc'
      include 'scan.inc'
      include 'calib.inc'
      include 'ceos.inc'
      include 'pmf.inc'
      integer kb(5),status
      character*60 in_fname                        ! 3-5-97 qdn
      character*60 scan_fname
      character*60 calib_fname
      character*60 ceos_temp_file
      real*4 sc_head
      integer strlen
      character*16 ceos_fname,sc_id,dt_id
      character*1 sc_id_buf(16)
      character*12 img_id
      character*1 img_id_buf(12)
      integer orbit_no,orbit_seg
      character*1 dtake_id(16)
      character*5 rrdt,rr
      character*2 dthead                             !  JMS changed 4-4-96
      character*1 rrbuf(5)
      character*2 nndt,nn
      character*1 nnbuf(2)
      integer*4 nlines,nsamples
      integer linec,i,npix,j,k
      real*8 sc_time_sec
      character*256 ldr_file
      character*1 image_file_buf(64)

      integer inp_frame_id,job_id
      character*64 platform,sensor
      integer rev,sequence
      character*64 act_id,media_type,media_loc
      character*64 recorder_id,station_id
      character*64 frame_mode,site_name
      integer seg_id,start_addr,end_addr
      real*8 noise_flr_def
      real*4 meansamp,stdvsamp
      real*8 noise_flr,noise_pwr,total_pwr
      real*8 s_n_r,signal_pwr
      character*64 dummy_str_1,dummy_str_2

      real*8 chirp_rate,fs,plen,avg_terrain,proc_gain,gha              
      integer burst_start,burst_end,nbeams
      real*8 prf(burst_max),gain_rec(burst_max)
      integer np(burst_max),np_v(burst_max),sv_type
      real*8 p1_pxl,p2_pxl,peak_bm_ang(4)

      real*8 c1_frm(2,2),c2_frm(2,2)
      integer terr_corr_flag
      real*8 x_sc(burst_max),y_sc(burst_max),z_sc(burst_max)
      real*8 v_x_sc(burst_max),v_y_sc(burst_max),v_z_sc(burst_max)
      real*8 sv_x_pos,sv_y_pos,sv_z_pos,t_burst(burst_max)
      real*8 sv_x_vel,sv_y_vel,sv_z_vel

      integer np_air(burst_max)
      real*8 roll_rate(burst_max),yaw_rate(burst_max)
      real*8 pitch_rate(burst_max)

      real*8 pitch_o(burst_max),yaw_o(burst_max),roll_o(burst_max)
      real*8 pitch(burst_max),yaw(burst_max),roll(burst_max)
      real*8 c1_pxl,c2_pxl,c1_flm(2,2,burst_max),c2_flm(2,2,burst_max)
      real*8 fd_low_near(burst_max),fd_low_far(burst_max)
      real*8 fr_near(burst_max),fr_far(burst_max),i_ang_bip(burst_max)
      real*8 look_ang(burst_max),sv_ref_time
      real*8 r_low(burst_max),r_high(burst_max)
      real*8 p_x(burst_max),p_y(burst_max),p_z(burst_max)
      real*8 chirp_bw
      real*8 alpha1,alpha2,alpha3
      integer beam_mode
      real*8 r_bip(burst_max),coef_fd(burst_max)
      integer fdc_est_mode,rgc_est_mode
      integer res_prod,proj
      real*8 ps_lat,ps_lon,lamb_lat_n,lamb_lat_s
      integer stime,tarray(9)
      integer time_
      integer cor_year,cor_doy,cor_hr,cor_min,cor_sec,cor_usec
      character*6 corr_yr
      character*27 corr_time
      character*64 sitename
      real*8 pre_cal1,pre_cal2,post_cal1,post_cal2

      real*8 window_start_time(1600),replica_agc(1600)
      real*8 lna_temp(1600),subsystem_temp(1600)
      real*8 protector_temp(1600),rx_agc(1600)
      real*8 cal_sys_temp(1600),repl_pow
      character*64 sw_ver
      character*64 sv_prec
      character*64 prod_creat_time
      character*256 image_path
      character*256 image_file_path
      character*256 leader_path
      character*64 prod_id,frq_media_id
      character*64 frq_media_type,frq_media_loc
      character*64 product_id
      character*256 arch_path,product_path
      character*1  fbuf1(256),fbuf2(256)
      character*1  fbuf3(256),fbuf4(256)
      character*1  fbuf5(256),fbuf6(256)
      character*64 pmf_media_id
      integer product_type
      real*4 bit_error_rate                        ! 4-4-96
      integer ant_flag                             ! 4-17-96

      character*1 ascbuf(2),xc
      integer kb1,kb2,nburst
      integer istatus                              ! 5-3-96
      real*8 ns_h,ne_h,fs_h,fe_h                   ! 5-8-96
      real*8 gha_time                              ! 5-9-96
      integer utm_zone                             ! 5-9-96
      character*4 utm_str                          ! 5-9-96
      character*1 utm_buf(4)                       ! 5-9-96
      real*4 lin_gain                              ! 5-9-96
      character*10 aux_yaw_valid(1600)             ! 5-10-96
      character*10 aux_roll_valid(1600)            ! 5-10-96
      character*10 aux_pitch_valid(1600)           ! 5-10-96
      real*8 r_cntr(burst_max)                     ! 5-10-96
      real*8 dlon_topo                             ! 5-10-96
      real*8 dlat_topo                             ! 5-10-96
      integer dem_ellip                            ! 5-10-96
      integer*2 topo                               ! 5-10-96
      real*8 t_b(burst_max)                        ! 5-15-96
      real*8  rlocal_mean                          ! 6-10-96
      real*8 plat1,plat2,slat,along0               ! 7-1-96
      integer scmon,scdom                          ! 9-7-96
      character*64 scstr                           ! 9-7-96
      character*60 scan_vers                       ! 9-10-96
      character*60 rds_vers                        ! 9-10-96
      character*256 cali_file,scan_file            ! 9-10-96
      character*256 calfile_whole                  ! 9-10-96
      character*256 scanfile_whole                 ! 9-10-96
      character*256 scanfile                       ! 9-10-96
      character*256 calfile                        ! 9-10-96
      character*60 subsystem_name                  ! 9-13-96
      real*8 lat_orig,long_orig                    ! 9-26-96
      real*8 p1_fm(2,2),p2_fm(2,2)                 ! 9-26-96
      integer zone                                 ! 9-26-96
      real*8 proj_nelat,proj_nelon                 ! 9-26-96
      real*8 proj_felat,proj_felon                 ! 9-26-96
      real*8 proj_nslat,proj_nslon                 ! 9-26-96
      real*8 proj_fslat,proj_fslon                 ! 9-26-96
      real*8 dummy_lat                             ! 9-27-96 for proj_to_ll
      character*20 calstat                         ! 10-3-96
      character*204 calcom                         ! 10-3-96

      character*1 hem
      real*8 tmp_latc,tmp_latc_d,tmp_lonc
      integer k3,k4

      real*8    xc1_frm(2,2),xc2_frm(2,2)          ! 3-5-97 qdn
      real*8    xp1_fm(2,2),xp2_fm(2,2)            ! 3-5-97 qdn
      real*8    p1_from_c1(2,2),p2_from_c2(2,2)    ! 3-5-97 qdn
      real*8    xlat,xlat_d,xlon                   ! 3-5-97 qdn
      integer*4 k1,k2                              ! 3-5-97 qdn
      real*8    xorig,yorig                        ! 3-5-97 qdn
      real*8    xc1_min,xc1_max,xc2_min,xc2_max    ! 4-3-97 qdn
      real*8    xp1_min,xp1_max,xp2_min,xp2_max    ! 4-3-97 qdn
      real*8    xc1_center,xc2_center              ! 4-3-97 qdn
      real*8    xp1_center,xp2_center              ! 4-3-97 qdn
      real*8    xnslat,xnslon,xfslat,xfslon        ! 7-22-97
      real*8    xfelat,xfelon,xnelat,xnelon        ! 7-22-97


      equivalence(sc_id,sc_id_buf(1))
      equivalence(img_id,img_id_buf(1))
      equivalence(dthead,dtake_id(1))
      equivalence(rrdt,dtake_id(3))              !  JMS changed 4-4-96
      equivalence(nndt,dtake_id(8))              !  JMS changed 4-4-96
      equivalence(nn,nnbuf(1))
      equivalence(rr,rrbuf(1))
      equivalence(dt_id,dtake_id(1))
      equivalence(image_file,image_file_buf(1))
      equivalence(ascdes,ascbuf(1))
      equivalence(product_path,fbuf1(1))
      equivalence(arch_path,fbuf2(1))
      equivalence(utm_str,utm_buf(1))              ! 5-9-96
      equivalence(calfile_whole,fbuf3(1))          ! 9-10-96
      equivalence(calfile,fbuf4(1))                ! 9-10-96
      equivalence(scanfile_whole,fbuf5(1))         ! 9-10-96
      equivalence(scanfile,fbuf6(1))               ! 9-10-96


c qdn 11/13/97 add the radiometric pattern for SWB mode
      real*4    SWB_pat(256)
      data SWB_pat /
     .     0.0490, 0.0401, 0.0347, 0.0267, 0.0225, 0.0194, 0.0180,
     .     0.0159, 0.0141, 0.0129, 0.0124, 0.0114, 0.0107, 0.0102,
     .     0.0100, 0.0097, 0.0094, 0.0093, 0.0093, 0.0093, 0.0093,
     .     0.0093, 0.0094, 0.0097, 0.0098, 0.0100, 0.0102, 0.0105,
     .     0.0108, 0.0109, 0.0114, 0.0118, 0.0122, 0.0123, 0.0128,
     .     0.0132, 0.0136, 0.0138, 0.0144, 0.0149, 0.0154, 0.0158,
     .     0.0163, 0.0170, 0.0173, 0.0181, 0.0187, 0.0194, 0.0198,
     .     0.0203, 0.0210, 0.0217, 0.0221, 0.0229, 0.0236, 0.0243,
     .     0.0247, 0.0253, 0.0257, 0.0266, 0.0269, 0.0273, 0.0277,
     .     0.0279, 0.0281, 0.0282, 0.0283, 0.0282, 0.0284, 0.0277,
     .     0.0277, 0.0275, 0.0269, 0.0268, 0.0264, 0.0262, 0.0260,
     .     0.0261, 0.0260, 0.0262, 0.0263, 0.0266, 0.0270, 0.0274,
     .     0.0273, 0.0278, 0.0283, 0.0293, 0.0297, 0.0301, 0.0302,
     .     0.0298, 0.0296, 0.0286, 0.0281, 0.0273, 0.0265, 0.0260,
     .     0.0256, 0.0245, 0.0238, 0.0234, 0.0232, 0.0231, 0.0232,
     .     0.0236, 0.0241, 0.0251, 0.0272, 0.0277, 0.0299, 0.0336,
     .     0.0508, 0.0499, 0.0490, 0.0489, 0.0494, 0.0497, 0.0509,
     .     0.0515, 0.0530, 0.0540, 0.0553, 0.0556, 0.0555, 0.0554,
     .     0.0547, 0.0530, 0.0514, 0.0502, 0.0479, 0.0450, 0.0440,
     .     0.0417, 0.0396, 0.0388, 0.0383, 0.0367, 0.0351, 0.0346,
     .     0.0346, 0.0347, 0.0350, 0.0355, 0.0360, 0.0365, 0.0376,
     .     0.0396, 0.0400, 0.0410, 0.0433, 0.0448, 0.0456, 0.0476,
     .     0.0496, 0.0507, 0.0512, 0.0518, 0.0515, 0.0515, 0.0508,
     .     0.0509, 0.0502, 0.0499, 0.0495, 0.0473, 0.0468, 0.0467,
     .     0.0466, 0.0461, 0.0460, 0.0459, 0.0405, 0.0418, 0.0425,
     .     0.0430, 0.0437, 0.0446, 0.0443, 0.0443, 0.0431, 0.0425,
     .     0.0417, 0.0398, 0.0383, 0.0368, 0.0364, 0.0356, 0.0341,
     .     0.0333, 0.0331, 0.0328, 0.0328, 0.0326, 0.0332, 0.0345,
     .     0.0357, 0.0372, 0.0388, 0.0411, 0.0445, 0.0473, 0.0492,
     .     0.0467, 0.0453, 0.0443, 0.0435, 0.0425, 0.0419, 0.0422,
     .     0.0424, 0.0418, 0.0414, 0.0415, 0.0413, 0.0402, 0.0394,
     .     0.0388, 0.0380, 0.0367, 0.0356, 0.0347, 0.0339, 0.0331,
     .     0.0325, 0.0320, 0.0316, 0.0313, 0.0310, 0.0321, 0.0328,
     .     0.0332, 0.0346, 0.0371, 0.0402, 0.0440, 0.0471, 0.0536,
     .     0.0624, 0.0771, 0.1041, 0.2417, 1.0000, 1.0000, 1.0000,
     .     1.0000, 1.0000, 1.0000, 1.0000, 1.0000, 1.0000, 1.0000,
     .     1.0000, 1.0000, 1.0000, 1.0000 /


      istatus=iok

      call ldr_defaults
      pro_head=sc_head
      if (pro_head.lt.0.0d0) pro_head=pro_head+360.0d0   !  7-15-96
      sc_id=ceos_fname
      scene_id=sc_id

c  JMS 8-12-96 file descriptor record
      file_name=sc_id
      write (*,*) 'JEFFS file_name = ',file_name

      do 10 i=1,12
      img_id_buf(i)=' '
   10 continue
      do 20 i=1,9
      img_id_buf(i)=sc_id_buf(i+7)
   20 continue
      fac_image_id=img_id

      write (*,*) 'In get_ldr, terr_corr_flag = ',terr_corr_flag
      write (*,*) 'In get_ldr, proj = ',proj
      write (*,*) 'In get_ldr, frq_media_id = ',frq_media_id
      write (*,*) 'In get_ldr, cali_file = ',cali_file
      write (*,*) 'In get_ldr, scan_file = ',scan_file
      write (*,*) 'inp_frame_id = ',inp_frame_id
c      call getscandata(inp_frame_id,scan_fname,status,job_id,
c     *platform,sensor,rev,sequence,act_id,media_id,media_type,
c     *media_loc,recorder_id,station_id,frame_mode,sitename,
c     *seg_id,svyr,svdoy,svhr,svmin,svsec,svusec,scyr,scdoy,schr,
c     *scmin,scsec,scusec,styr,stdoy,sthr,stmin,stsec,stusec,edyr,
c     *eddoy,edhr,edmin,edsec,edusec,start_addr,end_addr,xpos,
c     *ypos,zpos,xvel,yvel,zvel,nslat,nslon,nelat,nelon,
c     *fslat,fslon,felat,felon,sclat,sclon,ascdes,pre_cal1,pre_cal2,
c     *post_cal1,post_cal2,sv_prec)


      call getscandata(inp_frame_id,scan_fname,status,
     *seg_id,styr,
     *stdoy,sthr,stmin,stsec,stusec,edyr,eddoy,edhr,edmin,edsec,
     *edusec,start_addr,end_addr,
     *ascdes,pre_cal1,pre_cal2,
     *post_cal1,post_cal2,scan_vers,istatus)
      scanner_version=scan_vers         ! JMS 9-10-96
      if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return

      write (*,*) 'Back from getscandata'
      write (*,*) 'ascdes = ',ascdes
      write (*,*) 'start_addr = ',start_addr
      write (*,*) 'end_addr = ',end_addr



      write (*,*) 'In get_ldr, state vector precision = ',sv_prec

c  File descriptor record     JMS 2-25-96
      if (proj.eq.3) then
        n_map_proj=1                     ! 7/22/97 ATCT has map projection
      else
        n_map_proj=1
      endif
      if (terr_corr_flag.eq.0) then
        n_dem_desc=0                     ! No DEM record if no terrain corr
      else
        n_dem_desc=1
      endif

c  Facility data record
      proc_version=sw_ver
      ASC_DESC=ascdes
      pre_cal1_pow=pre_cal1
      pre_cal2_pow=pre_cal2
      post_cal1_pow=post_cal1
      post_cal2_pow=post_cal2
      nburst=burst_end-burst_start+1
      kb1=nburst/2+burst_start-1
      kb2=kb1+1
      if(mod(nburst,2).eq.1) kb1=kb2
      ssar_roll_ang=roll(kb1)

c      if (ascdes.eq.'A') then
c        asc_des='ASCENDING'
c      elseif (ascdes.eq.'D') then
c        asc_des='DESCENDING'
c      endif
      if (ascbuf(1).eq.'A') then
        asc_des='ASCENDING'
      elseif (ascbuf(1).eq.'D') then
        asc_des='DESCENDING'
      endif
      write (*,*) 'asc_des = ',asc_des

c  2-15-96 JMS modified datatake id to use rev from scan result instead of 
c  orbit number which seems to no longer be in key_pp
      write (*,*) 'encode 1 in get_ldr'
      write(unit=rr,fmt=1000) rev
      write (*,*) 'encode 2 in get_ldr'
c      encode(5,1000,rr) orbit_no
      write(unit=nn,fmt=1020) orbit_seg
c      encode(2,1020,nn) orbit_seg
      write (*,*) 'back from encode 2 in get_ldr'
      write (*,*) 'JEFFS in get_ldr'
      write (*,*) 'alpha1 = ',alpha1
      write (*,*) 'alpha2 = ',alpha2
      write (*,*) 'alpha3 = ',alpha3
      do 30 i=1,5
      if (rrbuf(i).eq.' ') rrbuf(i)='0'
   30 continue
      do 40 i=1,2
      if (nnbuf(i).eq.' ') nnbuf(i)='0'
   40 continue
      dthead='R1'                               !  JMS changed 4-4-96
      rrdt=rr
      nndt=nn
      do 50 i=10,16
      dtake_id(i)=' '
   50 continue
      datatake_id=dt_id
      strlen=64

c  Data rate added by JMS 2-12-96
      if (act_id.eq.'RES') then
        data_rate=85.0
      else
        data_rate=105.0
      endif

      call corn_cent(c1_frm,c2_frm,terr_corr_flag,
     *burst_end,nbeams,gha,sv_ref_time,sv_x_pos,sv_y_pos,
     *sv_z_pos,sv_x_vel,sv_y_vel,sv_z_vel,t_burst,alpha1,
     *alpha2,alpha3,r_bip,coef_fd,sc_time_sec,sclat,sclon,
     *scyr,scdoy,schr,scmin,scsec,scusec,nslat,
     *nslon,nelat,nelon,fslat,fslon,felat,felon,
     *ns_h,ne_h,fs_h,fe_h,gha_time,dem_ellip,
     *dlon_topo,dlat_topo,topo,avg_terrain,rlocal_mean,
     *scmon,scdom)
      write (*,*) 'back from corn_cent'
      call sctim_str(scyr,scmon,scdom,schr,scmin,scsec,
     *scusec,scstr)
      inp_sctim=scstr                              !  JMS 9-7-96
      write (*,*) 'sc_time_sec = ',sc_time_sec
      call se_time(burst_start,burst_end,t_b,t_burst,
     *np_v,prf,styr,stdoy,sthr,stmin,stsec,stusec,
     *edyr,eddoy,edhr,edmin,edsec,edusec)

c
c qdn: Add new function to override the value of corners for valid data
c
c qdn: 4/3/97 computing the center image as well
c

      write(*,*) ' Before refining the corners'
      write(*,*) 'nslat=',nslat,' nslon=',nslon
      write(*,*) 'fslat=',fslat,' fslon=',fslon
      write(*,*) 'nelat=',nelat,' nelon=',nelon
      write(*,*) 'felat=',felat,' nslon=',felon
c     if(proj.eq.proj) then

      if(proj.eq.atct) then
         write(*,*) 'Along track-Cross track projection'
         write(*,*) 'Input for find_corner is ',in_fname
         write(*,*) ' nsamples=',nsamples,' nlines=',nlines
         xorig = min(c2_frm(1,1),c2_frm(1,2),c2_frm(2,1),c2_frm(2,2))
         yorig = min(c1_frm(1,1),c1_frm(1,2),c1_frm(2,1),c1_frm(2,2))
         write(*,*) 'before find_corners xorig=',xorig,yorig
         call find_corners(in_fname,nsamples,nlines,
     *            xorig,yorig,c2_pxl,c1_pxl,
     *            c2_frm,c1_frm,
     *            xc2_frm,xc1_frm)
         write(*,*)' after find_corners c1_frm=',c1_frm
         write(*,*)' after find_corners c2_frm=',c2_frm
         write(*,*)' after find_corners xc1_frm=',xc1_frm
         write(*,*)' after find_corners xc2_frm=',xc2_frm

         xc1_min = min(xc1_frm(1,1),xc1_frm(1,2),xc1_frm(2,1),xc1_frm(2,2))
         xc1_max = max(xc1_frm(1,1),xc1_frm(1,2),xc1_frm(2,1),xc1_frm(2,2))
         xc2_min = min(xc2_frm(1,1),xc2_frm(1,2),xc2_frm(2,1),xc2_frm(2,2))
         xc2_max = max(xc2_frm(1,1),xc2_frm(1,2),xc2_frm(2,1),xc2_frm(2,2))
         xc1_center = (xc1_min+xc1_max)/2.0d0
         xc2_center = (xc2_min+xc2_max)/2.0d0

         call ac_to_ll(xc1_frm(1,1),xc2_frm(1,1),xlat,nslat,nslon,
     *                 alpha1,alpha2,alpha3,rlocal_mean)
         call ac_to_ll(xc1_frm(1,2),xc2_frm(1,2),xlat,fslat,fslon,
     *                 alpha1,alpha2,alpha3,rlocal_mean)
         call ac_to_ll(xc1_frm(2,1),xc2_frm(2,1),xlat,nelat,nelon,
     *                 alpha1,alpha2,alpha3,rlocal_mean)
         call ac_to_ll(xc1_frm(2,2),xc2_frm(2,2),xlat,felat,felon,
     *                 alpha1,alpha2,alpha3,rlocal_mean)
         call ac_to_ll(xc1_center,xc2_center,xlat,sclat,sclon,
     *                 alpha1,alpha2,alpha3,rlocal_mean)
      else
         write(*,*) 'P1/P2 projection',proj,' zone=',zone
         write(*,*) 'slat=',slat,' along0=',along0
         write(*,*) 'plat1=',plat1,'plat2=',plat2
         write(*,*) 'Input for find_corner is ',in_fname
         hem='n'
         do k1=1,2      ! Convert from c1,c2 to P1,P2 
            do k2=1,2
               call ac_to_ll(c1_frm(k1,k2),c2_frm(k1,k2),xlat,
     *              xlat_d,xlon,alpha1,alpha2,alpha3,rlocal_mean)
               if(xlat_d.lt.0) hem='s'
               call ll_to_proj(xlat_d,xlon,
     *              p1_from_c1(k1,k2),p2_from_c2(k1,k2),
     *              proj,zone,slat,along0,plat1,plat2,
     *              lat_orig,long_orig)
            end do
         end do
         write(*,*)' from calculateion= p1_from_c1',p1_from_c1
         write(*,*)' from calculation=  p2_from_c2',p2_from_c2
         xorig = min(p1_fm(1,1),p1_fm(1,2),p1_fm(2,1),p1_fm(2,2))
         yorig = min(p2_fm(1,1),p2_fm(1,2),p2_fm(2,1),p2_fm(2,2))

         call find_corners(in_fname,nsamples,nlines,
     *           xorig,yorig,p2_pxl,p1_pxl,
     *           p1_from_c1,p2_from_c2,
     *           xp1_fm, xp2_fm)
c        do k1=1,2
c            do k2=1,2
c              xp1_fm(k1,k2) = p1_from_c1(k1,k2)
c              xp2_fm(k1,k2) = p2_from_c2(k1,k2)
c            end do
c        end do
         write(*,*)" orig corner location p1=",p1_from_c1
         write(*,*)" orig corner location p2=",p2_from_c2
         write(*,*)" corner location p1=",xp1_fm
         write(*,*)" corner location p2=",xp2_fm

         xp1_min = min(xp1_fm(1,1),xp1_fm(1,2),xp1_fm(2,1),xp1_fm(2,2))
         xp1_max = max(xp1_fm(1,1),xp1_fm(1,2),xp1_fm(2,1),xp1_fm(2,2))
         xp2_min = min(xp2_fm(1,1),xp2_fm(1,2),xp2_fm(2,1),xp2_fm(2,2))
         xp2_max = max(xp2_fm(1,1),xp2_fm(1,2),xp2_fm(2,1),xp2_fm(2,2))
         xp1_center = (xp1_min+xp1_max)/2.0d0
         xp2_center = (xp2_min+xp2_max)/2.0d0

         call proj_to_ll(xp1_fm(1,1),xp2_fm(1,1),xlat,nslat,nslon,
     *           proj,zone,slat,along0,hem,plat1,plat2,
     *           lat_orig,long_orig)
         call proj_to_ll(xp1_fm(1,2),xp2_fm(1,2),xlat,fslat,fslon,
     *           proj,zone,slat,along0,hem,plat1,plat2,
     *           lat_orig,long_orig)
         call proj_to_ll(xp1_fm(2,1),xp2_fm(2,1),xlat,nelat,nelon,
     *           proj,zone,slat,along0,hem,plat1,plat2,
     *           lat_orig,long_orig)
         call proj_to_ll(xp1_fm(2,2),xp2_fm(2,2),xlat,felat,felon,
     *           proj,zone,slat,along0,hem,plat1,plat2,
     *           lat_orig,long_orig)
         call proj_to_ll(xp1_center,xp2_center,xlat,sclat,sclon,
     *           proj,zone,slat,along0,hem,plat1,plat2,
     *           lat_orig,long_orig)
      end if

c     end if
      write(*,*) ' After refining the corners'
      write(*,*) 'sclat=',sclat,' sc_lon=',sclon
      write(*,*) 'nslat=',nslat,' nslon=',nslon
      write(*,*) 'fslat=',fslat,' fslon=',fslon
      write(*,*) 'nelat=',nelat,' nelon=',nelon
      write(*,*) 'felat=',nslat,' nslon=',felon

      pro_lat=sclat
      pro_long=sclon
      center_LAT=sclat
      center_LON=sclon
      near_sta_LAT=nslat
      near_sta_LON=nslon
      near_end_LAT=nelat
      near_end_LON=nelon
      far_sta_LAT=fslat
      far_sta_LON=fslon
      far_end_LAT=felat
      far_end_LON=felon
      write (*,*) ' in get_ldr 1'


      write (*,*) 'JEFFS in get_ldr about to call getcaldata'
      write (*,*) 'JEFFS in get_ldr about to call getcaldata'
      write (*,*) 'calib_fname = ',calib_fname
        call getcaldata(calib_fname,calib_noise_fact,lin_conv_fact,
     *  calib_offset_conv_fact,abs_coeff,noise_pro,noise_pvs,noise_del,
     *  rng_res,az_res,rng_pslr,az_pslr,rng_islr,az_islr,rng_amb,
     *  az_amb,calib_ori_err,skew_err,cross_s_err,along_s_err,
     *  cross_l_err,along_l_err,iso_ratio,rel_radio_acc,calib_dyn_rng,
     *  calib_snr,calib_npix,calib_elang,calib_incang,calib_slrng,
     *  calib_radgain,calib_nvals,calib_temp,calib_tempgain,
     *  calib_status,rad_v_stat,gain_stat,cnv_fac_stat,noise_stat,
     *  im_qual_stat,geo_anal_stat,rad_acc_stat,istatus)
      if ((cnv_fac_stat.eq.-1).or.(rad_acc_stat.eq.-1)) then
        write (*,*) 'HELLO new format'
        call getcal_new(calib_fname,calib_noise_fact,lin_conv_fact,
     *  calib_offset_conv_fact,abs_coeff,noise_pro,noise_pvs,noise_del,
     *  rng_res,az_res,rng_pslr,az_pslr,rng_islr,az_islr,rng_amb,
     *  az_amb,calib_ori_err,skew_err,cross_s_err,along_s_err,
     *  cross_l_err,along_l_err,iso_ratio,rel_radio_acc,calib_status,
     *  pvs_obj_stat,cal_fac_stat,noise_stat,im_qual_stat,
     *  geo_anal_stat,calstat,calcom,istatus)
      else
        write (*,*) 'HELLO old format'
      endif
      if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return

c qdn 11/13/97 add the radiometric correction vector for SWB
      if(beam_mode.eq.2) then
         do i=1,rad_n_samp
            lookup_tab(i) = SWB_pat(i)
         end do
      end if

c Change below made because proc_gain being passed in as linear, not db
      lin_gain=proc_gain                                       !  JMS changed 9-26-96
      noise_fact=calib_noise_fact*lin_gain                     !  JMS changed 5-10-96
      linear_conv_fact=lin_conv_fact/lin_gain                  !  JMS changed 5-10-96
      offset_conv_fact=calib_offset_conv_fact
      write (*,*) 'Back from getcaldata'
      write (*,*) 'noise_fact = ',calib_noise_fact
      write (*,*) 'lin_conv_fact = ',lin_conv_fact
      write (*,*) 'offset_conv_fact = ',calib_offset_conv_fact
      write (*,*) 'abs_coeff = ',abs_coeff
      write (*,*) 'noise_pro = ',noise_pro
      write (*,*) 'noise_pvs = ',noise_pvs
      write (*,*) 'noise_del = ',noise_del
      write (*,*) 'rng_islr = ',rng_islr
      write (*,*) 'cross_s_err = ',cross_s_err
      write (*,*) 'rel radio acc = ',rel_radio_acc
      write (*,*) 'ori err = ',calib_ori_err
      write (*,*) 'skew err = ',skew_err
      write (*,*) 'along_l_err = ',along_l_err
      write (*,*) 'along_s_err = ',along_s_err
      write (*,*) 'cross_l_err = ',cross_l_err
      write (*,*) 'cross_s_err = ',cross_s_err
      write (*,*) 'iso ratio = ',iso_ratio
      write (*,*) 'rel_radio_acc = ',rel_radio_acc
      write (*,*) 'az amb = ',az_amb
      write (*,*) 'rg amb = ',rng_amb
      write (*,*) 'az res = ',az_res
      write (*,*) 'rg res = ',rng_res
      write (*,*) 'az pslr = ',az_pslr
      write (*,*) 'rg pslr = ',rng_pslr
      write (*,*) 'az islr = ',az_islr
      write (*,*) 'rg islr = ',rng_islr

      write (*,*) ' in get_ldr 2'
      img_rec_cnt=nlines+1
      npix=left_fill_cnt(1)+image_pix_cnt(1)+right_fill_cnt(1)
      img_1st_rec_len=npix+192
      img_max_rec_len=npix+192

      image_file=ceos_fname
      image_file_buf(17)='.'
      image_file_buf(18)='D'
      do i=19,64
        image_file_buf(i)=' '
      enddo
c      call sgmt(styr,stdoy,sthr,stmin,stsec,stusec,start_time,
c     *dummy_str_1,dummy_str_2)
c      call sgmt(edyr,eddoy,edhr,edmin,edsec,edusec,end_time,
c     *dummy_str_1,dummy_str_2)
c      call sgmt(scyr,scdoy,schr,scmin,scsec,scusec,center_time,
c     *dummy_str_1,dummy_str_2)
      call gmt_str(styr,stdoy,sthr,stmin,stsec,stusec,start_time)
      call gmt_str(edyr,eddoy,edhr,edmin,edsec,edusec,end_time)
      call gmt_str(scyr,scdoy,schr,scmin,scsec,scusec,center_time)

c  JMS added 4-10-96 for attitude data record
      att_gmt_day(1)=stdoy
      att_gmt_day(2)=scdoy
      att_gmt_day(3)=eddoy
      att_gmt_sec(1)=(3600*sthr+60*stmin+stsec)*1000+stusec/1000
      att_gmt_sec(2)=(3600*schr+60*scmin+scsec)*1000+scusec/1000
      att_gmt_sec(3)=(3600*edhr+60*edmin+edsec)*1000+edusec/1000

      if (calib_status.eq.-1) then
        noise_flr=noise_flr_def
      else
        noise_flr=noise_pro
      endif
      noise_pwr=10.0d0**(noise_flr/10.0d0)
      total_pwr=lin_conv_fact*(meansamp**2+stdvsamp**2)
      if (total_pwr.gt.noise_pwr) then       ! 1-6-96 put in better check later
        signal_pwr=total_pwr-noise_pwr
        s_n_r=10.0d0*log10(signal_pwr/noise_pwr)
      else
        s_n_r=0.0d0
      endif
      dqs_snr=s_n_r

      write (*,*) ' in get_ldr 3'
c  JMS 2-12-96 added
      nesz=noise_pvs
      azi_ambig=az_amb
      rng_ambig=rng_amb
      dqs_azi_res=az_res
      dqs_rng_res=rng_res
      alt_locerr=along_l_err
      crt_locerr=cross_l_err
      alt_scale=along_s_err
      crt_scale=cross_s_err
      dis_skew=skew_err
      ori_err=calib_ori_err
      if (az_islr.gt.rng_islr) then    ! change lt to gt JMS 7-19-96
        islr=az_islr
      else
        islr=rng_islr
      endif
      if (az_pslr.gt.rng_pslr) then    ! change lt to gt JMS 7-19-96
        pslr=az_pslr
      else
        pslr=rng_pslr
      endif
      stime=time_()
      call ibm_gmtime(stime,tarray)
      cor_year=tarray(6)+1900
      cor_doy=tarray(8)+1
      cor_hr=tarray(3)
      cor_min=tarray(2)
      cor_sec=tarray(1)
      cor_usec=0
      call gmt_str(cor_year,cor_doy,cor_hr,cor_min,cor_sec,cor_usec,
     *prod_creat_time)
      call sgmt(cor_year,cor_doy,cor_hr,cor_min,cor_sec,cor_usec,
     *dummy_str_1,corr_time,corr_yr)
      corr_year=corr_yr
      corr_GMT=corr_time
      prod_time=prod_creat_time            ! JMS 2-25-96 
      pmf_time=prod_creat_time             ! JMS 2-25-96 

c  2-9-96 JMS added
      fac_site_name=sitename
      site_name=sitename
      write (*,*) 'In get_ldr, site_name = ',site_name
c  JMS 2-12-96
      if (beam_mode.eq.1) then
        mode='SWA'
      else if (beam_mode.eq.2) then
        mode='SWB'
      else if (beam_mode.eq.3) then
        mode='SNA'
      else if (beam_mode.eq.4) then
        mode='SNB'
      endif
c  JMS added 2-25-96 for pmf
      ldr_rec_cnt=10                                        ! Basic w/o map proj or DEM
c     if (proj.ne.3) ldr_rec_cnt=ldr_rec_cnt+1              ! map projection record added
      ldr_rec_cnt=ldr_rec_cnt+1                             ! map projection record added
      if (terr_corr_flag.ne.0) ldr_rec_cnt=ldr_rec_cnt+1    ! DEM record
      ldr_max_rec_len=5120

      pmf_media_id=frq_media_id
      write (*,*) 'In get_ldr , pmf_media_id = ',pmf_media_id
      media_type=frq_media_type
      media_loc=frq_media_loc
      if (product_type.eq.prod_1) then
        if (proj.eq.3) then 
          prod_type='GRF'
          dataset='RADARSAT-1 SCANSAR STANDARD GRF PRODUCT'
        else
          if (terr_corr_flag.eq.0) then
            prod_type='GCD'
            dataset='RADARSAT-1 SCANSAR STANDARD GCD PRODUCT'
          else
            prod_type='GTC'
            dataset='RADARSAT-1 SCANSAR STANDARD GTC PRODUCT'
          endif
        endif
      else if (product_type.eq.prod_pvs_2.or.
     *                 product_type.eq.prod_pvs_3) then
        prod_type='GRF'
        dataset='RADARSAT-1 SCANSAR MULTILOOK PRODUCT'
      endif
      dest='CP'
      pmf_source='SSP'
      num_rec=1
      proc_ver=sw_ver
      pmf_snr=s_n_r
      msg_type='SAR_FRAME_METADATA'
      pmf_radio_acc=rel_radio_acc
      image_file_path=image_path
      ldr_file=leader_path
      prod_creat='SSP'
      product_id=prod_id

      product_path=image_path                           ! JMS 2-26-96

c  Strip path information from output file name
c  Begin by searching backwards from end of string for a "/"
      j=256
      do 640 i=1,256
      xc=fbuf1(j)
      if (xc.eq."/") go to 650
      j=j-1
  640 continue
  650 continue
c  Now copy path to buffer
      do 670 i=1,j
      fbuf2(i)=fbuf1(i)
  670 continue
      do 680 i=(j+1),256
      fbuf2(i)=' '
  680 continue
      loc_arch_dir=arch_path


      write (*,*) 'pmf temp = ',pmf_temp_file
      write (*,*) 'pmf file = ',pmf_file
      write (*,*) 'About to call pmf'
      write (*,*) pmf_temp_file
      write (*,*) pmf_file
      write (*,*) job_id
      write (*,*) platform
      write (*,*) sensor
      write (*,*) rev
      write (*,*) sequence
      write (*,*) act_id
      write (*,*) media_id
      write (*,*) media_type
      write (*,*) media_loc
      write (*,*) recorder_id
      write (*,*) station_id
      write (*,*) frame_mode
      write (*,*) site_name
      write (*,*) mode
      write (*,*) seg_id
      write (*,*) dataset
      write (*,*) loc_arch_dir
      write (*,*) product_id
      write (*,*) image_file
      write (*,*) image_file_path
      call pmf(pmf_temp_file,pmf_file,job_id,platform,sensor,rev,
     *sequence,act_id,pmf_media_id,media_type,media_loc,recorder_id,
     *station_id,frame_mode,site_name,mode,seg_id,dataset,
     *loc_arch_dir,product_id,image_file,image_file_path,
     *ldr_file,inp_frame_id,subframe_id,
     *start_addr,end_addr,img_rec_cnt,img_1st_rec_len,
     *img_max_rec_len,ldr_max_rec_len,ldr_rec_cnt,prod_creat,
     *prod_type,start_time,end_time,sclat,sclon,center_time,nslat,
     *nslon,nelat,nelon,fslat,fslon,felat,felon,ascdes,
     *proc_ver,sv_prec,pmf_snr,pmf_radio_acc,prod_time,pmf_time,
     *msg_type,dest,pmf_source,num_rec,istatus)
      if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return

      write (*,*) ' in get_ldr 4'
      call aux_to_ldr(prf,np_air,window_start_time,
     *burst_start,burst_end,np,roll_rate,yaw_rate,
     *pitch_rate,nbeams,replica_agc,
     *lna_temp,subsystem_temp,protector_temp,rx_agc,
     *cal_sys_temp,repl_pow,aux_yaw_valid,aux_roll_valid,
     *aux_pitch_valid)

      write (*,*) ' in get_ldr 5'
      call keypp_2_ldr(chirp_rate,fs,plen,burst_start,
     *burst_end,prf,gain_rec,nbeams,np,np_v,avg_terrain,
     *proc_gain,p1_pxl,p2_pxl,orbit_no,peak_bm_ang,gha,
     *sv_type,fdc_est_mode,rgc_est_mode,res_prod,proj,
c     *sclon,ps_lat,ps_lon,lamb_lat_n,lamb_lat_s,rev)    !  changed JMS 7-1-96
     *sclon,slat,along0,plat1,plat2,rev)

      write (*,*) ' in get_ldr 6'
      call ceos_calc(nlines,left_fill_cnt,image_pix_cnt,
     *right_fill_cnt,scyr,scdoy,schr,scmin,scsec,scusec,
     *sclat,sclon,sc_time_sec,linec,burst_start,burst_end,x_sc,
     *y_sc,z_sc,v_x_sc,v_y_sc,v_z_sc,pitch_o,yaw_o,roll_o,pitch,
     *yaw,roll,c1_pxl,c2_pxl,c1_flm,c2_flm,fd_low_near,
     *fd_low_far,fr_near,fr_far,i_ang_bip,look_ang,gha,
     *sv_ref_time,t_burst,np,np_air,r_low,r_high,prf,p_x,p_y,
     *p_z,fs,chirp_bw,nbeams,beam_mode,res_prod,r_cntr)

c  JMS 4-3-96   fill in beam id's in DSS
      if (beam_mode.eq.1) then
        dss_beam1='WD1'
        dss_beam2='WD2'
        dss_beam3='WD3'
        dss_beam4='ST7'
      else if (beam_mode.eq.2) then
        dss_beam1='WD1'
        dss_beam2='WD2'
        dss_beam3='ST5'
        dss_beam4='ST6'
      else if (beam_mode.eq.3) then
        dss_beam1='WD1'
        dss_beam2='WD2'
        dss_beam3=' '
        dss_beam4=' '
      else if (beam_mode.eq.4) then
        dss_beam1='WD2'
        dss_beam2='ST5'
        dss_beam3='ST6'
        dss_beam4=' '
      endif

c  JMS 4-4-96 added bit error rate to dqs and fac
      ber=bit_error_rate                   ! dqs
      bit_err_rate=bit_error_rate          ! fac

c  JMS 5-10-96 fill in data input source in dss
      data_inpsrc=frq_media_id

c  JMS 5-9-96 fill in utm zone signature
      write (unit=utm_str,fmt=1040) utm_zone
      do i=1,4
        if (utm_buf(i).eq.' ') utm_buf(i)='0'
      enddo
      utm_zone_sig=utm_str

c  JMS 5-9-96 fill in map projection corner lat lon and height
c  JMS 9-26-96 changed to use projection coordinates
      write (*,*) 'slat = ',slat
      write (*,*) 'along0 = ',along0
      write (*,*) 'proj = ',proj
c ************************************************************
	  if (proj.eq.ps) then
	   hem = 'n'
	   do k3=1,2
	      do k4=1,2
		 call ac_to_ll(c1_frm(k3,k4),c2_frm(k3,k4),
     .		 tmp_latc,tmp_latc_d,tmp_lonc,
     .		 alpha1,alpha2,alpha3,rlocal_mean)
		 if(tmp_latc_d.lt.0) hem = 's'
	      end do
	   end do
	  end if
c ************************************************************
      call proj_to_ll(p1_fm(1,1),p2_fm(1,1),dummy_lat,proj_nelat,
     *proj_nelon,proj,zone,slat,along0,hem,plat1,plat2,lat_orig,
     *long_orig)
      call proj_to_ll(p1_fm(1,2),p2_fm(1,2),dummy_lat,proj_felat,
     *proj_felon,proj,zone,slat,along0,hem,plat1,plat2,lat_orig,
     *long_orig)
      call proj_to_ll(p1_fm(2,1),p2_fm(2,1),dummy_lat,proj_nslat,
     *proj_nslon,proj,zone,slat,along0,hem,plat1,plat2,lat_orig,
     *long_orig)
      call proj_to_ll(p1_fm(2,2),p2_fm(2,2),dummy_lat,proj_fslat,
     *proj_fslon,proj,zone,slat,along0,hem,plat1,plat2,lat_orig,
     *long_orig)
      write(*,*)'In project cord nelat=',proj_nelat,'nelon=',proj_nelon
      write(*,*)'In project cord felat=',proj_felat,'felon=',proj_felon
      write(*,*)'In project cord nslat=',proj_nslat,'nslon=',proj_nslon
      write(*,*)'In project cord nelat=',proj_fslat,'nelon=',proj_fslon
c      if (proj.ne.ps) then     ! JMS 10-25-96
c        corner_ll(1)=proj_nelat
c        corner_ll(2)=proj_nelon
c        corner_ll(3)=proj_felat
c        corner_ll(4)=proj_felon
c        corner_ll(5)=proj_fslat
c        corner_ll(6)=proj_fslon
c        corner_ll(7)=proj_nslat
c        corner_ll(8)=proj_nslon
c      else
         corner_ll(1)=proj_nslat
         corner_ll(2)=proj_nslon
         corner_ll(3)=proj_fslat
         corner_ll(4)=proj_fslon
         corner_ll(5)=proj_felat
         corner_ll(6)=proj_felon
         corner_ll(7)=proj_nelat
         corner_ll(8)=proj_nelon
c  *** added to correct problem with missing proj coordinates  12/18/96 
        corner_ne(1)=p2_fm(2,1)
        corner_ne(2)=p1_fm(2,1)
        corner_ne(3)=p2_fm(2,2)
        corner_ne(4)=p1_fm(2,2)
        corner_ne(5)=p2_fm(1,2)
        corner_ne(6)=p1_fm(1,2)
        corner_ne(7)=p2_fm(1,1)
        corner_ne(8)=p1_fm(1,1)
c      endif
      if(proj.eq.atct) then
         corner_ne(1)=c2_frm(2,1)
         corner_ne(2)=c1_frm(2,1)
         corner_ne(3)=c2_frm(2,2)
         corner_ne(4)=c1_frm(2,2)
         corner_ne(5)=c2_frm(1,2)
         corner_ne(6)=c1_frm(1,2)
         corner_ne(7)=c2_frm(1,1)
         corner_ne(8)=c1_frm(1,1)

         call ac_to_ll(c1_frm(1,1),c2_frm(1,1),xlat,xnslat,xnslon,
     *                 alpha1,alpha2,alpha3,rlocal_mean)
         call ac_to_ll(c1_frm(1,2),c2_frm(1,2),xlat,xfslat,xfslon,
     *                 alpha1,alpha2,alpha3,rlocal_mean)
         call ac_to_ll(c1_frm(2,1),c2_frm(2,1),xlat,xnelat,xnelon,
     *                 alpha1,alpha2,alpha3,rlocal_mean)
         call ac_to_ll(c1_frm(2,2),c2_frm(2,2),xlat,xfelat,xfelon,
     *                 alpha1,alpha2,alpha3,rlocal_mean)

         corner_ll(1)=xnslat
         corner_ll(2)=xnslon
         corner_ll(3)=xfslat
         corner_ll(4)=xfslon
         corner_ll(5)=xfelat
         corner_ll(6)=xfelon
         corner_ll(7)=xnelat
         corner_ll(8)=xnelon


      end if
      terr_height(1)=ne_h
      terr_height(2)=fe_h
      terr_height(3)=fs_h
      terr_height(4)=ns_h

c  JMS 5-9-96 fill in dqs item 23 and fac item 102
      db(1)=rel_radio_acc
      fac_SNR=s_n_r

c  JMS fill in fac items 74-75 and 103
      induced_azimuth=sqrt(along_s_err**2+skew_err**2)
      induced_range=sqrt(cross_s_err**2+skew_err**2)
      est_noise_flr=noise_pro

      write (*,*) ' in get_ldr 7'

c  JMS 5-6-96 fix error in fac record
      actual_lines=act_lines

c  JMS 8-12-96 data set summary record
      ver_id=sw_ver

c  JMS 9-10-96 add decode_vers, scan results file and calib file to DSS
c  scanner vers was added immediately after call to getscandata
      decode_version=rds_vers

c  Strip path information from calib file name
c  Begin by searching backwards from end of string for a "/"
      do 730 i=1,256
      fbuf4(i)=' '
  730 continue
      calfile_whole=cali_file
      j=256
      do 740 i=1,256
      xc=fbuf3(j)
      if (xc.eq."/") go to 750
      j=j-1
  740 continue
  750 continue
c  Now copy stripped file name to buffer
      do 770 i=1,(256-j)
      k=j+i
      fbuf4(i)=fbuf3(k)
  770 continue
      cal_params_file=calfile

c  Strip path information from scan file name
c  Begin by searching backwards from end of string for a "/"
      do 830 i=1,256
      fbuf6(i)=' '
  830 continue
      scanfile_whole=scan_file
      j=256
      do 840 i=1,256
      xc=fbuf5(j)
      if (xc.eq."/") go to 850
      j=j-1
  840 continue
  850 continue
c  Now copy stripped file name to buffer
      do 870 i=1,(256-j)
      k=j+i
      fbuf6(i)=fbuf5(k)
  870 continue
      scan_results_file=scanfile

c  9-13-96 dss sys id
      sys_id=subsystem_name
      write (*,*) 'subsystem_name = ',subsystem_name

      write (*,*) 'proj = ',proj

c 10-3-96 JMS 2 new fields in dqs
      cal_status=calstat
      cal_comment=calcom

      call write_ceos_ldr(ceos_temp_file,ldr_fname,
     *terr_corr_flag,proj,istatus)

      write (*,*) ' in get_ldr 8'
      return
 1000 format (i5)
 1020 format (i2)
 1040 format (i4)
      end



c  This subroutine copies data directly from the variables defined in
c  auxiliary.inc to the applicable common blocks in ceos.inc.

      subroutine aux_to_ldr(prf,np_air,window_start_time,
     *burst_start,burst_end,np,roll_rate,yaw_rate,
     *pitch_rate,nbeams,replica_agc,
     *lna_temp,subsystem_temp,protector_temp,rx_agc,
     *cal_sys_temp,repl_pow,aux_yaw_valid,aux_roll_valid,
     *aux_pitch_valid)

      implicit none
      include 'ssp2_const.inc'
      include 'ceos.inc'
      integer i,kbcent,kb
      integer nburst,kb1,kb2,nbeams
      real*4 airtime,sf_rnge_gate
      real*4 cnt_roll_rate,cnt_pitch_rate,cnt_yaw_rate

      integer burst_start,burst_end
      integer np(burst_max),np_air(burst_max)
      real*8 prf(burst_max)
      real*8 roll_rate(burst_max),yaw_rate(burst_max)
      real*8 pitch_rate(burst_max)
      real*8 window_start_time(1600),replica_agc(1600)
      real*8 lna_temp(1600),subsystem_temp(1600)
      real*8 protector_temp(1600),rx_agc(1600)
      real*8 cal_sys_temp(1600),repl_pow
      character*10 aux_yaw_valid(1600)                 !  JMS 5-10-96
      character*10 aux_roll_valid(1600)                !  JMS 5-10-96
      character*10 aux_pitch_valid(1600)               !  JMS 5-10-96

c   For now (11-12-95) load dummy value into window start time
c   until we start reading aux file
c      do 100 i=1,burst_end
c      window_start_time(i)=0.0001708075d0
c  100 continue

      kbcent=(burst_start+burst_end)/2
      airtime=float(np_air(kbcent))/prf(kbcent)
      sf_rnge_gate=window_start_time(kbcent)+1000000.0*airtime
      data_win_pos=window_start_time(kbcent)      ! JMS 2-12-96
      rng_gate=sf_rnge_gate
      range_gate_del=sf_rnge_gate

      kb=1
      airtime=float(np_air(kb))/prf(kb)
      sf_rnge_gate=window_start_time(kb)+1000000.0*airtime
      dss_rng_gate1=sf_rnge_gate

      kb=2
      airtime=float(np_air(kb))/prf(kb)
      sf_rnge_gate=window_start_time(kb)+1000000.0*airtime
      dss_rng_gate2=sf_rnge_gate

      if (nbeams.gt.2) then
        kb=3
        airtime=float(np_air(kb))/prf(kb)
        sf_rnge_gate=window_start_time(kb)+1000000.0*airtime
        dss_rng_gate3=sf_rnge_gate
      endif

      if (nbeams.gt.3) then
        kb=4
        airtime=float(np_air(kb))/prf(kb)
        sf_rnge_gate=window_start_time(kb)+1000000.0*airtime
        dss_rng_gate4=sf_rnge_gate
      endif

      nburst=burst_end-burst_start+1
      kb1=nburst/2+burst_start-1
      kb2=kb1+1
      if (mod(nburst,2).eq.1) kb1=kb2
      cnt_roll_rate=(roll_rate(kb1)+roll_rate(kb2))/2.0d0
      cnt_yaw_rate=(yaw_rate(kb1)+yaw_rate(kb2))/2.0d0
      cnt_pitch_rate=(pitch_rate(kb1)+pitch_rate(kb2))/2.0d0
      fac_roll_rate=cnt_roll_rate
      fac_pitch_rate=cnt_pitch_rate
      fac_yaw_rate=cnt_yaw_rate
      att_roll_rate(1)=roll_rate(burst_start)
      att_pitch_rate(1)=pitch_rate(burst_start)
      att_yaw_rate(1)=yaw_rate(burst_start)
      att_roll_rate(2)=cnt_roll_rate
      att_pitch_rate(2)=cnt_pitch_rate
      att_yaw_rate(2)=cnt_yaw_rate
      att_roll_rate(3)=roll_rate(burst_end)
      att_pitch_rate(3)=pitch_rate(burst_end)
      att_yaw_rate(3)=yaw_rate(burst_end)

c  JMS added 2-14-96
      repl_agc=replica_agc(kb1)
      temp_rx_lna=lna_temp(kb1)
      temp_rx_sub=subsystem_temp(kb1)
      temp_rx_prot=protector_temp(kb1)
      temp_cal_sys=cal_sys_temp(kb1)
      fac_rx_agc=rx_agc(kb1)
      fac_repl_pow=repl_pow

c  JMS added 5-10-96
      do i=1,3
      if (i.eq.1) kb=burst_start
      if (i.eq.2) kb=kb1
      if (i.eq.3) kb=burst_end
      if (aux_yaw_valid(kb).eq.'YES') then
        yaw_flag(i)=1
        yaw_rate_flag(i)=1
      else
        yaw_flag(i)=0
        yaw_rate_flag(i)=0
      endif
      if (aux_roll_valid(kb).eq.'YES') then
        roll_flag(i)=1
        roll_rate_flag(i)=1
      else
        roll_flag(i)=0
        roll_rate_flag(i)=0
      endif
      if (aux_pitch_valid(kb).eq.'YES') then
        pitch_flag(i)=1
        pitch_rate_flag(i)=1
      else
        pitch_flag(i)=0
        pitch_rate_flag(i)=0
      endif
      enddo

c  JMS added 5-13-96
      if (aux_yaw_valid(kb1).eq.'YES') then
        fac_yaw_flag=1
        fac_yaw_rate_flag=1
      else
        fac_yaw_flag=0
        fac_yaw_rate_flag=0
      endif
      if (aux_roll_valid(kb1).eq.'YES') then
        fac_roll_flag=1
        fac_roll_rate_flag=1
      else
        fac_roll_flag=0
        fac_roll_rate_flag=0
      endif
      if (aux_pitch_valid(kb1).eq.'YES') then
        fac_pitch_flag=1
        fac_pitch_rate_flag=1
      else
        fac_pitch_flag=0
        fac_pitch_rate_flag=0
      endif


      return
      end



c  This subroutine copies data directly from the variables defined in
c  key_pp.inc to the applicable common blocks in ceos.inc.

      subroutine keypp_2_ldr(chirp_rate,fs,plen,burst_start,
     *burst_end,prf,gain_rec,nbeams,np,np_v,avg_terrain,
     *proc_gain,p1_pxl,p2_pxl,orbit_num,peak_bm_ang,gha,
     *sv_type,fdc_est_mode,rgc_est_mode,res_prod,proj,sclon,
     *ps_lat,ps_lon,lamb_lat_n,lamb_lat_s,rev)


      implicit none
      include 'ssp2_const.inc'
      include 'ceos.inc'
      integer burst_cen,i,res_prod

      real*8 chirp_rate,fs,plen,avg_terrain,proc_gain,gha              
      integer burst_start,burst_end,nbeams,orbit_num
      real*8 prf(burst_max),gain_rec(burst_max)
      integer np(burst_max),np_v(burst_max),sv_type
      real*8 p1_pxl,p2_pxl,peak_bm_ang(4)
      real*4 p1_pix,p2_pix
      integer fdc_est_mode,rgc_est_mode
      real*8 sclon,ps_lat,ps_lon,lamb_lat_n,lamb_lat_s
      integer proj,rev

      character*1 orb_buf(12)
      character*12 orbit
      equivalence (orbit,orb_buf(1))

      if (res_prod.eq.75) then
        p1_pix=50.0
        p2_pix=50.0
      else if (res_prod.eq.150) then
        p1_pix=100.0
        p2_pix=100.0
      else if (res_prod.eq.600) then
        p1_pix=400.0
        p2_pix=400.0
      endif

      write (*,*) 'JEFFS in keypp_to_ldr'
      write (*,*) 'chirp_rate = ',chirp_rate
      ampl_coef(2)=chirp_rate
      fr=fs/1.0d06                                ! JMS 7-15-96
      rng_length=plen*1.0d06                      ! JMS 7-15-96
      burst_cen=(burst_start+burst_end)/2
      fac_PRF=prf(burst_cen)
      fac_gain=gain_rec(burst_cen)
      dss_prf1=prf(1)
      dss_prf2=prf(2)
      if (nbeams.gt.2) dss_prf3=prf(3)
      if (nbeams.gt.3) dss_prf4=prf(4)
      dss_no_beams=nbeams
      dss_tot_pls_burst=np(burst_cen)
      dss_val_pls_burst=np_v(burst_cen)
      wave_length=lambda
      frequency=fc/1.0d09                      ! JMS 7-15-96
      
      avg_ter_height=avg_terrain
      terrain_h=avg_terrain
      fac_proc_gain=int(10.0*dlog10(proc_gain))    !  JMS 9-26-96
      write (*,*) 'fac_proc_gain = ',fac_proc_gain
      write (*,*) 'proc_gain = ',proc_gain
      dss_line_spacing=p1_pix
      dss_pix_spacing=p2_pix
      azimuth_pixel=p1_pix
      range_pixel=p2_pix
      map_pix_spacing=p2_pix
      map_line_spacing=p1_pix
      map_desc='GEOCODED'
      nsp_stand_par(1)=-9999.99                   ! 8-12-96
      nsp_stand_par(2)=-9999.99                   ! 8-12-96
      nsp_stand_par(3)=-9999.99                   ! 8-12-96
      nsp_stand_par(4)=-9999.99                   ! 8-12-96
      nsp_stand_mer(1)=-9999.99                   ! 8-12-96
      nsp_stand_mer(2)=-9999.99                   ! 8-12-96
      nsp_stand_mer(3)=-9999.99                   ! 8-12-96
      nsp_desc='LAMBERT CONFORMAL'                ! 8-12-96

      write (*,*) 'proj = ',proj
      if (proj.eq.1) then                         ! UTM
        proj_desc='UTM'
        utm_stand_par(1)=sclon
      else if (proj.eq.2) then                    ! PS
        proj_desc='PS-SMM/I'
c  Note that as of 7-1-96 what is passed in is really slat and along0
        ups_cent_lat=ps_lat
        ups_cent_long=ps_lon
        ups_scale=1.0
      else if (proj.eq.3) then                    ! ATCT
        proj_desc='GROUND RANGE'
      else if (proj.eq.4) then                    ! LAMBERT
        proj_desc='LAMBERT'
        nsp_stand_par(1)=lamb_lat_s               ! 7-1-96
        nsp_stand_par(2)=lamb_lat_n               ! 7-1-96
      endif
      write (*,*) 'encode in keypp2ldr'
      write (unit=orbit,fmt=1000) rev
c      write (unit=orbit,fmt=1000) orbit_num
c      encode(8,1000,orb_buf) orbit_num
      write (*,*) 'back from encode in keypp2ldr'
      do 10 i=1,8
      if (orb_buf(i).eq.' ') orb_buf(i)='0'
   10 continue
      do 12 i=9,12
      orb_buf(i)=' '
   12 continue
      dss_orbit_num=orbit
      dss_azi_res=float(res_prod)
      dss_rng_res=float(res_prod)
      ele_sight=peak_bm_ang(1)
      hr_angle=gha                   !platform position record
      if (sv_type.eq.restituted) type_ephemeris='RES'
      if (sv_type.eq.predicted) type_ephemeris='PRE'
      dss_tot_pls_burst=np(burst_cen)
      dss_val_pls_burst=np_v(burst_cen)
      if (fdc_est_mode.eq.1) then
        clutter_lock='YES'
        clutterlock_flg='YES'              !  JMS 4-10-96
      else
        clutter_lock='NOT'
        clutterlock_flg='NOT'              !  JMS 4-10-96
      endif
      if (rgc_est_mode.eq.1) then
        auto_focus='YES'
        autofocus_flag='YES'               !  JMS 4-10-96
      else
        auto_focus='NOT'
        autofocus_flag='NOT'               !  JMS 4-10-96
      endif

      return
 1000 format(i8)
      end



c  This subroutine copies data directly from the variables defined in
c  key_pp.inc to the applicable common blocks in ceos.inc.

      subroutine keypp_2_ldr_flm(kburst,chirp_rate,fs,
     *plen,prf,gain_rec,nbeams,np,np_v,avg_terrain,
     *proc_gain,c1_pxl,c2_pxl,orbit_num,peak_bm_ang,gha,
     *sv_type,fdc_est_mode,rgc_est_mode)

      implicit none
      include 'ssp2_const.inc'
      include 'ceos.inc'
      integer kburst,i
      real*8 chirp_rate,fs,plen,avg_terrain,proc_gain,gha              
      integer nbeams,orbit_num
      real*8 prf(burst_max),gain_rec(burst_max)
      integer np(burst_max),np_v(burst_max),sv_type
      real*8 c1_pxl,c2_pxl,peak_bm_ang(4)
      real*4 c1_pix,c2_pix
      integer fdc_est_mode,rgc_est_mode
      character*1 orb_buf(12)
      character*12 orbit
      equivalence (orbit,orb_buf(1))

      c1_pix=100.0
      c2_pix=100.0
      ampl_coef(2)=chirp_rate
      fr=fs/1.0d06                                ! JMS 7-15-96
      rng_length=plen*1.0d06                      ! JMS 7-15-96
      fac_PRF=prf(kburst)
      fac_gain=gain_rec(kburst)
      dss_prf1=prf(1)
      dss_prf2=prf(2)
      if (nbeams.gt.2) dss_prf3=prf(3)
      if (nbeams.gt.3) dss_prf4=prf(4)
      dss_no_beams=nbeams
      dss_tot_pls_burst=np(kburst)
      dss_val_pls_burst=np_v(kburst)
      wave_length=lambda
      frequency=fc/1.0d09                    ! JMS 7-15-96

      avg_ter_height=avg_terrain
      terrain_h=avg_terrain
      fac_proc_gain=int(10.0*dlog10(proc_gain))         !  JMS 9-26-96
      dss_line_spacing=c1_pix
      dss_pix_spacing=c2_pix
      azimuth_pixel=c1_pix
      range_pixel=c2_pix
      map_pix_spacing=c2_pix
      map_line_spacing=c1_pix
      write (*,*) ' encode in keypp2ldr_flm'
      write (unit=orbit,fmt=1000) orbit_num
c      encode(8,1000,orb_buf) orbit_num
      write (*,*) ' back fromencode in keypp2ldr_flm'
      do 10 i=1,8
      if (orb_buf(i).eq.' ') orb_buf(i)='0'
   10 continue
      do 12 i=9,12
      orb_buf(i)=' '
   12 continue
      dss_orbit_num=orbit
      dss_azi_res=1.5*c1_pix
      dss_rng_res=1.5*c2_pix
      ele_sight=peak_bm_ang(1)
      hr_angle=gha                   !platform position record
      if (sv_type.eq.restituted) type_ephemeris='RES'
      if (sv_type.eq.predicted) type_ephemeris='PRE'
      if (fdc_est_mode.eq.1) then
        clutter_lock='YES'
      else
        clutter_lock='NOT'
      endif
      if (rgc_est_mode.eq.1) then
        auto_focus='YES'
      else
        auto_focus='NOT'
      endif

      return
 1000 format(i8)
      end


      subroutine get_ldr_flm(nlines,kburst,scan_fname,inp_frame_id,
     *ldr_fname,sc_head,ceos_fname,orbit_no,act_lines,
     *act_pix,chirp_rate,fs,plen,burst_start,burst_end,prf,
     *gain_rec,nbeams,np,np_v,avg_terrain,proc_gain,peak_bm_ang,
     *gha,sv_type,c1_frm,c2_frm,terr_corr_flag,x_sc,y_sc,z_sc,
     *v_x_sc,v_y_sc,v_z_sc,sv_x_pos,sv_y_pos,sv_z_pos,t_burst,
     *window_start_time,np_air,roll_rate,yaw_rate,pitch_rate,
     *pitch_o,yaw_o,roll_o,pitch,yaw,roll,c1_pxl,c2_pxl,c1_flm,
     *c2_flm,fd_low_near,fd_low_far,fr_near,fr_far,i_ang_bip,
     *look_ang,sv_ref_time,r_low,r_high,p_x,p_y,p_z,chirp_bw,
     *beam_mode,ceos_temp_file,alpha1,alpha2,alpha3,
     *c1_g,c2_g,c2_cell,c1size_blk,n_c2_blk,r_bip,coef_fd,sv_x_vel,
     *sv_y_vel,sv_z_vel,fdc_est_mode,rgc_est_mode,res_prod,
     *replica_agc,lna_temp,subsystem_temp,
     *protector_temp,rx_agc,cal_sys_temp,repl_pow,
     *pmf_file,pmf_temp_file,sitename,sw_ver,
     *image_path,leader_path,proj,prod_id,frq_media_id,
     *frq_media_type,frq_media_loc,job_id,platform,sensor,
     *rev,sequence,act_id,recorder_id,station_id,
     *frame_mode,sv_prec,product_type,npix,s_n_r,radio_acc,
     *subframe_id,bit_error_rate,gha_time,aux_yaw_valid,
     *aux_roll_valid,aux_pitch_valid,r_cntr,dem_ellip,
     *dlon_topo,dlat_topo,topo,t_b,rlocal_mean,istatus)
      implicit none
      include 'ceos.inc'
      include 'ssp2_const.inc'
      include 'scan.inc'
      include 'pmf.inc'                          ! JMS 3-17-96
      integer kb(5),status
      character*60 scan_fname
      character*60 ceos_temp_file
      real*4 sc_head
      integer strlen
      character*16 ceos_fname,sc_id,dt_id
      character*1 sc_id_buf(16)
      character*12 img_id
      character*1 img_id_buf(12)
      integer orbit_no,orbit_seg
      character*1 dtake_id(16)
      character*5 rrdt,rr
      character*2 dthead                            !  JMS changed 4-4-96
      character*1 rrbuf(5)
      character*2 nndt,nn
      character*1 nnbuf(2)
      integer nlines,kburst,i
      real*8 sc_time_sec
      integer act_lines,act_pix

      integer inp_frame_id,job_id
      character*64 platform,sensor
      integer rev,sequence
      character*64 act_id,media_type,media_loc
      character*64 recorder_id,station_id
      character*64 frame_mode,site_name
      integer seg_id,start_addr,end_addr

      real*8 chirp_rate,fs,plen,avg_terrain,proc_gain,gha              
      integer burst_start,burst_end,nbeams
      real*8 prf(burst_max),gain_rec(burst_max)
      integer np(burst_max),np_v(burst_max),sv_type
      real*8 c1_pxl,c2_pxl,peak_bm_ang(4)

      real*8 c1_frm(2,2),c2_frm(2,2)
      integer terr_corr_flag
      real*8 x_sc(burst_max),y_sc(burst_max),z_sc(burst_max)
      real*8 v_x_sc(burst_max),v_y_sc(burst_max),v_z_sc(burst_max)
      real*8 sv_x_pos,sv_y_pos,sv_z_pos,t_burst(burst_max)
      real*8 sv_x_vel,sv_y_vel,sv_z_vel

      integer np_air(burst_max)
      real*8 roll_rate(burst_max),yaw_rate(burst_max)
      real*8 pitch_rate(burst_max)

      real*8 pitch_o(burst_max),yaw_o(burst_max),roll_o(burst_max)
      real*8 pitch(burst_max),yaw(burst_max),roll(burst_max)
      real*8 c1_flm(2,2,burst_max),c2_flm(2,2,burst_max)
      real*8 fd_low_near(burst_max),fd_low_far(burst_max)
      real*8 fr_near(burst_max),fr_far(burst_max),i_ang_bip(burst_max)
      real*8 look_ang(burst_max),sv_ref_time
      real*8 r_low(burst_max),r_high(burst_max)
      real*8 p_x(burst_max),p_y(burst_max),p_z(burst_max)
      real*8 chirp_bw
      real*8 alpha1,alpha2,alpha3
      integer beam_mode

      real*8 c1_g(3,3,n_az,n_rg,burst_max)
      real*8 c2_g(3,3,n_az,n_rg,burst_max)
      real*8 c2_cell(burst_max),c1size_blk(burst_max)
      integer n_c2_blk(burst_max)
      real*8 r_bip(burst_max),coef_fd(4,burst_max)
      integer fdc_est_mode,rgc_est_mode
      integer res_prod
      real*8 pre_cal1,pre_cal2,post_cal1,post_cal2

      real*8 window_start_time(1600),replica_agc(1600)
      real*8 lna_temp(1600),subsystem_temp(1600)
      real*8 protector_temp(1600),rx_agc(1600)
      real*8 cal_sys_temp,repl_pow
      character*64 sv_prec
      character*64 sitename

      integer proj

c  New 3-17-96 JMS
      character*64 sw_ver
      character*64 prod_creat_time
      character*256 image_path
      character*256 image_file_path
      character*256 leader_path
      character*256 ldr_file
      character*64 prod_id,frq_media_id
      character*64 frq_media_type,frq_media_loc
      character*64 product_id
      character*256 arch_path,product_path
      character*1  fbuf1(256),fbuf2(256)
      character*64 pmf_media_id
      integer product_type
      character*64 dummy_str_1,dummy_str_2
      character*1 xc
      character*1 image_file_buf(64)
      integer npix
      real*8 s_n_r,radio_acc
      integer stime,tarray(9)
      integer time_
      integer cor_year,cor_doy,cor_hr,cor_min,cor_sec,cor_usec
      integer j
      real*4 bit_error_rate                        ! 4-4-96
      integer istatus                              ! 5-3-96
      real*8 ns_h,ne_h,fs_h,fe_h                   ! 5-8-96
      real*8 gha_time                              ! 5-9-96
      character*10 aux_yaw_valid(1600)             ! 5-10-96
      character*10 aux_roll_valid(1600)            ! 5-10-96
      character*10 aux_pitch_valid(1600)           ! 5-10-96
      real*8 r_cntr(burst_max)                     ! 5-10-96
      real*8 dlon_topo                             ! 5-10-96
      real*8 dlat_topo                             ! 5-10-96
      integer dem_ellip                            ! 5-10-96
      integer*2 topo                               ! 5-10-96
      real*8 t_b(burst_max)                        ! 5-15-96
      real*8 rlocal_mean                           ! 6-10-96
      integer scmon,scdom                          ! 9-7-96
      character*64 scstr                           ! 9-7-96
      character*60 scan_vers                       ! 9-10-96

      equivalence(sc_id,sc_id_buf(1))
      equivalence(img_id,img_id_buf(1))
      equivalence(dthead,dtake_id(1))
      equivalence(rrdt,dtake_id(3))                          !  JMS changed 4-4-96
      equivalence(nndt,dtake_id(8))                          !  JMS changed 4-4-96
      equivalence(nn,nnbuf(1))
      equivalence(rr,rrbuf(1))
      equivalence(dt_id,dtake_id(1))

c  New 3-17-96 JMS
      equivalence(image_file,image_file_buf(1))
      equivalence(product_path,fbuf1(1))
      equivalence(arch_path,fbuf2(1))


      istatus=iok

c  For now (11-12-95) load in default values for radiometric table and
c  framelet overlap values
      call ldr_defaults
      proj=3                             ! No map proj for framelets
      pro_head=0.0
      sc_id=ceos_fname
      scene_id=sc_id

      do 10 i=1,12
      img_id_buf(i)=' '
   10 continue
      do 20 i=1,9
      img_id_buf(i)=sc_id_buf(i+7)
   20 continue
      fac_image_id=img_id
      write (*,*) 'encode 1 in get ldr flm'
      write (unit=rr,fmt=1000) orbit_no
c      encode(5,1000,rr) orbit_no
      write (unit=nn,fmt=1020) orbit_seg
      write (*,*) 'encode 2 in get ldr flm'
c      encode(2,1020,nn) orbit_seg
      write (*,*) 'back from encode 2 in get ldr flm'
      do 30 i=1,5
      if (rrbuf(i).eq.' ') rrbuf(i)='0'
   30 continue
      do 40 i=1,2
      if (nnbuf(i).eq.' ') nnbuf(i)='0'
   40 continue
      dthead='R1'                     !  JMS changed 4-4-96
      rrdt=rr
      nndt=nn
      do 50 i=10,16                   !  JMS changed 4-4-96
      dtake_id(i)=' '
   50 continue
      datatake_id=dt_id
      actual_lines=act_lines
      actual_pixels=act_pix

      call getscandata(inp_frame_id,scan_fname,status,
     *seg_id,styr,
     *stdoy,sthr,stmin,stsec,stusec,edyr,eddoy,edhr,edmin,edsec,
     *edusec,start_addr,end_addr,
     *ascdes,pre_cal1,pre_cal2,
     *post_cal1,post_cal2,scan_vers,istatus)
      if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return

      strlen=64
c      call getscandata(inp_frame_id,scan_fname,status,job_id,
c     *platform,sensor,rev,sequence,act_id,media_id,media_type,
c     *media_loc,recorder_id,station_id,frame_mode,sitename,
c     *seg_id,svyr,svdoy,svhr,
c     *svmin,svsec,svusec,scyr,scdoy,schr,scmin,scsec,scusec,styr,
c     *stdoy,sthr,stmin,stsec,stusec,edyr,eddoy,edhr,edmin,edsec,
c     *edusec,start_addr,end_addr,xpos,ypos,zpos,xvel,yvel,zvel,
c     *nslat,nslon,nelat,nelon,
c     *fslat,fslon,felat,felon,sclat,sclon,ascdes,pre_cal1,pre_cal2,
c     *post_cal1,post_cal2,sv_prec)
      ASC_DESC=ascdes
      asc_des=ascdes


      call corn_cent(c1_frm,c2_frm,terr_corr_flag,
     *burst_end,nbeams,gha,sv_ref_time,sv_x_pos,sv_y_pos,
     *sv_z_pos,sv_x_vel,sv_y_vel,sv_z_vel,t_burst,alpha1,
     *alpha2,alpha3,r_bip,coef_fd,sc_time_sec,sclat,sclon,
     *scyr,scdoy,schr,scmin,scsec,scusec,nslat,
     *nslon,nelat,nelon,fslat,fslon,felat,felon,
     *ns_h,ne_h,fs_h,fe_h,gha_time,dem_ellip,
     *dlon_topo,dlat_topo,topo,avg_terrain,rlocal_mean,
     *scmon,scdom)
      call sctim_str(scyr,scmon,scdom,schr,scmin,scsec,
     *scusec,scstr)
      inp_sctim=scstr                                 !  JMS 9-7-96
      call se_time(burst_start,burst_end,t_b,t_burst,
     *np_v,prf,styr,stdoy,sthr,stmin,stsec,stusec,
     *edyr,eddoy,edhr,edmin,edsec,edusec)
      pro_lat=sclat
      pro_long=sclon
      center_LAT=sclat
      center_LON=sclon
      near_sta_LAT=nslat
      near_sta_LON=nslon
      near_end_LAT=nelat
      near_end_LON=nelon
      far_sta_LAT=fslat
      far_sta_LON=fslon
      far_end_LAT=felat
      far_end_LON=felon


c  JEFFS 3-17-96 new for pmf
      img_rec_cnt=nlines+1
      img_1st_rec_len=npix+192
      img_max_rec_len=npix+192
      image_file=ceos_fname
      image_file_buf(17)='.'
      image_file_buf(18)='.'
      do i=19,64
        image_file_buf(i)=' '
      enddo
c      call sgmt(styr,stdoy,sthr,stmin,stsec,stusec,start_time,
c     *dummy_str_1,dummy_str_2)
c      call sgmt(edyr,eddoy,edhr,edmin,edsec,edusec,end_time,
c     *dummy_str_1,dummy_str_2)
c      call sgmt(scyr,scdoy,schr,scmin,scsec,scusec,center_time,
c     *dummy_str_1,dummy_str_2)
      call gmt_str(styr,stdoy,sthr,stmin,stsec,stusec,start_time)
      call gmt_str(edyr,eddoy,edhr,edmin,edsec,edusec,end_time)
      call gmt_str(scyr,scdoy,schr,scmin,scsec,scusec,center_time)

c  JMS added 4-10-96 for attitude data record
      att_gmt_day(1)=stdoy
      att_gmt_day(2)=scdoy
      att_gmt_day(3)=eddoy
      att_gmt_sec(1)=(3600*sthr+60*stmin+stsec)*1000+stusec/1000
      att_gmt_sec(2)=(3600*schr+60*scmin+scsec)*1000+scusec/1000
      att_gmt_sec(3)=(3600*edhr+60*edmin+edsec)*1000+edusec/1000

      
      stime=time_()
      call ibm_gmtime(stime,tarray)
      cor_year=tarray(6)+1900
      cor_doy=tarray(8)+1
      cor_hr=tarray(3)
      cor_min=tarray(2)
      cor_sec=tarray(1)
      cor_usec=0
      call gmt_str(cor_year,cor_doy,cor_hr,cor_min,cor_sec,cor_usec,
     *prod_creat_time)
      prod_time=prod_creat_time            ! JMS 2-25-96
      pmf_time=prod_creat_time             ! JMS 2-25-96
      site_name=sitename
      if (beam_mode.eq.1) then
        mode='SWA'
      else if (beam_mode.eq.2) then
        mode='SWB'
      else if (beam_mode.eq.3) then
        mode='SNA'
      else if (beam_mode.eq.4) then
        mode='SNB'
      endif
      ldr_rec_cnt=10                                        ! Basic w/o map proj or DEM
c      if (proj.ne.3) ldr_rec_cnt=ldr_rec_cnt+1              ! map projection record added
      ldr_rec_cnt=ldr_rec_cnt+1                              ! map projection record added
      if (terr_corr_flag.ne.0) ldr_rec_cnt=ldr_rec_cnt+1    ! DEM record
      ldr_max_rec_len=5120

      pmf_media_id=frq_media_id
      write (*,*) 'In get_ldr_flm , pmf_media_id = ',pmf_media_id
      media_type=frq_media_type
      media_loc=frq_media_loc
      prod_type='GRF'
      dataset='RADARSAT-1 SCANSAR SINGLELOOK PRODUCT'
      dest='CP'
      pmf_source='SSP'
      num_rec=1
      proc_ver=sw_ver
      pmf_snr=s_n_r
      pmf_radio_acc=radio_acc
      msg_type='SAR_FRAME_METADATA'
      image_file_path=image_path
      ldr_file=leader_path
      prod_creat='SSP'
      product_id=prod_id

      product_path=image_path                           ! JMS 2-26-96

c  Strip path information from output file name
c  Begin by searching backwards from end of string for a "/"
      j=256
      do 640 i=1,256
      xc=fbuf1(j)
      if (xc.eq."/") go to 650
      j=j-1
  640 continue
  650 continue
c  Now copy path to buffer
      do 670 i=1,j
      fbuf2(i)=fbuf1(i)
  670 continue
      do 680 i=(j+1),256
      fbuf2(i)=' '
  680 continue
      loc_arch_dir=arch_path

      write (*,*) 'pmf temp = ',pmf_temp_file
      write (*,*) 'pmf file = ',pmf_file
      write (*,*) 'About to call pmf'
      write (*,*) pmf_temp_file
      write (*,*) pmf_file
      write (*,*) job_id
      write (*,*) platform
      write (*,*) sensor
      write (*,*) rev
      write (*,*) sequence
      write (*,*) act_id
      write (*,*) media_id
      write (*,*) media_type
      write (*,*) media_loc
      write (*,*) recorder_id
      write (*,*) station_id
      write (*,*) frame_mode
      write (*,*) site_name
      write (*,*) mode
      write (*,*) seg_id
      write (*,*) dataset
      write (*,*) loc_arch_dir
      write (*,*) product_id
      write (*,*) image_file
      write (*,*) image_file_path
      call pmf(pmf_temp_file,pmf_file,job_id,platform,sensor,rev,
     *sequence,act_id,pmf_media_id,media_type,media_loc,recorder_id,
     *station_id,frame_mode,site_name,mode,seg_id,dataset,
     *loc_arch_dir,product_id,image_file,image_file_path,
     *ldr_file,inp_frame_id,subframe_id,
     *start_addr,end_addr,img_rec_cnt,img_1st_rec_len,
     *img_max_rec_len,ldr_max_rec_len,ldr_rec_cnt,prod_creat,
     *prod_type,start_time,end_time,sclat,sclon,center_time,nslat,
     *nslon,nelat,nelon,fslat,fslon,felat,felon,ascdes,
     *proc_ver,sv_prec,pmf_snr,pmf_radio_acc,prod_time,pmf_time,
     *msg_type,dest,pmf_source,num_rec,istatus)
      if (istatus.ge.ierr_lower.and.istatus.le.ierr_upper) return


      call aux_to_ldr(prf,np_air,window_start_time,
     *burst_start,burst_end,np,roll_rate,yaw_rate,
     *pitch_rate,nbeams,replica_agc,
     *lna_temp,subsystem_temp,protector_temp,rx_agc,
     *cal_sys_temp,repl_pow,aux_yaw_valid,aux_roll_valid,
     *aux_pitch_valid)

      call keypp_2_ldr_flm(kburst,chirp_rate,fs,
     *plen,prf,gain_rec,nbeams,np,np_v,avg_terrain,
     *proc_gain,c1_pxl,c2_pxl,orbit_no,peak_bm_ang,gha,
     *sv_type,fdc_est_mode,rgc_est_mode)

      call ceos_calc_flm(nlines,kburst,scyr,scdoy,schr,
     *scmin,scsec,scusec,sclat,sclon,sc_time_sec,burst_start,
     *burst_end,x_sc,y_sc,z_sc,v_x_sc,v_y_sc,v_z_sc,pitch_o,yaw_o,
     *roll_o,pitch,yaw,roll,c1_pxl,c2_pxl,c1_flm,c2_flm,fd_low_near,
     *fd_low_far,fr_near,fr_far,i_ang_bip,look_ang,gha,sv_ref_time,
     *prf,t_burst,r_low,r_high,np,np_air,p_x,p_y,p_z,fs,chirp_bw,
     *nbeams,beam_mode,c1_g,c2_g,c2_cell,c1size_blk,n_c2_blk,
     *res_prod,r_cntr)

c  JMS 4-4-96 added bit error rate to dqs and fac
      ber=bit_error_rate                   ! dqs
      bit_err_rate=bit_error_rate          ! fac

c  JMS 5-9-96 fill in beam id's in dss
      if (beam_mode.eq.1) then
        dss_beam1='WD1'
        dss_beam2='WD2'
        dss_beam3='WD3'
        dss_beam4='ST7'
      else if (beam_mode.eq.2) then
        dss_beam1='WD1'
        dss_beam2='WD2'
        dss_beam3='ST5'
        dss_beam4='ST6'
      else if (beam_mode.eq.3) then
        dss_beam1='WD1'
        dss_beam2='WD2'
        dss_beam3=' '
        dss_beam4=' '
      else if (beam_mode.eq.4) then
        dss_beam1='WD2'
        dss_beam2='ST5'
        dss_beam3='ST6'
        dss_beam4=' '
      endif

c  JMS 5-9-96 fill in total lines and pixels here instead of in ceos_calc_flm
      total_lines=nlines
      total_pixels=sam_post

c      call write_ceos_ldr(ceos_temp_file,ldr_fname,terr_corr_flag,
c     *proj)
      call write_ceos_ldr(ceos_temp_file,ldr_fname,terr_corr_flag,
     *proj,istatus)
      return
 1000 format (i5)
 1020 format (i2)
      end


      subroutine ldr_defaults
      include 'ceos.inc'

c      dss_no_beams=4
      mech_sight=29.4
      clock_ang=90.0
c      wave_length=0.05656

      rad_n_samp=256
      do i=1,256
      lookup_tab(i)=1.0
      end do

      return
      end



      subroutine corn_cent(c1_frm,c2_frm,terr_corr_flag,
     *burst_end,nbeams,gha,sv_ref_time,sv_x_pos,sv_y_pos,
     *sv_z_pos,sv_x_vel,sv_y_vel,sv_z_vel,t_burst,alpha1,
     *alpha2,alpha3,r_bip,coef_fd,sc_time_sec,sc_lat,sc_lon,
     *sc_year,sc_doy,sc_hr,sc_min,sc_sec,sc_usec,ns_lat,
     *ns_lon,ne_lat,ne_lon,fs_lat,fs_lon,fe_lat,fe_lon,
     *ns_h,ne_h,fs_h,fe_h,gha_time,dem_ellip,dlon_topo,
     *dlat_topo,topo,avg_terrain,rlocal_mean,sc_mon,sc_dom)


      implicit none
      include 'ssp2_const.inc'
      integer stime,tarray(9)
      integer burst_end,nbeams
      integer sc_year,sc_doy,sc_hr,sc_min,sc_sec,sc_usec
      integer sc_mon,sc_dom   ! JMS 9-7-96
      real*8 c1,c2,c1_center,c2_center
      real*8 lat,lat_d,lon,height
      real*8 time,usec
      real*8 sc_lat,sc_lon,ns_lat,ns_lon
      real*8 ne_lat,ne_lon,fs_lat,fs_lon,fe_lat,fe_lon
      real*8 sc_time_sec
      real*8 jd_1970_sec
      real*8 c1_frm(2,2),c2_frm(2,2)
      integer terr_corr_flag
      real*8 sv_ref_time
      real*8 sv_x_pos,sv_y_pos,sv_z_pos,t_burst(burst_max)
      real*8 sv_x_vel,sv_y_vel,sv_z_vel
      real*8 alpha1,alpha2,alpha3
      real*8 coef_fd(4,burst_max),r_bip(burst_max)
      real*8 gha
      real*8 gha_time                              ! 5-9-96
      real*8 ns_h,ne_h,fs_h,fe_h                   ! 5-8-96
      real*8 dlon_topo,dlat_topo                   ! 5-10-96
      integer dem_ellip                            ! 5-10-96
      integer*2 topo(ns_ew,ns_ns)                  ! 5-10-96
      real*8 avg_terrain                           ! 5-15-96
      real*8 rlocal_mean                           ! 6-10-96

c      jd_1970_sec=2440586.5d0*86400.0d0
      jd_1970_sec=2440587.5d0*86400.0d0            ! JMS 8-6-96

      write (*,*) 'JEFFS in corn_cent'
      write (*,*) 'c1_frm,c2_frm(1,1) = ',c1_frm(1,1),c2_frm(1,1)
      write (*,*) 'c1_frm,c2_frm(1,2) = ',c1_frm(1,2),c2_frm(1,2)
      write (*,*) 'c1_frm,c2_frm(2,1) = ',c1_frm(2,1),c2_frm(2,1)
      write (*,*) 'c1_frm,c2_frm(2,2) = ',c1_frm(2,2),c2_frm(2,2)
      write (*,*) ' alpha1 = ',alpha1
      write (*,*) ' alpha2 = ',alpha2
      write (*,*) ' alpha3 = ',alpha3
c  Compute scene center latitude, longitude, and time
      c1_center=(c1_frm(1,1)+c1_frm(2,1))/2.0d0
      c2_center=(c2_frm(1,1)+c2_frm(1,2))/2.0d0
      write (*,*) 'c1_center = ',c1_center
      write (*,*) 'c2_center = ',c2_center

      call ac_to_ll(c1_center,c2_center,lat,lat_d,lon,
     *alpha1,alpha2,alpha3,rlocal_mean)
      write (*,*) 'lat,lat_d,lon = ',lat,lat_d,lon
      write (*,*) 'gha_time = ',gha_time
      if (terr_corr_flag.eq.1) then
        call elevation(lat_d,lon,height,avg_terrain,     ! changed lat to lat_d 7-15-96
     *    dem_ellip,dlon_topo,dlat_topo,topo)
         height=0.0d0
      else
        height=0.0d0
      endif
      call geo_time(burst_end,nbeams,gha,sv_ref_time,sv_x_pos,
     *sv_y_pos,sv_z_pos,sv_x_vel,sv_y_vel,sv_z_vel,t_burst,
     *r_bip,coef_fd,lat,lat_d,lon,height,time,gha_time)
      write (*,*) 'time = ',time
      sc_time_sec=time
      time=time-jd_1970_sec
      usec=dmod(time,1.0d0)
      stime=int(time-usec)
      write (*,*) ' about to call ibm gmtime'
      call ibm_gmtime(stime,tarray)
c      call gmtime(stime,tarray)
      write (*,*) ' back from ibm gmtime'
      sc_lat=lat_d
      sc_lon=lon
      sc_year=tarray(6)+1900
      sc_mon=tarray(5)+1   !  JMS 9-7-96
      sc_dom=tarray(4)     !  JMS 9-7-96
      sc_doy=tarray(8)+1
      sc_hr=tarray(3)
      sc_min=tarray(2)
      sc_sec=tarray(1)
      sc_usec=int(usec*1000000.0d0)

c  Compute corner latitudes and longitudes    
      write (*,*) 'EIEIO computing corner lats and longs'
      c1=c1_frm(1,1)
      c2=c2_frm(1,1)
      write (*,*) 'c1,c2 = ',c1,c2
      write (*,*) 'alphas are ',alpha1,alpha2,alpha3
      write (*,*) 'rlocal_mean = ',rlocal_mean
      call ac_to_ll(c1,c2,lat,lat_d,lon,
     *alpha1,alpha2,alpha3,rlocal_mean)
      ns_lat=lat_d
      ns_lon=lon
      write (*,*) 'lat, lon = ',lat_d,lon
      if (terr_corr_flag.eq.1) then
        call elevation(lat_d,lon,height,avg_terrain,
     *   dem_ellip,dlon_topo,dlat_topo,topo)
      else
        height=0.0d0
      endif
      ns_h=height

      c1=c1_frm(1,2)
      c2=c2_frm(1,2)
      write (*,*) 'c1,c2 = ',c1,c2
      call ac_to_ll(c1,c2,lat,lat_d,lon,
     *alpha1,alpha2,alpha3,rlocal_mean)
      fs_lat=lat_d
      fs_lon=lon
      write (*,*) 'lat, lon = ',lat_d,lon
      if (terr_corr_flag.eq.1) then
        call elevation(lat_d,lon,height,avg_terrain,
     *   dem_ellip,dlon_topo,dlat_topo,topo)
      else
        height=0.0d0
      endif
      fs_h=height

      c1=c1_frm(2,1)
      c2=c2_frm(2,1)
      write (*,*) 'c1,c2 = ',c1,c2
      call ac_to_ll(c1,c2,lat,lat_d,lon,
     *alpha1,alpha2,alpha3,rlocal_mean)
      ne_lat=lat_d
      ne_lon=lon
      write (*,*) 'lat, lon = ',lat_d,lon
      if (terr_corr_flag.eq.1) then
        call elevation(lat_d,lon,height,avg_terrain,
     *   dem_ellip,dlon_topo,dlat_topo,topo)
      else
        height=0.0d0
      endif
      ne_h=height

      c1=c1_frm(2,2)
      c2=c2_frm(2,2)
      write (*,*) 'c1,c2 = ',c1,c2
      call ac_to_ll(c1,c2,lat,lat_d,lon,
     *alpha1,alpha2,alpha3,rlocal_mean)
      fe_lat=lat_d
      fe_lon=lon
      write (*,*) 'lat, lon = ',lat_d,lon
      if (terr_corr_flag.eq.1) then
        call elevation(lat_d,lon,height,avg_terrain,
     *    dem_ellip,dlon_topo,dlat_topo,topo)
      else
        height=0.0d0
      endif
      fe_h=height

      return
      end


c   JMS 4-10-96 subroutine to compute start and end times

      subroutine se_time(burst_start,burst_end,t_b,
     *t_burst,np_v,prf,
     *st_year,st_doy,st_hr,st_min,st_sec,st_usec,
     *ed_year,ed_doy,ed_hr,ed_min,ed_sec,ed_usec)


      implicit none
      include 'ssp2_const.inc'
      integer stime,tarray(9)
      integer st_year,st_doy,st_hr,st_min,st_sec,st_usec
      integer ed_year,ed_doy,ed_hr,ed_min,ed_sec,ed_usec
      real*8 t_b(burst_max),t_burst(burst_max)
      real*8 prf(burst_max)
      integer np_v(burst_max)
      real*8 time,usec,delt
      real*8 jd_1970_sec
      integer burst_start,burst_end


c      jd_1970_sec=2440586.5d0*86400.0d0
      jd_1970_sec=2440587.5d0*86400.0d0       ! JMS 8-6-96

      write (*,*) 'In se_time'
     
      time=t_b(burst_start)
      write (*,*) 'time = ',time
      time=time-jd_1970_sec
      usec=dmod(time,1.0d0)
      stime=int(time-usec)
      call ibm_gmtime(stime,tarray)
      st_year=tarray(6)+1900
      st_doy=tarray(8)+1
      st_hr=tarray(3)
      st_min=tarray(2)
      st_sec=tarray(1)
      st_usec=int(usec*1000000.0d0)

      delt=dble(np_v(burst_end))/(2.0d0*prf(burst_end))
      time=t_burst(burst_end)+delt
      write (*,*) 'time = ',time
      time=time-jd_1970_sec
      usec=dmod(time,1.0d0)
      stime=int(time-usec)
      call ibm_gmtime(stime,tarray)
      ed_year=tarray(6)+1900
      ed_doy=tarray(8)+1
      ed_hr=tarray(3)
      ed_min=tarray(2)
      ed_sec=tarray(1)
      ed_usec=int(usec*1000000.0d0)
      write (*,*) 'start'
      write (*,*) st_year,st_doy,st_hr
      write (*,*) st_min,st_sec,st_usec
      write (*,*) 'end'
      write (*,*) ed_year,ed_doy,ed_hr
      write (*,*) ed_min,ed_sec,ed_usec


      return
      end


c	Calculate the time of a location for  geo-image from lat_d/lon
c	Input: lat,lat_d,lon & h_mean
c	Output: time in sec ref to state vector time


        subroutine geo_time(burst_end,nbeams,gha,sv_ref_time,
     *  sv_x_pos,sv_y_pos,sv_z_pos,sv_x_vel,sv_y_vel,sv_z_vel,
     *  t_burst,r_bip,coef_fd,lat,lat_d,lon,h_mean,t,gha_time)

	implicit none
        include 'ssp2_const.inc'
	real*8 lat,lon,lat_d,h_mean,t
	real*8 dis, fd, vst
	integer m,center,burst_end,nbeams,kcount
        real*8 r_bip(burst_max)
        real*8 coef_fd(4,burst_max)
        real*8 t_burst(burst_max)
        real*8 sv_x_pos
        real*8 sv_y_pos
        real*8 sv_z_pos
        real*8 sv_x_vel
        real*8 sv_y_vel
        real*8 sv_z_vel
        real*8 fd_t,fd_r,sv_ref_time,x,y,z,vx,vy,vz,r
        real*8 vxs,vys,vzs,dsquint,dt,da_err,da_dis
        real*8 xs,ys,zs
        real*8 gha
        real*8 gha_time                                     ! 5-9-96
        real*8 rr,vv                                        ! 5-9-96

	center=(burst_end/2)/nbeams*nbeams+nbeams/2
	write (*,*) 'center/end burst #:',center,burst_end
        write (*,*) 'r_bip coef_fd:',r_bip(center),coef_fd(1,center)
        write (*,*) 'sv_ref_time,tburst(1) = ',sv_ref_time,t_burst(1)
        write (*,*) 'sv_x_pos = ',sv_x_pos
        write (*,*) 'sv_y_pos = ',sv_y_pos
        write (*,*) 'sv_z_pos = ',sv_z_pos
        write (*,*) 'sv_x_vel = ',sv_x_vel
        write (*,*) 'sv_y_vel = ',sv_y_vel
        write (*,*) 'sv_z_vel = ',sv_z_vel
        write (*,*) 'lat,lat_d,lon = ',lat,lat_d,lon
        write (*,*) 'h_mean = ',h_mean
	call get_tv22(lat,lon,lat_d,h_mean,x,y,z,vx,vy,vz) !get Cartesian Coordinate
        write (*,*) 'x,y,z = ',x,y,z
        write (*,*) 'vx,vy,vz = ',vx,vy,vz
        rr=sqrt(x**2+y**2+z**2)                            ! 5-9-96
        write (*,*) '1)geo_time target pos',rr,x,y,z
        vv=sqrt(vx**2+vy**2+vz**2)                         ! 5-9-96
        write (*,*) 'geo_time target vel',vv,vx,vy,vz

	call time_of_radar(gha,sv_ref_time,sv_x_pos,sv_y_pos,
     *  sv_z_pos,sv_x_vel,sv_y_vel,sv_z_vel,t_burst,x,y,z,t,
     *  gha_time)
        write (*,*) '2)geo_time t(int)',t
	da_err=0.5
	kcount = 0
1111	continue
        call pro_ephemeris(gha,sv_ref_time,sv_x_pos,sv_y_pos,
     *  sv_z_pos,sv_x_vel,sv_y_vel,sv_z_vel,t,xs,ys,zs,vxs,
     *  vys,vzs,gha_time)

        write (*,*) 'LOOP kcount ',kcount
        write (*,*) 'xs,ys,zs = ',xs,ys,zs
        write (*,*) 'vxs,vys,vzs = ',vxs,vys,vzs
	r = dis(x,y,z,xs,ys,zs)
        write (*,*) ' r:',r
	fd_t = fd(lambda,xs,ys,zs,vxs,vys,vzs,x,y,z,vx,vy,vz)

	m=4
        call v_poly(r,r_bip(center),coef_fd(1,center),m,fd_r)

	vst = dis(vxs,vys,vzs,vx,vy,vz)		!vst: sensor-target relative velocity
        write (*,*) 'fd_t, fd_r = ',fd_t,fd_r
        write (*,*) 'lambda = ',lambda
	dsquint = asin((fd_t - fd_r)*lambda/(2*vst))
        write (*,*) 'dsquint lambda,vst',dsquint,lambda,vst
	da_dis =  r * sin(dsquint)		!da_dis: along-track distance
	dt = da_dis/vst				!dt: time correction factor
	t = t + dt
        write (*,*) '*** da_dis fd_t,fd_r = ',da_dis,fd_t,fd_r
        write (*,*) 'Geo_time k r dsq dt t',kcount,r,dsquint,dt,t
	kcount = kcount + 1
	if(abs(da_dis).gt.da_err) go to 1111

        write (*,*) 'JEFFS in geo_time, tb1 = ',t_burst(1)
	write (*,*) '**geo_time T ** ', t

	return
	end


        subroutine get_tv22(lat,lon,lat_d,h_mean,
     1                  xt,yt,zt,vxt,vyt,vzt)
        implicit none
        include 'ssp2_const.inc'
        real*8 lat,lon,lat_d
        real*8 h,h_mean,rxyt,u1,u2,u3,rn
        real*8 xt,yt,zt,vxt,vyt,vzt

	h = h_mean
        Rn = Re/sqrt(1-(ecc_e*sind(lat_d))**2)
        xt = (Rn+h)*cosd(lon)*cosd(lat_d)
        yt = (Rn+h)*sind(lon)*cosd(lat_d)
        zt = (Rn*(1.0d0-ecc_e**2)+h)*sind(lat_d)
        rxyt = sqrt(xt**2+yt**2)
        call cross(0.d0,0.d0,1.d0,xt,yt,zt,u1,u2,u3)
        vxt = omega_e*rxyt*u1
        vyt = omega_e*rxyt*u2
        vzt = omega_e*rxyt*u3

        return
        end


	subroutine time_of_radar(gha,sv_ref_time,sv_x_pos,sv_y_pos,
     *  sv_z_pos,sv_x_vel,sv_y_vel,sv_z_vel,t_burst,x,y,z,t,gha_time)
	implicit none
        include 'ssp2_const.inc'
        real*8 t_burst(burst_max)
        real*8 t,a1,a2,a3,b1,b2,b3,xs,ys,zs,vs,ang,d_dis
        real*8 vxs,vys,vzs
        real*8 sv_x_pos
        real*8 sv_y_pos
        real*8 sv_z_pos
        real*8 sv_x_vel
        real*8 sv_y_vel
        real*8 sv_z_vel
        real*8 sv_ref_time,x,y,z
        real*8 gha
        real*8 gha_time                         ! 5-9-96

	t = 3.0d0 + t_burst(1)

        call pro_ephemeris(gha,sv_ref_time,sv_x_pos,sv_y_pos,
     *  sv_z_pos,sv_x_vel,sv_y_vel,sv_z_vel,t,xs,ys,zs,vxs,
     *  vys,vzs,gha_time)
	write (*,*) 'ephemeris  t ',t

	a1 = xs - sv_x_pos	!xs0
	a2 = ys - sv_y_pos 	!ys0
	a3 = zs - sv_z_pos	!zs0

	b1 = xs - x
	b2 = ys - y
	b3 = zs - z

	vs = sqrt(vxs**2+vys**2+vzs**2)
	call angle_anb(a1,a2,a3,b1,b2,b3,ang) 	 !angle between a and b
	d_dis = sqrt(b1**2+b2**2+b3**2)*cosd(ang)

	if(ang.gt.90.0d0) then
	t = t+d_dis/vs
	else
	t = t-d_dis/vs
	end if
	write (*,*) ' d_dis vs t ANG ',d_dis,vs,t,ang

	return
	end


	subroutine angle_anb(a1,a2,a3,b1,b2,b3,ang)  !angle between a and b
	implicit none
        real*8 a1,a2,a3,b1,b2,b3,a,b,dot,ang

	dot = a1*b1 + a2*b2 + a3*b3

	a = sqrt(a1**2+a2**2+a3**2)
	b = sqrt(b1**2+b2**2+b3**2)
	ang = acosd(dot/(a*b))
	if(ang.lt.0.0d0) ang = 180.0d0+ang

	return
	end

        subroutine pro_ephemeris(gha,sv_ref_time,sv_x_pos,sv_y_pos,
     *  sv_z_pos,sv_x_vel,sv_y_vel,sv_z_vel,t,xs,ys,zs,vxs,vys,vzs,
     *  gha_time)
        implicit none
        include 'ssp2_const.inc'
        real*8 t,t0,gha,sv_ref_time,delta_t
        real*8 vxs,vys,vzs
        real*8 vxs0,vys0,vzs0
        real*8 vxs1,vys1,vzs1
        real*8 xs,ys,zs
        real*8 xs0,ys0,zs0
        real*8 xs1,ys1,zs1
        real*8 sv_x_pos
        real*8 sv_y_pos
        real*8 sv_z_pos
        real*8 sv_x_vel
        real*8 sv_y_vel
        real*8 sv_z_vel
        real*8 gha_time

         t0= sv_ref_time
         delta_t = t - t0
        write (*,*) 'Inside ephemeris'
        write (*,*) 't t0 delta = ',t,t0,delta_t

	xs0=sv_x_pos
	ys0=sv_y_pos
	zs0=sv_z_pos
	vxs0=sv_x_vel
	vys0=sv_y_vel
        vzs0=sv_z_vel

        write (*,*) 'About to call propagation'
        write (*,*) 'gha = ',gha
        write (*,*) 'sv_ref_time = ',sv_ref_time
        write (*,*) 'sv_x_pos = ',sv_x_pos
        write (*,*) 'sv_y_pos = ',sv_y_pos
        write (*,*) 'sv_z_pos = ',sv_z_pos
        write (*,*) 'sv_x_vel = ',sv_x_vel
        write (*,*) 'sv_y_vel = ',sv_y_vel
        write (*,*) 'sv_z_vel = ',sv_z_vel
	call propagation(xs0,ys0,zs0,vxs0,vys0,vzs0,
     *              t0,delta_t,xs1,ys1,zs1,vxs1,vys1,vzs1)
        write (*,*) 'Back from call propagation'
        call EME_to_EBF(gha,gha_time,t,xs1,ys1,zs1,xs,ys,zs)
        call EME_to_EBF(gha,gha_time,t,vxs1,vys1,vzs1,vxs,vys,vzs)

        return
        end

      subroutine ceos_calc(nlines,left_fill_cnt,image_pix_cnt,
     *right_fill_cnt,sc_year,sc_doy,sc_hr,sc_min,sc_sec,sc_usec,
     *sc_lat,sc_lon,sc_jd_sec,linec,burst_start,burst_end,x_sc,
     *y_sc,z_sc,v_x_sc,v_y_sc,v_z_sc,pitch_o,yaw_o,roll_o,pitch,
     *yaw,roll,c1_pxl,c2_pxl,c1_flm,c2_flm,fd_low_near,
     *fd_low_far,fr_near,fr_far,i_ang_bip,look_ang,gha,
     *sv_ref_time,t_burst,np,np_air,r_low,r_high,prf,p_x,p_y,
     *p_z,fs,chirp_bw,nbeams,beam_mode,res_prod,r_cntr)
      implicit none
      integer maxlines
      parameter (maxlines=15554)
      integer left_fill_cnt(maxlines)
      integer right_fill_cnt(maxlines)
      integer image_pix_cnt(maxlines)
      include 'ceos.inc'
      include 'ssp2_const.inc'
      character*1 m,d,s
      real*8 alt_dopcen(3)
      real*8 alt_rate(3)
      real*8 crt_dopcen(3)
      real*8 crt_rate(3)
      real*8 kepler(6)
      integer year,doy,hr
      integer sc_year,sc_doy,sc_hr,sc_min,sc_sec,sc_usec
      character*32 gmtstr
      character*20 scen_id
      character*16 dtake_id
      character*12 img_id
      integer orbit,seg,frame,ver
      real*8 inc_ang,lk_ang
      real*8 xpos(3),ypos(3),zpos(3)
      real*8 xvel(3),yvel(3),zvel(3)
      real*8 ppd_time
      real*8 p_ang(3),r_ang(3),y_ang(3)
      character*32 gmtstr1
      character*27 gmtstr2
      character*6 datayear
      real*8 inc_ang_near(4),inc_ang_far(4)
      real*8 sc_len(4),sc_wid(4)
      integer usec,i
      integer nlines,linec,act_lines,nburst,kb1,kb2
      real*8 sc_lat,sc_lon,sc_jd_sec,dtr,pie4
      real*8 xs,ys,zs,vxs,vys,vzs,re_nadir,re_center
      real*8 pix_spacing,alt,sclen,xb,yb,zb,xm,ym,zm,xe,ye,ze
      real*8 trk_ang,gha_start,omega,start_time,sctime,p_lat,p_lon
      real*8 vx,vy,vz,px,py,pz,sq_ang,speed,s_near,s_far
      integer imax,ifirst,ilast
      real*8 del_rg_near,del_rg_far,x_nlok_near,x_nlok_far
      real*8 sum,avg_prf,fusec,ppd_int
      integer stime,tarray(9)
      integer burst_start,burst_end
      real*8 x_sc(burst_max),y_sc(burst_max),z_sc(burst_max)
      real*8 v_x_sc(burst_max),v_y_sc(burst_max),v_z_sc(burst_max)
      real*8 pitch_o(burst_max),yaw_o(burst_max),roll_o(burst_max)
      real*8 pitch(burst_max),yaw(burst_max),roll(burst_max)
      real*8 c1_pxl,c2_pxl,c1_flm(2,2,burst_max),c2_flm(2,2,burst_max)
      real*8 fd_low_near(burst_max),fd_low_far(burst_max)
      real*8 fr_near(burst_max),fr_far(burst_max),i_ang_bip(burst_max)
      real*8 look_ang(burst_max),gha,sv_ref_time,prf(burst_max)
      real*8 t_burst(burst_max),r_low(burst_max),r_high(burst_max)
      integer np(burst_max),np_air(burst_max)
      real*8 p_x(burst_max),p_y(burst_max),p_z(burst_max)
      real*8 fs,chirp_bw
      integer nbeams,beam_mode
      integer res_prod
      real*8 c1_pix,c2_pix
      real*4 del_x_az                             !  JMS 5-10-96
      integer kb                                  !  JMS 5-10-96
      real*8 r_cntr(burst_max)                    !  JMS 5-10-96


c   May want to move near and far range incidence angles to inc file.
      data inc_ang_near/20.0d0,20.0d0,20.0d0,30.81d0/
      data inc_ang_far/49.42d0,46.58d0,39.55d0,46.58d0/
c   May want to move scene dimensions to inc file.
      data sc_len/512.0d0,512.0d0,307.2d0,307.2d0/
      data sc_wid/512.0d0,459.8d0,307.2d0,298.3d0/

      pie4=datan(1.0d0)
      dtr=pie4/45.0d0
      usec=sc_usec

      if (res_prod.eq.75) then
       c1_pix=50.0d0
       c2_pix=50.0d0
      else if (res_prod.eq.150) then
       c1_pix=100.0d0
       c2_pix=100.0d0
      else if (res_prod.eq.600) then
       c1_pix=400.0d0
       c2_pix=400.0d0
      endif

      nburst=burst_end-burst_start+1
      kb1=nburst/2+burst_start-1
      kb2=kb1+1
      if (mod(nburst,2).eq.1) kb1=kb2
      xs=(x_sc(kb1)+x_sc(kb2))/2.0d0   ! may want to redo these based on lat & lon
      ys=(y_sc(kb1)+y_sc(kb2))/2.0d0
      zs=(z_sc(kb1)+z_sc(kb2))/2.0d0
      vxs=(v_x_sc(kb1)+v_x_sc(kb2))/2.0d0 
      vys=(v_y_sc(kb1)+v_y_sc(kb2))/2.0d0 
      vzs=(v_z_sc(kb1)+v_z_sc(kb2))/2.0d0 
      call radii(re,rp,xs,ys,zs,sc_lat,re_nadir,re_center)                       !OK
      nadir_radius=re_nadir/1000.0
      image_radius=re_center/1000.0

      call attdata(burst_start,burst_end,pitch_o,yaw_o,roll_o,                  !OK
     *pitch,yaw,roll,p_ang,r_ang,y_ang)
      do 50 i=1,3
      att_pitch(i)=p_ang(i)
      att_roll(i)=r_ang(i)
      att_yaw(i)=y_ang(i)
   50 continue
      fac_roll=r_ang(2)
      fac_yaw=y_ang(2)
      fac_pitch=p_ang(2)


      pix_spacing=c1_pxl
      call dop_coefs(burst_start,burst_end,nbeams,c1_flm,                       !OK
     *c2_flm,res_prod,fd_low_near,fd_low_far,fr_near,
     *fr_far,alt_dopcen,alt_rate,crt_dopcen,crt_rate)
      do 100 i=1,3
      dss_alt_dopcen(i)=alt_dopcen(i)
      dss_alt_rate(i)=alt_rate(i)
      dss_crt_dopcen(i)=crt_dopcen(i)
      dss_crt_rate(i)=crt_rate(i)
  100 continue
      dop_frq_const=crt_dopcen(1)
      dop_frq_slope=crt_dopcen(2)
      dop_frq_quad=crt_dopcen(3)
      dop_frq_r_cnst=crt_rate(1)
      dop_frq_r_slope=crt_rate(2)
      dop_frq_r_quad=crt_rate(3)

      call geo_alt(re,rp,xs,ys,zs,re_nadir,alt)                                 !OK
      S_C_altitude=alt/1000.0
      isc_dist=re_nadir+alt       !Map projection record
      map_geo_alt=alt

      call inclook(burst_start,burst_end,i_ang_bip,                             !OK
     *look_ang,inc_ang,lk_ang)
      incident_ang=inc_ang
      incidence_angle=inc_ang
      ant_look_angle=lk_ang

      sclen=sc_len(beam_mode)*1000.0d0
      scene_len=sclen/1000.0d0                        ! changed 7-15-96
      scene_wid=sc_wid(beam_mode)                     ! changed 7-15-96
      actual_azimuth=sclen                            ! changed 7-15-96
      actual_range=scene_wid*1000.0d0                 ! changed 7-15-96

      xb=x_sc(burst_start)
      yb=y_sc(burst_start)
      zb=z_sc(burst_start)
      xm=xs
      ym=ys
      zm=zs
      xe=x_sc(burst_end)
      ye=y_sc(burst_end)
      ze=z_sc(burst_end)
      call plat_head(xb,yb,zb,xm,ym,zm,xe,ye,ze,re,rp,                          !OK
     *re_nadir,sc_lat,sclen,trk_ang)
      dss_plat_head=trk_ang
      track_angle=trk_ang
      map_plat_head=trk_ang


      gha_start=gha
      omega=omega_e/dtr
      start_time=sv_ref_time
      sctime=sc_jd_sec
      call plat_loc(sctime,burst_start,burst_end,                    !OK
     *t_burst,x_sc,y_sc,z_sc,re_nadir,
     *p_lat,p_lon)
      plat_lat=p_lat
      plat_long=p_lon
      write (*,*) 'JEFF back from plat_loc'

      write (*,*) 'JEFFS about to call posdata'
      write (*,*) 'tb(i) = ',t_burst(1)
      write (*,*) 'tb(i) = ',t_burst(2)
      write (*,*) 'tb(i) = ',t_burst(3)
      write (*,*) 'tb(i) = ',t_burst(4)
      write (*,*) 'tb(i) = ',t_burst(5)
      write (*,*) 'tb(i) = ',t_burst(6)
      call posdata(burst_start,burst_end,prf,t_burst,np,
     *np_air,x_sc,y_sc,z_sc,v_x_sc,v_y_sc,v_z_sc,xpos,ypos,
     *zpos,xvel,yvel,zvel,ppd_time,ppd_int)
      do 200 i=1,3
      x_pos(i)=xpos(i)/1000.0d0
      y_pos(i)=ypos(i)/1000.0d0
      z_pos(i)=zpos(i)/1000.0d0
      x_vel(i)=xvel(i)
      y_vel(i)=yvel(i)
      z_vel(i)=zvel(i)
  200 continue
      fac_X_pos=xpos(2)/1000.0d0
      fac_Y_pos=ypos(2)/1000.0d0
      fac_Z_pos=zpos(2)/1000.0d0
      fac_X_vel=xvel(2)
      fac_Y_vel=yvel(2)
      fac_Z_vel=zvel(2)
      write (*,*) 'JEFF back from posdata'
      write (*,*) 'ppdtime = ',ppd_time
      fusec=dmod(ppd_time,1.0d0)
      stime=int(ppd_time-fusec)
      call ibm_gmtime(stime,tarray)
c      call gmtime(stime,tarray)
      pos_year=tarray(6)+1900
      pos_month=tarray(5)+1
      pos_day=tarray(4)
      pos_gmt_day=tarray(8)+1
      pos_gmt_sec=dble(3600*tarray(3)+60*tarray(2)+tarray(1))+fusec
      data_int=ppd_int

      vx=v_x_sc(burst_start)
      vy=v_y_sc(burst_start)
      vz=v_z_sc(burst_start)
      px=p_x(burst_start)
      py=p_y(burst_start)
      pz=p_z(burst_start)
      call squint(vx,vy,vz,px,py,pz,sq_ang)                                     !OK
      fac_squint_angle=sq_ang
      write (*,*) 'JEFF back from squint'

      call sw_speed(re_nadir,xs,ys,zs,vxs,vys,vzs,speed)                        !OK
      swath_velocity=speed
      isc_vel=speed     !Map projection record
      write (*,*) 'JEFF back from swath speed'

      call eme_kepler(xb,yb,zb,vx,vy,vz,kepler)                                 !OK

      do 34 i=1,6
      write (*,*) 'kepler',i,' = ',kepler(i)
      orbit_ele(i)=kepler(i)
   34 continue

c  Map projection record
      orb_incl=kepler(3)
      asc_node=kepler(4)

      write (*,*) 'JEFF back from eme_kepler'

      call sgmt(sc_year,sc_doy,sc_hr,sc_min,sc_sec,sc_usec,
     *gmtstr1,gmtstr2,datayear)       
      write (*,*) 'JEFF back from sgmt'
c      inp_sctim=gmtstr1      ! commented out by JMS 9-7-96 this val set in get_ldr
      center_GMT=gmtstr2
      data_year=datayear
      total_pixels=image_pix_cnt(1)+left_fill_cnt(1)+right_fill_cnt(1)
      n_pixel=total_pixels   ! Map projection record
      total_lines=nlines
      n_line=nlines          ! Map projection record
      imax=0
      do 47 i=1,nlines
      if (imax.lt.image_pix_cnt(i)) imax=image_pix_cnt(i)
   47 continue
      write (*,*) 'JEFF done with imax loop'
      actual_pixels=imax
      ifirst=left_fill_cnt(linec)+1
      ilast=left_fill_cnt(linec)+image_pix_cnt(linec)
      sc_lin=linec
      sc_pix=(ifirst+ilast)/2
      sl_rng_1st_pix=r_low(1)/1000.0
      sl_rng_last_pix=r_high(burst_end)/1000.0
c      n_azilok=8.0/float(nbeams)
      kb=(burst_start+burst_end)/2                        !  JMS 5-10-96
      del_x_az=prf(kb)*r_cntr(kb)*dtr*1.223d0/(4607.31d0*dble(np(kb)))
      n_azilok=2.0*float(res_prod)/del_x_az
      write (*,*) 'JEFF done with n_azilok'
      s_near=dsin(dtr*inc_ang_near(beam_mode))
      s_far=dsin(dtr*inc_ang_far(beam_mode))
      del_rg_near=cspeed/(2.0d0*fs*s_near)
      del_rg_far=cspeed/(2.0d0*fs*s_far)
      x_nlok_near=float(res_prod)/del_rg_near
      x_nlok_far=float(res_prod)/del_rg_far
c      n_rnglok=(x_nlok_near+x_nlok_far)/2.0d0
      n_rnglok=(x_nlok_near+x_nlok_far)/2.4d0              ! JMS 5-10-96 per TB reduce by 1.2
      write (*,*) 'JEFF done with n_rnglok'
      sum=0.0d0
      do 56 i=burst_start,burst_end
      sum=sum+prf(i)
   56 continue
      avg_prf=sum/dble(burst_end-burst_start+1)
      bnd_azilok=7.0d-01*avg_prf/n_azilok
      bnd_rnglok=chirp_bw/n_rnglok
      bnd_azi=7.0d-01*(prf(1)+prf(burst_end-nbeams+1))/2.0d0
      bnd_rng=chirp_bw

      dss_az_ovlp_nxt_img=0
      dss_rg_off_nxt_img=0

c  JMS 5-9-96 fill in dqs item 77
      enl=n_azilok*n_rnglok

c  JMS 5-9-96 fill in fac items 69-70
      looks_azimuth=n_azilok
      looks_range=n_rnglok

      write (*,*) 'JEFF back from everything'
      return
      end


c  This subroutine computes the scene center attitude angles by 
c  interpolating from the values contained in key_pp.inc.
c  It also loads the first and last pixel angles from the 
c  corresponding kburst values.

      subroutine attdata(burst_start,burst_end,pitch_o,yaw_o,roll_o,
     *dpitch,dyaw,droll,pitch,roll,yaw)
      implicit none
      integer burst_start,burst_end
      include 'ssp2_const.inc'
      real*8 pitch_o(burst_max),roll_o(burst_max),yaw_o(burst_max)
      real*8 dpitch(burst_max),droll(burst_max),dyaw(burst_max)
      real*8 pitch(3),roll(3),yaw(3)
      integer kburst,kb1,kb2,nburst
      real*8 p1,r1,y1,p2,r2,y2

c     do first pixel

      kburst=burst_start
      pitch(1)=pitch_o(kburst)+dpitch(kburst)
      roll(1)=roll_o(kburst)+droll(kburst)
      yaw(1)=yaw_o(kburst)+dyaw(kburst)

c     do last pixel

      kburst=burst_end
      pitch(3)=pitch_o(kburst)+dpitch(kburst)
      roll(3)=roll_o(kburst)+droll(kburst)
      yaw(3)=yaw_o(kburst)+dyaw(kburst)

c     do scene center pixel

      nburst=burst_end-burst_start+1
      kb1=nburst/2+burst_start-1
      kb2=kb1+1
      if (mod(nburst,2).eq.1) kb1=kb2
      p1=pitch_o(kb1)+dpitch(kb1)
      r1=roll_o(kb1)+droll(kb1)
      y1=yaw_o(kb1)+dyaw(kb1)
      p2=pitch_o(kb2)+dpitch(kb2)
      r2=roll_o(kb2)+droll(kb2)
      y2=yaw_o(kb2)+dyaw(kb2)
      pitch(2)=(p1+p2)/2.0d0
      roll(2)=(r1+r2)/2.0d0
      yaw(2)=(y1+y2)/2.0d0

      return
      end



c Compute coefficients of Doppler center frequency and rate as
c functions of along track and cross track distance in pixels.
c Selection of points to fit is according to scheme developed
c by A. Chu, which depends on the the beam mode of the track.
c A least squares fit is used to compute the coefficients.


      subroutine dop_coefs(burst_start,burst_end,nbeams,c1_flm,
     *c2_flm,res_prod,fd_low_near,fd_low_far,fr_near,
     *fr_far,alt_dopcen,alt_rate,crt_dopcen,crt_rate)
      implicit none
      real*8 lat,lon,latc
      integer burst_start,burst_end,burst0
      include 'ssp2_const.inc'
      real*8 c1_flm(2,2,burst_max),c2_flm(2,2,burst_max)
      real*8 fd_low_near(burst_max),fd_low_far(burst_max)
      real*8 fr_near(burst_max),fr_far(burst_max)
      real*8 alt_dopcen(3)
      real*8 alt_rate(3)
      real*8 crt_dopcen(3)
      real*8 crt_rate(3)
      real*8 x(6),yd(6),yr(6)
      real*8 cmat(3,4),pix_spacing
      integer npts,nbursts,ncoefs,nbeams,nb_alt,int_alt
      integer i,kb,res_prod

c Calculate pixel spacing
      if (res_prod.eq.75) then
        pix_spacing=50.0d0
      else if (res_prod.eq.150) then
        pix_spacing=100.0d0
      else if (res_prod.eq.600) then
        pix_spacing=100.0d0
      endif

c Along-track coefficients

      npts=6
      ncoefs=3
      nbursts=burst_end-burst_start+1
      nb_alt=nbursts/nbeams
      int_alt=nb_alt/5                     ! six points
      burst0=burst_start+nbeams-2
      do 10 i=1,npts
      kb=burst0+(i-1)*int_alt*nbeams
      x(i)=(c1_flm(1,1,kb)-c1_flm(1,1,burst0))/pix_spacing
      yd(i)=fd_low_near(kb)
      yr(i)=fr_near(kb)
   10 continue
      call lsf(x,yd,npts,alt_dopcen,ncoefs,cmat)
      call lsf(x,yr,npts,alt_rate,ncoefs,cmat)

      if (int_alt.lt.1) then
        do 14 i=1,3
        alt_dopcen(i)=0.0d0
        alt_rate(i)=0.0d0
   14   continue
      endif

c Cross-track coefficients

      burst0=burst_start
      if (nbeams.eq.2) then
        npts=4
        x(1)=0.0d0
        x(2)=(c2_flm(1,1,burst0+1)-c2_flm(1,1,burst0))/pix_spacing
        x(3)=(c2_flm(1,2,burst0)-c2_flm(1,1,burst0))/pix_spacing
        x(4)=(c2_flm(1,2,burst0+1)-c2_flm(1,1,burst0))/pix_spacing
        yd(1)=fd_low_near(burst0)
        yd(2)=fd_low_near(burst0+1)
        yd(3)=fd_low_far(burst0)
        yd(4)=fd_low_far(burst0+1)
        yr(1)=fr_near(burst0)
        yr(2)=fr_near(burst0+1)
        yr(3)=fr_far(burst0)
        yr(4)=fr_far(burst0+1)
      elseif (nbeams.eq.3) then
        npts=6
        x(1)=0.0d0
        x(2)=(c2_flm(1,1,burst0+1)-c2_flm(1,1,burst0))/pix_spacing
        x(3)=(c2_flm(1,2,burst0)-c2_flm(1,1,burst0))/pix_spacing
        x(4)=(c2_flm(1,1,burst0+2)-c2_flm(1,1,burst0))/pix_spacing
        x(5)=(c2_flm(1,2,burst0+1)-c2_flm(1,1,burst0))/pix_spacing
        x(6)=(c2_flm(1,2,burst0+2)-c2_flm(1,1,burst0))/pix_spacing
        yd(1)=fd_low_near(burst0)
        yd(2)=fd_low_near(burst0+1)
        yd(3)=fd_low_far(burst0)
        yd(4)=fd_low_near(burst0+2)
        yd(5)=fd_low_far(burst0+1)
        yd(6)=fd_low_far(burst0+2)
        yr(1)=fr_near(burst0)
        yr(2)=fr_near(burst0+1)
        yr(3)=fr_far(burst0)
        yr(4)=fr_near(burst0+2)
        yr(5)=fr_far(burst0+1)
        yr(6)=fr_far(burst0+2)
      elseif (nbeams.eq.4) then
        npts=6
        x(1)=0.0d0
        x(2)=(c2_flm(1,1,burst0+1)-c2_flm(1,1,burst0))/pix_spacing
        x(3)=(c2_flm(1,1,burst0+2)-c2_flm(1,1,burst0))/pix_spacing
        x(4)=(c2_flm(1,2,burst0+1)-c2_flm(1,1,burst0))/pix_spacing
        x(5)=(c2_flm(1,1,burst0+3)-c2_flm(1,1,burst0))/pix_spacing
        x(6)=(c2_flm(1,2,burst0+3)-c2_flm(1,1,burst0))/pix_spacing
        yd(1)=fd_low_near(burst0)
        yd(2)=fd_low_near(burst0+1)
        yd(3)=fd_low_near(burst0+2)
        yd(4)=fd_low_far(burst0+1)
        yd(5)=fd_low_near(burst0+3)
        yd(6)=fd_low_far(burst0+3)
        yr(1)=fr_near(burst0)
        yr(2)=fr_near(burst0+1)
        yr(3)=fr_near(burst0+2)
        yr(4)=fr_far(burst0+1)
        yr(5)=fr_near(burst0+3)
        yr(6)=fr_far(burst0+3)
      endif

      call lsf(x,yd,npts,crt_dopcen,ncoefs,cmat)
      call lsf(x,yr,npts,crt_rate,ncoefs,cmat)

      return
      end


c  Computes geodetic satellite altitude using satellite coordinates
c  and nadir radius. 

      subroutine geo_alt(re,rp,xs,ys,zs,re_nadir,alt)
      implicit none
      real*8 xx,zz,xn,zn,dx,dz,alt
      real*8 re,rp,xs,ys,zs,re_nadir

c  Compute x,z coordinates at nadir, in plane containing nadir
      xx=re**2*((re_nadir**2-rp**2)/(re**2-rp**2))
      zz=rp**2*(1.0d0-xx/re**2)
      xn=dsqrt(xx)
      zn=dsqrt(zz)

c  Compute altitude
      dx=dsqrt(xs**2+ys**2)-xn
      dz=zs-zn
      alt=dsqrt(dx**2+dz**2)
      return
      end



      subroutine radii(re,rp,xs,ys,zs,sclat,re_nadir,re_center)
      implicit none
      real*8 pie4,dtr,re,rp,xs,ys,zs,sclat,top,bot
      real*8 re_nadir,re_center,tl,ff,xx,zz,dd

      pie4=datan(1.0d0)
      dtr=pie4/45.0d0

c  Compute tangent of scene center geodetic latitude and reciprocal
c  of flattening factor.
      tl=dtan(dtr*sclat)
      if (abs(sclat-90.0d0).lt.1.0d-09) tl=1.0d09
      ff=(re-rp)/re

c  Compute earth radius at nadir
      xx=xs**2+ys**2
      zz=zs**2
      dd=xx/(xx+zz)
      re_nadir=re*(1.0d0-ff)/dsqrt(1.0d0-(ff*(2.0d0-ff))*dd)

c  Compute earth radius at scene center
      top=1.0d0+(1.0d0-ff)**4*tl**2
      bot=1.0d0+tl**2*(1.0d0-ff)**2
      re_center=re*dsqrt(top/bot)

      return
      end



c Assemble GMT as a character string per CEOS format, from 
c input year, day of year, hours, minutes, and seconds.

      subroutine sgmt(year,doy,hr,min,sec,usec,gmtstr1,
     *gmtstr2,datayear)
      implicit none
      integer i
      real*8 fsec
      integer year,doy,hr,sec,usec,yr,min
      character*32 gmtstr1,gmts1
      character*27 gmtstr2,gmts2
      character*6 datayear,dystr1,dystr2
      character*1 gmtbuf(32),dybuf(6)
      equivalence(gmts1,gmtbuf(1))
      equivalence(gmts2,gmtbuf(6))
      equivalence(dystr1,gmtbuf(1))
      equivalence(dystr2,dybuf(1))

      do 10 i=1,32
      gmtbuf(i)=' '
   10 continue
      yr=year
      fsec=dble(sec)+dble(usec)/1000000.0d0
      write (*,*) 'encode in sgmt'
      write (unit=gmts1,fmt=1000) yr,doy,hr,min,fsec
      write (*,*) 'back from encode in sgmt'
c      encode(21,1000,gmts1) yr,doy,hr,min,fsec
      do 20 i=1,21
      if(gmtbuf(i).eq.' ') gmtbuf(i)='0'
   20 continue
      gmtstr1=gmts1
      gmtstr2=gmts2
      dystr2=dystr1
      dybuf(5)=' '
      dybuf(6)=' '
      datayear=dystr2
      return
 1000 format (i4,'-',i3,':',i2,':',i2,':',f6.3)
      end
      


c  This subroutine computes the scene center incidence and look 
c  angles by interpolating from the values contained in key_pp.inc.

      subroutine inclook(burst_start,burst_end,i_ang_bip,
     *look_ang,inc_ang,lk_ang)
      implicit none
      integer nburst,kb1,kb2
      integer burst_start,burst_end
      real*8  inc_ang,lk_ang,i1,i2,l1,l2
      include 'ssp2_const.inc'
      real*8 i_ang_bip(burst_max)
      real*8 look_ang(burst_max)

c     do scene center pixel

      nburst=burst_end-burst_start+1
      kb1=nburst/2+burst_start-1
      kb2=kb1+1
      if (mod(nburst,2).eq.1) kb1=kb2
      i1=i_ang_bip(kb1)
      l1=look_ang(kb1)
      i2=i_ang_bip(kb2)
      l2=look_ang(kb2)
      lk_ang=(l1+l2)/2.0d0
      inc_ang=(i1+i2)/2.0d0
      return
      end

 



c This subroutine computes the platform heading, using the
c ASP algorithm.

      subroutine plat_head(xb,yb,zb,xm,ym,zm,xe,ye,ze,re,rp,
     *re_nadir,sclat,sclen,trk_ang)
      implicit none
      real*8 dtr,pie4,xb,yb,zb,xm,ym,zm,xe,ye,ze
      real*8 re,rp,re_nadir,sclat,sclen,trk_ang
      real*8 xx,yy,zz,dd1,dd2,arg

      pie4=datan(1.0d0)
      dtr=pie4/45.0d0

      xx=xb**2
      yy=yb**2
      zz=zb**2
      dd1=(xx+yy)/(xx+yy+zz)
      dd1=dacos(dsqrt(dd1))

      xx=xe**2
      yy=ye**2
      zz=ze**2
      dd2=(xx+yy)/(xx+yy+zz)
      dd2=dacos(dsqrt(dd2))

      arg=(re_nadir*dabs(dd2-dd1)/sclen)
      if (dabs(arg).gt.1.0d0) arg=arg/dabs(arg)
      trk_ang=dasin(arg)
      trk_ang=trk_ang/dtr
      if((dd2.gt.dd1).or.((dd2.eq.dd1).and.zm.gt.0.0d0)) then
        trk_ang=270.0d0+trk_ang
      else 
        trk_ang=270.0d0-trk_ang
      endif

      return
      end


c Compute platform latitude and longitude.

      subroutine plat_loc(cent_time,burst_start,burst_end,
     *tb,xsc,ysc,zsc,re_nadir,lat,lon)
      implicit none
      real*8 lat,lon,latc
      integer burst_start,burst_end
      include 'ssp2_const.inc'
      real*8 xsc(burst_max),ysc(burst_max),zsc(burst_max)
      real*8 tb(burst_max),cent_time
      real*8 re_nadir,pie4,dtr,ecc,ff
      real*8 xpos,ypos,zpos,xm,zm
      integer i,j

      pie4=datan(1.0d0)
      dtr=pie4/45.0d0
      ecc=dsqrt(1.0d0-rp**2/re**2)

c First, look up correct burst to correlate S/C position

      do 10 i=burst_start,burst_end
      j=i-1
      if (cent_time.lt.tb(i)) go to 15
   10 continue
   15 continue
      if (j.lt.1) j=1

c Now do linear interpolation of S/C coordinates from this burst to
c next one (I hope linear is good enough)

      ff=(cent_time-tb(j))/(tb(j+1)-tb(j))
      xpos=xsc(j)+ff*(xsc(j+1)-xsc(j))
      ypos=ysc(j)+ff*(ysc(j+1)-ysc(j))
      zpos=zsc(j)+ff*(zsc(j+1)-zsc(j))
      write (*,*) 'JEFF in plat_loc'
      write (*,*) 'j,cent_time = ',j,cent_time

c Compute x and z coordinates at earth surface in S/C meridian plane 
c (y=0) and latitude

      xm=re*dsqrt((re_nadir**2-rp**2)/(re**2-rp**2))
      zm=rp*dsqrt(1.0d0-xm**2/re**2)
      if(zpos.lt.0.0d0) zm=-zm
      latc=datan2(zm,xm)
      lat=atan(tan(latc)/(1.0d0-ecc**2))/dtr

c Compute longitude

      lon=datan2(ypos,xpos)/dtr

      return
      end

c  This subroutine computes the first and last pixel, and scene center
c  positions and velocities by interpolating and extrapolating from
c  the values contained in key_pp.inc.

      subroutine posdata(burst_start,burst_end,prf,tb,np,np_air,x_sc,
     *y_sc,z_sc,v_x_sc,v_y_sc,v_z_sc,xpos,ypos,zpos,xvel,yvel,zvel,
     *ppd_time,ppd_int)
      implicit none
      integer burst_start,burst_end
      include 'ssp2_const.inc'
      real*8 prf(burst_max),tb(burst_max)
      integer np(burst_max),np_air(burst_max)
      integer kburst,npvalid,kb1,kb2,nburst
      real*8 x_sc(burst_max),y_sc(burst_max),z_sc(burst_max)
      real*8 v_x_sc(burst_max),v_y_sc(burst_max),v_z_sc(burst_max)
      real*8 xpos(3),ypos(3),zpos(3),delt
      real*8 xvel(3),yvel(3),zvel(3),xacc,yacc,zacc
      real*8 ppd_time,jd_1970_sec,ppd_int,ppdt

c      jd_1970_sec=2440586.5d0*86400.0d0
      jd_1970_sec=2440587.5d0*86400.0d0        ! JMS 8-6-96

c     do first pixel

      kburst=burst_start
      npvalid=np(kburst)-np_air(kburst)
      delt=dble(npvalid)/(2.0d0*prf(kburst))
      xpos(1)=x_sc(kburst)-delt*v_x_sc(kburst)
      ypos(1)=y_sc(kburst)-delt*v_y_sc(kburst)
      zpos(1)=z_sc(kburst)-delt*v_z_sc(kburst)

      xacc=v_x_sc(kburst+1)-v_x_sc(kburst)
      yacc=v_y_sc(kburst+1)-v_y_sc(kburst)
      zacc=v_z_sc(kburst+1)-v_z_sc(kburst)
      xvel(1)=v_x_sc(kburst)-xacc/2.0d0
      yvel(1)=v_y_sc(kburst)-yacc/2.0d0
      zvel(1)=v_z_sc(kburst)-zacc/2.0d0

      ppd_time=tb(kburst)-delt-jd_1970_sec

c     do last pixel

      kburst=burst_end
      npvalid=np(kburst)-np_air(kburst)
      delt=dble(npvalid)/(2.0d0*prf(kburst))
      xpos(3)=x_sc(kburst)+delt*v_x_sc(kburst)
      ypos(3)=y_sc(kburst)+delt*v_y_sc(kburst)
      zpos(3)=z_sc(kburst)+delt*v_z_sc(kburst)

      xacc=v_x_sc(kburst)-v_x_sc(kburst-1)
      yacc=v_y_sc(kburst)-v_y_sc(kburst-1)
      zacc=v_z_sc(kburst)-v_z_sc(kburst-1)
      xvel(3)=v_x_sc(kburst)+xacc/2.0d0
      yvel(3)=v_y_sc(kburst)+yacc/2.0d0
      zvel(3)=v_z_sc(kburst)+zacc/2.0d0


c     do scene center pixel

      nburst=burst_end-burst_start+1
      kb1=nburst/2+burst_start-1
      kb2=kb1+1
      if (mod(nburst,2).eq.1) kb1=kb2
      xpos(2)=(x_sc(kb1)+x_sc(kb2))/2.0d0
      ypos(2)=(y_sc(kb1)+y_sc(kb2))/2.0d0
      zpos(2)=(z_sc(kb1)+z_sc(kb2))/2.0d0
      xvel(2)=(v_x_sc(kb1)+v_x_sc(kb2))/2.0d0
      yvel(2)=(v_y_sc(kb1)+v_y_sc(kb2))/2.0d0
      zvel(2)=(v_z_sc(kb1)+v_z_sc(kb2))/2.0d0

      ppdt=(tb(kb1)+tb(kb2))/2.0d0-jd_1970_sec
      ppd_int=ppdt-ppd_time


      return
      end

 




c Compute platform squint angle from pointing vector
c and velocity vector.

      subroutine squint(vx,vy,vz,px,py,pz,sq_ang)
      implicit none
      real*8 pie4,dtr,vx,vy,vz,px,py,pz
      real*8 vdotp,vmag,pmag,theta,sq_ang

      pie4=datan(1.0d0)
      dtr=pie4/45.0d0

      write (*,*) 'JEFFS in squint'
      write (*,*) 'px,py,pz = ',px,py,pz
      write (*,*) 'vx,vy,vz = ',vx,vy,vz

      vdotp=vx*px+vy*py+vz*pz
      vmag=dsqrt(vx**2+vy**2+vz**2)
      pmag=dsqrt(px**2+py**2+pz**2)

      theta=dacos(vdotp/(vmag*pmag))
      sq_ang=90.0d0-theta/dtr

      return
      end




c Compute swath speed using A. Chu improved algorithm

      subroutine sw_speed(re_nadir,xs,ys,zs,vx,vy,vz,speed)
      implicit none
      include 'ssp2_const.inc'
      real*8 re_nadir,xs,ys,zs,vx,vy,vz,speed
      real*8 vdotr,rs,vmag,vrmag,cos_theta,sin_theta
      real*8 vs_tang

c Compute dot product of s/c radius vector with velocity
      vdotr=xs*vx+ys*vy+zs*vz

c Compute s/c distance from earth center
      rs=dsqrt(xs**2+ys**2+zs**2)

c Compute s/c velocity magnitude
      vmag=dsqrt(vx**2+vy**2+vz**2)

c Compute cosine and sine of angle between velocity vector
c and tangential component
      vrmag=rs*vmag
      cos_theta=vdotr/vrmag
      sin_theta=dsqrt(1.0d0-cos_theta**2)

c Compute tangential component and scale it by ratio of s/c
c distance to earth radius to get ground velocity
      vs_tang=sin_theta*vmag
      speed=vs_tang*re_nadir/rs

      return
      end



      subroutine ceos_calc_flm(nlines,kburst,sc_year,sc_doy,sc_hr,
     *sc_min,sc_sec,sc_usec,sc_lat,sc_lon,sc_jd_sec,burst_start,
     *burst_end,x_sc,y_sc,z_sc,v_x_sc,v_y_sc,v_z_sc,pitch_o,yaw_o,
     *roll_o,pitch,yaw,roll,c1_pxl,c2_pxl,c1_flm,c2_flm,fd_low_near,
     *fd_low_far,fr_near,fr_far,i_ang_bip,look_ang,gha,sv_ref_time,
     *prf,t_burst,r_low,r_high,np,np_air,p_x,p_y,p_z,fs,chirp_bw,
     *nbeams,beam_mode,c1_g,c2_g,c2_cell,c1size_blk,n_c2_blk,
     *res_prod,r_cntr)



      implicit none
      include 'ceos.inc'
      include 'ssp2_const.inc'
      character*1 m,d,s
      real*8 alt_dopcen(3)
      real*8 alt_rate(3)
      real*8 crt_dopcen(3)
      real*8 crt_rate(3)
      real*8 kepler(6)
      integer year,doy,hr
      integer sc_year,sc_doy,sc_hr,sc_min,sc_sec,sc_usec
      character*32 gmtstr
      character*20 scen_id
      character*16 dtake_id
      character*12 img_id
      integer orbit,seg,frame,ver
      real*8 inc_ang,lk_ang
      real*8 xpos(3),ypos(3),zpos(3)
      real*8 xvel(3),yvel(3),zvel(3)
      real*8 p_ang(3),r_ang(3),y_ang(3)
      character*32 gmtstr1
      character*27 gmtstr2
      character*6 datayear
      real*8 inc_ang_near(4),inc_ang_far(4)
      real*8 sc_len(4),sc_wid(4)
      integer usec,i
      integer nlines,linec,act_lines,nburst,kb1,kb2
      real*8 sc_lat,sc_lon,sc_jd_sec,dtr,pie4
      real*8 xs,ys,zs,vxs,vys,vzs,re_nadir,re_center
      real*8 pix_spacing,alt,sclen,xb,yb,zb,xm,ym,zm,xe,ye,ze
      real*8 trk_ang,gha_start,omega,start_time,sctime,p_lat,p_lon
      real*8 vx,vy,vz,px,py,pz,sq_ang,speed,s_near,s_far
      integer imax,ifirst,ilast,kburst,idummy
      real*8 del_rg_near,del_rg_far,x_nlok_near,x_nlok_far
      real*8 sum,avg_prf,x
      real*8 ppd_time,ppd_int
      integer burst_start,burst_end
      real*8 x_sc(burst_max),y_sc(burst_max),z_sc(burst_max)
      real*8 v_x_sc(burst_max),v_y_sc(burst_max),v_z_sc(burst_max)
      real*8 pitch_o(burst_max),yaw_o(burst_max),roll_o(burst_max)
      real*8 pitch(burst_max),yaw(burst_max),roll(burst_max)
      real*8 c1_pxl,c2_pxl,c1_flm(2,2,burst_max),c2_flm(2,2,burst_max)
      real*8 fd_low_near(burst_max),fd_low_far(burst_max)
      real*8 fr_near(burst_max),fr_far(burst_max),i_ang_bip(burst_max)
      real*8 look_ang(burst_max),gha,sv_ref_time,prf(burst_max)
      real*8 t_burst(burst_max),r_low(burst_max),r_high(burst_max)
      integer np(burst_max),np_air(burst_max)
      real*8 p_x(burst_max),p_y(burst_max),p_z(burst_max)
      real*8 fs,chirp_bw
      integer nbeams,beam_mode
      real*8 c1_g(3,3,n_az,n_rg,burst_max)
      real*8 c2_g(3,3,n_az,n_rg,burst_max)
      real*8 c2_cell(burst_max),c1size_blk(burst_max)
      integer n_c2_blk(burst_max)
      integer res_prod
      real*8 c1_pix,c2_pix
      real*8 r_cntr(burst_max)              ! JMS 5-10-96
      real*4 del_x_az                       ! JMS 5-10-96
      integer kb                            ! JMS 5-10-96


c   May want to move near and far range incidence angles to inc file.
      data inc_ang_near/20.0d0,20.0d0,20.0d0,30.81d0/
      data inc_ang_far/49.42d0,46.58d0,39.55d0,46.58d0/
c   May want to move scene dimensions to inc file.
      data sc_len/512.0d0,512.0d0,307.2d0,307.2d0/
      data sc_wid/512.0d0,459.8d0,307.2d0,298.3d0/

      pie4=datan(1.0d0)
      dtr=pie4/45.0d0
      usec=sc_usec

      nburst=burst_end-burst_start+1
      kb1=nburst/2+burst_start-1
      kb2=kb1+1
      if (mod(nburst,2).eq.1) kb1=kb2
      xs=(x_sc(kb1)+x_sc(kb2))/2.0d0   ! may want to redo these based on lat & lon
      ys=(y_sc(kb1)+y_sc(kb2))/2.0d0
      zs=(z_sc(kb1)+z_sc(kb2))/2.0d0
      vxs=(v_x_sc(kb1)+v_x_sc(kb2))/2.0d0 
      vys=(v_y_sc(kb1)+v_y_sc(kb2))/2.0d0 
      vzs=(v_z_sc(kb1)+v_z_sc(kb2))/2.0d0 
      call radii(re,rp,xs,ys,zs,sc_lat,re_nadir,re_center)                       !OK
      nadir_radius=re_nadir/1000.0
      image_radius=re_center/1000.0

      call attdata(burst_start,burst_end,pitch_o,yaw_o,roll_o,                  !OK
     *pitch,yaw,roll,p_ang,r_ang,y_ang)
      do 50 i=1,3
      att_pitch(i)=p_ang(i)
      att_roll(i)=r_ang(i)
      att_yaw(i)=y_ang(i)
   50 continue
      fac_roll=r_ang(2)
      fac_yaw=y_ang(2)
      fac_pitch=p_ang(2)


      pix_spacing=c1_pxl
      call dop_coefs(burst_start,burst_end,nbeams,c1_flm,
     *c2_flm,res_prod,fd_low_near,fd_low_far,fr_near,
     *fr_far,alt_dopcen,alt_rate,crt_dopcen,crt_rate)
      do 100 i=1,3
      dss_alt_dopcen(i)=alt_dopcen(i)
      dss_alt_rate(i)=alt_rate(i)
      dss_crt_dopcen(i)=crt_dopcen(i)
      dss_crt_rate(i)=crt_rate(i)
  100 continue
      dop_frq_const=crt_dopcen(1)
      dop_frq_slope=crt_dopcen(2)
      dop_frq_quad=crt_dopcen(3)
      dop_frq_r_cnst=crt_rate(1)
      dop_frq_r_slope=crt_rate(2)
      dop_frq_r_quad=crt_rate(3)

      call geo_alt(re,rp,xs,ys,zs,re_nadir,alt)                                 !OK
      S_C_altitude=alt/1000.0
      isc_dist=re_nadir+alt       !Map projection record
      map_geo_alt=alt

      call inclook(burst_start,burst_end,i_ang_bip,                             !OK
     *look_ang,inc_ang,lk_ang)
      incident_ang=inc_ang
      incidence_angle=inc_ang
      ant_look_angle=lk_ang

      sclen=sc_len(beam_mode)*1000.0d0
      scene_len=sclen
      scene_wid=sc_wid(beam_mode)*1000.0d0
      actual_azimuth=scene_len
      actual_range=scene_wid

      xb=x_sc(burst_start)
      yb=y_sc(burst_start)
      zb=z_sc(burst_start)
      xm=xs
      ym=ys
      zm=zs
      xe=x_sc(burst_end)
      ye=y_sc(burst_end)
      ze=z_sc(burst_end)
      call plat_head(xb,yb,zb,xm,ym,zm,xe,ye,ze,re,rp,                          !OK
     *re_nadir,sc_lat,sclen,trk_ang)
      dss_plat_head=trk_ang
      track_angle=trk_ang
      map_plat_head=trk_ang


      gha_start=gha
      omega=omega_e/dtr
      start_time=sv_ref_time
      sctime=sc_jd_sec
      write (*,*) 'JEFF in ceos_calc'
      write (*,*) 'sctime = ',sctime
      call plat_loc(sctime,burst_start,burst_end,                    !OK
     *t_burst,x_sc,y_sc,z_sc,re_nadir,
     *p_lat,p_lon)
      plat_lat=p_lat
      plat_long=p_lon

      call posdata(burst_start,burst_end,prf,t_burst,np,
     *np_air,x_sc,y_sc,z_sc,v_x_sc,v_y_sc,v_z_sc,xpos,ypos,
     *zpos,xvel,yvel,zvel,ppd_time,ppd_int)
      do 200 i=1,3
      x_pos(i)=xpos(i)/1000.0d0
      y_pos(i)=ypos(i)/1000.0d0
      z_pos(i)=zpos(i)/1000.0d0
      x_vel(i)=xvel(i)
      y_vel(i)=yvel(i)
      z_vel(i)=zvel(i)
  200 continue
      fac_X_pos=xpos(2)/1000.0d0
      fac_Y_pos=ypos(2)/1000.0d0
      fac_Z_pos=zpos(2)/1000.0d0
      fac_X_vel=xvel(2)
      fac_Y_vel=yvel(2)
      fac_Z_vel=zvel(2)


      vx=v_x_sc(burst_start)
      vy=v_y_sc(burst_start)
      vz=v_z_sc(burst_start)
      px=p_x(burst_start)
      py=p_y(burst_start)
      pz=p_z(burst_start)
      call squint(vx,vy,vz,px,py,pz,sq_ang)                                     !OK
      fac_squint_angle=sq_ang

      call sw_speed(re_nadir,xs,ys,zs,vxs,vys,vzs,speed)                        !OK
      swath_velocity=speed
      isc_vel=speed     !Map projection record

      call eme_kepler(xb,yb,zb,vx,vy,vz,kepler)                                 !OK

      call sgmt(sc_year,sc_doy,sc_hr,sc_min,sc_sec,sc_usec,
     *gmtstr1,gmtstr2,datayear)
c      inp_sctim=gmtstr1     ! commented out JMS 9-7-96 now set in get_ldr
      center_GMT=gmtstr2
      data_year=datayear
      c1_pix=100.0d0
      c2_pix=100.0d0
      idummy=int(sam_raw*c2_cell(kburst)/c2_pix)
c      actual_pixels=min(sam_post,idummy)
c      total_pixels=sam_post
      idummy=int(c1size_blk(kburst)/(c1_pix+5.0d0))
c      actual_lines=min(np_geo,idummy)
c      total_lines=np_geo
      sl_rng_1st_pix=r_low(1)/1000.0
      sl_rng_last_pix=r_high(burst_end)/1000.0
c      n_azilok=8.0/float(nbeams)
      kb=(burst_start+burst_end)/2                   ! 5-10-96
      del_x_az=prf(kb)*r_cntr(kb)*dtr*1.223d0/(4607.31d0*dble(np(kb)))
      n_azilok=2.0*float(res_prod)/del_x_az
      s_near=dsin(dtr*inc_ang_near(beam_mode))
      s_far=dsin(dtr*inc_ang_far(beam_mode))
      del_rg_near=cspeed/(2.0d0*fs*s_near)
      del_rg_far=cspeed/(2.0d0*fs*s_far)
      x_nlok_near=float(res_prod)/del_rg_near
      x_nlok_far=float(res_prod)/del_rg_far
      n_rnglok=(x_nlok_near+x_nlok_far)/2.4d0
      sum=0.0d0
      do 56 i=burst_start,burst_end
      sum=sum+prf(i)
   56 continue
      avg_prf=sum/dble(burst_end-burst_start+1)
      bnd_azilok=7.0d-01*avg_prf/n_azilok
      bnd_rnglok=chirp_bw/n_rnglok
      bnd_azi=7.0d-01*(prf(1)+prf(burst_end-nbeams+1))/2.0d0
      bnd_rng=chirp_bw

      x=dabs(c1_g(1,3,1,n_c2_blk(kburst),kburst)-
     *c1_g(1,1,1,1,(kburst+1)))
      dss_az_ovlp_nxt_img=int(x/c1_pix)
      x=dabs(c2_g(1,1,1,1,(kburst+1))-c2_g(1,1,1,1,kburst))
      dss_rg_off_nxt_img=int(x/c2_pix)
      return
      end


      subroutine pvs_fname(in_fname,look_no,out1_fname,
     *                      status)

      implicit none
      character*60 fname1,in_fname,ldr_fname
      character*60 out1_fname
      character*60 out2_fname
      character*60 out3_fname
      character*60 out4_fname
      character*1 look_no
      integer status,i,j

c  Declarations for variables to strip path information from output
c  file name
      integer fbuf1len
      parameter (fbuf1len=60)
      byte fbuf1(fbuf1len),xc
      equivalence (fbuf1(1),fname1)

c  Strip path information from output file name
c  Begin by searching backwards from end of string for a "/"
      j=fbuf1len
      fname1=in_fname
      do 40 i=1,fbuf1len
      xc=fbuf1(j)
      if (xc.eq."/") go to 50
      j=j-1
   40 continue
   50 continue


      if (fbuf1(j+17).ne.'.') then
        write (*,*) 'bad file name'
        status=-1
      else
        status=0
cc        fbuf1(j+11)=mode+48
        fbuf1(j+13)='F'
        fbuf1(j+11)=look_no
        out1_fname=fname1

      endif

      return
      end

c---------------------------------------------------------------------

c	Get the path name of the file passed from the CP
c	Using for CEOS leader
c------------------------------------------------------------
c
      subroutine get_frame_id(in_fname,frame_id,status)

      implicit none
      character*256 fname1,in_fname
      integer status,i,j
      integer frame_id,frameno
      character*3 frame_number
      character*1 fr_num_buf(3)

c  Declarations for variables to strip path information from output
c  file name
      integer fbuf1len
      parameter (fbuf1len=256)
      byte fbuf1(fbuf1len),xc
      equivalence (fbuf1(1),fname1)
      equivalence(frame_number,fr_num_buf(1))

c  Strip path information from output file name
c  Begin by searching backwards from end of string for a "/"
      j=fbuf1len
      fname1=in_fname
      do 40 i=1,fbuf1len
      xc=fbuf1(j)
      if (xc.eq."/") go to 50
      j=j-1
   40 continue
   50 continue


      if (fbuf1(j+17).ne.'.') then
        write (*,*) 'bad file name'
        status=-1
      else
        status=0
        write (*,*) 'decode in get frame id'
c        decode(3,1000,fbuf1(j+8)) frameno
        fr_num_buf(1)=fbuf1(j+8)
        fr_num_buf(2)=fbuf1(j+9)
        fr_num_buf(3)=fbuf1(j+10)
        read(unit=frame_number,fmt=1000) frameno
        write (*,*) 'back from decode in get frame id'
        frame_id=frameno
      endif

1000  format (i3)
      return
      end
c------------------------------------------
c  Load the histogram processing image into the Leader file
c------------------------------------------
        subroutine ld_im_hist(act_lines,npix_max,minfreq,
     *  maxfreq,meansamp,stdvsamp,meanfreq,stdvfreq,hist)
        implicit none
        real*4 minfreq,maxfreq,meansamp,stdvsamp,meanfreq,stdvfreq
        integer hist(256),i,act_lines,npix_max
        integer act_max_bin         !  JMS 4-10-96 highest nonzero bin
        real*4  max_inten           !  JMS 4-10-96
        integer act_min_bin         !  JMS 5-9-96 lowest nonzero bin
        real*4  min_inten           !  JMS 5-9-96
        include 'ceos.inc'

        proc_nbin=256
        proc_min_smp=0
        proc_max_smp=255
        proc_mean_smp=meansamp
        proc_std_smp=stdvsamp
        proc_min_hist=minfreq
        proc_max_hist=maxfreq
        proc_mean_hist=meanfreq
        proc_std_hist=stdvfreq

        act_max_bin=2                    !  Good default in case of all zeroes.
        do 100 i=1,256
        proc_data_values(i)=hist(i)
        if (hist(i).gt.0) act_max_bin=i     !  JMS 4-10-96
100     continue

        act_min_bin=256                  !  Good default in case of all zeroes.
        do 150 i=1,255
        if (hist(257-i).gt.0) act_min_bin=257-i     !  JMS 5-9-96
150     continue

        proc_ns_lin=npix_max
        proc_ns_pix=act_lines
        proc_ngrp_lin=npix_max
        proc_ngrp_pix=act_lines
        proc_nsamp_lin=npix_max
        proc_nsamp_pix=act_lines

c  For Facility Data Record
        satur_points=hist(256)

c  For Data Quality Summary Record
        max_inten=float(act_max_bin-1)
        min_inten=float(act_min_bin-1)
        dyn_rng=20.0*alog10(max_inten/min_inten)

        return
        end


      subroutine ld_iq_hist(minfreq_i,maxfreq_i,meansamp_i,
     *stdvsamp_i,meanfreq_i,stdvfreq_i,hist_i,minfreq_q,
     *maxfreq_q,meansamp_q,stdvsamp_q,meanfreq_q,stdvfreq_q,
     *hist_q,np_v,burst_start,burst_end,ns)
	implicit none
        real*4 minfreq_i,maxfreq_i,meansamp_i,stdvsamp_i
        real*4 meanfreq_i,stdvfreq_i
        real*4 minfreq_q,maxfreq_q,meansamp_q,stdvsamp_q
        real*4 meanfreq_q,stdvfreq_q
        integer hist_i(256),hist_q(256),i
        include 'ceos.inc'
        include 'ssp2_const.inc'
        integer ns_min,npv
        integer burst_cen,nburst
        integer ns(burst_max),np_v(burst_max)
        integer burst_start,burst_end
        real*8 tol,vali,valq                      !  JMS 5-17-96

        tol=1.0d-10
        nburst=(burst_end-burst_start)+1
        burst_cen=(burst_end+burst_start)/2
        ns_min=999999999
        do 50 i=burst_start,burst_end
        if (ns(i).lt.ns_min) ns_min=ns(i)
   50   continue
        npv=np_v(burst_cen)

        raw_i_nbin=256
        raw_i_min_smp=-128
        raw_i_max_smp=127
        raw_i_mean_smp=meansamp_i
        raw_i_std_smp=stdvsamp_i
        raw_i_min_hist=minfreq_i
        raw_i_max_hist=maxfreq_i
        raw_i_mean_hist=meanfreq_i
        raw_i_std_hist=stdvfreq_i
        do 100 i=1,256
        raw_i_data_values(i)=hist_i(i)
  100   continue
        raw_i_ns_lin=ns_min
        raw_i_ngrp_lin=ns_min
        raw_i_nsamp_lin=ns_min
        raw_i_ns_pix=npv*nburst
        raw_i_ngrp_pix=npv*nburst
        raw_i_nsamp_pix=npv*nburst

        raw_q_nbin=256
        raw_q_min_smp=-128
        raw_q_max_smp=127
        raw_q_mean_smp=meansamp_q
        raw_q_std_smp=stdvsamp_q
        raw_q_min_hist=minfreq_q
        raw_q_max_hist=maxfreq_q
        raw_q_mean_hist=meanfreq_q
        raw_q_std_hist=stdvfreq_q
        do 200 i=1,256
        raw_q_data_values(i)=hist_q(i)
  200   continue
        raw_q_ns_lin=ns_min
        raw_q_ngrp_lin=ns_min
        raw_q_nsamp_lin=ns_min
        raw_q_ns_pix=npv*nburst
        raw_q_ngrp_pix=npv*nburst
        raw_q_nsamp_pix=npv*nburst

        i_bias=(meansamp_i-128.0)*7.5/127.0
        q_bias=(meansamp_q-128.0)*7.5/127.0
        vali=stdvsamp_i
        if (vali.lt.tol) vali=tol
        valq=stdvsamp_q
        if (valq.lt.tol) valq=tol
        iq_ratio=vali/valq

	return
	end


c	subroutine get_spec(kburst,buff1,ns,np,np_v,buff_spec)
c
c	implicit none
c
c	include 'ssp2_const.inc'
cc	----------------
cc	INPUT PARAMETERS
cc	----------------
c	complex buff1(sam_raw,line_raw)
c	integer*4	n_fft
c	integer*4	nvals
c	integer*4	np(burst_max)
c	integer*4	ns(burst_max)
c	integer*4       np_v(burst_max)
c	integer*4       kburst
c
cc	----------------
cc	OUTPUT PARAMETERS
cc	----------------
c	complex  	buff_spec(max_fft_size)
c
cc	----------------
cc	LOCAL VARIABLES
cc	----------------
c	complex		buff_t(max_fft_size)
c	integer*4	i
c	integer*4	np_index
c
c        n_fft=4096
c	call init_fft(n_fft)
c
cc	CLEAR BUFFERS
c
c        do i = 1, n_fft
c           buff_t(i) = cmplx(0., 0.)
c        enddo
c
cc	TAKE CFFT & ACCUMULATION THEM
c
c        do np_index =(np(kburst)-np_v(kburst)+1),np(kburst)
c           nvals=ns(kburst)
c           if (nvals.gt.n_fft) nvals=n_fft
c           do i = 1, nvals
c              buff_t(i) = buff1(i, np_index)
c           enddo
c           call cfft(buff_t,1,n_fft,1)
c           do i = 1, n_fft
c              buff_spec(i) = (buff_t(i)/float(n_fft))+buff_spec(i)
c           enddo
c        enddo
c
c
c	return
c	end
c
c
c        subroutine init_spec(buff_spec)
c        implicit none
c        include 'ssp2_const.inc'
c        complex buff_spec(max_fft_size)
c        integer i
c
c        do 100 i=1,max_fft_size
c        buff_spec(i)=cmplx(0.0,0.0)
c  100   continue
c
c        return
c        end


      subroutine ld_r_spec(nburst,buff,fs)
	implicit none
        complex buff(8192)
        integer nburst,nvals,i,n_fft,i_low_pwr,i_high_pwr
        real*8 fs
        real*4 low_pwr,high_pwr
        include 'ceos.inc'

        write (*,*) 'JEFFS nburst = ',nburst

        n_fft=4096
        nvals=128
        rsp_n_bins=nvals
        do 100 i=1,nvals
        rsp_data_values(i)=cabs(buff(i))/float(nburst)
  100   continue

        first_freq=fs/dble(n_fft)
        last_freq=float(nvals-1)*first_freq
        high_pwr=0.0
        low_pwr=999999999.0
        do 200 i=1,nvals
        if (rsp_data_values(i).gt.high_pwr) then
          high_pwr=rsp_data_values(i)
          i_high_pwr=i
        endif
        if (rsp_data_values(i).lt.low_pwr) then
          low_pwr=rsp_data_values(i)
          i_low_pwr=i
        endif
  200   continue
        min_power=i_low_pwr
        max_power=i_high_pwr

	return
	end


        subroutine pmfname(file_ceos_data,pmf_file)
        implicit none
        character*60 file_ceos_data,pmf_file
        character*60 dummy
        character*1 dummy_buf(60),c
        integer i,j
        equivalence(dummy,dummy_buf(1))

        dummy=file_ceos_data
        do 100 i=1,60
        j=61-i
        c=dummy_buf(j)
        if (c.eq."/") go to 110
  100   continue
  110   continue

        dummy_buf(j+18)='p'
        dummy_buf(j+19)='m'
        dummy_buf(j+20)='f'
        pmf_file=dummy
        return
        end




c  Assemble GMT as a character string per CEOS format, from
c  input year, day of year, hours, minutes, and seconds.

      subroutine gmt_str(year,doy,hr,min,sec,usec,gmtstr)
      implicit none
      integer i
      real*8 fsec
      integer year,doy,hr,sec,usec,yr,min
      character*64 gmtstr,gmts1
      character*1 gmtbuf(64)
      equivalence(gmts1,gmtbuf(1))

      do 10 i=1,64
      gmtbuf(i)=' '
   10 continue
      yr=year
      fsec=dble(sec)+dble(usec)/1000000.0d0
c      encode(21,1000,gmts1) yr,doy,hr,min,fsec
      write(unit=gmts1,fmt=1000) yr,doy,hr,min,fsec
      do 20 i=1,21
      if(gmtbuf(i).eq.' ') gmtbuf(i)='0'
   20 continue
      gmtstr=gmts1
      return
 1000 format (i4,'-',i3,'T',i2,':',i2,':',f6.3)
      end

c  Assemble scene center time as a character string per CEOS format, 
c  from input year, month, day of month, hours, minutes, and seconds.

      subroutine sctim_str(year,mon,dom,hr,min,sec,usec,gmtstr)
      implicit none
      integer i
      real*8 fsec
      integer year,mon,dom,hr,sec,usec,yr,min,msec
      character*64 gmtstr,gmts1
      character*1 gmtbuf(64)
      equivalence(gmts1,gmtbuf(1))

      do 10 i=1,64
      gmtbuf(i)=' '
   10 continue
      yr=year
      fsec=dble(usec)/1000.0d0
      msec=int(fsec)
      if (msec.gt.999) msec=999
c      encode(21,1000,gmts1) yr,doy,hr,min,sec,fsec
      write(unit=gmts1,fmt=1000) yr,mon,dom,hr,min,sec,msec
      do 20 i=1,17
      if(gmtbuf(i).eq.' ') gmtbuf(i)='0'
   20 continue
      gmtstr=gmts1
      return
 1000 format (i4,i2,i2,i2,i2,i2,i3)
      end

