*****************************************************************

      Program dgx
      
c****************************************************************
c**   
c**   FILE NAME: dgx.f
c**
c**   PROGRAM NAME: dgx
c**
c**   VERSION: 61.0
c**   
c**   DATE WRITTEN: 2000.05.11
c**   
c**   PROGRAMMER:   Scott Shaffer
c**                 Jet Propulsion Lab
c**   
c**   DESCRIPTION:  This program uses my graphx routines to display images. 
c**                      It can read many image formats including RMG, 
c**                      complex, real*4, byte, integer*2, and integer*4.
c**   
c**   ROUTINES CALLED:   init_gx
c**                      get_wininfo
c**                      getevent
c**                      setcolor
c**                      display_label
c**                      display_rmg
c**                      getarg
c**                      plus lot more
c**   
c**   NOTES:        Tons of Fun
c**
c**   
c**   UPDATE LOG:
c*****************************************************************
      
      implicit none

        integer i_wsamps,I_BMAX
        integer i_maxsamps,i_maxlines
#ifdef M4
        parameter(i_wsamps=4000000)
#else
        parameter(i_wsamps= 100000)
#endif
        parameter(i_maxsamps=100000)
        parameter(i_maxlines=100000)
        parameter(I_BMAX=100)
      
c  INPUT VARIABLES:

        character*80 a_value
        character*80 a_labl(0:20)
        character*40 a_msg
        character*40 a_rsp
        character*120 a_rmgfile
        character*120 a_magfile
        character*120 a_dtefile
        character*120 a_cmpfile
        character*120 a_filename
        character*120 a_cmdfile
        character*120 a_prtcmnd
        character*120 a_pltcmnd
        character*120 a_jpgcmnd
        character*255 a_string

        character*120 a_hdrfile

        character*120 a_type
        character*4 a_fmt(0:8)

        integer i_lsize
        integer i_ssize
        real*8 r_peg(3)
        real*8 r_str(2)
        real*8 r_spc(2)
        real*8 v_loc1(3)
        real*8 v_loc2(3)
        real*8 v_loc3(3)
        real*8 r_atm(3,4)
        real*8 r_mta(3,4)
        real*8 r_lat
        real*8 r_lon
        real*8 r_hhh
        real*8 r_a
        real*8 r_e2
        real*8 r_rad
        real*8 r_pi
        real*8 r_rtod

        character*1 a_grid              ! UTM identifier
        integer*4   i_zone

        integer i_rmgfile
        integer i_magfile
        integer i_dtefile
        integer i_cmpfile

        integer i_magtype
        integer i_dtetype
        integer i_cmptype

        integer i,j
        integer i_bcnt
        integer i_samps
        integer i_lines
        integer i_inarg
        integer i_comma
        integer i_brack


        integer il
        integer ix
        integer iy
        integer ixx
        integer ixxx
        integer iyy
        integer iyyy
        integer i_strt
        integer i_stop
        integer i_avg

        real r_sav(i_wsamps)
        real r_dat1(i_wsamps)      
        real r_dat2(i_wsamps)
        real r_amp1(i_maxsamps)
        real r_hgt1(i_maxsamps)
        real r_apath(i_maxsamps)
        real r_hpath(i_maxsamps)
        integer i_out(i_maxsamps)
        integer i_xpath(i_maxsamps)
        integer i_ypath(i_maxsamps)
        real r_amp
        real r_hgt

        integer*4 i_type(0:20)
        integer*4 i_frx(0:20)
        integer*4 i_fry(0:20)

        integer i_widget
        integer i_err
        integer i_pos
        integer i_lks
        integer i_fline
        integer i_nline
        integer i_mlines
        integer i_dlines
        integer i_fpline
        integer i_npline
        integer i_fpsamp
        integer i_npsamp
        integer i_skip
        integer i_dpi
        integer i_mag
        integer i_col
        integer i_orient
        integer i_xmax
        integer i_ymax

        integer i_ctable(3,0:255)

        integer i_colnum
        integer i_colors(0:255,3)

        real r_aspect

        real*8 r_sum
        real*8 r_sqr

        real r_amin
        real r_amax
        real r_aavg
        real r_astd
        real r_hmin
        real r_hmax

        real r_avg
        real r_std

        real r_avge
        real r_stde

        real r_hscale
        real r_ascale
        real r_expnt
        real r_wstrt

        real r_xlast
        real r_x0
        real r_y0
        real r_xx
        real r_yy
        real r_ddd
        real r_ccc
        real r_dist
        real r_xs
        real r_ys
        real r_curve

        integer i_gxi			! Number of windows
        integer i_wxs(0:20)		! Size of window canvas in x direction
        integer i_wys(0:20)		! Size of window canvas in y direction

        integer i_flg
        integer i_flgtgl
        integer i_flip
        integer i_colr
        integer i_lock

        integer i_loop
        integer i_done			! Loop flag

        integer i_arg

        integer i_value
        integer i_default
        integer i_path
        integer i_epath
        integer i_eline
        integer i_x1
        integer i_x2
        integer i_y1
        integer i_y2
        integer i_case

        real r_value

        real r_mmul
        real r_dmul
        real r_madd
        real r_dadd

        integer i_dlsize
        integer i_dssize
        integer i_dbytes
        integer i_doff

        integer i_mlsize
        integer i_mssize
        integer i_mbytes
        integer i_moff

        integer i_ccc
        integer i_zmw
        integer i_zmcnt
        integer i_scale

        real*4 r_bias

C 	Display variables

        integer i_event(10)
        integer i_bdat(10,I_BMAX)

        integer i_cnt
        integer i_clrs

        integer i_ch
        integer i_cw
        integer i_vh
        integer i_vw
        integer i_vx
        integer i_vy

        integer i_vxs(20)
        integer i_vys(20)
        integer i_vxo(20)
        integer i_vyo(20)

        integer i_key
        integer i_asc
        integer i_button
        integer i_wb
        integer i_wait

        integer i_tgl
        integer i_tgk
        integer i_clk
        integer i_mem
        integer i_bline
        integer i_blks
        integer ib

        integer i_debug
        
        real r_zoom
        real r_zmstrt

        save r_dat1,r_dat2,r_amp1,r_hgt1,r_sav

        character*200 a_out
        byte b_out(3*i_maxsamps)
        equivalence(a_out,b_out)

#ifdef IO64
        integer*8 i_fsize
#else
        integer*4 i_fsize
#endif

        integer*4 getfsize
        external getfsize

c  FUNCTIONS

      integer iargc
      external iargc

      integer length
      external length

      integer initdk
      external initdk

      real*8 rdir
      external rdir

c  PROCESSING STEPS:

      write(6,*) '  '
      write(6,*) '   << DGX Version 61.0-Multvis   12-MAY-2000 >>   '
      write(6,*) '  '

c
c  Initialize display stuff
c
      i_debug = 2
      r_zoom = 0.
      r_zmstrt = 0.25
      r_hscale = 100.
      r_ascale = 1.
      r_expnt = 1.
      r_wstrt = 0.
      r_bias = 0.5
      r_hmin = -1.e20
      r_hmax =  1.e20
      r_curve = 0.0
      i_scale = 1
      i_samps = 0
      i_flg = 0
      i_flgtgl = 2
      i_zmcnt = 0
      i_zmw = 7
      i_dpi = 300
      i_flip = 0
      i_clrs = 0
      i_key = 0
      i_asc = 0
      i_button=0
      i_wb=0
      i_clk = 3
      i_tgl = 0
      i_tgk = 32
      i_mem = 0
      i_lks = 0
      i_ccc = 0
      ixxx=-1
      iyyy=-1
      i_fline = 1
      i_nline = i_maxlines
      i_mlines = i_maxlines
      i_dlines = i_maxlines
      a_rmgfile=' '
      a_magfile = ' '
      a_dtefile = ' '
      a_cmpfile = ' '
      a_hdrfile = ' '
      a_filename = ' '
      a_type='sch'
      i_magtype = 0
      i_dtetype = 0
      i_cmptype = 0
      i_colr = 0
      i_lock = 0
      i_dbytes=4
      i_mbytes=4
      i_mssize=0
      i_dssize=0
      i_mlsize=0
      i_dlsize=0
      i_lsize=0
      i_ssize=0
      i_zone=0
      i_path=0
      i_epath=0
      i_eline=0
      i_avg=0

      a_cmdfile = ' '
      a_prtcmnd = 'lpr '
      a_pltcmnd = 'xmgr -xy '
      a_jpgcmnd = 'cjpeg -quality 95 -outfile out.jpg '

      a_fmt(0) = ' -  '
      a_fmt(1) = ' -b1'
      a_fmt(2) = ' -i2'
      a_fmt(3) = ' -i4'
      a_fmt(4) = ' -r4'
      a_fmt(5) = ' -bs'
      a_fmt(6) = '-slx'
      a_fmt(7) = '    '
      a_fmt(8) = '    '

c
c  Initialize datum stuff
c
        r_a =  6378137.0
        r_e2 = 0.00669438
c
c  Initialize pi and conversions
c
        r_pi = 4.d0*atan(1.0d0)
        r_rtod = 180.0d0/r_pi

        r_lat = -r_pi
        r_lon = -r_pi

        r_mmul = 1.0
        r_madd = 0.0

        r_dmul = 1.0
        r_dadd = 0.0

        i_doff = 0
        i_moff = 0

        r_spc(1) = 0.0
        r_spc(2) = 0.0

        r_amin = 0.0
        r_amax = 0.0
        r_aavg = 0.0
        r_astd = 0.0

        call defcolors(i_colnum,i_colors)

      i_inarg = iargc()
      if(i_inarg .eq. 0)then
         call write_greeting()
         stop 'done'
      else
        i_arg=0
        i_loop = 1
        i_value = 0
        i_default = 0
        do while(i_arg .lt. i_inarg .and. i_loop .eq. 1)
          i_arg=i_arg + 1
          call getarg(i_arg,a_value)
          if (a_value .eq. ' ') then
            ! error
          else if (a_value .eq. '-debug') then
            i_arg=i_arg+1
            call getarg(i_arg,a_value)
            read(a_value,*) i_debug
          else if (a_value .eq. '-D' .or. a_value .eq. '-DEBUG') then
            i_debug = 2
          else if (a_value .eq. '-s' .or. a_value .eq. '-samples') then
            i_arg=i_arg+1
            call getarg(i_arg,a_value)
            read(a_value,*) i_samps
          else if (a_value .eq. '-d' .or. a_value .eq. '-dte_scale') then
            i_arg=i_arg+1
            call getarg(i_arg,a_value)
            read(a_value,*) r_hscale
          else if (a_value .eq. '-m' .or. a_value .eq. '-mag_scale') then
            i_arg=i_arg+1
            call getarg(i_arg,a_value)
            read(a_value,*) r_ascale
          else if (a_value .eq. '-amin' .or. a_value .eq. '-mag_min') then
            i_arg=i_arg+1
            call getarg(i_arg,a_value)
            read(a_value,*) r_amin
          else if (a_value .eq. '-amax' .or. a_value .eq. '-mag_max') then
            i_arg=i_arg+1
            call getarg(i_arg,a_value)
            read(a_value,*) r_amax
          else if (a_value .eq. '-aavg' .or. a_value .eq. '-mag_avg') then
            i_arg=i_arg+1
            call getarg(i_arg,a_value)
            read(a_value,*) r_aavg
          else if (a_value .eq. '-astd' .or. a_value .eq. '-mag_std') then
            i_arg=i_arg+1
            call getarg(i_arg,a_value)
            read(a_value,*) r_astd
          else if (a_value .eq. '-e' .or. a_value .eq. '-expnt') then
            i_arg=i_arg+1
            call getarg(i_arg,a_value)
            read(a_value,*) r_expnt
          else if (a_value .eq. '-w' .or. a_value .eq. '-ws') then
            i_arg=i_arg+1
            call getarg(i_arg,a_value)
            read(a_value,*) r_wstrt
          else if (a_value .eq. '-c' .or. a_value .eq. '-color') then
            i_arg=i_arg+1
            call getarg(i_arg,a_value)
            read(a_value,*) i_clrs
          else if (a_value .eq. '-cmap' .or. a_value .eq. '-ctable') then
            i_arg=i_arg+1
            call getarg(i_arg,a_value)
            call getcolors(a_value,i_colnum,i_colors)
          else if (a_value .eq. '-bmap' .or. a_value .eq. '-btable') then
            i_arg=i_arg+1
            call getarg(i_arg,a_value)
            call getbcolors(a_value,i_colnum,i_colors)
          else if (a_value .eq. '-h' .or. a_value .eq. '-hdr') then
            i_arg=i_arg+1
            call getarg(i_arg,a_hdrfile)
          else if (a_value .eq. '-g' .or. a_value .eq. '-cmd') then
            i_arg=i_arg+1
            call getarg(i_arg,a_cmdfile)
          else if (a_value .eq. '-p' .or. a_value .eq. '-prt') then
            i_arg=i_arg+1
            call getarg(i_arg,a_prtcmnd)
          else if (a_value .eq. '-o' .or. a_value .eq. '-plt') then
            i_arg=i_arg+1
            call getarg(i_arg,a_pltcmnd)
          else if (a_value .eq. '-j' .or. a_value .eq. '-jpg') then
            i_arg=i_arg+1
            call getarg(i_arg,a_jpgcmnd)
          else if (a_value .eq. '-f' .or. a_value .eq. '-looks') then
            i_arg=i_arg+1
            call getarg(i_arg,a_value)
            read(a_value,*) i_lks
          else if (a_value .eq. '-l' .or. a_value .eq. '-sl') then
            i_arg=i_arg+1
            call getarg(i_arg,a_value)
            read(a_value,*) i_fline
          else if (a_value .eq. '-n' .or. a_value .eq. '-nl') then
            i_arg=i_arg+1
            call getarg(i_arg,a_value)
            read(a_value,*) i_nline
          else if (a_value .eq. '-x' .or. a_value .eq. '-xc') then
            i_arg=i_arg+1
            call getarg(i_arg,a_value)
            read(a_value,*) ixxx
          else if (a_value .eq. '-y' .or. a_value .eq. '-yc') then
            i_arg=i_arg+1
            call getarg(i_arg,a_value)
            read(a_value,*) iyyy
          else if (a_value .eq. '-lat' .or. a_value .eq. '-yl') then
            i_arg=i_arg+1
            call getarg(i_arg,a_value)
            read(a_value,*) r_lat
            r_lat = r_lat/r_rtod
          else if (a_value .eq. '-lon' .or. a_value .eq. '-xl') then
            i_arg=i_arg+1
            call getarg(i_arg,a_value)
            read(a_value,*) r_lon
            r_lon = r_lon/r_rtod
          else if (a_value .eq. '-rc') then
            i_arg=i_arg+1
            call getarg(i_arg,a_value)
            read(a_value,*) r_curve
          else if (a_value .eq. '-avg') then
            i_arg=i_arg+1
            call getarg(i_arg,a_value)
            read(a_value,*) i_avg
          else if (a_value .eq. '-hmin') then
            i_arg=i_arg+1
            call getarg(i_arg,a_value)
            read(a_value,*) r_hmin
          else if (a_value .eq. '-hmax') then
            i_arg=i_arg+1
            call getarg(i_arg,a_value)
            read(a_value,*) r_hmax
          else if (a_value .eq. '-dm') then
            i_arg=i_arg+1
            call getarg(i_arg,a_value)
            read(a_value,*) r_dmul
          else if (a_value .eq. '-da') then
            i_arg=i_arg+1
            call getarg(i_arg,a_value)
            read(a_value,*) r_dadd
          else if (a_value .eq. '-mm') then
            i_arg=i_arg+1
            call getarg(i_arg,a_value)
            read(a_value,*) r_mmul
          else if (a_value .eq. '-ma') then
            i_arg=i_arg+1
            call getarg(i_arg,a_value)
            read(a_value,*) r_madd
          else if (a_value .eq. '-z' .or. a_value .eq. '-zms') then
            i_arg=i_arg+1
            call getarg(i_arg,a_value)
            read(a_value,*) r_zmstrt
            if (r_zmstrt .lt. 0) r_zmstrt = -1./r_zmstrt
            r_zmstrt=2.**(nint(log(r_zmstrt)/log(2.)))
          else if (a_value .eq. '-dpi' ) then
            i_arg=i_arg+1
            call getarg(i_arg,a_value)
            read(a_value,*) i_dpi
            if (i_dpi .le. 0) i_dpi = 300
          else if (a_value .eq. '-t' .or. a_value .eq. '-tgk') then
            i_arg=i_arg+1
            call getarg(i_arg,a_value)
            call gettgk(a_value,i_tgk,i_tgl,i_clk)
          else if (a_value .eq. '-F' .or. a_value .eq. '-flip') then
            i_arg=i_arg+0
            i_flip = 1
          else if (a_value .eq. '-S1' .or. a_value .eq. '-DX1' .or. a_value .eq. '-slx1') then
            i_flg = 1
          else if (a_value .eq. '-S' .or. a_value .eq. '-X'  .or. a_value .eq. '-slx' .or.
     &             a_value .eq. '-S2'.or. a_value .eq. '-DX2'.or. a_value .eq. '-slx2') then
            if (a_magfile .eq. ' ') then
              a_magfile = 'slx2'
              i_magtype = 9
            else if (a_magfile .ne. ' ' .and. a_dtefile .eq. ' ') then
              a_dtefile = a_magfile
              i_dtetype = i_magtype
              i_dbytes = i_mbytes
              a_magfile = 'slx2'
              i_magtype = 9
            end if
            i_flg = 2
            i_flgtgl = 2
          else if (a_value .eq. '-T' .or. a_value .eq. '-tgl') then
            i_arg=i_arg+0
            i_tgl = 1
          else if (a_value .eq. '-P' .or. a_value .eq. '-par') then
            i_arg=i_arg+0
            i_mem = 1
          else if (a_value .eq. '-D' .or. a_value .eq. '-dyn') then
            i_arg=i_arg+0
            i_colr = 1
            i_lock = 1
          else if (a_value .eq. '-M' .or. a_value .eq. '-mix') then
            i_arg=i_arg+0
            i_colr = 0
            i_lock = 1
          else if (a_value .eq. '-Z' .or. a_value .eq. '-zzz') then
            i_arg=i_arg+0
            r_zmstrt = .25
          else if (a_value .eq. '-C' .or. a_value .eq. '-cw') then
            i_arg=i_arg+0
            i_scale = 0
          else if (a_value .eq. '-rmg') then
            i_arg=i_arg+1
            call getarg(i_arg,a_rmgfile)
            i_magtype = 0
            i_dtetype = 0
          else if (a_value .eq. '-c8' .or. a_value .eq. '-cmp') then
            i_arg=i_arg+1
            call getarg(i_arg,a_cmpfile)
            i_magtype = 0
            i_dtetype = 0
            i_cmptype = 1
            r_hscale=360.
          else if (a_value .eq. '-c4' .or. a_value .eq. '-ers') then
            i_arg=i_arg+1
            call getarg(i_arg,a_cmpfile)
            i_magtype = 0
            i_dtetype = 0
            i_cmptype = 2
            r_hscale=360.
          else if (a_value .eq. '-b' .or. a_value .eq. '-i1' .or. a_value .eq. '-i*1'  .or. a_value .eq. '-byt') then
            i_arg=i_arg+1
            if (a_magfile .eq. ' ') then
              call getarg(i_arg,a_magfile)
              i_magtype = 1
              i_mbytes = 1
            else if (a_dtefile .eq. ' ') then
              call getarg(i_arg,a_dtefile)
              i_dtetype = 1
              i_dbytes = 1
            end if
          else if (a_value .eq. '-ii' .or. a_value .eq. '-i2' .or. a_value .eq. '-i*2') then
            i_arg=i_arg+1
            if (a_magfile .eq. ' ') then
              call getarg(i_arg,a_magfile)
              i_magtype = 2
              i_mbytes = 2
            else if (a_dtefile .eq. ' ') then
              call getarg(i_arg,a_dtefile)
              i_dtetype = 2
              i_dbytes = 2
            end if
          else if (a_value .eq. '-bs' .or. a_value .eq. '-calgis') then
            i_arg=i_arg+1
            if (a_magfile .eq. ' ') then
              call getarg(i_arg,a_magfile)
              i_magtype = 5
              i_mbytes = 2
            else if (a_dtefile .eq. ' ') then
              call getarg(i_arg,a_dtefile)
              i_dtetype = 5
              i_dbytes = 2
            end if
          else if (a_value .eq. '-si2' .or. a_value .eq. '-signedi2') then
            i_arg=i_arg+1
            if (a_magfile .eq. ' ') then
              call getarg(i_arg,a_magfile)
              i_magtype = 6
              i_mbytes = 2
            else if (a_dtefile .eq. ' ') then
              call getarg(i_arg,a_dtefile)
              i_dtetype = 6
              i_dbytes = 2
            end if
          else if (a_value .eq. '-i' .or. a_value .eq. '-i4' .or. a_value .eq. '-i*4') then
            i_arg=i_arg+1
            if (a_magfile .eq. ' ') then
              call getarg(i_arg,a_magfile)
              i_magtype = 3
            else if (a_dtefile .eq. ' ') then
              call getarg(i_arg,a_dtefile)
              i_dtetype = 3
            end if
          else if (a_value .eq. '-r' .or. a_value .eq. '-r4' .or. a_value .eq. '-r*4') then
            i_arg=i_arg+1
            if (a_magfile .eq. ' ') then
              call getarg(i_arg,a_magfile)
              i_magtype = 4
            else if (a_dtefile .eq. ' ') then
              call getarg(i_arg,a_dtefile)
              i_dtetype = 4
            end if
c          else if (a_value .eq. '-a'  .or. a_value .eq. '-asc' .or. 
c     &             a_value .eq. '-ai' .or. a_value .eq. '-ar'  ) then
c            i_arg=i_arg+1
c            if (a_magfile .eq. ' ') then
c              call getarg(i_arg,a_magfile)
c              i_magtype = 7
c            else if (a_dtefile .eq. ' ') then
c              call getarg(i_arg,a_dtefile)
c              i_dtetype = 7
c            end if
          else if (a_value .eq. '-mag') then
            i_arg=i_arg+1
            call getarg(i_arg,a_magfile)
            i_magtype = 4
          else if (a_value .eq. '-dte') then
            i_arg=i_arg+1
            call getarg(i_arg,a_dtefile)
            i_dtetype = 4
          else if (a_value(1:1) .eq. '-' .and. length(a_value) .gt. 1) then  ! ignore flags that are not recognized
            i_case = 0
            do i=2,length(a_value)
              if (ichar(a_value(i:i)) .ge. 65 .and. ichar(a_value(i:i)) .le. 65+26) i_case = 1
            end do
            if (i_case .eq. 0) i_arg=i_arg+1         ! if flags have no uppercase letters, then they have an argument to skip            
          else 
            r_value = 0.
            read(a_value,*,iostat=i_err) r_value
            do i=1,length(a_value)
              if ((ichar(a_value(i:i)) .ge. 43 .and. ichar(a_value(i:i)) .le. 57) .or.  
     &            (ichar(a_value(i:i)) .eq. 69 .or.  ichar(a_value(i:i)) .eq. 101) ) then
                ! do nothing
              else
                i_err = 1
              end if
            end do
            if (i_err .eq. 0 .and. r_value .ne. 0.) then
              i_value = i_value + 1
              if (i_value .eq. 1) then
                read(a_value,*) i_samps
              else if (i_value .eq. 2) then
                read(a_value,*) r_hscale
              else if (i_value .eq. 3) then
                read(a_value,*) r_ascale
              else if (i_value .eq. 4) then
                read(a_value,*) r_expnt
              else if (i_value .eq. 5) then
                read(a_value,*) i_flip
              else if (i_value .eq. 6) then
                read(a_value,*) i_clrs
              else
                if (i_debug .ge. 1) write(6,*) '*** Warning - To many params.   ',a_value
              end if
            else if (a_value .eq. '-' .and. a_magfile .ne. ' ') then
              i_value = i_value + 1
            else ! must be a file in r4 format
              i_default = i_default + 1
              if (a_magfile .eq. ' ') then
                a_magfile=a_value
                i_magtype = 4
              else if (a_dtefile .eq. ' ') then
                a_dtefile=a_value
                i_dtetype = 4
              end if
            end if
          end if        
                
        end do

        i_comma=index(a_magfile,',')
        i_brack=index(a_magfile,']')
        if (i_brack .gt. 1) then
          a_cmpfile = a_magfile(1:i_brack-1)
          a_magfile = ' '
          i_magtype = 0
          i_dtetype = 0
          i_cmptype = 1
          r_hscale=360.
        else if (i_comma .ne. 0) then
          i_magtype = 4
          a_value = a_magfile
          if (i_comma .gt. 1) then
            a_magfile = a_value(1:i_comma-1)
          else
            a_magfile = ' '
          end if
          if (i_comma .lt. length(a_value)) then
            a_dtefile = a_value(i_comma+1:)
            i_dtetype = 4
          end if
        else
          if (a_magfile .ne. ' ' .and. a_dtefile .eq. ' ' .and. 
     &        i_default .eq. 1) then                             ! may need some adjustment
            a_rmgfile = a_magfile
            a_magfile = ' '
            i_magtype = 0
            i_dtetype = 0
          end if
        end if

        if (a_magfile .eq. '-') then
          a_magfile=' '
          i_magtype=0
        end if
        if (a_dtefile .eq. '-') then
          a_dtefile=' '
          i_dtetype=0
        end if
        if (a_magfile .eq. 'slx') then
          a_magfile=' '
          i_magtype=9
          i_flg=2
          i_flgtgl = 2
        end if
        if (a_magfile .eq. 'slx2') then
          a_magfile=' '
          i_magtype=9
          i_flg=2
          i_flgtgl = 2
        end if

      end if


      call read_cmd(a_cmdfile,a_prtcmnd,a_pltcmnd,a_jpgcmnd)

      if (i_scale .eq. 0) then
        r_ascale = 1./r_ascale
      else
        r_ascale = 2*r_ascale
      end if
c     open image file

      if (a_rmgfile .ne. ' ') then
          open( 21,file=a_rmgfile,form='unformatted',recl=1,
     &       access='direct',status='old',readonly)
          close(21)
          if (i_debug .ge. 1) write(6,*) 'Opening: ',a_rmgfile(1:60)
          i_rmgfile = initdk(21,a_rmgfile)
      else if (a_cmpfile .ne. ' ') then
          open( 21,file=a_cmpfile,form='unformatted',recl=1,
     &       access='direct',status='old',readonly)
          close(21)
          if (i_debug .ge. 1) write(6,*) 'Opening Complex file: ',a_cmpfile(1:60)
          i_cmpfile = initdk(21,a_cmpfile)
      else
        if (a_magfile .ne. ' ') then
          open( 22,file=a_magfile,form='unformatted',recl=1,
     &      access='direct',status='old',readonly)
          close(22)
          if (i_debug .ge. 1) write(6,*) 'Opening mag file: ',a_magfile(1:60)
          i_magfile = initdk(22,a_magfile)
        end if
        if (a_dtefile .ne. ' ') then
          open( 23,file=a_dtefile,form='unformatted',recl=1,
     &      access='direct',status='old',readonly)
          close(23)
          if (i_debug .ge. 1) write(6,*) 'Opening dte file: ',a_dtefile(1:60)
          i_dtefile = initdk(23,a_dtefile)
        end if

      end if


c  Open header file if specified
   
        if (a_magfile .ne. ' ')  then
          call read_mhdr(i_magfile,i_mlsize,i_mssize,i_mbytes,i_moff)
          if (i_mssize .gt. 0) then
            if (i_mbytes .eq. 1) then
              i_magtype = 1
            else if (i_mbytes .eq. 2) then
              i_magtype = 6
            else if (i_mbytes .eq. 4) then
              i_magtype = 4
            else
              stop 'mag file type not recognized from airsar hdr'
            end if
          end if
        end if

        if (a_rmgfile .ne. ' ')  then
          call read_mhdr(i_rmgfile,i_mlsize,i_mssize,i_mbytes,i_moff)
          if (i_mssize .gt. 0) then
            i_magfile=i_rmgfile
            a_magfile=a_rmgfile
            i_rmgfile=0
            a_rmgfile=' '
            if (i_mbytes .eq. 1) then
              i_magtype = 1
            else if (i_mbytes .eq. 2) then
              i_magtype = 6
            else if (i_mbytes .eq. 4) then
              i_magtype = 4
            else
              stop 'mag file type not recognized from airsar hdr'
            end if
          end if
        end if

        if (a_dtefile .ne. ' ') then
          call read_dhdr(i_dtefile,i_dlsize,i_dssize,i_dbytes,i_doff,r_dmul,r_dadd,
     &           r_peg,r_str,r_spc)
          if (i_dssize .gt. 0) then
            if (i_dbytes .eq. 1) then
              i_dtetype = 1
            else if (i_dbytes .eq. 2) then
              i_dtetype = 6
            else if (i_dbytes .eq. 4) then
              i_dtetype = 4
            else
              stop 'dte file type not recognized from airsar hdr'
            end if
          end if
        end if
        

        if (i_mlsize .gt. 0 .and. i_dlsize .gt. 0 .and. i_mlsize .ne. i_dlsize)
     &             stop '*** Error - mag and dte file length do not agree'
        if (i_mssize .gt. 0 .and. i_dssize .gt. 0 .and. i_mssize .ne. i_dssize)
     &             stop '*** Error - mag and dte file width do not agree'
        if (i_mssize .gt. 0) i_ssize = i_mssize
        if (i_dssize .gt. 0) i_ssize = i_dssize
        if (i_mlsize .gt. 0) i_lsize = i_mlsize
        if (i_dlsize .gt. 0) i_lsize = i_dlsize

        if (i_samps .gt. 0 .and. i_ssize .gt. 0 .and. i_samps .ne. i_ssize) then
          write(6,*) '*** Error - File size in hdr is not equal to size on cmnd line.  ',
     &        i_samps,i_ssize
          stop ' '
        end if

        if (a_hdrfile .eq. ' ' .and. i_mssize .eq. 0 .and. i_dssize .eq. 0) then

          if (a_rmgfile .ne. ' ') a_hdrfile=a_rmgfile
          if (a_dtefile .ne. ' ') a_hdrfile=a_dtefile
          if (a_magfile .ne. ' ') a_hdrfile=a_magfile
          if (a_hdrfile .ne. ' ') then
            i_pos = 0
            do while (index(a_hdrfile(i_pos+1:),'.') .ne. 0)
              i_pos = i_pos + index(a_hdrfile(i_pos+1:),'.')
            end do
            if (i_pos .gt. 0) then
              a_hdrfile=a_hdrfile(1:i_pos)//'hdr'
            else
              a_hdrfile=a_hdrfile(:length(a_hdrfile))//'.hdr'
            end if
          end if
        end if
        if (a_hdrfile .ne. ' ') then
          call read_hdr(a_hdrfile,i_lsize,i_ssize,r_peg,a_type,
     &              r_str,r_spc,i_mbytes,i_dbytes,r_mmul,r_madd,
     &              r_dmul,r_dadd,i_err)

          if (i_ssize .gt. 0) then
            if (a_dtefile .ne. ' ') then
              if (i_dbytes .eq. 1) then
                i_dtetype = 1
              else if (i_dbytes .eq. 2) then
                i_dtetype = 2
              else if (i_dbytes .eq. 4) then
                i_dtetype = 4
              else
                stop 'dte file type not recognized from airsar hdr'
              end if
            end if

            if (a_magfile .ne. ' ') then
              if (i_mbytes .eq. 1) then
                i_magtype = 1
              else if (i_mbytes .eq. 2) then
                i_magtype = 2
              else if (i_mbytes .eq. 4) then
                i_magtype = 4
              else
                stop 'mag file type not recognized from airsar hdr'
              end if
            end if
          end if
          if (i_err .eq. 0 .and. i_debug .ge. 2) 
     &              write(6,*) 'Reading Header File: ',a_hdrfile(:length(a_hdrfile))
        end if
        
        if (r_spc(1) .ne. 0.) then
            r_rad = rdir(r_a,r_e2,r_peg(3),r_peg(1))
            if (a_type .eq. 'utm' .or. a_type .eq. 'UTM' .or.
     &          a_type .eq. 'neu' .or. a_type .eq. 'NEU' .or.
     &          a_type .eq. 'enu' .or. a_type .eq. 'ENU' ) then
              if (i_zone .eq. 0) then
                call utmtoll(r_a,r_e2,i_zone,a_grid,v_loc1,r_peg(1),r_peg(2),2)
              end if
            end if
            call tcnatm(r_a,r_e2,r_peg,r_atm)
            call invrstrn(r_atm,r_mta)
            if (i_lks .eq. 0 ) then
              if (i_ssize .ne. 0 .and. i_samps .ne. 0) then
                i_lks = nint(float(i_ssize)/i_samps)
              else
                i_lks = 1
              end if
            end if
            i_lsize = i_lsize/i_lks
            i_ssize = i_ssize/i_lks
            r_spc(1) = r_spc(1)*i_lks
            r_spc(2) = r_spc(2)*i_lks
            if (i_debug .ge. 3) then
              write(6,*) ' '
              write(6,*) '  siz = ',i_lsize,i_ssize
              write(6,*) '  peg = ',r_peg(1)*r_rtod,r_peg(2)*r_rtod,r_peg(3)*r_rtod
              write(6,*) '  str = ',r_str
              write(6,*) '  spc = ',r_spc
              write(6,*) '  rad = ',r_rad
              write(6,*) '  atm = ',r_atm
              write(6,*) '  zon = ',i_zone
              if (r_dmul .ne. 1. .or. r_dadd .ne. 0. .or.
     &            r_mmul .ne. 1. .or. r_madd .ne. 0. ) then
                write(6,*) '  mul = ',r_mmul,r_dmul
                write(6,*) '  add = ',r_madd,r_dadd
              end if
            end if
            if (a_type .eq. 'utm' .or. a_type .eq. 'UTM') r_spc(1) = -r_spc(1)
        end if


      if (i_samps .eq. 0 .and. i_ssize .gt. 0) i_samps = i_ssize
      if (i_samps .eq. 0) stop 'Error - Must specify image size in pixels'
      if (i_nline .eq. i_maxlines .and. i_lsize .gt. 0) i_nline=i_lsize-i_fline+1
      if (i_nline .gt. i_maxlines) then
        if (i_debug .ge. 1) then
          write(6,*) ' '
          write(6,*) '*** Error - Number of lines to display exceeds maximum, truncating to ',i_maxlines,i_nline
          write(6,*) ' '
        end if
        i_nline = i_maxlines
      end if

      if (r_spc(1) .eq. 0) then
        r_xs = 1.
      else
         r_xs = r_spc(1)
      end if
      if (r_spc(2) .eq. 0) then
        r_ys = 1.
      else
        r_ys = r_spc(2)
      end if

c
c  Read In data
c

         if (i_debug .ge. 3) then
           write(6,*) ' '
           write(6,'(x,a)') 'Checking file size...'
         end if

         if (a_rmgfile .ne. ' ') then
           i_err = getfsize(a_rmgfile,i_fsize)
           i_lines = min(i_fsize/(8*i_samps) - i_fline + 1,i_nline)
         else if (a_cmpfile .ne. ' ') then
           i_err = getfsize(a_cmpfile,i_fsize)
           if (i_cmptype .eq. 1) then
             i_lines = min(i_fsize/(8*i_samps) - i_fline + 1,i_nline)
           else
             i_lines = min(i_fsize/(4*i_samps) - i_fline + 1,i_nline)
           end if
         else
           if (a_magfile .ne. ' ') then
             i_err = getfsize(a_magfile,i_fsize)
             i_mlines = min(i_fsize/(i_mbytes*i_samps) - i_fline + 1,i_nline)
           end if
           if (a_dtefile .ne. ' ') then
             i_err = getfsize(a_dtefile,i_fsize)
             i_dlines = min(i_fsize/(i_dbytes*i_samps) - i_fline + 1,i_nline)
           end if
           i_lines = min(i_mlines,i_dlines)
         end if
         
         if (i_debug .ge. 2) then 
           write(6,*) '  samps = ',i_samps
           write(6,*) '  lines = ',i_lines+i_fline-1
         end if

         if (i_debug .ge. 3) then
           write(6,*) ' '
           write(6,'(x,a)') 'Computing scale factor.... '
         end if
         
         iy = 0
         i_err = 0
         i_cnt = 0
         r_sum = 0.
         r_sqr = 0.
         if (r_aavg .ne. 0.0 .and. r_astd .ne. 0.0) then
           r_avg = r_aavg
           r_std = r_astd
         else if (r_amin .ne. 0 .or. r_amax .ne. 0) then
           r_avg = (r_amin+r_amax)/2.
           r_std = (r_amax-r_amin)/4.
         else
           r_amax = -9.e+20
           r_amin =  9.e+20
           r_amax = -9.e+20
           r_amin =  9.e+20
           do ix=1,i_samps
             r_amp1(ix)=1.
             r_hgt1(ix)=0.
           end do
           do iy = 1,i_lines,min(max(i_lines/100,1),200) 
               if (a_rmgfile .ne. ' ') then
                 call readrmg(i_rmgfile,1,iy+i_fline-1, i_samps, i_samps,i_maxlines+i_fline,r_amp1,r_hgt1,i_flip,
     &                    i_flg,i_err)
               else if (a_cmpfile .ne. ' ') then
                 call readcmp(i_cmpfile,i_cmptype,1,iy+i_fline-1, i_samps, i_samps,i_maxlines+i_fline,
     &                    r_amp1,r_hgt1,i_flip,i_err)
               else
                 call readdat(i_dtefile,i_dtetype,1,iy+i_fline-1,i_doff,i_samps, i_samps,i_maxlines+i_fline,
     &                  r_dmul,r_dadd,r_hgt1,i_flip,0,i_err)
                 call readdat(i_magfile,i_magtype,1,iy+i_fline-1,i_moff,i_samps, i_samps,i_maxlines+i_fline,
     &                  r_mmul,r_madd,r_amp1,i_flip,i_flg,i_err)
               end if

               do ix = 1, i_samps, min(max(i_samps/100,1),200)
                 r_amax=max(r_amax,r_amp1(ix))
                 r_amin=min(r_amin,r_amp1(ix))
                 if (r_amp1(ix) .ne. 0. .and. r_amp1(ix) .ne. 10000.0 .and.
     &               r_amp1(ix) .ne. -10000. .and. r_amp1(ix) .lt. 1.0e20) then
                   i_cnt = i_cnt + 1
c                   r_sum = r_sum + (dfloat(r_amp1(ix))**r_expnt)
c                   r_sqr = r_sqr + (dfloat(r_amp1(ix))**r_expnt)**2
                   r_sum = r_sum + sign(abs(r_amp1(ix))**r_expnt,r_amp1(ix))
                   r_sqr = r_sqr + sign(abs(r_amp1(ix))**r_expnt,r_amp1(ix))**2
                 end if
               end do
           end do

           if (a_rmgfile .eq. ' ' .and. a_cmpfile .eq. ' '.and. i_magtype .eq. 0 ) then
             r_avg=1.
             r_std=1.
           else
             if (i_cnt .gt. 0) then
               if (r_aavg .eq. 0.0) then
                 r_avg = r_sum/max(i_cnt,1)
               else
                 r_avg = r_aavg
               end if
               if (r_astd .eq. 0.0) then
                 r_std = sqrt(max(1.d-25,(r_sqr/max(i_cnt,1))-r_avg**2))
               else
                 r_std = r_astd
               end if
               if (i_debug .ge. 2) then
                 write(6,*) '  avera = ',r_avg
                 write(6,*) '  stddv = ',r_std
               end if
               if (i_scale .eq. 0 ) then
                 r_std = r_avg
                 r_avg = 0
               end if
             else
               write(6,*) '  amin  = ',r_amin
               write(6,*) '  amax  = ',r_amax
               write(6,*) ' '
               write(6,*) 'No data found at ',max(min(100,abs(i_nline)/20),1), ' pix spacing!'
               write(6,'(x,a,$)') 'Enter avg and std: '
               read(5,*) r_avg,r_std
             end if
           end if
           write(6,*) ' '
           write(6,*) ' '
         end if
c
c  Initialize graphics
c
                i_gxi = 6
                if (i_mem .eq. 0) then
                  i_type(1) = 4
                else
                  i_type(1) = 5
                end if
                i_type(2) = 1
                i_type(3) = 1
                i_type(4) = 1
                i_type(5) = 1
                i_type(6) = 1


          if (a_rmgfile .ne. ' ') then
            i_pos = 0
            do while (index(a_rmgfile(i_pos+1:),'/') .ne. 0)
              i_pos = i_pos + index(a_rmgfile(i_pos+1:),'/')
            end do
            a_filename=a_rmgfile(i_pos+1:)
          else if (a_cmpfile .ne. ' ') then
            i_pos = 0
            do while (index(a_cmpfile(i_pos+1:),'/') .ne. 0)
              i_pos = i_pos + index(a_cmpfile(i_pos+1:),'/')
            end do
            a_filename=a_cmpfile(i_pos+1:)
          else if (a_magfile .ne. ' ' .and. a_dtefile .ne. ' ') then
            i_pos = 0
            do while (index(a_magfile(i_pos+1:),'/') .ne. 0)
              i_pos = i_pos + index(a_magfile(i_pos+1:),'/')
            end do
            a_filename=a_magfile(i_pos+1:)

            i_pos = 0
            do while (index(a_dtefile(i_pos+1:),'/') .ne. 0)
              i_pos = i_pos + index(a_dtefile(i_pos+1:),'/')
            end do
            a_filename=a_filename(1:max(length(a_filename),1))//', '//a_dtefile(i_pos+1:)
          else if (a_magfile .ne. ' ') then
            i_pos = 0
            do while (index(a_magfile(i_pos+1:),'/') .ne. 0)
              i_pos = i_pos + index(a_magfile(i_pos+1:),'/')
            end do
            a_filename=a_magfile(i_pos+1:)
          else if (a_dtefile .ne. ' ') then
            i_pos = 0
            do while (index(a_dtefile(i_pos+1:),'/') .ne. 0)
              i_pos = i_pos + index(a_dtefile(i_pos+1:),'/')
            end do
            a_filename=a_dtefile(i_pos+1:)
          else
            a_filename='Filename Error'
          end if
                a_labl(0) = a_filename
                a_labl(1) = 'Image Window'
                if ((a_rmgfile .ne. ' ') .or.
     &              (a_magfile .ne. ' ' .and. a_dtefile .ne. ' ') ) then
                  a_labl(2) = 'Amp'
                  a_labl(3) = 'Hgt'
                  a_labl(4) = 'Mix'
                else if (i_magtype .eq. 9 .and. i_dtetype .ne. 0) then
                  a_labl(2) = 'Slx'
                  a_labl(3) = 'Hgt'
                  a_labl(4) = 'Mix'
                else if (a_cmpfile .ne. ' ') then
                  a_labl(2) = 'Mag'
                  a_labl(3) = 'Pha'
                  a_labl(4) = 'Mix'
                else
                  a_labl(2) = 'Grey'
                  a_labl(3) = 'Colr'
                  a_labl(4) = ' '
                end if
                a_labl(5) = ' '
                a_labl(6) = 'Quit'

                i_wxs(0) = 500
                i_wys(0) = 600
                i_wxs(1) = i_samps
                i_wys(1) = i_lines
                i_wxs(2) = 50
                i_wys(2) = 50
                i_wxs(3) = 50
                i_wys(3) = 50
                i_wxs(4) = 50
                i_wys(4) = 50
                i_wxs(5) = 50
                i_wys(5) = 50
                i_wxs(6) = 50
                i_wys(6) = 50

                i_frx(0) = 5
                i_frx(1) = 5
                i_frx(2) = 1
                i_frx(3) = 1
                i_frx(4) = 1
                i_frx(5) = 1
                i_frx(6) = 1

                i_fry(0) = 0
                i_fry(1) = 400
                i_fry(2) = -40
                i_fry(3) = -40
                i_fry(4) = -40
                i_fry(5) = -40
                i_fry(6) = -40

                CALL init_gx(i_gxi,i_type,a_labl,i_wxs,i_wys,i_frx,i_fry,i_clrs,i_debug)
                call get_wininfo(1,i_vx,i_vy,i_vw,i_vh,i_cw,i_ch,i_widget) 
                call settable(i_colnum,i_colors)
                i_vxs(1)=i_vw                
                i_vys(1)=i_vh                
                i_vxo(1)=i_vx
                i_vyo(1)=i_vy
                if (ixxx .gt. 0) then
                  if (i_debug .ge. 3) write(6,*) 'Moving to ',ixxx,iyyy,'  xy set'
                  call move_scroll(1,max(ixxx-i_vxs(1)/2,0),max(iyyy-i_vys(1)/2,0))
                end if
                if (r_lat .gt. -r_pi/2.) then
                   if (a_type .eq. 'sch' .or. a_type .eq. 'SCH') then
                     r_hhh=0.
                     call latlon(r_a,r_e2,v_loc3,r_lat,r_lon,r_hhh,1)
                     call vecmulti(r_mta,v_loc3,v_loc2) ! convert from input xyz to output xyz
                     call vecaddit(r_mta(1,4),v_loc2,v_loc2)
                     call sch_to_tcn(r_rad,v_loc2,r_lat,r_lon,r_hhh,2)

                     v_loc1(1) = r_lon*r_rad
                     v_loc1(2) = r_lat*r_rad
                     v_loc1(3) = r_hhh

                   else if (a_type .eq. 'utm' .or. a_type .eq. 'UTM') then
                     call utmtoll(r_a,r_e2,i_zone,a_grid,v_loc1,r_lat,r_lon,2)

                   else if (a_type .eq. 'neu' .or. a_type .eq. 'NEU') then
                     call utmtoll(r_a,r_e2,i_zone,a_grid,v_loc1,r_lat,r_lon,2)

                   else if (a_type .eq. 'enu' .or. a_type .eq. 'ENU') then
                     call enutoll(r_a,r_e2,i_zone,a_grid,v_loc1,r_lat,r_lon,2)

                   else if (a_type .eq. 'eqa' .or. a_type .eq. 'EQA') then
                     v_loc1(1) = r_lat
                     v_loc1(2) = r_lon
                   end if
                   if (i_flip .eq. 0) then
                     iyyy = (v_loc1(1)-r_str(1))/r_spc(1)-i_fline+1
                     ixxx = (v_loc1(2)-r_str(2))/r_spc(2)
                   else
                     iyyy =   (v_loc1(1)-r_str(1))/r_spc(1)-i_fline+1
                     ixxx =  -(v_loc1(2)-r_str(2))/r_spc(2) + i_samps + 1
                   end if

                   if (ixxx .gt. 0) then
                     if (i_debug .ge. 3) write(6,*) 'Moving to ',ixxx,iyyy,'  ll set'
                     call move_scroll(1,max(ixxx-i_vxs(1)/2,0),max(iyyy-i_vys(1)/2,0))
                   end if
                end if
                if (i_colr .eq. 0) then
                  if (a_rmgfile .eq. ' ' .and. a_magfile .eq. ' ' .and. a_cmpfile .eq. ' ') then 
                    call setcolor(4)
                    i_ccc=2
                  else
                    call setcolor(2)
                    i_ccc=1
                  end if
                else 
                  if (a_rmgfile .eq. ' ' .and. a_magfile .eq. ' ' .and. a_cmpfile .eq. ' ') then 
                    i_colr = 2
                    call setcolor(9)
                    i_ccc=2
                  else
                    call setcolor(1)
                    i_ccc=1
                  end if
                end if 


c
c  Start Managing Window
c
                i_bcnt = 0
                i_done = 0
                i_wait = 0  											! 0 = wait for event 
                do while(i_done .eq. 0)
                  i_event(1) = -1
                  do while(i_event(1) .ne. 0 .and. i_done .eq. 0 .and. i_bcnt .lt. I_BMAX)  
                    if (i_bcnt .lt. I_BMAX) call getevent(i_wait,i_event)

                    if (i_debug .ge. 4) then
                      if (i_event(2) .ne. 0 .or. i_debug .ge. 5) then
                        write(6,'(x,a,6i10)') 
     &                     'i_event=',i_event(1),i_event(2),i_event(3),i_event(4),i_event(5),i_event(6)
                      end if
                    end if
c
c  patch to do Pauls right mouse color table change like Charlies software
c
                    if (i_tgl .eq. 1) then   ! 
                      if (i_tgk .eq. 0) then
                        if (i_event(2) .eq. 4) then ! a click
                          if (i_event(1) .eq. 1 .or. i_event(1) .eq. i_zmw) then ! a click in main window
                            if (i_event(3) .eq. i_clk) then ! desired mouse button
                              i_event(1) = mod(i_ccc,3) + 2
                              if (i_colr .eq. 0) then
                                i_event(3) = 1
                              else
                                i_event(3) = 2
                              end if
                            end if
                          end if
                        end if
                      else if (i_event(2) .eq. 6) then ! key press
                        if (i_event(1) .eq. 1 .or. i_event(1) .eq. i_zmw) then ! in main window
                          if (i_event(6) .eq. i_tgk) then
                            i_event(1) = mod(i_ccc,3) + 2
                            i_event(2) = 4
                            if (i_asc .eq. 0) then
                              i_event(6) = 1
                            else
                              i_event(6) = 2
                              if (i_asc .eq. 65505) i_asc=0
                            end if
                          end if
                        end if
                      end if
                    end if

c
c  patch to do color table change via function keys
c  
                    if (i_event(2) .eq. 6) then ! key press
                      if (i_debug .ge. 4) write(6,*) 'key =',i_event(6),char(i_event(6))
                      if (i_event(1) .eq. 1 .or. i_event(1) .eq. i_zmw) then ! in main window
                        if (i_event(6) .eq. ichar('s') .or. i_event(6) .eq. ichar('S') .or. 
     &                      i_event(6) .eq. ichar('j') .or. i_event(6) .eq. ichar('J') .or.
     &                      i_event(6) .eq. ichar('1') .or. i_event(6) .eq. ichar('!') .or.
     &                      i_event(6) .eq. 65470      .or. i_event(6) .eq. 65361    ) then
                          i_event(1) = 2
                          i_event(2) = 4
                          if (i_asc .eq. 0) then
                            i_event(3) = 1
                          else
                            i_event(3) = 2
                            if (i_asc .eq. 65505) i_asc=0
                          end if
                        else if (i_event(6) .eq. ichar('d') .or. i_event(6) .eq. ichar('D') .or. 
     &                           i_event(6) .eq. ichar('k') .or. i_event(6) .eq. ichar('K') .or.
     &                           i_event(6) .eq. ichar('2') .or. i_event(6) .eq. ichar('@') .or.
     &                           i_event(6) .eq. 65471      .or. i_event(6) .eq. 65364    ) then
                          i_event(1) = 3
                          i_event(2) = 4
                          if (i_asc .eq. 0) then
                            i_event(3) = 1
                          else
                            i_event(3) = 2
                            if (i_asc .eq. 65505) i_asc=0
                          end if
                        else if (i_event(6) .eq. ichar('f') .or. i_event(6) .eq. ichar('F') .or. 
     &                           i_event(6) .eq. ichar('l') .or. i_event(6) .eq. ichar('L') .or.
     &                           i_event(6) .eq. ichar('3') .or. i_event(6) .eq. ichar('#') .or.
     &                           i_event(6) .eq. 65472      .or. i_event(6) .eq. 65363    ) then
                          i_event(1) = 4
                          i_event(2) = 4
                          if (i_asc .eq. 0) then
                            i_event(3) = 1
                          else
                            i_event(3) = 2
                            if (i_asc .eq. 65505) i_asc=0
                          end if
                        else if (i_event(6) .eq. ichar('q') .or. i_event(6) .eq. ichar('Q') .or. 
     &                           i_event(6) .eq. ichar('x') .or. i_event(6) .eq. ichar('x') .or.
     &                           i_event(6) .eq. 65307 ) then
                          i_event(1) = 6
                          i_event(2) = 4
                          i_event(3) = 1
                        end if
                      end if
                    end if

                    if (i_event(2) .eq. 0) then
                      ! do nothing
                    else if (i_event(2) .eq. 1) then  						! Expose event
                      if (i_event(1) .eq. 0) then
                        ! do nothing
                      else if (i_event(1) .le. 1 .or. i_event(1) .eq. i_zmw) then	! Click in window 1 or i_zmw
                        if (i_event(5)*i_event(6) .lt. i_wsamps) then
                          i_bcnt = i_bcnt + 1
                          if (i_bcnt .gt. I_BMAX) i_bcnt = I_BMAX
                          do i = 1,10
                            i_bdat(i,i_bcnt) = i_event(i)
                          end do
                        else
                          i_bline = i_wsamps/i_event(5)
                          i_blks  = (i_event(6)-1)/i_bline + 1
                          do ib = 1,i_blks
                            i_bcnt = min(i_bcnt+1,I_BMAX)
                            i_bdat(1,i_bcnt) = i_event(1)
                            i_bdat(2,i_bcnt) = i_event(2)
                            i_bdat(3,i_bcnt) = i_event(3)
                            i_bdat(4,i_bcnt) = i_event(4)+((ib-1)*i_bline)
                            i_bdat(5,i_bcnt) = i_event(5)
                            i_bdat(6,i_bcnt) = i_bline
                            i_bdat(7,i_bcnt) = i_event(7)
                            i_bdat(8,i_bcnt) = i_event(8)
                            i_bdat(9,i_bcnt) = i_event(9)
                            i_bdat(10,i_bcnt) = i_event(10)
                          end do
                        end if
                        if (i_wait .eq. 0) then
                          i_wait = 1
                          iy = 0
                          if (i_debug .ge. 4) write(6,*) 'turning wait off',i_wait
                        end if
                      end if

                    else if (i_event(2) .eq. 2) then  						! Configure window event
                      i_vxo(i_event(1)) = i_event(3)
                      i_vyo(i_event(1)) = i_event(4)
                      i_wxs(i_event(1)) = i_event(5)
                      i_wys(i_event(1)) = i_event(6)
                      if (i_event(1) .eq. i_zmw) then
                        if (i_vxs(i_zmw)*i_vys(i_zmw) .le. i_wsamps) then
                          i_bcnt = min(i_bcnt+1,I_BMAX)
                          i_bdat(1,i_bcnt) = i_zmw
                          i_bdat(2,i_bcnt) = 1
                          i_bdat(3,i_bcnt) = i_vxo(i_zmw)
                          i_bdat(4,i_bcnt) = i_vyo(i_zmw)
                          i_bdat(5,i_bcnt) = i_vxs(i_zmw) 
                          i_bdat(6,i_bcnt) = i_vys(i_zmw) 
                        else
                          i_bline = i_wsamps/i_vxs(i_zmw) 
                          i_blks  = (i_vys(i_zmw)-1)/i_bline + 1
                          do ib = 1,i_blks
                            i_bcnt = min(i_bcnt+1,I_BMAX)
                            i_bdat(1,i_bcnt) = 1
                            i_bdat(2,i_bcnt) = 1
                            i_bdat(3,i_bcnt) = i_vxo(i_zmw)-1
                            i_bdat(4,i_bcnt) = i_vyo(i_zmw)+((ib-1)*i_bline)-1
                            i_bdat(5,i_bcnt) = i_vxs(i_zmw)
                            i_bdat(6,i_bcnt) = i_bline
                          enddo
                        endif
                        if (i_wait .eq. 0) then
                          i_wait = 1
                          iy = 0
                          if (i_debug .ge. 4) write(6,*) 'turning wait off',i_wait
                        end if
                      end if

                    else if (i_event(2) .eq. 3) then  						! Configure window event
                      i_vxs(i_event(1)) = i_event(5)
                      i_vys(i_event(1)) = i_event(6)
                      if (i_event(1) .eq. i_zmw) then
                        if (i_vxs(i_zmw)*i_vys(i_zmw) .le. i_wsamps) then
                          i_bcnt = min(i_bcnt+1,I_BMAX)
                          i_bdat(1,i_bcnt) = i_zmw
                          i_bdat(2,i_bcnt) = 1
                          i_bdat(3,i_bcnt) = i_vxo(i_zmw)
                          i_bdat(4,i_bcnt) = i_vyo(i_zmw)
                          i_bdat(5,i_bcnt) = i_vxs(i_zmw) 
                          i_bdat(6,i_bcnt) = i_vys(i_zmw) 
                        else
                          i_bline = i_wsamps/i_vxs(i_zmw) 
                          i_blks  = (i_wys(i_zmw)-1)/i_bline + 1
                          do ib = 1,i_blks
                            i_bcnt = min(i_bcnt+1,I_BMAX)
                            i_bdat(1,i_bcnt) = 1
                            i_bdat(2,i_bcnt) = 1
                            i_bdat(3,i_bcnt) = i_vxo(i_zmw)-1
                            i_bdat(4,i_bcnt) = i_vyo(i_zmw)+((ib-1)*i_bline)-1
                            i_bdat(5,i_bcnt) = i_vxs(i_zmw)
                            i_bdat(6,i_bcnt) = i_bline
                          enddo
                        endif
                        if (i_wait .eq. 0) then
                          i_wait = 1
                          iy = 0
                          if (i_debug .ge. 4) write(6,*) 'turning wait off',i_wait
                        end if
                      end if

                    else if (i_event(2) .eq. 4) then                        ! Click Event
                      if (i_event(1) .eq. 0) then
                        ! do nothing
                      else if (i_event(1) .eq. 1 .or. i_event(1) .eq. i_zmw) then	! Click in window 1 or i_zmw
                        i_button=i_event(3)
                        i_wb=i_event(1)
                        i_bcnt = i_bcnt + 1										! buffer click for later
                        if (i_bcnt .gt. I_BMAX) i_bcnt = I_BMAX
                        do i = 1,10
                          i_bdat(i,i_bcnt) = i_event(i)
                        end do
                        if (i_wait .eq. 0) then
                          i_wait = 1
                          iy = 0
                          if (i_debug .ge. 4) write(6,*) 'turning wait off',i_wait
                        end if
                      else if (i_event(1) .eq. -1) then					! Click in label for window 1
                        a_rsp = ' '
                        if (i_debug .ge. 5) write(6,*) 'i_event(3)=',i_event(3)
                        if ((i_asc .ne. 0 .or. i_event(3) .eq. 2) .and. 
     &                      (a_type .eq. 'sch' .or. a_type .eq. 'SCH')) then
                          write(a_msg,'(a,f5.2,a,f5.2,a)') 
     &                        'Enter location (lat,long): '
                          call get_dialog(a_msg,a_rsp)
                          if (a_rsp .ne. ' ') then
                            read(a_rsp,*,iostat=i_err) r_lat,r_lon

                            r_lat=r_lat/r_rtod
                            r_lon=r_lon/r_rtod
                            r_hhh=0.
                            
                            call latlon(r_a,r_e2,v_loc3,r_lat,r_lon,r_hhh,1)
                            call vecmulti(r_mta,v_loc3,v_loc2)            
                            call vecaddit(r_mta(1,4),v_loc2,v_loc2)
                            call sch_to_tcn(r_rad,v_loc2,r_lat,r_lon,r_hhh,2)
                            v_loc1(1) = r_lon*r_rad
                            v_loc1(2) = r_lat*r_rad
                            v_loc1(3) = r_hhh

                            if (i_flip .eq. 0) then
                              iyyy = (v_loc1(1) - r_str(1))/r_spc(1) - i_fline+1 
                              ixxx = (v_loc1(2) - r_str(2))/r_spc(2)
                            else
                              iyyy =          (v_loc1(1) - r_str(1))/r_spc(1) - i_fline+1 
                              ixxx = (i_samps-(v_loc1(2) - r_str(2))/r_spc(2)) + 1
                            end if
                            call move_scroll(1,max(ixxx-i_vxs(1)/2,0),max(iyyy-i_vys(1)/2,0))
                          end if
                        else if ((i_asc .ne. 0 .or. i_event(3) .eq. 2) .and. 
     &                        (a_type .eq. 'eqa' .or. a_type .eq. 'EQA')) then
                          write(a_msg,'(a,f5.2,a,f5.2,a)') 
     &                        'Enter location (lat,long): '
                          call get_dialog(a_msg,a_rsp)
                          if (a_rsp .ne. ' ') then
                            read(a_rsp,*,iostat=i_err) r_lat,r_lon

                            v_loc1(1) = r_lat
                            v_loc1(2) = r_lon
                            v_loc1(3) = 0.

                            if (i_flip .eq. 0) then
                              iyyy = (v_loc1(1) - r_str(1))/r_spc(1) - i_fline+1 
                              ixxx = (v_loc1(2) - r_str(2))/r_spc(2)
                            else
                              iyyy =          (v_loc1(1) - r_str(1))/r_spc(1) - i_fline+1 
                              ixxx = (i_samps-(v_loc1(2) - r_str(2))/r_spc(2)) + 1
                            end if
                            call move_scroll(1,max(ixxx-i_vxs(1)/2,0),max(iyyy-i_vys(1)/2,0))
                          end if
                        else
                          write(a_msg,'(a,f5.2,a,f5.2,a)') 
     &                        'Enter pixel (x,y): '
                          call get_dialog(a_msg,a_rsp)
                          if (a_rsp .ne. ' ') then
                            read(a_rsp,*,iostat=i_err) ixxx,iyyy
                            call move_scroll(1,max(ixxx-i_vxs(1)/2,0),max(iyyy-i_vys(1)/2,0))
                          end if
                        end if

                      else if (i_event(1) .eq. 2) then					! Button 1
                        i_ccc = 1
                        if (i_event(2) .eq. 4) then
                          if (i_asc .ne. 0 .and. (a_rmgfile .ne. ' ' .or. 
     &                                           (a_dtefile .ne. ' ' .and. a_magfile .ne. ' '))) then ! Change to slope and recompute scale
                            if (i_asc .eq. 48) then
                              i_flg = 0
                            else if (i_asc .eq. 49) then
                              i_flg = 1
                              i_flgtgl = 1
                            else if (i_asc .eq. 50) then
                              i_flg = 2
                              i_flgtgl = 2
                            else
                              i_flg = i_flgtgl - i_flg
                            end if

                              if (i_debug .ge. 3) write(6,'(x,a,2x,i)') 'Re-computing scale factor.... ', i_flg

                              i_err=0
                              i_cnt = 0
                              r_sum = 0.
                              r_sqr = 0.
                              r_amax = -9.e+20
                              r_amin =  9.e+20
                              r_amax = -9.e+20
                              r_amin =  9.e+20
                              do ix=1,i_samps
                                r_amp1(ix)=1.
                                r_hgt1(ix)=0.
                              end do
                              do il = 1,i_lines 
                                ix = 1
                                if (mod(il,max(min(100,abs(i_nline)/20),1)) .eq. 1) then
                                  if (a_rmgfile .ne. ' ') then
                                    call readrmg(i_rmgfile,ix,il+i_fline-1, i_samps, i_samps,i_maxlines+i_fline,r_amp1,
     &                                    r_hgt1,i_flip,i_flg,i_err)
                                  else if (a_cmpfile .ne. ' ') then
                                    call readcmp(i_cmpfile,i_cmptype,ix,il+i_fline-1, i_samps, i_samps,
     &                                    i_maxlines+i_fline,r_amp1,r_hgt1,i_flip,i_err)
                                  else
                                    call readdat(i_dtefile,i_dtetype,ix,il+i_fline-1,i_doff,i_samps, i_samps,
     &                                     i_maxlines+i_fline,r_dmul,r_dadd,r_hgt1,i_flip,0,i_err)
                                    call readdat(i_magfile,i_magtype,ix,il+i_fline-1,i_moff,i_samps, i_samps,
     &                                     i_maxlines+i_fline,r_mmul,r_madd,r_amp1,i_flip,i_flg,i_err)
                                  end if
                                  do ix = 1, i_samps, max(i_samps/100,1)
                                    r_amax=max(r_amax,r_amp1(ix))
                                    r_amin=min(r_amin,r_amp1(ix))
                                    if (r_amp1(ix) .ne. 0. .and. r_amp1(ix) .ne. 10000.0 .and.
     &                                  r_amp1(ix) .ne. -10000. .and. r_amp1(ix) .lt. 1.0e20) then
                                      i_cnt = i_cnt + 1
c                                      r_sum = r_sum + (dfloat(r_amp1(ix))**r_expnt)
c                                      r_sqr = r_sqr + (dfloat(r_amp1(ix))**r_expnt)**2
                                      r_sum = r_sum + sign(abs(r_amp1(ix))**r_expnt,r_amp1(ix))
                                      r_sqr = r_sqr + sign(abs(r_amp1(ix))**r_expnt,r_amp1(ix))**2
                                    end if
                                  end do
                                end if
                              end do
                     
                              if (a_rmgfile .eq. ' ' .and. a_cmpfile .eq. ' '.and. i_magtype .eq. 0 ) then
                                r_avg=1.
                                r_std=1.
                              else
                                if (i_cnt .gt. 0) then
                                  r_avg = r_sum/max(i_cnt,1)
                                  r_std = sqrt(max(1.d-25,(r_sqr/max(i_cnt,1))-r_avg**2))
                                  if (i_debug .ge. 3) write(6,*) '  avera = ',r_avg
                                  if (i_debug .ge. 3) write(6,*) '  stddv = ',r_std
                                  if (i_scale .eq. 0 ) then
                                    r_std = r_avg
                                    r_avg = 0
                                  end if
                                else
                                  write(6,*) '  amin  = ',r_amin
                                  write(6,*) '  amax  = ',r_amax
                                  write(6,*) ' '
                                  write(6,*) 'No data found at ',max(min(100,abs(i_nline)/20),1), ' pix spacing!'
                                  write(6,'(x,a,$)') 'Enter avg and std: '
                                  read(5,*) r_avg,r_std
                                end if
                              end if
                              write(6,*) ' '
                              write(6,*) ' '
                     

                          end if 
                          if (i_event(3) .ne. 1 .or. i_colr .ne. 0 .or. i_asc .ne. 0 .or. i_lock .eq. 1) then
                            if (i_mem .eq. 0 .and. i_vxs(1)*i_vys(1) .le. i_wsamps) then
                              i_bcnt = min(i_bcnt+1,I_BMAX)
                              i_bdat(1,i_bcnt) = 1
                              i_bdat(2,i_bcnt) = 1
                              i_bdat(3,i_bcnt) = i_vxo(1)-1
                              i_bdat(4,i_bcnt) = i_vyo(1)-1
                              i_bdat(5,i_bcnt) = i_vxs(1) 
                              i_bdat(6,i_bcnt) = i_vys(1) 
                            else
                              i_bline = i_wsamps/i_wxs(1) 
                              i_blks  = (i_wys(1)-1)/i_bline + 1
                              do ib = 1,i_blks
                                i_bcnt = min(i_bcnt+1,I_BMAX)
                                i_bdat(1,i_bcnt) = 1
                                i_bdat(2,i_bcnt) = 1
                                i_bdat(3,i_bcnt) = 0
                                i_bdat(4,i_bcnt) = ((ib-1)*i_bline)
                                i_bdat(5,i_bcnt) = i_wxs(1)
                                i_bdat(6,i_bcnt) = i_bline
                              end do
                            end if
                            if (r_zoom .ne. 0) then
                             if (i_vxs(i_zmw)*i_vys(i_zmw) .le.  i_wsamps) then 
                              i_bcnt = min(i_bcnt+1,I_BMAX)
                              i_bdat(1,i_bcnt) = i_zmw
                              i_bdat(2,i_bcnt) = 1
                              i_bdat(3,i_bcnt) = i_vxo(i_zmw)
                              i_bdat(4,i_bcnt) = i_vyo(i_zmw)
                              i_bdat(5,i_bcnt) = i_vxs(i_zmw)
                              i_bdat(6,i_bcnt) = i_vys(i_zmw) 
                             else
                              i_bline = i_wsamps/i_vxs(i_zmw) 
                              i_blks  = (i_vys(i_zmw)-1)/i_bline + 1
                              do ib = 1,i_blks
                                i_bcnt = min(i_bcnt+1,I_BMAX)
                                i_bdat(1,i_bcnt) = 1
                                i_bdat(2,i_bcnt) = 1
                                i_bdat(3,i_bcnt) = i_vxo(i_zmw)-1
                                i_bdat(4,i_bcnt) = i_vyo(i_zmw)+((ib-1)*i_bline)-1
                                i_bdat(5,i_bcnt) = i_vxs(i_zmw)
                                i_bdat(6,i_bcnt) = i_bline
                              enddo
                             endif
                            end if
                            if (i_wait .eq. 0) then
                              i_wait = 1
                              iy = 0
                              if (i_debug .ge. 4) write(6,*) 'turning wait off',i_wait
                            end if
                          end if
                          if (i_event(3) .le. 0) then
                            if (i_debug .ge. 1) write(6,*) 'Button press error',i_event(3)
                          else if (i_event(3) .eq. 1) then
                            if (i_lock .eq. 0) then
                              i_colr = 0
                            else
                              if (i_colr .ne. 0) i_colr = 1
                            end if
                          else if (i_event(3) .eq. 2) then
                            if (i_lock .eq. 0 .or. i_colr .ne. 0) i_colr = 1
                          else if (i_event(3) .eq. 3) then
                            r_xlast = r_expnt
                            a_rsp = ' '
                            if (i_scale .eq. 0) then
                              r_ascale = 1./r_ascale
                            else
                              r_ascale = r_ascale/2.
                            end if
c                            r_ascale=1.
c                            r_expnt=1.
                            write(a_msg,'(a,f5.2,a,f5.2,a)') 
     &                          'Enter amp scale & exp [def=',r_ascale,',',r_expnt,']'
                            call get_dialog(a_msg,a_rsp)
                            if (a_rsp .ne. ' ') then
                              read(a_rsp,*,iostat=i_err) r_ascale,r_expnt
                            end if
                            if (r_ascale .eq. 0.) r_ascale = 1.
                            if (i_scale .eq. 0) then
                              r_ascale = 1./r_ascale
                            else
                              r_ascale = 2.*r_ascale
                            end if

                            if (r_expnt  .eq. 0.) r_expnt  = 1.
                            if (r_expnt .ne. r_xlast) then

                              if (i_debug .ge. 3) write(6,'(x,a)') 'Re-computing scale factor.... '

                              i_err=0
                              i_cnt = 0
                              r_sum = 0.
                              r_sqr = 0.
                              r_amax = -9.e+20
                              r_amin =  9.e+20
                              do ix=1,i_samps
                                r_amp1(ix)=1.
                                r_hgt1(ix)=0.
                              end do
                              do il = 1,i_lines 
                                ix = 1
                                if (mod(il,max(min(100,abs(i_nline)/20),1)) .eq. 1) then
                                  if (a_rmgfile .ne. ' ') then
                                    call readrmg(i_rmgfile,ix,il+i_fline-1, i_samps, i_samps,i_maxlines+i_fline,r_amp1,
     &                                    r_hgt1,i_flip,i_flg,i_err)
                                  else if (a_cmpfile .ne. ' ') then
                                    call readcmp(i_cmpfile,i_cmptype,ix,il+i_fline-1, i_samps, i_samps,
     &                                    i_maxlines+i_fline,r_amp1,r_hgt1,i_flip,i_err)
                                  else
                                    call readdat(i_dtefile,i_dtetype,ix,il+i_fline-1,i_doff,i_samps, i_samps,
     &                                     i_maxlines+i_fline,r_dmul,r_dadd,r_hgt1,i_flip,0,i_err)
                                    call readdat(i_magfile,i_magtype,ix,il+i_fline-1,i_moff,i_samps, i_samps,
     &                                     i_maxlines+i_fline,r_mmul,r_madd,r_amp1,i_flip,i_flg,i_err)
                                  end if
                                  do ix = 1, i_samps, max(i_samps/100,1)
                                    r_amax=max(r_amax,r_amp1(ix))
                                    r_amin=min(r_amin,r_amp1(ix))
                                    if (r_amp1(ix) .ne. 0. .and. r_amp1(ix) .ne. 10000.0 .and.
     &                                  r_amp1(ix) .ne. -10000. .and. r_amp1(ix) .lt. 1.0e20) then
                                      i_cnt = i_cnt + 1
c                                      r_sum = r_sum + (dfloat(r_amp1(ix))**r_expnt)
c                                      r_sqr = r_sqr + (dfloat(r_amp1(ix))**r_expnt)**2
                                      r_sum = r_sum + sign(abs(r_amp1(ix))**r_expnt,r_amp1(ix))
                                      r_sqr = r_sqr + sign(abs(r_amp1(ix))**r_expnt,r_amp1(ix))**2
                                    end if
                                  end do
                                end if
                              end do
                     
                              if (a_rmgfile .eq. ' ' .and. a_cmpfile .eq. ' '.and. i_magtype .eq. 0 ) then
                                r_avg=1.
                                r_std=1.
                              else
                                if (i_cnt .gt. 0) then
                                  r_avg = r_sum/max(i_cnt,1)
                                  r_std = sqrt(max(1.d-25,(r_sqr/max(i_cnt,1))-r_avg**2))
                                  if (i_debug .ge. 3) write(6,*) '  avera = ',r_avg
                                  if (i_debug .ge. 3) write(6,*) '  stddv = ',r_std
                                  if (i_scale .eq. 0 ) then
                                    r_std = r_avg
                                    r_avg = 0
                                  end if
                                else
                                  write(6,*) '  amin  = ',r_amin
                                  write(6,*) '  amax  = ',r_amax
                                  write(6,*) ' '
                                  write(6,*) 'No data found at ',max(min(100,abs(i_nline)/20),1), ' pix spacing!'
                                  write(6,'(x,a,$)') 'Enter avg and std: '
                                  read(5,*) r_avg,r_std
                                end if
                              end if
                              write(6,*) ' '
                              write(6,*) ' '
                     
                            end if
                          end if
                          if (i_colr .eq. 0) then
                            call setcolor(2)
                          else
                            call setcolor(1)
                          end if
                        end if
                      else if (i_event(1) .eq. 3) then					! Button 2
                        if (i_asc .ne. 0) then
                          a_rsp = ' '
                          if (i_asc .eq. ichar('b') .or. i_asc .eq. ichar('B')) then
                            write(a_msg,'(a)') 
     &                        'Enter Binary Colormap Filename: '
                            call get_dialog(a_msg,a_rsp)
                            a_rsp = a_rsp(1:max(1,length(a_rsp)))
                            if (a_rsp .ne. ' ') then
                              call getbcolors(a_rsp,i_colnum,i_colors)
                              call settable(i_colnum,i_colors)
                            end if
                          else
                            write(a_msg,'(a)') 
     &                        'Enter ASCII Colormap Filename: '
                            call get_dialog(a_msg,a_rsp)
                            a_rsp = a_rsp(1:max(1,length(a_rsp)))
                            if (a_rsp .ne. ' ') then
                              call getcolors(a_rsp,i_colnum,i_colors)
                              call settable(i_colnum,i_colors)
                            end if
                          end if
                        end if
                        i_ccc = 2
                        if (i_event(2) .eq. 4) then
                          if (i_event(3) .ne. 1 .or. i_colr .ne. 0 .or. i_lock .eq. 1) then
                            if (i_mem .eq. 0 .and. i_vxs(1)*i_vys(1) .le. i_wsamps) then
                              i_bcnt = min(i_bcnt+1,I_BMAX)
                              i_bdat(1,i_bcnt) = 1
                              i_bdat(2,i_bcnt) = 1
                              i_bdat(3,i_bcnt) = i_vxo(1)-1
                              i_bdat(4,i_bcnt) = i_vyo(1)-1
                              i_bdat(5,i_bcnt) = i_vxs(1) 
                              i_bdat(6,i_bcnt) = i_vys(1) 
                            else
                              i_bline = i_wsamps/i_wxs(1) 
                              i_blks  = (i_wys(1)-1)/i_bline + 1
                              do ib = 1,i_blks
                                i_bcnt = min(i_bcnt+1,I_BMAX)
                                i_bdat(1,i_bcnt) = 1
                                i_bdat(2,i_bcnt) = 1
                                i_bdat(3,i_bcnt) = 0
                                i_bdat(4,i_bcnt) = ((ib-1)*i_bline)
                                i_bdat(5,i_bcnt) = i_wxs(1)
                                i_bdat(6,i_bcnt) = i_bline
                              end do
                            end if
                            if (r_zoom .ne. 0) then
                             if (i_vxs(i_zmw)*i_vys(i_zmw) .le.  i_wsamps) then
                              i_bcnt = min(i_bcnt+1,I_BMAX)
                              i_bdat(1,i_bcnt) = i_zmw
                              i_bdat(2,i_bcnt) = 1
                              i_bdat(3,i_bcnt) = i_vxo(i_zmw)
                              i_bdat(4,i_bcnt) = i_vyo(i_zmw)
                              i_bdat(5,i_bcnt) = i_vxs(i_zmw)
                              i_bdat(6,i_bcnt) = i_vys(i_zmw) 
                             else
                              i_bline = i_wsamps/i_vxs(i_zmw) 
                              i_blks  = (i_vys(i_zmw)-1)/i_bline + 1
                              do ib = 1,i_blks
                                i_bcnt = min(i_bcnt+1,I_BMAX)
                                i_bdat(1,i_bcnt) = 1
                                i_bdat(2,i_bcnt) = 1
                                i_bdat(3,i_bcnt) = i_vxo(i_zmw)-1
                                i_bdat(4,i_bcnt) = i_vyo(i_zmw)+((ib-1)*i_bline)-1
                                i_bdat(5,i_bcnt) = i_vxs(i_zmw)
                                i_bdat(6,i_bcnt) = i_bline
                              enddo
                             endif
                            end if
                            if (i_wait .eq. 0) then
                              i_wait = 1
                              iy = 0
                              if (i_debug .ge. 4) write(6,*) 'turning wait off',i_wait
                            end if
                          end if
                          if (i_event(3) .le. 0) then
                            if (i_debug .ge. 1) write(6,*) 'Button press error',i_event(3)
                          else if (i_event(3) .eq. 1) then
                            if (i_lock .eq. 0) then
                              i_colr = 0
                            else
                              if (i_colr .ne. 0) i_colr = 2
                            end if
                          else if (i_event(3) .eq. 2) then
                            if (i_lock .eq. 0 .or. i_colr .ne. 0) i_colr = 2
                          else if (i_event(3) .eq. 3) then
                            a_rsp = ' '
                            r_hscale=100.
                            write(a_msg,'(a,f7.1,a)') 
     &                          'Enter height scale [def=',r_hscale,']'
                            call get_dialog(a_msg,a_rsp)
                            if (a_rsp .ne. ' ') then
                              read(a_rsp,*,iostat=i_err) r_hscale
                            end if
                            if (r_hscale .eq. 0.) r_hscale = 100.
                          end if
                          if (i_colr .eq. 0) then
                            call setcolor(4)
                          else
                            call setcolor(9)
                          end if
                        end if
                      else if (i_event(1) .eq. 4) then					! Button 3
                        i_ccc = 3
                        if (i_event(2) .eq. 4) then
                          if (i_event(3) .ne. 1 .or. i_colr .ne. 0 .or. i_lock .eq. 1) then
                            if (i_mem .eq. 0 .and. i_vxs(1)*i_vys(1) .le. i_wsamps) then
                              i_bcnt = min(i_bcnt+1,I_BMAX)
                              i_bdat(1,i_bcnt) = 1
                              i_bdat(2,i_bcnt) = 1
                              i_bdat(3,i_bcnt) = i_vxo(1)-1
                              i_bdat(4,i_bcnt) = i_vyo(1)-1
                              i_bdat(5,i_bcnt) = i_vxs(1) 
                              i_bdat(6,i_bcnt) = i_vys(1) 
                            else
                              i_bline = i_wsamps/i_wxs(1) 
                              i_blks  = (i_wys(1)-1)/i_bline + 1
                              do ib = 1,i_blks
                                i_bcnt = min(i_bcnt+1,I_BMAX)
                                i_bdat(1,i_bcnt) = 1
                                i_bdat(2,i_bcnt) = 1
                                i_bdat(3,i_bcnt) = 0
                                i_bdat(4,i_bcnt) = ((ib-1)*i_bline)
                                i_bdat(5,i_bcnt) = i_wxs(1)
                                i_bdat(6,i_bcnt) = i_bline
                              end do
                            end if
                            if (r_zoom .ne. 0) then
                             if (i_vxs(i_zmw)*i_vys(i_zmw) .le.  i_wsamps) then
                              i_bcnt = min(i_bcnt+1,I_BMAX)
                              i_bdat(1,i_bcnt) = i_zmw
                              i_bdat(2,i_bcnt) = 1
                              i_bdat(3,i_bcnt) = i_vxo(i_zmw)
                              i_bdat(4,i_bcnt) = i_vyo(i_zmw)
                              i_bdat(5,i_bcnt) = i_vxs(i_zmw)
                              i_bdat(6,i_bcnt) = i_vys(i_zmw) 
                             else
                              i_bline = i_wsamps/i_vxs(i_zmw) 
                              i_blks  = (i_vys(i_zmw)-1)/i_bline + 1
                              do ib = 1,i_blks
                                i_bcnt = min(i_bcnt+1,I_BMAX)
                                i_bdat(1,i_bcnt) = 1
                                i_bdat(2,i_bcnt) = 1
                                i_bdat(3,i_bcnt) = i_vxo(i_zmw)-1
                                i_bdat(4,i_bcnt) = i_vyo(i_zmw)+((ib-1)*i_bline)-1
                                i_bdat(5,i_bcnt) = i_vxs(i_zmw)
                                i_bdat(6,i_bcnt) = i_bline
                              enddo
                             endif
                            end if
                            if (i_wait .eq. 0) then
                              i_wait = 1
                              iy = 0
                              if (i_debug .ge. 4) write(6,*) 'turning wait off',i_wait
                            end if
                          end if
                          if (i_event(3) .le. 0) then
                            if (i_debug .ge. 1) write(6,*) 'Button press error',i_event(3)
                          else if (i_event(3) .eq. 1) then
                            if (i_lock .eq. 0) then
                              i_colr = 0
                            else
                              if (i_colr .ne. 0) i_colr = 3
                            end if
                          else if (i_event(3) .eq. 2) then
                            if (i_lock .eq. 0 .or. i_colr .ne. 0) i_colr = 3
                          else if (i_event(3) .eq. 3) then
                            a_rsp = ' '
                            if (i_asc .eq. 0) then
                              write(a_msg,'(a)') 
     &                          'Enter ASCII Colormap Filename: '
                              call get_dialog(a_msg,a_rsp)
                              a_rsp = a_rsp(1:max(1,length(a_rsp)))
                              if (a_rsp .ne. ' ') then
                                call getcolors(a_rsp,i_colnum,i_colors)
                                call settable(i_colnum,i_colors)
                              end if
                            else
                              write(a_msg,'(a)') 
     &                          'Enter Binary Colormap Filename: '
                              call get_dialog(a_msg,a_rsp)
                              a_rsp = a_rsp(1:max(1,length(a_rsp)))
                              if (a_rsp .ne. ' ') then
                                call getbcolors(a_rsp,i_colnum,i_colors)
                                call settable(i_colnum,i_colors)
                              end if
                            end if
                            if (r_hscale .eq. 0.) r_hscale = 100.
                          end if
                          call setcolor(6)
                        end if
                      else if (i_event(1) .eq. 5) then					! Button 4
c                            !
c                            ! Put button 4 code here
c                            !

                      else if (i_event(1) .eq. 6) then					! Button 5
                        if (i_event(2) .eq. 4) then
                          i_done = 1
                        end if
                      else if (i_event(1) .eq. i_zmw+1) then			! Button 6
                        r_zoom=min(r_zoom*2.,64.)

                        i_bcnt = i_bcnt + 1
                        if (i_bcnt .gt. I_BMAX) i_bcnt = I_BMAX
                        i_bdat(1,i_bcnt) = 1
                        i_bdat(2,i_bcnt) = 4
                        i_bdat(3,i_bcnt) = 2
                        i_bdat(4,i_bcnt) = ixx
                        i_bdat(5,i_bcnt) = iyy
                        if (i_wait .eq. 0) then
                          i_wait = 1
                          iy = 0
                          if (i_debug .ge. 4) write(6,*) 'turning wait off',i_wait
                        end if

                      else if (i_event(1) .eq. i_zmw+2) then			! Button 7
                        r_zoom=max(r_zoom/2.,0.015625)

                        i_bcnt = i_bcnt + 1
                        if (i_bcnt .gt. I_BMAX) i_bcnt = I_BMAX
                        i_bdat(1,i_bcnt) = 1
                        i_bdat(2,i_bcnt) = 4
                        i_bdat(3,i_bcnt) = 2
                        i_bdat(4,i_bcnt) = ixx
                        i_bdat(5,i_bcnt) = iyy
                        if (i_wait .eq. 0) then
                          i_wait = 1
                          iy = 0
                          if (i_debug .ge. 4) write(6,*) 'turning wait off',i_wait
                        end if

                      else if (i_event(1) .eq. i_zmw+3) then			! Button 8

                        if (i_event(3) .eq. 1) then  !  write out in JPEG format
                          i_fpline = max(min(max(iyy-int(i_vys(i_zmw)/(2.*r_zoom)),1),i_lines+i_fline-1),i_fline)
                          i_npline = min(i_nline,min(max(int(i_vys(i_zmw)/r_zoom),1),
     &                           i_lines+i_fline-1-(min(max(iyy-int(i_vys(i_zmw)/(2.*r_zoom)),1),i_lines+i_fline-1))))
                          i_fpsamp = min(max(ixx-int(i_vxs(i_zmw)/(2.*r_zoom)),1),i_samps)
                          i_npsamp = min(max(int(i_vxs(i_zmw)/r_zoom),1),
     &                           i_samps-(min(max(ixx-int(i_vxs(i_zmw)/(2.*r_zoom)),1),i_samps)))

                          if (i_ccc .eq. 1) then
                            i_mag=256
                            i_col=1
                          else if (i_ccc .eq. 2) then
                            i_mag=1
                            i_col=256
                          else if (i_ccc .eq.3) then
                            i_mag=16
                            i_col=16
                          else
                            stop 'Error in color table selection'
                          end if 

                          call mixcolor(i_colnum,i_colors,i_mag,i_col,i_ctable)

                          open(unit=30,file='out.ppm',status='unknown',form='unformatted',
     &                         access='direct',recl=i_npsamp*3,iostat=i_err)

                          if (i_err .ne. 0) then
                            write(6,*) ' '
                            write(6,*) 'Error Opening out.ppm   PPM file not created.  ',i_err
                            write(6,*)
                            i_err=0
                          else

                            if (i_debug .ge. 2) then
                              write(6,*) ' '
                              write(6,*) ' '
                              write(6,'(x,a)') 'Creating PPM File -- '
                            end if 
                            
                            a_out='P6'
                            b_out(3)=13
                            do i=4,3*i_npsamp
                              b_out(i)=32
                            end do
                            write(a_value,'(3i)') i_npsamp,i_npline,255
                            do i = 1,length(a_value)
                              b_out(3*i_npsamp-length(a_value)-1+i) = ichar(a_value(i:i))
                            end do
                            b_out(i_npsamp*3) = 13
                            write(30,rec=1) (b_out(i),i=1,i_npsamp*3)

                            i_skip = 1
                            do iy=1,i_npline
                              if (mod(iy,1000) .eq. 0 .and. i_debug .ge. 3) write(6,*) ' At line: ',iy
                              iyyy = iy*i_skip+i_fpline-i_skip
                              if (a_rmgfile .ne. ' ') then
                                call readrmg(i_rmgfile,i_fpsamp,iyyy,i_npsamp*i_skip+1,
     &                                 i_samps,i_lines+i_fpline-1,r_amp1,r_hgt1,i_flip,i_flg,i_err)
                              else if (a_cmpfile .ne. ' ') then
                                call readcmp(i_cmpfile,i_cmptype,i_fpsamp,iyyy,i_npsamp*i_skip+1,
     &                                 i_samps,i_lines+i_fpline-1,r_amp1,r_hgt1,i_flip,i_err)
                              else
                                call readdat(i_dtefile,i_dtetype,i_fpsamp,iyyy,i_doff,i_npsamp*i_skip+1,
     &                                i_samps,i_lines+i_fpline-1,r_dmul,r_dadd,r_hgt1,i_flip,0,i_err)
                                call readdat(i_magfile,i_magtype,i_fpsamp,iyyy,i_moff,i_npsamp*i_skip+1,
     &                                i_samps,i_lines+i_fpline-1,r_mmul,r_madd,r_amp1,i_flip,i_flg,i_err)
                              end if
                              do ix=1,i_npsamp
                                ixxx=ix*i_skip-i_skip+1
                                if (r_expnt .eq. 1) then
                                  r_dat1(ix) = max(min((0.5*((r_amp1(ixxx))-r_avg)/
     &                                    (r_ascale*r_std)+ i_scale*r_bias), 0.9999), 0.0)
                                else
                                  r_dat1(ix) = max(min((0.5*((sign(abs(r_amp1(ixxx))**r_expnt,r_amp1(ixxx)))-r_avg)/
     &                                    (r_ascale*r_std)+ i_scale*r_bias), 0.9999), 0.0)
                                end if
                                r_dat2(ix) = mod(abs(max(min(r_hgt1(ixxx),r_hmax),r_hmin)+100.*r_hscale)-
     &                               r_wstrt,r_hscale)/r_hscale                                
  
                              end do
                    
                              do ix=1,i_npsamp
                                if (i_ccc .eq. 1) then              ! Grey only
                                  b_out(ix*3-2) = i_ctable(1,int(r_dat1(ix)*255))
                                  b_out(ix*3-1) = i_ctable(2,int(r_dat1(ix)*255))
                                  b_out(ix*3-0) = i_ctable(3,int(r_dat1(ix)*255))
                                else if (i_ccc .eq. 2) then         ! Color only
                                  b_out(ix*3-2) = i_ctable(1,int(r_dat2(ix)*255))
                                  b_out(ix*3-1) = i_ctable(2,int(r_dat2(ix)*255))
                                  b_out(ix*3-0) = i_ctable(3,int(r_dat2(ix)*255))
                                else                                    ! Mix
  
                                  b_out(ix*3-2) = i_ctable(1,int(r_dat1(ix)*i_mag)*i_col + int(r_dat2(ix)*i_col))
                                  b_out(ix*3-1) = i_ctable(2,int(r_dat1(ix)*i_mag)*i_col + int(r_dat2(ix)*i_col))
                                  b_out(ix*3-0) = i_ctable(3,int(r_dat1(ix)*i_mag)*i_col + int(r_dat2(ix)*i_col))
                                end if
                              end do

                              write(30,rec=1+iy) (b_out(ix),ix=1,3*i_npsamp) 

                            end do

                            close(30)
                            if (i_debug .ge. 1) write(6,*) 'Done'
                            if (a_jpgcmnd .ne. ' ') then
                              if (i_debug .ge. 1) write(6,*) ' '
                              if (i_debug .ge. 1) write(6,'(x,a)') 'Converting to JPEG -- '
                              a_value=a_jpgcmnd(1:max(1,length(a_jpgcmnd)))//' out.ppm'
                              call system(a_value)
                              if (i_debug .ge. 1) write(6,*) 'Done '
                            endif
                          end if
                        else if (i_event(3) .eq. 2) then  !  write out in PPM format
                          i_fpline = max(min(max(iyy-int(i_vys(i_zmw)/(2.*r_zoom)),1),i_lines+i_fline-1),i_fline)
                          i_npline = min(i_nline,min(max(int(i_vys(i_zmw)/r_zoom),1),
     &                           i_lines+i_fline-1-(min(max(iyy-int(i_vys(i_zmw)/(2.*r_zoom)),1),i_lines+i_fline-1))))
                          i_fpsamp = min(max(ixx-int(i_vxs(i_zmw)/(2.*r_zoom)),1),i_samps)
                          i_npsamp = min(max(int(i_vxs(i_zmw)/r_zoom),1),
     &                           i_samps-(min(max(ixx-int(i_vxs(i_zmw)/(2.*r_zoom)),1),i_samps)))

                          if (i_ccc .eq. 1) then
                            i_mag=256
                            i_col=1
                          else if (i_ccc .eq. 2) then
                            i_mag=1
                            i_col=256
                          else if (i_ccc .eq.3) then
                            i_mag=16
                            i_col=16
                          else
                            stop 'Error in color table selection'
                          end if 

                          call mixcolor(i_colnum,i_colors,i_mag,i_col,i_ctable)

                          open(unit=30,file='out.ppm',status='unknown',form='unformatted',
     &                         access='direct',recl=i_npsamp*3,iostat=i_err)

                          if (i_err .ne. 0) then
                            write(6,*) ' '
                            write(6,*) 'Error Opening out.ppm   PPM file not created.  ',i_err
                            write(6,*)
                            i_err=0
                          else

                            if (i_debug .ge. 2) then
                              write(6,*) ' '
                              write(6,*) ' '
                              write(6,'(x,a)') 'Creating PPM File -- '
                            end if

                            a_out='P6'
                            b_out(3)=13
                            do i=4,3*i_npsamp
                              b_out(i)=32
                            end do
                            write(a_value,'(3i)') i_npsamp,i_npline,255
                            do i = 1,length(a_value)
                              b_out(3*i_npsamp-length(a_value)-1+i) = ichar(a_value(i:i))
                            end do
                            b_out(i_npsamp*3) = 13
                            write(30,rec=1) (b_out(i),i=1,i_npsamp*3)

                            i_skip = 1
                            do iy=1,i_npline
                              if (mod(iy,1000) .eq. 0 .and. i_debug .ge. 3) write(6,*) ' At line: ',iy
                              iyyy = iy*i_skip+i_fpline-i_skip
                              if (a_rmgfile .ne. ' ') then
                                call readrmg(i_rmgfile,i_fpsamp,iyyy,i_npsamp*i_skip+1,
     &                                 i_samps,i_lines+i_fpline-1,r_amp1,r_hgt1,i_flip,i_flg,i_err)
                              else if (a_cmpfile .ne. ' ') then
                                call readcmp(i_cmpfile,i_cmptype,i_fpsamp,iyyy,i_npsamp*i_skip+1,
     &                                 i_samps,i_lines+i_fpline-1,r_amp1,r_hgt1,i_flip,i_err)
                              else
                                call readdat(i_dtefile,i_dtetype,i_fpsamp,iyyy,i_doff,i_npsamp*i_skip+1,
     &                                i_samps,i_lines+i_fpline-1,r_dmul,r_dadd,r_hgt1,i_flip,0,i_err)
                                call readdat(i_magfile,i_magtype,i_fpsamp,iyyy,i_moff,i_npsamp*i_skip+1,
     &                                i_samps,i_lines+i_fpline-1,r_mmul,r_madd,r_amp1,i_flip,i_flg,i_err)
                              end if
                              do ix=1,i_npsamp
                                ixxx=ix*i_skip-i_skip+1
                                if (r_expnt .eq. 1) then
                                  r_dat1(ix) = max(min((0.5*((r_amp1(ixxx))-r_avg)/
     &                                    (r_ascale*r_std)+ i_scale*r_bias), 0.9999), 0.0)
                                else
                                  r_dat1(ix) = max(min((0.5*((sign(abs(r_amp1(ixxx))**r_expnt,r_amp1(ixxx)))-r_avg)/
     &                                    (r_ascale*r_std)+ i_scale*r_bias), 0.9999), 0.0)
                                end if
                                r_dat2(ix) = mod(abs(max(min(r_hgt1(ixxx),r_hmax),r_hmin)+100.*r_hscale)-
     &                                     r_wstrt,r_hscale)/r_hscale                                
  
                              end do
                
                              do ix=1,i_npsamp
                                if (i_ccc .eq. 1) then              ! Grey only
                                  b_out(ix*3-2) = i_ctable(1,int(r_dat1(ix)*255))
                                  b_out(ix*3-1) = i_ctable(2,int(r_dat1(ix)*255))
                                  b_out(ix*3-0) = i_ctable(3,int(r_dat1(ix)*255))
                                else if (i_ccc .eq. 2) then         ! Color only
                                  b_out(ix*3-2) = i_ctable(1,int(r_dat2(ix)*255))
                                  b_out(ix*3-1) = i_ctable(2,int(r_dat2(ix)*255))
                                  b_out(ix*3-0) = i_ctable(3,int(r_dat2(ix)*255))
                                else                                    ! Mix

                                  b_out(ix*3-2) = i_ctable(1,int(r_dat1(ix)*i_mag)*i_col + int(r_dat2(ix)*i_col))
                                  b_out(ix*3-1) = i_ctable(2,int(r_dat1(ix)*i_mag)*i_col + int(r_dat2(ix)*i_col))
                                  b_out(ix*3-0) = i_ctable(3,int(r_dat1(ix)*i_mag)*i_col + int(r_dat2(ix)*i_col))
                                end if
                              end do

                              write(30,rec=1+iy) (b_out(ix),ix=1,3*i_npsamp) 

                            end do

                            close(30)
                            if (i_debug .ge. 3) write(6,*) 'Done'
                            if (i_debug .ge. 3) write(6,*) ' '
                          end if
                        else ! use postscript format

         i_fpline = max(min(max(iyy-int(i_vys(i_zmw)/(2.*r_zoom)),1),i_lines+i_fline-1),i_fline)
         i_npline = min(i_nline,min(max(int(i_vys(i_zmw)/r_zoom),1),
     &                           i_lines+i_fline-1-(min(max(iyy-int(i_vys(i_zmw)/(2.*r_zoom)),1),i_lines+i_fline-1))))
         i_fpsamp = min(max(ixx-int(i_vxs(i_zmw)/(2.*r_zoom)),1),i_samps)
         i_npsamp = min(max(int(i_vxs(i_zmw)/r_zoom),1),
     &                           i_samps-(min(max(ixx-int(i_vxs(i_zmw)/(2.*r_zoom)),1),i_samps)))

         if (i_ccc .eq. 1) then
           i_mag=256
           i_col=1
         else if (i_ccc .eq. 2) then
           i_mag=1
           i_col=256
         else if (i_ccc .eq.3) then
           i_mag=16
           i_col=16
         else
           stop 'Error in color table selection'
         end if 

         call mixcolor(i_colnum,i_colors,i_mag,i_col,i_ctable)

         if (i_npsamp .gt. i_npline) then
           i_orient=1  ! landscape
         else
           i_orient=2  ! portrait 
         end if

         if (i_orient .eq. 1) then
           i_skip=max(max(i_npsamp/(11*i_dpi),i_npline/(8*i_dpi)),1)
           i_npsamp=(i_npsamp/i_skip)+1
           i_npline=(i_npline/i_skip)+1
           r_aspect=min(25400./i_npsamp,17780./i_npline)
           i_xmax = int(r_aspect*i_npsamp)
           i_ymax = int(r_aspect*i_npline)
         else
           i_skip=max(max(i_npsamp/(6*i_dpi),i_npline/(11*i_dpi)),1)
           i_npsamp=(i_npsamp/i_skip)+1
           i_npline=(i_npline/i_skip)+1
           r_aspect=min(19050./i_npsamp,24130./i_npline)
           i_xmax = int(r_aspect*i_npsamp)
           i_ymax = int(r_aspect*i_npline)
         end if
         if (i_debug .ge. 3) write(6,*) 'i_skip = ',i_skip,i_npsamp,i_npline
         open(unit=30,file='out.ps',status='unknown',form='formatted',iostat=i_err)

         if (i_err .ne. 0) then
           write(6,*) ' '
           write(6,*) 'Error Opening out.ps   PostScript file not created.  ',i_err
           write(6,*)
           i_err=0
         else

         if (i_debug .ge. 2) then
           write(6,*) ' '
           write(6,*) ' '
           write(6,'(a)') 'Writing Postscript file: out.ps'
         end if

         call write_ps_hdr(30,i_orient,i_xmax,i_ymax,i_npsamp,i_npline,i_ctable)

                          do iy=i_npline,1,-1 
                            if (mod(iy,1000) .eq. 0 .and. i_debug .ge. 3) write(6,*) ' At line: ',iy
                            iyyy = iy*i_skip+i_fpline-i_skip
                            if (a_rmgfile .ne. ' ') then
                              call readrmg(i_rmgfile,i_fpsamp,iyyy,i_npsamp*i_skip+1,
     &                               i_samps,i_lines+i_fpline-1,r_amp1,r_hgt1,i_flip,i_flg,i_err)
                            else if (a_cmpfile .ne. ' ') then
                              call readcmp(i_cmpfile,i_cmptype,i_fpsamp,iyyy,i_npsamp*i_skip+1,
     &                               i_samps,i_lines+i_fpline-1,r_amp1,r_hgt1,i_flip,i_err)
                            else
                              call readdat(i_dtefile,i_dtetype,i_fpsamp,iyyy,i_doff,i_npsamp*i_skip+1,
     &                              i_samps,i_lines+i_fpline-1,r_dmul,r_dadd,r_hgt1,i_flip,0,i_err)
                              call readdat(i_magfile,i_magtype,i_fpsamp,iyyy,i_moff,i_npsamp*i_skip+1,
     &                              i_samps,i_lines+i_fpline-1,r_mmul,r_madd,r_amp1,i_flip,i_flg,i_err)
                            end if
                            do ix=1,i_npsamp
                              ixxx=ix*i_skip-i_skip+1
                              if (r_expnt .eq. 1) then
                                r_dat1(ix) = max(min((0.5*((r_amp1(ixxx))-r_avg)/
     &                                  (r_ascale*r_std)+ i_scale*r_bias), 0.9999), 0.0)
                              else
                                r_dat1(ix) = max(min((0.5*((sign(abs(r_amp1(ixxx))**r_expnt,r_amp1(ixxx)))-r_avg)/
     &                                  (r_ascale*r_std)+ i_scale*r_bias), 0.9999), 0.0)
                              end if
                              r_dat2(ix) = mod(abs(max(min(r_hgt1(ixxx),r_hmax),r_hmin)+100.*r_hscale)-
     &                                  r_wstrt,r_hscale)/r_hscale                                

                            end do
                  
                            do ix=1,i_npsamp
                              if (i_ccc .eq. 1) then              ! Grey only
                                i_out(ix) = int(r_dat1(ix)*255)
                              else if (i_ccc .eq. 2) then         ! Color only
                                i_out(ix) = int(r_dat2(ix)*255)
                              else                                    ! Mix

                                i_out(ix) = int(r_dat1(ix)*i_mag)*i_col + int(r_dat2(ix)*i_col)
                              end if
                            end do

                            write(30,'(40z2.2)') (i_out(ix),ix=1,i_npsamp) 

                          end do

          write(30,'(a)') 'grestore '

          if (r_zoom .ge. 1.) then
            write(a_value,'(a,a,i5,a,i5,i4,a)') 
     &         a_filename(1:length(a_filename)),' x=',ixxx,'  y=',iyyy,nint(r_zoom),'X'
          else
            write(a_value,'(a,a,i5,a,i5,i4,a)') 
     &         a_filename(1:length(a_filename)),' x=',ixxx,'  y=',iyyy,-nint(1./r_zoom),'X'
          end if

          if (i_orient .eq. 1) then
            write(30,'(a)') '0.000 0.000 0.000 setrgbcolor gsave 2000 0 translate 0 0 moveto'
            write(30,'(a)') '2 dup scale -929.428 0 rmoveto'
            write(30,'(a)') '('//a_value(1:length(a_value))//') show grestore'
          else
            write(30,'(a)') '0.000 0.000 0.000 setrgbcolor gsave 2000 0 translate 0 0 moveto'
            write(30,'(a)') '2 dup scale -929.428 0 rmoveto'
            write(30,'(a)') '('//a_value(1:length(a_value))//') show grestore'
          end if
          write(30,'(a)') '%%PageTrailer'
          write(30,'(a)') 'showpage'
          write(30,'(a)') '%%PageResources: font Helvetica'
          write(30,'(a)') '%%Trailer'
          write(30,'(a)') 'restore'
          write(30,'(a)') '%%Pages: 1'
          write(30,'(a)') '%%DocumentNeededResources: font Helvetica'
          write(30,'(a)') '%%EOF'

          close(30)
          if (i_debug .ge. 3) write(6,'(a)') 'Done'
          if (i_debug .ge. 3) write(6,*) ' '
          if (i_key .ne. 0 .and. a_prtcmnd .ne. ' ') then
            a_value = a_prtcmnd(1:max(1,length(a_prtcmnd)))//' out.ps &'
            call system(a_value)
          endif

          end if
        end if
 
                      end if

                    else if (i_event(2) .eq. 5) then                    ! button Release
                      i_button = 0
                      i_wb=0
                      if (i_event(1) .eq. 0) then
                         ! do nothing
                      else if (i_event(1) .eq. 1 .or. i_event(1) .eq. i_zmw) then	! Click in window 1 or i_zmw
                        i_wb=i_event(1)
                        i_bcnt = i_bcnt + 1										! buffer click for later
                        if (i_bcnt .gt. I_BMAX) i_bcnt = I_BMAX
                        do i = 1,10
                          i_bdat(i,i_bcnt) = i_event(i)
                        end do
                        if (i_wait .eq. 0) then
                          i_wait = 1
                          iy = 0
                          if (i_debug .ge. 4) write(6,*) 'turning wait off',i_wait
                        end if
                      end if
                    else if (i_event(2) .eq. 6) then                    ! Key Press
                      i_key = i_event(3)
                      i_asc = i_event(6)
                      if (i_asc .eq. ichar('p') .or. i_asc .eq. ichar('P')) then
                        if (i_epath .eq. 0) then
                          i_epath = 1
                          if (i_debug .ge. 3) write(6,*) 'Turning on path'
                        else
                          i_epath = 0
                          if (i_debug .ge. 3) write(6,*) 'Turning off path'
                        end if
                      end if
                      if (i_asc .eq. ichar('e') .or. i_asc .eq. ichar('E')) then
                        if (i_eline .eq. 0) then
                          i_eline = 1
                          if (i_debug .ge. 3) write(6,*) 'Turning on eline'
                        else
                          i_eline = 0
                          if (i_debug .ge. 3) write(6,*) 'Turning off eline'
                        end if
                      end if
                    else if (i_event(2) .eq. 7) then                    ! Key Release
                      i_key = 0
                      i_asc = 0
                    else if (i_event(2) .eq. 8) then                    ! Destroy Window event
                      if (i_event(1) .eq. i_zmw ) then
                        if (i_debug .ge. 4) write(6,*) 'closing zoom window'
                        r_zoom=0.
                      else if (i_event(1) .eq. 1) then
                        stop 'Done'
                      end if
                    else if (i_event(2) .eq. 9) then                    ! Mouse motion
                      if (i_button .eq. 1) then     
                        i_bcnt = i_bcnt + 1										! buffer click for later
                        if (i_bcnt .gt. I_BMAX) i_bcnt = I_BMAX
                        do i = 1,10
                          i_bdat(i,i_bcnt) = i_event(i)
                        end do
                        i_bdat(2,1)=4
                        if (i_wait .eq. 0) then
                          i_wait = 1
                          iy = 0
                          if (i_debug .ge. 4) write(6,*) 'turning wait off',i_wait
                        end if
                      end if
                    end if

                  end do

                  if (i_bcnt .gt. 0) then
                    if (i_bdat(1,1) .eq. 0) then							! Window 0
                      if (i_debug .ge. 1) stop 'Error - Got event for window number 0'

                    else if (i_bdat(1,1) .eq. 1) then						! Window 1
                      if (i_bdat(2,1) .eq. 0) then
                        if (i_debug .ge. 1) stop 'Error - event number = 0'

                      else if (i_bdat(2,1) .eq. 1) then			! expose
                        if ((i_bdat(4,1) .gt. i_vyo(1) + i_vys(1) .or.
     &                      i_bdat(4,1) + i_bdat(6,1) .lt. i_vyo(1) ) .and. i_mem .eq. 0) then
                          iy = i_bdat(6,1) + 1
                          if (i_debug .ge. 4) write(6,*) 'aborting expose'
                        else if ((i_bdat(3,1) .gt. i_vxo(1) + i_vxs(1) .or.
     &                      i_bdat(3,1) + i_bdat(5,1) .lt. i_vxo(1) ) .and. i_mem .eq. 0) then
                          iy = i_bdat(6,1) + 1
                          if (i_debug .ge. 4) write(6,*) 'aborting expose'
                        end if
                        if (iy .lt. i_bdat(6,1)) then
                          iy = iy + 1
                          if ((i_bdat(4,1)+iy .ge. i_vyo(1) .and. i_bdat(4,1)+iy .le. i_vyo(1)+i_vys(1))
     &                         .or. i_mem .ne. 0) then
                            if (a_rmgfile .ne. ' ') then
                              call readrmg(i_rmgfile,i_bdat(3,1) + 1,i_bdat(4,1)+iy+i_fline-1,i_bdat(5,1),
     &                               i_samps,i_lines+i_fline-1,r_amp1,r_hgt1,i_flip,i_flg,i_err)
                            else if (a_cmpfile .ne. ' ') then
                              call readcmp(i_cmpfile,i_cmptype,i_bdat(3,1) + 1,i_bdat(4,1)+iy+i_fline-1,i_bdat(5,1),
     &                               i_samps,i_lines+i_fline-1,r_amp1,r_hgt1,i_flip,i_err)
                            else
                              call readdat(i_dtefile,i_dtetype,i_bdat(3,1) + 1,i_bdat(4,1)+iy+i_fline-1,i_doff, 
     &                            max(i_bdat(5,1),2),i_samps,i_lines+i_fline-1,r_dmul,r_dadd,r_hgt1,i_flip,0,i_err)
                              call readdat(i_magfile,i_magtype,i_bdat(3,1) + 1,i_bdat(4,1)+iy+i_fline-1,i_moff, 
     &                            max(i_bdat(5,1),2),i_samps,i_lines+i_fline-1,r_mmul,r_madd,r_amp1,i_flip,i_flg,i_err)
                            end if
                            do ix=1,i_bdat(5,1)
                              if (r_expnt .eq. 1) then
                                r_dat1(ix+(iy-1)*i_bdat(5,1)) = max(min((0.5*((r_amp1(ix))-r_avg)/
     &                                  (r_ascale*r_std)+ i_scale*r_bias), 0.9999), 0.0)
                              else
                                r_dat1(ix+(iy-1)*i_bdat(5,1)) = 
     &                                  max(min((0.5*(( sign(abs(r_amp1(ix))**r_expnt,r_amp1(ix)))-r_avg)/
     &                                  (r_ascale*r_std)+ i_scale*r_bias), 0.9999), 0.0)
                              end if
                              r_dat2(ix+(iy-1)*i_bdat(5,1)) = mod(abs(max(min(r_hgt1(ix),r_hmax),r_hmin)+100.*r_hscale)-
     &                               r_wstrt,r_hscale)/r_hscale
                            end do
                          end if
                        else if (iy .eq. i_bdat(6,1)) then
c                          type '(x,a,6i10)',
c     &                     'display=',i_bdat(1,1),i_bdat(2,1),i_bdat(3,1),i_bdat(4,1),i_bdat(5,1),i_bdat(6,1)
                          if ((a_rmgfile .ne. ' ') .or. (a_cmpfile .ne. ' ') .or.
     &                        (i_magtype .ne. 0 .and. i_dtetype .ne. 0) ) then
                            if (i_colr .eq. 0 .or. i_colr .eq. 3) then
                              CALL display_rmg(1,  i_bdat(3,1), i_bdat(4,1), 
     &                              i_bdat(5,1), i_bdat(6,1), i_bdat(5,1), r_dat1, r_dat2)
                            else if (i_colr .eq. 1) then
                              CALL display_img(1,  i_bdat(3,1), i_bdat(4,1), 
     &                              i_bdat(5,1), i_bdat(6,1), i_bdat(5,1), r_dat1)
                            else if (i_colr .eq. 2) then
                              CALL display_img(1,  i_bdat(3,1), i_bdat(4,1), 
     &                              i_bdat(5,1), i_bdat(6,1), i_bdat(5,1), r_dat2)
                            end if
                          else
                            if (a_dtefile .ne. ' ') then
                              if (i_colr .eq. 0) then
                                CALL display_rmg(1,  i_bdat(3,1), i_bdat(4,1), 
     &                                i_bdat(5,1), i_bdat(6,1), i_bdat(5,1), r_dat2, r_dat2)
                              else
                                CALL display_img(1,  i_bdat(3,1), i_bdat(4,1), 
     &                                i_bdat(5,1), i_bdat(6,1), i_bdat(5,1), r_dat2)
                              end if
                            else
                              if (i_colr .eq. 0) then
                                CALL display_rmg(1,  i_bdat(3,1), i_bdat(4,1), 
     &                                i_bdat(5,1), i_bdat(6,1), i_bdat(5,1), r_dat1, r_dat1)
                              else
                                CALL display_img(1,  i_bdat(3,1), i_bdat(4,1), 
     &                                i_bdat(5,1), i_bdat(6,1), i_bdat(5,1), r_dat1)
                              end if
                            end if
                          end if
                          call shiftexp(i_bdat,i_bcnt,i_vxo,i_vyo,i_vxs,i_vys,i_mem,i_zmw,i_wsamps)
                          iy = 0
                        else
                          call shiftexp(i_bdat,i_bcnt,i_vxo,i_vyo,i_vxs,i_vys,i_mem,i_zmw,i_wsamps)
                          iy = 0
                        end if

                      else if (i_bdat(2,1) .eq. 4) then			! Button Press
                        iy = 0
                        ixxx=i_bdat(4,1)+1
                        iyyy=i_bdat(5,1)+1
                        if (a_rmgfile .ne. ' ') then
                          call readrmg(i_rmgfile,ixxx-1,iyyy+i_fline-1,3,i_samps,i_lines+i_fline-1,
     &                          r_amp1,r_hgt1,i_flip,i_flg,i_err)
                        else if (a_cmpfile .ne. ' ') then
                          call readcmp(i_cmpfile,i_cmptype,ixxx-1,iyyy+i_fline-1,3,i_samps,i_lines+i_fline-1,
     &                          r_amp1,r_hgt1,i_flip,i_err)
                        else
                          call readdat(i_dtefile,i_dtetype,ixxx-1,iyyy+i_fline-1,i_doff,3,i_samps,i_lines+i_fline-1,
     &                          r_dmul,r_dadd,r_hgt1,i_flip,0,i_err)
                          call readdat(i_magfile,i_magtype,ixxx-1,iyyy+i_fline-1,i_moff,3,i_samps,i_lines+i_fline-1,
     &                          r_mmul,r_madd,r_amp1,i_flip,i_flg,i_err)
                        end if

                        r_amp = r_amp1(2)
                        r_hgt = r_hgt1(2)
                        if (r_spc(1) .ne. 0. .and. i_asc .ne. 0) then

                          if (i_flip .eq. 0) then
                            v_loc1(1) = (iyyy+i_fline-1)*r_spc(1)+r_str(1)
                            v_loc1(2) = ixxx*r_spc(2)+r_str(2)
                          else
                            v_loc1(1) = (iyyy+i_fline-1)*r_spc(1)+r_str(1)
                            v_loc1(2) = (i_samps-ixxx+1)*r_spc(2)+r_str(2)
                          end if
                          v_loc1(3) = r_hgt

                          if (a_type .eq. 'sch' .or. a_type .eq. 'SCH') then
                            r_lon=v_loc1(1)/r_rad
                            r_lat=v_loc1(2)/r_rad
                            r_hhh=v_loc1(3)
                            call sch_to_tcn(r_rad,v_loc2,r_lat,r_lon,r_hhh,1)
                            call vecmulti(r_atm,v_loc2,v_loc3) ! convert from input xyz to output xyz
                            call vecaddit(r_atm(1,4),v_loc3,v_loc3)
                            call latlon(r_a,r_e2,v_loc3,r_lat,r_lon,r_hhh,2)
                          else if (a_type .eq. 'eqa' .or. a_type .eq. 'EQA') then
                            r_lat=v_loc1(1)/r_rtod
                            r_lon=v_loc1(2)/r_rtod
                            r_hhh=v_loc1(3)
                          else if (a_type .eq. 'utm' .or. a_type .eq. 'UTM') then
                            call utmtoll(r_a,r_e2,i_zone,a_grid,v_loc1,r_lat,r_lon,1)
                            r_hhh=v_loc1(3)
                          else if (a_type .eq. 'neu' .or. a_type .eq. 'NEU') then
                            call utmtoll(r_a,r_e2,i_zone,a_grid,v_loc1,r_lat,r_lon,1)
                            r_hhh=v_loc1(3)
                          else if (a_type .eq. 'enu' .or. a_type .eq. 'ENU') then
                            call enutoll(r_a,r_e2,i_zone,a_grid,v_loc1,r_lat,r_lon,1)
                            r_hhh=v_loc1(3)
                          else
                            write(6,*) 'Lat/Long output not supported for ',a_type(1:max(1,length(a_type)))
                          end if
                          write(a_string,'(x,f10.4,f10.4,f10.2,e14.4)') r_lat*r_rtod,r_lon*r_rtod,r_hhh,r_amp

                        else
                          if (i_flip .eq. 0) then
                            write(a_string,'(x,i6,i7,f10.2,e14.4)') ixxx,iyyy+i_fline-1,r_hgt,r_amp
                          else
                            write(a_string,'(x,i6,i7,f10.2,e14.4)') i_samps-ixxx+1,iyyy+i_fline-1,r_hgt,r_amp
                          end if
                        end if
                        if (i_bdat(3,1) .eq. 3) write(6,'(a)') a_string(1:max(1,length(a_string)))//'  '//
     &                            a_filename(1:length(a_filename))
                        if (i_path .eq. i_maxsamps-1 .and. i_debug .ge. 1) write(6,*) 'Warning -- path buffer full.  ',i_maxsamps
                        i_path = min(i_path+1,i_maxsamps)
                        i_xpath(i_path) = ixxx
                        i_ypath(i_path) = iyyy
                        r_apath(i_path) = r_amp
                        r_hpath(i_path) = r_hgt
                        call display_label(1,a_string)
                        if (i_bdat(3,1) .eq. 2 .and. r_zmstrt .ne. 0) then
                          iyy = iyyy - 1
                          ixx = ixxx - 1
                          if (r_zoom .eq. 0.) then
                            i_gxi = 4
                            i_type(1) = 3
                            i_type(2) = 1
                            i_type(3) = 1
                            i_type(4) = 1

                            a_labl(0) = a_filename
                            a_labl(1) = 'Zoom Window'
                            a_labl(2) = 'In'
                            a_labl(3) = 'Out'
                            a_labl(4) = 'Print'

                            i_wxs(0) = 200
                            i_wys(0) = 275
                            i_wxs(1) = 200
                            i_wys(1) = 200
                            i_wxs(2) = 50
                            i_wys(2) = 50
                            i_wxs(3) = 50
                            i_wys(3) = 50
                            i_wxs(4) = 50
                            i_wys(4) = 50

                            i_frx(0) = 4
                            i_frx(1) = 4
                            i_frx(2) = 1
                            i_frx(3) = 1
                            i_frx(4) = 2

                            i_fry(0) = 0
                            i_fry(1) = 400
                            i_fry(2) = -40
                            i_fry(3) = -40
                            i_fry(4) = -40

                            CALL init_gx(i_gxi,i_type,a_labl,i_wxs,i_wys,i_frx,i_fry,i_clrs,i_debug)
                            i_zmw = i_gxi-3
                            i_zmcnt = i_zmcnt + 1

                            call get_wininfo(i_zmw,i_vx,i_vy,i_vw,i_vh,i_cw,i_ch,i_widget) 
                            i_vxs(i_zmw)=i_vw
                            i_vys(i_zmw)=i_vh   
                            r_zoom = r_zmstrt
                          end if
                          
                          i_bcnt = i_bcnt + 1
                          if (i_bcnt .gt. I_BMAX) i_bcnt = I_BMAX
                          i_bdat(1,i_bcnt) = i_zmw
                          i_bdat(2,i_bcnt) = 1
                          i_bdat(3,i_bcnt) = ixx-1
                          i_bdat(4,i_bcnt) = iyy-1
                          i_bdat(5,i_bcnt) = 1
                          i_bdat(6,i_bcnt) = 1
                          if (i_wait .eq. 0) then
                            i_wait = 1
                            iy = 0
                            if (i_debug .ge. 4) write(6,*) 'turning wait off',i_wait
                          end if

                          if (r_zoom .ge. 1.) then
                            write(a_string,'(i2,a)') nint(r_zoom),':1  Zoom'
                          else
                            write(a_string,'(a,i2,a)') '1:',nint(1./r_zoom),'  Zoom'
                          end if
                          call display_label(i_zmw,a_string)

                        end if

                        iy = 0
                        call shiftexp(i_bdat,i_bcnt,i_vxo,i_vyo,i_vxs,i_vys,i_mem,i_zmw,i_wsamps)

                      else if (i_bdat(2,1) .eq. 5) then			! mouse release
                        if (i_path .ge. 2) then
                          if (i_epath .eq. 1) then
                            if (i_debug .ge. 3) write(6,*) ' '
                            if (i_debug .ge. 3) write(6,'(x,a,i)') 'Displaying height along path',i_path
                            open(unit=33,file='path.dat',status='unknown',form='formatted')
                            r_dist=0.
                            write(33,*) r_dist,r_hpath(1),r_apath(1)
                            do i=2,i_path
                              i_strt=max(i-1,1)
                              do while(i_strt.gt.1 .and. 
     &                              sqrt((1.0*(i_xpath(i_strt)-i_xpath(i)))**2 + 
     &                                   (1.0*(i_ypath(i_strt)-i_ypath(i)))**2) .lt. r_curve/2.)
                                i_strt=i_strt-1
                              end do
                              i_stop=min(i+1,i_path)
                              do while(i_stop.lt.i_path .and. 
     &                           sqrt((1.0*(i_xpath(i_stop)-i_xpath(i_strt)))**2 + 
     &                                (1.0*(i_ypath(i_stop)-i_ypath(i_strt)))**2) .lt. r_curve/2.)
                                i_stop=i_stop+1
                              end do
                              r_ddd = sqrt((r_xs*(i_xpath(i_stop)-i_xpath(i_strt)))**2 + 
     &                                     (r_ys*(i_ypath(i_stop)-i_ypath(i_strt)))**2)
                              if (i_debug .ge. 5) write(6,*) 'r_ddd=',r_ddd,i_strt,i,i_stop
                              if (r_ddd .ne. 0.) then
                                r_ccc = ((i_xpath(i)-i_xpath(i-1))*(i_xpath(i_stop)-i_xpath(i_strt))*r_xs**2 + 
     &                                   (i_ypath(i)-i_ypath(i-1))*(i_ypath(i_stop)-i_ypath(i_strt))*r_ys**2 ) / r_ddd
                                r_dist = r_dist + r_ccc
                              endif
                              write(33,*) r_dist,r_hpath(i),r_apath(i)
                            end do
                            close(33)
                            if (a_pltcmnd .ne. ' ') then
                              a_value=a_pltcmnd(1:max(1,length(a_pltcmnd)))//' path.dat &'
                              call system(a_value)
                            endif
                            write(6,*) ' '
                          end if
                          if (i_eline .eq. 1) then
                            i_x1 = i_xpath(1)
                            i_y1 = i_ypath(1)
                            i_x2 = i_xpath(i_path)
                            i_y2 = i_ypath(i_path)

          if (i_ccc .eq. 1 .or. i_ccc .eq. 3) then
            open(unit=31,file='dxl1.out',status='unknown',form='formatted')

            r_dist = sqrt(float(i_x2-i_x1)**2 + float(i_y2-i_y1)**2)
            do r_ddd = 0., r_dist, 1
              i_cnt = 0
              r_sum = 0.
              r_sqr = 0.
              do r_ccc = - i_avg/2., i_avg/2.,1
                        ixx = i_x1 + nint((i_x2-i_x1)*r_ddd/r_dist)
                        iyy = i_y1 + nint((i_y2-i_y1)*r_ddd/r_dist)
                        ixxx = ixx + nint((i_y2-i_y1)*r_ccc/r_dist)
                        iyyy = iyy + nint((i_x2-i_x1)*r_ccc/r_dist)
                        if (a_rmgfile .ne. ' ') then
                          call readrmg(i_rmgfile,ixxx-1,iyyy+i_fline-1,3,i_samps,i_lines+i_fline-1,
     &                          r_amp1,r_hgt1,i_flip,i_flg,i_err)
                        else if (a_cmpfile .ne. ' ') then
                          call readcmp(i_cmpfile,i_cmptype,ixxx-1,iyyy+i_fline-1,3,i_samps,i_lines+i_fline-1,
     &                          r_amp1,r_hgt1,i_flip,i_err)
                        else
                          call readdat(i_dtefile,i_dtetype,ixxx-1,iyyy+i_fline-1,i_doff,3,i_samps,i_lines+i_fline-1,
     &                          r_dmul,r_dadd,r_hgt1,i_flip,0,i_err)
                          call readdat(i_magfile,i_magtype,ixxx-1,iyyy+i_fline-1,i_moff,3,i_samps,i_lines+i_fline-1,
     &                          r_mmul,r_madd,r_amp1,i_flip,i_flg,i_err)
                        end if

                        r_amp = r_amp1(2)
                        r_hgt = r_hgt1(2)
                        if (r_spc(1) .ne. 0. .and. r_spc(2) .ne. 0) then

                          if (i_flip .eq. 0) then
                            v_loc1(1) = (iyyy+i_fline-1)*r_spc(1)+r_str(1)
                            v_loc1(2) = ixxx*r_spc(2)+r_str(2)
                          else
                            v_loc1(1) = (iyyy+i_fline-1)*r_spc(1)+r_str(1)
                            v_loc1(2) = (i_samps-ixxx+1)*r_spc(2)+r_str(2)
                          end if
                          v_loc1(3) = r_hgt

                          if (a_type .eq. 'sch' .or. a_type .eq. 'SCH') then
                            r_lon=v_loc1(1)/r_rad
                            r_lat=v_loc1(2)/r_rad
                            r_hhh=v_loc1(3)
                            call sch_to_tcn(r_rad,v_loc2,r_lat,r_lon,r_hhh,1)
                            call vecmulti(r_atm,v_loc2,v_loc3) ! convert from input xyz to output xyz
                            call vecaddit(r_atm(1,4),v_loc3,v_loc3)
                            call latlon(r_a,r_e2,v_loc3,r_lat,r_lon,r_hhh,2)
                          else if (a_type .eq. 'eqa' .or. a_type .eq. 'EQA') then
                            r_lat=v_loc1(1)/r_rtod
                            r_lon=v_loc1(2)/r_rtod
                            r_hhh=v_loc1(3)
                          else if (a_type .eq. 'utm' .or. a_type .eq. 'UTM') then
                            call utmtoll(r_a,r_e2,i_zone,a_grid,v_loc1,r_lat,r_lon,1)
                            r_hhh=v_loc1(3)
                          else if (a_type .eq. 'neu' .or. a_type .eq. 'NEU') then
                            call utmtoll(r_a,r_e2,i_zone,a_grid,v_loc1,r_lat,r_lon,1)
                            r_hhh=v_loc1(3)
                          else if (a_type .eq. 'enu' .or. a_type .eq. 'ENU') then
                            call enutoll(r_a,r_e2,i_zone,a_grid,v_loc1,r_lat,r_lon,1)
                            r_hhh=v_loc1(3)
                          end if
                          write(a_string,'(x,f10.4,f10.4,f10.2,e14.4)') r_lat*r_rtod,r_lon*r_rtod,r_hhh,r_amp
                        else
                          if (i_flip .eq. 0) then
                            write(a_string,'(x,i6,i7,f10.2,e14.4)') ixxx,iyyy+i_fline-1,r_hgt,r_amp
                          else
                            write(a_string,'(x,i6,i7,f10.2,e14.4)') i_samps-ixxx+1,iyyy+i_fline-1,r_hgt,r_amp
                          end if
                        end if
                        if (r_amp .ne. 0.) then
                          i_cnt = i_cnt+1
                          r_sum = r_sum + (dble(r_amp))
                          r_sqr = r_sqr + (dble(r_amp))**2
                        end if
                      end do
                      if (i_cnt .gt. 0) then
                        r_avge = r_sum/max(i_cnt,1)
                        if (i_cnt .gt. 1) then
                          r_stde = sqrt(max(1.d-25,(r_sqr/max(i_cnt,1))-r_avge**2))
                        else
                          r_stde = 0.
                        end if
                        if (r_spc(1) .ne. 0. .and. r_spc(2) .ne. 0.) then
                          write(31,'(x,5f)') sqrt(((ixx-i_x1)*r_spc(1))**2 + ((iyy-i_y1)*r_spc(2))**2), 
     &                       r_avge, r_stde, sngl(r_lat*r_rtod), sngl(r_lon*r_rtod)
                        else
                          write(31,'(x,3f)') r_ddd,r_avge, r_stde
                        end if
                      end if
                    end do
                    close(31)
                    if (a_pltcmnd .ne. ' ') then
                      a_value=a_pltcmnd(1:max(1,length(a_pltcmnd)))//' dxl1.out &'
                      call system(a_value)
                    endif
                  end if

                  if (i_ccc .eq. 2 .or. i_ccc .eq. 3) then
                    open(unit=32,file='dxl2.out',status='unknown',form='formatted')

                    r_dist = sqrt(float(i_x2-i_x1)**2 + float(i_y2-i_y1)**2)
                    do r_ddd = 0., r_dist, 1
                      i_cnt = 0
                      r_sum = 0.
                      r_sqr = 0.
                      do r_ccc = - i_avg/2., i_avg/2.,1
                        ixx = i_x1 + nint((i_x2-i_x1)*r_ddd/r_dist)
                        iyy = i_y1 + nint((i_y2-i_y1)*r_ddd/r_dist)
                        ixxx = ixx + nint((i_y2-i_y1)*r_ccc/r_dist)
                        iyyy = iyy + nint((i_x2-i_x1)*r_ccc/r_dist)
                        if (a_rmgfile .ne. ' ') then
                          call readrmg(i_rmgfile,ixxx-1,iyyy+i_fline-1,3,i_samps,i_lines+i_fline-1,
     &                          r_amp1,r_hgt1,i_flip,i_flg,i_err)
                        else if (a_cmpfile .ne. ' ') then
                          call readcmp(i_cmpfile,i_cmptype,ixxx-1,iyyy+i_fline-1,3,i_samps,i_lines+i_fline-1,
     &                          r_amp1,r_hgt1,i_flip,i_err)
                        else
                          call readdat(i_dtefile,i_dtetype,ixxx-1,iyyy+i_fline-1,i_doff,3,i_samps,i_lines+i_fline-1,
     &                          r_dmul,r_dadd,r_hgt1,i_flip,0,i_err)
                          call readdat(i_magfile,i_magtype,ixxx-1,iyyy+i_fline-1,i_moff,3,i_samps,i_lines+i_fline-1,
     &                          r_mmul,r_madd,r_amp1,i_flip,i_flg,i_err)
                        end if

                        r_amp = r_amp1(2)
                        r_hgt = r_hgt1(2)
                        if (r_spc(1) .ne. 0. .and. r_spc(2) .ne. 0) then

                          if (i_flip .eq. 0) then
                            v_loc1(1) = (iyyy+i_fline-1)*r_spc(1)+r_str(1)
                            v_loc1(2) = ixxx*r_spc(2)+r_str(2)
                          else
                            v_loc1(1) = (iyyy+i_fline-1)*r_spc(1)+r_str(1)
                            v_loc1(2) = (i_samps-ixxx+1)*r_spc(2)+r_str(2)
                          end if
                          v_loc1(3) = r_hgt

                          if (a_type .eq. 'sch' .or. a_type .eq. 'SCH') then
                            r_lon=v_loc1(1)/r_rad
                            r_lat=v_loc1(2)/r_rad
                            r_hhh=v_loc1(3)
                            call sch_to_tcn(r_rad,v_loc2,r_lat,r_lon,r_hhh,1)
                            call vecmulti(r_atm,v_loc2,v_loc3) ! convert from input xyz to output xyz
                            call vecaddit(r_atm(1,4),v_loc3,v_loc3)
                            call latlon(r_a,r_e2,v_loc3,r_lat,r_lon,r_hhh,2)
                          else if (a_type .eq. 'eqa' .or. a_type .eq. 'EQA') then
                            r_lat=v_loc1(1)/r_rtod
                            r_lon=v_loc1(2)/r_rtod
                            r_hhh=v_loc1(3)
                          else if (a_type .eq. 'utm' .or. a_type .eq. 'UTM') then
                            call utmtoll(r_a,r_e2,i_zone,a_grid,v_loc1,r_lat,r_lon,1)
                            r_hhh=v_loc1(3)
                          else if (a_type .eq. 'neu' .or. a_type .eq. 'NEU') then
                            call utmtoll(r_a,r_e2,i_zone,a_grid,v_loc1,r_lat,r_lon,1)
                            r_hhh=v_loc1(3)
                          else if (a_type .eq. 'enu' .or. a_type .eq. 'ENU') then
                            call enutoll(r_a,r_e2,i_zone,a_grid,v_loc1,r_lat,r_lon,1)
                            r_hhh=v_loc1(3)
                          end if
                          write(a_string,'(x,f10.4,f10.4,f10.2,e14.4)') r_lat*r_rtod,r_lon*r_rtod,r_hhh,r_amp
                        else
                          if (i_flip .eq. 0) then
                            write(a_string,'(x,i6,i7,f10.2,e14.4)') ixxx,iyyy+i_fline-1,r_hgt,r_amp
                          else
                            write(a_string,'(x,i6,i7,f10.2,e14.4)') i_samps-ixxx+1,iyyy+i_fline-1,r_hgt,r_amp
                          end if
                        end if
                        if (r_hgt .ne. 0.) then
                          i_cnt = i_cnt+1
                          r_sum = r_sum + (dble(r_hgt))
                          r_sqr = r_sqr + (dble(r_hgt))**2
                        end if
                      end do
                      if (i_cnt .gt. 0) then
                       r_avge = r_sum/max(i_cnt,1)
                        if (i_cnt .gt. 1) then
                          r_stde = sqrt(max(1.d-25,(r_sqr/max(i_cnt,1))-r_avge**2))
                        else
                          r_stde = 0.
                        end if
                        if (r_spc(1) .ne. 0. .and. r_spc(2) .ne. 0.) then
                          write(32,'(x,5f)') sqrt(((ixx-i_x1)*r_spc(1))**2 + ((iyy-i_y1)*r_spc(2))**2), 
     &                       r_avge, r_stde, sngl(r_lat*r_rtod), sngl(r_lon*r_rtod)
                        else
                          write(32,'(x,3f)') r_ddd,r_avge, r_stde
                        end if
                      end if
                    end do
                    close(32)
                    if (a_pltcmnd .ne. ' ') then
                      a_value=a_pltcmnd(1:max(1,length(a_pltcmnd)))//' dxl2.out &'
                      call system(a_value)
                    endif
                  end if
                          end if
                        end if
                        i_path = 0

                        iy = 0
                        call shiftexp(i_bdat,i_bcnt,i_vxo,i_vyo,i_vxs,i_vys,i_mem,i_zmw,i_wsamps)

                      else										! Some Other CMND
                        iy = 0
                        call shiftexp(i_bdat,i_bcnt,i_vxo,i_vyo,i_vxs,i_vys,i_mem,i_zmw,i_wsamps)
                      end if

                    else if (i_bdat(1,1) .eq. i_zmw) then 		! Window i_zmw (zoom window)
                      if (i_bdat(2,1) .eq. 0) then ! 
                         if (i_debug .ge. 1) write(6,*) 'Error - i_bdat(2,1) = 0  for window i_zmw'
                      else if (i_bdat(2,1) .eq. 1) then             ! expose
                          do iy=1,int(i_vys(i_zmw)/max(r_zoom,1.))+1
                            iyyy = iyy-int(i_vys(i_zmw)/(2.*r_zoom)) + iy*int(1./min(r_zoom,1.))
                            if (iyyy .ge. 0 .and. iyyy .le. i_nline) then
                              if (a_rmgfile .ne. ' ') then
                                call readrmg(i_rmgfile,1+ixx-int(i_vxs(i_zmw)/(2.*r_zoom)),iyyy+i_fline-1,
     &                            int(i_vxs(i_zmw)/r_zoom)+1,i_samps,i_lines+i_fline-1,r_amp1,r_hgt1,i_flip,i_flg,i_err)
                              else if (a_cmpfile .ne. ' ') then
                                call readcmp(i_cmpfile,i_cmptype,1+ixx-int(i_vxs(i_zmw)/(2.*r_zoom)),iyyy+i_fline-1,
     &                            int(i_vxs(i_zmw)/r_zoom)+1,i_samps,i_lines+i_fline-1,r_amp1,r_hgt1,i_flip,i_err)
                              else
                                call readdat(i_dtefile,i_dtetype,1+ixx-int(i_vxs(i_zmw)/(2.*r_zoom)),iyyy+i_fline-1,
     &                            i_doff,max(int(i_vxs(i_zmw)/r_zoom)+1,2),i_samps,i_lines+i_fline-1,r_dmul,r_dadd,
     &                             r_hgt1,i_flip,0,i_err)
                                call readdat(i_magfile,i_magtype,1+ixx-int(i_vxs(i_zmw)/(2.*r_zoom)),iyyy+i_fline-1,
     &                             i_moff,max(int(i_vxs(i_zmw)/r_zoom)+1,2),i_samps,i_lines+i_fline-1,r_mmul,r_madd,
     &                             r_amp1,i_flip,i_flg,i_err)
                              end if
                              do ix=1,int(i_vxs(i_zmw)/max(r_zoom,1.))+1
                                ixxx=ix*int(1./min(r_zoom,1.))
                                if (r_expnt .eq. 1) then
                                  r_dat1(ix+(iy-1)*int(i_vxs(i_zmw)/max(r_zoom,1.))) = 
     &                                 max(min((0.5*((r_amp1(ixxx))-r_avg)/(r_ascale*r_std)+ i_scale*r_bias), 
     &                                 0.9999), 0.0)
                                else
                                  r_dat1(ix+(iy-1)*int(i_vxs(i_zmw)/max(r_zoom,1.))) = 
     &                                    max(min((0.5*((sign(abs(r_amp1(ixxx))**r_expnt,r_amp1(ixxx)))-r_avg)/
     &                                    (r_ascale*r_std)+ i_scale*r_bias), 0.9999), 0.0)
                                end if
                                r_dat2(ix+(iy-1)*int(i_vxs(i_zmw)/max(r_zoom,1.))) = 
     &                               mod(abs(max(min(r_hgt1(ixxx),r_hmax),r_hmin)+10.*r_hscale)-
     &                               r_wstrt,r_hscale)/r_hscale
                              end do
                            else
                              do ix=1,int(i_vxs(i_zmw)/max(r_zoom,1.))+1
                                r_dat1(ix+(iy-1)*int(i_vxs(i_zmw)/max(r_zoom,1.))) = 0
                                r_dat2(ix+(iy-1)*int(i_vxs(i_zmw)/max(r_zoom,1.))) = 0
                              end do
                            end if

                          end do
                          if (i_zmcnt .gt. 0) call topwin(i_zmcnt+1)
                          if ((a_rmgfile .ne. ' ') .or. (a_cmpfile .ne. ' ') .or.
     &                        (i_magtype .ne. 0 .and. i_dtetype .ne. 0) ) then
                            call zoom(i_vxs(i_zmw),i_vys(i_zmw),r_zoom,i_wsamps,r_dat1)
                            call zoom(i_vxs(i_zmw),i_vys(i_zmw),r_zoom,i_wsamps,r_dat2)
                            if (i_colr .eq. 0 .or. i_colr .eq. 3) then
                              CALL display_rmg(i_zmw,  0, 0, 
     &                              i_vxs(i_zmw), i_vys(i_zmw), i_vxs(i_zmw), r_dat1, r_dat2)
                            else if (i_colr .eq. 1) then
                              CALL display_img(i_zmw,  0, 0, 
     &                              i_vxs(i_zmw), i_vys(i_zmw), i_vxs(i_zmw), r_dat1)
                            else if (i_colr .eq. 2) then
                              CALL display_img(i_zmw,  0, 0, 
     &                              i_vxs(i_zmw), i_vys(i_zmw), i_vxs(i_zmw), r_dat2)
                            end if
                          else
                            if (a_dtefile .ne. ' ') then
                              call zoom(i_vxs(i_zmw),i_vys(i_zmw),r_zoom,i_wsamps,r_dat2)
                              if (i_colr .eq. 0) then
                                CALL display_rmg(i_zmw,  0, 0, 
     &                                i_vxs(i_zmw), i_vys(i_zmw), i_vxs(i_zmw), r_dat2, r_dat2)
                              else
                                CALL display_img(i_zmw,  0, 0, 
     &                                i_vxs(i_zmw), i_vys(i_zmw), i_vxs(i_zmw), r_dat2)
                              end if
                            else
                              call zoom(i_vxs(i_zmw),i_vys(i_zmw),r_zoom,i_wsamps,r_dat1)
                              if (i_colr .eq. 0) then
                                CALL display_rmg(i_zmw,  0, 0, 
     &                                i_vxs(i_zmw), i_vys(i_zmw), i_vxs(i_zmw), r_dat1, r_dat1)
                              else
                                CALL display_img(i_zmw,  0, 0, 
     &                                i_vxs(i_zmw), i_vys(i_zmw), i_vxs(i_zmw), r_dat1)
                              end if
                            end if
                          end if
                          if (r_zoom .ge. 1. ) then
                            write(a_string,'(i2,a)') nint(r_zoom),':1  Zoom'
                          else
                            write(a_string,'(a,i2,a)') '1:',nint(1./r_zoom),'  Zoom'
                          end if
                          call display_label(i_zmw,a_string)
  
                      else if (i_bdat(2,1) .eq. 4) then             ! click
                        iy = 0
                        iyyy=int(i_bdat(5,1)/r_zoom)-int(i_vys(i_zmw)/(2.*r_zoom)) + iyy + 1
                        ixxx=int(i_bdat(4,1)/r_zoom)-int(i_vxs(i_zmw)/(2.*r_zoom)) + ixx + 1
                        if (a_rmgfile .ne. ' ') then
                          call readrmg(i_rmgfile,ixxx-1,iyyy+i_fline-1,3,i_samps,i_lines+i_fline-1,
     &                         r_amp1,r_hgt1,i_flip,i_flg,i_err)
                        else if (a_cmpfile .ne. ' ') then
                          call readcmp(i_cmpfile,i_cmptype,ixxx-1,iyyy+i_fline-1,3,i_samps,i_lines+i_fline-1,
     &                         r_amp1,r_hgt1,i_flip,i_err)
                        else
                          call readdat(i_dtefile,i_dtetype,ixxx-1,iyyy+i_fline-1,i_doff,3,i_samps,i_lines+i_fline-1,
     &                             r_dmul,r_dadd,r_hgt1,i_flip,0,i_err)
                          call readdat(i_magfile,i_magtype,ixxx-1,iyyy+i_fline-1,i_moff,3,i_samps,i_lines+i_fline-1,
     &                             r_mmul,r_madd,r_amp1,i_flip,i_flg,i_err)
                        end if
                        r_amp = r_amp1(2)
                        r_hgt = r_hgt1(2)
                        if (a_hdrfile .ne. ' ' .and. i_asc .ne. 0) then

                          if (i_flip .eq. 0) then
                            v_loc1(1) = (iyyy+i_fline-1)*r_spc(1)+r_str(1)
                            v_loc1(2) = ixxx*r_spc(2)+r_str(2)
                          else
                            v_loc1(1) = (iyyy+i_fline-1)*r_spc(1)+r_str(1)
                            v_loc1(2) = (i_samps-ixxx+1)*r_spc(2)+r_str(2)
                          end if
                          v_loc1(3) = r_hgt

                          if (a_type .eq. 'sch' .or. a_type .eq. 'SCH') then
                            r_lon=v_loc1(1)/r_rad
                            r_lat=v_loc1(2)/r_rad
                            r_hhh=v_loc1(3)

                            call sch_to_tcn(r_rad,v_loc2,r_lat,r_lon,r_hhh,1)
                            call vecmulti(r_atm,v_loc2,v_loc3) ! convert from input xyz to output xyz
                            call vecaddit(r_atm(1,4),v_loc3,v_loc3)
                            call latlon(r_a,r_e2,v_loc3,r_lat,r_lon,r_hhh,2)
                          else if (a_type .eq. 'eqa' .or. a_type .eq. 'EQA') then
                            r_lat=v_loc1(1)/r_rtod
                            r_lon=v_loc1(2)/r_rtod
                            r_hhh=v_loc1(3)
                          else if (a_type .eq. 'utm' .or. a_type .eq. 'UTM') then
                            call utmtoll(r_a,r_e2,i_zone,a_grid,v_loc1,r_lat,r_lon,1)
                            r_hhh=v_loc1(3)
                          else if (a_type .eq. 'neu' .or. a_type .eq. 'NEU') then
                            call utmtoll(r_a,r_e2,i_zone,a_grid,v_loc1,r_lat,r_lon,1)
                            r_hhh=v_loc1(3)
                          else if (a_type .eq. 'enu' .or. a_type .eq. 'ENU') then
                            call enutoll(r_a,r_e2,i_zone,a_grid,v_loc1,r_lat,r_lon,1)
                            r_hhh=v_loc1(3)
                          end if

                          write(a_string,'(x,f10.4,f10.4,f10.2,e14.4)') r_lat*r_rtod,r_lon*r_rtod,r_hhh,r_amp

                        else
                          if (i_flip .eq. 0) then
                            write(a_string,'(x,i6,i7,f10.2,e14.4)') ixxx,iyyy+i_fline-1,r_hgt,r_amp
                          else
                            write(a_string,'(x,i6,i7,f10.2,e14.4)') i_samps-ixxx+1,iyyy+i_fline-1,r_hgt,r_amp
                          end if
                        end if
                        if (i_bdat(3,1) .eq. 3) write(6,'(a)') a_string(1:max(1,length(a_string)))//'  '//
     &                        a_filename(1:length(a_filename))
                        if (i_path .eq. i_maxsamps-1) write(6,*) 'Warning -- path buffer full.  ',i_maxsamps
                        i_path = min(i_path+1,i_maxsamps)
                        i_xpath(i_path) = ixxx
                        i_ypath(i_path) = iyyy
                        r_apath(i_path) = r_amp
                        r_hpath(i_path) = r_hgt
                        call display_label(1,a_string)

                        if (i_bdat(3,1) .eq. 2) then
                          if (i_asc .eq. 0) then

                            ixx = ixxx - 1
                            iyy = iyyy - 1

                            i_bcnt = i_bcnt + 1
                            if (i_bcnt .gt. I_BMAX) i_bcnt = I_BMAX
                            i_bdat(1,i_bcnt) = i_zmw
                            i_bdat(2,i_bcnt) = 1
                            i_bdat(3,i_bcnt) = ixx-1
                            i_bdat(4,i_bcnt) = iyy-1
                            i_bdat(5,i_bcnt) = 1
                            i_bdat(6,i_bcnt) = 1
                            if (i_wait .eq. 0) then
                              i_wait = 1
                              iy = 0
                              if (i_debug .ge. 4) write(6,*) 'turning wait off',i_wait
                            end if
                          else
                            call move_scroll(1,max(ixxx-i_vxs(1)/2,0),max(iyyy-i_vys(1)/2,0))
                          end if
                          if (r_zoom .ge. 1.) then
                            write(a_string,'(i2,a)') nint(r_zoom),':1  Zoom'
                          else
                            write(a_string,'(a,i2,a)') '1:',nint(1./r_zoom),'  Zoom'
                          end if
                          call display_label(i_zmw,a_string)

                        end if

                      else if (i_bdat(2,1) .eq. 5) then			! mouse release
                        if (i_path .ge. 2) then
                          if (i_epath .eq. 1) then
                            if (i_debug .ge. 1) write(6,*) ' '
                            if (i_debug .ge. 1) write(6,'(x,a,i)') 'Displaying height along path',i_path
                            open(unit=33,file='path.dat',status='unknown',form='formatted')
                            r_dist=0.
                            write(33,*) r_dist,r_hpath(1),r_apath(1)
                            do i=2,i_path
                              i_strt=max(i-1,1)
                              do while(i_strt.gt.1 .and. 
     &                              sqrt((1.0*(i_xpath(i_strt)-i_xpath(i)))**2 + 
     &                                   (1.0*(i_ypath(i_strt)-i_ypath(i)))**2) .lt. r_curve/2.)
                                i_strt=i_strt-1
                              end do
                              i_stop=min(i+1,i_path)
                              do while(i_stop.lt.i_path .and. 
     &                           sqrt((1.0*(i_xpath(i_stop)-i_xpath(i_strt)))**2 + 
     &                                (1.0*(i_ypath(i_stop)-i_ypath(i_strt)))**2) .lt. r_curve/2.)
                                i_stop=i_stop+1
                              end do
                              r_ddd = sqrt((r_xs*(i_xpath(i_stop)-i_xpath(i_strt)))**2 + 
     &                                     (r_ys*(i_ypath(i_stop)-i_ypath(i_strt)))**2)
                              if (i_debug .ge. 3) write(6,*) 'r_ddd=',r_ddd,i_strt,i,i_stop
                              if (r_ddd .ne. 0.) then
                                r_ccc = ((i_xpath(i)-i_xpath(i-1))*(i_xpath(i_stop)-i_xpath(i_strt))*r_xs**2 + 
     &                                   (i_ypath(i)-i_ypath(i-1))*(i_ypath(i_stop)-i_ypath(i_strt))*r_ys**2 ) / r_ddd
                                r_dist = r_dist + r_ccc
                              endif
                              write(33,*) r_dist,r_hpath(i),r_apath(i)
                            end do
                            close(33)
                            if (a_pltcmnd .ne. ' ') then
                              a_value=a_pltcmnd(1:max(1,length(a_pltcmnd)))//' path.dat &'
                              call system(a_value)
                            endif
                            write(6,*) ' '
                          end if
                          if (i_eline .eq. 1) then
                            i_x1 = i_xpath(1)
                            i_y1 = i_ypath(1)
                            i_x2 = i_xpath(i_path)
                            i_y2 = i_ypath(i_path)

          if (i_ccc .eq. 1 .or. i_ccc .eq. 3) then
            open(unit=31,file='dxl1.out',status='unknown',form='formatted')

            r_dist = sqrt(float(i_x2-i_x1)**2 + float(i_y2-i_y1)**2)
            do r_ddd = 0., r_dist, 1
              i_cnt = 0
              r_sum = 0.
              r_sqr = 0.
              do r_ccc = - i_avg/2., i_avg/2.,1
                        ixx = i_x1 + nint((i_x2-i_x1)*r_ddd/r_dist)
                        iyy = i_y1 + nint((i_y2-i_y1)*r_ddd/r_dist)
                        ixxx = ixx + nint((i_y2-i_y1)*r_ccc/r_dist)
                        iyyy = iyy + nint((i_x2-i_x1)*r_ccc/r_dist)
                        if (a_rmgfile .ne. ' ') then
                          call readrmg(i_rmgfile,ixxx-1,iyyy+i_fline-1,3,i_samps,i_lines+i_fline-1,
     &                          r_amp1,r_hgt1,i_flip,i_flg,i_err)
                        else if (a_cmpfile .ne. ' ') then
                          call readcmp(i_cmpfile,i_cmptype,ixxx-1,iyyy+i_fline-1,3,i_samps,i_lines+i_fline-1,
     &                          r_amp1,r_hgt1,i_flip,i_err)
                        else
                          call readdat(i_dtefile,i_dtetype,ixxx-1,iyyy+i_fline-1,i_doff,3,i_samps,i_lines+i_fline-1,
     &                          r_dmul,r_dadd,r_hgt1,i_flip,0,i_err)
                          call readdat(i_magfile,i_magtype,ixxx-1,iyyy+i_fline-1,i_moff,3,i_samps,i_lines+i_fline-1,
     &                          r_mmul,r_madd,r_amp1,i_flip,i_flg,i_err)
                        end if

                        r_amp = r_amp1(2)
                        r_hgt = r_hgt1(2)
                        if (r_spc(1) .ne. 0. .and. r_spc(2) .ne. 0) then

                          if (i_flip .eq. 0) then
                            v_loc1(1) = (iyyy+i_fline-1)*r_spc(1)+r_str(1)
                            v_loc1(2) = ixxx*r_spc(2)+r_str(2)
                          else
                            v_loc1(1) = (iyyy+i_fline-1)*r_spc(1)+r_str(1)
                            v_loc1(2) = (i_samps-ixxx+1)*r_spc(2)+r_str(2)
                          end if
                          v_loc1(3) = r_hgt

                          if (a_type .eq. 'sch' .or. a_type .eq. 'SCH') then
                            r_lon=v_loc1(1)/r_rad
                            r_lat=v_loc1(2)/r_rad
                            r_hhh=v_loc1(3)
                            call sch_to_tcn(r_rad,v_loc2,r_lat,r_lon,r_hhh,1)
                            call vecmulti(r_atm,v_loc2,v_loc3) ! convert from input xyz to output xyz
                            call vecaddit(r_atm(1,4),v_loc3,v_loc3)
                            call latlon(r_a,r_e2,v_loc3,r_lat,r_lon,r_hhh,2)
                          else if (a_type .eq. 'eqa' .or. a_type .eq. 'EQA') then
                            r_lat=v_loc1(1)/r_rtod
                            r_lon=v_loc1(2)/r_rtod
                            r_hhh=v_loc1(3)
                          else if (a_type .eq. 'utm' .or. a_type .eq. 'UTM') then
                            call utmtoll(r_a,r_e2,i_zone,a_grid,v_loc1,r_lat,r_lon,1)
                            r_hhh=v_loc1(3)
                          else if (a_type .eq. 'neu' .or. a_type .eq. 'NEU') then
                            call utmtoll(r_a,r_e2,i_zone,a_grid,v_loc1,r_lat,r_lon,1)
                            r_hhh=v_loc1(3)
                          else if (a_type .eq. 'enu' .or. a_type .eq. 'ENU') then
                            call enutoll(r_a,r_e2,i_zone,a_grid,v_loc1,r_lat,r_lon,1)
                            r_hhh=v_loc1(3)
                          end if
                          write(a_string,'(x,f10.4,f10.4,f10.2,e14.4)') r_lat*r_rtod,r_lon*r_rtod,r_hhh,r_amp
                        else
                          if (i_flip .eq. 0) then
                            write(a_string,'(x,i6,i7,f10.2,e14.4)') ixxx,iyyy+i_fline-1,r_hgt,r_amp
                          else
                            write(a_string,'(x,i6,i7,f10.2,e14.4)') i_samps-ixxx+1,iyyy+i_fline-1,r_hgt,r_amp
                          end if
                        end if
                        if (r_amp .ne. 0.) then
                          i_cnt = i_cnt+1
                          r_sum = r_sum + (dble(r_amp))
                          r_sqr = r_sqr + (dble(r_amp))**2
                        end if
                      end do
                      if (i_cnt .gt. 0) then
                       r_avge = r_sum/max(i_cnt,1)
                        if (i_cnt .gt. 1) then
                          r_stde = sqrt(max(1.d-25,(r_sqr/max(i_cnt,1))-r_avge**2))
                        else
                          r_stde = 0.
                        end if
                        if (r_spc(1) .ne. 0. .and. r_spc(2) .ne. 0.) then
                          write(31,'(x,5f)') sqrt(((ixx-i_x1)*r_spc(1))**2 + ((iyy-i_y1)*r_spc(2))**2), 
     &                       r_avge, r_stde, sngl(r_lat*r_rtod), sngl(r_lon*r_rtod)
                        else
                          write(31,'(x,3f)') r_ddd,r_avge, r_stde
                        end if
                      end if
                    end do
                    close(31)
                    if (a_pltcmnd .ne. ' ') then
                      a_value=a_pltcmnd(1:max(1,length(a_pltcmnd)))//' dxl1.out &'
                      call system(a_value)
                    endif
                  end if

                  if (i_ccc .eq. 2 .or. i_ccc .eq. 3) then
                    open(unit=32,file='dxl2.out',status='unknown',form='formatted')

                    r_dist = sqrt(float(i_x2-i_x1)**2 + float(i_y2-i_y1)**2)
                    do r_ddd = 0., r_dist, 1
                      i_cnt = 0
                      r_sum = 0.
                      r_sqr = 0.
                      do r_ccc = - i_avg/2., i_avg/2.,1
                        ixx = i_x1 + nint((i_x2-i_x1)*r_ddd/r_dist)
                        iyy = i_y1 + nint((i_y2-i_y1)*r_ddd/r_dist)
                        ixxx = ixx + nint((i_y2-i_y1)*r_ccc/r_dist)
                        iyyy = iyy + nint((i_x2-i_x1)*r_ccc/r_dist)
                        if (a_rmgfile .ne. ' ') then
                          call readrmg(i_rmgfile,ixxx-1,iyyy+i_fline-1,3,i_samps,i_lines+i_fline-1,
     &                          r_amp1,r_hgt1,i_flip,i_flg,i_err)
                        else if (a_cmpfile .ne. ' ') then
                          call readcmp(i_cmpfile,i_cmptype,ixxx-1,iyyy+i_fline-1,3,i_samps,i_lines+i_fline-1,
     &                          r_amp1,r_hgt1,i_flip,i_err)
                        else
                          call readdat(i_dtefile,i_dtetype,ixxx-1,iyyy+i_fline-1,i_doff,3,i_samps,i_lines+i_fline-1,
     &                          r_dmul,r_dadd,r_hgt1,i_flip,0,i_err)
                          call readdat(i_magfile,i_magtype,ixxx-1,iyyy+i_fline-1,i_moff,3,i_samps,i_lines+i_fline-1,
     &                          r_mmul,r_madd,r_amp1,i_flip,i_flg,i_err)
                        end if

                        r_amp = r_amp1(2)
                        r_hgt = r_hgt1(2)
                        if (r_spc(1) .ne. 0. .and. r_spc(2) .ne. 0) then

                          if (i_flip .eq. 0) then
                            v_loc1(1) = (iyyy+i_fline-1)*r_spc(1)+r_str(1)
                            v_loc1(2) = ixxx*r_spc(2)+r_str(2)
                          else
                            v_loc1(1) = (iyyy+i_fline-1)*r_spc(1)+r_str(1)
                            v_loc1(2) = (i_samps-ixxx+1)*r_spc(2)+r_str(2)
                          end if
                          v_loc1(3) = r_hgt

                          if (a_type .eq. 'sch' .or. a_type .eq. 'SCH') then
                            r_lon=v_loc1(1)/r_rad
                            r_lat=v_loc1(2)/r_rad
                            r_hhh=v_loc1(3)
                            call sch_to_tcn(r_rad,v_loc2,r_lat,r_lon,r_hhh,1)
                            call vecmulti(r_atm,v_loc2,v_loc3) ! convert from input xyz to output xyz
                            call vecaddit(r_atm(1,4),v_loc3,v_loc3)
                            call latlon(r_a,r_e2,v_loc3,r_lat,r_lon,r_hhh,2)
                          else if (a_type .eq. 'eqa' .or. a_type .eq. 'EQA') then
                            r_lat=v_loc1(1)/r_rtod
                            r_lon=v_loc1(2)/r_rtod
                            r_hhh=v_loc1(3)
                          else if (a_type .eq. 'utm' .or. a_type .eq. 'UTM') then
                            call utmtoll(r_a,r_e2,i_zone,a_grid,v_loc1,r_lat,r_lon,1)
                            r_hhh=v_loc1(3)
                          else if (a_type .eq. 'neu' .or. a_type .eq. 'NEU') then
                            call utmtoll(r_a,r_e2,i_zone,a_grid,v_loc1,r_lat,r_lon,1)
                            r_hhh=v_loc1(3)
                          else if (a_type .eq. 'enu' .or. a_type .eq. 'ENU') then
                            call enutoll(r_a,r_e2,i_zone,a_grid,v_loc1,r_lat,r_lon,1)
                            r_hhh=v_loc1(3)
                          end if
                          write(a_string,'(x,f10.4,f10.4,f10.2,e14.4)') r_lat*r_rtod,r_lon*r_rtod,r_hhh,r_amp
                        else
                          if (i_flip .eq. 0) then
                            write(a_string,'(x,i6,i7,f10.2,e14.4)') ixxx,iyyy+i_fline-1,r_hgt,r_amp
                          else
                            write(a_string,'(x,i6,i7,f10.2,e14.4)') i_samps-ixxx+1,iyyy+i_fline-1,r_hgt,r_amp
                          end if
                        end if
                        if (r_hgt .ne. 0.) then
                          i_cnt = i_cnt+1
                          r_sum = r_sum + (dble(r_hgt))
                          r_sqr = r_sqr + (dble(r_hgt))**2
                        end if
                      end do
                      if (i_cnt .gt. 0) then
                        r_avge = r_sum/max(i_cnt,1)
                        if (i_cnt .gt. 1) then
                          r_stde = sqrt(max(1.d-25,(r_sqr/max(i_cnt,1))-r_avge**2))
                        else
                          r_stde = 0.
                        end if
                        if (r_spc(1) .ne. 0. .and. r_spc(2) .ne. 0.) then
                          write(32,'(x,5f)') sqrt(((ixx-i_x1)*r_spc(1))**2 + ((iyy-i_y1)*r_spc(2))**2), 
     &                       r_avge, r_stde, sngl(r_lat*r_rtod), sngl(r_lon*r_rtod)
                        else
                          write(32,'(x,3f)') r_ddd,r_avge, r_stde
                        end if
                      end if
                    end do
                    close(32)
                    if (a_pltcmnd .ne. ' ') then
                      a_value=a_pltcmnd(1:max(1,length(a_pltcmnd)))//' dxl2.out &'
                      call system(a_value)
                    endif
                  end if
                          end if
                        end if
                        i_path = 0

                      end if
                      call shiftexp(i_bdat,i_bcnt,i_vxo,i_vyo,i_vxs,i_vys,i_mem,i_zmw,i_wsamps)
                      iy = 0
                    else													! Some Other Window
                      if (i_debug .ge. 1) write(6,*) 'error - i_bdat(1,1) = ',i_bdat(1,1)
                      iy = 0
                      do j = 1,i_bcnt-1
                        do i = 1,10
                          i_bdat(i,j) = i_bdat(i,j+1)
                        end do
                      end do
                      i_bcnt = i_bcnt - 1
                      if (i_debug .ge. 4) write(6,*) 'i_bcnt = ',i_bcnt,-1

                    end if
                  else
                    if (i_wait .eq. 1) then
                      i_wait = 0
                      if (i_debug .ge. 4) write(6,*) 'turning on wait',i_wait
                    end if
                  end if
                end do

                Write(6,*) ' '
                Write(6,*) ' '
                Write(6,'(x,a)') 'DGX: Done'
                Write(6,*) ' '
         end


      integer*4 function getfsize(a_file,i_data)

        implicit none

        character*(*) a_file
#ifdef IO64
        integer*8 i_data
#else
        integer*4 i_data
#endif

        integer i_byte
        integer i_skip
        integer i_err

        byte b_byte

          open(unit=43,file=a_file,status='old',form='unformatted',access='direct',recl=1)
          i_byte = 0
          i_skip = 1000000
          do while (i_skip .gt. 1)
            i_skip = max(1,i_skip/10)
            i_err = 0
            do while (i_err .eq. 0)
              read(43,rec=i_byte+i_skip,iostat=i_err) b_byte
              if (i_err .eq. 0) i_byte = i_byte+i_skip
            end do
          end do
          i_data = i_byte
        getfsize  = 0
        return
      end

      subroutine read_cmd(a_cmdfile,a_prtcmnd,a_pltcmnd,a_jpgcmnd)

        implicit none

        integer i_stat

        character*120 a_cmdfile
        character*120 a_prtcmnd
        character*120 a_pltcmnd
        character*120 a_jpgcmnd

        integer length
        external length
        
        if (a_cmdfile .ne. ' ') then
          open(unit=30,file=a_cmdfile,form='formatted',status='old',readonly,iostat=i_stat)
          if (i_stat .eq. 0) then
            read(30,'(a)',iostat=i_stat) a_prtcmnd
            read(30,'(a)',iostat=i_stat) a_pltcmnd
            read(30,'(a)',iostat=i_stat) a_jpgcmnd
            close(30)
          else
            write(6,*) '*** ERROR opening cmnd file: ',a_cmdfile(1:max(1,length(a_cmdfile)))
          endif
        else
          open(unit=30,file='./dgxcmnds.dat',form='formatted',status='old',readonly,iostat=i_stat)
          if (i_stat .eq. 0) then
            read(30,'(a)',iostat=i_stat) a_prtcmnd
            read(30,'(a)',iostat=i_stat) a_pltcmnd
            read(30,'(a)',iostat=i_stat) a_jpgcmnd
            close(30)
          else
            open(unit=30,file='~/dgxcmnds.dat',form='formatted',status='old',readonly,iostat=i_stat)
            if (i_stat .eq. 0) then
              read(30,'(a)',iostat=i_stat) a_prtcmnd
              read(30,'(a)',iostat=i_stat) a_pltcmnd
              read(30,'(a)',iostat=i_stat) a_jpgcmnd
              close(30)
            endif
          endif
        endif

        return
      end

      subroutine read_mhdr(i_unit,i_mlsize,i_mssize,i_mbytes,i_moff)

        implicit none

        integer i
        integer i_unit
        integer i_mlsize
        integer i_mssize
        integer i_mbytes

        integer i_moff

        character*50 a_string(100)

        integer nread

        integer ioread
        external ioread

#ifdef IO64
        integer*8 i_eight
        integer*8 nseek64
        integer*8 ioseek64
        external i_eight
        external ioseek64
#else
        integer*4 nseek

        integer*4 ioseek
        external ioseek
#endif


          i_mlsize = 0
          i_mssize = 0
c          i_mbytes = 0

#ifdef IO64
          nseek64 = ioseek64(i_unit,i_eight(0))
#else
          nseek = ioseek(i_unit,0)
#endif
          nread = ioread(i_unit,a_string,5000)
          
          do i=1,(nread-1)/50+1
            if (a_string(i) .eq. ' ') then
              ! do nothing
            else if (index(a_string(i),'NUMBER OF SAMPLES PER RECORD =') .gt. 0) then
              read(a_string(i)(35:),*) i_mssize
              write(6,*) ' '
              write(6,*) 'Reading airsar magnitude header ',nread
            else if (index(a_string(i),'NUMBER OF LINES IN IMAGE =') .gt. 0) then
              read(a_string(i)(35:),*) i_mlsize
            else if (index(a_string(i),'NUMBER OF BYTES PER SAMPLE =') .gt. 0) then
              read(a_string(i)(35:),*) i_mbytes
            else if (index(a_string(i),'BYTE OFFSET OF FIRST DATA RECORD =') .gt. 0) then
              read(a_string(i)(35:),*) i_moff
            end if
          end do
              
        return

      end

      subroutine read_dhdr(i_unit,i_dlsize,i_dssize,i_dbytes,i_doff,r_dmul,r_dadd,
     &              r_peg,r_str,r_spc)

        implicit none

        integer i
        integer i_err
        integer i_unit
        integer i_dlsize
        integer i_dssize
        integer i_dbytes

        integer i_demoff

        integer i_doff

        real r_dmul
        real r_dadd 

        real*8 r_peg(3)
        real*8 r_str(2)
        real*8 r_spc(2)

        character*50 a_string(100)

        real*8 r_pi
        real*8 r_rtod

        integer nread

        integer ioread
        external ioread

#ifdef IO64
        integer*8 i_eight
        integer*8 nseek64
        integer*8 ioseek64
        external i_eight
        external ioseek64
#else
        integer*4 nseek

        integer*4 ioseek
        external ioseek
#endif

c
c  Initialize pi and conversions
c
        r_pi = 4.d0*atan(1.0d0)
        r_rtod = 180.0d0/r_pi

          i_dlsize = 0
          i_dssize = 0
c          i_dbytes = 0
c          i_demoff = 0
c          i_doff = 0

#ifdef IO64
          nseek64 = ioseek64(i_unit,i_eight(0))
#else
          nseek = ioseek(i_unit,0)
#endif
          nread = ioread(i_unit,a_string,5000)

          do i=1,(nread-1)/50+1
            if (a_string(i) .eq. ' ') then
              ! do nothing
            else if (index(a_string(i),'NUMBER OF SAMPLES PER RECORD =') .gt. 0) then
              read(a_string(i)(35:),*) i_dssize
              write(6,*) ' '
              write(6,*) 'Reading airsar elevation header ',nread
            else if (index(a_string(i),'NUMBER OF LINES IN IMAGE =') .gt. 0) then
              read(a_string(i)(35:),*) i_dlsize
            else if (index(a_string(i),'NUMBER OF BYTES PER SAMPLE =') .gt. 0) then
              read(a_string(i)(35:),*) i_dbytes
            else if (index(a_string(i),'BYTE OFFSET OF FIRST DATA RECORD =') .gt. 0) then
              read(a_string(i)(35:),*) i_doff
            else if (index(a_string(i),'BYTE OFFSET OF DEM HEADER =') .gt. 0) then
              read(a_string(i)(35:),*) i_demoff
            end if
          end do
              
#ifdef IO64
          nseek64 = ioseek64(i_unit,i_eight(i_demoff))
#else
          nseek = ioseek(i_unit,i_demoff)
#endif
          nread = ioread(i_unit,a_string,4550)

          do i=1,(nread-1)/50+1
            if (a_string(i) .eq. ' ') then
              ! do nothing
            else if (index(a_string(i),'X-DIRECTION POST SPACING (M) =') .gt. 0) then
              read(a_string(i)(35:),*) r_spc(1)
            else if (index(a_string(i),'Y-DIRECTION POST SPACING (M) =') .gt. 0) then
              read(a_string(i)(35:),*) r_spc(2)
            else if (index(a_string(i),'ELEVATION INCREMENT (M) =') .gt. 0) then
              read(a_string(i)(35:),*,iostat=i_err) r_dmul
            else if (index(a_string(i),'ELEVATION OFFSET (M) =') .gt. 0) then
              read(a_string(i)(35:),*,iostat=i_err) r_dadd
            else if (index(a_string(i),'LATITUDE OF PEG POINT =') .gt. 0) then
              read(a_string(i)(35:),*) r_peg(1)
              r_peg(1) = r_peg(1) / r_rtod
            else if (index(a_string(i),'LONGITUDE OF PEG POINT =') .gt. 0) then
              read(a_string(i)(35:),*) r_peg(2)
              r_peg(2) = r_peg(2) / r_rtod
            else if (index(a_string(i),'HEADING AT PEG POINT (DEGREES) =') .gt. 0) then
              read(a_string(i)(35:),*) r_peg(3)
              r_peg(3) = r_peg(3) / r_rtod
            else if (index(a_string(i),'ALONG-TRACK OFFSET S0 (M) =') .gt. 0) then
              read(a_string(i)(35:),*) r_str(1)
            else if (index(a_string(i),'CROSS-TRACK OFFSET C0 (M) =') .gt. 0) then
              read(a_string(i)(35:),*) r_str(2)
            end if
          end do
              

        return

      end

      subroutine zoom(i_wsx,i_wsy,r_zoom,i_wsamps,r_dat)

        implicit none

        integer ix
        integer iy
        integer i_wsx
        integer i_wsy
        integer i_wsamps
        real r_zoom

        real*4 r_dat(i_wsamps)

          if (r_zoom .ge. 1.) then
            do iy = i_wsy,1,-1
              do ix = i_wsx,1,-1
                r_dat(ix+(iy-1)*i_wsx) = r_dat(min((ix+nint(r_zoom)-1)/nint(r_zoom),int(i_wsx/nint(r_zoom)))+
     &              ((iy+nint(r_zoom)-1)/nint(r_zoom)-1)*int(i_wsx/r_zoom))
              end do
            end do
          end if
          return

        end


        subroutine defcolors(i_colnum,i_colors)

          implicit none

          integer j
          integer i_step

          integer i_colnum
          integer i_colors(0:255,3)

          real r_delt

             i_colnum = 256
             i_step = int(i_colnum/3.)
    
                r_delt = 255./(i_step-1 - 0 +1)
                do j = 0,i_step-1
                  i_colors (j,1) = nint( (r_delt*j) )
                  i_colors (j,2) = nint( (255. - r_delt*j) )
                  i_colors (j,3) = nint( (255.) )
                end do
                r_delt = 255./(2*i_step-1 - i_step +1)
                do j = i_step,2*i_step-1
                  i_colors (j,1) = nint( (255.) )
                  i_colors (j,2) = nint( r_delt*(j-i_step) )
                  i_colors (j,3) = nint( (255. - r_delt*(j-i_step)) )
                end do
                r_delt = 255./(i_colnum-1 - 2*i_step +1)
                do j = 2*i_step,i_colnum-1
                  i_colors (j,1) = nint( (255. - r_delt*(j-2*i_step)) )
                  i_colors (j,2) = nint( (255.) )
                  i_colors (j,3) = nint( (r_delt*(j-2*i_step)) )
                end do 
                do j = 0,i_colnum-1
                  i_colors(j,1) = max(min(i_colors(j,1),255),0)
                  i_colors(j,2) = max(min(i_colors(j,2),255),0)
                  i_colors(j,3) = max(min(i_colors(j,3),255),0)
                end do
              return
           end

        subroutine getcolors(a_filename,i_colnum,i_colors)

          implicit none
          character*(*) a_filename

          integer i
          integer i_colnum
          integer i_colors(0:255,3)

          integer length
          external length

            open(unit=20,file=a_filename,status='old',form='formatted',readonly,err=880)
            write(6,*) 'Opening ASCII colortable file: ',a_filename(1:max(1,length(a_filename)))

            i_colnum=0
            do i=0,255
              read(20,*,end=900,err=900) i_colors(i,1),i_colors(i,2),i_colors(i,3)
c              write(6,*) i,')  rgb = ',i_colors(i,1),i_colors(i,2),i_colors(i,3)
              i_colnum = i+1
            end do
900         close(20)
            write(6,*) 'Number of colors read: ',i_colnum
          return
880       write(6,*) ' '
          write(6,*) 'Color map file not Found.  Using Default'
          call defcolors(i_colnum,i_colors)
          write(6,*) ' '
          return
        end

        subroutine getbcolors(a_filename,i_colnum,i_colors)

          implicit none
          character*(*) a_filename

          integer i
          integer i_colnum
          integer i_colors(0:255,3)

          byte b_red
          byte b_grn
          byte b_blu

          integer length
          external length

            open(unit=20,file=a_filename,status='old',form='unformatted',access='direct',
     &           recl=3,readonly,err=880)
            write(6,*) 'Reading colortable file: ',a_filename(1:max(1,length(a_filename)))
            i_colnum=0
            do i=0,255
              read(20,rec=i+1,err=900) b_red,b_grn,b_blu
              i_colors(i,1) = b_red
              i_colors(i,2) = b_grn
              i_colors(i,3) = b_blu
              if (i_colors(i,1) .lt. 0) i_colors(i,1) = i_colors(i,1)+256
              if (i_colors(i,2) .lt. 0) i_colors(i,2) = i_colors(i,2)+256
              if (i_colors(i,3) .lt. 0) i_colors(i,3) = i_colors(i,3)+256
c              write(6,*) i,')  rgb = ',i_colors(i,1),i_colors(i,2),i_colors(i,3)
              i_colnum = i+1
            end do
900         close(20)
            write(6,*) 'Number of colors read: ',i_colnum
          return
880       write(6,*) ' '
          write(6,*) 'Color map file not Found.  Using Default'
          call defcolors(i_colnum,i_colors)
          write(6,*) ' '
          return
        end

        subroutine mixcolor(i_colnum,i_colors,i_magn,i_colr,i_ctable)

          implicit none

          integer i,j
          integer i_magn
          integer i_colr
          integer i_step
          real    r_delt

          integer i_colnum
          integer i_colors(0:255,3)
          integer i_ctable(3,0:255)
          integer i_rrr(0:255)
          integer i_ggg(0:255)
          integer i_bbb(0:255)

          i_step = int(i_colr/3.)
          r_delt = 255./i_step
    
          if (i_colr .gt. 1) then
            if (i_magn .gt. 1) then
              do i = 0 ,i_magn-1
                do j = 0,i_colr-1
                  i_rrr (i*i_colr+j) = nint( (float(i)/float(i_magn-1)) * i_colors((j*i_colnum)/i_colr,1) )
                  i_ggg (i*i_colr+j) = nint( (float(i)/float(i_magn-1)) * i_colors((j*i_colnum)/i_colr,2) )
                  i_bbb (i*i_colr+j) = nint( (float(i)/float(i_magn-1)) * i_colors((j*i_colnum)/i_colr,3) )
                end do
              end do
            else
              do j = 0,i_colr-1
                i_rrr (j) =  i_colors((j*i_colnum)/i_colr,1) 
                i_ggg (j) =  i_colors((j*i_colnum)/i_colr,2) 
                i_bbb (j) =  i_colors((j*i_colnum)/i_colr,3) 
              end do
            end if 
            do i = i_magn*i_colr ,256-1
              i_rrr (i) = i_rrr(0)
              i_ggg (i) = i_ggg(0)
              i_bbb (i) = i_bbb(0)
            end do
          else
            do i=0,i_magn-1
              i_rrr (i) = nint( (float(i)/float(i_magn-1)) *255 )
              i_ggg (i) = nint( (float(i)/float(i_magn-1)) *255 )
              i_bbb (i) = nint( (float(i)/float(i_magn-1)) *255 )
            end do
          end if 
          do i=0,255
            i_rrr(i) = max(min(i_rrr(i),255),0)
            i_ggg(i) = max(min(i_ggg(i),255),0)
            i_bbb(i) = max(min(i_bbb(i),255),0)
c            write(6,*) 'colr = (',i,')',i_rrr(i),i_ggg(i),i_bbb(i)
          end do
          do i=0,255
            i_ctable(1,i) = i_rrr(i)
            i_ctable(2,i) = i_ggg(i) 
            i_ctable(3,i) = i_bbb(i) 
          end do
          return
        end

      subroutine shiftexp(i_bdat,i_bcnt,i_vxo,i_vyo,i_vxs,i_vys,i_mem,i_zmw,i_wsamps)

        implicit none
        integer I_BMAX
        parameter(I_BMAX=100)
        integer j
        integer i_w
        integer i_bcnt
        integer i_vxo(20)
        integer i_vyo(20)
        integer i_vxs(20)
        integer i_vys(20)
        integer i_shft
        integer i_mem
        integer i_wsamps
        integer i_bdat(10,I_BMAX)
        integer i_fd7
        integer i_zmw
        integer i

          i_fd7 = 0
          i_shft=1
          do j = 2,i_bcnt
          if (i_bdat(1,j) .eq. 1) then
            i_w = i_bdat(1,j)
            if (i_bdat(2,j) .eq. 1) then  										! expose command
              if ((i_bdat(4,j) .gt. i_vyo(i_w)+i_vys(i_w) .or.
     &            i_bdat(4,j) + i_bdat(6,j) .lt. i_vyo(i_w)) .and. i_mem .eq. 0 ) then
                i_shft = i_shft + 1
              else if ((i_bdat(3,j) .gt. i_vxo(i_w)+i_vxs(i_w) .or.
     &            i_bdat(3,j) + i_bdat(5,j) .lt. i_vxo(i_w)) .and. i_mem .eq. 0 ) then
                i_shft = i_shft + 1
              else if (j-i_shft-1 .ge. 1) then 
                if (i_bdat(1,j) .eq. i_bdat(1,j-i_shft-1) .and.                  ! scrolling down
     &              i_bdat(2,j) .eq. i_bdat(2,j-i_shft-1) .and. 
     &              i_bdat(4,j) .eq. i_bdat(4,j-i_shft-1) .and. 
     &              i_bdat(6,j) .eq. i_bdat(6,j-i_shft-1) .and.
     &              i_bdat(6,j) * (i_bdat(5,j-i_shft-1)+i_bdat(5,j)) .lt. i_wsamps .and.
     &              i_bdat(3,j) .eq. i_bdat(3,j-i_shft-1) + i_bdat(5,j-i_shft-1) ) then
                  i_bdat(5,j-i_shft-1) = i_bdat(5,j-i_shft-1)+i_bdat(5,j)
                  i_shft = i_shft + 1
                else if (i_bdat(1,j) .eq. i_bdat(1,j-i_shft-1) .and.             ! scrolling up
     &              i_bdat(2,j) .eq. i_bdat(2,j-i_shft-1) .and. 
     &              i_bdat(4,j) .eq. i_bdat(4,j-i_shft-1) .and. 
     &              i_bdat(6,j) .eq. i_bdat(6,j-i_shft-1) .and.
     &              i_bdat(6,j) * (i_bdat(5,j-i_shft-1)+i_bdat(5,j)) .lt. i_wsamps .and.
     &              i_bdat(3,j) + i_bdat(5,j) .eq. i_bdat(3,j-i_shft-1) ) then
                  i_bdat(3,j-i_shft-1) = i_bdat(3,j)
                  i_bdat(5,j-i_shft-1) = i_bdat(5,j-i_shft-1)+i_bdat(5,j)
                  i_shft = i_shft + 1
                else
                  do i = 1,10
                    i_bdat(i,j-i_shft) = i_bdat(i,j)
                  end do
                end if
              else
                do i = 1,10
                  i_bdat(i,j-i_shft) = i_bdat(i,j)
                end do
              end if
            else
              do i = 1,10
                i_bdat(i,j-i_shft) = i_bdat(i,j)
              end do
            end if
          else if (i_bdat(1,j) .eq. i_zmw) then
            if (i_bdat(2,j) .eq. 1) then
              if (i_fd7 .eq. 0) then
                do i = 1,10
                  i_bdat(i,j-i_shft) = i_bdat(i,j)
                end do
                i_fd7 = j-i_shft
              else
                do i = 1,10
                  i_bdat(i,i_fd7) = i_bdat(i,j)
                end do
                i_shft = i_shft + 1
              end if
            else
              do i = 1,10
                i_bdat(i,j-i_shft) = i_bdat(i,j)
              end do
            end if
          else
            do i = 1,10
              i_bdat(i,j-i_shft) = i_bdat(i,j)
            end do
          end if
        end do
        i_bcnt = i_bcnt - i_shft

        return
      end

      subroutine readrmg(i_unit,i_xpos,i_ypos,i_num,i_samples,i_lines,r_dat1,r_dat2,i_flip,i_flg,i_err)

        implicit none

        integer i
        integer i_err
        integer i_num
        integer i_xpos
        integer i_ypos
        integer i_unit
        integer i_flip
        integer i_lines
        integer i_samples
        integer i_flg

        integer i_strtx
        integer i_stopx
        integer i_numxx

        integer nread

        integer ioread
        external ioread

#ifdef IO64
        integer*8 i_eight
        integer*8 nseek64
        integer*8 ioseek64
        external i_eight
        external ioseek64
#else
        integer*4 nseek

        integer*4 ioseek
        external ioseek
#endif

        real r_temp
        real r_dat1(i_samples)
        real r_dat2(i_samples)
        real r_dat3(100000)

        if (i_num .gt. 100000) stop 'i_num too big in readrmg'
        if (i_ypos .ge. 1 .and. i_ypos .le. i_lines) then

          i_strtx = max(i_xpos,1)
          i_stopx = min(i_xpos+i_num-1,i_samples)
          i_numxx = max(i_stopx-i_strtx+1,0)

          if (i_flip .eq. 0) then

            if (i_numxx .gt. 0) then
#ifdef IO64
              nseek64 = ioseek64(i_unit,4*((2*i_eight(i_ypos-1)*i_samples) + i_strtx-1))
#else
              nseek = ioseek(i_unit,4*((2*(i_ypos-1)*i_samples) + i_strtx-1))
#endif
              nread = ioread(i_unit,r_dat1(i_strtx-i_xpos+1),4*i_numxx)

#ifdef IO64
              nseek64 = ioseek64(i_unit,4*((2*i_eight(i_ypos-1)*i_samples) + i_strtx-1 + i_samples))
#else
              nseek = ioseek(i_unit,4*((2*(i_ypos-1)*i_samples) + i_strtx-1 + i_samples))
#endif
              nread = ioread(i_unit,r_dat2(i_strtx-i_xpos+1),4*i_numxx)
            end if

            if (i_flg .eq. 1) then
              if (r_dat1(1) .ne.      0. .and. r_dat1(2) .ne.      0 .and.
     &            r_dat1(1) .ne. -10000. .and. r_dat1(2) .ne. -10000) then
                r_dat3(1) = (r_dat1(2)-r_dat1(1))
              else
                r_dat3(1) = 0.
              end if
              if (r_dat1(i_num-1) .ne.      0. .and. r_dat1(i_num) .ne.      0 .and.
     &            r_dat1(i_num-1) .ne. -10000. .and. r_dat1(i_num) .ne. -10000) then
                r_dat3(i_num) = (r_dat1(i_num)-r_dat1(max(i_num-1,1)))
              else
                r_dat3(i_num) = 0.
              end if
              do i=2,i_num-1
                if (r_dat1(i-1) .ne.      0. .and. r_dat1(i+1) .ne.      0 .and.
     &              r_dat1(i-1) .ne. -10000. .and. r_dat1(i+1) .ne. -10000) then
                  r_dat3(i) = (r_dat1(i+1)-r_dat1(i-1))/2.
                else
                  r_dat3(i) = 0.
                end if
              end do
              do i=1,i_num
                 r_dat1(i) = r_dat3(i)
              end do
            else if (i_flg .eq. 2) then
              if (r_dat2(1) .ne.      0. .and. r_dat2(2) .ne.      0 .and.
     &            r_dat2(1) .ne. -10000. .and. r_dat2(2) .ne. -10000) then
                r_dat1(1) = (r_dat2(2)-r_dat2(1))
              else
                r_dat1(1) = 0.
              end if
              if (r_dat2(i_num-1) .ne.      0. .and. r_dat2(i_num) .ne.      0 .and.
     &            r_dat2(i_num-1) .ne. -10000. .and. r_dat2(i_num) .ne. -10000) then
                r_dat1(i_num) = (r_dat2(i_num)-r_dat2(max(i_num-1,1)))
              else
                r_dat1(i_num) = 0.
              end if
              do i=2,i_num-1
                if (r_dat2(i-1) .ne.      0. .and. r_dat2(i+1) .ne.      0 .and.
     &              r_dat2(i-1) .ne. -10000. .and. r_dat2(i+1) .ne. -10000) then
                  r_dat1(i) = (r_dat2(i+1)-r_dat2(i-1))/2.
                else
                  r_dat1(i) = 0.
                end if
              end do
            end if

          else
  
#ifdef IO64
            nseek64 = ioseek64(i_unit,4*((2*i_eight(i_ypos-1)*i_samples) + i_samples-((i_strtx-1)+i_numxx)))
            nread = ioread(i_unit,r_dat1((i_xpos+i_num)-i_stopx),4*i_numxx)

            nseek64 = ioseek64(i_unit,4*((2*i_eight(i_ypos-1)*i_samples) + i_samples-((i_strtx-1)+i_numxx) + i_samples))
            nread = ioread(i_unit,r_dat2((i_xpos+i_num)-i_stopx),4*i_numxx)
#else
            nseek = ioseek(i_unit,4*((2*(i_ypos-1)*i_samples) + i_samples-((i_strtx-1)+i_numxx)))
            nread = ioread(i_unit,r_dat1((i_xpos+i_num)-i_stopx),4*i_numxx)

            nseek = ioseek(i_unit,4*((2*(i_ypos-1)*i_samples) + i_samples-((i_strtx-1)+i_numxx) + i_samples))
            nread = ioread(i_unit,r_dat2((i_xpos+i_num)-i_stopx),4*i_numxx)
#endif

            if (i_flg .eq. 1) then
              if (r_dat1(1) .ne.      0. .and. r_dat1(2) .ne.      0 .and.
     &            r_dat1(1) .ne. -10000. .and. r_dat1(2) .ne. -10000) then
                r_dat3(1) = -(r_dat1(2)-r_dat1(1))
              else
                r_dat1(1) = 0.
              end if
              if (r_dat1(i_num-1) .ne.      0. .and. r_dat1(i_num) .ne.      0 .and.
     &            r_dat1(i_num-1) .ne. -10000. .and. r_dat1(i_num) .ne. -10000) then
                r_dat3(i_num) = -(r_dat1(i_num)-r_dat1(max(i_num-1,1)))
              else
                r_dat3(i_num) = 0.
              end if
              do i=2,i_num-1
                if (r_dat1(i-1) .ne.      0. .and. r_dat1(i+1) .ne.      0 .and.
     &              r_dat1(i-1) .ne. -10000. .and. r_dat1(i+1) .ne. -10000) then
                  r_dat3(i) = -(r_dat1(i+1)-r_dat1(i-1))/2.
                else
                  r_dat3(i) = 0.
                end if
              end do
              do i=1,i_num
                r_dat1(i) = r_dat3(i)
              end do
            end if
            if (i_flg .eq. 2) then
              if (r_dat2(1) .ne.      0. .and. r_dat2(2) .ne.      0 .and.
     &            r_dat2(1) .ne. -10000. .and. r_dat2(2) .ne. -10000) then
                r_dat1(1) = -(r_dat2(2)-r_dat2(1))
              else
                r_dat1(1) = 0.
              end if
              if (r_dat2(i_num-1) .ne.      0. .and. r_dat2(i_num) .ne.      0 .and.
     &            r_dat2(i_num-1) .ne. -10000. .and. r_dat2(i_num) .ne. -10000) then
                r_dat1(i_num) = -(r_dat2(i_num)-r_dat2(max(i_num-1,1)))
              else
                r_dat1(i_num) = 0.
              end if
              do i=2,i_num-1
                if (r_dat2(i-1) .ne.      0. .and. r_dat2(i+1) .ne.      0 .and.
     &              r_dat2(i-1) .ne. -10000. .and. r_dat2(i+1) .ne. -10000) then
                  r_dat1(i) = -(r_dat2(i+1)-r_dat2(i-1))/2.
                else
                  r_dat1(i) = 0.
                end if
              end do
            end if

            do i=1,i_num/2
              r_temp = r_dat1(i)
              r_dat1(i) = r_dat1(i_num-i+1)
              r_dat1(i_num-i+1) = r_temp

              r_temp = r_dat2(i)
              r_dat2(i) = r_dat2(i_num-i+1)
              r_dat2(i_num-i+1) = r_temp
            end do

          end if

c          write(6,*) 'zeroing data from 1 to ',i_strtx-i_xpos
          do i = 1, i_strtx-i_xpos
            r_dat1(i) = 0.
            r_dat2(i) = 0.
          end do

c          write(6,*) 'zeroing data from ', i_stopx - i_xpos + 2 , ' to ',i_num
          do i = i_stopx - i_xpos + 2 , i_num
            r_dat1(i) = 0
            r_dat2(i) = 0
          end do

          if (nread .ne. 4*i_numxx) then
            i_err=1
c            write(6,*) 'read error ',nread,i_num,i_xpos,i_ypos,i_strtx,i_stopx,i_numxx
          end if
        else
          do i=1,i_num
            r_dat1(i)=0.
            r_dat2(i)=0.
          end do
          i_err=1
        end if
        return
      end      


      subroutine readcmp(i_unit,i_type,i_xpos,i_ypos,i_num,i_samples,i_lines,r_dat1,r_dat2,i_flip,i_err)

        implicit none

        integer i
        integer i_err
        integer i_num
        integer i_xpos
        integer i_ypos
        integer i_unit
        integer i_type
        integer i_flip
        integer i_lines
        integer i_samples

        integer i_strtx
        integer i_stopx
        integer i_numxx

        real*4 r_dat1(i_samples)
        real*4 r_dat2(i_samples)

        complex*8 c_temp
        complex*8 c_dat(100000)

        integer*2 i_dat2(200000)

        integer nread

        integer ioread
        external ioread

#ifdef IO64
        integer*8 i_eight
        integer*8 nseek64
        integer*8 ioseek64
        external i_eight
        external ioseek64
#else
        integer*4 nseek

        integer*4 ioseek
        external ioseek
#endif


c        write(6,*) ' * readcmd ',i_xpos,i_ypos,i_num
        if (i_num .gt. 100000) stop 'Error - input number of samples too big for readcmp.'  !   adjust max and recompile
        if (i_ypos .ge. 1 .and. i_ypos .le. i_lines) then

          i_strtx = max(i_xpos,1)
          i_stopx = min(i_xpos+i_num-1,i_samples)
          i_numxx = max(i_stopx-i_strtx+1,0)

          if (i_type .eq. 1) then
            if (i_flip .eq. 0) then

              if (i_numxx .gt. 0) then
#ifdef IO64
                nseek64 = ioseek64(i_unit,8*((i_eight(i_ypos-1)*i_samples) + i_strtx-1))
#else
                nseek = ioseek(i_unit,8*(((i_ypos-1)*i_samples) + i_strtx-1))
#endif
                nread = ioread(i_unit,c_dat(i_strtx-i_xpos+1),8*i_numxx)
              end if

            else

#ifdef IO64
              nseek64 = ioseek64(i_unit,8*((i_eight(i_ypos-1)*i_samples) + i_samples-((i_strtx-1)+i_numxx)))
#else
              nseek = ioseek(i_unit,8*(((i_ypos-1)*i_samples) + i_samples-((i_strtx-1)+i_numxx)))
#endif
              nread = ioread(i_unit,c_dat((i_xpos+i_num)-i_stopx),8*i_numxx)

              do i=1,i_num/2
                c_temp = c_dat(i)
                c_dat(i) = c_dat(i_num-i+1)
                c_dat(i_num-i+1) = c_temp
              end do

            end if

          else

            if (i_flip .eq. 0) then

              if (i_numxx .gt. 0) then
#ifdef IO64
                nseek64 = ioseek64(i_unit,4*((i_eight(i_ypos-1)*i_samples) + i_strtx-1))
#else
                nseek = ioseek(i_unit,4*(((i_ypos-1)*i_samples) + i_strtx-1))
#endif
                nread = ioread(i_unit,i_dat2(i_strtx-i_xpos+1),4*i_numxx)
              end if
              do i=1,i_num
                c_dat(i) = cmplx(float(i_dat2(2*i-1)),float(i_dat2(2*i)))
              end do

            else

#ifdef IO64
              nseek64 = ioseek64(i_unit,4*((i_eight(i_ypos-1)*i_samples) + i_samples-((i_strtx-1)+i_numxx)))
#else
              nseek = ioseek(i_unit,4*(((i_ypos-1)*i_samples) + i_samples-((i_strtx-1)+i_numxx)))
#endif
              nread = ioread(i_unit,i_dat2((i_xpos+i_num)-i_stopx),4*i_numxx)

              do i=1,i_num/2
                c_temp = cmplx(float(i_dat2(2*i-1)),float(i_dat2(2*i)))
                c_dat(i) = cmplx(float(i_dat2(2*(i_num-i+1)-1)),float(i_dat2(2*(i_num-i+1))))
                c_dat(i_num-i+1) = c_temp
              end do

            end if

          end if

          do i = 1, i_strtx-i_xpos
            c_dat(i) = 0.
          end do

          do i = i_stopx - i_xpos + 2 , i_num
            c_dat(i) = 0
          end do

          do i=1,i_num
            r_dat1(i) = abs(c_dat(i))
            if (r_dat1(i) .gt. 0.) then
c              r_dat2(i) = (180.0/r_pi)*atan2(aimag(c_dat(i)),real(c_dat(i)))   
            else
              r_dat2(i) = 0.
            end if
          end do

          if (nread .ne. 8*i_numxx) then
            i_err=1
c            write(6,*) 'read error ',nread,i_num,i_xpos,i_ypos,i_strtx,i_stopx,i_numxx
          end if
        else
          do i=1,i_num
            r_dat1(i)=0.
            r_dat2(i)=0.
          end do
          i_err=1
        end if
        return
      end      

      subroutine readdat(i_unit,i_type,i_xpos,i_ypos,i_off,i_num,i_samples,i_lines,
     &            r_mul,r_add,r_dat1,i_flip,i_flg,i_err)

        implicit none

        integer i
        integer i_err
        integer i_num
        integer i_xpos
        integer i_ypos
        integer i_unit
        integer i_type
        integer i_flip
        integer i_lines
        integer i_samples
        integer i_off
        integer i_flg

        integer i_strtx
        integer i_stopx
        integer i_numxx

        real r_mul
        real r_add

        integer nread

        integer ioread
        external ioread

#ifdef IO64
        integer*8 i_eight
        integer*8 nseek64
        integer*8 ioseek64
        external i_eight
        external ioseek64
#else
        integer*4 nseek

        integer*4 ioseek
        external ioseek
#endif


        real r_dat1(i_samples)
        real r_dat2(100000)

        byte b_temp
        byte b_temp2(2)
        byte b_dat(100000)


        real      r_temp
        integer*2 i_temp2
        integer*4 i_temp4

        integer*2 i_dat2(100000)
        integer*4 i_dat4(100000)

        equivalence (i_temp2,b_temp2)

        save r_dat2

        if (i_num .gt. 100000) stop 'Error - i_num too big in readdat'

        if (i_flg .ne. 2) then
        goto (100,200,300,400,500,600) i_type

          return

100     continue ! Read byte format
          if (i_ypos .ge. 1 .and. i_ypos .le. i_lines) then

            i_strtx = max(i_xpos,1)
            i_stopx = min(i_xpos+i_num-1,i_samples)
            i_numxx = max(i_stopx-i_strtx+1,0)

            if (i_flip .eq. 0) then

#ifdef IO64
              nseek64 = ioseek64(i_unit,i_off + 1*((i_eight(i_ypos-1)*i_samples) + i_strtx-1))
#else
              nseek = ioseek(i_unit,i_off + 1*(((i_ypos-1)*i_samples) + i_strtx-1))
#endif
              nread = ioread(i_unit,b_dat(i_strtx-i_xpos+1),1*i_numxx)

            else

#ifdef IO64
              nseek64 = ioseek64(i_unit,i_off + 1*((i_eight(i_ypos-1)*i_samples) + i_samples-((i_strtx-1)+i_numxx)))
#else
              nseek = ioseek(i_unit,i_off + 1*(((i_ypos-1)*i_samples) + i_samples-((i_strtx-1)+i_numxx)))
#endif
              nread = ioread(i_unit,b_dat((i_xpos+i_num)-i_stopx),1*i_numxx)

              do i=1,i_num/2
                b_temp = b_dat(i)
                b_dat(i) = b_dat(i_num-i+1)
                b_dat(i_num-i+1) = b_temp
              end do

            end if
  
            do i = 1, i_strtx-i_xpos
              b_dat(i) = 0
            end do

            do i = i_stopx - i_xpos + 2 , i_num
              b_dat(i) = 0
            end do

            do i=1,i_num
              if (b_dat(i) .lt. 0) then
                r_dat1(i) = (b_dat(i)+256)*r_mul + r_add
              else
                r_dat1(i) = (b_dat(i))*r_mul + r_add
              end if
            end do

            if (nread .ne. 1*i_numxx) then
              i_err=1
c              write(6,*) 'read error ',nread,i_num,i_xpos,i_ypos,i_strtx,i_stopx,i_numxx
            end if

          else
            do i=1,i_num
              r_dat1(i)=(0)*r_mul + r_add
            end do
            i_err=1
          end if
          do i=1,i_num
            r_dat2(i) = r_dat1(i)
          end do

          go to 888

200     continue ! Integer*2 format
        if (i_ypos .ge. 1 .and. i_ypos .le. i_lines) then

          i_strtx = max(i_xpos,1)
          i_stopx = min(i_xpos+i_num-1,i_samples)
          i_numxx = max(i_stopx-i_strtx+1,0)

          if (i_flip .eq. 0) then

#ifdef IO64
            nseek64 = ioseek64(i_unit,i_off + 2*((i_eight(i_ypos-1)*i_samples) + i_strtx-1))
#else
            nseek = ioseek(i_unit,i_off + 2*(((i_ypos-1)*i_samples) + i_strtx-1))
#endif
            nread = ioread(i_unit,i_dat2(i_strtx-i_xpos+1),2*i_numxx)

          else

#ifdef IO64
            nseek64 = ioseek64(i_unit,i_off + 2*((i_eight(i_ypos-1)*i_samples) + i_samples-((i_strtx-1)+i_numxx)))
#else
            nseek = ioseek(i_unit,i_off + 2*(((i_ypos-1)*i_samples) + i_samples-((i_strtx-1)+i_numxx)))
#endif
            nread = ioread(i_unit,i_dat2((i_xpos+i_num)-i_stopx),2*i_numxx)

            do i=1,i_num/2
              i_temp2 = i_dat2(i)
              i_dat2(i) = i_dat2(i_num-i+1)
              i_dat2(i_num-i+1) = i_temp2
            end do

          end if

          do i = 1, i_strtx-i_xpos
            i_dat2(i) = 0
          end do

          do i = i_stopx - i_xpos + 2 , i_num
            i_dat2(i) = 0
          end do

          do i=1,i_num
            i_temp4 = i_dat2(i)
            if (i_temp4 .lt. 0) i_temp4=65536+i_temp4
            r_dat1(i)=(i_temp4)*r_mul + r_add
          end do

          if (nread .ne. 2*i_numxx) then
c            write(6,*) 'read error ',nread,i_num,i_xpos,i_ypos,i_strtx,i_stopx,i_numxx
            i_err=1
          end if
        else
          do i=1,i_num
            r_dat1(i)=(0)*r_mul + r_add
          end do
          i_err=1
        end if
        do i=1,i_num
          r_dat2(i) = r_dat1(i)
        end do

        go to 888

300     continue ! Integer*4 format
        if (i_ypos .ge. 1 .and. i_ypos .le. i_lines) then

          i_strtx = max(i_xpos,1)
          i_stopx = min(i_xpos+i_num-1,i_samples)
          i_numxx = max(i_stopx-i_strtx+1,0)

          if (i_flip .eq. 0) then

#ifdef IO64
            nseek64 = ioseek64(i_unit,i_off + 4*((i_eight(i_ypos-1)*i_samples) + i_strtx-1))
#else
            nseek = ioseek(i_unit,i_off + 4*(((i_ypos-1)*i_samples) + i_strtx-1))
#endif
            nread = ioread(i_unit,i_dat4(i_strtx-i_xpos+1),4*i_numxx)

          else

#ifdef IO64
            nseek64 = ioseek64(i_unit,i_off + 4*((i_eight(i_ypos-1)*i_samples) + i_samples-((i_strtx-1)+i_numxx)))
#else
            nseek = ioseek(i_unit,i_off + 4*(((i_ypos-1)*i_samples) + i_samples-((i_strtx-1)+i_numxx)))
#endif
            nread = ioread(i_unit,i_dat4((i_xpos+i_num)-i_stopx),4*i_numxx)

            do i=1,i_num/2
              i_temp4 = i_dat4(i)
              i_dat4(i) = i_dat4(i_num-i+1)
              i_dat4(i_num-i+1) = i_temp4
            end do

          end if

          do i = 1, i_strtx-i_xpos
            i_dat4(i) = 0
          end do

          do i = i_stopx - i_xpos + 2 , i_num
            i_dat4(i) = 0
          end do


          do i=1,i_num
            r_dat1(i)=i_dat4(i)
          end do

          if (nread .ne. 4*i_numxx) then
c            write(6,*) 'read error ',nread,i_num,i_xpos,i_ypos,i_strtx,i_stopx,i_numxx
            i_err=1
          end if
        else
          do i=1,i_num
            r_dat1(i)=0.
          end do
          i_err=1
        end if
        do i=1,i_num
          r_dat2(i) = r_dat1(i)
        end do

        go to 888

400     continue ! real*4 format
        if (i_ypos .ge. 1 .and. i_ypos .le. i_lines) then

          i_strtx = max(i_xpos,1)
          i_stopx = min(i_xpos+i_num-1,i_samples)
          i_numxx = max(i_stopx-i_strtx+1,0)

          if (i_flip .eq. 0) then

#ifdef IO64
            nseek64 = ioseek64(i_unit,i_off + 4*((i_eight(i_ypos-1)*i_samples) + i_strtx-1))
#else
            nseek = ioseek(i_unit,i_off + 4*(((i_ypos-1)*i_samples) + i_strtx-1))
#endif
            nread = ioread(i_unit,r_dat1(i_strtx-i_xpos+1),4*i_numxx)

          else

#ifdef IO64
            nseek64 = ioseek64(i_unit,i_off + 4*((i_eight(i_ypos-1)*i_samples) + i_samples-((i_strtx-1)+i_numxx)))
#else
            nseek = ioseek(i_unit,i_off + 4*(((i_ypos-1)*i_samples) + i_samples-((i_strtx-1)+i_numxx)))
#endif
            nread = ioread(i_unit,r_dat1((i_xpos+i_num)-i_stopx),4*i_numxx)

            do i=1,i_num/2
              r_temp = r_dat1(i)
              r_dat1(i) = r_dat1(i_num-i+1)
              r_dat1(i_num-i+1) = r_temp
            end do

          end if

          do i = 1, i_strtx-i_xpos
            r_dat1(i) = 0.
          end do

          do i = i_stopx - i_xpos + 2 , i_num
            r_dat1(i) = 0
          end do


          if (nread .ne. 4*i_numxx) then
c            write(6,*) 'read error ',nread,i_num,i_xpos,i_ypos,i_strtx,i_stopx,i_numxx
            i_err=1
          end if
        else
          if (i_num .lt. 1) write(6,*) 'i_num = ',i_num
          do i=1,i_num
            if (i .le. 0) write(6,*) 'i = ',i
            r_dat1(i)=0.
          end do
          i_err=1
        end if
        do i=1,i_num
          r_dat2(i) = r_dat1(i)
        end do

        go to 888

500     continue ! Integer*2 Byte swap format 
        if (i_ypos .ge. 1 .and. i_ypos .le. i_lines) then

          i_strtx = max(i_xpos,1)
          i_stopx = min(i_xpos+i_num-1,i_samples)
          i_numxx = max(i_stopx-i_strtx+1,0)

          if (i_flip .eq. 0) then

#ifdef IO64
            nseek64 = ioseek64(i_unit,i_off + 2*((i_eight(i_ypos-1)*i_samples) + i_strtx-1))
#else
            nseek = ioseek(i_unit,i_off + 2*(((i_ypos-1)*i_samples) + i_strtx-1))
#endif
            nread = ioread(i_unit,i_dat2(i_strtx-i_xpos+1),2*i_numxx)

          else

#ifdef IO64
            nseek64 = ioseek64(i_unit,i_off + 2*((i_eight(i_ypos-1)*i_samples) + i_samples-((i_strtx-1)+i_numxx)))
#else
            nseek = ioseek(i_unit,i_off + 2*(((i_ypos-1)*i_samples) + i_samples-((i_strtx-1)+i_numxx)))
#endif
            nread = ioread(i_unit,i_dat2((i_xpos+i_num)-i_stopx),2*i_numxx)

            do i=1,i_num/2
              i_temp2 = i_dat2(i)
              i_dat2(i) = i_dat2(i_num-i+1)
              i_dat2(i_num-i+1) = i_temp2
            end do

          end if

          do i = 1, i_strtx-i_xpos
            i_dat2(i) = 0
          end do

          do i = i_stopx - i_xpos + 2 , i_num
            i_dat2(i) = 0
          end do

          do i=1,i_num
            i_temp2 = i_dat2(i)
            b_temp  = b_temp2(1)
            b_temp2(1) = b_temp2(2)
            b_temp2(2) = b_temp
            i_temp4=i_temp2
            if (i_temp4 .lt. 0) i_temp4=65536+i_temp4
            r_dat1(i)=(i_temp4)*r_mul + r_add
          end do

          if (nread .ne. 2*i_numxx) then
c            write(6,*) 'read error ',nread,i_num,i_xpos,i_ypos,i_strtx,i_stopx,i_numxx
            i_err=1
          end if
        else
          do i=1,i_num
            r_dat1(i)=(0)*r_mul + r_add
          end do
          i_err=1
        end if

        do i=1,i_num
          r_dat2(i) = r_dat1(i)
        end do
        go to 888
600     continue ! Signed Integer*2 format
        if (i_ypos .ge. 1 .and. i_ypos .le. i_lines) then

          i_strtx = max(i_xpos,1)
          i_stopx = min(i_xpos+i_num-1,i_samples)
          i_numxx = max(i_stopx-i_strtx+1,0)

          if (i_flip .eq. 0) then

#ifdef IO64
            nseek64 = ioseek64(i_unit,i_off + 2*((i_eight(i_ypos-1)*i_samples) + i_strtx-1))
#else
            nseek = ioseek(i_unit,i_off + 2*(((i_ypos-1)*i_samples) + i_strtx-1))
#endif
            nread = ioread(i_unit,i_dat2(i_strtx-i_xpos+1),2*i_numxx)

          else

#ifdef IO64
            nseek64 = ioseek64(i_unit,i_off + 2*((i_eight(i_ypos-1)*i_samples) + i_samples-((i_strtx-1)+i_numxx)))
#else
            nseek = ioseek(i_unit,i_off + 2*(((i_ypos-1)*i_samples) + i_samples-((i_strtx-1)+i_numxx)))
#endif
            nread = ioread(i_unit,i_dat2((i_xpos+i_num)-i_stopx),2*i_numxx)

            do i=1,i_num/2
              i_temp2 = i_dat2(i)
              i_dat2(i) = i_dat2(i_num-i+1)
              i_dat2(i_num-i+1) = i_temp2
            end do

          end if

          do i = 1, i_strtx-i_xpos
            i_dat2(i) = 0
          end do

          do i = i_stopx - i_xpos + 2 , i_num
            i_dat2(i) = 0
          end do

          do i=1,i_num
            i_temp4 = i_dat2(i)
            if (i_temp4 .gt. 65536/2 - 1) i_temp4=i_temp4-65536
            r_dat1(i)=(i_temp4)*r_mul + r_add
          end do

          if (nread .ne. 2*i_numxx) then
c            write(6,*) 'read error ',nread,i_num,i_xpos,i_ypos,i_strtx,i_stopx,i_numxx
            i_err=1
          end if
        else
          do i=1,i_num
            r_dat1(i)=(0)*r_mul + r_add
          end do
          i_err=1
        end if
        do i=1,i_num
          r_dat2(i) = r_dat1(i)
        end do

        go to 888


      endif
888   continue
      if (i_flg .ne. 0) then
        if (abs(r_dat2(1)) .ne. 10000. .and. abs(r_dat2(2)) .ne. 10000. .and.
     &          r_dat2(1)  .ne.     0. .and.     r_dat2(2)  .ne.     0.) then
          r_dat1(1) = r_dat2(2) - r_dat2(1)
        else
          r_dat1(1) = 0.
        end if
        if (abs(r_dat2(i_num)) .ne. 10000. .and. abs(r_dat2(i_num-1)) .ne. 10000. .and.
     &          r_dat2(i_num)  .ne.     0. .and.     r_dat2(i_num-1)  .ne.     0.) then
          r_dat1(i_num) = r_dat2(i_num) - r_dat2(i_num-1)
        else
          r_dat1(i_samples) = 0.
        end if
        do i = 2,i_num-1
          if (abs(r_dat2(i-1)) .ne. 10000. .and. abs(r_dat2(i+1)) .ne. 10000. .and.
     &            r_dat2(i-1)  .ne.     0. .and.     r_dat2(i+1)  .ne.     0.) then
            r_dat1(i) = (r_dat2(i+1)-r_dat2(i-1))/2.
          else if (abs(r_dat2(i)) .ne. 10000. .and. r_dat2(i) .ne. 0.) then
            if (abs(r_dat2(i-1)) .ne. 10000. .and. r_dat2(i-1) .ne. 0.) then
              r_dat1(i) = r_dat2(i) - r_dat2(i-1)
            else if (abs(r_dat2(i+1)) .ne. 10000. .and. r_dat2(i+1) .ne. 0.) then
              r_dat1(i) = r_dat2(i+1) - r_dat2(i)
            else 
              r_dat1(i) = 0.
            end if
          else
            r_dat1(i) = 0.
          end if
        end do
        return
      end if
      return
      end


c ======================================================================
c
        integer function length(a_string)
c
c  This function returns the position of the last none blank character
c                   in the string
c
c  Date  :      6/29/93
c  Author:      Scott Shaffer
c
c  Input:  a_string
c
c  Output: length
c
c ======================================================================
        implicit none

        character*(*) a_string

        integer i_len

          i_len=len(a_string)
          do while(i_len .gt. 0 .and. (a_string(i_len:i_len) .eq. ' ' .or. 
     &           ichar(a_string(i_len:i_len)) .eq. 0) )
            i_len=i_len-1
          end do

          length=i_len
          return
        end



****************************************************************
        subroutine read_hdr(a_hdrfile,i_lsize,i_ssize,r_peg,a_type,
     &              r_str,r_spc,i_mbytes,i_dbytes,r_mmul,r_madd,
     &              r_dmul,r_dadd,i_err)

c****************************************************************
c**
c**	FILE NAME: read_hdr.f
c**
c**     DATE WRITTEN: 2/15/96
c**
c**     PROGRAMMER:Scott Shaffer
c**
c** 	FUNCTIONAL DESCRIPTION: Reads some of an IFPROC header file.
c**
c**     ROUTINES CALLED:none
c**  
c**     NOTES: 
c**
c**
c*****************************************************************

       	implicit none

c	INPUT VARIABLES:
        
        character*(*) a_hdrfile         !header file

c	OUTPUT VARIABLES: 

        character*(*) a_type

        integer*4 i_err
        integer*4 i_lsize
        integer*4 i_ssize

        integer*4 i_mbytes
        integer*4 i_dbytes

        real*8 r_peg(3)
        real*8 r_str(2)
        real*8 r_spc(2)
        real r_mmul
        real r_madd
        real r_dmul
        real r_dadd


c	LOCAL VARIABLES: 

        integer*4 i
        integer*4 j
        integer*4 i_cnt
        real*8 r_atm(3,4)
        real*8 r_pi
        real*8 r_rtod
        real*8 r_mdnc(2)
        real*8 r_ddnc(2)

        character*255 a_tmp

c	FUNCTION STATEMENTS: 

        integer length
        external length

c	DATA STATEMENTS: none

c  	PROCESSING STEPS:
        
c
c  Initialize pi and conversions
c
        r_pi = 4.d0*atan(1.0d0)
        r_rtod = 180.0d0/r_pi

        i_err = 1
        i_cnt = 0

        open(12,file=a_hdrfile,status='old',form='formatted',err=900)
c        write(6,*) ' '
c        write(6,*) 'Opening hdr input  file: ',a_hdrfile(1:52)

        do i=1,100000
           read(12,'(a)',end=900) a_tmp
           if (a_tmp .eq. ' ') then
             ! do nothing
           else if (index(a_tmp,'Data file dimensions') .gt. 0) then
             read(a_tmp,*) i_lsize,i_ssize
             i_cnt = i_cnt + 1
           else if (index(a_tmp,'Post Spacing') .gt. 0) then
             read(a_tmp,*) r_spc
             i_cnt = i_cnt + 2
           else if (index(a_tmp,'Peg position (WGS-84)') .gt. 0) then
             read(a_tmp,*) r_peg
             r_peg(1) = r_peg(1)/r_rtod
             r_peg(2) = r_peg(2)/r_rtod
             r_peg(3) = r_peg(3)/r_rtod
             i_cnt = i_cnt + 4
           else if (index(a_tmp,'Starting corner position (s,c)') .gt. 0) then
             read(a_tmp,*) r_str
             i_cnt = i_cnt + 8
           else if (index(a_tmp,'M11 M12 M13') .gt. 0) then
             read(a_tmp,*) r_atm(1,1),r_atm(1,2),r_atm(1,3)
c             i_cnt = i_cnt + 16
           else if (index(a_tmp,'M21 M22 M23') .gt. 0) then
             read(a_tmp,*) r_atm(2,1),r_atm(2,2),r_atm(2,3)
c             i_cnt = i_cnt + 32
           else if (index(a_tmp,'M31 M32 M33') .gt. 0) then
             read(a_tmp,*) r_atm(3,1),r_atm(3,2),r_atm(3,3)
c             i_cnt = i_cnt + 64
           else if (index(a_tmp,'O1 O2 O3') .gt. 0) then
             read(a_tmp,*) r_atm(1,4),r_atm(2,4),r_atm(3,4)
c             i_cnt = i_cnt + 128
           else if (index(a_tmp,'Magnitude Scale and Shift') .gt. 0) then
             read(a_tmp,*) r_mdnc
             r_mmul=r_mdnc(1)
             r_madd=r_mdnc(2)
           else if (index(a_tmp,'Elevation Scale and Shift') .gt. 0) then
             read(a_tmp,*) r_ddnc
             r_dmul=r_ddnc(1)
             r_dadd=r_ddnc(2)
             write(6,*) 'r_dm,r_da=',r_dmul,r_dadd
           else if (index(a_tmp,'Magnitude Bytes per Pixel') .gt. 0) then
             read(a_tmp,*) i_mbytes
           else if (index(a_tmp,'Elevation Bytes per Pixel') .gt. 0) then
             read(a_tmp,*) i_dbytes
             write(6,*) 'i_dbytes=',i_dbytes
           else if (index(a_tmp,'Data file type') .gt. 0) then
             a_type = a_tmp(1:max(1,index(a_tmp,';')-1))
             do j=1,length(a_type)
               if (ichar(a_type(1:1)) .eq. 32 .or. ichar(a_type(1:1)) .eq. 9) a_type = a_type(2:)
             end do
           end if
        end do
        close(12)
        stop 'Error reading header file, too many lines'

900     close(12,err=910)
910     if (i_cnt .eq. 15) i_err = 0 
        return
      end

c****************************************************************

	subroutine tcnatm(r_a,r_e2,r_peg,r_atm)

c****************************************************************
c**
c**	FILE NAME: tcnatm.for
c**
c**     DATE WRITTEN:10/25/95 
c**
c**     PROGRAMMER:Scott Shaffer
c**
c** 	FUNCTIONAL DESCRIPTION:This routine computes the transformation
c**     matris and translation vector needed to get between radar (t,c,n)
c**     coordinates and (x,y,z) WGS-84 coordinates.
c**
c**     ROUTINES CALLED:
c**  
c**     NOTES: none
c**
c**     UPDATE LOG:
c**
c*****************************************************************

       	implicit none

c	INPUT VARIABLES:
        real*8 r_a                    !semimajor axis
        real*8 r_e2                   !eccentricity squared
        real*8 r_peg(3)               !peg latitude,longitude,heading
   
c   	OUTPUT VARIABLES:
        real*8 r_atm(3,4)             !rotation matris
 
c	LOCAL VARIABLES:
        integer i_type
        real*8 r_hgt
        real*8 r_slt,r_clt,r_clo,r_slo,r_chg,r_shg

        real*8 rdir
        external rdir

c	DATA STATEMENTS:none

c  	PROCESSING STEPS:

c       first determine the rotation matris

        r_clt = cos(r_peg(1))
        r_slt = sin(r_peg(1))
        r_clo = cos(r_peg(2))
        r_slo = sin(r_peg(2))
        r_chg = cos(r_peg(3))
        r_shg = sin(r_peg(3))

        r_atm(1,1) = - r_slo*r_shg - r_slt*r_clo*r_chg
        r_atm(1,2) =   r_slo*r_chg - r_slt*r_clo*r_shg
        r_atm(1,3) =   r_clt*r_clo
        r_atm(2,1) =   r_clo*r_shg - r_slt*r_slo*r_chg
        r_atm(2,2) = - r_clo*r_chg - r_slt*r_slo*r_shg
        r_atm(2,3) =   r_clt*r_slo
        r_atm(3,1) =   r_clt*r_chg
        r_atm(3,2) =   r_clt*r_shg
        r_atm(3,3) =   r_slt

c       find the translation vector

        i_type = 1
        r_hgt = 0.
        call latlon(r_a,r_e2,r_atm(1,4),r_peg(1),r_peg(2),r_hgt,i_type)

        return
        end  

c****************************************************************
        subroutine latlon(r_a,r_e2,r_v,r_lat,r_lon,r_hgt,i_type) 

c****************************************************************
c**   
c**   FILE NAME: latlon.f
c**   
c**   DATE WRITTEN:7/22/93 
c**   
c**   PROGRAMMER:Scott Hensley
c**   
c**   FUNCTIONAL DESCRIPTION:This program converts a vector to 
c**   lat,lon and height above the reference ellipsoid or given a
c**   lat,lon and height produces a geocentric vector. 
c**   
c**   ROUTINES CALLED:none
c**   
c**   NOTES: none
c**   
c**   UPDATE LOG:
c**   
c****************************************************************
        
        implicit none
        
c     INPUT VARIABLES:
        integer i_type                   !1=lat,lon to vector,2= vector to lat,lon
        real*8 r_a                       !ellispoid semi-major axis
        real*8 r_e2                      !ellipsoid eccentricity squared  
        real*8 r_v(3)                    !geocentric vector (meters)
        real*8 r_lat                     !latitude (deg -90 to 90)
        real*8 r_lon                     !longitude (deg -180 to 180)
        real*8 r_hgt                     !height above ellipsoid (meters)
   
c       OUTPUT VARIABLES:see input

c       LOCAL VARIABLES:
        integer i_ft
        real*8 pi,r_dtor,r_re,r_q2,r_q3,r_b,r_q
        real*8 r_p,r_tant,r_theta

c       DATA STATEMENTS:
        data pi /3.141592653589793238d0/
        data r_dtor /1.74532925199d-2/
        data i_ft /0/

C       FUNCTION STATEMENTS:

c       PROCESSING STEPS:

        if(i_type .eq. 1)then  !convert lat,lon to vector
           
           r_re = r_a/sqrt(1.d0 - r_e2*sin(r_lat)**2)
           
           r_v(1) = (r_re + r_hgt)*cos(r_lat)*cos(r_lon)
           r_v(2) = (r_re + r_hgt)*cos(r_lat)*sin(r_lon)
           r_v(3) = (r_re*(1.d0-r_e2) + r_hgt)*sin(r_lat)               
           
        elseif(i_type .eq. 2)then  !convert vector to lat,lon 
           
           if(i_ft .eq. 0)then
              r_q2 = 1.d0/(1.d0 - r_e2)
              r_q = sqrt(r_q2)
              r_q3 = r_q2 - 1.d0
              r_b = r_a*sqrt(1.d0 - r_e2)
           end if
           
           r_lon = atan2(r_v(2),r_v(1))
           
           r_p = sqrt(r_v(1)**2 + r_v(2)**2)
           r_tant = (r_v(3)/r_p)*r_q
           r_theta = atan(r_tant)
           r_tant = (r_v(3) + r_q3*r_b*sin(r_theta)**3)/
     +          (r_p - r_e2*r_a*cos(r_theta)**3)
           r_lat =  atan(r_tant)
           r_re = r_a/sqrt(1.d0 - r_e2*sin(r_lat)**2)
           r_hgt = r_p/cos(r_lat) - r_re          
  
        end if
      
        return
        end  

c****************************************************************
        subroutine sch_to_tcn(r_a,r_v,r_lat,r_lon,r_hgt,i_type) 

c****************************************************************
c**   
c**   FILE NAME: sch_to_tcn.f
c**   
c**   DATE WRITTEN:7/22/93 
c**   
c**   PROGRAMMER:Scott Hensley
c**   
c**   FUNCTIONAL DESCRIPTION:This program converts a vector to 
c**   lat,lon and height above the reference ellipsoid or given a
c**   lat,lon and height produces a geocentric vector. 
c**   
c**   ROUTINES CALLED:none
c**   
c**   NOTES: none
c**   
c**   UPDATE LOG:
c**   
c****************************************************************
        
        implicit none
        
c     INPUT VARIABLES:
        integer i_type                   !1=lat,lon to vector,2= vector to lat,lon
        real*8 r_a                       !ellispoid semi-major axis
        real*8 r_v(3)                    !geocentric vector (meters)
        real*8 r_lat                     !latitude (deg -90 to 90)
        real*8 r_lon                     !longitude (deg -180 to 180)
        real*8 r_hgt                     !height above ellipsoid (meters)
   
c       OUTPUT VARIABLES:see input

c       LOCAL VARIABLES:
        real*8 r_p

C       FUNCTION STATEMENTS:

c       PROCESSING STEPS:

        if(i_type .eq. 1)then  !convert lat,lon to vector
                      
           r_v(3) = (r_a + r_hgt)*cos(r_lat)*cos(r_lon) - r_a
           r_v(1) = (r_a + r_hgt)*cos(r_lat)*sin(r_lon)
           r_v(2) = (r_a + r_hgt)*sin(r_lat)               
           
        elseif(i_type .eq. 2)then  !convert vector to lat,lon, hgt
           
           r_p = sqrt(r_v(1)**2 + r_v(2)**2 + (r_v(3)+r_a)**2)
           r_lat = asin(r_v(2)/r_p)
           r_lon = atan2(r_v(1),(r_v(3)+r_a))
           r_hgt = r_p - r_a          
  
        end if
      
        return
        end  

c****************************************************************
c
c       Various curvature functions
c 
c
c****************************************************************
c**
c**	FILE NAME: curvature.f
c**
c**     DATE WRITTEN: 12/02/93
c**
c**     PROGRAMMER:Scott Hensley
c**
c** 	FUNCTIONAL DESCRIPTION: This routine computes the curvature for 
c**     of various types required for ellipsoidal or spherical earth 
c**     calculations.  
c**
c**     ROUTINES CALLED:none
c**  
c**     NOTES: none
c**
c**     UPDATE LOG:
c**
c*****************************************************************

        real*8 function  reast(r_a,r_e2,r_lat)

       	implicit none
        real*8 r_a,r_e2,r_lat
        
        reast = r_a/sqrt(1.d0 - r_e2*sin(r_lat)**2) 
      
        end  

        real*8 function  rnorth(r_a,r_e2,r_lat)

       	implicit none
        real*8 r_a,r_e2,r_lat
        
        rnorth = (r_a*(1.d0 - r_e2))/
     1			(1.d0 - r_e2*sin(r_lat)**2)**(1.5d0) 

        end

        real*8 function  rdir(r_a,r_e2,r_hdg,r_lat)

       	implicit none
        real*8 r_a,r_e2,r_lat,r_hdg,r_re,r_rn,reast,rnorth
        
        r_re = reast(r_a,r_e2,r_lat)
        r_rn = rnorth(r_a,r_e2,r_lat)

        rdir = (r_re*r_rn)/(r_re*cos(r_hdg)**2 + r_rn*sin(r_hdg)**2) 

        end      


c****************************************************************

        subroutine utmtoll(r_a,r_e2,i_zone,a_grid,r_vec,r_lat,
     +     r_lon,i_type) 

c****************************************************************
c**   
c**   FILE NAME: utmtoll.f
c**   
c**   DATE WRITTEN:7/22/93 
c**   
c**   PROGRAMMER:Scott Hensley
c**   
c**   FUNCTIONAL DESCRIPTION: This routine converts between lat
c**   lon and utm coordinates for a datum determined from the input 
c**   a and e2.
c**   
c**   ROUTINES CALLED:none
c**   
c**   NOTES: none
c**   
c**   UPDATE LOG:
c**   
c****************************************************************
        
        implicit none
        
c     INPUT VARIABLES:
        integer i_type                   !1=lat,lon to utm,2= utm to lat,lon
        real*8 r_a                       !ellispoid semi-major axis
        real*8 r_e2                      !ellipsoid eccentricity squared  
        real*8 r_vec(2)                    !Northing,Easting(m)
        real*8 r_lat                     !latitude (deg -90 to 90)
        real*8 r_lon                     !longitude (deg -180 to 180)
        integer i_zone                   !UTM zone
        character*1 a_grid               !UTM North-South grid
   
c       OUTPUT VARIABLES:see input

c       LOCAL VARIABLES:
        integer i_ft,i_gi
        real*8 r_v(2)                    !Northing,Easting(m)
        real*8 pi,r_dtor
        real*8 r_ep2,r_k0,r_k
        real*8 r_fe,r_fn(2)
        real*8 r_e4,r_e6,r_n,r_t,r_t2,r_c,r_c2,r_ba
        real*8 r_a2,r_a3,r_a4,r_a5,r_a6 
        real*8 r_d,r_d2,r_d3,r_d4,r_d5,r_d6
        real*8 r_lon0,r_lat1,r_m,r_m0,r_mu,r_lat0
        real*8 r_et,r_e1,r_e12,r_e13,r_e14,r_r
        character*1 a_griddes(20)

c       DATA STATEMENTS:
        data pi /3.141592653589793238d0/
        data r_dtor /1.74532925199d-2/
        data i_ft /0/
        data a_griddes /'C','D','E','F','G','H','J',
     +       'K','L','M','N','P','Q','R','S','T','U',
     +       'V','W','X'/
        data r_k0 /.9996d0/    !scale at center 
        data r_lat0 /0.d0/
        data r_fe,r_fn /500000.d0,0.d0,10000000.d0/

C       FUNCTION STATEMENTS:none

c       PROCESSING STEPS:

        r_ep2 = r_e2/(1.d0 - r_e2)
        r_e4 = r_e2**2
        r_e6 = r_e2**3
        pi =  4.d0*atan(1.d0)
        r_dtor = pi/180.d0

        if(i_type .eq. 2)then  !convert lat,lon to UTM

           if (i_zone .le. 0) i_zone = int(mod(r_lon+3.d0*pi,2.d0*pi)/(r_dtor*6.d0))
     +          + 1             

           i_zone = max(min(i_zone,60),1)
           r_lon0 = -pi + 6.d0*r_dtor*(i_zone-1) + 3.d0*r_dtor
           
           r_n = r_a/sqrt(1.d0 - r_e2*sin(r_lat)**2)
           r_t = tan(r_lat)**2
           r_t2 = r_t**2
           r_c = r_ep2*cos(r_lat)**2
           r_ba = (r_lon - r_lon0)*cos(r_lat)
           r_a2 = r_ba**2
           r_a3 = r_ba*r_a2 
           r_a4 = r_ba*r_a3
           r_a5 = r_ba*r_a4
           r_a6 = r_ba*r_a5
           r_m = r_a*((1.d0-r_e2/4 - 3.d0*r_e4/64.d0 - 
     +      5.d0*r_e6/256.d0)*r_lat - (3.d0*r_e2/8.d0 + 
     +          3.d0*r_e4/32.d0 + 
     +      45.d0*r_e6/1024.d0)*sin(2.d0*r_lat) +  
     +          (15.d0*r_e4/256.d0 + 
     +      45.d0*r_e6/1024.d0)*sin(4.d0*r_lat) -
     +          (35.d0*r_e6/3072.d0)*
     +      sin(6.d0*r_lat))
           r_m0 = r_a*((1.d0-r_e2/4 - 3.d0*r_e4/64.d0 - 
     +      5.d0*r_e6/256.d0)*r_lat0 - (3.d0*r_e2/8.d0 + 
     +          3.d0*r_e4/32.d0 + 
     +      45.d0*r_e6/1024.d0)*sin(2.d0*r_lat0) + 
     +          (15.d0*r_e4/256.d0 + 
     +      45.d0*r_e6/1024.d0)*sin(4.d0*r_lat0) -
     +          (35.d0*r_e6/3072.d0)*
     +      sin(6.d0*r_lat0))
           
           r_vec(2) = r_k0*r_n*(r_ba+(1.d0-r_t+r_c)*r_a3/6.d0 + 
     +       (5.d0-18.d0*r_t+r_t2+72.d0*r_c-58.d0*r_ep2)*r_a5/120.d0)
           r_vec(2) = r_vec(2) + r_fe

           r_vec(1) = r_k0*(r_m - r_m0 + r_n*tan(r_lat)*
     +       ( r_a2/2.d0 + (5.d0-r_t+9.d0*r_c+4.d0*r_c**2)*
     +        (r_a4/24.d0) + (61.d0-58.d0*r_t+r_t2+600.d0*r_c-
     +        330.d0*r_ep2)*(r_a6/720.d0) ))
           if(r_lat .ge. 0)then
              r_vec(1) = r_vec(1) + r_fn(1)
           else
              r_vec(1) = r_vec(1) + r_fn(2)
           end if

           r_k = r_k0*(1.d0+(1.d0+r_ep2*cos(r_lat)**2)*
     +          (r_vec(2)-r_fe)**2/
     +          (2.d0*(r_k0**2)*r_n**2))

           i_gi = int((r_lat/r_dtor+80.d0)/8.d0) + 1
           i_gi = max(min(i_gi,20),1)
           a_grid = a_griddes(i_gi)
           
        elseif(i_type .eq. 1)then  !convert UTM to lat,lon 

           r_v(1) = r_vec(1)
           r_v(2) = r_vec(2)
           r_v(2) = r_v(2) - r_fe
           if(r_v(1) .ge. r_fn(2))then
              r_v(1) = r_v(1) - r_fn(2)
           end if
           r_lon0 = -pi + 6.d0*r_dtor*(i_zone-1) + 3.d0*r_dtor
           
           r_et = sqrt(1.d0-r_e2)
           r_e1 = (1.d0-r_et)/(1.d0+r_et)
           r_e12 = r_e1**2
           r_e13 = r_e1*r_e12
           r_e14 = r_e1*r_e13
           r_m = r_v(1)/r_k0
           r_mu = r_m/(r_a*(1.d0-r_e2/4.d0-3.d0*r_e4/64.d0-
     +          5.d0*r_e6/256.d0))
           r_lat1 = r_mu + (3.d0*r_e1/2.d0-27.d0*r_e13/32.d0)*
     +          sin(2.d0*r_mu)+
     +          (21.d0*r_e12/16.d0-55.d0*r_e14/32.d0)*sin(4.d0*r_mu)+  
     +          (51.d0*r_e13/96.d0)*sin(6.d0*r_mu) +  
     +          (1097.d0*r_e14/512.d0)*sin(8.d0*r_mu) 

           r_n = r_a/sqrt(1.d0 - r_e2*sin(r_lat1)**2)
           r_r = (r_a*(1.d0-r_e2))/sqrt(1.d0 - r_e2*sin(r_lat1)**2)**3
           r_t = tan(r_lat1)**2
           r_t2 = r_t**2
           r_c = r_ep2*cos(r_lat1)**2
           r_c2 = r_c**2
           r_d = r_v(2)/(r_n*r_k0)
           r_d2 = r_d**2
           r_d3 = r_d2*r_d
           r_d4 = r_d3*r_d
           r_d5 = r_d4*r_d
           r_d6 = r_d5*r_d
 
           r_lat = r_lat1 - (r_n*tan(r_lat1)/r_r)*(r_d2/2.d0+
     +       (5.d0+3.d0*r_t+10.d0*r_c-4.d0*r_c2-9.d0*r_ep2)*
     +          r_d4/24.d0 +
     +       (61.d0+90*r_t+298.d0*r_c+45.d0*r_t2-252.d0*r_ep2-3.d0*
     +          r_c2)*
     +       (r_d6/720.d0))
           r_lon = r_lon0 + (r_d - (1.d0+2.d0*r_t+r_c)*r_d3/6.d0 + 
     +       (5.d0-2.d0*r_c+28.d0*r_t-3.d0*r_c2+8.d0*r_ep2+
     +          24.d0*r_t2)*
     +       (r_d5/120.d0))/cos(r_lat1)

        end if
      
        end  

c****************************************************************

        subroutine enutoll(r_a,r_e2,i_zone,a_grid,r_vec,r_lat,
     +     r_lon,i_type) 

c****************************************************************
c**   
c**   FILE NAME: enutoll.f
c**   
c**   DATE WRITTEN:7/22/93 
c**   
c**   PROGRAMMER:Scott Hensley
c**   
c**   FUNCTIONAL DESCRIPTION: This routine converts between lat
c**   lon and enu coordinates for a datum determined from the input 
c**   a and e2.
c**   
c**   ROUTINES CALLED:none
c**   
c**   NOTES: none
c**   
c**   UPDATE LOG:  added zone selection logic SJS 3/28/96
c**   
c****************************************************************
        
        implicit none
        
c     INPUT VARIABLES:
        integer i_type                   !2=lat,lon to utm,1= utm to lat,lon
        real*8 r_a                       !ellispoid semi-major axis
        real*8 r_e2                      !ellipsoid eccentricity squared  
        real*8 r_vec(2)                  !Northing,Easting(m)
        real*8 r_lat                     !latitude (deg -90 to 90)
        real*8 r_lon                     !longitude (deg -180 to 180)
        integer i_zone                   !UTM zone
        character*1 a_grid               !UTM North-South grid
   
c       OUTPUT VARIABLES:see input

c       LOCAL VARIABLES:
        integer i_ft,i_gi
        real*8 pi,r_dtor
        real*8 r_v(2)                    !Northing,Easting(m)
        real*8 r_ep2,r_k0,r_k
        real*8 r_fe,r_fn(2)
        real*8 r_e4,r_e6,r_n,r_t,r_t2,r_c,r_c2,r_ba
        real*8 r_a2,r_a3,r_a4,r_a5,r_a6 
        real*8 r_d,r_d2,r_d3,r_d4,r_d5,r_d6
        real*8 r_lon0,r_lat1,r_m,r_m0,r_mu,r_lat0
        real*8 r_et,r_e1,r_e12,r_e13,r_e14,r_r
        character*1 a_griddes(20)

c       DATA STATEMENTS:
        data pi /3.141592653589793238d0/
        data r_dtor /1.74532925199d-2/
        data i_ft /0/
        data a_griddes /'C','D','E','F','G','H','J',
     +       'K','L','M','N','P','Q','R','S','T','U',
     +       'V','W','X'/
        data r_k0 /.9996d0/    !scale at center 
        data r_lat0 /0.d0/
        data r_fe,r_fn /500000.d0,0.d0,10000000.d0/

C       FUNCTION STATEMENTS:none

c       PROCESSING STEPS:

        r_ep2 = r_e2/(1.d0 - r_e2)
        r_e4 = r_e2**2
        r_e6 = r_e2**3
        pi =  4.d0*atan(1.d0)
        r_dtor = pi/180.d0

        if(i_type .eq. 2)then  !convert lat,lon to UTM

           if (i_zone .le. 0) i_zone = int(mod(r_lon+3.d0*pi,2.d0*pi)/(r_dtor*6.d0))
     +          + 1             

           i_zone = max(min(i_zone,60),1)
           r_lon0 = -pi + 6.d0*r_dtor*(i_zone-1) + 3.d0*r_dtor
           
           r_n = r_a/sqrt(1.d0 - r_e2*sin(r_lat)**2)
           r_t = tan(r_lat)**2
           r_t2 = r_t**2
           r_c = r_ep2*cos(r_lat)**2
           r_ba = (r_lon - r_lon0)*cos(r_lat)
           r_a2 = r_ba**2
           r_a3 = r_ba*r_a2 
           r_a4 = r_ba*r_a3
           r_a5 = r_ba*r_a4
           r_a6 = r_ba*r_a5
           r_m = r_a*((1.d0-r_e2/4 - 3.d0*r_e4/64.d0 - 
     +      5.d0*r_e6/256.d0)*r_lat - (3.d0*r_e2/8.d0 + 
     +          3.d0*r_e4/32.d0 + 
     +      45.d0*r_e6/1024.d0)*sin(2.d0*r_lat) +  
     +          (15.d0*r_e4/256.d0 + 
     +      45.d0*r_e6/1024.d0)*sin(4.d0*r_lat) -
     +          (35.d0*r_e6/3072.d0)*
     +      sin(6.d0*r_lat))
           r_m0 = r_a*((1.d0-r_e2/4 - 3.d0*r_e4/64.d0 - 
     +      5.d0*r_e6/256.d0)*r_lat0 - (3.d0*r_e2/8.d0 + 
     +          3.d0*r_e4/32.d0 + 
     +      45.d0*r_e6/1024.d0)*sin(2.d0*r_lat0) + 
     +          (15.d0*r_e4/256.d0 + 
     +      45.d0*r_e6/1024.d0)*sin(4.d0*r_lat0) -
     +          (35.d0*r_e6/3072.d0)*
     +      sin(6.d0*r_lat0))
           
           r_vec(1) = r_k0*r_n*(r_ba+(1.d0-r_t+r_c)*r_a3/6.d0 + 
     +       (5.d0-18.d0*r_t+r_t2+72.d0*r_c-58.d0*r_ep2)*r_a5/120.d0)
           r_vec(1) = r_vec(1) + r_fe

           r_vec(2) = r_k0*(r_m - r_m0 + r_n*tan(r_lat)*
     +       ( r_a2/2.d0 + (5.d0-r_t+9.d0*r_c+4.d0*r_c**2)*
     +        (r_a4/24.d0) + (61.d0-58.d0*r_t+r_t2+600.d0*r_c-
     +        330.d0*r_ep2)*(r_a6/720.d0) ))
           if(r_lat .ge. 0)then
              r_vec(2) = r_vec(2) + r_fn(1)
           else
              r_vec(2) = r_vec(2) + r_fn(2)
           end if

           r_k = r_k0*(1.d0+(1.d0+r_ep2*cos(r_lat)**2)*
     +          (r_vec(1)-r_fe)**2/
     +          (2.d0*(r_k0**2)*r_n**2))

           i_gi = int((r_lat/r_dtor+80.d0)/8.d0) + 1
           i_gi = max(min(i_gi,20),1)
           a_grid = a_griddes(i_gi)
           
        elseif(i_type .eq. 1)then  !convert UTM to lat,lon 

           r_v(1) = r_vec(1)
           r_v(2) = r_vec(2)
           r_v(1) = r_v(1) - r_fe
           if(r_v(2) .ge. r_fn(2))then
              r_v(2) = r_v(2) - r_fn(2)
           end if
           r_lon0 = -pi + 6.d0*r_dtor*(i_zone-1) + 3.d0*r_dtor
           
           r_et = sqrt(1.d0-r_e2)
           r_e1 = (1.d0-r_et)/(1.d0+r_et)
           r_e12 = r_e1**2
           r_e13 = r_e1*r_e12
           r_e14 = r_e1*r_e13
           r_m = r_v(2)/r_k0
           r_mu = r_m/(r_a*(1.d0-r_e2/4.d0-3.d0*r_e4/64.d0-
     +          5.d0*r_e6/256.d0))
           r_lat1 = r_mu + (3.d0*r_e1/2.d0-27.d0*r_e13/32.d0)*
     +          sin(2.d0*r_mu)+
     +          (21.d0*r_e12/16.d0-55.d0*r_e14/32.d0)*sin(4.d0*r_mu)+  
     +          (51.d0*r_e13/96.d0)*sin(6.d0*r_mu) +  
     +          (1097.d0*r_e14/512.d0)*sin(8.d0*r_mu) 

           r_n = r_a/sqrt(1.d0 - r_e2*sin(r_lat1)**2)
           r_r = (r_a*(1.d0-r_e2))/sqrt(1.d0 - r_e2*sin(r_lat1)**2)**3
           r_t = tan(r_lat1)**2
           r_t2 = r_t**2
           r_c = r_ep2*cos(r_lat1)**2
           r_c2 = r_c**2
           r_d = r_v(1)/(r_n*r_k0)
           r_d2 = r_d**2
           r_d3 = r_d2*r_d
           r_d4 = r_d3*r_d
           r_d5 = r_d4*r_d
           r_d6 = r_d5*r_d
 
           r_lat = r_lat1 - (r_n*tan(r_lat1)/r_r)*(r_d2/2.d0+
     +       (5.d0+3.d0*r_t+10.d0*r_c-4.d0*r_c2-9.d0*r_ep2)*
     +          r_d4/24.d0 +
     +       (61.d0+90*r_t+298.d0*r_c+45.d0*r_t2-252.d0*r_ep2-3.d0*
     +          r_c2)*
     +       (r_d6/720.d0))
           r_lon = r_lon0 + (r_d - (1.d0+2.d0*r_t+r_c)*r_d3/6.d0 + 
     +       (5.d0-2.d0*r_c+28.d0*r_t-3.d0*r_c2+8.d0*r_ep2+
     +          24.d0*r_t2)*
     +       (r_d5/120.d0))/cos(r_lat1)

        end if
      
        end  

c****************************************************************

	subroutine matvec(r_t,r_v,r_w)

c****************************************************************
c**
c**	FILE NAME: matvec.for
c**
c**     DATE WRITTEN: 7/20/90
c**
c**     PROGRAMMER:Scott Hensley
c**
c** 	FUNCTIONAL DESCRIPTION: The subroutine takes a 3x3 matris 
c**     and a 3x1 vector a multiplies them to return another 3x1
c**	vector.
c**
c**     ROUTINES CALLED:none
c**  
c**     NOTES: none
c**
c**     UPDATE LOG:
c**
c****************************************************************

       	implicit none

c	INPUT VARIABLES:
 	real*8 r_t(3,3)                            !3x3 matris
        real*8 r_v(3)                              !3x1 vector
   
c   	OUTPUT VARIABLES:
        real*8 r_w(3)                              !3x1 vector

c	LOCAL VARIABLES:none

c  	PROCESSING STEPS:

c       compute matris product

	r_w(1) = r_t(1,1)*r_v(1) + r_t(1,2)*r_v(2) + r_t(1,3)*r_v(3)
	r_w(2) = r_t(2,1)*r_v(1) + r_t(2,2)*r_v(2) + r_t(2,3)*r_v(3)
	r_w(3) = r_t(3,1)*r_v(1) + r_t(3,2)*r_v(2) + r_t(3,3)*r_v(3)
          
        return
        end  

c****************************************************************

	subroutine lincomb(r_k1,r_u,r_k2,r_v,r_w)

c****************************************************************
c**
c**	FILE NAME: lincomb.for
c**
c**     DATE WRITTEN: 8/3/90
c**
c**     PROGRAMMER:Scott Hensley
c**
c** 	FUNCTIONAL DESCRIPTION: The subroutine forms the linear combination
c**	of two vectors.
c**
c**     ROUTINES CALLED:none
c**  
c**     NOTES: none
c**
c**     UPDATE LOG:
c**
c*****************************************************************

       	implicit none

c	INPUT VARIABLES:
        real*8 r_u(3)                              !3x1 vector
        real*8 r_v(3)                              !3x1 vector
        real*8 r_k1				 !scalar
        real*8 r_k2				 !scalar
   
c   	OUTPUT VARIABLES:
        real*8 r_w(3)                              !3x1 vector

c	LOCAL VARIABLES:none

c  	PROCESSING STEPS:

c       compute linear combination

	r_w(1) = r_k1*r_u(1) + r_k2*r_v(1)
	r_w(2) = r_k1*r_u(2) + r_k2*r_v(2)
	r_w(3) = r_k1*r_u(3) + r_k2*r_v(3)
      
        return
        end  
        subroutine multitrn(r_trn1,r_trn2,r_trn)

          implicit none

          real*8 r_trn(3,4)
          real*8 r_trn1(3,4)
          real*8 r_trn2(3,4)

            call matmulti(r_trn1,r_trn2,r_trn)
            call vecmulti(r_trn1,r_trn2(1,4),r_trn(1,4))
            call vecaddit(r_trn1(1,4),r_trn(1,4),r_trn(1,4))          

          return
        end

        subroutine invrstrn(r_atm,r_mta)
c
c  This subroutine finds the inverse of an affine transformation   
c                  including the translation vector
c
          implicit none

          real*8 r_atm(3,4)
          real*8 r_mta(3,4)
          real*8 r_tmp(3)
          real*8 r_one

            r_one = -1.0

            call matinvrt(r_atm,r_mta)
            call vecmulti(r_mta,r_atm(1,4),r_tmp)
            call vecscale(r_one,r_tmp,r_mta(1,4))

            return
        end

        subroutine matinvrt(r_a,r_b)

          implicit none

          real*8 a11
          real*8 a12
          real*8 a13
          real*8 a21
          real*8 a22
          real*8 a23
          real*8 a31
          real*8 a32
          real*8 a33

          real*8 r_a(3,3)
          real*8 r_b(3,3)

          real*8 r_dd

          a11=r_a(1,1)
          a12=r_a(1,2)
          a13=r_a(1,3)
          a21=r_a(2,1)
          a22=r_a(2,2)
          a23=r_a(2,3)
          a31=r_a(3,1)
          a32=r_a(3,2)
          a33=r_a(3,3)

          r_dd=a11*(a22*a33-a23*a32)-a12*(a21*a33-a23*a31)+
     &         a13*(a21*a32-a22*a31)

          if (r_dd .ne. 0.) then
            r_b(1,1)=(a22*a33-a23*a32)/r_dd
            r_b(1,2)=(a13*a32-a12*a33)/r_dd
            r_b(1,3)=(a12*a23-a13*a22)/r_dd
            r_b(2,1)=(a23*a31-a21*a33)/r_dd
            r_b(2,2)=(a11*a33-a13*a31)/r_dd
            r_b(2,3)=(a13*a21-a11*a23)/r_dd
            r_b(3,1)=(a21*a32-a22*a31)/r_dd
            r_b(3,2)=(a12*a31-a11*a32)/r_dd
            r_b(3,3)=(a11*a22-a12*a21)/r_dd
          else
            write(6,*) 'Determinant =  0 in Subroutine matinvrt'
            r_b(1,1)=1.
            r_b(1,2)=0.
            r_b(1,3)=0.
            r_b(2,1)=0.
            r_b(2,2)=1.
            r_b(2,3)=0.
            r_b(3,1)=0.
            r_b(3,2)=0.
            r_b(3,3)=1.
          end if

          return

        end

        subroutine matmulti(r_a,r_b,r_c)

          implicit none

          real*8 r_a(3,3)
          real*8 r_b(3,3)
          real*8 r_c(3,3)

          r_c(1,1)=r_a(1,1)*r_b(1,1)+r_a(1,2)*r_b(2,1)+r_a(1,3)*r_b(3,1)
          r_c(1,2)=r_a(1,1)*r_b(1,2)+r_a(1,2)*r_b(2,2)+r_a(1,3)*r_b(3,2)
          r_c(1,3)=r_a(1,1)*r_b(1,3)+r_a(1,2)*r_b(2,3)+r_a(1,3)*r_b(3,3)

          r_c(2,1)=r_a(2,1)*r_b(1,1)+r_a(2,2)*r_b(2,1)+r_a(2,3)*r_b(3,1)
          r_c(2,2)=r_a(2,1)*r_b(1,2)+r_a(2,2)*r_b(2,2)+r_a(2,3)*r_b(3,2)
          r_c(2,3)=r_a(2,1)*r_b(1,3)+r_a(2,2)*r_b(2,3)+r_a(2,3)*r_b(3,3)

          r_c(3,1)=r_a(3,1)*r_b(1,1)+r_a(3,2)*r_b(2,1)+r_a(3,3)*r_b(3,1)
          r_c(3,2)=r_a(3,1)*r_b(1,2)+r_a(3,2)*r_b(2,2)+r_a(3,3)*r_b(3,2)
          r_c(3,3)=r_a(3,1)*r_b(1,3)+r_a(3,2)*r_b(2,3)+r_a(3,3)*r_b(3,3)

          return

        end

        subroutine vecmulti(r_a,r_b,r_c)

          implicit none

          real*8 r_a(3,3)
          real*8 r_b(3)
          real*8 r_c(3)

          r_c(1)=r_a(1,1)*r_b(1)+r_a(1,2)*r_b(2)+r_a(1,3)*r_b(3)
          r_c(2)=r_a(2,1)*r_b(1)+r_a(2,2)*r_b(2)+r_a(2,3)*r_b(3)
          r_c(3)=r_a(3,1)*r_b(1)+r_a(3,2)*r_b(2)+r_a(3,3)*r_b(3)

          return

        end


        subroutine vecscale(r_scale,r_a,r_b)

          implicit none

          real*8 r_scale
          real*8 r_a(3)
          real*8 r_b(3)

            r_b(1)=r_scale*r_a(1)
            r_b(2)=r_scale*r_a(2)
            r_b(3)=r_scale*r_a(3)

            return

        end

        subroutine vecaddit(r_a,r_b,r_c)

          implicit none

          real*8 r_a(3)
          real*8 r_b(3)
          real*8 r_c(3)

            r_c(1)=r_a(1)+r_b(1)
            r_c(2)=r_a(2)+r_b(2)
            r_c(3)=r_a(3)+r_b(3)

            return

        end


#ifdef IO64
        integer*8 function i_eight(i_val)

          implicit none
          integer i_val

          i_eight = i_val

          return
        end
#endif

         subroutine gettgk(a_value,i_tgk,i_tgl,i_clk)
           implicit none

           character*120 a_value
           integer i_tgk
           integer i_tgl
           integer i_clk

            if (a_value(1:1) .eq. '#') then
              read(a_value(2:),*) i_tgk
            else if (a_value .eq. 'sp' .or. a_value .eq. 'space' .or. a_value .eq. 'SPACE') then
              i_tgk = 32
            else if (a_value .eq. 'cntl' .or. a_value .eq. 'control' .or. a_value .eq. 'CONTROL') then
              i_tgk = 65507
            else if (a_value .eq. 'shft' .or. a_value .eq. 'shift' .or. a_value .eq. 'SHIFT') then
              i_tgk = 65505
            else if (a_value .eq. 'shiftl' .or. a_value .eq. 'SHIFTL') then
              i_tgk = 65505
            else if (a_value .eq. 'shiftr' .or. a_value .eq. 'SHIFTR') then
              i_tgk = 65506
            else if (a_value .eq. 'esc' .or. a_value .eq. 'escape' .or. a_value .eq. 'ESCAPE') then
              i_tgk = 65307
            else if (a_value .eq. 'tab' .or. a_value .eq. 'TAB') then
              i_tgk = 65289
            else if (a_value .eq. 'rtn' .or. a_value .eq. 'return' .or. a_value .eq. 'RETURN') then
              i_tgk = 65293
            else if (a_value .eq. 'enter' .or. a_value .eq. 'ENTER') then
              i_tgk = 65421
            else if (a_value .eq. 'bksp' .or. a_value .eq. 'backspace' .or. a_value .eq. 'BACKSPACE') then
              i_tgk = 65288
            else if (a_value .eq. 'extendr' .or. a_value .eq. 'EXTENDR') then
              i_tgk = 65511
            else if (a_value .eq. 'extendl' .or. a_value .eq. 'EXTENDL') then
              i_tgk = 65512
            else if (a_value .eq. 'f1' .or. a_value .eq. 'F1') then
              i_tgk = 65470
            else if (a_value .eq. 'f2' .or. a_value .eq. 'F2') then
              i_tgk = 65471
            else if (a_value .eq. 'f3' .or. a_value .eq. 'F3') then
              i_tgk = 65472
            else if (a_value .eq. 'f4' .or. a_value .eq. 'F4') then
              i_tgk = 65473
            else if (a_value .eq. 'b1' .or. a_value .eq. 'B1') then
              i_tgk = 65478
            else if (a_value .eq. 'b2' .or. a_value .eq. 'B2') then
              i_tgk = 65479
            else if (a_value .eq. 'b3' .or. a_value .eq. 'B3') then
              i_tgk = 65480
            else if (a_value .eq. 'b4' .or. a_value .eq. 'B4') then
              i_tgk = 65481
            else if (a_value .eq. 'left'  .or. a_value .eq. 'LEFT'  .or. a_value .eq. 'arrow1') then
              i_tgk = 65361
            else if (a_value .eq. 'down'  .or. a_value .eq. 'DOWN'  .or. a_value .eq. 'arrow2') then
              i_tgk = 65364
            else if (a_value .eq. 'right' .or. a_value .eq. 'RIGHT' .or. a_value .eq. 'arrow3') then
              i_tgk = 65363
            else if (a_value .eq. 'click' .or. a_value .eq. 'CLICK') then
              i_tgk = 0
            else if (a_value .eq. 'c1' .or. a_value .eq. 'click1' .or. a_value .eq. 'CLICK1') then
              i_tgk = 0
              i_clk = 1
            else if (a_value .eq. 'c2' .or. a_value .eq. 'click2' .or. a_value .eq. 'CLICK2') then
              i_tgk = 0
              i_clk = 2
            else if (a_value .eq. 'c3' .or. a_value .eq. 'click3' .or. a_value .eq. 'CLICK3') then
              i_tgk = 0
              i_clk = 3
            else if (a_value .eq. 'button' .or. a_value .eq. 'BUTTON') then
              i_tgk = 0
            else if (a_value .eq. 'b1' .or. a_value .eq. 'button1' .or. a_value .eq. 'BUTTON1') then
              i_tgk = 0
              i_clk = 1
            else if (a_value .eq. 'b2' .or. a_value .eq. 'button2' .or. a_value .eq. 'BUTTON2') then
              i_tgk = 0
              i_clk = 2
            else if (a_value .eq. 'b3' .or. a_value .eq. 'button3' .or. a_value .eq. 'BUTTON3') then
              i_tgk = 0
              i_clk = 3
            else
              i_tgk = ichar(a_value(1:1))
            end if
            i_tgl = 1
          return
        end


        subroutine write_ps_hdr(i_num,i_orient,i_xmax,i_ymax,i_npsamp,i_npline,i_ctable)
          implicit none
        
          integer i
          integer i_num
          integer i_orient
          integer i_xmax
          integer i_ymax
          integer i_npsamp
          integer i_npline
          integer i_ctable(3,0:255)

          write(30,'(a)') '%!PS-Adobe-3.0'
          if (i_orient .eq. 1) then
            write(i_num,'(a)') '%%BoundingBox: 36 18 540 738'
          else
            write(i_num,'(a)') '%%BoundingBox: 36 36 576 720'
          end if
          write(i_num,'(a)') '%%Title: Graphics produced by DGX'
          write(i_num,'(a)') '%%For: Jet Propulsion Laboratory Section 334'
          write(i_num,'(a)') '%%Creator: DGX v1.0'
          write(i_num,'(a)') '%%CreationDate: Thu Nov 21 16:15:50 1996'
          write(i_num,'(a)') '%%DocumentData: Clean7bit'
          write(i_num,'(a)') '%%Requirements: color'
          write(i_num,'(a)') '%%LanguageLevel: 1'
          write(i_num,'(a)') '%%PageOrder: Ascend'
          write(i_num,'(a)') '%%Pages: (atend)'
          write(i_num,'(a)') '%%DocumentNeededResources: (atend)'
          write(i_num,'(a)') '%%EndComments'
          write(i_num,'(a)') '%%Page: 0 1'
          write(i_num,'(a)') '%%PageRequirements: color'
          write(i_num,'(a)') '%%PageResources: (atend)'
          if (i_orient .eq. 1) then
            write(i_num,'(a)') '%%PageOrientation: Landscape'
            write(i_num,'(a)') '%%PageBoundingBox: 36 18 540 738'
          else
            write(i_num,'(a)') '%%PageOrientation: Portrait'
            write(i_num,'(a)') '%%BoundingBox: 36 36 576 720'
          end if
          write(i_num,'(a)') '%%BeginPageSetup'
          write(i_num,'(a)') 'save'
c          write(i_num,'(a)') 'gsave'
          if (i_orient .eq. 1) then
            write(i_num,'(a)') '36 738 translate 0.0283465 dup scale 270 rotate'
          else
            write(i_num,'(a)') '36 36 translate 0.0283465 dup scale'
          end if
          write(i_num,'(a)') '%%IncludeResource: font Helvetica'
          write(i_num,'(a)') '423.333 /Helvetica findfont exch scalefont setfont'
          write(i_num,'(a)') '%%EndPageSetup'

          write(i_num,'(a)') '/saved_logo_matrix matrix def'
          write(i_num,'(a)') ' '
          write(i_num,'(a)') '/jpl '
          write(i_num,'(a)') ' {	/size_y exch def'
          write(i_num,'(a)') '	/size_x exch def'
          write(i_num,'(a)') '	/y exch def'
          write(i_num,'(a)') '	/x exch def'
          write(i_num,'(a)') '	/savematrix saved_logo_matrix currentmatrix def'
          write(i_num,'(a)') '	x y translate'
          write(i_num,'(a)') '	size_x size_y scale'
          write(i_num,'(a)') '	newpath'
          write(i_num,'(a)') '	0.0 0.0 moveto    % J'
          write(i_num,'(a)') '	0.75 0.0 lineto'
          write(i_num,'(a)') '	0.75 0.25 0.25 270 360 arc'
          write(i_num,'(a)') '	1.0 1.0 lineto'
          write(i_num,'(a)') '	0.6785 1.0 lineto'
          write(i_num,'(a)') '	0.6785 0.35 lineto'
          write(i_num,'(a)') '	0.5785 0.35 0.1 360 270 arcn'
          write(i_num,'(a)') '	0.125 0.25 lineto'
          write(i_num,'(a)') '	0.0 0.0 lineto'
          write(i_num,'(a)') '	1.1 0.0 moveto    % P'
          write(i_num,'(a)') '	1.4125 0.0 lineto'
          write(i_num,'(a)') '	1.4125 0.75 lineto'
          write(i_num,'(a)') '	2.0 0.75 lineto'
          write(i_num,'(a)') '	2.0 0.625 0.125 90 270 arcn'
          write(i_num,'(a)') '	1.4125 0.5 lineto'
          write(i_num,'(a)') '	1.5375 0.25 lineto'
          write(i_num,'(a)') '	2.125 0.25 lineto'
          write(i_num,'(a)') '	2.125 0.5 0.25 270 360 arc'
          write(i_num,'(a)') '	2.375 0.75 lineto'
          write(i_num,'(a)') '	2.125 0.75 0.25 0 90 arc'
          write(i_num,'(a)') '	1.1 1.0 lineto'
          write(i_num,'(a)') '	1.1 0.0 lineto'
          write(i_num,'(a)') '	2.475 1.0 moveto    % L'
          write(i_num,'(a)') '	2.475 0.1875 lineto'
          write(i_num,'(a)') '	2.6625 0.1875 0.1875 180 270 arc'
          write(i_num,'(a)') '	3.5 0.0 lineto'
          write(i_num,'(a)') '	3.375 0.25 lineto'
          write(i_num,'(a)') '	2.7875 0.25 lineto'
          write(i_num,'(a)') '	2.7875 1.0 lineto'
          write(i_num,'(a)') '	closepath'
          write(i_num,'(a)') '	savematrix setmatrix'
          write(i_num,'(a)') ' } def'
          write(i_num,'(a)') ' '
          write(i_num,'(a)') '/nasa'
          write(i_num,'(a)') ' {	/size_y exch def'
          write(i_num,'(a)') '	/size_x exch def'
          write(i_num,'(a)') '	/y exch def'
          write(i_num,'(a)') '	/x exch def'
          write(i_num,'(a)') '	/savematrix saved_logo_matrix currentmatrix def'
          write(i_num,'(a)') '	x y translate'
          write(i_num,'(a)') '	size_x size_y scale'
          write(i_num,'(a)') '	newpath'
          write(i_num,'(a)') '	0.0 0.0 moveto		% Draw the N'
          write(i_num,'(a)') '	0.213836 0.0 lineto'
          write(i_num,'(a)') '	0.264151 0.754717 0.050314 180.0 15.0 arcn'
          write(i_num,'(a)') '	0.735849 0.238994 0.264151 195.0 0.0 arc'
          write(i_num,'(a)') '	1.0 1.0 lineto'
          write(i_num,'(a)') '	0.786164 1.0 lineto'
          write(i_num,'(a)') '	0.735849 0.238994 0.050314 0.0 195.0 arcn'
          write(i_num,'(a)') '	0.264151 0.754717 0.264151 15.0 180.0 arc'
          write(i_num,'(a)') '	0.0 0.0 lineto'
          write(i_num,'(a)') '	0.974843 0.0 moveto	% Draw the A and S'
          write(i_num,'(a)') '	1.201258 0.0 lineto'
          write(i_num,'(a)') '	1.490566 0.779874 0.034591 162.0 18.0 arcn'
          write(i_num,'(a)') '	1.779874 0.0 lineto'
          write(i_num,'(a)') '	2.474214 0.301887 0.301887 270.0 90.0 arc'
          write(i_num,'(a)') '	2.201258 0.698113 0.094340 270.0 90.0 arcn'
          write(i_num,'(a)') '	2.735849 0.792453 lineto'
          write(i_num,'(a)') '	2.735849 1.0 lineto'
          write(i_num,'(a)') '	2.201258 0.698113 0.301887 90.0 270.0 arc'
          write(i_num,'(a)') '	2.474214 0.301887 0.094340 90.0 270.0 arcn'
          write(i_num,'(a)') '	1.934591 0.207547 lineto'
          write(i_num,'(a)') '	1.490566 0.779874 0.251572 18.0 162.0 arc'
          write(i_num,'(a)') '	0.974843 0.0 lineto'
          write(i_num,'(a)') '	2.735849 0.0 moveto	% Draw the A'
          write(i_num,'(a)') '	2.953459 0.0 lineto'
          write(i_num,'(a)') '	3.242767 0.779874 0.034591 162.0 18.0 arcn'
          write(i_num,'(a)') '	3.532075 0.0 lineto'
          write(i_num,'(a)') '	3.749686 0.0 lineto'
          write(i_num,'(a)') '	3.242767 0.779874 0.251572 18.0 162.0 arc'
          write(i_num,'(a)') '	2.735849 0.0 lineto'
          write(i_num,'(a)') '	closepath'
          write(i_num,'(a)') '	savematrix setmatrix'
          write(i_num,'(a)') ' } def'
          if (i_orient .eq. 1) then
            write(i_num,'(a)') '23200 19200 700 700 jpl   fill'
          else
            write(i_num,'(a)') '17500 25500 700 700 jpl   fill'
c            write(i_num,'(a)') '50    24000 700 700 nasa fill'
          end if

          write(i_num,'(a)') 'gsave 0 800 translate /picstr 1 string def '
          write(i_num,'(2i,a)') i_xmax,i_ymax,' scale '
          write(i_num,'(3i,a,6i,a)') i_npsamp,i_npline,8,' [',i_npsamp,0,0,i_npline,0,0,']'
          write(i_num,'(a)') '/COLORTAB < '
       
          write(i_num,'(8(3z2.2,x))') (i_ctable(1,i),i_ctable(2,i),i_ctable(3,i),i=0,255)

          write(i_num,'(a)') '> def'
          write(i_num,'(a)') '{COLORTAB currentfile picstr readhexstring pop 0 get'
          write(i_num,'(a)') '3 mul 3 getinterval } bind false 3 colorimage'

          return
        end

        subroutine write_greeting()

         implicit none

         write(6,*) ' '
         write(6,*) ' '
         write(6,*) ' '
         write(6,*) 'Usage: dgx    rmgfile            samples hgt_scale mag_scale expnt' 
         write(6,*) '       dgx    magfile    dtefile samples hgt_scale mag_scale expnt' 
         write(6,*) '       dgx -x magfile -y dtefile -z zvalue -z zvalue -z zvalue ...' 
         write(6,*) ' '
         write(6,*) 'Where x and y are file format descriptors such as:'
         write(6,*) '       i1 = byte              r4 = real*4          si2 = signed i*2'
         write(6,*) '       i2 = integer*2         c4 = complex*4        c8 = complex*8'
         write(6,*) '       i4 = integer*4        mag = real*4 mag file'
         write(6,*) '       bs = byte swapped i2  dte = real*4 dte file'
         write(6,*) 'Where z is a display descriptor such as:'
         write(6,*) '        s = samples                     h = hdr file name'
         write(6,*) '        d = dte_scale                   f = look down factor'
         write(6,*) '        m = mag_scale                   c = number of colors'
         write(6,*) '        e = exponent                    F = flip x on'
         write(6,*) '        l = start line                  S = display x slope on'
         write(6,*) '        n = number of lines             P = memory backing on'
         write(6,*) '        w = start height wrap           Z = zooms out by 4 at start'
         write(6,*) '    x,  y = center samp,line at start   D = Full dynamic range'
         write(6,*) '  lat,lon = center lat,lon at start'
         write(6,*) ' ' 
         write(6,*) 'hgt_scale = The number of units per color wrap [def=100 (360)]'
         write(6,*) 'mag_scale = The number of std deviations before saturation [def=2]'
         write(6,*) 'expnt     = Power to raise data before displaying [def=1]'
         write(6,'(a,$)') '  <Type RETURN to continue>'
         read(5,*)

         write(6,*) ' '
         write(6,*) ' '
         write(6,*) ' '
         write(6,*) 'When two files are specified, the first file is assumed to be the '
         write(6,*) 'amplitude data and the second file is height data unless specified'
         write(6,*) 'explicitly with -mag or -dte.'
         write(6,*) ' '
         write(6,*) 'Input parameters identified by dash descriptors can be entered in' 
         write(6,*) 'any order, including data file names.'
         write(6,*) ' '
         write(6,*) 'A dash in any field except the width will result in the default '
         write(6,*) 'value for that field.'
         write(6,*) ' '
         write(6,*) 'The line, sample, height and magnitude of a pixel can be displayed'
         write(6,*) 'by clicking on the desired pixel with the left mouse button.  For'
         write(6,*) 'files containing a Standard AIRSAR header or having an associated '
         write(6,*) 'IFPROC header file, the pixel latitude and longitude can also be'
         write(6,*) 'obtained by holding the space bar down while clicking.'
         write(6,*) ' '   
         write(6,*) 'To obtain a Zoom window of the region around a pixel, click on the'
         write(6,*) 'pixel with the center mouse button.  The zoom factor can then be '
         write(6,*) 'adjusted by powers of 2 using the provided buttons.  To select a'
         write(6,*) 'new zoom region, use the center button to again click on a pixel in '
         write(6,*) 'either window.  The contents of the zoom window can be converted to '
         write(6,*) 'Jpeg, PPM, or PostScript and saved to a file by clicking Print with'
         write(6,*) 'the left, middle or right mouse buttons respectively. '
         write(6,'(a,$)') '  <Type RETURN to continue>'
         read(5,*)

         write(6,*) ' '
         write(6,*) ' '
         write(6,*) 'If the number of colors are not specified, dgx will only allocate'
         write(6,*) 'positions in the color table which are not currently in use. This'
         write(6,*) 'preserves the window color when switching between applications, '
         write(6,*) 'but when several programs are competing for the colors, there may'
         write(6,*) 'not be enough left for dgx to run.  Specifing the -c parameter'
         write(6,*) 'enables dgx to allocate as many colors as desired so it can run'
         write(6,*) 'and still maintain sufficient dynamic range in the display.'
         write(6,*) ' '
         write(6,*) 'dgx has two ways to handle window updates.  The default method '
         write(6,*) 'only draws the visable portion of a window to the screen device.'
         write(6,*) 'As the window is scrolled, the data shifting out of the visable'
         write(6,*) 'region is discarded and the program grabs new data from disk to  '
         write(6,*) 'fill the exposed region. The advantage of this method is that it '
         write(6,*) 'requires verly little memory at both the display client and server.  '
         write(6,*) 'The disadvantage is that it is slow to move around.'
         write(6,*) ' '
         write(6,*) 'By specifing the -P flag, dgx loads an entire data set to be '
         write(6,*) 'displayed into the local display device and shifts the job of '
         write(6,*) 'updating the windows to it.  This greatly increases the scroll'
         write(6,*) 'speed but requires time upfront to transfer the image.  It also'
         write(6,*) 'requires enough memory in the local display to hold an 8 bit map'
         write(6,*) 'of the image.  No check of the memory size is made by dgx and it'
         write(6,*) 'may crash if the available memory is not sufficient.'
         write(6,'(a,$)') '  <Type RETURN to continue>'
         read(5,*)

         write(6,*) ' '
         write(6,*) ' '
         write(6,*) ' '
         write(6,*) 'Dgx''s default display scheme maps data from the first pixel in '
         write(6,*) 'the file to the the upper left corner of the display with sample '
         write(6,*) 'number increasing from left to right across the screen and line '
         write(6,*) 'number increasing from top to bottom down the screen.  This '
         write(6,*) 'mapping produces properly oriented images and dtes for left looking'
         write(6,*) 'radar systems processed through the IFPROC or similiar processors.'
         write(6,*) 'If the data is from a right looking sensor, or was processed on '
         write(6,*) 'some other processor, it may require a flip in one direction to '
         write(6,*) 'properly display it.  The -F flag swaps the sample order so that '
         write(6,*) 'the first pixel is in the upper right corner and samples increase '
         write(6,*) 'from right to left.'
         write(6,*) ' '
         write(6,*) 'When no amplitude data is available for a dte, the -S flag can be'
         write(6,*) 'used to display a central difference slope estimate of the surface'
         write(6,*) 'along with the height.  The slopes give the appearance similiar to'
         write(6,*) 'radar illumination from the left side of the screen and can make'
         write(6,*) 'comparisons to radar data much easier.'
         write(6,*) ' '
         write(6,*) 'If integer data files do not have a header but do require scaling '
         write(6,*) 'for proper display of magnitude and height values, the following'
         write(6,*) 'parameter descriptors can be specified along with their values:'
         write(6,*) '          mm = mag multiplier    md = dte multiplier'
         write(6,*) '          am = mag additive      ad = dte additive  '
         write(6,'(a,$)') '  <Type RETURN to continue>'
         read(5,*)

         write(6,*) ' '
         write(6,*) ' '
         write(6,*) ' '
         write(6,*) 'To center the display on a particular pixel at startup, use the -x '
         write(6,*) 'and -y descriptors, followed by the desired sample and line numbers '
         write(6,*) 'respectively.  If a header file is present, the image can also be '
         write(6,*) 'positioned to a specific latitude and lonitude by using the -lat '
         write(6,*) 'and -lon descriptors followed by the respective values in degrees.'
         write(6,*) ' '
         write(6,*) 'Once the image is visible, the main window can be moved to a '
         write(6,*) 'specific pixel location by clicking on the position label at the '
         write(6,*) 'top of the window and entering the desired pixel in the dialog '
         write(6,*) 'box.  To select the center lat/lon, hold the space bar down while'
         write(6,*) 'clicking on the label and enter the latitude and longitude in the'
         write(6,*) 'dialog box.'
         write(6,*) ' '
         write(6,*) 'To move the main window to the center of the zoom window, hold down'
         write(6,*) 'the space bar and click on the zoom window with the middle mouse '
         write(6,*) 'button.'
         write(6,*) ' '
         write(6,*) 'To change the amplitude or height scaling while viewing the image,'
         write(6,*) 'click on the amplitude of height selection button with the right'
         write(6,*) 'mouse button and enter the new parameters in the dialog box.'
         write(6,*) ' '
         write(6,*) 'For images with both amplitude and height, one can switch between'
         write(6,*) 'the amplitude and the x slope by holding down the space bar and '
         write(6,*) 'clicking the amplitude button.'
         write(6,'(a,$)') '  <Type RETURN to continue>'
         read(5,*)

         write(6,*) ' '
         write(6,*) ' '
         write(6,*) ' '
         write(6,*) 'Selecting the Amplitude or Height buttons with the left mouse '
         write(6,*) 'button will switch back and forth between the displays quickly, '
         write(6,*) 'but will use a reduced dynamic range.  Using the center button '
         write(6,*) 'will take longer, but will use the full color table.  The default '
         write(6,*) 'at startup is to use the reduce dynamic range, but this can be '
         write(6,*) 'overriden by  specifing -D on the command line at startup.'
         write(6,*) ' '
         write(6,*) 'To display the slope of the amplitude data instead of the height, '
         write(6,*) 'hold down the number 1 and click on the amplitude button.  To get '
         write(6,*) 'back to the slope of the height, hold the number 2 and click on  '
         write(6,*) 'the amplitude button.'
         write(6,*) ' '
         write(6,*) '  To change the level of debug statements that are displayed '
         write(6,*) 'during execution, use the -debug flag, followed by the a number. '
         write(6,*) 'The value can range from 0 (for no debug info) to 6 (for the '
         write(6,*) 'maximum info).  The default value is 2. '
         write(6,*) ' '
         write(6,*) 'Please forward any comments or suggestions '
         write(6,*) 'regarding DGX to:  Scott.Shaffer@jpl.nasa.gov    '
         write(6,*) ' '
         write(6,*) ' '
         write(6,*) ' '
         write(6,*) ' '
         write(6,*) ' '
         write(6,*) ' '
         write(6,*) ' '
         return
        end
