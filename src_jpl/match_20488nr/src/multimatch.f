!!TRACE-
c ******************************************************************************
        program multimatch3
c
c       FILE NAME: multimatch.f
c   
c       DATE WRITTEN: 08/01/96 
c  
c       PROGRAMMER: Scott Shaffer
c  
c       FUNCTIONAL DESCRIPTION: Matches two SCH image files together by first
c                               reprojecting the second image into the first
c                               images coordinates and then doing a normalize
c                               cross correlation.
c                
c  
c       ROUTINES CALLED: 
c 
c       NOTES: none
c     
c       UPDATE LOG:
c       
c ******************************************************************************

        implicit none

        integer I_MIF
        integer I_MXS
        integer I_MFL
        integer I_BSS
        integer I_BSL

        real*8  R_MIN
        real*8  R_MAX

c       PARAMETER STATEMENTS:
        parameter (I_MIF=1)             ! Max number of input files
        parameter (I_MXS=40960)         ! Max samples in i/o files
        parameter (I_MFL=50)            ! Max feather width in pix
        parameter (I_BSS=512)           ! Buffer size in X
        parameter (I_BSL=512)           ! Buffer size in Y
        parameter (R_MIN= -500)         ! Minimum valid altitude
        parameter (R_MAX= 9000)         ! Maximum valid altitude        

c       VARIABLES:
        character*1   a_grid              ! UTM identifier
        character*255 a_cmdfile           ! Command file name
        character*255 a_rmgfile(0:I_MIF)  ! Data file names
        character*255 a_magfile(0:I_MIF)  ! Data file names
        character*255 a_dtefile(0:I_MIF)  ! Data file names
        character*255 a_afffile(0:I_MIF)  ! Affine transformation files
        character*255 a_hdrfile(0:I_MIF)  ! Header file
        character*255 a_type(0:I_MIF)     ! File type
        character*255 a_vals(10)
        character*255 a_input
        character*255 a_outfile
        character*255 a_value

        integer*4 i_vals
        integer*4 i_ssize(0:I_MIF)      ! Number of samples in input files
        integer*4 i_lsize(0:I_MIF)      ! Number of lines in input files

        integer*4 i_fln                 ! Feather length around gaps
        integer*4 i_nof                 ! Number of files to mosaic
        integer*4 i_gpc                 ! Number of gap pixels found in Block
        integer*4 i_set					! Data set number to match on

        integer*4 i_oloc1(2,I_MXS)


        integer*4 i
        integer*4 j
        integer*4 k
        integer*4 l
        integer*4 is
        integer*4 il
        integer*4 iz
        integer*4 iss
        integer*4 ill
        integer*4 i_bs
        integer*4 i_bl
        integer*4 i_osoff
        integer*4 i_oloff
        integer*4 i_osnum
        integer*4 i_olnum
        integer*4 i_isoff
        integer*4 i_iloff
        integer*4 i_isnum
        integer*4 i_ilnum
        integer*4 i_file
        integer*4 i_sum
        integer*4 i_cnt
        integer*4 i_flg
        integer*4 i_smin
        integer*4 i_smax
        integer*4 i_lmin
        integer*4 i_lmax
        integer*4 i_err
        integer*4 i_pos

        integer*4 i_gps
        integer*4 i_gpl

        integer*4 i_fstln
        integer*4 i_lstln
        integer*4 i_fstsp
        integer*4 i_lstsp

        integer*4 i_zone(0:I_MIF)
        integer*4 i_type(0:I_MIF)
        integer*4 i_mbytes(0:I_MIF)                      ! Number of bytes per pixel in mag file
        integer*4 i_dbytes(0:I_MIF)                      ! Number of bytes per pixel in dte file

        integer*4 i_moff(0:I_MIF)
        integer*4 i_doff(0:I_MIF)

        integer*4 i_mlsize
        integer*4 i_mssize
        integer*4 i_dlsize
        integer*4 i_dssize

        integer*4 i_isfac
        integer*4 i_ilfac
        integer*4 i_ifacmin

        integer*4 i_comma

        real*4 r_data(I_MXS)
        real*4 r_datb(I_MXS)
        real*4 r_ibufa(3*I_BSS,3*I_BSL)  
        real*4 r_ibufb(3*I_BSS,3*I_BSL)  
        real*4 r_obufa(1*I_BSS,1*I_BSL)  
        real*4 r_obufb(1*I_BSS,1*I_BSL)  
        real*4 r_sbuff(1*I_BSS,1*I_BSL)  
        real*4 r_scale(1*I_BSS,1*I_BSL)

        real*4 r_dmul
        real*4 r_dadd

        real*8 r_iat(3,4)
        real*8 r_inv(3,4)
        real*8 r_aff(3,4)
        real*8 r_tmp(3,4)

        real*8 r_atm(3,4,0:I_MIF)
        real*8 r_mta(3,4,0:I_MIF)
        real*8 r_dist(-I_MFL:I_MFL,-I_MFL:I_MFL)

        real*8 r_a
        real*8 r_e2
        real*8 r_peg(3,0:I_MIF)
        real*8 r_rad(0:I_MIF)
        real*8 r_lat
        real*8 r_lon
        real*8 r_hgt

        real*8 r_spc(2,0:I_MIF)
        real*8 r_str(2,0:I_MIF)

        real*8 r_aa
        real*8 r_bb
        real*8 r_mdnc(2,0:I_MIF)
        real*8 r_ddnc(2,0:I_MIF)

        integer*2 i_data2(I_MXS)
        integer*2 i_datb2(I_MXS)

        byte b_data1(I_MXS)
        byte b_datb1(I_MXS)

        real*8 r_pi
        real*8 r_rtod

        real*8 v_oloc(3)
        real*8 v_oloc1(3)
        real*8 v_oloc2(3)
        real*8 v_oloc3(3)
        real*8 v_iloc(3)
        real*8 v_iloc1(3)
        real*8 v_iloc2(3)
        real*8 v_iloc3(3)

        real*8 v_tmp(3)

        real*8 v_pnt(4)
        real*8 v_olocc(4,2,I_MXS)


        integer i_shiftx,i_shifty

        real*4 r_shfty,r_shftx,r_peak,r_meani,r_meanj
        real*4 r_stdvi,r_stdvj,r_noise,r_cov(3),r_eval1
        real*4 r_eval2,r_evec1(2),r_evec2(2)
        real*4 r_covth,r_snrth

        integer i_clrs
        integer i_flag,i_edge(2)

        integer i_x,i_xx
        integer i_y,i_yy

        integer i_rws
        integer i_sdx
        integer i_sdy
        integer i_mdx
        integer i_mdy
        integer i_dlx
        integer i_dly
        integer i_stx
        integer i_sty
        integer i_spx
        integer i_spy

        real r_imgi(I_BSS,I_BSL)
        real r_imgj(I_BSS,I_BSL)
        real r_imgc(I_BSS,I_BSL)

        INTEGER I_NX
        INTEGER I_NY
        INTEGER I_PX
        INTEGER I_PY
        INTEGER I_PZX
        INTEGER I_PZY
        INTEGER I_CX
        INTEGER I_CY
        INTEGER I_CZX
        INTEGER I_CZY
        INTEGER I_LOOP
        INTEGER I_NWMAX

        INTEGER I_XP
        INTEGER I_YP

        INTEGER I_NP
        INTEGER I_ZOOM
        INTEGER I_ZOOMC
        INTEGER I_XDISP
        INTEGER I_YDISP

        REAL R_CPEAK
        REAL R_SHFTXX
        REAL R_SHFTYY

        REAL R_X(2)
        REAL R_Y(2)

        REAL R_VAL
        REAL R_PIX(100000)

        INTEGER I_FLAGC
        CHARACTER*80 A_TEMP
c
c       Display variables
c
        integer i_gx
        integer i_gxi			! Number of windows
        integer i_wxs(0:10)		! Size of window canvas in x direction
        integer i_wys(0:10)		! Size of window canvas in y direction
        integer i_frx(0:10)
        integer i_fry(0:10)
        integer i_typ(0:10)
        integer i_event(10)
        integer i_scale

        character*80 a_lbl(0:10)
        character*80 a_flag

        integer i_pause

        real*8 r_sum
        real*8 r_sqr
        real*8 r_avg
        real*8 r_std

        integer i_cnta
        real r_outside
        real r_snr


        real r_dat(512,512)
        real r_dat1(512,512)
        real r_dat2(512,512)

        LOGICAL L_TDISP
        LOGICAL L_IDISP
        LOGICAL L_MDISP
        LOGICAL L_MDUPP
        LOGICAL L_ITOGL
        LOGICAL L_MTOGL
        LOGICAL L_TTOGL
        LOGICAL L_FMTOT
        LOGICAL L_FLAGI
        LOGICAL L_FLAGJ

        DATA L_ITOGL/.TRUE./
        DATA L_MTOGL/.TRUE./
        DATA L_TTOGL/.TRUE./

c       FUNCTIONS:
        integer i_inarg,iargc
        integer length
        external length
        real*8 rdir
        external rdir

        save r_ibufa
        save r_ibufb
        save r_obufa
        save r_obufb
        save r_sbuff
        save r_scale
        save v_oloc1
        save i_oloc1
        save r_imgi
        save r_imgj
        save r_imgc
        save r_data
        save r_datb
        save i_data2
        save i_datb2
        save b_data1
        save b_datb1
        save v_olocc

        write(6,*) ' '
        write(6,*) '     << Multimatch Program   Version 20.0   7/1/98 >>    '
        write(6,*) ' '

c
c  Initialize pi and conversions
c
        r_pi = 4.d0*atan(1.0d0)
        r_rtod = 180.0d0/r_pi
c
c  Initialize datum stuff
c
        r_a =  6378137.0
        r_e2 = 0.00669438

c
c  Initialize distance lookup array
c
        do is = 0,I_MFL
          do il = 0,I_MFL
            r_aa=max(sqrt(float(is)**2 + float(il)**2),0.5)
            r_dist( is, il) = r_aa
            r_dist(-is, il) = r_aa
            r_dist( is,-il) = r_aa
            r_dist(-is,-il) = r_aa
          end do
        end do 

c
c  Initialize Mosaic Dimensions
c
        i_fstsp= 999999999
        i_lstsp=-999999999
        i_fstln= 999999999
        i_lstln=-999999999

c
c  Initialize input variables
c
        i_fln = 0
        i_set=1
        i_rws = 64
        i_sdx = 8
        i_sdy = 6
        i_mdx = 0
        i_mdy = 0
        i_dlx = 100
        i_dly = 200 
        i_sty = 0
        i_spy = 0
        i_stx = 0
        i_spx = 0
        i_ifacmin=1
        do i_file = 0,I_MIF
          r_peg(1,i_file) = 0.
          r_peg(2,i_file) = 0.
          r_peg(3,i_file) = 0.
          r_str(1,i_file) = 0.
          r_str(2,i_file) = 0.
          r_spc(1,i_file) = 1.
          r_spc(2,i_file) = 1.
          a_rmgfile(i_file) = ' '
          a_magfile(i_file) = ' '
          a_dtefile(i_file) = ' '
          a_afffile(i_file) = ' '
          a_hdrfile(i_file) = ' '
          a_type(i_file) = 'sch'
          i_zone(i_file)=0
          i_ssize(i_file)=0
          i_lsize(i_file)=0
          i_mbytes(i_file)=4
          i_dbytes(i_file)=4
          r_mdnc(1,i_file)=1.0
          r_mdnc(2,i_file)=0.0
          r_ddnc(1,i_file)=1.0
          r_ddnc(2,i_file)=0.0
          i_moff(i_file) = 0
          i_doff(i_file) = 0
        end do

c
c  Initialize graphics stuff
c
        L_TDISP = .true.
        L_IDISP = .true.
        L_MDISP = .true.
        i_clrs = 0

c
c  Get command file name
c
        i_inarg = iargc()
        if (i_inarg .eq. 0)then
           write(6,*) 'Usage: multimatch cmdfile [-notext] [-noimage] [-nomap] [-noall]'
           stop
           write(6,*) ' '
           write(6,'(x,a,$)') 'Enter command filename: '
           read(5,'(a)') a_cmdfile
        else
           call getarg(1,a_cmdfile)
           do i=2,i_inarg
             call getarg(i,a_flag)
             if (a_flag .eq. '-noimage' .or. a_flag .eq. '-NOIMAGE') then
                L_IDISP = .false.
             end if
             if (a_flag .eq. '-notext' .or. a_flag .eq. '-NOTEXT') then
                L_TTOGL = .false.
             end if
             if (a_flag .eq. '-nomap' .or. a_flag .eq. '-NOMAP') then
                L_MDISP = .false.
             end if
             if (a_flag .eq. '-noall' .or. a_flag .eq. '-NOALL') then
                L_TDISP = .false.
                L_IDISP = .false.
                L_MDISP = .false.
             end if
             if (a_flag .eq. '-c' .or. a_flag .eq. '-color') then
               if (i_inarg .ge. i+1) then
                 call getarg(i+1,a_value)
                 read(a_value,*) i_clrs
               end if
             end if
           end do
         
        end if

c
c  Read Command file
c
        write(6,*) ' '
        write(6,*) 'Opening cmd input  file: ',a_cmdfile(1:52)
        open(unit=20,file=a_cmdfile,status='old',form='formatted',readonly)

        read(20,'(a)',end=110) a_outfile
        i_err = 0
        i_file = -1
        do while(i_err .eq. 0)
          read(20,'(a)',end=110) a_input
          call parse(a_input,i_vals,a_vals)     
          do i = 1,i_vals
            if (a_vals(i) .eq. ' ') then
              type *,'parse error'
            else if (a_vals(i)(1:4) .eq. 'rws=') then
              read(a_vals(i)(5:),*) i_rws
            else if (a_vals(i)(1:7) .eq. 'rwsize=') then
              read(a_vals(i)(8:),*) i_rws
            else if (a_vals(i)(1:7) .eq. 'srchxy=') then
              read(a_vals(i)(8:),*) i_sdx,i_sdy
            else if (a_vals(i)(1:4) .eq. 'sdx=') then
              read(a_vals(i)(5:),*) i_sdx
            else if (a_vals(i)(1:4) .eq. 'sdy=') then
              read(a_vals(i)(5:),*) i_sdy
            else if (a_vals(i)(1:7) .eq. 'meanxy=') then
              read(a_vals(i)(8:),*) i_mdx,i_mdy
            else if (a_vals(i)(1:4) .eq. 'mdx=') then
              read(a_vals(i)(5:),*) i_mdx
            else if (a_vals(i)(1:4) .eq. 'mdy=') then
              read(a_vals(i)(5:),*) i_mdy
            else if (a_vals(i)(1:7) .eq. 'skipxy=') then
              read(a_vals(i)(8:),*) i_dlx,i_dly
            else if (a_vals(i)(1:4) .eq. 'dlx=') then
              read(a_vals(i)(5:),*) i_dlx
            else if (a_vals(i)(1:4) .eq. 'dly=') then
              read(a_vals(i)(5:),*) i_dly
            else if (a_vals(i)(1:7) .eq. 'strtxy=') then
              read(a_vals(i)(8:),*) i_stx,i_sty
            else if (a_vals(i)(1:4) .eq. 'stx=') then
              read(a_vals(i)(5:),*) i_stx
            else if (a_vals(i)(1:4) .eq. 'sty=') then
              read(a_vals(i)(5:),*) i_sty
            else if (a_vals(i)(1:7) .eq. 'stopxy=') then
              read(a_vals(i)(8:),*) i_spx,i_spy
            else if (a_vals(i)(1:4) .eq. 'spx=') then
              read(a_vals(i)(5:),*) i_spx
            else if (a_vals(i)(1:4) .eq. 'spy=') then
              read(a_vals(i)(5:),*) i_spy
            else if (a_vals(i)(1:7) .eq. 'setnum=') then
              read(a_vals(i)(8:),*) i_set
            else if (a_vals(i)(1:4) .eq. 'fac=') then
              read(a_vals(i)(5:),*) i_ifacmin
            else if (a_vals(i)(1:4) .eq. 'typ=') then
              a_type(i_file)=a_vals(i)(5:)
            else if (a_vals(i)(1:4) .eq. 'mss=') then
              read(a_vals(i)(5:),*) r_mdnc(1,i_file),r_mdnc(2,i_file)
            else if (a_vals(i)(1:4) .eq. 'dss=') then
              read(a_vals(i)(5:),*) r_ddnc(1,i_file),r_ddnc(2,i_file)
            else if (a_vals(i)(1:4) .eq. 'mbp=') then
              read(a_vals(i)(5:),*) i_mbytes(i_file)
            else if (a_vals(i)(1:4) .eq. 'dbp=') then
              read(a_vals(i)(5:),*) i_dbytes(i_file)
            else if (a_vals(i)(1:4) .eq. 'siz=') then
              read(a_vals(i)(5:),*) i_ssize(i_file),i_lsize(i_file)
            else if (a_vals(i)(1:4) .eq. 'off=') then
              read(a_vals(i)(5:),*) r_str(1,i_file),r_str(2,i_file)
            else if (a_vals(i)(1:4) .eq. 'spc=') then
              read(a_vals(i)(5:),*) r_spc(1,i_file),r_spc(2,i_file)
            else if (a_vals(i)(1:4) .eq. 'peg=') then
              read(a_vals(i)(5:),*) r_peg(1,i_file),r_peg(2,i_file),r_peg(3,i_file)
              r_peg(1,i_file) = r_peg(1,i_file)/r_rtod
              r_peg(2,i_file) = r_peg(2,i_file)/r_rtod
              r_peg(3,i_file) = r_peg(3,i_file)/r_rtod
            else if (a_vals(i)(1:4) .eq. 'zon=') then
              read(a_vals(i)(5:),*) i_zone(i_file)
            else if (a_vals(i)(1:4) .eq. 'hdr=') then
              a_hdrfile(i_file)=a_vals(i)(5:)
            else if (a_vals(i)(1:4) .eq. 'aff=') then
              a_afffile(i_file)=a_vals(i)(5:)
            else if (a_vals(i)(1:4) .eq. 'amp=' .or. 
     &               a_vals(i)(1:4) .eq. 'mag=' .or.
     &               a_vals(i)(1:4) .eq. 'pwr=' ) then
              i_file=i_file+1
              a_magfile(i_file)=a_vals(i)(5:)
              i_pos = index(a_magfile(i_file),'.')
              if (i_pos .eq. 0) i_pos = length(a_magfile(i_file))+1
              a_hdrfile(i_file)=a_magfile(i_file)(1:i_pos-1)//'.hdr'
            else if (a_vals(i)(1:4) .eq. 'dte=' .or. 
     &               a_vals(i)(1:4) .eq. 'dem=' .or.
     &               a_vals(i)(1:4) .eq. 'hgt=' ) then
              i_file=i_file+1
              a_dtefile(i_file)=a_vals(i)(5:)
              i_pos = index(a_dtefile(i_file),'.')
              if (i_pos .eq. 0) i_pos = length(a_dtefile(i_file))+1
              a_hdrfile(i_file)=a_dtefile(i_file)(1:i_pos-1)//'.hdr'
            else if (a_vals(i)(1:4) .eq. 'm*1=' ) then
              i_file=i_file+1
              a_magfile(i_file)=a_vals(i)(5:)
              i_pos = 0
              do while (index(a_magfile(i_file)(i_pos+1:),'.') .ne. 0)
                i_pos = i_pos + index(a_magfile(i_file)(i_pos+1:),'.')
              end do
              if (i_pos .gt. 0) then
                a_hdrfile(i_file)=a_magfile(i_file)(1:i_pos)//'hdr'
              else
                a_hdrfile(i_file)=a_magfile(i_file)(:length(a_magfile(i_file)))//'.hdr'
              end if
              i_mbytes(i_file)=1
            else if (a_vals(i)(1:4) .eq. 'd*1=' ) then
              i_file=i_file+1
              a_dtefile(i_file)=a_vals(i)(5:)
              i_pos = 0
              do while (index(a_dtefile(i_file)(i_pos+1:),'.') .ne. 0)
                i_pos = i_pos + index(a_dtefile(i_file)(i_pos+1:),'.')
              end do
              if (i_pos .gt. 0) then
                a_hdrfile(i_file)=a_dtefile(i_file)(1:i_pos)//'hdr'
              else
                a_hdrfile(i_file)=a_dtefile(i_file)(:length(a_dtefile(i_file)))//'.hdr'
              end if
              i_dbytes(i_file)=1
            else if (a_vals(i)(1:4) .eq. 'dma=' .or. 
     &               a_vals(i)(1:4) .eq. 'd*2=' ) then
              i_file=i_file+1
              a_dtefile(i_file)=a_vals(i)(5:)
              i_pos = 0
              do while (index(a_dtefile(i_file)(i_pos+1:),'.') .ne. 0)
                i_pos = i_pos + index(a_dtefile(i_file)(i_pos+1:),'.')
              end do
              if (i_pos .gt. 0) then
                a_hdrfile(i_file)=a_dtefile(i_file)(1:i_pos)//'hdr'
              else
                a_hdrfile(i_file)=a_dtefile(i_file)(:length(a_dtefile(i_file)))//'.hdr'
              end if
              i_dbytes(i_file)=2
              a_type(i_file)='eqa'
            else if (a_vals(i)(1:4) .eq. 'mgh=') then
              i_file=i_file+1
              i_comma=index(a_vals(i),',')
              a_magfile(i_file)=a_vals(i)(5:i_comma-1)
              a_dtefile(i_file)=a_vals(i)(i_comma+1:)
              i_pos = index(a_dtefile(i_file),'.')
              if (i_pos .eq. 0) i_pos = length(a_dtefile(i_file))+1
              a_hdrfile(i_file)=a_dtefile(i_file)(1:i_pos-1)//'.hdr'
            else if (a_vals(i)(1:4) .eq. 'rmg=') then
              i_file=i_file+1
              a_rmgfile(i_file)=a_vals(i)(5:)
              i_pos = index(a_rmgfile(i_file),'.')
              if (i_pos .eq. 0) i_pos = length(a_rmgfile(i_file))+1
              a_hdrfile(i_file)=a_rmgfile(i_file)(1:i_pos-1)//'.hdr'
            else
              i_file=i_file+1
              a_rmgfile(i_file)=a_vals(i)
              i_pos = index(a_rmgfile(i_file),'.')
              if (i_pos .eq. 0) i_pos = length(a_rmgfile(i_file))+1
              a_hdrfile(i_file)=a_rmgfile(i_file)(1:i_pos-1)//'.hdr'
            end if
          end do
        end do

110     close(20)
        i_nof=i_file

        write(6,*) ' '
        if (i_nof .lt.  0) stop '***** Error - No files specified'
        if (i_nof .eq.  0) stop '***** Error - Only one input file specified'
        if (i_nof .gt. I_MIF) stop '***** Error - Too many input files, input unit numbers may stomp on each other'

        open(15,file=a_outfile,status='unknown')
c
c  Compute transform matricies
c
        do i_file = 0,i_nof

          if (a_afffile(i_file) .ne. ' ') then  ! Read incremental affine transformation
            open(unit=40,file=a_afffile(i_file),form='formatted',status='old')
            read(40,*)
            read(40,*)
            read(40,*)
            read(40,*) r_aff(1,1),r_aff(1,2),r_aff(1,3)
            read(40,*) r_aff(2,1),r_aff(2,2),r_aff(2,3)
            read(40,*) r_aff(3,1),r_aff(3,2),r_aff(3,3)
            read(40,*)
            read(40,*) r_aff(1,4),r_aff(2,4),r_aff(3,4)
            close(40)
          else
            r_aff(1,1) = 1.
            r_aff(1,2) = 0.
            r_aff(1,3) = 0.
            r_aff(1,4) = 0.
            r_aff(2,1) = 0.
            r_aff(2,2) = 1.
            r_aff(2,3) = 0.
            r_aff(2,4) = 0.
            r_aff(3,1) = 0.
            r_aff(3,2) = 0.
            r_aff(3,3) = 1.
            r_aff(3,4) = 0.
          end if

          if (a_hdrfile(i_file) .ne. ' ') then  ! Read incremental affine transformation
            call read_hdr(a_hdrfile(i_file),a_type(i_file),i_lsize(i_file),i_ssize(i_file),
     &            r_peg(1,i_file),i_zone(i_file),r_str(1,i_file),r_spc(1,i_file),
     &            i_mbytes(i_file),i_dbytes(i_file),
     &            r_mdnc(1,i_file),r_ddnc(1,i_file),i_err)
          end if

          if (a_magfile(i_file) .ne. ' ')  then
            call read_mhdr(a_magfile(i_file),i_mlsize,i_mssize,i_mbytes(i_file),i_moff(i_file))
            if (i_mssize .gt. 0) then
              i_lsize(i_file)=i_mlsize
              i_ssize(i_file)=i_mssize
            end if
          end if

          if (a_rmgfile(i_file) .ne. ' ')  then
            call read_mhdr(a_rmgfile(i_file),i_mlsize,i_mssize,i_mbytes(i_file),i_moff(i_file))
            if (i_mssize .gt. 0) then
              a_magfile(i_file)=a_rmgfile(i_file)
              a_rmgfile(i_file)=' '
              i_lsize(i_file)=i_mlsize
              i_ssize(i_file)=i_mssize
            end if
          end if

          if (a_dtefile(i_file) .ne. ' ') then
            call read_dhdr(a_dtefile(i_file),i_dlsize,i_dssize,i_dbytes(i_file),i_doff(i_file),r_dmul,r_dadd,
     &             r_peg(1,i_file),r_str(1,i_file),r_spc(1,i_file))
            if (i_dssize .gt. 0) then
              i_lsize(i_file)=i_dlsize
              i_ssize(i_file)=i_dssize
              r_ddnc(1,i_file) = r_dmul
              r_ddnc(2,i_file) = r_dadd
            end if
          end if

          r_rad(i_file) = rdir(r_a,r_e2,r_peg(3,i_file),r_peg(1,i_file))

          if (a_type(i_file) .eq. ' ') then
            stop 'Error - No file type'
          else if (a_type(i_file).eq.'XYZ' .or. a_type(i_file).eq.'xyz') then
            i_type(i_file) = 1
            r_iat(1,1) = 1.
            r_iat(1,2) = 0.
            r_iat(1,3) = 0.
            r_iat(1,4) = 0.
            r_iat(2,1) = 0.
            r_iat(2,2) = 1.
            r_iat(2,3) = 0.
            r_iat(2,4) = 0.
            r_iat(3,1) = 0.
            r_iat(3,2) = 0.
            r_iat(3,3) = 1.
            r_iat(1,4) = 0.
            r_iat(2,4) = 0.
            r_iat(3,4) = 0.
          else if (a_type(i_file).eq.'TCN' .or. a_type(i_file).eq.'tcn') then
            i_type(i_file) = 1
            call tcnatm(r_a,r_e2,r_peg(1,i_file),r_iat)
          else if (a_type(i_file).eq.'SCH' .or. a_type(i_file).eq.'sch' .or.
     &             a_type(i_file).eq.'SLH' .or. a_type(i_file).eq.'slh') then
            i_type(i_file) = 2
            call tcnatm(r_a,r_e2,r_peg(1,i_file),r_iat)
          else if (a_type(i_file).eq.'SRH' .or. a_type(i_file).eq.'srh') then
            i_type(i_file) = -2
            call tcnatm(r_a,r_e2,r_peg(1,i_file),r_iat)
          else if (a_type(i_file).eq.'ENU' .or. a_type(i_file).eq.'enu' ) then
            i_type(i_file) = 3
            r_iat(1,1) = 1.
            r_iat(1,2) = 0.
            r_iat(1,3) = 0.
            r_iat(1,4) = 0.
            r_iat(2,1) = 0.
            r_iat(2,2) = 1.
            r_iat(2,3) = 0.
            r_iat(2,4) = 0.
            r_iat(3,1) = 0.
            r_iat(3,2) = 0.
            r_iat(3,3) = 1.
            r_iat(1,4) = 0.
            r_iat(2,4) = 0.
            r_iat(3,4) = 0.
            if (i_zone(i_file) .le. 0) then
              call enutoll(r_a,r_e2,i_zone(i_file),a_grid,v_oloc,r_peg(1,i_file),r_peg(2,i_file),2)
            end if
          else if (a_type(i_file).eq.'UTM' .or. a_type(i_file).eq.'utm' ) then
            i_type(i_file) = 4
            r_iat(1,1) = 1.
            r_iat(1,2) = 0.
            r_iat(1,3) = 0.
            r_iat(1,4) = 0.
            r_iat(2,1) = 0.
            r_iat(2,2) = 1.
            r_iat(2,3) = 0.
            r_iat(2,4) = 0.
            r_iat(3,1) = 0.
            r_iat(3,2) = 0.
            r_iat(3,3) = 1.
            r_iat(1,4) = 0.
            r_iat(2,4) = 0.
            r_iat(3,4) = 0.
            if (i_zone(i_file) .le. 0) then
              call utmtoll(r_a,r_e2,i_zone(i_file),a_grid,v_oloc,r_peg(1,i_file),r_peg(2,i_file),2)
            end if
          else if (a_type(i_file).eq.'EQA' .or. a_type(i_file).eq.'eqa' ) then
            i_type(i_file) = 5
            r_iat(1,1) = 1.
            r_iat(1,2) = 0.
            r_iat(1,3) = 0.
            r_iat(1,4) = 0.
            r_iat(2,1) = 0.
            r_iat(2,2) = 1.
            r_iat(2,3) = 0.
            r_iat(2,4) = 0.
            r_iat(3,1) = 0.
            r_iat(3,2) = 0.
            r_iat(3,3) = 1.
            r_iat(1,4) = 0.
            r_iat(2,4) = 0.
            r_iat(3,4) = 0.
          else
            type *,'File type = ',a_type(i_file)
            stop 'File type not recognized'
          end if

          if (i_type(i_file) .eq. -2) then
            r_spc(2,i_file) = -r_spc(2,i_file)
            r_str(2,i_file) = -r_str(2,i_file)
          end if
          if (i_type(i_file) .eq. 4 ) r_spc(1,i_file) = -r_spc(1,i_file)

c          type *,' '
c          type *,'r_iat='
c          type *,r_iat(1,1),r_iat(1,2),r_iat(1,3)
c          type *,r_iat(2,1),r_iat(2,2),r_iat(2,3)
c          type *,r_iat(3,1),r_iat(3,2),r_iat(3,3)
c          type *,r_iat(1,4),r_iat(2,4),r_iat(3,4)

          if (i_file .eq. 0) then   ! output file

            call invrstrn(r_iat,r_inv)

c            type *,' '
c            type *,'r_inv='
c            type *,r_inv(1,1),r_inv(1,2),r_inv(1,3)
c            type *,r_inv(2,1),r_inv(2,2),r_inv(2,3)
c            type *,r_inv(3,1),r_inv(3,2),r_inv(3,3)
c            type *,r_inv(1,4),r_inv(2,4),r_inv(3,4)
c            type *,' '

          else

            call multitrn(r_iat,r_aff,r_tmp)
            call multitrn(r_inv,r_tmp,r_atm(1,1,i_file))
            call invrstrn(r_atm(1,1,i_file),r_mta(1,1,i_file))

c            type *,' '
c            type *,'r_atm='
c            type *,r_atm(1,1,i_file),r_atm(1,2,i_file),r_atm(1,3,i_file)
c            type *,r_atm(2,1,i_file),r_atm(2,2,i_file),r_atm(2,3,i_file)
c            type *,r_atm(3,1,i_file),r_atm(3,2,i_file),r_atm(3,3,i_file)
c            type *,r_atm(1,4,i_file),r_atm(2,4,i_file),r_atm(3,4,i_file)
c            type *,'r_mta='
c            type *,r_mta(1,1,i_file),r_mta(1,2,i_file),r_mta(1,3,i_file)
c            type *,r_mta(2,1,i_file),r_mta(2,2,i_file),r_mta(2,3,i_file)
c            type *,r_mta(3,1,i_file),r_mta(3,2,i_file),r_mta(3,3,i_file)
c            type *,r_mta(1,4,i_file),r_mta(2,4,i_file),r_mta(3,4,i_file)

c            call multitrn(r_mta(1,1,i_file),r_atm(1,1,i_file),r_tmp)
c            type *,'r_tmp='
c            type *,r_tmp(1,1),r_tmp(1,2),r_tmp(1,3)
c            type *,r_tmp(2,1),r_tmp(2,2),r_tmp(2,3)
c            type *,r_tmp(3,1),r_tmp(3,2),r_tmp(3,3)
c            type *,r_tmp(1,4),r_tmp(2,4),r_tmp(3,4)

            if (i_ssize(i_file) .le.     0) then
              write(6,*) 'Error - Input file has 0 pixels per line'
              stop 'Execution Halted'
            end if
            if (i_ssize(i_file) .gt. I_MXS) then
              write(6,*) 'Error - Input file exceeds max pixels per line. ',i_ssize(i_file),I_MXS
              stop 'Execution Halted'
            end if

            do il = 0,1
              v_iloc1(1) = ((il*(i_lsize(i_file)-1))+1)*r_spc(1,i_file) + r_str(1,i_file)
              do is=0,1
                v_iloc1(2)=((is*(i_ssize(i_file)-1))+1)*r_spc(2,i_file) + r_str(2,i_file)
                do iz=0,1
                  v_iloc1(3)=iz*(R_MAX-R_MIN)+R_MIN

                  if (i_type(i_file) .eq. 1) then 
                    v_iloc2(1) = v_iloc1(1)
                    v_iloc2(2) = v_iloc1(2)
                    v_iloc2(3) = v_iloc1(3)
                  else if (i_type(i_file) .eq. 2 .or. i_type(i_file) .eq. -2) then        ! convert input from sch to xyz
                    r_lon=v_iloc1(1)/r_rad(i_file)
                    r_lat=v_iloc1(2)/r_rad(i_file)
                    r_hgt=v_iloc1(3)
                    call sch_to_tcn(r_rad(i_file),v_iloc2,r_lat,r_lon,r_hgt,1)
                  else if (i_type(i_file) .eq. 3) then        ! Convert input from enu to xyz
                    r_hgt=v_iloc1(3)
                    call enutoll(r_a,r_e2,i_zone(i_file),a_grid,v_iloc1,r_lat,r_lon,1)
                    call latlon(r_a,r_e2,v_iloc2,r_lat,r_lon,r_hgt,1)
                  else if (i_type(i_file) .eq. 4) then        ! Convert input from utm to xyz
                    r_hgt=v_iloc1(3)
                    call utmtoll(r_a,r_e2,i_zone(i_file),a_grid,v_iloc1,r_lat,r_lon,1)
                    call latlon(r_a,r_e2,v_iloc2,r_lat,r_lon,r_hgt,1)
                  else if (i_type(i_file) .eq. 5) then        ! Convert input from equal angle to xyz
                    r_lat=v_iloc1(1)/r_rtod
                    r_lon=v_iloc1(2)/r_rtod
                    r_hgt=v_iloc1(3)
                    call latlon(r_a,r_e2,v_iloc2,r_lat,r_lon,r_hgt,1)
                  end if

                  call vecmulti(r_atm(1,1,i_file),v_iloc2,v_iloc3) ! convert from input xyz to output xyz
                  call vecaddit(r_atm(1,4,i_file),v_iloc3,v_iloc3)

                  if (i_type(0) .eq. 1) then
                    v_oloc(1) = v_iloc3(1)
                    v_oloc(2) = v_iloc3(2)
                    v_oloc(3) = v_iloc3(3)
                  else if (i_type(0) .eq. 2 .or. i_type(0) .eq. -2) then                ! Convert output from xyz to sch
                    call sch_to_tcn(r_rad(0),v_iloc3,r_lat,r_lon,r_hgt,2)
                    v_oloc(1) = r_lon*r_rad(0)
                    v_oloc(2) = r_lat*r_rad(0)
                    v_oloc(3) = r_hgt
                  else if (i_type(0) .eq. 3) then                ! Convert output from xyz to enu
                    call latlon(r_a,r_e2,v_iloc3,r_lat,r_lon,r_hgt,2)
                    call enutoll(r_a,r_e2,i_zone(0),a_grid,v_oloc,r_lat,r_lon,2)
                    v_oloc(3) = r_hgt
                  else if (i_type(0) .eq. 4) then                ! Convert output from xyz to utm
                    call latlon(r_a,r_e2,v_iloc3,r_lat,r_lon,r_hgt,2)
                    call utmtoll(r_a,r_e2,i_zone(0),a_grid,v_oloc,r_lat,r_lon,2)
                    v_oloc(3) = r_hgt
                  else if (i_type(0) .eq. 5) then                ! Convert output from xyz to ll
                    call latlon(r_a,r_e2,v_iloc3,r_lat,r_lon,r_hgt,2)
                    v_oloc(1) = r_lat*r_rtod
                    v_oloc(2) = r_lon*r_rtod
                    v_oloc(3) = r_hgt
                  end if

                  v_oloc(1) = (v_oloc(1) - r_str(1,0))/r_spc(1,0)
                  v_oloc(2) = (v_oloc(2) - r_str(2,0))/r_spc(2,0)

                  i_fstln=min(i_fstln,nint(v_oloc(1)))
                  i_lstln=max(i_lstln,nint(v_oloc(1)))
                  i_fstsp=min(i_fstsp,nint(v_oloc(2)))
                  i_lstsp=max(i_lstsp,nint(v_oloc(2)))

                end do
              end do
            end do

          end if

          if (a_rmgfile(i_file) .ne. ' ') then
            write(6,'(x,a,a,2i6)') 'Opening rmg file: ',a_rmgfile(i_file)(1:40),i_ssize(i_file),i_lsize(i_file)
            open(unit=40+i_file,file=a_rmgfile(i_file),form='unformatted',
     &         status='old',access='direct',recl=8*i_ssize(i_file),readonly)
          end if
          if (a_magfile(i_file) .ne. ' ') then
            write(6,'(x,a,a,2i6)') 'Opening amp file: ',a_magfile(i_file)(1:40),i_ssize(i_file),i_lsize(i_file)
            open(unit= 60+i_file,file=a_magfile(i_file),form='unformatted',
     &       status='old',access='direct',recl=i_mbytes(i_file)*i_ssize(i_file),readonly)
          end if
          if (a_dtefile(i_file) .ne. ' ') then
            write(6,'(x,a,a,2i6)') 'Opening hgt file: ',a_dtefile(i_file)(1:40),i_ssize(i_file),i_lsize(i_file)
            open(unit= 80+i_file,file=a_dtefile(i_file),form='unformatted',
     &       status='old',access='direct',recl=i_dbytes(i_file)*i_ssize(i_file),readonly)
          end if


        end do

        if (i_ssize(0) .le.     0) stop 'Error - Reference file has 0 pixels per line'
        if (i_ssize(0) .gt. I_MXS) then
          type *, 'Error - Reference file exceeds max pixels per line', i_ssize(0),I_MXS
          stop 'Execution Halted'
        end if


        i_osoff = i_fstsp-1+i_mdx
        i_osnum = i_lstsp-i_fstsp+1
        i_oloff = i_fstln-1+i_mdy
        i_olnum = i_lstln-i_fstln+1

        if (i_sty .eq. 0) i_sty = max(min(i_oloff-i_dly,i_lsize(0)),1)
        if (i_stx .eq. 0) i_stx = max(min(i_osoff-i_dlx,i_ssize(0)),1)

        if (i_spy .eq. 0) i_spy = max(min(i_oloff+i_olnum+i_dly,i_lsize(0)),1)
        if (i_spx .eq. 0) i_spx = max(min(i_osoff+i_osnum+i_dlx,i_ssize(0)),1)

        write(6,*) ' '
        write(6,*) 'Reference file offsets = ',r_str(1,0),r_str(2,0)
        if (i_zone(0) .ne. 0) write(6,*) 'Reference file zone    = ',i_zone(0)
        write(6,*) ' '
        call write_hdr(15,'REFF ',a_type(0),i_lsize(0),i_ssize(0),r_peg(1,0),
     &              r_str(1,0),r_spc(1,0),i_zone(0),i_err)
        call write_hdr(15,'SRCH ',a_type(1),i_lsize(1),i_ssize(1),r_peg(1,1),
     &              r_str(1,1),r_spc(1,1),i_zone(1),i_err)

                i_py = 2*i_sdy+i_rws
                i_px = 2*i_sdx+i_rws

                I_CX=2*I_SDX+1
                I_CY=2*I_SDY+1


c
c  Initialize graphics
c

                IF (L_IDISP .OR. L_MDISP) THEN
                    i_gxi = 10

                    i_typ( 1) = 1
                    i_typ( 2) = 1
                    i_typ( 3) = 1
                    i_typ( 4) = 1
                    i_typ( 5) = 1
                    i_typ( 6) = 1
                    i_typ( 7) = 1
                    i_typ( 8) = 1
                    i_typ( 9) = 1
                    i_typ(10) = 1

                    a_lbl( 0) = 'Multimatch Control'
                    a_lbl( 1) = 'Grey'
                    a_lbl( 2) = 'Inv'
                    a_lbl( 3) = ' '
                    a_lbl( 4) = 'Text Off'
                    a_lbl( 5) = 'Text On'
                    a_lbl( 6) = 'Pause'
                    a_lbl( 7) = 'Step'
                    a_lbl( 8) = 'Run'
                    a_lbl( 9) = ' '
                    a_lbl(10) = 'Quit'

                    i_wxs( 0) = 320
                    i_wys( 0) = 100

                    i_frx( 0) = 5
                    i_frx( 1) = 1
                    i_frx( 2) = 1
                    i_frx( 3) = 1
                    i_frx( 4) = 1
                    i_frx( 5) = 1
                    i_frx( 6) = 1
                    i_frx( 7) = 1
                    i_frx( 8) = 1
                    i_frx( 9) = 1
                    i_frx(10) = 1

                    i_fry( 0) = 0
                    i_fry( 1) = -40
                    i_fry( 2) = -40
                    i_fry( 3) = -40
                    i_fry( 4) = -40
                    i_fry( 5) = -40
                    i_fry( 6) = -40
                    i_fry( 7) = -40
                    i_fry( 8) = -40
                    i_fry( 9) = -40
                    i_fry(10) = -40

                    call init_gx(i_gxi,i_typ,a_lbl,i_wxs,i_wys,i_frx,i_fry,i_clrs)
                    i_gx = i_gxi

                    call setcolor(1)
                    call getevent(1,i_event)


                END IF
                IF (L_IDISP) THEN

                    i_gxi = 6

                    i_typ(1) = 4
                    i_typ(2) = 4
                    i_typ(3) = 4
                    i_typ(4) = 4
                    i_typ(5) = 1
                    i_typ(6) = 1

                    a_lbl(0) = 'Multimatch'
                    a_lbl(1) = 'Image I Window'
                    a_lbl(2) = 'Image J Window'
                    a_lbl(3) = 'Correlation'
                    a_lbl(4) = 'Eigen Vectors'
                    a_lbl(5) = 'Disable'
                    a_lbl(6) = 'Enable'

                    i_wxs(0) = 320
                    i_wys(0) = 380
                    i_wxs(1) = 128
                    i_wys(1) = 128
                    i_wxs(2) = 128
                    i_wys(2) = 128
                    i_wxs(3) = 128
                    i_wys(3) = 128
                    i_wxs(4) = 128
                    i_wys(4) = 128
                    i_wxs(5) = 128
                    i_wys(5) = 128
                    i_wxs(6) = 128
                    i_wys(6) = 128

                    i_frx(0) = 4
                    i_frx(1) = 2
                    i_frx(2) = 2
                    i_frx(3) = 2
                    i_frx(4) = 2
                    i_frx(5) = 1
                    i_frx(6) = 1

                    i_fry(0) = 0
                    i_fry(1) = 160
                    i_fry(2) = 160
                    i_fry(3) = 160
                    i_fry(4) = 160
                    i_fry(5) = -40
                    i_fry(6) = -40

                    call init_gx(i_gxi,i_typ,a_lbl,i_wxs,i_wys,i_frx,i_fry,i_clrs)
                    i_gx = i_gxi

                    type *,'i_gx=',i_gx
                    CALL DISPLAY_LABEL(11,a_lbl(1))
                    CALL DISPLAY_LABEL(12,a_lbl(2))
                    CALL DISPLAY_LABEL(13,a_lbl(3))
                    CALL DISPLAY_LABEL(14,a_lbl(4))

                    call getevent(1,i_event)

                    call clear_win(11)
                    call clear_win(12)
                    call clear_win(13)
                    call clear_win(14)
        
                    I_NWMAX=MAX(I_PX,I_PY)
                    type *,'i_nwmax=',i_nwmax
                    I_ZOOM=MAX(MIN(128/I_NWMAX,8),1)
                    type *,'i_zoom=',i_zoom
                    I_PZX=I_PX*I_ZOOM
                    I_PZY=I_PY*I_ZOOM

                    I_NWMAX=MAX(I_CX,I_CY)
                    I_ZOOMC=MAX(MIN(128/I_NWMAX,8),1)
                    I_CZX=I_CX*I_ZOOMC
                    I_CZY=I_CY*I_ZOOMC
                    TYPE *,'I_CY =',I_CY
                    TYPE *,'I_CZY=',I_CZY
                    TYPE *,'I_ZMC=',I_ZOOMC
                END IF

                   
                IF (L_MDISP) THEN  
                    I_NP=8
                    I_XDISP=((I_SPX-I_STX+I_DLX)/I_DLX)*I_NP
                    I_YDISP=((I_SPY-I_STY+I_DLY)/I_DLY)*I_NP
                    TYPE *,'I_XDISP,I_YDISP=',I_XDISP,I_YDISP
                    A_TEMP='DISPARITY'

                    i_gxi = 3

                    i_typ(1) = -4
                    i_typ(2) = 1
                    i_typ(3) = 1

                    a_lbl(0) = 'Multimatch'
                    a_lbl(1) = 'Displarity Map'
                    a_lbl(2) = 'Disable'
                    a_lbl(3) = 'Enable'

                    i_wxs(0) = I_XDISP+35
                    i_wys(0) = I_YDISP+35+50
                    i_wxs(1) = I_XDISP
                    i_wys(1) = I_YDISP

                    i_frx(0) = 4
                    i_frx(1) = 4
                    i_frx(2) = 1
                    i_frx(3) = 1

                    i_fry(0) = 0
                    i_fry(1) = I_YDISP
                    i_fry(2) = -40
                    i_fry(3) = -40

                    call init_gx(i_gxi,i_typ,a_lbl,i_wxs,i_wys,i_frx,i_fry,i_clrs)
                    i_gx = i_gxi

                    CALL DISPLAY_LABEL(I_GX-2,A_TEMP)

                    i_scale = 2*i_sdx
                END IF

        I_NY=-1
        do i_oloff=i_sty,i_spy,i_dly
          i_olnum=min(min(I_BSL,i_rws+2*i_sdy),i_lsize(0)-i_oloff+1)
          I_NY = I_NY + 1
          I_NX = -1
          do i_osoff=i_stx,i_spx,i_dlx
            i_osnum=min(min(I_BSS,i_rws+2*i_sdx),i_ssize(0)-i_osoff+1)
            I_NX = I_NX + 1
c            type *,'Clearing output arrays'
            do il=1,i_olnum
              do is=1,i_osnum
                r_obufa(is,il)=0.
                r_obufb(is,il)=0.
                r_sbuff(is,il)=0.
              end do
            end do

            do i_file=1,i_nof          ! Loop over input files

              do is = 1, i_osnum       ! Initialize scale array
                do il = 1, i_olnum
                  r_scale(is,il)=1.
                end do
              end do

              i_fstsp= 999999999
              i_lstsp=-999999999
              i_fstln= 999999999
              i_lstln=-999999999
              
              do il = 0,1
                v_oloc1(1) = (il*(i_olnum-1)+(i_oloff+i_mdy)+1)*r_spc(1,0) + r_str(1,0)
                do is=0,1
                  v_oloc1(2) = (is*(i_osnum-1)+(i_osoff+i_mdx)+1)*r_spc(2,0) + r_str(2,0)
                  do iz=0,1
                    v_oloc1(3)=iz*(R_MAX-R_MIN)+R_MIN

                    if (i_type(0) .eq. 1) then
                      v_oloc2(1) = v_oloc1(1)
                      v_oloc2(2) = v_oloc1(2)
                      v_oloc2(3) = v_oloc1(3)
                    else if (i_type(0) .eq. 2 .or. i_type(0) .eq. -2) then                ! Convert output from xyz to sch
                      r_lon=v_oloc1(1)/r_rad(0)
                      r_lat=v_oloc1(2)/r_rad(0)
                      r_hgt=v_oloc1(3)
                      call sch_to_tcn(r_rad(0),v_oloc2,r_lat,r_lon,r_hgt,1)
                    else if (i_type(0) .eq. 3) then                ! Convert output from xyz to utm
                      r_hgt=v_oloc1(3)
                      call enutoll(r_a,r_e2,i_zone(0),a_grid,v_oloc1,r_lat,r_lon,1)
                      call latlon(r_a,r_e2,v_oloc2,r_lat,r_lon,r_hgt,1)
                    else if (i_type(0) .eq. 4) then                ! Convert output from xyz to utm
                      r_hgt=v_oloc1(3)
                      call utmtoll(r_a,r_e2,i_zone(0),a_grid,v_oloc1,r_lat,r_lon,1)
                      call latlon(r_a,r_e2,v_oloc2,r_lat,r_lon,r_hgt,1)
                    else if (i_type(0) .eq. 5) then                ! Convert output from xyz to equal angle
                      r_lat=v_oloc1(1)/r_rtod
                      r_lon=v_oloc1(2)/r_rtod
                      r_hgt=v_oloc1(3)
                      call latlon(r_a,r_e2,v_oloc2,r_lat,r_lon,r_hgt,1)
                    end if

                    call vecmulti(r_mta(1,1,i_file),v_oloc2,v_tmp)
                    call vecaddit(v_tmp,r_mta(1,4,i_file),v_oloc3)

                    if (i_type(i_file) .eq. 1) then 
                      v_iloc(1) = v_oloc3(1)
                      v_iloc(2) = v_oloc3(2)
                      v_iloc(3) = v_oloc3(3)
                    else if (i_type(i_file) .eq. 2 .or. i_type(i_file) .eq. -2) then        ! convert input from sch to xyz
                      call sch_to_tcn(r_rad(i_file),v_oloc3,r_lat,r_lon,r_hgt,2)
                      v_iloc(1) = r_lon*r_rad(i_file)
                      v_iloc(2) = r_lat*r_rad(i_file)
                      v_iloc(3) = r_hgt
                    else if (i_type(i_file) .eq. 3) then        ! Convert input from utm to xyz
                      call latlon(r_a,r_e2,v_oloc3,r_lat,r_lon,r_hgt,2)
                      call enutoll(r_a,r_e2,i_zone(i_file),a_grid,v_iloc,r_lat,r_lon,2)
                      v_iloc(3) = r_hgt
                    else if (i_type(i_file) .eq. 4) then        ! Convert input from utm to xyz
                      call latlon(r_a,r_e2,v_oloc3,r_lat,r_lon,r_hgt,2)
                      call utmtoll(r_a,r_e2,i_zone(i_file),a_grid,v_iloc,r_lat,r_lon,2)
                      v_iloc(3) = r_hgt
                    else if (i_type(i_file) .eq. 5) then                ! Convert output from xyz to ll
                      call latlon(r_a,r_e2,v_oloc3,r_lat,r_lon,r_hgt,2)
                      v_iloc(1) = r_lat*r_rtod
                      v_iloc(2) = r_lon*r_rtod
                      v_iloc(3) = r_hgt
                    end if

                    v_iloc(1) = (v_iloc(1) - r_str(1,i_file))/r_spc(1,i_file)
                    v_iloc(2) = (v_iloc(2) - r_str(2,i_file))/r_spc(2,i_file)

                    i_fstln=min(i_fstln,nint(v_iloc(1)))
                    i_lstln=max(i_lstln,nint(v_iloc(1)))
                    i_fstsp=min(i_fstsp,nint(v_iloc(2)))
                    i_lstsp=max(i_lstsp,nint(v_iloc(2)))
                  end do
                end do
              end do

              i_fstln=max(i_fstln-1,1)
              i_lstln=min(i_lstln+1,i_lsize(i_file))
              i_fstsp=max(i_fstsp-1,1)
              i_lstsp=min(i_lstsp+1,i_ssize(i_file))

              i_isoff = i_fstsp-1
              i_isnum = i_lstsp-i_fstsp+1
              i_iloff = i_fstln-1
              i_ilnum = i_lstln-i_fstln+1

              i_isfac = max((i_isnum+2*I_BSS-1)/(2*I_BSS),i_ifacmin)
              i_ilfac = max((i_ilnum+2*I_BSL-1)/(2*I_BSL),i_ifacmin)

              if (i_isnum .gt. 0 .and. i_ilnum .gt. 0) then

                if (a_rmgfile(i_file) .ne. ' ') then
                  do il=1,i_ilnum/i_ilfac
                    read(40+i_file,rec=il+i_iloff) (r_data(is),is=1,i_ssize(i_file)),(r_datb(iss),iss=1,i_ssize(i_file))
                    do is=1,i_isnum/i_isfac
                      r_ibufa(is,il)=r_data((is*i_isfac)+i_isoff)             
                      r_ibufb(is,il)=r_datb((is*i_isfac)+i_isoff)             
                    end do
                  end do
                else
                  if (a_magfile(i_file) .ne. ' ') then
                    if (i_mbytes(i_file) .eq. 1) then
                      do il=1,i_ilnum/i_ilfac
                        read(60+i_file,rec=il+i_iloff+i_moff(i_file)) (b_data1(is),is=1,i_ssize(i_file))
                        do is=1,i_isnum/i_isfac
                          i_data2(is+i_isoff) = b_data1(is+i_isoff)
                          if (i_data2(is+i_isoff) .lt. 0) then
                            i_data2(is+i_isoff) = 256 + i_data2(is+i_isoff)
                          end if  
                          r_ibufa(is,il)=r_mdnc(1,i_file)*i_data2(is+i_isoff)  +  r_mdnc(2,i_file)           
                        end do
                      end do
                    else if (i_mbytes(i_file) .eq. 2) then
                      do il=1,i_ilnum/i_ilfac
                        read(60+i_file,rec=il+i_iloff+i_moff(i_file)) (i_data2(is),is=1,i_ssize(i_file))
                        do is=1,i_isnum/i_isfac
                          r_ibufa(is,il)=r_mdnc(1,i_file)*i_data2(is+i_isoff)  +  r_mdnc(2,i_file)           
                        end do
                      end do
                    else
                      do il=1,i_ilnum/i_ilfac
                        read(60+i_file,rec=il+i_iloff+i_moff(i_file)) (r_data(is),is=1,i_ssize(i_file))
                        do is=1,i_isnum/i_isfac
                          r_ibufa(is,il)=r_data(is+i_isoff)             
                        end do
                      end do
                    end if
                  else
                    do il=1,i_ilnum/i_ilfac
                      do is=1,i_isnum/i_isfac
                        r_ibufa(is,il)=1.            
                      end do
                    end do
                  end if

                  if (a_dtefile(i_file) .ne. ' ') then
                    if (i_dbytes(i_file) .eq. 1) then
                      do il=1,i_ilnum/i_ilfac
                        read(80+i_file,rec=il+i_iloff+i_doff(i_file)) (b_datb1(is),is=1,i_ssize(i_file))
                        do is=1,i_isnum/i_isfac
                          i_datb2(is+i_isoff) = b_datb1(is+i_isoff)
                          if (i_datb2(is+i_isoff) .lt. 0) then
                            i_datb2(is+i_isoff) = 256 + i_datb2(is+i_isoff)
                          end if
                          r_ibufb(is,il)=r_ddnc(1,i_file)*i_datb2(is+i_isoff) + r_ddnc(2,i_file)
                          if (r_ibufb(is,il).lt.R_MIN .or. r_ibufb(is,il).gt.R_MAX) r_ibufa(is,il)=0.        
                          if (r_ibufb(is,il).lt.R_MIN .or. r_ibufb(is,il).gt.R_MAX) r_ibufb(is,il)=0.        
                        end do
                      end do
                    else if (i_dbytes(i_file) .eq. 2) then
                      do il=1,i_ilnum/i_ilfac
                        read(80+i_file,rec=il+i_iloff+i_doff(i_file)) (i_datb2(is),is=1,i_ssize(i_file))
                        do is=1,i_isnum/i_isfac
                          r_ibufb(is,il)=r_ddnc(1,i_file)*i_datb2(is+i_isoff) + r_ddnc(2,i_file)
                          if (r_ibufb(is,il).lt.R_MIN .or. r_ibufb(is,il).gt.R_MAX) r_ibufa(is,il)=0.        
                          if (r_ibufb(is,il).lt.R_MIN .or. r_ibufb(is,il).gt.R_MAX) r_ibufb(is,il)=0.        
                        end do
                      end do
                    else
                      do il=1,i_ilnum/i_ilfac
                        read(80+i_file,rec=il+i_iloff+i_doff(i_file)) (r_datb(is),is=1,i_ssize(i_file))
                        do is=1,i_isnum/i_isfac
                          r_ibufb(is,il)=r_datb(is+i_isoff) 
                          if (r_ibufb(is,il).lt.R_MIN .or. r_ibufb(is,il).gt.R_MAX) r_ibufa(is,il)=0.        
                          if (r_ibufb(is,il).lt.R_MIN .or. r_ibufb(is,il).gt.R_MAX) r_ibufb(is,il)=0.        
                        end do
                      end do
                    end if
                  else
                    do il=1,i_ilnum/i_ilfac
                      do is=1,i_isnum/i_isfac
                        r_ibufb(is,il)=0            
                      end do
                    end do
                  end if
                end if

c
c  Mosaic Image block
c
                do is=1,i_isnum/i_isfac
                  if (r_ibufa(is,1) .ne. 0.) then
                    v_iloc(1) = ( 1*i_ilfac+i_iloff)*r_spc(1,i_file)+r_str(1,i_file)
                    v_iloc(2) = (is*i_isfac+i_isoff)*r_spc(2,i_file)+r_str(2,i_file)
                    v_iloc(3) = r_ibufb(is,1)

                          if (i_type(i_file) .eq. 1) then 
                            v_iloc2(1) = v_iloc(1)
                            v_iloc2(2) = v_iloc(2)
                            v_iloc2(3) = v_iloc(3)
                          else if (i_type(i_file) .eq. 2 .or. i_type(i_file) .eq. -2) then        ! convert input from sch to xyz
                            r_lon=v_iloc(1)/r_rad(i_file)
                            r_lat=v_iloc(2)/r_rad(i_file)
                            r_hgt=v_iloc(3)
                            call sch_to_tcn(r_rad(i_file),v_iloc2,r_lat,r_lon,r_hgt,1)
                          else if (i_type(i_file) .eq. 3) then        ! Convert input from utm to xyz
                            r_hgt=v_iloc(3)
                            call enutoll(r_a,r_e2,i_zone(i_file),a_grid,v_iloc,r_lat,r_lon,1)
                            call latlon(r_a,r_e2,v_iloc2,r_lat,r_lon,r_hgt,1)
                          else if (i_type(i_file) .eq. 4) then        ! Convert input from utm to xyz
                            r_hgt=v_iloc(3)
                            call utmtoll(r_a,r_e2,i_zone(i_file),a_grid,v_iloc,r_lat,r_lon,1)
                            call latlon(r_a,r_e2,v_iloc2,r_lat,r_lon,r_hgt,1)
                          else if (i_type(i_file) .eq. 5) then        ! Convert input from equal angle to xyz
                            r_lat=v_iloc(1)/r_rtod
                            r_lon=v_iloc(2)/r_rtod
                            r_hgt=v_iloc(3)
                            call latlon(r_a,r_e2,v_iloc2,r_lat,r_lon,r_hgt,1)
                          end if

                          call vecmulti(r_atm(1,1,i_file),v_iloc2,v_iloc3) ! convert from input xyz to output xyz
                          call vecaddit(r_atm(1,4,i_file),v_iloc3,v_iloc3)

                          if (i_type(0) .eq. 1) then
                            v_olocc(1,2,is) = v_iloc3(1)
                            v_olocc(2,2,is) = v_iloc3(2)
                            v_olocc(3,2,is) = v_iloc3(3)
                          else if (i_type(0) .eq. 2 .or. i_type(0) .eq. -2) then                ! Convert output from xyz to sch
                            call sch_to_tcn(r_rad(0),v_iloc3,r_lat,r_lon,r_hgt,2)
                            v_olocc(1,2,is) = r_lon*r_rad(0)
                            v_olocc(2,2,is) = r_lat*r_rad(0)
                            v_olocc(3,2,is) = r_hgt
                          else if (i_type(0) .eq. 3) then                ! Convert output from xyz to utm
                            call latlon(r_a,r_e2,v_iloc3,r_lat,r_lon,r_hgt,2)
                            call enutoll(r_a,r_e2,i_zone(0),a_grid,v_olocc(1,2,is),r_lat,r_lon,2)
                            v_olocc(3,2,is) = r_hgt
                          else if (i_type(0) .eq. 4) then                ! Convert output from xyz to utm
                            call latlon(r_a,r_e2,v_iloc3,r_lat,r_lon,r_hgt,2)
                            call utmtoll(r_a,r_e2,i_zone(0),a_grid,v_olocc(1,2,is),r_lat,r_lon,2)
                            v_olocc(3,2,is) = r_hgt
                          else if (i_type(0) .eq. 5) then                ! Convert output from xyz to ll
                            call latlon(r_a,r_e2,v_iloc3,r_lat,r_lon,r_hgt,2)
                            v_olocc(1,2,is) = r_lat*r_rtod
                            v_olocc(2,2,is) = r_lon*r_rtod
                            v_olocc(3,2,is) = r_hgt
                          end if

                    v_olocc(1,2,is)=nint((v_olocc(1,2,is)-r_str(1,0))/r_spc(1,0))-(i_oloff+i_mdy)
                    v_olocc(2,2,is)=nint((v_olocc(2,2,is)-r_str(2,0))/r_spc(2,0))-(i_osoff+i_mdx)
                    v_olocc(4,2,is)=r_ibufa(is,1)
                    i_oloc1(2,is)=1.
                  else
                    i_oloc1(2,is)=0.
                  end if
                end do
                do il=1,i_ilnum/i_ilfac-1
c                  type *,'il=',il !@#$%
                  do is=1,i_isnum/i_isfac
 
                    v_olocc(1,1,is) = v_olocc(1,2,is)
                    v_olocc(2,1,is) = v_olocc(2,2,is)
                    v_olocc(3,1,is) = v_olocc(3,2,is)
                    v_olocc(4,1,is) = v_olocc(4,2,is)
                    i_oloc1(1,is) = i_oloc1(2,is)

                    if (r_ibufa(is,il+1) .ne. 0.) then
                      v_iloc(1) = (il*i_ilfac+i_iloff+1)*r_spc(1,i_file)+r_str(1,i_file)
                      v_iloc(2) = (is*i_isfac+i_isoff  )*r_spc(2,i_file)+r_str(2,i_file)
                      v_iloc(3) = r_ibufb(is,il+1)

                          if (i_type(i_file) .eq. 1) then 
                            v_iloc2(1) = v_iloc(1)
                            v_iloc2(2) = v_iloc(2)
                            v_iloc2(3) = v_iloc(3)
                          else if (i_type(i_file) .eq. 2 .or. i_type(i_file) .eq. -2) then        ! convert input from sch to xyz
                            r_lon=v_iloc(1)/r_rad(i_file)
                            r_lat=v_iloc(2)/r_rad(i_file)
                            r_hgt=v_iloc(3)
                            call sch_to_tcn(r_rad(i_file),v_iloc2,r_lat,r_lon,r_hgt,1)
                          else if (i_type(i_file) .eq. 3) then        ! Convert input from utm to xyz
                            r_hgt=v_iloc(3)
                            call enutoll(r_a,r_e2,i_zone(i_file),a_grid,v_iloc,r_lat,r_lon,1)
                            call latlon(r_a,r_e2,v_iloc2,r_lat,r_lon,r_hgt,1)
                          else if (i_type(i_file) .eq. 4) then        ! Convert input from utm to xyz
                            r_hgt=v_iloc(3)
                            call utmtoll(r_a,r_e2,i_zone(i_file),a_grid,v_iloc,r_lat,r_lon,1)
                            call latlon(r_a,r_e2,v_iloc2,r_lat,r_lon,r_hgt,1)
                          else if (i_type(i_file) .eq. 5) then        ! Convert input from equal angle to xyz
                            r_lat=v_iloc(1)/r_rtod
                            r_lon=v_iloc(2)/r_rtod
                            r_hgt=v_iloc(3)
                            call latlon(r_a,r_e2,v_iloc2,r_lat,r_lon,r_hgt,1)
                          end if

                          call vecmulti(r_atm(1,1,i_file),v_iloc2,v_iloc3) ! convert from input xyz to output xyz
                          call vecaddit(r_atm(1,4,i_file),v_iloc3,v_iloc3)

                          if (i_type(0) .eq. 1) then
                            v_olocc(1,2,is) = v_iloc3(1)
                            v_olocc(2,2,is) = v_iloc3(2)
                            v_olocc(3,2,is) = v_iloc3(3)
                          else if (i_type(0) .eq. 2 .or. i_type(0) .eq. -2) then                ! Convert output from xyz to sch
                            call sch_to_tcn(r_rad(0),v_iloc3,r_lat,r_lon,r_hgt,2)
                            v_olocc(1,2,is) = r_lon*r_rad(0)
                            v_olocc(2,2,is) = r_lat*r_rad(0)
                            v_olocc(3,2,is) = r_hgt
                          else if (i_type(0) .eq. 3) then                ! Convert output from xyz to utm
                            call latlon(r_a,r_e2,v_iloc3,r_lat,r_lon,r_hgt,2)
                            call enutoll(r_a,r_e2,i_zone(0),a_grid,v_olocc(1,2,is),r_lat,r_lon,2)
                            v_olocc(3,2,is) = r_hgt
                          else if (i_type(0) .eq. 4) then                ! Convert output from xyz to utm
                            call latlon(r_a,r_e2,v_iloc3,r_lat,r_lon,r_hgt,2)
                            call utmtoll(r_a,r_e2,i_zone(0),a_grid,v_olocc(1,2,is),r_lat,r_lon,2)
                            v_olocc(3,2,is) = r_hgt
                          else if (i_type(0) .eq. 5) then                ! Convert output from xyz to ll
                            call latlon(r_a,r_e2,v_iloc3,r_lat,r_lon,r_hgt,2)
                            v_olocc(1,2,is) = r_lat*r_rtod
                            v_olocc(2,2,is) = r_lon*r_rtod
                            v_olocc(3,2,is) = r_hgt
                          end if

                      v_olocc(1,2,is)=((v_olocc(1,2,is)-r_str(1,0))/r_spc(1,0))-(i_oloff+i_mdy)
                      v_olocc(2,2,is)=((v_olocc(2,2,is)-r_str(2,0))/r_spc(2,0))-(i_osoff+i_mdx)
                      v_olocc(4,2,is)=r_ibufa(is,il+1)
                      i_oloc1(2,is)=1.
                    else
                      i_oloc1(2,is)=0.
                    end if

                  end do
                  do is=1,i_isnum/i_isfac-1
                    i_smin=+100000000
                    i_smax=-100000000
                    i_lmin=+100000000
                    i_lmax=-100000000
                    do i=0,1
                      do j=1,2
                        if (i_oloc1(j,is+i) .gt. 0) then
                          i_lmin=min(int(v_olocc(1,j,is+i)),i_lmin)
                          i_lmax=max(int(v_olocc(1,j,is+i)),i_lmax)
                          i_smin=min(int(v_olocc(2,j,is+i)),i_smin)
                          i_smax=max(int(v_olocc(2,j,is+i)),i_smax)
                        end if
                      end do
                    end do
c                    type *,'is=',is,max(i_smin+1,1),min(i_smax,i_osnum) !@#$%
c                    type *,'   ',il,max(i_lmin+1,1),min(i_lmax,i_olnum) !@#$%

                    do iss = max(i_smin,1),min(i_smax+1,i_osnum)
                      do ill = max(i_lmin,1),min(i_lmax+1,i_olnum)

                        if ( r_scale(iss,ill) .gt. 0.) then
                          v_pnt(1) = ill
                          v_pnt(2) = iss
                          v_pnt(3) = 0.
                          v_pnt(4) = 0.

                          call inplane2d(v_olocc(1,1,is),i_oloc1(1,is),v_pnt,i_flg)
                          if (i_flg .ne. 0) then
                            r_aa = v_pnt(4)
                            r_bb = v_pnt(3)
                            r_obufa(iss,ill) = r_obufa(iss,ill)+r_scale(iss,ill)*r_aa
                            r_obufb(iss,ill) = r_obufb(iss,ill)+r_scale(iss,ill)*r_bb
                            r_sbuff(iss,ill) = r_sbuff(iss,ill)+r_scale(iss,ill)
                            r_scale(iss,ill) = 0.
                          end if
                        end if
                      end do
                    end do


                  end do
                end do
              end if
            end do


c !@#$%^ put correlation code here

                do i_y = 1,i_py
                  do i_x = 1,i_px
                    if (i_set .eq. 1) then
                      r_imgj(i_x,i_y) = r_obufa(i_x,i_y)
                    else
                      r_imgj(i_x,i_y) = r_obufb(i_x,i_y)
                    end if
                  end do
                end do


                if (a_rmgfile(0) .ne. ' ') then
                  do il=1,i_olnum
                    read(40+0,rec=il+i_oloff) (r_data(is),is=1,i_ssize(0)),(r_datb(iss),iss=1,i_ssize(0))
                    do is=1,i_osnum
                      if ((is + i_osoff) .le. i_ssize(0)) then
                        r_ibufa(is,il)=r_data(is+i_osoff)             
                        r_ibufb(is,il)=r_datb(is+i_osoff) 
                      else
                        r_ibufa(is,il) = 0.
                        r_ibufb(is,il) = -10000.
                      end if            
                    end do
                  end do
                else
                  if (a_magfile(0) .ne. ' ') then
                    if (i_mbytes(0) .eq. 1) then
                      do il=1,i_olnum
                        read(60+0,rec=il+i_oloff+i_moff(0)) (b_data1(is),is=1,i_ssize(0))
                        do is=1,i_osnum
                          i_data2(is+i_osoff) = b_data1(is+i_osoff)
                          if (i_data2(is+i_osoff) .lt. 0) then
                            i_data2(is+i_osoff) = 256 + i_data2(is+i_osoff)
                          end if 
                          r_ibufa(is,il)=r_mdnc(1,0)*i_data2(is+i_osoff)  +  r_mdnc(2,0)           
                        end do
                      end do
                    else if (i_mbytes(0) .eq. 2) then
                      do il=1,i_olnum
                        read(60+0,rec=il+i_oloff+i_moff(0)) (i_data2(is),is=1,i_ssize(0))
                        do is=1,i_osnum
                          r_ibufa(is,il)=r_mdnc(1,0)*i_data2(is+i_osoff)  +  r_mdnc(2,0)           
                        end do
                      end do
                    else
                      do il=1,i_olnum
                        read(60+0,rec=il+i_oloff+i_moff(0)) (r_data(is),is=1,i_ssize(0))
                        do is=1,i_osnum
                          r_ibufa(is,il)=r_data(is+i_osoff)             
                        end do
                      end do
                    end if
                  else
                    do il=1,i_olnum
                      do is=1,i_osnum
                        r_ibufa(is,il)=1.            
                      end do
                    end do
                  end if
                  if (a_dtefile(0) .ne. ' ') then
                    if (i_dbytes(0) .eq. 1) then
                      do il=1,i_olnum
                        read(80+0,rec=il+i_oloff+i_doff(0)) (b_datb1(is),is=1,i_ssize(0))
                        do is=1,i_osnum
                          i_datb2(is+i_osoff) = b_datb1(is+i_osoff)
                          if (i_datb2(is+i_osoff) .lt. 0) then
                            i_datb2(is+i_osoff) = 256 + i_datb2(is+i_osoff)
                          end if
                          r_ibufb(is,il)=r_ddnc(1,0)*i_datb2(is+i_osoff) + r_ddnc(2,0)
                          if (r_ibufb(is,il).lt.R_MIN .or. r_ibufb(is,il).gt.R_MAX) r_ibufa(is,il)=0.        
                          if (r_ibufb(is,il).lt.R_MIN .or. r_ibufb(is,il).gt.R_MAX) r_ibufb(is,il)=0.        
                        end do
                      end do
                    else if (i_dbytes(0) .eq. 2) then
                      do il=1,i_olnum
                        read(80+0,rec=il+i_oloff+i_doff(0)) (i_datb2(is),is=1,i_ssize(0))
                        do is=1,i_osnum
                          r_ibufb(is,il)=r_ddnc(1,0)*i_datb2(is+i_osoff) + r_ddnc(2,0)
                          if (r_ibufb(is,il).lt.R_MIN .or. r_ibufb(is,il).gt.R_MAX) r_ibufa(is,il)=0.        
                          if (r_ibufb(is,il).lt.R_MIN .or. r_ibufb(is,il).gt.R_MAX) r_ibufb(is,il)=0.        
                        end do
                      end do
                    else
                      do il=1,i_olnum
                        read(80+0,rec=il+i_oloff+i_doff(0)) (r_datb(is),is=1,i_ssize(0))
                        do is=1,i_osnum
                          r_ibufb(is,il)=r_datb(is+i_osoff) 
                          if (r_ibufb(is,il).lt.R_MIN .or. r_ibufb(is,il).gt.R_MAX) r_ibufa(is,il)=0.        
                          if (r_ibufb(is,il).lt.R_MIN .or. r_ibufb(is,il).gt.R_MAX) r_ibufb(is,il)=0.        
                        end do
                      end do
                    end if
                  else
                    do il=1,i_olnum
                      do is=1,i_osnum
                        r_ibufb(is,il)=0            
                      end do
                    end do
                  end if
                end if

                do i_y = 1,i_py
                  do i_x = 1,i_px
                    if (i_y .le. i_olnum .and. i_x .le. i_osnum) then
                      if (i_set .eq. 1) then
                        r_imgi(i_x,i_y) = r_ibufa(i_x,i_y)
                      else
                        r_imgi(i_x,i_y) = r_ibufb(i_x,i_y)
                      end if
                    else
                      r_imgi(i_x,i_y) = 0.
                    end if
                  end do
                end do

c
c  Display images
c
                IF (L_IDISP) THEN
                  if (L_ITOGL) then
                    i_cnt = 0
                    r_sum = 0.
                    r_sqr = 0.
                    do i_y = 1,I_PY
                      do i_x = 1, I_PX
                        if (r_imgi(i_x,i_y) .ne. 0. .and. abs(r_imgi(i_x,i_y)) .ne. 10000.) then
                          i_cnt = i_cnt + 1
                          r_sum = r_sum + r_imgi(i_x,i_y)
                          r_sqr = r_sqr + r_imgi(i_x,i_y)**2
                        end if
                      end do
                    end do

                    if (i_cnt .gt. 0) then
                      r_avg = r_sum/max(i_cnt,1)
                      if ((r_sqr/max(i_cnt,1))-r_avg**2 .lt. 0.) 
     &                         type *,'******* std error = ',(r_sqr/max(i_cnt,1))-r_avg**2
                      r_std = sqrt(abs((r_sqr/max(i_cnt,1))-r_avg**2))
                    else
                      r_avg = 1.e20
                      r_std = .25
                    end if

                    do i_y = 1,I_PY
                      do i_x = 1, I_PX
                        r_dat1(i_x,i_y) = max( min( (r_imgi(i_x,i_y)-r_avg)/(4*r_std)+.5,0.9999), 0.0 )
                      end do
                    end do

                    i_cnt = 0
                    r_sum = 0.
                    r_sqr = 0.
                    do i_y = 1,I_PY
                      do i_x = 1, I_PX
                        if (r_imgj(i_x,i_y) .ne. 0. .and. abs(r_imgj(i_x,i_y)) .ne. 10000.) then
                          i_cnt = i_cnt + 1
                          r_sum = r_sum + r_imgj(i_x,i_y)
                          r_sqr = r_sqr + r_imgj(i_x,i_y)**2
                        end if
                      end do
                    end do

                    if (i_cnt .gt. 0) then
                      r_avg = r_sum/max(i_cnt,1)
                      if ((r_sqr/max(i_cnt,1))-r_avg**2 .lt. 0.) 
     &                         type *,'******* std error = ',(r_sqr/max(i_cnt,1))-r_avg**2
                      r_std = sqrt(abs((r_sqr/max(i_cnt,1))-r_avg**2))
                    else
                      r_avg = 1.e20
                      r_std = .25
                    end if

                    do i_y = 1,I_PY
                      do i_x = 1, I_PX
                        r_dat2(i_x,i_y) = max( min( (r_imgj(i_x,i_y)-r_avg)/(4*r_std)+.5,0.9999), 0.0 )
                      end do
                    end do

                    CALL ZOOM_IMAGE(r_dat1,r_dat,I_PX,I_PY,I_ZOOM)
                    CALL display_img(11, 0, 0, I_PZX, I_PZY, 512, r_dat)

                    CALL ZOOM_IMAGE(r_dat2,r_dat,I_PX,I_PY,I_ZOOM)
                    CALL display_img(12, 0, 0, I_PZX, I_PZY, 512, r_dat)
                  end if
                  call getevent(1,i_event)
                  do while(i_event(2) .ne. 0)
                    if (i_event(2) .eq. 4) then
                      if (i_event(1) .eq.  1) call setcolor(7)
                      if (i_event(1) .eq.  2) call setcolor(8)
                      if (i_event(1) .eq.  4) l_ttogl = .false. 
                      if (i_event(1) .eq.  5) l_ttogl = .true.
                      if (i_event(1) .eq.  6) i_pause =  1
                      if (i_event(1) .eq.  7) i_pause = -1
                      if (i_event(1) .eq.  8) i_pause =  0
                      if (i_event(1) .eq. 10) stop 'Multimatch Done'
                      if (i_event(1) .eq. 15) l_itogl = .false.
                      if (i_event(1) .eq. 16) l_itogl = .true.
                      if (i_event(1) .eq. i_gx-1 .and. l_mdisp) l_mtogl = .false.
                      if (i_event(1) .eq. i_gx   .and. l_mdisp) l_mtogl = .true.
                    end if
                    call getevent(1,i_event)
                  end do
                END IF


                CALL CORRELATE(R_IMGI(I_SDX+1,I_SDY+1),R_IMGJ,I_RWS,I_RWS,
     &                 2*I_SDX+I_RWS,2*I_SDY+I_RWS,1,1,
     &                 R_MEANI,R_STDVI,R_MEANJ,R_STDVJ,
     &                 R_CPEAK,R_NOISE,R_COV,R_EVAL1,R_EVAL2,
     &                 R_EVEC1,R_EVEC2,
     &                 R_IMGC,R_SHFTX,R_SHFTY,I_EDGE,I_FLAGC)
                  R_COV(3)=-R_COV(3)
                  R_SHFTYY=(R_SHFTY-I_SDY)+i_mdy
                  R_SHFTXX=(R_SHFTX-I_SDX)+i_mdx

                i_cnta = 0
                r_outside = 0.0
                do l=max(int(r_shfty)-9,1),min(int(r_shfty)+11,2*i_sdy)
                   do k=max(int(r_shftx)-9,1),min(int(r_shftx)+11,2*i_sdx)
                      i_cnta = i_cnta + 1
                      r_outside = r_outside + r_imgc(k,l)**2
                   end do
                end do
                r_outside = r_outside - r_cpeak**2
                r_outside = r_outside/(max(i_cnta-1,1))

                r_snr = r_cpeak**2/max(r_outside,1.e-10)
               
                IF ( I_FLAGC .EQ. 0 ) THEN
                  IF ( I_EDGE(1) .EQ. 0 .AND. I_EDGE(2) .EQ. 0 ) THEN
                    IF ( L_TDISP .EQ. .TRUE. .AND. L_TTOGL .EQ. .TRUE.) THEN
                      WRITE(6,'(2I10,2F8.2,F7.3,2F8.3)') I_OSOFF,I_OLOFF,
     &                  R_SHFTXX,R_SHFTYY,R_CPEAK,
     &                  MIN(R_EVAL1,999.E0),MIN(R_EVAL2,999.E0)
                    END IF

                    v_oloc1(1) = (i_oloff+i_sdy+i_rws/2 + r_shftyy)*r_spc(1,0) + r_str(1,0)
                    v_oloc1(2) = (i_osoff+i_sdx+i_rws/2 + r_shftxx)*r_spc(2,0) + r_str(2,0)
                    v_oloc1(3)=r_obufb(i_sdx+i_rws/2+int(r_shftxx)-i_mdx,i_sdy+i_rws/2+int(r_shftyy)-i_mdy)

                    if (i_type(0) .eq. 1) then
                      v_oloc2(1) = v_oloc1(1)
                      v_oloc2(2) = v_oloc1(2)
                      v_oloc2(3) = v_oloc1(3)
                    else if (i_type(0) .eq. 2 .or. i_type(0) .eq. -2) then                ! Convert output from xyz to sch
                      r_lon=v_oloc1(1)/r_rad(0)
                      r_lat=v_oloc1(2)/r_rad(0)
                      r_hgt=v_oloc1(3)
                      call sch_to_tcn(r_rad(0),v_oloc2,r_lat,r_lon,r_hgt,1)
                    else if (i_type(0) .eq. 3) then                ! Convert output from xyz to utm
                      r_hgt=v_oloc1(3)
                      call enutoll(r_a,r_e2,i_zone(0),a_grid,v_oloc1,r_lat,r_lon,1)
                      call latlon(r_a,r_e2,v_oloc2,r_lat,r_lon,r_hgt,1)
                    else if (i_type(0) .eq. 4) then                ! Convert output from xyz to utm
                      r_hgt=v_oloc1(3)
                      call utmtoll(r_a,r_e2,i_zone(0),a_grid,v_oloc1,r_lat,r_lon,1)
                      call latlon(r_a,r_e2,v_oloc2,r_lat,r_lon,r_hgt,1)
                    else if (i_type(0) .eq. 5) then                ! Convert output from xyz to equal angle
                      r_lat=v_oloc1(1)/r_rtod
                      r_lon=v_oloc1(2)/r_rtod
                      r_hgt=v_oloc1(3)
                      call latlon(r_a,r_e2,v_oloc2,r_lat,r_lon,r_hgt,1)
                    end if

                    call vecmulti(r_mta(1,1,1),v_oloc2,v_tmp)
                    call vecaddit(v_tmp,r_mta(1,4,1),v_oloc3)

                    if (i_type(1) .eq. 1) then 
                      v_iloc(1) = v_oloc3(1)
                      v_iloc(2) = v_oloc3(2)
                      v_iloc(3) = v_oloc3(3)
                    else if (i_type(1) .eq. 2 .or. i_type(1) .eq. -2) then        ! convert input from sch to xyz
                      call sch_to_tcn(r_rad(1),v_oloc3,r_lat,r_lon,r_hgt,2)
                      v_iloc(1) = r_lon*r_rad(1)
                      v_iloc(2) = r_lat*r_rad(1)
                      v_iloc(3) = r_hgt
                    else if (i_type(1) .eq. 3) then        ! Convert input from utm to xyz
                      call latlon(r_a,r_e2,v_oloc3,r_lat,r_lon,r_hgt,2)
                      call enutoll(r_a,r_e2,i_zone(1),a_grid,v_iloc,r_lat,r_lon,2)
                      v_iloc(3) = r_hgt
                    else if (i_type(1) .eq. 4) then        ! Convert input from utm to xyz
                      call latlon(r_a,r_e2,v_oloc3,r_lat,r_lon,r_hgt,2)
                      call utmtoll(r_a,r_e2,i_zone(1),a_grid,v_iloc,r_lat,r_lon,2)
                      v_iloc(3) = r_hgt
                    else if (i_type(1) .eq. 5) then                ! Convert output from xyz to ll
                      call latlon(r_a,r_e2,v_oloc3,r_lat,r_lon,r_hgt,2)
                      v_iloc(1) = r_lat*r_rtod
                      v_iloc(2) = r_lon*r_rtod
                      v_iloc(3) = r_hgt
                    end if

                    v_iloc(1) = (v_iloc(1) - r_str(1,1))/r_spc(1,1)
                    v_iloc(2) = (v_iloc(2) - r_str(2,1))/r_spc(2,1)


c                    write(15,160) i_osoff+i_sdx+i_rws/2,r_shftxx,
c     &                            i_oloff+i_sdy+i_rws/2,r_shftyy,
c     &                            r_snr,r_cov(1),r_cov(2),r_cov(3),
c     &                            r_ibufb(i_sdx+i_rws/2,i_sdy+i_rws/2),
c     &                            r_obufb(i_sdx+i_rws/2+int(r_shftxx),i_sdy+i_rws/2+int(r_shftyy))
c 160                 format(2(i7,f11.3),4f10.5,2f10.2)

       if ((r_ibufb(i_sdx+i_rws/2,i_sdy+i_rws/2) .ne. 0. .or. (a_dtefile(0) .eq. ' ' .and. a_rmgfile(0) .eq. ' ')) .and. 
     &     (                          v_oloc1(3) .ne. 0. .or. (a_dtefile(1) .eq. ' ' .and. a_rmgfile(1) .eq. ' '))) then
                       write(15,'(x,2(2f10.2,f10.3),2f8.2,4f8.4)') 
     &                    i_oloff+i_sdy+i_rws/2.,i_osoff+i_sdx+i_rws/2.,r_ibufb(i_sdx+i_rws/2,i_sdy+i_rws/2),
     &                    v_iloc(1),v_iloc(2),v_iloc(3),r_shftxx,r_shftyy,r_snr,r_cov(1),r_cov(2),r_cov(3)
                     end if
                  ELSE
                    IF ( L_TDISP .EQ. .TRUE. .AND. L_TTOGL .EQ. .TRUE.) THEN
                      WRITE(6,*) 'Match on Edge'
                    END IF
                  END IF
                END IF

c
c  Display correlation
c
                IF (L_IDISP) THEN
                  if (L_ITOGL) then
                    i_cnt = 0
                    r_sum = 0.
                    r_sqr = 0.
                    do i_y = 1,I_CY
                      do i_x = 1, I_CX
                        if (r_imgc(i_x,i_y) .ne. 0.) then
                          i_cnt = i_cnt + 1
                          r_sum = r_sum + r_imgc(i_x,i_y)
                          r_sqr = r_sqr + r_imgc(i_x,i_y)**2
                        end if
                      end do
                    end do

                    if (i_cnt .gt. 0) then
                      r_avg = r_sum/max(i_cnt,1)
                      if ((r_sqr/max(i_cnt,1))-r_avg**2 .lt. 0.) 
     &                         type *,'******* std error = ',(r_sqr/max(i_cnt,1))-r_avg**2
                      r_std = sqrt(abs((r_sqr/max(i_cnt,1))-r_avg**2))
                    else
                      r_avg = 1.e20
                      r_std = .25
                    end if

                    do i_y = 1,I_CY
                      do i_x = 1, I_CX
                        r_dat1(i_x,i_y) = max( min( (r_imgc(i_x,i_y)-r_avg)/(4*r_std)+.5,0.9999), 0.0 )
                      end do
                    end do

                    if (R_CPEAK .gt. 0) Then
                      r_dat1(MAX(MIN(INT((R_SHFTX))+1,512),1),
     &                     MAX(MIN(INT((R_SHFTY))+1,512),1)) = 1.0
                    end if

                    CALL ZOOM_IMAGE(r_dat1,r_dat,I_CX,I_CY,I_ZOOMC)
                    CALL display_img(13, 0, 0, I_CZX, I_CZY, 512, r_dat)

                    CALL CLEAR_WIN(14)

                    R_X(1) = 0.5
                    R_Y(1) = 0.5
                    R_X(2) = 0.5 + 0.5*R_EVEC1(1)
                    R_Y(2) = 0.5 + 0.5*R_EVEC1(2)
                    CALL PLOT_DATA(14,2,r_x,r_y)

                    R_X(1) = 0.5
                    R_Y(1) = 0.5
                    R_X(2) = 0.5 + 0.5*R_EVEC2(1)
                    R_Y(2) = 0.5 + 0.5*R_EVEC2(2)
                    CALL PLOT_DATA(14,2,r_x,r_y)

                  end if

                  i_loop = 1
                  do while (i_loop .eq. 1)
                    call getevent(1,i_event)
                    if (i_event(2) .eq. 4) then
                      if (i_event(1) .eq.  1) call setcolor(7)
                      if (i_event(1) .eq.  2) call setcolor(8)
                      if (i_event(1) .eq.  4) l_ttogl = .false. 
                      if (i_event(1) .eq.  5) l_ttogl = .true.
                      if (i_event(1) .eq.  6) i_pause =  1
                      if (i_event(1) .eq.  7) i_pause = -1
                      if (i_event(1) .eq.  8) i_pause =  0
                      if (i_event(1) .eq. 10) stop 'Multimatch Done'
                      if (i_event(1) .eq. 15) l_itogl = .false.
                      if (i_event(1) .eq. 16) l_itogl = .true.
                      if (i_event(1) .eq. i_gx-1 .and. l_mdisp) l_mtogl = .false.
                      if (i_event(1) .eq. i_gx   .and. l_mdisp) l_mtogl = .true.
                    end if
                    if (i_event(1) .eq. 0) i_loop = i_pause
                    if (l_mdisp) i_loop = 0
                  end do
                  i_pause = abs(i_pause)
 
                END IF

              IF (L_MDISP) THEN
                if (L_MTOGL) then
                  I_YP = (I_NY)*I_NP
                  I_XP = (I_NX)*I_NP 

                  IF ( I_FLAGC .EQ. 0) THEN
                    R_VAL = (R_SHFTX-I_SDX)/float(i_scale) + .5
                    R_VAL = MAX(MIN(R_VAL,0.9999),0.)
                  ELSE
                    R_VAL = 1.0
                  END IF
                  CALL MAKE_PIX(R_VAL,I_NP,R_PIX,1)
                  CALL display_img(i_gx-2, I_XP, I_YP, I_NP+1, I_NP, I_NP+1, R_PIX)
                end if

                i_loop = 1
                do while (i_loop .eq. 1)
                  call getevent(1,i_event)
                  if (i_event(2) .eq. 4) then
                    if (i_event(1) .eq.  1) call setcolor(7)
                    if (i_event(1) .eq.  2) call setcolor(8)
                    if (i_event(1) .eq.  4) l_ttogl = .false. 
                    if (i_event(1) .eq.  5) l_ttogl = .true.
                    if (i_event(1) .eq.  6) i_pause =  1
                    if (i_event(1) .eq.  7) i_pause = -1
                    if (i_event(1) .eq.  8) i_pause =  0
                    if (i_event(1) .eq. 10) stop 'Multimatch Done'
                    if (i_event(1) .eq. 15 .and. l_idisp) l_itogl = .false.
                    if (i_event(1) .eq. 16 .and. l_idisp) l_itogl = .true.
                    if (i_event(1) .eq. i_gx-1) l_mtogl = .false.
                    if (i_event(1) .eq. i_gx  ) l_mtogl = .true.
                  end if
                  if (i_event(1) .eq. 0) i_loop = i_pause
                end do
c                i_pause = abs(i_pause)
              END IF

C  !@#$%

          end do
        end do

c
c  Close data files
c
        do i_file=0,i_nof
          close(40+i_file,err=910)
910       close(60+i_file,err=920)
920       close(80+i_file,err=930)
930     end do
        close(30,err=940)
940     close(31,err=950)
950     close(32,err=960)
        close(15)

        write(6,*) ' '
        write(6,'(x,a,$)') 'Type <Return> to end '
        read(5,*)
        write(6,*) ' '
        write(6,*) ' '
960     stop 'Done'
        end

        subroutine parse(a_string,i_vals,a_vals)

          implicit none

          character*(*) a_string
          character*(*) a_vals(10)

          integer i_vals

          integer i
          integer i_on
          integer i_cnt
          integer i_cmnt

          i_on = 0
          i_cnt = 0
          i_vals = 0
          i_cmnt = 0
          if (a_string(1:1) .ne. '#' .and. a_string(1:1) .ne. '!' .and. a_string(1:1) .ne. '%') then
            do i=1,len(a_string)
              if (a_string(i:i) .ne. '!' .and. i_cmnt .eq. 0) then
                if (a_string(i:i) .ne. ';') then
                  if (a_string(i:i) .ne. ' ') then
                    if (i_on .eq. 0) then
                      i_on = 1
                      i_cnt = 0
                      i_vals=min(i_vals+1,10)
                      a_vals(i_vals)=' '
                    end if
                    i_cnt = i_cnt+1
                    a_vals(i_vals)(i_cnt:i_cnt) = a_string(i:i)
                  end if
                else 
                  i_on = 0
                  i_cnt = 0
                end if
              else
                i_cmnt = 1
              end if
            end do
          end if
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
            type *,'Determinant =  0 in Subroutine matinvrt'
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

        subroutine inplane2d(v_pln,i_pln,v_pnt,i_flg)

          implicit none
        
          integer i_flg

          integer i_pln(4)

          real*8 v_pln(4,4)

          real*8 v_t1(3)
          real*8 v_t2(3)
          real*8 v_t3(3)
          real*8 v_t4(3)

          real*8 v_pnt(4)

          real*8 r_a
          real*8 r_b
          real*8 r_c
          real*8 r_d

          real*8 r_crs12
          real*8 r_crs13
          real*8 r_crs23
          real*8 r_crs24
          real*8 r_crs34
          real*8 r_crs31
          real*8 r_crs41
          real*8 r_crs42

          integer*4 i_try(0:1,0:1,0:1,0:1)
          data i_try(0,0,0,0) / 6 /
          data i_try(0,0,0,1) / 6 /
          data i_try(0,0,1,0) / 6 /
          data i_try(0,0,1,1) / 6 /
          data i_try(0,1,0,0) / 6 /
          data i_try(0,1,0,1) / 6 /
          data i_try(0,1,1,0) / 6 /
          data i_try(0,1,1,1) / 1 /
          data i_try(1,0,0,0) / 6 /
          data i_try(1,0,0,1) / 6 /
          data i_try(1,0,1,0) / 6 /
          data i_try(1,0,1,1) / 2 /
          data i_try(1,1,0,0) / 6 /
          data i_try(1,1,0,1) / 3 /
          data i_try(1,1,1,0) / 4 /
          data i_try(1,1,1,1) / 5 /

          save i_try

c
c  check if within triangle
c

          i_flg = 0
          v_pnt(3) = 0.
          v_pnt(4) = 0.
          goto (100,200,300,400,500,600) , i_try(i_pln(1),i_pln(2),i_pln(3),i_pln(4))
          stop 'error executing conditional goto in inplane2d'

100       continue
            v_pln(1,1) = v_pln(1,2)
            v_pln(2,1) = v_pln(2,3)
            v_t1(1) =  v_pnt(1) - v_pln(1,1)
            v_t1(2) =  v_pnt(2) - v_pln(2,1)
            v_t2(1) =  v_pnt(1) - v_pln(1,2)
            v_t2(2) =  v_pnt(2) - v_pln(2,2)
            v_t3(1) =  v_pnt(1) - v_pln(1,3)
            v_t3(2) =  v_pnt(2) - v_pln(2,3)
            v_t4(1) =  v_pnt(1) - v_pln(1,4)
            v_t4(2) =  v_pnt(2) - v_pln(2,4)

            r_crs23 = v_t2(1)*v_t3(2) - v_t2(2)*v_t3(1)
            r_crs34 = v_t3(1)*v_t4(2) - v_t3(2)*v_t4(1)
            r_crs42 = v_t4(1)*v_t2(2) - v_t4(2)*v_t2(1)

            r_crs12 = v_t1(1)*v_t2(2) - v_t1(2)*v_t2(1)
            r_crs31 = v_t3(1)*v_t1(2) - v_t3(2)*v_t1(1)

            if ((r_crs23*r_crs34.ge.0. .and. r_crs34*r_crs42.ge.0. .and. r_crs42*r_crs23.ge.0.) .or.		! interpolate
     &          (r_crs12*r_crs23.ge.0. .and. r_crs23*r_crs31.ge.0. .and. r_crs31*r_crs12.ge.0.) ) then	! extrapolate

              r_c = (v_pln(1,2)-v_pln(1,4))*(v_pln(2,3)-v_pln(2,4)) - 
     &              (v_pln(2,2)-v_pln(2,4))*(v_pln(1,3)-v_pln(1,4))
              if (r_c .ne. 0) then
                i_flg=1

                r_a = (v_pln(2,2)-v_pln(2,4))*(v_pln(3,3)-v_pln(3,4)) - 
     &                (v_pln(3,2)-v_pln(3,4))*(v_pln(2,3)-v_pln(2,4))
                r_b = (v_pln(3,2)-v_pln(3,4))*(v_pln(1,3)-v_pln(1,4)) - 
     &                (v_pln(1,2)-v_pln(1,4))*(v_pln(3,3)-v_pln(3,4))
                r_d = r_a*v_pln(1,4)+r_b*v_pln(2,4)+r_c*v_pln(3,4)
                v_pnt(3) = (r_d - r_a*v_pnt(1) - r_b*v_pnt(2))/r_c

                r_a = (v_pln(2,2)-v_pln(2,4))*(v_pln(4,3)-v_pln(4,4)) - 
     &                (v_pln(4,2)-v_pln(4,4))*(v_pln(2,3)-v_pln(2,4))
                r_b = (v_pln(4,2)-v_pln(4,4))*(v_pln(1,3)-v_pln(1,4)) - 
     &                (v_pln(1,2)-v_pln(1,4))*(v_pln(4,3)-v_pln(4,4))
                r_d = r_a*v_pln(1,4)+r_b*v_pln(2,4)+r_c*v_pln(4,4)
                v_pnt(4) = (r_d - r_a*v_pnt(1) - r_b*v_pnt(2))/r_c
              end if
            else

            end if
            goto 600
200       continue
            v_pln(1,2) = v_pln(1,1)
            v_pln(2,2) = v_pln(2,4)
            v_t1(1) =  v_pnt(1) - v_pln(1,1)
            v_t1(2) =  v_pnt(2) - v_pln(2,1)
            v_t2(1) =  v_pnt(1) - v_pln(1,2)
            v_t2(2) =  v_pnt(2) - v_pln(2,2)
            v_t3(1) =  v_pnt(1) - v_pln(1,3)
            v_t3(2) =  v_pnt(2) - v_pln(2,3)
            v_t4(1) =  v_pnt(1) - v_pln(1,4)
            v_t4(2) =  v_pnt(2) - v_pln(2,4)

            r_crs13 = v_t1(1)*v_t3(2) - v_t1(2)*v_t3(1)
            r_crs34 = v_t3(1)*v_t4(2) - v_t3(2)*v_t4(1)
            r_crs41 = v_t4(1)*v_t1(2) - v_t4(2)*v_t1(1)

            r_crs12 = v_t1(1)*v_t2(2) - v_t1(2)*v_t2(1)
            r_crs24 = v_t2(1)*v_t4(2) - v_t2(2)*v_t4(1)

            if ((r_crs13*r_crs34.ge.0. .and. r_crs34*r_crs41.ge.0. .and. r_crs41*r_crs13.ge.0.) .or.
     &          (r_crs12*r_crs24.ge.0. .and. r_crs24*r_crs41.ge.0. .and. r_crs41*r_crs12.ge.0.) ) then

              r_c = (v_pln(1,1)-v_pln(1,3))*(v_pln(2,4)-v_pln(2,3)) - 
     &              (v_pln(2,1)-v_pln(2,3))*(v_pln(1,4)-v_pln(1,3))
              if (r_c .ne. 0) then
                i_flg=2

                r_a = (v_pln(2,1)-v_pln(2,3))*(v_pln(3,4)-v_pln(3,3)) - 
     &                (v_pln(3,1)-v_pln(3,3))*(v_pln(2,4)-v_pln(2,3))
                r_b = (v_pln(3,1)-v_pln(3,3))*(v_pln(1,4)-v_pln(1,3)) - 
     &                (v_pln(1,1)-v_pln(1,3))*(v_pln(3,4)-v_pln(3,3))
                r_d = r_a*v_pln(1,3)+r_b*v_pln(2,3)+r_c*v_pln(3,3)
                v_pnt(3) = (r_d - r_a*v_pnt(1) - r_b*v_pnt(2))/r_c

                r_a = (v_pln(2,1)-v_pln(2,3))*(v_pln(4,4)-v_pln(4,3)) - 
     &                (v_pln(4,1)-v_pln(4,3))*(v_pln(2,4)-v_pln(2,3))
                r_b = (v_pln(4,1)-v_pln(4,3))*(v_pln(1,4)-v_pln(1,3)) - 
     &                (v_pln(1,1)-v_pln(1,3))*(v_pln(4,4)-v_pln(4,3))
                r_d = r_a*v_pln(1,3)+r_b*v_pln(2,3)+r_c*v_pln(4,3)
                v_pnt(4) = (r_d - r_a*v_pnt(1) - r_b*v_pnt(2))/r_c
              end if
            end if
            goto 600
300       continue
            v_pln(1,3) = v_pln(1,4)
            v_pln(2,3) = v_pln(2,1)
            v_t1(1) =  v_pnt(1) - v_pln(1,1)
            v_t1(2) =  v_pnt(2) - v_pln(2,1)
            v_t2(1) =  v_pnt(1) - v_pln(1,2)
            v_t2(2) =  v_pnt(2) - v_pln(2,2)
            v_t3(1) =  v_pnt(1) - v_pln(1,3)
            v_t3(2) =  v_pnt(2) - v_pln(2,3)
            v_t4(1) =  v_pnt(1) - v_pln(1,4)
            v_t4(2) =  v_pnt(2) - v_pln(2,4)

            r_crs12 = v_t1(1)*v_t2(2) - v_t1(2)*v_t2(1)
            r_crs24 = v_t2(1)*v_t4(2) - v_t2(2)*v_t4(1)
            r_crs41 = v_t4(1)*v_t1(2) - v_t4(2)*v_t1(1)

            r_crs13 = v_t1(1)*v_t3(2) - v_t1(2)*v_t3(1)
            r_crs34 = v_t3(1)*v_t4(2) - v_t3(2)*v_t4(1)

            if ((r_crs12*r_crs24.ge.0. .and. r_crs24*r_crs41.ge.0. .and. r_crs41*r_crs12.ge.0.) .or.
     &          (r_crs13*r_crs34.ge.0. .and. r_crs34*r_crs41.ge.0. .and. r_crs41*r_crs13.ge.0.) ) then

              r_c = (v_pln(1,1)-v_pln(1,2))*(v_pln(2,4)-v_pln(2,2)) - 
     &              (v_pln(2,1)-v_pln(2,2))*(v_pln(1,4)-v_pln(1,2))
              if (r_c .ne. 0) then
                i_flg=3

                r_a = (v_pln(2,1)-v_pln(2,2))*(v_pln(3,4)-v_pln(3,2)) - 
     &                (v_pln(3,1)-v_pln(3,2))*(v_pln(2,4)-v_pln(2,2))
                r_b = (v_pln(3,1)-v_pln(3,2))*(v_pln(1,4)-v_pln(1,2)) - 
     &                (v_pln(1,1)-v_pln(1,2))*(v_pln(3,4)-v_pln(3,2))
                r_d = r_a*v_pln(1,2)+r_b*v_pln(2,2)+r_c*v_pln(3,2)
                v_pnt(3) = (r_d - r_a*v_pnt(1) - r_b*v_pnt(2))/r_c

                r_a = (v_pln(2,1)-v_pln(2,2))*(v_pln(4,4)-v_pln(4,2)) - 
     &                (v_pln(4,1)-v_pln(4,2))*(v_pln(2,4)-v_pln(2,2))
                r_b = (v_pln(4,1)-v_pln(4,2))*(v_pln(1,4)-v_pln(1,2)) - 
     &                (v_pln(1,1)-v_pln(1,2))*(v_pln(4,4)-v_pln(4,2))
                r_d = r_a*v_pln(1,2)+r_b*v_pln(2,2)+r_c*v_pln(4,2)
                v_pnt(4) = (r_d - r_a*v_pnt(1) - r_b*v_pnt(2))/r_c

              end if
            end if
            goto 600
400       continue
            v_pln(1,4) = v_pln(1,3)
            v_pln(2,4) = v_pln(2,2)
            v_t1(1) =  v_pnt(1) - v_pln(1,1)
            v_t1(2) =  v_pnt(2) - v_pln(2,1)
            v_t2(1) =  v_pnt(1) - v_pln(1,2)
            v_t2(2) =  v_pnt(2) - v_pln(2,2)
            v_t3(1) =  v_pnt(1) - v_pln(1,3)
            v_t3(2) =  v_pnt(2) - v_pln(2,3)
            v_t4(1) =  v_pnt(1) - v_pln(1,4)
            v_t4(2) =  v_pnt(2) - v_pln(2,4)

            r_crs12 = v_t1(1)*v_t2(2) - v_t1(2)*v_t2(1)
            r_crs23 = v_t2(1)*v_t3(2) - v_t2(2)*v_t3(1)
            r_crs31 = v_t3(1)*v_t1(2) - v_t3(2)*v_t1(1)

            r_crs34 = v_t3(1)*v_t4(2) - v_t3(2)*v_t4(1)
            r_crs42 = v_t4(1)*v_t2(2) - v_t4(2)*v_t2(1)

            if ((r_crs12*r_crs23.ge.0. .and. r_crs23*r_crs31.ge.0. .and. r_crs31*r_crs12.ge.0.) .or.
     &          (r_crs23*r_crs34.ge.0. .and. r_crs34*r_crs42.ge.0. .and. r_crs42*r_crs23.ge.0.) ) then

              r_c = (v_pln(1,2)-v_pln(1,1))*(v_pln(2,3)-v_pln(2,1)) - 
     &              (v_pln(2,2)-v_pln(2,1))*(v_pln(1,3)-v_pln(1,1))

              if (r_c .ne. 0) then
                i_flg=4

                r_a = (v_pln(2,2)-v_pln(2,1))*(v_pln(3,3)-v_pln(3,1)) - 
     &                (v_pln(3,2)-v_pln(3,1))*(v_pln(2,3)-v_pln(2,1))
                r_b = (v_pln(3,2)-v_pln(3,1))*(v_pln(1,3)-v_pln(1,1)) - 
     &                (v_pln(1,2)-v_pln(1,1))*(v_pln(3,3)-v_pln(3,1))
                r_d = r_a*v_pln(1,1)+r_b*v_pln(2,1)+r_c*v_pln(3,1)
                v_pnt(3) = (r_d - r_a*v_pnt(1) - r_b*v_pnt(2))/r_c

                r_a = (v_pln(2,2)-v_pln(2,1))*(v_pln(4,3)-v_pln(4,1)) - 
     &                (v_pln(4,2)-v_pln(4,1))*(v_pln(2,3)-v_pln(2,1))
                r_b = (v_pln(4,2)-v_pln(4,1))*(v_pln(1,3)-v_pln(1,1)) - 
     &                (v_pln(1,2)-v_pln(1,1))*(v_pln(4,3)-v_pln(4,1))
                r_d = r_a*v_pln(1,1)+r_b*v_pln(2,1)+r_c*v_pln(4,1)
                v_pnt(4) = (r_d - r_a*v_pnt(1) - r_b*v_pnt(2))/r_c

              end if

            end if
            goto 600
500       continue
            v_t1(1) =  v_pnt(1) - v_pln(1,1)
            v_t1(2) =  v_pnt(2) - v_pln(2,1)
            v_t2(1) =  v_pnt(1) - v_pln(1,2)
            v_t2(2) =  v_pnt(2) - v_pln(2,2)
            v_t3(1) =  v_pnt(1) - v_pln(1,3)
            v_t3(2) =  v_pnt(2) - v_pln(2,3)

            r_crs12 = v_t1(1)*v_t2(2) - v_t1(2)*v_t2(1)
            r_crs23 = v_t2(1)*v_t3(2) - v_t2(2)*v_t3(1)
            r_crs31 = v_t3(1)*v_t1(2) - v_t3(2)*v_t1(1)

            if (r_crs12*r_crs23.ge.0. .and. r_crs23*r_crs31.ge.0. .and. r_crs31*r_crs12.ge.0.) then  ! Inside T1

              r_c = (v_pln(1,1)-v_pln(1,2))*(v_pln(2,3)-v_pln(2,2)) - 
     &              (v_pln(2,1)-v_pln(2,2))*(v_pln(1,3)-v_pln(1,2))

              if (r_c .ne. 0.) then
                i_flg=4

                r_a = (v_pln(2,1)-v_pln(2,2))*(v_pln(3,3)-v_pln(3,2)) - 
     &                (v_pln(3,1)-v_pln(3,2))*(v_pln(2,3)-v_pln(2,2))
                r_b = (v_pln(3,1)-v_pln(3,2))*(v_pln(1,3)-v_pln(1,2)) - 
     &                (v_pln(1,1)-v_pln(1,2))*(v_pln(3,3)-v_pln(3,2))
                r_d = r_a*v_pln(1,2)+r_b*v_pln(2,2)+r_c*v_pln(3,2)
                v_pnt(3) = (r_d - r_a*v_pnt(1) - r_b*v_pnt(2))/r_c

                r_a = (v_pln(2,1)-v_pln(2,2))*(v_pln(4,3)-v_pln(4,2)) - 
     &                (v_pln(4,1)-v_pln(4,2))*(v_pln(2,3)-v_pln(2,2))
                r_b = (v_pln(4,1)-v_pln(4,2))*(v_pln(1,3)-v_pln(1,2)) - 
     &                (v_pln(1,1)-v_pln(1,2))*(v_pln(4,3)-v_pln(4,2))
                r_d = r_a*v_pln(1,2)+r_b*v_pln(2,2)+r_c*v_pln(4,2)
                v_pnt(4) = (r_d - r_a*v_pnt(1) - r_b*v_pnt(2))/r_c

              else

c                type *,'r_c = 0',(v_pln(1,1)-v_pln(1,2)),(v_pln(2,3)-v_pln(2,2)) , 
c     &              (v_pln(2,1)-v_pln(2,2)),(v_pln(1,3)-v_pln(1,2))
c                type *,'       ',v_pln(1,1),v_pln(1,2),v_pln(2,3),v_pln(2,2),v_pln(2,1),v_pln(1,3)

              end if

            else 
              v_t4(1) =  v_pnt(1) - v_pln(1,4)
              v_t4(2) =  v_pnt(2) - v_pln(2,4)
              r_crs34 = v_t3(1)*v_t4(2) - v_t3(2)*v_t4(1)
              r_crs42 = v_t4(1)*v_t2(2) - v_t4(2)*v_t2(1)
              if (r_crs23*r_crs34.ge.0. .and. r_crs34*r_crs42.ge.0. .and. r_crs42*r_crs23.ge.0.) then

                r_c = (v_pln(1,2)-v_pln(1,4))*(v_pln(2,3)-v_pln(2,4)) - 
     &                (v_pln(2,2)-v_pln(2,4))*(v_pln(1,3)-v_pln(1,4))

                if (r_c .ne. 0) then
                  i_flg=1

                  r_a = (v_pln(2,2)-v_pln(2,4))*(v_pln(3,3)-v_pln(3,4)) - 
     &                  (v_pln(3,2)-v_pln(3,4))*(v_pln(2,3)-v_pln(2,4))
                  r_b = (v_pln(3,2)-v_pln(3,4))*(v_pln(1,3)-v_pln(1,4)) - 
     &                  (v_pln(1,2)-v_pln(1,4))*(v_pln(3,3)-v_pln(3,4))
                  r_d = r_a*v_pln(1,4)+r_b*v_pln(2,4)+r_c*v_pln(3,4)
                  v_pnt(3) = (r_d - r_a*v_pnt(1) - r_b*v_pnt(2))/r_c

                  r_a = (v_pln(2,2)-v_pln(2,4))*(v_pln(4,3)-v_pln(4,4)) - 
     &                  (v_pln(4,2)-v_pln(4,4))*(v_pln(2,3)-v_pln(2,4))
                  r_b = (v_pln(4,2)-v_pln(4,4))*(v_pln(1,3)-v_pln(1,4)) - 
     &                  (v_pln(1,2)-v_pln(1,4))*(v_pln(4,3)-v_pln(4,4))
                  r_d = r_a*v_pln(1,4)+r_b*v_pln(2,4)+r_c*v_pln(4,4)
                  v_pnt(4) = (r_d - r_a*v_pnt(1) - r_b*v_pnt(2))/r_c

                end if

              end if
            end if

600       continue

          if (v_pnt(4) .le. 0.) then 
            i_flg = 0
            v_pnt(4) = 0.
          end if

        return
        end

cccc


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
          do while(i_len .gt. 0 .and. a_string(i_len:i_len) .eq. ' ')
            i_len=i_len-1
          end do

          length=i_len
          return
        end


c****************************************************************
        subroutine read_hdr(a_hdrfile,a_type,i_lsize,i_ssize,r_peg,i_zone,
     &              r_str,r_spc,i_mbytes,i_dbytes,r_mdnc,r_ddnc,i_err)

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
        integer*4 i_zone

        integer*4 i_mbytes
        integer*4 i_dbytes

        real*8 r_peg(3)
        real*8 r_str(2)
        real*8 r_spc(2)
        real*8 r_mdnc(2)
        real*8 r_ddnc(2)


c	LOCAL VARIABLES: 

        integer*4 i
        integer*4 j
        integer*4 i_cnt
        real*8 r_atm(3,4)
        real*8 r_pi
        real*8 r_rtod

        character*255 a_tmp

        integer length
        external length

c	DATA STATEMENTS: none

C	FUNCTION STATEMENTS: none

c  	PROCESSING STEPS:
        
c
c  Initialize pi and conversions
c
        r_pi = 4.d0*atan(1.0d0)
        r_rtod = 180.0d0/r_pi

        i_err=1
        i_cnt=0

        open(12,file=a_hdrfile,status='old',form='formatted',err=900)
        write(6,*) ' '
        write(6,*) 'Opening hdr input  file: ',a_hdrfile(1:52)

        do i=1,1000
           read(12,'(a)',end=900) a_tmp
           if (a_tmp .eq. ' ') then
             ! do nothing
           else if (index(a_tmp,'Data file type') .gt. 0) then
             a_type = a_tmp(1:max(1,index(a_tmp,';')-1))
             do j=1,length(a_type)
               if (a_type(1:1) .eq. ' ') a_type = a_type(2:)
             end do
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
           else if (index(a_tmp,'Zone') .gt. 0) then
             read(a_tmp,*) i_zone
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
           else if (index(a_tmp,'Elevation Scale and Shift') .gt. 0) then
             read(a_tmp,*) r_ddnc
           else if (index(a_tmp,'Magnitude Bytes per Pixel') .gt. 0) then
             read(a_tmp,*) i_mbytes
           else if (index(a_tmp,'Elevation Bytes per pixel') .gt. 0) then
             read(a_tmp,*) i_dbytes
           else if (index(a_tmp,'Elevation Bytes per Pixel') .gt. 0) then
             read(a_tmp,*) i_dbytes
           end if
        end do
        close(12)
        stop 'Error reading header file, too many lines'

900     close(12,err=910)
910     if (i_cnt .eq. 15) i_err = 0
        return
      end

c****************************************************************
        subroutine write_hdr(i_hdrfile,a_filter,a_type,i_lsize,i_ssize,r_peg,
     &              r_str,r_spc,i_zone,i_err)

c****************************************************************

       	implicit none

c	INPUT VARIABLES:
        
        integer i_hdrfile
        character*(*) a_filter         !header file
        character*(*) a_type

c	OUTPUT VARIABLES: 

        integer*4 i_err
        integer*4 i_lsize
        integer*4 i_ssize
        integer*4 i_zone

        real*8 r_p(3)
        real*8 r_peg(3)
        real*8 r_str(2)
        real*8 r_spc(2)


c	LOCAL VARIABLES: 

        real*8 r_pi
        real*8 r_rtod


c	DATA STATEMENTS: none

C	FUNCTION STATEMENTS: 

        integer length
        external length

c  	PROCESSING STEPS:
        
c
c  Initialize pi and conversions
c
        r_pi = 4.d0*atan(1.0d0)
        r_rtod = 180.0d0/r_pi

           i_err=1
           write(6,*) 'Writing HDR info for file ',a_filter
           write(i_hdrfile,'(a45,3a)') a_type(:max(length(a_type),1)),' ; ',
     &               a_filter(:length(a_filter)),' Data file type '
           write(i_hdrfile,'(2i15,  15x,3a)') i_lsize,i_ssize,' ; ',a_filter(:length(a_filter)),
     &               ' Data file dimensions '

           if (a_type .eq. 'UTM' .or. a_type .eq. 'utm') r_spc(1) = -r_spc(1)   !  Flip for printout, then flip back
           if (a_type .eq. 'SRH' .or. a_type .eq. 'srh') then
             r_spc(2) = -r_spc(2)
             r_str(2) = -r_str(2)
           end if

           if (r_spc(1) .ge. 1.) then
             write(i_hdrfile,'(2f15.2,15x,3a)') r_spc,' ; ',a_filter(:length(a_filter)),
     &               ' Post Spacing'
           else
             write(i_hdrfile,'(2f15.8,15x,3a)') r_spc,' ; ',a_filter(:length(a_filter)),
     &               ' Post Spacing'
           end if
           if (a_type .eq. 'UTM' .or. a_type .eq. 'utm') r_spc(1) = -r_spc(1)   !  Flip for printout, then flip back
           if (a_type .eq. 'SRH' .or. a_type .eq. 'srh') then
             r_spc(2) = -r_spc(2)
             r_str(2) = -r_str(2)
           end if

           if (a_type .eq. 'EQA' .or. a_type .eq. 'eqa') then
             write(i_hdrfile,'(2f15.6,15x,3a)') r_str,' ; ',a_filter(:length(a_filter)),
     &               ' Starting corner position (s,c)'
           else
             write(i_hdrfile,'(2f15.2,15x,3a)') r_str,' ; ',a_filter(:length(a_filter)),
     &               ' Starting corner position (s,c)'
           end if
           r_p(1) = r_peg(1)*r_rtod
           r_p(2) = r_peg(2)*r_rtod
           r_p(3) = r_peg(3)*r_rtod
           write(i_hdrfile,'(3f15.7,3a)')  r_p, ' ; ',a_filter(:length(a_filter)),
     &               ' Peg position (WGS-84)'
           if (a_type .eq. 'neu' .or. a_type .eq. 'enu' .or. a_type .eq. 'utm') then 
             write(i_hdrfile,'(i15,30x,3a)') i_zone,' ; ',a_filter(:length(a_filter)),' UTM Zone '
           else
             write(i_hdrfile,'(a)') ' '
           end if
           write(i_hdrfile,'(a)') ' '
           i_err=0
        return
      end

      SUBROUTINE ZOOM_IMAGE(R_IMG,R_DSP,I_WX,I_WY,I_ZOOM)

        IMPLICIT NONE

        INTEGER I,J
        INTEGER II,JJ
        INTEGER I_WX,I_WY
        INTEGER I_ZOOM

        REAL R_IMG(512,512)
        REAL R_DSP(512,512)

          DO I=1,I_WY*I_ZOOM
            II=(I+I_ZOOM-1)/I_ZOOM
            DO J=1,I_WX*I_ZOOM
              JJ=(J+I_ZOOM-1)/I_ZOOM
              R_DSP(J,I)=MIN(MAX(ABS(R_IMG(JJ,II)),0.0),1.0)
            END DO
          END DO

        RETURN
      END

      SUBROUTINE MAKE_PIX(R_VAL,I_NP,R_PIX,I_FLAG)

        IMPLICIT NONE
        
        INTEGER I
        INTEGER J
        INTEGER I_CNT
        REAL R_VAL
        INTEGER I_NP
        INTEGER I_FLAG

        REAL R_PIX(1024)

        I_CNT=0
        DO I=1,I_NP
          DO J=1,I_NP+I_FLAG
            I_CNT=I_CNT+1
            IF ( J .LE. I_NP ) THEN
              R_PIX(I_CNT)=R_VAL
            ELSE
              R_PIX(I_CNT)=1.
            END IF
          END DO
        END DO
        RETURN
   
      END

      SUBROUTINE CORRELATE(R_IMGI,R_IMGJ,I_WSXI,I_WSYI,I_WSXJ,I_WSYJ,I_AVGX,I_AVGY,
     &     R_MEANI,R_STDVI,R_MEANJ,R_STDVJ,R_PEAK,
     &     R_NOISE,R_COV,R_EVAL1,R_EVAL2,R_EVEC1,R_EVEC2,
     &     R_IMGC,R_SHFTX,R_SHFTY,I_EDGE,I_FLAG)

        IMPLICIT NONE

        INTEGER I_IDX,I_IDY
        parameter(I_IDX=512)
        parameter(I_IDY=512)

        INTEGER I_WSYI
        INTEGER I_WSXI
        INTEGER I_WSYJ
        INTEGER I_WSXJ
        INTEGER I_AVGY
        INTEGER I_AVGX

        INTEGER I_WSAYI
        INTEGER I_WSAXI
        INTEGER I_WSAYJ
        INTEGER I_WSAXJ
        INTEGER I_WSAXYI
        INTEGER I_WSAXYJ

        INTEGER I
        INTEGER J
        INTEGER M
        INTEGER N
        INTEGER IX
        INTEGER IY
        INTEGER IXX
        INTEGER IYY

        INTEGER I_SHFTY
        INTEGER I_SHFTX
        integer i_cnt
        INTEGER I_CNTI
        INTEGER I_CNTJ
        INTEGER I_CNTAI
        INTEGER I_CNTAJ
        INTEGER I_EDGE(2)
        INTEGER I_FLAG
        INTEGER*4 i_numj(0:I_IDX,0:I_IDY)
        INTEGER*4 I_HOLEIX(I_IDX*I_IDY)
        INTEGER*4 I_HOLEIY(I_IDX*I_IDY)
        INTEGER*4 I_HOLEJX(I_IDX*I_IDY)
        INTEGER*4 I_HOLEJY(I_IDX*I_IDY)
        INTEGER*4 I_HOLEI
        INTEGER*4 I_HOLEJ

        REAL*4 R_SHFTY
        REAL*4 R_SHFTX
        REAL*4 R_PEAK
        REAL*4 R_SHRP
        REAL*4 R_MEANI
        REAL*4 R_MEANJ
        REAL*4 R_STDVI
        REAL*4 R_STDVJ 
        REAL*4 R_NOISE
        REAL*4 R_COV(3)
        REAL*4 R_EVAL1
        REAL*4 R_EVAL2
        REAL*4 R_EVEC1(2)
        REAL*4 R_EVEC2(2)

        REAL*8 R_SUMC
        REAL*8 R_SUMI
        REAL*8 R_SMQI
        REAL*8 R_SUMJ
        REAL*8 R_SMQJ

        REAL*8 R_SUMII
        REAL*8 R_SMQII
        REAL*8 R_SUMJJ(0:I_IDX,0:I_IDY)
        REAL*8 R_SMQJJ(0:I_IDX,0:I_IDY)

        REAL*8 R_CRPD(0:I_IDX,0:I_IDY)
        REAL*8 R_CORN(0:I_IDX,0:I_IDY)
        REAL*8 R_DENOM

        REAL*8 R_MEANIX
        REAL*8 R_MEANJX
        REAL*8 R_VARNIX
        REAL*8 R_VARNJX
        REAL*8 R_VARNI
        REAL*8 R_VARNJ

        REAL*4 R_DXX
        REAL*4 R_DYY
        REAL*4 R_DXY
        REAL*4 R_N2
        REAL*4 R_N4
        REAL*4 R_U
        REAL*4 R_U2

        REAL*8 R_IMI(I_IDX,I_IDY)
        REAL*8 R_IMJ(I_IDX,I_IDY)

        REAL*4 R_IMGI(I_IDX,I_IDY)
        REAL*4 R_IMGJ(I_IDX,I_IDY)
        REAL*4 R_IMGC(I_IDX,I_IDY)

        LOGICAL L_INIT
        DATA L_INIT/.FALSE./

        SAVE R_IMI,R_IMJ
        SAVE R_SUMJJ,R_SMQJJ
        SAVE I_NUMJ
        SAVE I_HOLEIX,I_HOLEIY
        SAVE I_HOLEJX,I_HOLEJY

          I_HOLEI=0
          I_HOLEJ=0
          I_EDGE(1)=0
          I_EDGE(2)=0
          IF ( I_AVGY .LE. 0 ) I_AVGY=1
          IF ( I_AVGX .LE. 0 ) I_AVGX=1
          I_WSAYI=I_WSYI/I_AVGY
          I_WSAXI=I_WSXI/I_AVGX
          I_WSAYJ=I_WSYJ/I_AVGY
          I_WSAXJ=I_WSXJ/I_AVGX
          I_WSAXYI=I_WSAYI*I_WSAXI
          I_WSAXYJ=I_WSAYJ*I_WSAXJ

          R_COV(1)=0.
          R_COV(2)=0.
          R_COV(3)=0. 

          I_CNTAI=0
          I_CNTAJ=0
          R_SUMII=0.
          R_SMQII=0.
          DO IY=1,I_WSAYJ
            DO IX=1,I_WSAXJ
              R_IMGC(IX,IY)=0.
              R_IMI(IX,IY)=0.d0
              R_IMJ(IX,IY)=0.d0
              I_CNTI=0
              I_CNTJ=0
              DO IYY=(IY-1)*I_AVGY+1,IY*I_AVGY
                DO IXX=(IX-1)*I_AVGX+1,IX*I_AVGX
                  IF ( IYY .LE. I_WSYI .AND. IXX .LE. I_WSXI ) THEN
                    IF ( R_IMGI(IXX,IYY) .NE. 0. .AND. ABS(R_IMGI(IXX,IYY)) .NE. 10000.  ) THEN
                      I_CNTI = I_CNTI+1
                      R_IMI(IX,IY)=R_IMI(IX,IY)+DBLE(R_IMGI(IXX,IYY))
                      R_SUMII=R_SUMII+R_IMI(IX,IY)
                      R_SMQII=R_SMQII+R_IMI(IX,IY)**2
                    END IF
                  END IF
                  IF ( R_IMGJ(IXX,IYY) .NE. 0. .AND. ABS(R_IMGJ(IXX,IYY)) .NE. 10000.  ) THEN
                    I_CNTJ = I_CNTJ+1
                    R_IMJ(IX,IY)=R_IMJ(IX,IY)+DBLE(R_IMGJ(IXX,IYY))
                  END IF
                END DO
              END DO
              IF ( I_CNTI .NE. 0 ) THEN
                I_CNTAI=I_CNTAI+1
                R_IMI(IX,IY)=R_IMI(IX,IY)/I_CNTI
              ELSE
                IF ( IY .LE. I_WSAYI .AND. IX .LE. I_WSAXI ) THEN
                  I_HOLEI=I_HOLEI+1
                  I_HOLEIX(I_HOLEI) = IX
                  I_HOLEIY(I_HOLEI) = IY
                END IF
              END IF
              IF ( I_CNTJ .NE. 0 ) THEN
                R_IMJ(IX,IY)=R_IMJ(IX,IY)/I_CNTJ
                I_CNTAJ=I_CNTAJ+1
              ELSE
                I_HOLEJ=I_HOLEJ+1
                I_HOLEJX(I_HOLEJ) = IX
                I_HOLEJY(I_HOLEJ) = IY
              END IF
            END DO
          END DO

        IF (I_CNTAI .GE. 0.5*I_WSAXYI .AND. 
     &      I_CNTAJ .GE. 0.5*I_WSAXYJ ) THEN

          DO IY=0,I_WSAYJ-1
            i_numj(0,iy) = 0
            i_numj(1,iy) = 0
            R_SUMJJ(0,IY)=0.
            R_SUMJJ(1,IY)=0.
            R_SMQJJ(0,IY)=0.
            R_SMQJJ(1,IY)=0.
            DO IX=1,I_WSAXI
              if (r_imj(ix,iy+1) .ne. 0.) i_numj(1,iy) = i_numj(1,iy) + 1
              R_SUMJJ(1,IY)=R_SUMJJ(1,IY)+R_IMJ(IX,IY+1)
              R_SMQJJ(1,IY)=R_SMQJJ(1,IY)+R_IMJ(IX,IY+1)**2
            END DO
            DO IX=1,I_WSAXJ-I_WSAXI
              i_numj(ix+1,iy) = i_numj(ix,iy)
              if (r_imj(ix,iy+1) .ne. 0.)         i_numj(ix+1,iy) = i_numj(ix+1,iy) - 1
              if (r_imj(ix+i_wsaxi,iy+1) .ne. 0.) i_numj(ix+1,iy) = i_numj(ix+1,iy) + 1
              R_SUMJJ(IX+1,IY)=R_SUMJJ(IX,IY)-R_IMJ(IX,IY+1)+R_IMJ(IX+I_WSAXI,IY+1)
              R_SMQJJ(IX+1,IY)=R_SMQJJ(IX,IY)-R_IMJ(IX,IY+1)**2+R_IMJ(IX+I_WSAXI,IY+1)**2
            END DO
          END DO
          DO IX=0,I_WSAXJ-I_WSAXI
            i_numj(ix,0)=0
            R_SUMJJ(IX,0)=0.
            R_SMQJJ(IX,0)=0.
            DO IY=0,I_WSAYI-1
              i_numj(ix,0)=i_numj(ix,0)+i_numj(ix+1,iy)
              R_SUMJJ(IX,0)=R_SUMJJ(IX,0)+R_SUMJJ(IX+1,IY)
              R_SMQJJ(IX,0)=R_SMQJJ(IX,0)+R_SMQJJ(IX+1,IY)
            END DO
            DO IY=0,I_WSAYJ-I_WSAYI-1
              i_numj(ix,iy+1)=i_numj(ix,iy)-i_numj(ix+1,iy)+i_numj(ix+1,iy+i_wsayi) 
              R_SUMJJ(IX,IY+1)=R_SUMJJ(IX,IY)-R_SUMJJ(IX+1,IY)+R_SUMJJ(IX+1,IY+I_WSAYI)
              R_SMQJJ(IX,IY+1)=R_SMQJJ(IX,IY)-R_SMQJJ(IX+1,IY)+R_SMQJJ(IX+1,IY+I_WSAYI)
            END DO
          END DO

          I_SHFTX=0
          I_SHFTY=0
          R_PEAK=-9.E27
          DO 300 ,M=0,I_WSAXJ-I_WSAXI
            DO 400,N=0,I_WSAYJ-I_WSAYI

              I_CNTI = I_CNTAI
              R_SUMI=R_SUMII
              R_SMQI=R_SMQII
              DO J=1,I_HOLEJ
                IF (I_HOLEJX(J)-M .GE. 1 .AND. I_HOLEJX(J)-M .LE. I_WSAXI .AND.
     &              I_HOLEJY(J)-N .GE. 1 .AND. I_HOLEJY(J)-N .LE. I_WSAYI ) THEN
                  IF (R_IMI(I_HOLEJX(J)-M,I_HOLEJY(J)-N) .NE. 0.) THEN
                    I_CNTI = I_CNTI -1
                    R_SUMI=R_SUMI-R_IMI(I_HOLEJX(J)-M,I_HOLEJY(J)-N)
                    R_SMQI=R_SMQI-R_IMI(I_HOLEJX(J)-M,I_HOLEJY(J)-N)**2
                  END IF
                END IF
              END DO

              I_CNTJ = I_NUMJ(M,N)
              R_SUMJ=R_SUMJJ(M,N)
              R_SMQJ=R_SMQJJ(M,N)
              DO I=1,I_HOLEI
                IF (R_IMJ(I_HOLEIX(I)+M,I_HOLEIY(I)+N) .NE. 0.) THEN
                  I_CNTJ = I_CNTJ -1
                  R_SUMJ=R_SUMJ-R_IMJ(I_HOLEIX(I)+M,I_HOLEIY(I)+N)
                  R_SMQJ=R_SMQJ-R_IMJ(I_HOLEIX(I)+M,I_HOLEIY(I)+N)**2
                END IF
              END DO

              IF (I_CNTI .EQ. I_CNTJ) THEN
                I_CNT=I_CNTI
              ELSE 
                WRITE(6,*) 'ERROR IN I_CNTI',I_CNTI,I_CNTJ,I_CNTAI,I_NUMJ(M,N)
                WRITE(6,*) '  HOLES = ',I_HOLEI,I_HOLEJ
                i_cnt=0.
                R_SUMC=0.
                R_SUMI=0.
                R_SMQI=0.
                R_SUMJ=0.
                R_SMQJ=0.
                i_cnti=0
                i_cntj=0
                DO J=1,I_WSAYI
                  DO I=1,I_WSAXI
                    if (r_imi(i,j) .ne. 0.)     i_cnti=i_cnti+1
                    if (r_imj(i+m,j+n) .ne. 0.) i_cntj=i_cntj+1
                    if (R_IMI(I,J) .ne. 0. .and. R_IMJ(I+M,J+N) .ne. 0.) then
                      i_cnt = i_cnt + 1
                      R_SUMI=R_SUMI+R_IMI(I,J)
                      R_SMQI=R_SMQI+R_IMI(I,J)**2
                      R_SUMJ=R_SUMJ+R_IMJ(I+M,J+N)
                      R_SMQJ=R_SMQJ+R_IMJ(I+M,J+N)**2
                      R_SUMC=R_SUMC+R_IMI(I,J)*R_IMJ(I+M,J+N)
                    end if
                  END DO
                END DO
                Write(6,*) 'i_cnti,j=',i_cnti,i_cntj
                WRITE(6,*) ' I_CNT = ',I_CNT,M,N
                I_CNTI=I_CNT
              END IF

              R_SUMC=0.
              DO J=1,I_WSAYI
                DO I=1,I_WSAXI
                  R_SUMC=R_SUMC+R_IMI(I,J)*R_IMJ(I+M,J+N)
                END DO
              END DO

              IF (I_CNT .GT. 0) THEN
                R_CRPD(M,N)=R_SUMC-((R_SUMI*R_SUMJ)/I_CNT)
                R_MEANIX=R_SUMI/I_CNT
                R_MEANJX=R_SUMJ/I_CNT
                R_VARNIX=R_SMQI/I_CNT - R_MEANIX**2
                R_VARNJX=R_SMQJ/I_CNT - R_MEANJX**2
                R_DENOM=I_CNT*SQRT(R_VARNIX*R_VARNJX)
                R_CORN(M,N)=ABS(R_CRPD(M,N))/R_DENOM !@#$%  added abs for opposite side matching
                R_IMGC(M+1,N+1)= R_CORN(M,N)
                IF ( R_PEAK .LT. R_CORN(M,N)) THEN
                  R_PEAK=R_CORN(M,N)
                  I_SHFTX=M
                  I_SHFTY=N
                  r_meani=R_MEANIX
                  r_meanj=R_MEANJX
                  r_varni=R_VARNIX
                  r_varnj=R_VARNJX
                END IF
              END IF
400         CONTINUE
300       CONTINUE
          r_stdvi = sqrt(r_varni)
          r_stdvj = sqrt(r_varnj)

          IF ( R_PEAK .GT. 0. ) THEN
            r_stdvi = sqrt(r_varni)
            r_stdvj = sqrt(r_varnj)
            IX=I_SHFTX
            IY=I_SHFTY
            IF ( IY .EQ. 0 .OR. IY .EQ. I_WSAYJ-I_WSAYI ) I_EDGE(1)=1
            IF ( IX .EQ. 0 .OR. IX .EQ. I_WSAXJ-I_WSAXI ) I_EDGE(2)=1
            R_SHFTX=IX*I_AVGX
            R_SHFTY=IY*I_AVGY
C            type *,'i_wsaxyi = ',i_wsaxyi
C            type *,'std = ',r_stdvi,r_stdvj
            R_SHRP=(R_PEAK-(R_CORN(MAX(IX-1,1),IY)+
     &          R_CORN(MIN(IX+1,I_WSAXJ-I_WSAXI),IY))/2.)
            I_FLAG=0
            IF ( IX .EQ. 0 ) THEN
              IF ( IY .EQ. 0 ) THEN
                R_DXX=-(R_CORN(IX+1,IY)+R_CORN(IX+1,IY)-2*R_CORN(IX,IY))/(I_AVGX**2)
                R_DYY=-(R_CORN(IX,IY+1)+R_CORN(IX,IY+1)-2*R_CORN(IX,IY))/(I_AVGY**2)
                R_DXY=0
                R_DXX=R_DXX/4 	! ADDED EMPERICALLY
                R_DYY=R_DYY/4
                R_DXY=R_DXY/4
                R_PEAK=R_PEAK/4
              ELSE IF ( IY .EQ. I_WSAYJ-I_WSAYI ) THEN
                R_DXX=-(R_CORN(IX+1,IY)+R_CORN(IX+1,IY)-2*R_CORN(IX,IY))/(I_AVGX**2)
                R_DYY=-(R_CORN(IX,IY-1)+R_CORN(IX,IY-1)-2*R_CORN(IX,IY))/(I_AVGY**2)
                R_DXY=0
                R_DXX=R_DXX/4 	! ADDED EMPERICALLY
                R_DYY=R_DYY/4
                R_DXY=R_DXY/4
                R_PEAK=R_PEAK/4
              ELSE
                R_DXX=-(R_CORN(IX+1,IY)+R_CORN(IX+1,IY)-2*R_CORN(IX,IY))/(I_AVGX**2)
                R_DYY=-(R_CORN(IX,IY+1)+R_CORN(IX,IY-1)-2*R_CORN(IX,IY))/(I_AVGY**2)
                R_DXY=2*(R_CORN(IX+1,IY+1)-R_CORN(IX+1,IY-1))/(4*I_AVGX*I_AVGY)
                R_DXX=R_DXX/2 	! ADDED EMPERICALLY
                R_DYY=R_DYY/2
                R_DXY=R_DXY/2
                R_PEAK=R_PEAK/2
              END IF
            ELSE IF ( IX .EQ. I_WSAXJ-I_WSAXI ) THEN
              IF ( IY .EQ. 0 ) THEN
                R_DXX=-(R_CORN(IX-1,IY)+R_CORN(IX-1,IY)-2*R_CORN(IX,IY))/(I_AVGX**2)
                R_DYY=-(R_CORN(IX,IY+1)+R_CORN(IX,IY+1)-2*R_CORN(IX,IY))/(I_AVGY**2)
                R_DXY=0
                R_DXX=R_DXX/4 	! ADDED EMPERICALLY
                R_DYY=R_DYY/4
                R_DXY=R_DXY/4
                R_PEAK=R_PEAK/4
              ELSE IF ( IY .EQ. I_WSAYJ-I_WSAYI ) THEN
                R_DXX=-(R_CORN(IX-1,IY)+R_CORN(IX-1,IY)-2*R_CORN(IX,IY))/(I_AVGX**2)
                R_DYY=-(R_CORN(IX,IY-1)+R_CORN(IX,IY-1)-2*R_CORN(IX,IY))/(I_AVGY**2)
                R_DXY=0
                R_DXX=R_DXX/4 	! ADDED EMPERICALLY
                R_DYY=R_DYY/4
                R_DXY=R_DXY/4
                R_PEAK=R_PEAK/4
              ELSE
                R_DXX=-(R_CORN(IX-1,IY)+R_CORN(IX-1,IY)-2*R_CORN(IX,IY))/(I_AVGX**2)
                R_DYY=-(R_CORN(IX,IY+1)+R_CORN(IX,IY-1)-2*R_CORN(IX,IY))/(I_AVGY**2)
                R_DXY=2*(R_CORN(IX-1,IY-1)-R_CORN(IX-1,IY+1))/(4*I_AVGX*I_AVGY)
                R_DXX=R_DXX/2 	! ADDED EMPERICALLY
                R_DYY=R_DYY/2
                R_DXY=R_DXY/2
                R_PEAK=R_PEAK/2
              END IF
            ELSE IF ( IY .EQ. 0 ) THEN
              R_DXX=-(R_CORN(IX+1,IY)+R_CORN(IX-1,IY)-2*R_CORN(IX,IY))/(I_AVGX**2)
              R_DYY=-(R_CORN(IX,IY+1)+R_CORN(IX,IY+1)-2*R_CORN(IX,IY))/(I_AVGY**2)
              R_DXY=2*(R_CORN(IX+1,IY+1)-R_CORN(IX-1,IY+1))/(4*I_AVGX*I_AVGY)
              R_DXX=R_DXX/2 	! ADDED EMPERICALLY
              R_DYY=R_DYY/2
              R_DXY=R_DXY/2
              R_PEAK=R_PEAK/2
            ELSE IF ( IY .EQ. I_WSAYJ-I_WSAYI ) THEN
              R_DXX=-(R_CORN(IX+1,IY)+R_CORN(IX-1,IY)-2*R_CORN(IX,IY))/(I_AVGX**2)
              R_DYY=-(R_CORN(IX,IY-1)+R_CORN(IX,IY-1)-2*R_CORN(IX,IY))/(I_AVGY**2)
              R_DXY=2*(R_CORN(IX-1,IY-1)-R_CORN(IX+1,IY-1))/(4*I_AVGX*I_AVGY)
              R_DXX=R_DXX/2 	! ADDED EMPERICALLY
              R_DYY=R_DYY/2
              R_DXY=R_DXY/2
              R_PEAK=R_PEAK/2
            ELSE
              R_DXX=-(R_CORN(IX+1,IY)+R_CORN(IX-1,IY)-2*R_CORN(IX,IY))/(I_AVGX**2)
              R_DYY=-(R_CORN(IX,IY+1)+R_CORN(IX,IY-1)-2*R_CORN(IX,IY))/(I_AVGY**2)
              R_DXY=(R_CORN(IX+1,IY+1)+R_CORN(IX-1,IY-1)-R_CORN(IX+1,IY-1)-
     &              R_CORN(IX-1,IY+1))/(4*I_AVGX*I_AVGY)
            END IF
C            R_N2=((R_STDVI**2+R_STDVJ**2)/2)-(R_CORR(IX,IY)/I_WSAXYI) 
            R_N2=MAX(1.-R_PEAK,0.E0)
            R_noise = Sqrt(R_n2)
            R_DXX=R_DXX*I_WSAXYI
            R_DYY=R_DYY*I_WSAXYI
            R_DXY=R_DXY*I_WSAXYI
C            type *,'d = ',r_dxx,r_dyy,r_dxy
            R_N4=R_N2**2
            R_N2=R_N2*2
            R_N4=R_N4*.5*I_WSAXYI
C            type *,'NOISE  =',R_NOISE,r_n2,r_n4
            R_U=R_DXY**2-R_DXX*R_DYY
            R_U2=R_U**2 !                    *I_AVGX*I_AVGY/I_WSAXYI
            IF ( R_U .EQ. 0 ) THEN
              R_COV(1)=99.
              R_COV(2)=99.
              R_COV(3)=0.
              I_FLAG=1
            ELSE
              R_COV(1)=(-R_N2*R_U*R_DYY+R_N4*(R_DYY**2+R_DXY**2)) /R_U2
              R_COV(2)=(-R_N2*R_U*R_DXX+R_N4*(R_DXX**2+R_DXY**2)) /R_U2
              R_COV(3)=((R_N2*R_U      -R_N4*(R_DXX+R_DYY))*R_DXY)/R_U2
            END IF
C            IF ( R_COV(1) .LE. 0. .OR. R_COV(2) .LE. 0. ) THEN
C              type *,'***  r_cov = ',r_cov
C              type *,'d = ',r_dxx,r_dyy,r_dxy
C              type *,'NOISE  =',R_NOISE,r_n2,r_n4
C              type *,'u = ',r_u,r_u2
C             READ(5,*) I
C            END IF
            R_U=SQRT((R_COV(1)+R_COV(2))**2.-4.*(R_COV(1)*R_COV(2)-R_COV(3)**2))
            R_EVAL1=(R_COV(1)+R_COV(2)+R_U)/2.
            R_EVAL2=(R_COV(1)+R_COV(2)-R_U)/2.
            IF ( R_EVAL1 .LE. 0 .OR. R_EVAL2 .LE. 0 ) THEN
C              TYPE *,'EVAL_ERROR',R_EVAL1,R_EVAL2
            END IF
C            type *,'eval = ',SQRT(ABS(r_eval1)),SQRT(ABS(r_eval2))

            
            IF ( R_COV(3) .EQ. 0 ) THEN
              IF ( R_COV(1) .GE. R_COV(2) ) THEN
                R_EVEC1(1)=1
                R_EVEC1(2)=0
                R_EVEC2(1)=0
                R_EVEC2(2)=1
              ELSE
                R_EVEC1(1)=0
                R_EVEC1(2)=1
                R_EVEC2(1)=1
                R_EVEC2(2)=0
              END IF
            ELSE
              IF ( R_COV(1)-R_EVAL1 .NE. 0. ) THEN
                R_EVEC1(1)=-R_COV(3)/(R_COV(1)-R_EVAL1)
              ELSE
                TYPE *,'E VECTOR 1 ERROR'
                R_EVEC1(1)=999.
              END IF
              R_EVEC1(2)=1.
              R_U=SQRT(R_EVEC1(1)**2+R_EVEC1(2)**2)
              R_EVEC1(1)=R_EVEC1(1)/R_U
              R_EVEC1(2)=R_EVEC1(2)/R_U

              IF ( R_COV(1)-R_EVAL2 .NE. 0. ) THEN
                 R_EVEC2(1)=-R_COV(3)/(R_COV(1)-R_EVAL2)
              ELSE
                TYPE *,'E VECTOR 2 ERROR'
                R_EVEC2(1)=999.
              END IF
              R_EVEC2(2)=1.
              R_U=SQRT(R_EVEC2(1)**2+R_EVEC2(2)**2)
              R_EVEC2(1)=R_EVEC2(1)/R_U
              R_EVEC2(2)=R_EVEC2(2)/R_U
            END IF

            R_EVEC1(1)=R_EVEC1(1)*SQRT(ABS(R_EVAL1)) 
            R_EVEC1(2)=R_EVEC1(2)*SQRT(ABS(R_EVAL1)) 
            R_EVEC2(1)=R_EVEC2(1)*SQRT(ABS(R_EVAL2)) 
            R_EVEC2(2)=R_EVEC2(2)*SQRT(ABS(R_EVAL2)) 
          ELSE
            R_SHFTY=0
            R_SHFTX=0
            R_SHRP=0.
            R_EVAL1 = 0
            R_EVAL2 = 0
            R_EVEC1(1) = 0.
            R_EVEC1(2) = 0. 
            R_EVEC2(1) = 0.
            R_EVEC2(2) = 0.
            I_FLAG=2
c            TYPE *,'CORRELATION ERROR'
          END IF
        ELSE
          R_SHFTY=0
          R_SHFTX=0
          R_SHRP=0.
          R_EVAL1 = 0
          R_EVAL2 = 0
          R_EVEC1(1) = 0.
          R_EVEC1(2) = 0. 
          R_EVEC2(1) = 0.
          R_EVEC2(2) = 0.
          R_PEAK = 0.
          DO  M=0,I_WSAXJ-I_WSAXI
            DO N=0,I_WSAYJ-I_WSAYI
              R_IMGC(M+1,N+1)= 0
            END DO
          END DO
          I_FLAG=1
        END IF

        RETURN

      END

      subroutine read_mhdr(a_magfile,i_mlsize,i_mssize,i_mbytes,i_moff)

        implicit none

        character*(*) a_magfile

        integer i
        integer i_unit
        integer i_mlsize
        integer i_mssize
        integer i_mbsize
        integer i_mbytes

        integer i_moff
        integer i_boff

        character*50 a_string(100)
        byte b_string(5000)

        equivalence(a_string,b_string)

          i_mlsize = 0
          i_mssize = 0
c          i_mbytes = 0

          open(unit=18,file=a_magfile,status='old',form='unformatted',
     &        access='direct',recl=50,readonly)

          do i=1,20
            read(18,rec=i,err=901) a_string(i)
            if (a_string(i) .eq. ' ') then
              ! do nothing
            else if (index(a_string(i),'RECORD LENGTH IN BYTES =') .gt. 0) then
              read(a_string(i)(35:),*) i_mbsize
              write(6,*) ' '
              write(6,*) 'Reading airsar magnitude header     ',i_mbsize
            else if (index(a_string(i),'NUMBER OF HEADER RECORDS =') .gt. 0) then
              read(a_string(i)(35:),*) i_moff
            else if (index(a_string(i),'NUMBER OF SAMPLES PER RECORD =') .gt. 0) then
              read(a_string(i)(35:),*) i_mssize
            else if (index(a_string(i),'NUMBER OF LINES IN IMAGE =') .gt. 0) then
              read(a_string(i)(35:),*) i_mlsize
            else if (index(a_string(i),'NUMBER OF BYTES PER SAMPLE =') .gt. 0) then
              read(a_string(i)(35:),*) i_mbytes
            else if (index(a_string(i),'BYTE OFFSET OF FIRST DATA RECORD =') .gt. 0) then
              read(a_string(i)(35:),*) i_boff
            end if
          end do
901       close(18)
              
        return

      end

      subroutine read_dhdr(a_demfile,i_dlsize,i_dssize,i_dbytes,i_doff,r_dmul,r_dadd,
     &              r_peg,r_str,r_spc)

        implicit none

        character*(*) a_demfile

        integer i
        integer j
        integer i_err
        integer i_unit
        integer i_dlsize
        integer i_dssize
        integer i_dbsize
        integer i_dbytes

        integer i_demoff

        integer i_doff
        integer i_boff

        real r_dmul
        real r_dadd 

        real*8 r_peg(3)
        real*8 r_str(2)
        real*8 r_spc(2)

        character*50 a_string(100)
        byte b_string(5000)

        equivalence(a_string,b_string)

        real*8 r_pi
        real*8 r_rtod

c  Initialize pi and conversions
c
        r_pi = 4.d0*atan(1.0d0)
        r_rtod = 180.0d0/r_pi

          i_dbsize = 0
          i_dlsize = 0
          i_dssize = 0
c          i_dbytes = 0
c          i_demoff = 0
c          i_doff = 0

          open(unit=18,file=a_demfile,status='old',form='unformatted',
     &        access='direct',recl=50,readonly)

          do i=1,20
            read(18,rec=i,err=901) a_string(i)
            if (a_string(i) .eq. ' ') then
              ! do nothing
            else if (index(a_string(i),'RECORD LENGTH IN BYTES =') .gt. 0) then
              read(a_string(i)(35:),*) i_dbsize
              write(6,*) ' '
              write(6,*) 'Reading airsar elevation header     ',i_dbsize
            else if (index(a_string(i),'NUMBER OF HEADER RECORDS =') .gt. 0) then
              read(a_string(i)(35:),*) i_doff
c              type *,'i_doff=',i_doff
            else if (index(a_string(i),'NUMBER OF SAMPLES PER RECORD =') .gt. 0) then
              read(a_string(i)(35:),*) i_dssize
c              type *,'i_dssize=',i_dssize
            else if (index(a_string(i),'NUMBER OF LINES IN IMAGE =') .gt. 0) then
              read(a_string(i)(35:),*) i_dlsize
c              type *,'i_dlsize=',i_dlsize
            else if (index(a_string(i),'NUMBER OF BYTES PER SAMPLE =') .gt. 0) then
              read(a_string(i)(35:),*) i_dbytes
c              type *,'i_dbytes=',i_dbytes
            else if (index(a_string(i),'BYTE OFFSET OF FIRST DATA RECORD =') .gt. 0) then
              read(a_string(i)(35:),*) i_boff
c              type *,'i_boff=',i_boff
            else if (index(a_string(i),'BYTE OFFSET OF DEM HEADER =') .gt. 0) then
              read(a_string(i)(35:),*) i_demoff
c              type *,'i_demoff=',i_demoff
            end if
          end do
901       close(18)
              

          if (i_dbsize .gt. 0) then

            open(unit=18,file=a_demfile,status='old',form='unformatted',
     &          access='direct',recl=i_dbsize,readonly)

            do i=1,(1100-1)/i_dbsize + 1
              read(18,rec=i+(i_demoff/i_dbsize),err=902) (b_string(j),j=1+(i-1)*i_dbsize,min(i*i_dbsize,5000))
            end do

            do i=1,21
c              type *,i,a_string(i)
              if (a_string(i) .eq. ' ') then
                ! do nothing
              else if (index(a_string(i),'X-DIRECTION POST SPACING (M) =') .gt. 0) then
                read(a_string(i)(35:),*,err=990) r_spc(1)
c                type *,'x spacing=',r_spc(1)
              else if (index(a_string(i),'Y-DIRECTION POST SPACING (M) =') .gt. 0) then
                read(a_string(i)(35:),*,err=990) r_spc(2)
c                type *,'y spacing=',r_spc(2)
              else if (index(a_string(i),'ELEVATION INCREMENT (M) =') .gt. 0) then
                read(a_string(i)(35:),*,iostat=i_err) r_dmul
              else if (index(a_string(i),'ELEVATION OFFSET (M) =') .gt. 0) then
                read(a_string(i)(35:),*,iostat=i_err) r_dadd
              else if (index(a_string(i),'LATITUDE OF PEG POINT =') .gt. 0) then
                read(a_string(i)(35:),*,err=990) r_peg(1)
c                type *,'lat=',r_peg(1)
                r_peg(1) = r_peg(1) / r_rtod
              else if (index(a_string(i),'LONGITUDE OF PEG POINT =') .gt. 0) then
                read(a_string(i)(35:),*,err=990) r_peg(2)
c                type *,'lon=',r_peg(2)
                r_peg(2) = r_peg(2) / r_rtod
              else if (index(a_string(i),'HEADING AT PEG POINT (DEGREES) =') .gt. 0) then
                read(a_string(i)(35:),*,err=990) r_peg(3)
c                type *,'hdg=',r_peg(3)
                r_peg(3) = r_peg(3) / r_rtod
              else if (index(a_string(i),'ALONG-TRACK OFFSET S0 (M) =') .gt. 0) then
                read(a_string(i)(35:),*,err=990) r_str(1)
c                type *,'s0 =',r_str(1)
              else if (index(a_string(i),'CROSS-TRACK OFFSET C0 (M) =') .gt. 0) then
                read(a_string(i)(35:),*,err=990) r_str(2)
c                type *,'c0 =',r_str(2)
              end if
              if (1.eq.2) then
990             write(6,*) 'Error - ',i,' ',a_string(i)
                write(6,*) ' '
              end if
            end do
            
              
          end if
902       close(18) 
       return

      end
