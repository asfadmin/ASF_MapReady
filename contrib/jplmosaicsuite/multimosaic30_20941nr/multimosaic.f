c ******************************************************************************
        program multimosaic
c
c       FILE NAME: multimosaic.f
c   
c       DATE WRITTEN: 12/19/97
c  
c       PROGRAMMER: Scott Shaffer
c  
c       FUNCTIONAL DESCRIPTION: Mosaics image files together 
c       using an affine matris transformation.
c
c       The basic algorithm is as follows:
c          Loop over output blocks (Left to right, top to bottom)
c            Clear input and output Buffers
c            Loop over input files
c              Clear input buffer
c              Loop over output corner pixels in block
c              Determine corresponding lines in input file
c              Read in corresponding line of input file into input buffer
c              Loop over pixels in input buffer
c                Find data edge pixels, translate to output coords. and save
c                Compute feathering factor K for each output pixel
c              Loop over pixels in input buffer in groups of 3
c                Compute corresponding pixel locations and heighst in output buffer
c                Interpolate to all output pixels within resulting triangle
c                Multiply pixel by k factor and add to data output buffer
c                Add  K factor to output scale buffer
c              End loop over input pixels
c            End Loop over output files
c            Loop over output pixels in data buffer
c              Divide each data buffer by cooresponding scale pix
c            End Loop over output pixels
c            Write out data block to disk
c          End Loop over blocks
c                
c  
c       ROUTINES CALLED: 
c 
c       NOTES: none
c     
c       UPDATE LOG:
c
c       
c ******************************************************************************

        implicit none

        integer I_MIF
        integer I_MXS
        integer I_MFL
        integer I_MGP
        integer I_IBS
        integer I_IBL
        integer I_OBS
        integer I_OBL

c       PARAMETER STATEMENTS:
        parameter (I_MIF=200)           ! Max number of input files
        parameter (I_MXS=40960)         ! Max samples in i/o files
        parameter (I_MGP=200000)        ! Max number of gaps per block
        parameter (I_MFL=50)            ! Max feather width in pix
        parameter (I_IBS=5000)          ! Buffer size in X
        parameter (I_IBL=5000)          ! Buffer size in Y
        parameter (I_OBS=2000)          ! Buffer size in X
        parameter (I_OBL=2000)          ! Buffer size in Y

c       VARIABLES:
        character*1   a_grid              ! UTM identifier
        character*255 a_cmdfile           ! Command file name
        character*255 a_rmgfile(0:I_MIF)  ! Data file names
        character*255 a_magfile(0:I_MIF)  ! Data file names
        character*255 a_dtefile(0:I_MIF)  ! Data file names
        character*255 a_afffile(0:I_MIF)  ! Affine transformation files
        character*255 a_hdrfile(0:I_MIF)  ! Header file
        character*255 a_errfile(0:I_MIF)  ! Error in/output file
        character*255 a_stdfile(0:I_MIF)  ! Standard deviation output file
        character*255 a_gcpfile(0:I_MIF)  ! Ground Control Point file
        character*255 a_type(0:I_MIF)     ! File type
        character*255 a_vals(10)
        character*255 a_input

        integer*4 i_vals
        integer*4 i_ssize(0:I_MIF)      ! Number of samples in input files
        integer*4 i_lsize(0:I_MIF)      ! Number of lines in input files
        integer*4 i_hdrstat(0:I_MIF)	! Header read flag

        integer*4 i_fln                 ! Feather length around gaps
        integer*4 i_nof                 ! Number of files to mosaic
        integer*4 i_gpc                 ! Number of gap pixels found in Block
        integer*4 i_nog                 ! Number of GCP files of overlay mosaic

        integer*4 i_oloc1(2,I_MXS)

        integer*4 i
        integer*4 j
        integer*4 ii
        integer*4 jj
        integer*4 is
        integer*4 il
        integer*4 iz
        integer*4 iss
        integer*4 isss
        integer*4 ill
        integer*4 i_bs
        integer*4 i_bl
        integer*4 i_nbs                ! Number of tiles in X direction
        integer*4 i_nbl                ! Number of tiles in Y direction
        integer*4 i_osoff
        integer*4 i_oloff
        integer*4 i_osnum
        integer*4 i_olnum
        integer*4 i_isoff
        integer*4 i_iloff
        integer*4 i_isnum
        integer*4 i_ilnum
        integer*4 i_file
        integer*4 i_ffff
        integer*4 i_sum
        integer*4 i_cnt
        integer*4 i_flg
        integer*4 i_smin
        integer*4 i_smax
        integer*4 i_lmin
        integer*4 i_lmax
        integer*4 i_err
        integer*4 i_pos
        integer*4 i_smp
        integer*4 i_cmb

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

        integer*4 i_moff(2,0:I_MIF)
        integer*4 i_doff(2,0:I_MIF)

        integer*4 i_mlsize
        integer*4 i_mssize
        integer*4 i_dlsize
        integer*4 i_dssize

        integer*4 i_isfac
        integer*4 i_ilfac
        integer*4 i_ifacmin(0:I_MIF)

        integer*4 i_fillsiz
        integer*4 i_fillval
        integer*4 i_fills(0:I_MIF)
        integer*4 i_fillv(0:I_MIF)

        integer*4 i_set(0:11,0:I_MIF)

        integer*4 i_comma
        integer*4 i_override

        byte b_data1(I_MXS)
        byte b_datb1(I_MXS)

        integer*2 i_data2(I_MXS)
        integer*2 i_datb2(I_MXS)

        real*4 r_data(I_MXS)
        real*4 r_datb(I_MXS)
        real*4 r_ibufa(I_IBS,I_IBL)  
        real*4 r_ibufb(I_IBS,I_IBL)  
        real*4 r_obufa(I_OBS,I_OBL)  
        real*4 r_obufb(I_OBS,I_OBL)  
        real*4 r_obufc(I_OBS,I_OBL)  
        real*4 r_sbuff(I_OBS,I_OBL)  
        real*4 r_scale(I_OBS,I_OBL)

        real*4 r_dmul
        real*4 r_dadd

        real*8 r_iat(3,4)
        real*8 r_inv(3,4)
        real*8 r_aff(3,4)
        real*8 r_tmp(3,4)

        real*8 r_atm(3,4,0:I_MIF)
        real*8 r_mta(3,4,0:I_MIF)
        real*8 r_dist(-I_MFL:I_MFL,-I_MFL:I_MFL)

        real*8  r_min
        real*8  r_max
        real*8  r_nul

        real*8 r_a
        real*8 r_e2
        real*8 r_peg(3,0:I_MIF)
        real*8 r_rad(0:I_MIF)
        real*8 r_lat
        real*8 r_lon
        real*8 r_hgt
        real*8 r_temp

        real*8 r_spc(2,0:I_MIF)
        real*8 r_str(2,0:I_MIF)

        real*8 r_aa
        real*8 r_bb
        real*8 r_sum 
        real*8 r_avg
        real*8 r_wgt(0:I_MIF)
        real*8 r_mdnc(2,0:I_MIF)
        real*8 r_ddnc(2,0:I_MIF)

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

        real*8 v_gpl(3,I_MGP)
        real*8 v_tmp(3)

        real*8 v_pnt(4)
        real*8 v_olocc(4,2,I_MXS)


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
        save r_obufc
        save r_sbuff
        save r_scale
        save v_oloc1
        save i_oloc1

        real*4 ttz, ttza(2), ttl, ttla(2), tt1, tt1a(2), tt2, tt2a(2)
        real etime

        write(6,*) ' '
        write(6,*) '     << Multimosaic   Version 30.0    Oct 22, 2000>>    '
        write(6,*) ' '

c
c  Code for argus to check the run time and cpu time for a job:
c     (stolen from ifproc)
c

      ttz = etime(ttza)  !csgi - log time for job beginning         !@#$% Machine Dependent Code
 

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
          enddo
        enddo 

c
c  Initialize Mosaic Dimensions
c
        i_fstsp= 999999999
        i_lstsp=-999999999
        i_fstln= 999999999
        i_lstln=-999999999

        r_min=   -500.         ! Minimum valid altitude
        r_max=   9000.         ! Maximum valid altitude  
        r_nul= -10000.      
c
c  Initialize input variables
c
        i_fln = 0
        i_fillsiz = 3
        i_fillval = 10000
        do i_file = 0,I_MIF
          i_ifacmin(i_file) = 1
          r_peg(1,i_file) = 0.
          r_peg(2,i_file) = 0.
          r_peg(3,i_file) = 0.
          r_str(1,i_file) = 0.
          r_str(2,i_file) = 0.
          r_spc(1,i_file) = 1.
          r_spc(2,i_file) = 1.
          i_set(0,i_file) = 0
          i_set(1,i_file) = 0
          i_set(2,i_file) = 0
          i_set(3,i_file) = 0
          i_set(4,i_file) = 0
          i_set(5,i_file) = 0
          i_set(6,i_file) = 0
          i_set(7,i_file) = 0
          i_set(8,i_file) = 0
          i_set(9,i_file) = 0
          i_hdrstat(i_file) = 1
          a_rmgfile(i_file) = ' '
          a_magfile(i_file) = ' '
          a_dtefile(i_file) = ' '
          a_afffile(i_file) = ' '
          a_hdrfile(i_file) = ' '
          a_stdfile(i_file) = ' '
          a_errfile(i_file) = ' '
          a_gcpfile(i_file) = ' '
          a_type(i_file) = 'sch'
          i_zone(i_file)=0
          i_ssize(i_file)=0
          i_lsize(i_file)=0
          i_mbytes(i_file)=4
          i_dbytes(i_file)=4
          r_wgt(i_file)=1.
          r_mdnc(1,i_file)=1.0
          r_mdnc(2,i_file)=0.0
          r_ddnc(1,i_file)=1.0
          r_ddnc(2,i_file)=0.0
          i_moff(1,i_file) = 0
          i_moff(2,i_file) = 0
          i_doff(1,i_file) = 0
          i_doff(2,i_file) = 0
        enddo
        i_smp = 0
        i_cmb = 0
        i_nog = 0

c
c  Get command file name
c
        i_inarg = iargc()
        if (i_inarg .ne. 1)then
           write(6,*) 'Usage 1 arguements : Mosaic cmd_file'
c           stop
           write(6,*) ' '
           write(6,'(x,a,$)') 'Enter command filename: '
           read(5,'(a)') a_cmdfile
        else
           call getarg(1,a_cmdfile)
        endif

c
c  Read Command file
c
        write(6,*) ' '
        write(6,*) 'Opening cmd input  file: ',a_cmdfile(1:52)
        open(unit=20,file=a_cmdfile,status='old',form='formatted')

        i_err = 0
        i_file = -1
        do while(i_err .eq. 0)
          read(20,'(a)',end=110) a_input
          if (a_input(1:1) .ne. '#' .and. a_input(1:1) .ne. '!' .and. a_input(1:1) .ne. '%') then
            call parse(a_input,i_vals,a_vals)     
            do i = 1,i_vals
              if (a_vals(i) .eq. ' ') then
                print *,'parse error'
              else if (a_vals(i)(1:4) .eq. 'smp=') then
                if (a_vals(i)(5:) .eq. 'simplicial' .or. a_vals(i)(5:) .eq. 'SIMPLICIAL' .or.
     &              a_vals(i)(5:) .eq. 'plane'      .or. a_vals(i)(5:) .eq. 'PLANE' .or.
     &              a_vals(i)(5:) .eq. '0') then
                  i_smp = 0
                else if (a_vals(i)(5:) .eq. 'nearest_neighbor' .or. a_vals(i)(5:) .eq. 'NEAREST_NEIGHBOR' .or.
     &              a_vals(i)(5:) .eq. 'nearest'          .or. a_vals(i)(5:) .eq. 'NEAREST' .or.
     &              a_vals(i)(5:) .eq. '1') then
                  i_smp = 1
                else
                  i_smp = 0
                endif
              else if (a_vals(i)(1:4) .eq. 'cmb=') then
                if (a_vals(i)(5:) .eq. 'avg' .or. a_vals(i)(5:) .eq. 'AVG' .or.
     &              a_vals(i)(5:) .eq. 'average' .or. a_vals(i)(5:) .eq. 'AVERAGE') then
                  i_cmb = 0
                else if (a_vals(i)(5:) .eq. 'max' .or. a_vals(i)(5:) .eq. 'MAX' .or.
     &              a_vals(i)(5:) .eq. 'maximum' .or. a_vals(i)(5:) .eq. 'MAXIMUM') then
                  i_cmb = 1
                else if (a_vals(i)(5:) .eq. 'split' .or. a_vals(i)(5:) .eq. 'SPLIT' .or.
     &              a_vals(i)(5:) .eq. '2') then
                  i_cmb = 2
                else
                  i_cmb = 0
                endif
              else if (a_vals(i)(1:4) .eq. 'gfs=') then
                read(a_vals(i)(5:),*) i_fillsiz
                if (i_nog .gt. 0) i_fills(i_nog) = i_fillsiz
              else if (a_vals(i)(1:4) .eq. 'gfv=') then
                read(a_vals(i)(5:),*) i_fillval
                if (i_nog .gt. 0) i_fillv(i_nog) = i_fillval
              else if (a_vals(i)(1:4) .eq. 'fln=') then
                read(a_vals(i)(5:),*) i_fln
              else if (a_vals(i)(1:4) .eq. 'max=') then
                read(a_vals(i)(5:),*) r_max
              else if (a_vals(i)(1:4) .eq. 'min=') then
                read(a_vals(i)(5:),*) r_min
              else if (a_vals(i)(1:4) .eq. 'nul=') then
                read(a_vals(i)(5:),*) r_nul
              else if (a_vals(i)(1:4) .eq. 'fac=') then
                read(a_vals(i)(5:),*) i_ifacmin(i_file)
                if (i_file .eq. 0) then
                  do j=1,I_MIF
                    i_ifacmin(i_file) = i_ifacmin(0)
                  enddo
                endif                
              else if (a_vals(i)(1:4) .eq. 'typ=' .or. a_vals(i)(1:4) .eq. 'prj=') then
                a_type(i_file)=a_vals(i)(5:)
                i_set(0,i_file) = 1
              else if (a_vals(i)(1:4) .eq. 'siz=') then
                read(a_vals(i)(5:),*) i_ssize(i_file),i_lsize(i_file)
                i_set(4,i_file) = 1
              else if (a_vals(i)(1:4) .eq. 'off=') then
                read(a_vals(i)(5:),*) r_str(1,i_file),r_str(2,i_file)
                i_set(2,i_file) = 1
              else if (a_vals(i)(1:4) .eq. 'spc=') then
                read(a_vals(i)(5:),*) r_spc(1,i_file),r_spc(2,i_file)
                i_set(3,i_file) = 1
              else if (a_vals(i)(1:4) .eq. 'mss=') then
                read(a_vals(i)(5:),*) r_mdnc(1,i_file),r_mdnc(2,i_file)
                i_set(8,i_file) = 1
              else if (a_vals(i)(1:4) .eq. 'dss=') then
                read(a_vals(i)(5:),*) r_ddnc(1,i_file),r_ddnc(2,i_file)
                i_set(9,i_file) = 1
              else if (a_vals(i)(1:4) .eq. 'mbp=') then
                read(a_vals(i)(5:),*) i_mbytes(i_file)
                i_set(6,i_file) = 1
              else if (a_vals(i)(1:4) .eq. 'dbp=') then
                read(a_vals(i)(5:),*) i_dbytes(i_file)
                i_set(7,i_file) = 1
              else if (a_vals(i)(1:4) .eq. 'mgo=' .or. a_vals(i)(1:4) .eq. 'mhl') then
                read(a_vals(i)(5:),*) i_moff(1,i_file)
              else if (a_vals(i)(1:4) .eq. 'mbo=' .or. a_vals(i)(1:4) .eq. 'mhs') then
                read(a_vals(i)(5:),*) i_moff(2,i_file)
              else if (a_vals(i)(1:4) .eq. 'dto=' .or. a_vals(i)(1:4) .eq. 'dhl') then
                read(a_vals(i)(5:),*) i_doff(1,i_file)
              else if (a_vals(i)(1:4) .eq. 'dbo=' .or. a_vals(i)(1:4) .eq. 'dhs') then
                read(a_vals(i)(5:),*) i_doff(2,i_file)
              else if (a_vals(i)(1:4) .eq. 'peg=') then
                read(a_vals(i)(5:),*) r_peg(1,i_file),r_peg(2,i_file),r_peg(3,i_file)
                r_peg(1,i_file) = r_peg(1,i_file)/r_rtod
                r_peg(2,i_file) = r_peg(2,i_file)/r_rtod
                r_peg(3,i_file) = r_peg(3,i_file)/r_rtod
                i_set(1,i_file) = 1
              else if (a_vals(i)(1:4) .eq. 'zon=') then
                read(a_vals(i)(5:),*) i_zone(i_file)
                i_set(5,i_file) = 1
              else if (a_vals(i)(1:4) .eq. 'wgt=') then
                read(a_vals(i)(5:),*) r_wgt(i_file)
              else if (a_vals(i)(1:4) .eq. 'hdr=') then
                a_hdrfile(i_file)=a_vals(i)(5:)
              else if (a_vals(i)(1:4) .eq. 'aff=') then
                a_afffile(i_file)=a_vals(i)(5:)
              else if (a_vals(i)(1:4) .eq. 'std=') then
                a_stdfile(i_file)=a_vals(i)(5:)
              else if (a_vals(i)(1:4) .eq. 'err=') then
                a_errfile(i_file)=a_vals(i)(5:)
              else if (a_vals(i)(1:4) .eq. 'gcp=') then
                i_nog = i_nog + 1
                a_gcpfile(i_nog)=a_vals(i)(5:)
                i_fills(i_nog) = i_fillsiz
                i_fillv(i_nog) = i_fillval
              else if (a_vals(i)(1:4) .eq. 'amp=' .or. 
     &                 a_vals(i)(1:4) .eq. 'mag=' .or.
     &                 a_vals(i)(1:4) .eq. 'pwr=' ) then
                i_file=i_file+1
                if (i_file .gt. I_MIF) stop '***** Error - Too many input files'
                a_magfile(i_file)=a_vals(i)(5:)
                i_pos = 0
                do while (index(a_magfile(i_file)(i_pos+1:),'.') .ne. 0)
                  i_pos = i_pos + index(a_magfile(i_file)(i_pos+1:),'.')
                enddo
                if (i_pos .gt. 0) then
                  a_hdrfile(i_file)=a_magfile(i_file)(1:i_pos)//'hdr'
                else
                  a_hdrfile(i_file)=a_magfile(i_file)(:length(a_magfile(i_file)))//'.hdr'
                endif
              else if (a_vals(i)(1:4) .eq. 'm*2=' ) then
                i_file=i_file+1
                if (i_file .gt. I_MIF) stop '***** Error - Too many input files'
                a_magfile(i_file)=a_vals(i)(5:)
                i_pos = 0
                do while (index(a_magfile(i_file)(i_pos+1:),'.') .ne. 0)
                  i_pos = i_pos + index(a_magfile(i_file)(i_pos+1:),'.')
                enddo
                if (i_pos .gt. 0) then
                  a_hdrfile(i_file)=a_magfile(i_file)(1:i_pos)//'hdr'
                else
                  a_hdrfile(i_file)=a_magfile(i_file)(:length(a_magfile(i_file)))//'.hdr'
                endif
                i_mbytes(i_file)=2
                i_set(6,i_file) = 1
              else if (a_vals(i)(1:4) .eq. 'm*1=' ) then
                i_file=i_file+1
                if (i_file .gt. I_MIF) stop '***** Error - Too many input files'
                a_magfile(i_file)=a_vals(i)(5:)
                i_pos = 0
                do while (index(a_magfile(i_file)(i_pos+1:),'.') .ne. 0)
                  i_pos = i_pos + index(a_magfile(i_file)(i_pos+1:),'.')
                enddo
                if (i_pos .gt. 0) then
                  a_hdrfile(i_file)=a_magfile(i_file)(1:i_pos)//'hdr'
                else
                  a_hdrfile(i_file)=a_magfile(i_file)(:length(a_magfile(i_file)))//'.hdr'
                endif
                i_mbytes(i_file)=1
                i_set(6,i_file) = 1
              else if (a_vals(i)(1:4) .eq. 'dte=' .or. 
     &                 a_vals(i)(1:4) .eq. 'dem=' .or.
     &                 a_vals(i)(1:4) .eq. 'hgt=' ) then
                i_file=i_file+1
                if (i_file .gt. I_MIF) stop '***** Error - Too many input files'
                a_dtefile(i_file)=a_vals(i)(5:)
                i_pos = 0
                do while (index(a_dtefile(i_file)(i_pos+1:),'.') .ne. 0)
                  i_pos = i_pos + index(a_dtefile(i_file)(i_pos+1:),'.')
                enddo
                if (i_pos .gt. 0) then
                  a_hdrfile(i_file)=a_dtefile(i_file)(1:i_pos)//'hdr'
                else
                  a_hdrfile(i_file)=a_dtefile(i_file)(:length(a_dtefile(i_file)))//'.hdr'
                endif
              else if (a_vals(i)(1:4) .eq. 'dma=' .or. 
     &                 a_vals(i)(1:4) .eq. 'd*2=' ) then
                i_file=i_file+1
                if (i_file .gt. I_MIF) stop '***** Error - Too many input files'
                a_dtefile(i_file)=a_vals(i)(5:)
                i_pos = 0
                do while (index(a_dtefile(i_file)(i_pos+1:),'.') .ne. 0)
                  i_pos = i_pos + index(a_dtefile(i_file)(i_pos+1:),'.')
                enddo
                if (i_pos .gt. 0) then
                  a_hdrfile(i_file)=a_dtefile(i_file)(1:i_pos)//'hdr'
                else
                  a_hdrfile(i_file)=a_dtefile(i_file)(:length(a_dtefile(i_file)))//'.hdr'
                endif
                i_dbytes(i_file)=2
                a_type(i_file)='eqa'
                i_set(7,i_file) = 1
                i_set(0,i_file) = 1
              else if (a_vals(i)(1:4) .eq. 'd*1=' ) then
                i_file=i_file+1
                if (i_file .gt. I_MIF) stop '***** Error - Too many input files'
                a_dtefile(i_file)=a_vals(i)(5:)
                i_pos = 0
                do while (index(a_dtefile(i_file)(i_pos+1:),'.') .ne. 0)
                  i_pos = i_pos + index(a_dtefile(i_file)(i_pos+1:),'.')
                enddo
                if (i_pos .gt. 0) then
                  a_hdrfile(i_file)=a_dtefile(i_file)(1:i_pos)//'hdr'
                else
                  a_hdrfile(i_file)=a_dtefile(i_file)(:length(a_dtefile(i_file)))//'.hdr'
                endif
                i_dbytes(i_file)=1
                a_type(i_file)='eqa'
                i_set(7,i_file) = 1
                i_set(0,i_file) = 1
              else if (a_vals(i)(1:4) .eq. 'mgh=') then
                i_file=i_file+1
                if (i_file .gt. I_MIF) stop '***** Error - Too many input files'
                i_comma=index(a_vals(i),',')
                a_magfile(i_file)=a_vals(i)(5:i_comma-1)
                a_dtefile(i_file)=a_vals(i)(i_comma+1:)
                i_pos = 0
                do while (index(a_dtefile(i_file)(i_pos+1:),'.') .ne. 0)
                  i_pos = i_pos + index(a_dtefile(i_file)(i_pos+1:),'.')
                enddo
                if (i_pos .gt. 0) then
                  a_hdrfile(i_file)=a_dtefile(i_file)(1:i_pos)//'hdr'
                else
                  a_hdrfile(i_file)=a_dtefile(i_file)(:length(a_dtefile(i_file)))//'.hdr'
                endif
              else if (a_vals(i)(1:4) .eq. 'rmg=') then
                i_file=i_file+1
                if (i_file .gt. I_MIF) stop '***** Error - Too many input files'
                a_rmgfile(i_file)=a_vals(i)(5:)
                i_pos = 0
                do while (index(a_rmgfile(i_file)(i_pos+1:),'.') .ne. 0)
                  i_pos = i_pos + index(a_rmgfile(i_file)(i_pos+1:),'.')
                enddo
                if (i_pos .gt. 0) then
                  a_hdrfile(i_file)=a_rmgfile(i_file)(1:i_pos)//'hdr'
                else
                  a_hdrfile(i_file)=a_rmgfile(i_file)(:length(a_rmgfile(i_file)))//'.hdr'
                endif
              else
                i_file=i_file+1
                if (i_file .gt. I_MIF) stop '***** Error - Too many input files'
                a_rmgfile(i_file)=a_vals(i)
                i_pos = 0
                do while (index(a_rmgfile(i_file)(i_pos+1:),'.') .ne. 0)
                  i_pos = i_pos + index(a_rmgfile(i_file)(i_pos+1:),'.')
                enddo
                if (i_pos .gt. 0) then
                  a_hdrfile(i_file)=a_rmgfile(i_file)(1:i_pos)//'hdr'
                else
                  a_hdrfile(i_file)=a_rmgfile(i_file)(:length(a_rmgfile(i_file)))//'.hdr'
                endif
              endif
            enddo
          endif
        enddo

110     close(20)
        i_nof=i_file

        write(6,*) ' '
        if (i_nof .lt.  0) stop '***** Error - No files specified'
        if (i_nof .eq.  0) stop '***** Error - No input files specified'

        if (i_fln .gt. I_MFL) stop 'Error - Feather length exceeds max'

c
c  Read Header Files
c
c        write(6,*) ' set0 = ',(i_set(i,0),i=0,9)
        i_override = i_set(1,0) + i_set(2,0) + i_set(3,0) + i_set(4,0) + i_set(5,0) + i_set(6,0) + i_set(7,0) +
     &            i_set(8,0) + i_set(9,0)
        do i_file = 0,i_nof
          if (a_hdrfile(i_file) .ne. ' ') then  ! Read header file if exists
            call read_hdr(a_hdrfile(i_file),a_type(i_file),i_lsize(i_file),i_ssize(i_file),
     &          r_peg(1,i_file),i_zone(i_file),r_str(1,i_file),r_spc(1,i_file),
     &          i_mbytes(i_file),i_dbytes(i_file),i_moff(1,i_file),i_doff(1,i_file),
     &          r_mdnc(1,i_file),r_ddnc(1,i_file),i_hdrstat(i_file),i_set(0,i_file))
          endif

          if (i_file .gt. 0) then
            if (a_magfile(i_file) .ne. ' ')  then
              call read_mhdr(a_magfile(i_file),i_mlsize,i_mssize,i_mbytes(i_file),i_moff(1,i_file))
              if (i_mssize .gt. 0) then
                i_lsize(i_file)=i_mlsize
                i_ssize(i_file)=i_mssize
              endif
            endif

            if (a_rmgfile(i_file) .ne. ' ')  then
              call read_mhdr(a_rmgfile(i_file),i_mlsize,i_mssize,i_mbytes(i_file),i_moff(1,i_file))
              if (i_mssize .gt. 0) then
                a_magfile(i_file)=a_rmgfile(i_file)
                a_rmgfile(i_file)=' '
                i_lsize(i_file)=i_mlsize
                i_ssize(i_file)=i_mssize
              endif
            endif

            if (a_dtefile(i_file) .ne. ' ') then
              call read_dhdr(a_dtefile(i_file),i_dlsize,i_dssize,i_dbytes(i_file),i_doff(1,i_file),r_dmul,r_dadd,
     &               r_peg(1,i_file),r_str(1,i_file),r_spc(1,i_file))
              if (i_dssize .gt. 0) then
                i_lsize(i_file)=i_dlsize
                i_ssize(i_file)=i_dssize
                r_ddnc(1,i_file) = r_dmul
                r_ddnc(2,i_file) = r_dadd
              endif
            endif
          endif

        enddo
c        write(6,*) ' set0 = ',(i_set(i,0),i=0,9)

        if (i_set(1,0) .eq. 0 ) then
          if (a_type(0) .eq. 'sch' .or. a_type(0) .eq. 'SCH') then
            print *,'Using Peg of 1st input image: ',sngl(r_peg(1,1)),sngl(r_peg(2,1)),
     &                    sngl(r_peg(3,1))
            r_peg(1,0) = r_peg(1,1)
            r_peg(2,0) = r_peg(2,1)
            r_peg(3,0) = r_peg(3,1)
          endif
        endif
        if (i_set(3,0) .eq. 0 ) then
          print *,'Using Spc of 1st input image: ',sngl(r_spc(1,1)),sngl(r_spc(2,1))
          r_spc(1,0) = r_spc(1,1)
          r_spc(2,0) = r_spc(2,1)
        endif

c
c  Compute transform matricies
c
        do i_file = 0,i_nof

          if (a_afffile(i_file) .ne. ' ') then  ! Read incremental affine transformation
            open(unit=39,file=a_afffile(i_file),form='formatted',status='old')
            read(39,*)
            read(39,*)
            read(39,*)
            read(39,*) r_aff(1,1),r_aff(1,2),r_aff(1,3)
            read(39,*) r_aff(2,1),r_aff(2,2),r_aff(2,3)
            read(39,*) r_aff(3,1),r_aff(3,2),r_aff(3,3)
            read(39,*)
            read(39,*) r_aff(1,4),r_aff(2,4),r_aff(3,4)
            close(39)
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
          endif

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
            endif
          else if (a_type(i_file).eq.'NEU' .or. a_type(i_file).eq.'neu' ) then
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
              print *,'setting zone = ',i_zone(i_file), i_file
              if (i_set(2,i_file) .eq. 0) then
                r_str(1,i_file) = v_oloc(1)
                r_str(2,i_file) = v_oloc(2)
                print *,'setting Northing/Easting = ',r_str(1,i_file),r_str(2,i_file)
              endif
            endif
          else if (a_type(i_file).eq.'UTM' .or. a_type(i_file).eq.'utm' ) then
            i_type(i_file) = -4
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
            endif
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
            stop 'File type not recognized'
          endif

          if (i_type(i_file) .eq. -2) then
            r_spc(2,i_file) = -r_spc(2,i_file)
            r_str(2,i_file) = -r_str(2,i_file)
          endif
          if (i_type(i_file) .eq. -4 ) r_spc(1,i_file) = -r_spc(1,i_file)

c          print *,' '
c          print *,'r_iat='
c          print *,r_iat(1,1),r_iat(1,2),r_iat(1,3)
c          print *,r_iat(2,1),r_iat(2,2),r_iat(2,3)
c          print *,r_iat(3,1),r_iat(3,2),r_iat(3,3)
c          print *,r_iat(1,4),r_iat(2,4),r_iat(3,4)

          if (i_file .eq. 0) then   ! output file

            call invrstrn(r_iat,r_inv)

c            print *,' '
c            print *,'r_inv='
c            print *,r_inv(1,1),r_inv(1,2),r_inv(1,3)
c            print *,r_inv(2,1),r_inv(2,2),r_inv(2,3)
c            print *,r_inv(3,1),r_inv(3,2),r_inv(3,3)
c            print *,r_inv(1,4),r_inv(2,4),r_inv(3,4)
c            print *,' '

          else

            call multitrn(r_iat,r_aff,r_tmp)
            call multitrn(r_inv,r_tmp,r_atm(1,1,i_file))
            call invrstrn(r_atm(1,1,i_file),r_mta(1,1,i_file))

c            print *,' '
c            print *,'r_atm='
c            print *,r_atm(1,1,i_file),r_atm(1,2,i_file),r_atm(1,3,i_file)
c            print *,r_atm(2,1,i_file),r_atm(2,2,i_file),r_atm(2,3,i_file)
c            print *,r_atm(3,1,i_file),r_atm(3,2,i_file),r_atm(3,3,i_file)
c            print *,r_atm(1,4,i_file),r_atm(2,4,i_file),r_atm(3,4,i_file)
c            print *,'r_mta='
c            print *,r_mta(1,1,i_file),r_mta(1,2,i_file),r_mta(1,3,i_file)
c            print *,r_mta(2,1,i_file),r_mta(2,2,i_file),r_mta(2,3,i_file)
c            print *,r_mta(3,1,i_file),r_mta(3,2,i_file),r_mta(3,3,i_file)
c            print *,r_mta(1,4,i_file),r_mta(2,4,i_file),r_mta(3,4,i_file)
            call multitrn(r_mta(1,1,i_file),r_atm(1,1,i_file),r_tmp)
c            print *,'r_tmp='
c            print *,r_tmp(1,1),r_tmp(1,2),r_tmp(1,3)
c            print *,r_tmp(2,1),r_tmp(2,2),r_tmp(2,3)
c            print *,r_tmp(3,1),r_tmp(3,2),r_tmp(3,3)
c            print *,r_tmp(1,4),r_tmp(2,4),r_tmp(3,4)

            if (i_ssize(i_file) .le.     0) then
              write(6,*) 'Error - Input file has 0 pixels per line'
              stop 'Execution Halted'
            endif
            if (i_ssize(i_file) .gt. I_MXS) then
              write(6,*) 'Error - Input file exceeds max pixels per line. ',i_ssize(i_file),I_MXS
              stop 'Execution Halted'
            endif

            do il = 0,1
              v_iloc1(1) = ((il*(i_lsize(i_file)-1))+1)*r_spc(1,i_file) + r_str(1,i_file)
              do is=0,1
                v_iloc1(2)=((is*(i_ssize(i_file)-1))+1)*r_spc(2,i_file) + r_str(2,i_file)
                do iz=0,1
                  v_iloc1(3)=iz*(r_max-r_min)+r_min

                  if (i_type(i_file) .eq. 1) then 
                    v_iloc2(1) = v_iloc1(1)
                    v_iloc2(2) = v_iloc1(2)
                    v_iloc2(3) = v_iloc1(3)
                  else if (i_type(i_file) .eq. 2 .or. i_type(i_file) .eq. -2) then  ! convert input from sch to xyz
                    r_lon=v_iloc1(1)/r_rad(i_file)
                    r_lat=v_iloc1(2)/r_rad(i_file)
                    r_hgt=v_iloc1(3)
                    call sch_to_tcn(r_rad(i_file),v_iloc2,r_lat,r_lon,r_hgt,1)
                  else if (i_type(i_file) .eq. 3) then        ! Convert input from enu to xyz
                    r_hgt=v_iloc1(3)
                    call enutoll(r_a,r_e2,i_zone(i_file),a_grid,v_iloc1,r_lat,r_lon,1)
                    call latlon(r_a,r_e2,v_iloc2,r_lat,r_lon,r_hgt,1)
                  else if (i_type(i_file) .eq. 4 .or. i_type(i_file) .eq. -4 ) then        ! Convert input from utm to xyz
                    r_hgt=v_iloc1(3)
                    call utmtoll(r_a,r_e2,i_zone(i_file),a_grid,v_iloc1,r_lat,r_lon,1)
                    call latlon(r_a,r_e2,v_iloc2,r_lat,r_lon,r_hgt,1)
                  else if (i_type(i_file) .eq. 5) then        ! Convert input from equal angle to xyz
                    r_lat=v_iloc1(1)/r_rtod
                    r_lon=v_iloc1(2)/r_rtod
                    r_hgt=v_iloc1(3)
                    call latlon(r_a,r_e2,v_iloc2,r_lat,r_lon,r_hgt,1)
                  endif

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
                  else if (i_type(0) .eq. 4 .or. i_type(0) .eq. -4) then                ! Convert output from xyz to utm
                    call latlon(r_a,r_e2,v_iloc3,r_lat,r_lon,r_hgt,2)
                    call utmtoll(r_a,r_e2,i_zone(0),a_grid,v_oloc,r_lat,r_lon,2)
                    v_oloc(3) = r_hgt
                  else if (i_type(0) .eq. 5) then                ! Convert output from xyz to ll
                    call latlon(r_a,r_e2,v_iloc3,r_lat,r_lon,r_hgt,2)
                    v_oloc(1) = r_lat*r_rtod
                    v_oloc(2) = r_lon*r_rtod
                    v_oloc(3) = r_hgt
                  endif

                  v_oloc(1) = (v_oloc(1) - r_str(1,0))/r_spc(1,0)
                  v_oloc(2) = (v_oloc(2) - r_str(2,0))/r_spc(2,0)

                  i_fstln=min(i_fstln,nint(v_oloc(1)))
                  i_lstln=max(i_lstln,nint(v_oloc(1)))
                  i_fstsp=min(i_fstsp,nint(v_oloc(2)))
                  i_lstsp=max(i_lstsp,nint(v_oloc(2)))

                enddo
              enddo
            enddo

            if (i_nof .lt. 20) then
              if (a_rmgfile(i_file) .ne. ' ') then
                write(6,'(x,a,a,2i6)') 'Opening rmg input  file: ',a_rmgfile(i_file)(1:40),i_ssize(i_file),i_lsize(i_file)
                open(unit=40+i_file,file=a_rmgfile(i_file),form='unformatted',
     &             status='old',access='direct',recl=8*i_ssize(i_file))
              endif
              if (a_magfile(i_file) .ne. ' ') then
                write(6,'(x,a,a,2i6)') 'Opening amp input  file: ',a_magfile(i_file)(1:40),i_ssize(i_file),i_lsize(i_file)
                open(unit=60+i_file,file=a_magfile(i_file),form='unformatted',
     &           status='old',access='direct',recl=i_mbytes(i_file)*i_ssize(i_file))
              endif
              if (a_dtefile(i_file) .ne. ' ') then
                write(6,'(x,a,a,2i6)') 'Opening hgt input  file: ',a_dtefile(i_file)(1:40),i_ssize(i_file),i_lsize(i_file)
                open(unit=80+i_file,file=a_dtefile(i_file),form='unformatted',
     &           status='old',access='direct',recl=i_dbytes(i_file)*i_ssize(i_file))
              endif
            endif

          endif

        enddo

        i_osoff = i_fstsp-1
        i_osnum = i_lstsp-i_fstsp+1
        i_oloff = i_fstln-1
        i_olnum = i_lstln-i_fstln+1

        if (i_lsize(0) .le. 0) then
          r_str(1,0) = r_str(1,0) + i_oloff*r_spc(1,0)
        endif

        if (i_ssize(0) .le. 0) then
          r_str(2,0) = r_str(2,0) + i_osoff*r_spc(2,0)
        endif

        if (i_ssize(0) .eq. 0) i_ssize(0) = i_osnum
        if (i_lsize(0) .eq. 0) i_lsize(0) = i_olnum
        i_ssize(0) = abs(i_ssize(0))
        i_lsize(0) = abs(i_lsize(0))

        if (i_ssize(0) .le.     0) stop 'Error - Output file has 0 pixels per line'
        if (i_ssize(0) .gt. I_MXS) then
          print *, 'Error - Output file exceeds max pixels per line', i_ssize(0),I_MXS
          stop 'Execution Halted'
        endif

        print *,' '
        if (a_stdfile(0) .ne. ' ') then
          write(6,'(x,a,a,2i6)') 'Opening std output file: ',a_stdfile(0)(1:40),i_ssize(0),i_lsize(0)
          open(unit=29,file=a_stdfile(0),form='unformatted',status='unknown',
     &       access='direct',recl=8*i_ssize(0))
        endif
        if (a_errfile(0) .ne. ' ') then
          write(6,'(x,a,a,2i6)') 'Opening err output file: ',a_errfile(0)(1:40),i_ssize(0),i_lsize(0)
          open(unit=28,file=a_errfile(0),form='unformatted',status='unknown',
     &       access='direct',recl=8*i_ssize(0))
        endif
        if (a_rmgfile(0) .ne. ' ') then
          write(6,'(x,a,a,2i6)') 'Opening rmg output file: ',a_rmgfile(0)(1:40),i_ssize(0),i_lsize(0)
          open(unit=30,file=a_rmgfile(0),form='unformatted',status='unknown',
     &       access='direct',recl=8*i_ssize(0))
        endif
        if (a_magfile(0) .ne. ' ') then
          write(6,'(x,a,a,2i6)') 'Opening amp output file: ',a_magfile(0)(1:40),i_ssize(0),i_lsize(0)
          open(unit=31,file=a_magfile(0),form='unformatted',status='unknown',
     &       access='direct',recl=i_mbytes(0)*(i_ssize(0)+i_moff(2,i_file)))
        endif
        if (a_dtefile(0) .ne. ' ') then
          write(6,'(x,a,a,2i6)') 'Opening hgt output file: ',a_dtefile(0)(1:40),i_ssize(0),i_lsize(0)
          open(unit=32,file=a_dtefile(0),form='unformatted',status='unknown',
     &       access='direct',recl=i_dbytes(0)*(i_ssize(0)+i_doff(2,i_file)))
        endif

        if (i_hdrstat(0) .ne. 0 .or. i_override .ne. 0) then ! write new hdr
          if (a_hdrfile(0) .ne. ' ') then
            call write_hdr(a_hdrfile,' ',a_type(0),i_lsize(0),i_ssize(0),
     &           r_peg(1,0),r_str(1,0),r_spc(1,0),i_zone,
     &           i_mbytes(0),i_dbytes(0),r_mdnc(1,0),r_ddnc(1,0),i_err)
          endif
        endif

        write(6,*) ' '
        write(6,*) 'Output file offsets = ',r_str(1,0),r_str(2,0)
        if (i_zone(0) .ne. 0) write(6,*) 'Output file zone    = ',i_zone(0)
        write(6,*) ' '

        i_nbs = (i_ssize(0)+I_OBS-1)/I_OBS
        i_nbl = (i_lsize(0)+I_OBL-1)/I_OBL
        do i_bl=1,i_nbl
          i_oloff=(i_bl-1)*I_OBL
          i_olnum=min(I_OBL,i_lsize(0)-i_oloff)
          do i_bs=1,i_nbs
            i_osoff=(i_bs-1)*I_OBS
            i_osnum=min(I_OBS,i_ssize(0)-i_osoff)
c            type *,'Clearing output arrays'
            do il=1,i_olnum
              do is=1,i_osnum
                r_obufa(is,il)=0.
                r_obufb(is,il)=0.
                r_obufc(is,il)=0.
                r_sbuff(is,il)=0.
              enddo
            enddo
            print *,'at block ',i_bs,i_bl

            do i_file=1,i_nof          ! Loop over input files

              i_fstsp= 999999999
              i_lstsp=-999999999
              i_fstln= 999999999
              i_lstln=-999999999
              
              do il = 0,1
                v_oloc1(1) = (il*(min(I_OBL,i_lsize(0)-i_oloff)-1)+i_oloff+1)*r_spc(1,0) + r_str(1,0)
                do is=0,1
                  v_oloc1(2) = (is*(min(I_OBS,i_ssize(0)-i_osoff)-1)+i_osoff+1)*r_spc(2,0) + r_str(2,0)
                  do iz=0,1
                    v_oloc1(3)=iz*(r_max-r_min)+r_min

                    if (i_type(0) .eq. 1) then
                      v_oloc2(1) = v_oloc1(1)
                      v_oloc2(2) = v_oloc1(2)
                      v_oloc2(3) = v_oloc1(3)
                    else if (i_type(0) .eq. 2 .or. i_type(0) .eq. -2) then                ! Convert output from xyz to sch
                      r_lon=v_oloc1(1)/r_rad(0)
                      r_lat=v_oloc1(2)/r_rad(0)
                      r_hgt=v_oloc1(3)
                      call sch_to_tcn(r_rad(0),v_oloc2,r_lat,r_lon,r_hgt,1)
                    else if (i_type(0) .eq. 3) then                ! Convert output from xyz to enu
                      r_hgt=v_oloc1(3)
                      call enutoll(r_a,r_e2,i_zone(0),a_grid,v_oloc1,r_lat,r_lon,1)
                      call latlon(r_a,r_e2,v_oloc2,r_lat,r_lon,r_hgt,1)
                    else if (i_type(0) .eq. 4 .or. i_type(0) .eq. -4) then                ! Convert output from xyz to utm
                      r_hgt=v_oloc1(3)
                      call utmtoll(r_a,r_e2,i_zone(0),a_grid,v_oloc1,r_lat,r_lon,1)
                      call latlon(r_a,r_e2,v_oloc2,r_lat,r_lon,r_hgt,1)
                    else if (i_type(0) .eq. 5) then                ! Convert output from xyz to equal angle
                      r_lat=v_oloc1(1)/r_rtod
                      r_lon=v_oloc1(2)/r_rtod
                      r_hgt=v_oloc1(3)
                      call latlon(r_a,r_e2,v_oloc2,r_lat,r_lon,r_hgt,1)
                    endif

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
                    else if (i_type(i_file) .eq. 3) then        ! Convert input from enu to xyz
                      call latlon(r_a,r_e2,v_oloc3,r_lat,r_lon,r_hgt,2)
                      call enutoll(r_a,r_e2,i_zone(i_file),a_grid,v_iloc,r_lat,r_lon,2)
                      v_iloc(3) = r_hgt
                    else if (i_type(i_file) .eq. 4 .or. i_type(i_file) .eq. -4) then        ! Convert input from utm to xyz
                      call latlon(r_a,r_e2,v_oloc3,r_lat,r_lon,r_hgt,2)
                      call utmtoll(r_a,r_e2,i_zone(i_file),a_grid,v_iloc,r_lat,r_lon,2)
                      v_iloc(3) = r_hgt
                    else if (i_type(i_file) .eq. 5) then        ! Convert output from xyz to ll
                      call latlon(r_a,r_e2,v_oloc3,r_lat,r_lon,r_hgt,2)
                      v_iloc(1) = r_lat*r_rtod
                      v_iloc(2) = r_lon*r_rtod
                      v_iloc(3) = r_hgt
                    endif

                    v_iloc(1) = (v_iloc(1) - r_str(1,i_file))/r_spc(1,i_file)
                    v_iloc(2) = (v_iloc(2) - r_str(2,i_file))/r_spc(2,i_file)

                    i_fstln=min(i_fstln,nint(v_iloc(1)))
                    i_lstln=max(i_lstln,nint(v_iloc(1)))
                    i_fstsp=min(i_fstsp,nint(v_iloc(2)))
                    i_lstsp=max(i_lstsp,nint(v_iloc(2)))
                  enddo
                enddo
              enddo

              i_fstln=max(i_fstln-i_fln,1)
              i_lstln=min(i_lstln+i_fln,i_lsize(i_file))
              i_fstsp=max(i_fstsp-i_fln,1)
              i_lstsp=min(i_lstsp+i_fln,i_ssize(i_file))

              i_isoff = i_fstsp-1
              i_isnum = i_lstsp-i_fstsp+1
              i_iloff = i_fstln-1
              i_ilnum = i_lstln-i_fstln+1

              i_isfac = max((i_isnum+I_IBS-1)/(I_IBS),i_ifacmin(i_file))
              i_ilfac = max((i_ilnum+I_IBL-1)/(I_IBL),i_ifacmin(i_file))

              i_fstln=max(i_fstln-i_ilfac,1)                          ! Have to iterate here alittle needs work
              i_lstln=min(i_lstln+i_ilfac,i_lsize(i_file))
              i_fstsp=max(i_fstsp-i_isfac,1)
              i_lstsp=min(i_lstsp+i_isfac,i_ssize(i_file))

              i_isoff = i_fstsp-1
              i_isnum = i_lstsp-i_fstsp+1
              i_iloff = i_fstln-1
              i_ilnum = i_lstln-i_fstln+1

              i_isfac = max((i_isnum+I_IBS-1)/(I_IBS),i_ifacmin(i_file))
              i_ilfac = max((i_ilnum+I_IBL-1)/(I_IBL),i_ifacmin(i_file))

c              print *,'so,sn,lo,ln = ',i_isoff,i_isnum,i_iloff,i_ilnum

              if (i_isnum .gt. 0 .and. i_ilnum .gt. 0) then
                if (i_nof .ge. 20) then
                  i_ffff = 0
                  if (a_rmgfile(i_file) .ne. ' ') then
                    write(6,'(x,a,a,2i6)') 'Opening rmg input  file: ',a_rmgfile(i_file)(1:40),i_ssize(i_file),i_lsize(i_file)
                    open(unit=40+i_ffff,file=a_rmgfile(i_file),form='unformatted',
     &                 status='old',access='direct',recl=8*i_ssize(i_file))
                  endif
                  if (a_magfile(i_file) .ne. ' ') then
                    write(6,'(x,a,a,2i6)') 'Opening amp input  file: ',a_magfile(i_file)(1:40),i_ssize(i_file),i_lsize(i_file)
                    open(unit=60+i_ffff,file=a_magfile(i_file),form='unformatted',
     &               status='old',access='direct',recl=i_mbytes(i_file)*(i_ssize(i_file)+i_moff(2,i_file)))
                  endif
                  if (a_dtefile(i_file) .ne. ' ') then
                    write(6,'(x,a,a,2i6)') 'Opening hgt input  file: ',a_dtefile(i_file)(1:40),i_ssize(i_file),i_lsize(i_file)
                    open(unit=80+i_ffff,file=a_dtefile(i_file),form='unformatted',
     &               status='old',access='direct',recl=i_dbytes(i_file)*(i_ssize(i_file)+i_doff(2,i_file)))
                  endif
                else
                  i_ffff = i_file
                endif
                print *,' Reading file ',i_file,'     ',i_isnum,i_ilnum,max(i_isfac,i_ilfac)
c                print *,'   lines: ',i_fstln,' to ',i_lstln !@#$%
c                print *,'   pixls: ',i_fstsp,' to ',i_lstsp !@#$%

                if (a_rmgfile(i_file) .ne. ' ') then
                  do il=1,i_ilnum/i_ilfac
                    read(40+i_ffff,rec=(il*i_ilfac)+i_iloff) 
     &                 (r_data(is),is=1,i_ssize(i_file)),(r_datb(iss),iss=1,i_ssize(i_file))
                    do is=1,i_isnum/i_isfac
c                      if (r_data((is*i_isfac)+i_isoff) .ne. 0.) then !@#$% SJS 9/24/96 commented out if statement
                        r_ibufa(is,il)=r_data((is*i_isfac)+i_isoff)             
                        r_ibufb(is,il)=r_datb((is*i_isfac)+i_isoff)
c                      endif
                      if (r_ibufb(is,il) .lt. r_min .or. r_ibufb(is,il) .gt. r_max) r_ibufa(is,il)=0.        
                      if (r_ibufb(is,il) .eq. r_nul) r_ibufa(is,il)=0.        
                    enddo   
                  enddo
                else
                  if (a_magfile(i_file) .ne. ' ') then
                    if (i_mbytes(i_file) .eq. 1) then
                      do il=1,i_ilnum/i_ilfac
                        read(60+i_ffff,rec=il*i_ilfac+i_iloff+i_moff(1,i_file)) (b_data1(is),is=1,i_ssize(i_file))
                        do is=1,i_isnum/i_isfac
                          i_data2(is*i_isfac+i_isoff) = b_data1(is*i_isfac+i_isoff+i_moff(2,i_file))
                          if (i_data2(is*i_isfac+i_isoff) .lt. 0) then
                            i_data2(is*i_isfac+i_isoff) = 256 + i_data2(is*i_isfac+i_isoff)
                          endif
                          r_ibufa(is,il)=r_mdnc(1,i_file)*i_data2(is*i_isfac+i_isoff)  +  r_mdnc(2,i_file)           
                        enddo
                      enddo
                    else if (i_mbytes(i_file) .eq. 2) then
                      do il=1,i_ilnum/i_ilfac
                        read(60+i_ffff,rec=il*i_ilfac+i_iloff+i_moff(1,i_file)) (i_data2(is),is=1,i_ssize(i_file))
                        do is=1,i_isnum/i_isfac
                          r_ibufa(is,il)=r_mdnc(1,i_file)*i_data2(is*i_isfac+i_isoff+i_moff(2,i_file))  +  r_mdnc(2,i_file)           
                        enddo
                      enddo
                    else
                      do il=1,i_ilnum/i_ilfac
                        read(60+i_ffff,rec=il*i_ilfac+i_iloff+i_moff(1,i_file)) (r_data(is),is=1,i_ssize(i_file))
                        do is=1,i_isnum/i_isfac
                          r_ibufa(is,il)=r_data(is*i_isfac+i_isoff+i_moff(2,i_file))             
                        enddo
                      enddo
                    endif
                  else
                    do il=1,i_ilnum/i_ilfac
                      do is=1,i_isnum/i_isfac
                        r_ibufa(is,il)=1.            
                      enddo
                    enddo
                  endif
                  if (a_dtefile(i_file) .ne. ' ') then
                    if (i_dbytes(i_file) .eq. 1) then
                      do il=1,i_ilnum/i_ilfac
                        read(80+i_ffff,rec=il*i_ilfac+i_iloff+i_doff(1,i_file)) (b_datb1(is),is=1,i_ssize(i_file))
                        do is=1,i_isnum/i_isfac
                          i_datb2(is*i_isfac+i_isoff) = b_datb1(is*i_isfac+i_isoff+i_doff(2,i_file))
                          if (i_datb2(is*i_isfac+i_isoff) .lt. 0) then
                            i_datb2(is*i_isfac+i_isoff) = 256 + i_datb2(is*i_isfac+i_isoff)
                          endif
                          r_ibufb(is,il)=r_ddnc(1,i_file)*i_datb2(is*i_isfac+i_isoff) + r_ddnc(2,i_file)
                          if (r_ibufb(is,il).lt.r_min .or. r_ibufb(is,il).gt.r_max) r_ibufa(is,il)=0.        
                          if (r_ibufb(is,il) .eq. r_nul) r_ibufa(is,il)=0.        
                        enddo
                      enddo
                    else if (i_dbytes(i_file) .eq. 2) then
                      do il=1,i_ilnum/i_ilfac
                        read(80+i_ffff,rec=il*i_ilfac+i_iloff+i_doff(1,i_file)) (i_datb2(is),is=1,i_ssize(i_file))
                        do is=1,i_isnum/i_isfac
                          r_ibufb(is,il)=r_ddnc(1,i_file)*i_datb2(is*i_isfac+i_isoff+i_doff(2,i_file)) + r_ddnc(2,i_file)
                          if (r_ibufb(is,il).lt.r_min .or. r_ibufb(is,il).gt.r_max) r_ibufa(is,il)=0.        
                          if (r_ibufb(is,il) .eq. r_nul) r_ibufa(is,il)=0.        
                        enddo
                      enddo
                    else
                      do il=1,i_ilnum/i_ilfac
                        read(80+i_ffff,rec=il*i_ilfac+i_iloff+i_doff(1,i_file)) (r_data(is),is=1,i_ssize(i_file))
                        do is=1,i_isnum/i_isfac
                          r_ibufb(is,il)=r_data(is*i_isfac+i_isoff+i_doff(2,i_file)) 
                          if (r_ibufb(is,il).lt.r_min .or. r_ibufb(is,il).gt.r_max) r_ibufa(is,il)=0.        
                          if (r_ibufb(is,il) .eq. r_nul) r_ibufa(is,il)=0.        
                        enddo
                      enddo
                    endif
                  else
                    do il=1,i_ilnum/i_ilfac
                      do is=1,i_isnum/i_isfac
                        r_ibufb(is,il)=0            
                      enddo
                    enddo
                  endif
                endif

c
c  Find Image Boundarys and Gaps
c
                do is = 1, i_osnum       ! Initialize scale array
                  do il = 1, i_olnum
                    r_scale(is,il)=max(i_fln,1)*r_wgt(i_file)
                  enddo
                enddo

                if (i_fln .gt. 0) then
c                  write(6,*) ' '
c                  write(6,'(x,a,i4)') 'Finding edges of input image ',i_file
                  i_gpc=0
                  i_sum=0
                  r_sum=0.
                  i_cnt=0
                  do il=1,i_ilnum/i_ilfac
                    do is=1,i_isnum/i_isfac
                      if (r_ibufa(is,il) .gt. 0 ) then
                        i_sum=i_sum+1
                        r_sum=r_sum+r_ibufb(is,il)
                        if (il .eq. 1 .or. il .eq. i_ilnum/i_ilfac .or.
     &                      is .eq. 1 .or. is .eq. i_isnum/i_isfac ) then
                          if (i_gpc .lt. I_MGP) then
                            i_gpc=i_gpc+1
                          else
                            i_cnt=i_cnt+1
                          endif
                          v_gpl(1,i_gpc)=(il*i_ilfac+i_iloff)*r_spc(1,i_file)+r_str(1,i_file)
                          v_gpl(2,i_gpc)=(is*i_isfac+i_isoff)*r_spc(2,i_file)+r_str(2,i_file)
                          v_gpl(3,i_gpc)=r_ibufb(is,il)
c     c                     print *,'i_gpc',i_gpc,v_gpl(1,i_gpc),v_gpl(2,i_gpc),v_gpl(3,i_gpc) !@#$%

                          if (i_type(i_file) .eq. 1) then 
                            v_iloc2(1) = v_gpl(1,i_gpc)
                            v_iloc2(2) = v_gpl(2,i_gpc)
                            v_iloc2(3) = v_gpl(3,i_gpc)
                          else if (i_type(i_file) .eq. 2 .or. i_type(i_file) .eq. -2) then        ! convert input from sch to xyz
                            r_lon=v_gpl(1,i_gpc)/r_rad(i_file)
                            r_lat=v_gpl(2,i_gpc)/r_rad(i_file)
                            r_hgt=v_gpl(3,i_gpc)
                            call sch_to_tcn(r_rad(i_file),v_iloc2,r_lat,r_lon,r_hgt,1)
                          else if (i_type(i_file) .eq. 3) then        ! Convert input from utm to xyz
                            r_hgt=v_gpl(3,i_gpc)
                            call enutoll(r_a,r_e2,i_zone(i_file),a_grid,v_gpl(1,i_gpc),r_lat,r_lon,1)
                            call latlon(r_a,r_e2,v_iloc2,r_lat,r_lon,r_hgt,1)
                          else if (i_type(i_file) .eq. 4 .or. i_type(i_file) .eq. -4) then        ! Convert input from utm to xyz
                            r_hgt=v_gpl(3,i_gpc)
                            call utmtoll(r_a,r_e2,i_zone(i_file),a_grid,v_gpl(1,i_gpc),r_lat,r_lon,1)
                            call latlon(r_a,r_e2,v_iloc2,r_lat,r_lon,r_hgt,1)
                          else if (i_type(i_file) .eq. 5) then        ! Convert input from equal angle to xyz
                            r_lat=v_gpl(1,i_gpc)/r_rtod
                            r_lon=v_gpl(2,i_gpc)/r_rtod
                            r_hgt=v_gpl(3,i_gpc)
                            call latlon(r_a,r_e2,v_iloc2,r_lat,r_lon,r_hgt,1)
                          endif

                          call vecmulti(r_atm(1,1,i_file),v_iloc2,v_iloc3) ! convert from input xyz to output xyz
                          call vecaddit(r_atm(1,4,i_file),v_iloc3,v_iloc3)

                          if (i_type(0) .eq. 1) then
                            v_gpl(1,i_gpc) = v_iloc3(1)
                            v_gpl(2,i_gpc) = v_iloc3(2)
                            v_gpl(3,i_gpc) = v_iloc3(3)
                          else if (i_type(0) .eq. 2 .or. i_type(0) .eq. -2) then                ! Convert output from xyz to sch
                            call sch_to_tcn(r_rad(0),v_iloc3,r_lat,r_lon,r_hgt,2)
                            v_gpl(1,i_gpc) = r_lon*r_rad(0)
                            v_gpl(2,i_gpc) = r_lat*r_rad(0)
                            v_gpl(3,i_gpc) = r_hgt
                          else if (i_type(0) .eq. 3) then                ! Convert output from xyz to utm
                            call latlon(r_a,r_e2,v_iloc3,r_lat,r_lon,r_hgt,2)
                            call enutoll(r_a,r_e2,i_zone(0),a_grid,v_gpl(1,i_gpc),r_lat,r_lon,2)
                            v_gpl(3,i_gpc) = r_hgt
                          else if (i_type(0) .eq. 4 .or. i_type(0) .eq. -4) then                ! Convert output from xyz to utm
                            call latlon(r_a,r_e2,v_iloc3,r_lat,r_lon,r_hgt,2)
                            call utmtoll(r_a,r_e2,i_zone(0),a_grid,v_gpl(1,i_gpc),r_lat,r_lon,2)
                            v_gpl(3,i_gpc) = r_hgt
                          else if (i_type(0) .eq. 5) then                ! Convert output from xyz to ll
                            call latlon(r_a,r_e2,v_iloc3,r_lat,r_lon,r_hgt,2)
                            v_gpl(1,i_gpc) = r_lat*r_rtod
                            v_gpl(2,i_gpc) = r_lon*r_rtod
                            v_gpl(3,i_gpc) = r_hgt
                          endif

                          i_gpl=nint((v_gpl(1,i_gpc)-r_str(1,0))/r_spc(1,0))-i_oloff
                          i_gps=nint((v_gpl(2,i_gpc)-r_str(2,0))/r_spc(2,0))-i_osoff

                          do iss=max(i_gps-i_fln,1),min(i_gps+i_fln,i_osnum)
                            do ill=max(i_gpl-i_fln,1),min(i_gpl+i_fln,i_olnum)
                              r_scale(iss,ill)=min(sngl(r_dist(iss-i_gps,ill-i_gpl)*r_wgt(i_file)),r_scale(iss,ill))
                            enddo
                          enddo
                        else if ((r_ibufa(is-1,il) .le. 0) .or.
     &                           (r_ibufa(is+1,il) .le. 0) .or.
     &                           (r_ibufa(is,il-1) .le. 0) .or.
     &                           (r_ibufa(is,il+1) .le. 0) ) then
                          if (i_gpc .lt. I_MGP) then
                            i_gpc=i_gpc+1
                          else
                            i_cnt=i_cnt+1
                          endif
                          v_gpl(1,i_gpc)=(il*i_ilfac+i_iloff)*r_spc(1,i_file)+r_str(1,i_file)
                          v_gpl(2,i_gpc)=(is*i_isfac+i_isoff)*r_spc(2,i_file)+r_str(2,i_file)
                          v_gpl(3,i_gpc)=r_ibufb(is,il)
c     c                     print *,'i_gpc',i_gpc,v_gpl(1,i_gpc),v_gpl(2,i_gpc),v_gpl(3,i_gpc) !@#$%

                          if (i_type(i_file) .eq. 1) then 
                            v_iloc2(1) = v_gpl(1,i_gpc)
                            v_iloc2(2) = v_gpl(2,i_gpc)
                            v_iloc2(3) = v_gpl(3,i_gpc)
                          else if (i_type(i_file) .eq. 2 .or. i_type(i_file) .eq. -2) then        ! convert input from sch to xyz
                            r_lon=v_gpl(1,i_gpc)/r_rad(i_file)
                            r_lat=v_gpl(2,i_gpc)/r_rad(i_file)
                            r_hgt=v_gpl(3,i_gpc)
                            call sch_to_tcn(r_rad(i_file),v_iloc2,r_lat,r_lon,r_hgt,1)
                          else if (i_type(i_file) .eq. 3) then        ! Convert input from utm to xyz
                            r_hgt=v_gpl(3,i_gpc)
                            call enutoll(r_a,r_e2,i_zone(i_file),a_grid,v_gpl(1,i_gpc),r_lat,r_lon,1)
                            call latlon(r_a,r_e2,v_iloc2,r_lat,r_lon,r_hgt,1)
                          else if (i_type(i_file) .eq. 4 .or. i_type(i_file) .eq. -4) then        ! Convert input from utm to xyz
                            r_hgt=v_gpl(3,i_gpc)
                            call utmtoll(r_a,r_e2,i_zone(i_file),a_grid,v_gpl(1,i_gpc),r_lat,r_lon,1)
                            call latlon(r_a,r_e2,v_iloc2,r_lat,r_lon,r_hgt,1)
                          else if (i_type(i_file) .eq. 5) then        ! Convert input from equal angle to xyz
                            r_lat=v_gpl(1,i_gpc)/r_rtod
                            r_lon=v_gpl(2,i_gpc)/r_rtod
                            r_hgt=v_gpl(3,i_gpc)
                            call latlon(r_a,r_e2,v_iloc2,r_lat,r_lon,r_hgt,1)
                          endif

                          call vecmulti(r_atm(1,1,i_file),v_iloc2,v_iloc3) ! convert from input xyz to output xyz
                          call vecaddit(r_atm(1,4,i_file),v_iloc3,v_iloc3)

                          if (i_type(0) .eq. 1) then
                            v_gpl(1,i_gpc) = v_iloc3(1)
                            v_gpl(2,i_gpc) = v_iloc3(2)
                            v_gpl(3,i_gpc) = v_iloc3(3)
                          else if (i_type(0) .eq. 2 .or. i_type(0) .eq. -2) then                ! Convert output from xyz to sch
                            call sch_to_tcn(r_rad(0),v_iloc3,r_lat,r_lon,r_hgt,2)
                            v_gpl(1,i_gpc) = r_lon*r_rad(0)
                            v_gpl(2,i_gpc) = r_lat*r_rad(0)
                            v_gpl(3,i_gpc) = r_hgt
                          else if (i_type(0) .eq. 3) then                ! Convert output from xyz to utm
                            call latlon(r_a,r_e2,v_iloc3,r_lat,r_lon,r_hgt,2)
                            call enutoll(r_a,r_e2,i_zone(0),a_grid,v_gpl(1,i_gpc),r_lat,r_lon,2)
                            v_gpl(3,i_gpc) = r_hgt
                          else if (i_type(0) .eq. 4 .or. i_type(0) .eq. -4) then                ! Convert output from xyz to utm
                            call latlon(r_a,r_e2,v_iloc3,r_lat,r_lon,r_hgt,2)
                            call utmtoll(r_a,r_e2,i_zone(0),a_grid,v_gpl(1,i_gpc),r_lat,r_lon,2)
                            v_gpl(3,i_gpc) = r_hgt
                          else if (i_type(0) .eq. 5) then                ! Convert output from xyz to ll
                            call latlon(r_a,r_e2,v_iloc3,r_lat,r_lon,r_hgt,2)
                            v_gpl(1,i_gpc) = r_lat*r_rtod
                            v_gpl(2,i_gpc) = r_lon*r_rtod
                            v_gpl(3,i_gpc) = r_hgt
                          endif

                          i_gpl=nint((v_gpl(1,i_gpc)-r_str(1,0))/r_spc(1,0))-i_oloff
                          i_gps=nint((v_gpl(2,i_gpc)-r_str(2,0))/r_spc(2,0))-i_osoff

                          do iss=max(i_gps-i_fln,1),min(i_gps+i_fln,i_osnum)
                            do ill=max(i_gpl-i_fln,1),min(i_gpl+i_fln,i_olnum)
                              r_scale(iss,ill)=min(sngl(r_dist(iss-i_gps,ill-i_gpl)*r_wgt(i_file)),r_scale(iss,ill))
                            enddo
                          enddo
                        endif
                      endif
                    enddo
                  enddo
                  r_avg=r_sum/max(i_sum,1)
c                  write(6,*) ' '
c                  write(6,*) 'Average pix =  ',r_avg
c                  write(6,*) ' '
                  if (i_cnt .gt. 0) then
                    write(6,*) ' '
                    write(6,*) 'Warning - Feathering will not work properly because the number of'
                    write(6,*) '          gaps exceed max per block: ',I_MGP,i_cnt+I_MGP
                    write(6,*) ' '
                  endif

c
c  Write out gap pixel info
c
c                  print *,'i_gpc = ',i_gpc
c                  do i=1,i_gpc
c                    write(6,*) i,v_gpl(1,i),v_gpl(2,i),v_gpl(3,i)
c                  enddo

                endif

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
                          else if (i_type(i_file) .eq. 4 .or. i_type(i_file) .eq. -4) then        ! Convert input from utm to xyz
                            r_hgt=v_iloc(3)
                            call utmtoll(r_a,r_e2,i_zone(i_file),a_grid,v_iloc,r_lat,r_lon,1)
                            call latlon(r_a,r_e2,v_iloc2,r_lat,r_lon,r_hgt,1)
                          else if (i_type(i_file) .eq. 5) then        ! Convert input from equal angle to xyz
                            r_lat=v_iloc(1)/r_rtod
                            r_lon=v_iloc(2)/r_rtod
                            r_hgt=v_iloc(3)
                            call latlon(r_a,r_e2,v_iloc2,r_lat,r_lon,r_hgt,1)
                          endif

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
                          else if (i_type(0) .eq. 4 .or. i_type(0) .eq. -4) then                ! Convert output from xyz to utm
                            call latlon(r_a,r_e2,v_iloc3,r_lat,r_lon,r_hgt,2)
                            call utmtoll(r_a,r_e2,i_zone(0),a_grid,v_olocc(1,2,is),r_lat,r_lon,2)
                            v_olocc(3,2,is) = r_hgt
                          else if (i_type(0) .eq. 5) then                ! Convert output from xyz to ll
                            call latlon(r_a,r_e2,v_iloc3,r_lat,r_lon,r_hgt,2)
                            v_olocc(1,2,is) = r_lat*r_rtod
                            v_olocc(2,2,is) = r_lon*r_rtod
                            v_olocc(3,2,is) = r_hgt
                          endif

                    v_olocc(1,2,is)=nint((v_olocc(1,2,is)-r_str(1,0))/r_spc(1,0))-i_oloff
                    v_olocc(2,2,is)=nint((v_olocc(2,2,is)-r_str(2,0))/r_spc(2,0))-i_osoff
                    v_olocc(4,2,is)=r_ibufa(is,1)
                    i_oloc1(2,is)=1.
                  else
                    i_oloc1(2,is)=0.
                  endif
                enddo

                do il=1,i_ilnum/i_ilfac-1
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
                          else if (i_type(i_file) .eq. 4 .or. i_type(i_file) .eq. -4) then        ! Convert input from utm to xyz
                            r_hgt=v_iloc(3)
                            call utmtoll(r_a,r_e2,i_zone(i_file),a_grid,v_iloc,r_lat,r_lon,1)
                            call latlon(r_a,r_e2,v_iloc2,r_lat,r_lon,r_hgt,1)
                          else if (i_type(i_file) .eq. 5) then        ! Convert input from equal angle to xyz
                            r_lat=v_iloc(1)/r_rtod
                            r_lon=v_iloc(2)/r_rtod
                            r_hgt=v_iloc(3)
                            call latlon(r_a,r_e2,v_iloc2,r_lat,r_lon,r_hgt,1)
                          endif

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
                          else if (i_type(0) .eq. 4 .or. i_type(0) .eq. -4) then                ! Convert output from xyz to utm
                            call latlon(r_a,r_e2,v_iloc3,r_lat,r_lon,r_hgt,2)
                            call utmtoll(r_a,r_e2,i_zone(0),a_grid,v_olocc(1,2,is),r_lat,r_lon,2)
                            v_olocc(3,2,is) = r_hgt
                          else if (i_type(0) .eq. 5) then                ! Convert output from xyz to ll
                            call latlon(r_a,r_e2,v_iloc3,r_lat,r_lon,r_hgt,2)
                            v_olocc(1,2,is) = r_lat*r_rtod
                            v_olocc(2,2,is) = r_lon*r_rtod
                            v_olocc(3,2,is) = r_hgt
                          endif

                      v_olocc(1,2,is)=((v_olocc(1,2,is)-r_str(1,0))/r_spc(1,0))-i_oloff
                      v_olocc(2,2,is)=((v_olocc(2,2,is)-r_str(2,0))/r_spc(2,0))-i_osoff
                      v_olocc(4,2,is)=r_ibufa(is,il+1)
                      i_oloc1(2,is)=1.
                    else
                      i_oloc1(2,is)=0.
                    endif

                  enddo
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
                        endif
                      enddo
                    enddo
c                    print *,'is=',is,max(i_smin+1,1),min(i_smax,i_osnum) !@#$%
c                    print *,'   ',il,max(i_lmin+1,1),min(i_lmax,i_olnum) !@#$%

                    do iss = max(i_smin,1),min(i_smax+1,i_osnum)
                      do ill = max(i_lmin,1),min(i_lmax+1,i_olnum)

                        if ( r_scale(iss,ill) .gt. 0.) then
                          v_pnt(1) = ill
                          v_pnt(2) = iss
                          v_pnt(3) = 0.
                          v_pnt(4) = 0.

                          if (i_smp .eq. 1) then
                            call nearestpp(v_olocc(1,1,is),i_oloc1(1,is),v_pnt,i_flg)
                          else
                            call inplane2d(v_olocc(1,1,is),i_oloc1(1,is),v_pnt,i_flg)
                          endif
                          if (i_flg .ne. 0 .and. r_scale(iss,ill) .gt. 0.) then
                            r_aa = v_pnt(4)
                            r_bb = v_pnt(3)
                            if (i_cmb .le. 0) then
                              r_sbuff(iss,ill) = r_sbuff(iss,ill)+r_scale(iss,ill)
                              r_obufa(iss,ill) = r_obufa(iss,ill)+r_scale(iss,ill)*r_aa
                              r_obufb(iss,ill) = r_obufb(iss,ill)+r_scale(iss,ill)*r_bb
                              r_obufc(iss,ill) = r_obufc(iss,ill)+r_scale(iss,ill)*r_bb**2
                            else if (i_cmb .eq. 1) then
                              if (r_sbuff(iss,ill) .gt. 0.) r_obufa(iss,ill) = r_obufa(iss,ill)/r_sbuff(iss,ill)
                              r_sbuff(iss,ill) = r_sbuff(iss,ill)+r_scale(iss,ill)
                              if (r_obufa(iss,ill) .lt. r_aa) r_obufa(iss,ill) = r_aa
                              r_obufa(iss,ill) = r_scale(iss,ill)*r_obufa(iss,ill)     
                              r_obufb(iss,ill) = r_obufb(iss,ill)+r_scale(iss,ill)*r_bb
                              r_obufc(iss,ill) = r_obufc(iss,ill)+r_scale(iss,ill)*r_bb**2
                            else if (i_cmb .eq. 2) then
                              if (r_scale(iss,ill) .gt. r_sbuff(iss,ill)) then
                                r_sbuff(iss,ill) = r_scale(iss,ill)
                                r_obufa(iss,ill) = r_scale(iss,ill)*r_aa     
                                r_obufb(iss,ill) = r_scale(iss,ill)*r_bb
                                r_obufc(iss,ill) = 0
                              endif
                            else
                              stop 'Combination not supported '
                            endif
                            r_scale(iss,ill) = 0.
                          endif
                        endif
                      enddo
                    enddo


                  enddo
                enddo
                if (i_nof .ge. 20) then
                  close(40+i_ffff,err=911)
911               close(60+i_ffff,err=921)
921               close(80+i_ffff,err=931)
931             endif
              endif
            enddo

c
c  Write out block to file
c
            write(6,*) ' Writing block'

            if (a_stdfile(0) .ne. ' ') then
              do il=1,i_olnum
                if (i_bs .gt. 1) then
                  read(29,rec=il+i_oloff) (r_data(is),is=1,i_ssize(0)),(r_datb(iss),iss=1,i_ssize(0))
                endif
                do is=1,i_osnum
                  if (r_sbuff(is,il) .ne. 0.) then
                    r_data(is+i_osoff) = r_obufa(is,il)/r_sbuff(is,il)
                    r_datb(is+i_osoff) = 
     &                 sqrt(abs(r_obufc(is,il)/r_sbuff(is,il) - (r_obufb(is,il)/r_sbuff(is,il))**2))
                  else
                    r_data(is+i_osoff) = 0.
                    r_datb(is+i_osoff) = 0.
                  endif
                enddo
                write(29,rec=il+i_oloff) (r_data(is),is=1,i_ssize(0)),(r_datb(iss),iss=1,i_ssize(0))
              enddo
            endif

            if (a_errfile(0) .ne. ' ') then
              do il=1,i_olnum
                if (i_bs .gt. 1) then
                  read(28,rec=il+i_oloff) (r_data(is),is=1,i_ssize(0)),(r_datb(iss),iss=1,i_ssize(0))
                endif
                do is=1,i_osnum
                  if (r_sbuff(is,il) .ne. 0.) then
                    r_data(is+i_osoff) = r_obufa(is,il)/r_sbuff(is,il)
                    r_datb(is+i_osoff) = 
     &                 sqrt(abs(r_obufc(is,il)/r_sbuff(is,il) - (r_obufb(is,il)/r_sbuff(is,il))**2))
                  else
                    r_data(is+i_osoff) = 0.
                    r_datb(is+i_osoff) = 0.
                  endif
                enddo
                write(28,rec=il+i_oloff) (r_data(is),is=1,i_ssize(0)),(r_datb(iss),iss=1,i_ssize(0))
              enddo
            endif

            if (a_rmgfile(0) .ne. ' ') then
              do il=1,i_olnum
                if (i_bs .gt. 1 .or. i_nof .eq. 0) then
                  read(30,rec=il+i_oloff) (r_data(is),is=1,i_ssize(0)),(r_datb(iss),iss=1,i_ssize(0))
                endif
                do is=1,i_osnum
                  if (r_sbuff(is,il) .ne. 0.) then
                    r_data(is+i_osoff) = r_obufa(is,il)/r_sbuff(is,il)
                    r_datb(is+i_osoff) = r_obufb(is,il)/r_sbuff(is,il)
                  else
                    r_data(is+i_osoff) = 0.
                    r_datb(is+i_osoff) = 0.
                  endif
                enddo
                write(30,rec=il+i_oloff) (r_data(is),is=1,i_ssize(0)),(r_datb(iss),iss=1,i_ssize(0))
              enddo
            endif

            if (a_magfile(0) .ne. ' ') then
              do il=1,i_olnum
                if (i_bs .gt. 1 .or. i_nof .eq. 0) then
                  if (i_mbytes(0) .eq. 1) then
                    read(31,rec=il+i_oloff) (b_data1(is),is=1,i_ssize(0))
                  else if (i_mbytes(0) .eq. 2) then
                    read(31,rec=il+i_oloff) (i_data2(is),is=1,i_ssize(0))
                  else
                    read(31,rec=il+i_oloff) (r_data(is),is=1,i_ssize(0))
                  endif
                endif
                if (i_mbytes(0) .eq. 1) then
                  do is=1,i_osnum
                    if (r_sbuff(is,il) .ne. 0.) then
                      r_data(is+i_osoff) = (r_obufa(is,il)/r_sbuff(is,il) -  r_mdnc(2,0))/r_mdnc(1,0)
                      if (int(r_data(is+i_osoff)) .lt. 128)  then         
                        b_data1(is+i_osoff) = max(int(r_data(is+i_osoff)),0)
                      else
                        b_data1(is+i_osoff) = min(int(r_data(is+i_osoff))-256,-1)
                      endif
                    else
                      b_data1(is+i_osoff) = 0
                    endif
                  enddo
                  write(31,rec=il+i_oloff) (b_data1(is),is=1,i_ssize(0))
                else if (i_mbytes(0) .eq. 2) then
                  do is=1,i_osnum
                    if (r_sbuff(is,il) .ne. 0.) then
                      r_data(is+i_osoff) = (r_obufa(is,il)/r_sbuff(is,il) -  r_mdnc(2,0))/r_mdnc(1,0)             
                      if (int(r_data(is+i_osoff)) .lt. 32768)  then         
                        i_data2(is+i_osoff) = max(int(r_data(is+i_osoff)),0)
                      else
                        i_data2(is+i_osoff) = min(int(r_data(is+i_osoff))-65536,-1)
                      endif
                    else
                      i_data2(is+i_osoff) = 0
                    endif
                  enddo
                  write(31,rec=il+i_oloff) (i_data2(is),is=1,i_ssize(0))
                else
                  do is=1,i_osnum
                    if (r_sbuff(is,il) .ne. 0.) then
                      r_data(is+i_osoff) = r_obufa(is,il)/r_sbuff(is,il)
                    else
                      r_data(is+i_osoff) = 0.
                    endif
                  enddo
                  write(31,rec=il+i_oloff) (r_data(is),is=1,i_ssize(0))
                endif
              enddo
            endif

            if (a_dtefile(0) .ne. ' ') then
              do il=1,i_olnum
                if (i_bs .gt. 1 .or. i_nof .eq. 0) then
                  if (i_dbytes(0) .eq. 1) then
                    read(32,rec=il+i_oloff) (b_datb1(is),is=1,i_ssize(0))
                  else if (i_dbytes(0) .eq. 2) then
                    read(32,rec=il+i_oloff) (i_datb2(is),is=1,i_ssize(0))
                  else
                    read(32,rec=il+i_oloff) (r_datb(is),is=1,i_ssize(0))
                  endif
                endif
                if (i_dbytes(0) .eq. 1) then
                  do is=1,i_osnum
                    if (r_sbuff(is,il) .ne. 0.) then
                      r_datb(is+i_osoff) = (r_obufb(is,il)/r_sbuff(is,il) -  r_ddnc(2,0))/r_ddnc(1,0)             
                      if (int(r_datb(is+i_osoff)) .lt. 128)  then         
                        b_datb1(is+i_osoff) = max(int(r_datb(is+i_osoff)),0)
                      else
                        b_datb1(is+i_osoff) = min(int(r_datb(is+i_osoff))-256,-1)
                      endif
                    else
                      b_datb1(is+i_osoff) = 0
                    endif
                  enddo
                  write(32,rec=il+i_oloff) (b_datb1(is),is=1,i_ssize(0))
                else if (i_dbytes(0) .eq. 2) then
                  do is=1,i_osnum
                    if (r_sbuff(is,il) .ne. 0.) then
                      r_datb(is+i_osoff) = (r_obufb(is,il)/r_sbuff(is,il) -  r_ddnc(2,0))/r_ddnc(1,0)             
                      if (int(r_datb(is+i_osoff)) .lt. 32768)  then         
                        i_datb2(is+i_osoff) = max(int(r_datb(is+i_osoff)),0)
                      else
                        i_datb2(is+i_osoff) = min(int(r_datb(is+i_osoff))-65536,-1)
                      endif
                    else
                      i_datb2(is+i_osoff) = 0
                    endif
                  enddo
                  write(32,rec=il+i_oloff) (i_datb2(is),is=1,i_ssize(0))
                else
                  do is=1,i_osnum
                    if (r_sbuff(is,il) .ne. 0.) then
                      r_datb(is+i_osoff) = r_obufb(is,il)/r_sbuff(is,il)
                    else
                      r_datb(is+i_osoff) = 0.
                    endif
                  enddo
                  write(32,rec=il+i_oloff) (r_datb(is),is=1,i_ssize(0))
                endif
              enddo
            endif

          enddo
        enddo

c
c  Close data files
c
        if (i_nof .lt. 20) then
          do i_file=1,i_nof
            close(40+i_file,err=910)
910         close(60+i_file,err=920)
920         close(80+i_file,err=930)
930       enddo
        endif

c 
c  Overlay GCP data on Mosaic
c 
        do i_file = 1,i_nog
          open(unit=27,file=a_gcpfile(i_file),status='old',form='formatted')
          write(6,*) 'Overlaying GCPs from: ',a_gcpfile(i_file)(1:length(a_gcpfile(i_file))),i_fills(i_file)
          do i=1,100000
            if (mod(i,100) .eq. 0) write(6,*) '  at point: ',i
            read(27,*,err=935) r_lat,r_lon,r_hgt
            r_lat=r_lat/r_rtod
            r_lon=r_lon/r_rtod
            call latlon(r_a,r_e2,v_iloc2,r_lat,r_lon,r_hgt,1)

            call vecmulti(r_inv(1,1),v_iloc2,v_iloc3) ! convert from input xyz to output xyz
            call vecaddit(r_inv(1,4),v_iloc3,v_iloc3)

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
                  else if (i_type(0) .eq. 4 .or. i_type(0) .eq. -4) then                ! Convert output from xyz to utm
                    call latlon(r_a,r_e2,v_iloc3,r_lat,r_lon,r_hgt,2)
                    call utmtoll(r_a,r_e2,i_zone(0),a_grid,v_oloc,r_lat,r_lon,2)
                    v_oloc(3) = r_hgt
                  else if (i_type(0) .eq. 5) then                ! Convert output from xyz to ll
                    call latlon(r_a,r_e2,v_iloc3,r_lat,r_lon,r_hgt,2)
                    v_oloc(1) = r_lat*r_rtod
                    v_oloc(2) = r_lon*r_rtod
                    v_oloc(3) = r_hgt
                  endif

                  il = int((v_oloc(1) - r_str(1,0))/r_spc(1,0))
                  is = int((v_oloc(2) - r_str(2,0))/r_spc(2,0))

                  do ii=-(i_fills(i_file)-1),(i_fills(i_file)-1)
 	                if (ii+il .ge. 1 .and. ii+il .le. i_lsize(0)) then

                      if (a_rmgfile(0) .ne. ' ') then
                        read(30,rec=il+ii) (r_data(isss),isss=1,i_ssize(0)),(r_datb(iss),iss=1,i_ssize(0))
                        do jj=-(i_fills(i_file)-1),(i_fills(i_file)-1)
                          if (jj+is .ge. 1 .and. jj+is .le. i_ssize(0)) then
                            if (abs(jj) .le. i_fills(i_file)-abs(ii)-1) then
                              r_data(jj+is) = i_fillv(i_file)
                            end if
	                        if (ii.eq.0.and.jj.eq.0) then
                              r_data(jj+is) = i_fillv(i_file)
                              r_datb(jj+is) = r_hgt
                            end if
                          end if
                        end do
                        write(30,rec=ii+il) (r_data(isss),isss=1,i_ssize(0)),(r_datb(iss),iss=1,i_ssize(0))
                      end if
                    end if

                    if (a_magfile(0) .ne. ' ') then
                      if (i_mbytes(0) .eq. 1) then
                        read(31,rec=ii+il) (b_data1(iss),iss=1,i_ssize(0))
                        do jj=-(i_fills(i_file)-1),(i_fills(i_file)-1)
                          if (jj+is .ge. 1 .and. jj+is .le. i_ssize(0)) then
                            if (abs(jj) .gt. i_fills(i_file)-abs(ii)-1) then
                              b_data1(jj+is) = -2
                            end if
	                        if (ii.eq.0.and.jj.eq.0) then
                              b_data1(jj+is) = -1
                            end if
                          end if
                        end do
                        write(31,rec=ii+il) (b_data1(iss),iss=1,i_ssize(0))
                      else if (i_mbytes(0) .eq. 2) then
                        read(31,rec=il+ii) (i_data2(iss),iss=1,i_ssize(0))
                        do jj=-(i_fills(i_file)-1),(i_fills(i_file)-1)
                          if (jj+is .ge. 1 .and. jj+is .le. i_ssize(0)) then
                            if (abs(jj) .gt. i_fills(i_file)-abs(ii)-1) then
                              i_data2(jj+is) = i_fillv(i_file)
                            end if
	                        if (ii.eq.0.and.jj.eq.0) then
                              i_data2(jj+is) = i_fillv(i_file)
                            end if
                          end if
                        end do
                        write(31,rec=ii+il) (i_data2(iss),iss=1,i_ssize(0))
                      else 
                        read(31,rec=ii+il) (r_data(iss),iss=1,i_ssize(0))
                        do jj=-(i_fills(i_file)-1),(i_fills(i_file)-1)
                          if (jj+is .ge. 1 .and. jj+is .le. i_ssize(0)) then
                            if (abs(jj) .gt. i_fills(i_file)-abs(ii)-1) then
                              r_data(jj+is) = i_fillv(i_file)
                            end if
	                        if (ii.eq.0.and.jj.eq.0) then
                              r_data(jj+is) = i_fillv(i_file)
                            end if
                          end if
                        end do
                        write(31,rec=ii+il) (r_data(iss),iss=1,i_ssize(0))
                      end if
                    end if

                    if (a_dtefile(0) .ne. ' ') then
	                  if (ii.eq.0 .and. is .ge. 1 .and. is .le. i_ssize(0)) then
                        if (i_dbytes(0) .eq. 1) then
                          read(32,rec=il) (b_datb1(iss),iss=1,i_ssize(0))
                          r_datb(is) = (r_hgt -  r_ddnc(2,0))/r_ddnc(1,0)
                          if (int(r_datb(is)) .lt. 128)  then         
                            b_datb1(is) = max(int(r_datb(is)),0)
                          else
                            b_datb1(is) = min(int(r_datb(is))-256,-1)
                          end if
                          write(32,rec=il) (b_datb1(iss),iss=1,i_ssize(0))
                        else if (i_dbytes(0) .eq. 2) then
                          read(32,rec=il) (i_datb2(iss),iss=1,i_ssize(0))
                          r_datb(is) = (r_hgt -  r_ddnc(2,0))/r_ddnc(1,0)
                          if (int(r_datb(is)) .lt. 2**15)  then         
                            i_datb2(is) = max(int(r_datb(is)),0)
                          else
                            i_datb2(is) = min(int(r_datb(is))-2**16,-1)
                          end if
                          write(32,rec=il) (i_datb2(iss),iss=1,i_ssize(0))
                        else 
                          read(32,rec=il) (r_datb(iss),iss=1,i_ssize(0))
                          r_datb(is) = r_hgt
                          write(32,rec=il) (r_datb(iss),iss=1,i_ssize(0))
                        end if
                      end if
                    end if
                  end do  ! loop over lines in symbol
          end do  ! loop over points in GCP file
935     end do  ! loop over GCP files

        close(30,err=940)
940     close(31,err=950)
950     close(32,err=960)
960     close(29,err=970)

970     tt2 = etime(tt2a)          !@#$% Machine Dependent Code
        write(6,*) ' '
        write(6,*) 'Total run time'
        write(6,*) '   cputime: ',tt2a(1)-ttza(1)
        write(6,*) '   systime: ',tt2a(2)-ttza(2)
        write(6,*) ' '
        write(6,*) ' '

        stop 'Done'
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
                    endif
                    i_cnt = i_cnt+1
                    a_vals(i_vals)(i_cnt:i_cnt) = a_string(i:i)
                  endif
                else 
                  i_on = 0
                  i_cnt = 0
                endif
              else
                i_cmnt = 1
              endif
            enddo
          endif
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
            print *,'Determinant =  0 in Subroutine matinvrt'
            r_b(1,1)=1.
            r_b(1,2)=0.
            r_b(1,3)=0.
            r_b(2,1)=0.
            r_b(2,2)=1.
            r_b(2,3)=0.
            r_b(3,1)=0.
            r_b(3,2)=0.
            r_b(3,3)=1.
          endif

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
              endif
            else

            endif
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
              endif
            endif
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

              endif
            endif
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

              endif

            endif
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

c                print *,'r_c = 0',(v_pln(1,1)-v_pln(1,2)),(v_pln(2,3)-v_pln(2,2)) , 
c     &              (v_pln(2,1)-v_pln(2,2)),(v_pln(1,3)-v_pln(1,2))
c                print *,'       ',v_pln(1,1),v_pln(1,2),v_pln(2,3),v_pln(2,2),v_pln(2,1),v_pln(1,3)

              endif

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

                endif

              endif
            endif

600       continue

          if (v_pnt(4) .le. 0.) then 
            i_flg = 0
            v_pnt(4) = 0.
          endif

        return
        end

cccc


        subroutine nearestpp(v_pln,i_pln,v_pnt,i_flg)

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
          real*8 r_crs14
          real*8 r_crs24
          real*8 r_crs23
          real*8 r_crs43
          real*8 r_crs41
          real*8 r_crs31
          real*8 r_crs32

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

          if (i_try(i_pln(1),i_pln(2),i_pln(3),i_pln(4)) .eq. 6 ) return

          if ( i_try(i_pln(1),i_pln(2),i_pln(3),i_pln(4)) .eq. 1 ) then
            v_t2(1) =  v_pnt(1) - v_pln(1,2)
            v_t2(2) =  v_pnt(2) - v_pln(2,2)
            v_t3(1) =  v_pnt(1) - v_pln(1,3)
            v_t3(2) =  v_pnt(2) - v_pln(2,3)
            v_t4(1) =  v_pnt(1) - v_pln(1,4)
            v_t4(2) =  v_pnt(2) - v_pln(2,4)

            r_crs24 = v_t2(1)*v_t4(2) - v_t2(2)*v_t4(1)
            r_crs43 = v_t4(1)*v_t3(2) - v_t4(2)*v_t3(1)
            r_crs32 = v_t3(1)*v_t2(2) - v_t3(2)*v_t2(1)

            if ( abs(sign(1.d0,r_crs24)+sign(1.d0,r_crs43)+sign(1.d0,r_crs32)) .gt. 3.9d0) then
              r_b = (v_t2(1)**2 + v_t2(2)**2)
              r_c = (v_t3(1)**2 + v_t3(2)**2)
              r_d = (v_t4(1)**2 + v_t4(2)**2)

              i_flg=1

              if (r_b .le. min(r_c,r_d) ) then
                  v_pnt(3) = v_pln(3,2)
                  v_pnt(4) = v_pln(4,2)
              else if (r_c .le. min(r_d,r_b) ) then
                  v_pnt(3) = v_pln(3,3)
                  v_pnt(4) = v_pln(4,3)
              else if (r_d .le. min(r_b,r_c) ) then
                  v_pnt(3) = v_pln(3,4)
                  v_pnt(4) = v_pln(4,4)
              endif
            endif
            return
          else if (i_try(i_pln(1),i_pln(2),i_pln(3),i_pln(4)) .eq. 2 ) then
            v_t1(1) =  v_pnt(1) - v_pln(1,1)
            v_t1(2) =  v_pnt(2) - v_pln(2,1)
            v_t3(1) =  v_pnt(1) - v_pln(1,3)
            v_t3(2) =  v_pnt(2) - v_pln(2,3)
            v_t4(1) =  v_pnt(1) - v_pln(1,4)
            v_t4(2) =  v_pnt(2) - v_pln(2,4)

            r_crs14 = v_t1(1)*v_t4(2) - v_t1(2)*v_t4(1)
            r_crs43 = v_t4(1)*v_t3(2) - v_t4(2)*v_t3(1)
            r_crs31 = v_t3(1)*v_t1(2) - v_t3(2)*v_t1(1)

            if ( abs(sign(1.d0,r_crs14)+sign(1.d0,r_crs43)+sign(1.d0,r_crs31)) .gt. 2.9d0) then
              r_a = (v_t1(1)**2 + v_t1(2)**2)
              r_c = (v_t3(1)**2 + v_t3(2)**2)
              r_d = (v_t4(1)**2 + v_t4(2)**2)

              i_flg=1

              if (r_a .le. min(r_c,r_d) ) then
                  v_pnt(3) = v_pln(3,1)
                  v_pnt(4) = v_pln(4,1)
              else if (r_c .le. min(r_d,r_a) ) then
                  v_pnt(3) = v_pln(3,3)
                  v_pnt(4) = v_pln(4,3)
              else if (r_d .le. min(r_a,r_c) ) then
                  v_pnt(3) = v_pln(3,4)
                  v_pnt(4) = v_pln(4,4)
              endif
            endif
            return
          else if (i_try(i_pln(1),i_pln(2),i_pln(3),i_pln(4)) .eq. 3 ) then
            v_t1(1) =  v_pnt(1) - v_pln(1,1)
            v_t1(2) =  v_pnt(2) - v_pln(2,1)
            v_t2(1) =  v_pnt(1) - v_pln(1,2)
            v_t2(2) =  v_pnt(2) - v_pln(2,2)
            v_t4(1) =  v_pnt(1) - v_pln(1,4)
            v_t4(2) =  v_pnt(2) - v_pln(2,4)

            r_crs12 = v_t1(1)*v_t2(2) - v_t1(2)*v_t2(1)
            r_crs24 = v_t2(1)*v_t4(2) - v_t2(2)*v_t4(1)
            r_crs41 = v_t4(1)*v_t1(2) - v_t4(2)*v_t1(1)

            if ( abs(sign(1.d0,r_crs12)+sign(1.d0,r_crs24)+sign(1.d0,r_crs41)) .gt. 2.9d0) then
              r_a = (v_t1(1)**2 + v_t1(2)**2)
              r_b = (v_t2(1)**2 + v_t2(2)**2)
              r_d = (v_t4(1)**2 + v_t4(2)**2)

              i_flg=1

              if (r_b .le. min(r_a,r_d) ) then
                  v_pnt(3) = v_pln(3,2)
                  v_pnt(4) = v_pln(4,2)
              else if (r_a .le. min(r_d,r_b) ) then
                  v_pnt(3) = v_pln(3,1)
                  v_pnt(4) = v_pln(4,1)
              else if (r_d .le. min(r_b,r_a) ) then
                  v_pnt(3) = v_pln(3,4)
                  v_pnt(4) = v_pln(4,4)
              endif
            endif
            return
          else if (i_try(i_pln(1),i_pln(2),i_pln(3),i_pln(4)) .eq. 4 ) then
            v_t1(1) =  v_pnt(1) - v_pln(1,1)
            v_t1(2) =  v_pnt(2) - v_pln(2,1)
            v_t2(1) =  v_pnt(1) - v_pln(1,2)
            v_t2(2) =  v_pnt(2) - v_pln(2,2)
            v_t3(1) =  v_pnt(1) - v_pln(1,3)
            v_t3(2) =  v_pnt(2) - v_pln(2,3)

            r_crs12 = v_t1(1)*v_t2(2) - v_t1(2)*v_t2(1)
            r_crs23 = v_t2(1)*v_t3(2) - v_t2(2)*v_t3(1)
            r_crs31 = v_t3(1)*v_t1(2) - v_t3(2)*v_t1(1)

            if ( abs(sign(1.d0,r_crs12)+sign(1.d0,r_crs23)+sign(1.d0,r_crs31)) .gt. 2.9d0) then
              r_a = (v_t1(1)**2 + v_t1(2)**2)
              r_b = (v_t2(1)**2 + v_t2(2)**2)
              r_c = (v_t3(1)**2 + v_t3(2)**2)

              i_flg=1

              if (r_b .le. min(r_c,r_a) ) then
                  v_pnt(3) = v_pln(3,2)
                  v_pnt(4) = v_pln(4,2)
              else if (r_c .le. min(r_a,r_b) ) then
                  v_pnt(3) = v_pln(3,3)
                  v_pnt(4) = v_pln(4,3)
              else if (r_a .le. min(r_b,r_c) ) then
                  v_pnt(3) = v_pln(3,1)
                  v_pnt(4) = v_pln(4,1)
              endif
            endif
            return
          else if (i_try(i_pln(1),i_pln(2),i_pln(3),i_pln(4)) .eq. 5 ) then
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
            r_crs43 = v_t4(1)*v_t3(2) - v_t4(2)*v_t3(1)
            r_crs31 = v_t3(1)*v_t1(2) - v_t3(2)*v_t1(1)

            if ( abs(sign(1.d0,r_crs12)+sign(1.d0,r_crs24)+sign(1.d0,r_crs43)+sign(1.d0,r_crs31)) .gt. 3.9) then
              r_a = (v_t1(1)**2 + v_t1(2)**2)
              r_b = (v_t2(1)**2 + v_t2(2)**2)
              r_c = (v_t3(1)**2 + v_t3(2)**2)
              r_d = (v_t4(1)**2 + v_t4(2)**2)

              i_flg=1

              if (r_a .le. min(min(r_b,r_c),r_d) ) then
                  v_pnt(3) = v_pln(3,1)
                  v_pnt(4) = v_pln(4,1)
              else if (r_b .le. min(min(r_c,r_d),r_a) ) then
                  v_pnt(3) = v_pln(3,2)
                  v_pnt(4) = v_pln(4,2)
              else if (r_c .le. min(min(r_d,r_b),r_a) ) then
                  v_pnt(3) = v_pln(3,3)
                  v_pnt(4) = v_pln(4,3)
              else if (r_d .le. min(min(r_b,r_c),r_a) ) then
                  v_pnt(3) = v_pln(3,4)
                  v_pnt(4) = v_pln(4,4)
              endif
            endif
            return
          endif

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
           endif
           
           r_lon = atan2(r_v(2),r_v(1))
           
           r_p = sqrt(r_v(1)**2 + r_v(2)**2)
           r_tant = (r_v(3)/r_p)*r_q
           r_theta = atan(r_tant)
           r_tant = (r_v(3) + r_q3*r_b*sin(r_theta)**3)/
     +          (r_p - r_e2*r_a*cos(r_theta)**3)
           r_lat =  atan(r_tant)
           r_re = r_a/sqrt(1.d0 - r_e2*sin(r_lat)**2)
           r_hgt = r_p/cos(r_lat) - r_re          
  
        endif
      
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
  
        endif
      
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
c        data r_fe,r_fn /500000.d0,0.d0,10000000.d0/
        data r_fe,r_fn /500000.d0,0.d0, 0.d0/

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
           endif

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
           endif
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

        endif
      
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
c        data r_fe,r_fn /500000.d0,0.d0,10000000.d0/
        data r_fe,r_fn /500000.d0,0.d0,0.d0/

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
           endif

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
           endif
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

        endif
      
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
          enddo

          length=i_len
          return
        end


c****************************************************************
        subroutine read_hdr(a_hdrfile,a_type,i_lsize,i_ssize,r_peg,i_zone,
     &              r_str,r_spc,i_mbytes,i_dbytes,i_moff,i_doff,
     &              r_mdnc,r_ddnc,i_err,i_set)

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
        integer*4 i_set(0:11)
        integer*4 i_lsize
        integer*4 i_ssize
        integer*4 i_zone

        integer*4 i_mbytes
        integer*4 i_dbytes

        integer*4 i_moff(2)
        integer*4 i_doff(2)

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

c	DATA STATEMENTS: none

c	FUNCTION STATEMENTS:  

        integer length
        external length

c  	PROCESSING STEPS:
        
c
c  Initialize pi and conversions
c
        r_pi = 4.d0*atan(1.0d0)
        r_rtod = 180.0d0/r_pi

        i_err = 1
        i_cnt = 0

        open(12,file=a_hdrfile,status='old',form='formatted',err=900)
        write(6,*) ' '
        write(6,*) 'Opening hdr input  file: ',a_hdrfile(1:52)
        i_err = 0

        do i=1,10000000
           read(12,'(a)',end=900) a_tmp
           if (a_tmp .eq. ' ') then
             ! do nothing
           else if (index(a_tmp,'Data file type') .gt. 0) then
             if (i_set(0) .eq. 0) then
               a_type = a_tmp(1:max(1,index(a_tmp,';')-1))
               do j=1,length(a_type)
                 if (a_type(1:1) .eq. ' ') a_type = a_type(2:)
               enddo
               i_set(0) = 1
             endif
           else if (index(a_tmp,'Data file dimensions') .gt. 0) then
             if (i_set(4) .eq. 0) then
               read(a_tmp,*) i_lsize,i_ssize
               i_cnt = i_cnt + 1
               i_set(4) = 1
             endif
           else if (index(a_tmp,'Post Spacing') .gt. 0) then
             if (i_set(3) .eq. 0) then
               read(a_tmp,*) r_spc
               i_cnt = i_cnt + 2
               i_set(3) = 1
             endif
           else if (index(a_tmp,'Peg position (WGS-84)') .gt. 0) then
             if (i_set(1) .eq. 0) then
               read(a_tmp,*) r_peg
               r_peg(1) = r_peg(1)/r_rtod
               r_peg(2) = r_peg(2)/r_rtod
               r_peg(3) = r_peg(3)/r_rtod
               i_cnt = i_cnt + 4
               i_set(1) = 1
             endif
           else if (index(a_tmp,'Zone') .gt. 0) then
             if (i_set(5) .eq. 0) then
               read(a_tmp,*) i_zone
               i_set(5) = 1
             endif
           else if (index(a_tmp,'Starting corner position') .gt. 0) then
             if (i_set(2) .eq. 0) then
               read(a_tmp,*) r_str
               i_cnt = i_cnt + 8
               i_set(2) = 1
             endif
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
             if (i_set(8) .eq. 0) then
               read(a_tmp,*) r_mdnc
               i_set(8) = 1
             endif
           else if (index(a_tmp,'Elevation Scale and Shift') .gt. 0) then
             if (i_set(9) .eq. 0) then
               read(a_tmp,*) r_ddnc
               i_set(9) = 1
             endif
           else if (index(a_tmp,'Magnitude Bytes per Pixel') .gt. 0) then
             if (i_set(6) .eq. 0) then
               read(a_tmp,*) i_mbytes
               i_set(6) = 1
             endif
           else if (index(a_tmp,'Magnitude Bytes per pixel') .gt. 0) then
             if (i_set(6) .eq. 0) then
               read(a_tmp,*) i_mbytes
               i_set(6) = 1
             endif
           else if (index(a_tmp,'Elevation Bytes per Pixel') .gt. 0) then
             if (i_set(7) .eq. 0) then
               read(a_tmp,*) i_dbytes
               i_set(7) = 1
             endif
           else if (index(a_tmp,'Elevation Bytes per pixel') .gt. 0) then
             if (i_set(7) .eq. 0) then
               read(a_tmp,*) i_dbytes
               i_set(7) = 1
             endif
           else if (index(a_tmp,'Magnitude File Header Lines (mhl)') .gt. 0) then
             if (i_set(8) .eq. 0) then
               read(a_tmp,*) i_moff(1)
               i_set(8) = 1
             endif
           else if (index(a_tmp,'Magnitude File Header Samples (mhs)') .gt. 0) then
             if (i_set(9) .eq. 0) then
               read(a_tmp,*) i_moff(2)
               i_set(9) = 1
             endif
           else if (index(a_tmp,'Elevation File Header Lines (dhl)') .gt. 0) then
             if (i_set(8) .eq. 0) then
               read(a_tmp,*) i_doff(1)
               i_set(10) = 1
             endif
           else if (index(a_tmp,'Elevation File Header Samples (dhs)') .gt. 0) then
             if (i_set(9) .eq. 0) then
               read(a_tmp,*) i_doff(2)
               i_set(11) = 1
             endif
           endif
        enddo
        close(12)
        stop 'Error reading header file, too many lines'

900     close(12,err=910)
910     continue !  i_err = 0 if file even exists  if (i_cnt .eq. 15) i_err = 0 
        return
      end

****************************************************************
        subroutine write_hdr(a_hdrfile,a_filter,a_type,i_lsize,i_ssize,r_peg,
     &      r_str,r_spc,i_zone,i_mbytes,i_dbytes,r_mdnc,r_ddnc,i_err)

c****************************************************************
c**
c**	FILE NAME: read_hdr.f
c**
c**     DATE WRITTEN: 8/26/96
c**
c**     PROGRAMMER:Scott Shaffer
c**
c** 	FUNCTIONAL DESCRIPTION: Writes some of an IFPROC header file.
c**
c**     ROUTINES CALLED:none
c**  
c**     NOTES: 
c**
c**
c*****************************************************************

       	implicit none

c	INPUT VARIABLES:
        
        character*(*) a_hdrfile
        character*(*) a_filter         !header file
        character*(*) a_type

c	OUTPUT VARIABLES: 

        integer*4 i_err
        integer*4 i_lsize
        integer*4 i_ssize
        integer*4 i_zone
        integer*4 i_mbytes
        integer*4 i_dbytes

        real*8 r_mdnc(2)
        real*8 r_ddnc(2)

        real*8 r_p(3)
        real*8 r_peg(3)
        real*8 r_str(2)
        real*8 r_spc(2)


c	LOCAL VARIABLES: 

        character*80  a_input

        integer i_hdrfile

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
           i_hdrfile = 19
           open(i_hdrfile,file=a_hdrfile,status='old',form='formatted',err=800)
           close(i_hdrfile)
           write(6,*) ' '
           write(6,'(x,a,$)') 'Output Header File Exists.  Overwrite (y/n)? '
           read(5,'(a)') a_input
           write(6,*) ' '
           if (a_input .ne. 'y' .and. a_input .ne. 'yes' .and.
     &         a_input .ne. 'Y' .and. a_input .ne. 'YES' ) return
800        open(i_hdrfile,file=a_hdrfile,status='unknown',form='formatted',err=900)

           write(6,*) 'Writing hdr output file: ',a_hdrfile(:max(length(a_hdrfile),1))

           write(i_hdrfile,'(a45,3a)') a_type(:max(length(a_type),1)),' ; ',
     &               a_filter(:length(a_filter)),' Data file type '
           write(i_hdrfile,'(2i15,  15x,3a)') i_lsize,i_ssize,' ; ',a_filter(:length(a_filter)),
     &               ' Data file dimensions '

           if (a_type .eq. 'UTM' .or. a_type .eq. 'utm') r_spc(1) = -r_spc(1)   !  Flip for printout, then flip back
           if (a_type .eq. 'SRH' .or. a_type .eq. 'srh') then
             r_spc(2) = -r_spc(2)
             r_str(2) = -r_str(2)
           endif
           if (r_spc(1) .ge. 1.) then
             write(i_hdrfile,'(2f15.3,15x,3a)') r_spc,' ; ',a_filter(:length(a_filter)),
     &               ' Post Spacing'
           else
             write(i_hdrfile,'(2f15.8,15x,3a)') r_spc,' ; ',a_filter(:length(a_filter)),
     &               ' Post Spacing'
           endif
           if (a_type .eq. 'UTM' .or. a_type .eq. 'utm') r_spc(1) = -r_spc(1)   !  Flip for printout, then flip back
           if (a_type .eq. 'SRH' .or. a_type .eq. 'srh') then
             r_spc(2) = -r_spc(2)
             r_str(2) = -r_str(2)
           endif

           if (a_type .eq. 'EQA' .or. a_type .eq. 'eqa') then
             write(i_hdrfile,'(2f15.6,15x,3a)') r_str,' ; ',a_filter(:length(a_filter)),
     &               ' Starting corner position (s,c)'
           else
             write(i_hdrfile,'(2f15.2,15x,3a)') r_str,' ; ',a_filter(:length(a_filter)),
     &               ' Starting corner position (s,c)'
           endif
           r_p(1) = r_peg(1)*r_rtod
           r_p(2) = r_peg(2)*r_rtod
           r_p(3) = r_peg(3)*r_rtod
           write(i_hdrfile,'(3f15.7,3a)')  r_p, ' ; ',a_filter(:length(a_filter)),
     &               ' Peg position (WGS-84)'
           if (a_type .eq. 'neu' .or. a_type .eq. 'enu' .or. a_type .eq. 'utm' .or.
     &         a_type .eq. 'NEU' .or. a_type .eq. 'ENU' .or. a_type .eq. 'UTM' ) then 
             write(i_hdrfile,'(i15,30x,3a)') i_zone,' ; ',a_filter(:length(a_filter)),' UTM Zone '
           endif
           if (i_mbytes .ne. 4) then
             write(i_hdrfile,'(1i15,  30x,3a)') i_mbytes,' ; ',a_filter(:length(a_filter)),
     *         'Magnitude Bytes per Pixel'
             write(i_hdrfile,'(2f15.6,15x,3a)') r_mdnc,  ' ; ',a_filter(:length(a_filter)),
     *         'Magnitude Scale and Shift'
           endif
           if (i_dbytes .ne. 4) then
             write(i_hdrfile,'(1i15,  30x,3a)') i_dbytes,' ; ',a_filter(:length(a_filter)),
     *         'Elevation Bytes per Pixel'
             write(i_hdrfile,'(2f15.6,15x,3a)') r_ddnc,  ' ; ',a_filter(:length(a_filter)),
     *         'Elevation Scale and Shift'
           endif
           close(i_hdrfile)
           i_err=0
        return
900       write(6,*) ' '
          write(6,*) 'Error - Cant create new hdr file: ',a_hdrfile(1:max(length(a_hdrfile),1))
        return
      end

      subroutine read_mhdr(a_magfile,i_mlsize,i_mssize,i_mbytes,i_moff)

        implicit none

        character*(*) a_magfile

        integer i
        integer i_unit
        integer i_mlsize
        integer i_mssize
        integer i_mbsize
        integer i_mbytes

        integer i_moff(2)
        integer i_boff

        character*50 a_string(100)
        byte b_string(5000)

        equivalence(a_string,b_string)

          i_mlsize = 0
          i_mssize = 0

          open(unit=18,file=a_magfile,status='old',form='unformatted',
     &        access='direct',recl=50)

          do i=1,20
            read(18,rec=i,err=901) a_string(i)
            if (a_string(i) .eq. ' ') then
              ! do nothing
            else if (index(a_string(i),'RECORD LENGTH IN BYTES =') .gt. 0) then
              read(a_string(i)(35:),*) i_mbsize
              write(6,*) ' '
              write(6,*) 'Reading airsar magnitude header     ',i_mbsize
            else if (index(a_string(i),'NUMBER OF HEADER RECORDS =') .gt. 0) then
              read(a_string(i)(35:),*) i_moff(1)
            else if (index(a_string(i),'NUMBER OF SAMPLES PER RECORD =') .gt. 0) then
              read(a_string(i)(35:),*) i_mssize
            else if (index(a_string(i),'NUMBER OF LINES IN IMAGE =') .gt. 0) then
              read(a_string(i)(35:),*) i_mlsize
            else if (index(a_string(i),'NUMBER OF BYTES PER SAMPLE =') .gt. 0) then
              read(a_string(i)(35:),*) i_mbytes
            else if (index(a_string(i),'BYTE OFFSET OF FIRST DATA RECORD =') .gt. 0) then
              read(a_string(i)(35:),*) i_boff
            endif
          enddo
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

        integer i_doff(2)
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

          i_dlsize = 0
          i_dssize = 0
          i_demoff = 0
          i_doff(1) = 0
          i_doff(2) = 0

          open(unit=18,file=a_demfile,status='old',form='unformatted',
     &        access='direct',recl=50)

          do i=1,20
            read(18,rec=i,err=901) a_string(i)
            if (a_string(i) .eq. ' ') then
              ! do nothing
            else if (index(a_string(i),'RECORD LENGTH IN BYTES =') .gt. 0) then
              read(a_string(i)(35:),*) i_dbsize
              write(6,*) ' '
              write(6,*) 'Reading airsar elevation header     ',i_dbsize
            else if (index(a_string(i),'NUMBER OF HEADER RECORDS =') .gt. 0) then
              read(a_string(i)(35:),*) i_doff(1)
c              print *,'i_doff=',i_doff
            else if (index(a_string(i),'NUMBER OF SAMPLES PER RECORD =') .gt. 0) then
              read(a_string(i)(35:),*) i_dssize
c              print *,'i_dssize=',i_dssize
            else if (index(a_string(i),'NUMBER OF LINES IN IMAGE =') .gt. 0) then
              read(a_string(i)(35:),*) i_dlsize
c              print *,'i_dlsize=',i_dlsize
            else if (index(a_string(i),'NUMBER OF BYTES PER SAMPLE =') .gt. 0) then
              read(a_string(i)(35:),*) i_dbytes
c              print *,'i_dbytes=',i_dbytes
            else if (index(a_string(i),'BYTE OFFSET OF FIRST DATA RECORD =') .gt. 0) then
              read(a_string(i)(35:),*) i_boff
c              print *,'i_boff=',i_boff
            else if (index(a_string(i),'BYTE OFFSET OF DEM HEADER =') .gt. 0) then
              read(a_string(i)(35:),*) i_demoff
c              print *,'i_demoff=',i_demoff
            endif
          enddo
901       close(18)
              

          if (i_dbsize .gt. 0) then

            open(unit=18,file=a_demfile,status='old',form='unformatted',
     &          access='direct',recl=i_dbsize)

            do i=1,(1100-1)/i_dbsize + 1
              read(18,rec=i+(i_demoff/i_dbsize),err=902) (b_string(j),j=1+(i-1)*i_dbsize,min(i*i_dbsize,5000))
            enddo

            do i=1,21
              if (a_string(i) .eq. ' ') then
                ! do nothing
              else if (index(a_string(i),'X-DIRECTION POST SPACING (M) =') .gt. 0) then
                read(a_string(i)(35:),*,err=990) r_spc(1)
c                print *,'x spacing=',r_spc(1)
              else if (index(a_string(i),'Y-DIRECTION POST SPACING (M) =') .gt. 0) then
                read(a_string(i)(35:),*,err=990) r_spc(2)
c                print *,'y spacing=',r_spc(2)
              else if (index(a_string(i),'ELEVATION INCREMENT (M) =') .gt. 0) then
                read(a_string(i)(35:),*,iostat=i_err) r_dmul
              else if (index(a_string(i),'ELEVATION OFFSET (M) =') .gt. 0) then
                read(a_string(i)(35:),*,iostat=i_err) r_dadd
              else if (index(a_string(i),'LATITUDE OF PEG POINT =') .gt. 0) then
                read(a_string(i)(35:),*,err=990) r_peg(1)
c                print *,'lat=',r_peg(1)
                r_peg(1) = r_peg(1) / r_rtod
              else if (index(a_string(i),'LONGITUDE OF PEG POINT =') .gt. 0) then
                read(a_string(i)(35:),*,err=990) r_peg(2)
c                print *,'lon=',r_peg(2)
                r_peg(2) = r_peg(2) / r_rtod
              else if (index(a_string(i),'HEADING AT PEG POINT (DEGREES) =') .gt. 0) then
                read(a_string(i)(35:),*,err=990) r_peg(3)
c                pring *,'hdg=',r_peg(3)
                r_peg(3) = r_peg(3) / r_rtod
              else if (index(a_string(i),'ALONG-TRACK OFFSET S0 (M) =') .gt. 0) then
                read(a_string(i)(35:),*,err=990) r_str(1)
c                print *,'s0 =',r_str(1)
              else if (index(a_string(i),'ORIGIN OF AZIMUTH (M) =') .gt. 0) then
                read(a_string(i)(35:),*,err=990) r_str(1)
c                print *,'s0 =',r_str(1)
              else if (index(a_string(i),'CROSS-TRACK OFFSET C0 (M) =') .gt. 0) then
                read(a_string(i)(35:),*,err=990) r_str(2)
c                print *,'c0 =',r_str(2)
              else if (index(a_string(i),'ORIGIN OF RANGE (M) =') .gt. 0) then
                read(a_string(i)(35:),*,err=990) r_str(2)
c                print *,'c0 =',r_str(2)
              endif
              if (1.eq.2) then
                write(6,*) ' '
              endif
            enddo
            
              
          endif
902       close(18) 
       return
990      close(18)
         write(6,*) 'Error - ',i,' ',a_string(i)
       return
      end

