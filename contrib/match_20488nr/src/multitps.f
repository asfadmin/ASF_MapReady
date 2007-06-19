c      This program must link to real*8 versions of the following 
c      Numerical Recipies routines (v1992) to work properly.

c      SUBROUTINE gaussj(a,n,np,b,m,mp)
c      SUBROUTINE svdcmp(a,m,n,mp,np,w,v)
c      SUBROUTINE svbksb(u,w,v,m,n,mp,np,b,x)
c      SUBROUTINE svdvar(v,ma,np,w,cvm,ncvm)
c      REAL*8 FUNCTION pythag(a,b)

c***************************************************************

      program multitps

c****************************************************************
c**     
c**   FILE NAME: multitps.f
c**     
c**   DATE WRITTEN: 10/20/96 
c**     
c**   PROGRAMMER: Scott Shaffer
c**     
c**   FUNCTIONAL DESCRIPTION: Converts ground truth info from a 
c**                           variete of sources into the files
c**                           needed by multiaffine3d.
c**
c**   ROUTINES CALLED: 
c**    
c**   NOTES: none
c**     
c**   UPDATE LOG:
c**     
c*****************************************************************

      IMPLICIT NONE

      integer*4 I_MT
      integer*4 I_MS

c     PARAMETER STATEMENTS:
      parameter (I_MT= 20)  				! Max number of ground truth files
      parameter (I_MS= 20000)  				! Max number samples cross track

c     INPUT VARIABLES:
      character*255 a_cmdfile
      character*255 a_outfile
      character*255 a_tpsfile(I_MT)
      character*255 a_magfile(0:I_MT)  ! Data file names
      character*255 a_dtefile(0:I_MT)  ! Data file names
      character*255 a_afffile(0:I_MT)  ! Affine transformation files
      character*255 a_hdrfile(0:I_MT)  ! Header file
      character*255 a_type(0:I_MT)     ! File type
      character*255 a_proj(0:I_MT)
      character*255 a_units(0:I_MT)
      character*255 a_vals(10)
      character*255 a_temp
      character*255 a_input
      character*255 a_filter
      character*1  a_grid              ! UTM identifier

      integer*4 i,j
      integer*4 ii,jj

      integer*4 i_m
      integer*4 i_tp
      integer*4 i_mfc
      integer*4 i_tps
      integer*4 i_mm
      integer*4 i_inarg,iargc
      integer*4 i_file
      integer*4 i_mmm
      integer*4 i_err
      integer*4 i_ll
      integer*4 is
      integer*4 iss
      integer*4 i_ss
      integer*4 i_pos
      integer*4 i_nof                 ! Number of files to mosaic

      integer*4 i_vals
      integer*4 i_ssize(0:I_MT)
      integer*4 i_lsize(0:I_MT)
      integer*4 i_hdrstat(0:I_MT)     ! Header read flag

      integer*4 i_zone(0:I_MT)
      integer*4 i_type(0:I_MT)
      integer*4 i_mbytes(0:I_MT)                      ! Number of bytes per pixel in mag file
      integer*4 i_dbytes(0:I_MT)                      ! Number of bytes per pixel in dte file

      integer*4 i_moff(0:I_MT)
      integer*4 i_doff(0:I_MT)

      integer*4 i_mlsize
      integer*4 i_mssize
      integer*4 i_dlsize
      integer*4 i_dssize


      integer*4 i_pegset(0:I_MT)
      integer*4 i_strset(0:I_MT)
      integer*4 i_spcset(0:I_MT)
      integer*4 i_sizset(0:I_MT)

      integer*4 i_comma

      real*8  r_peg(3,0:I_MT)
      real*8  r_str(2,0:I_MT)
      real*8  r_spc(2,0:I_MT)
      real*8  r_rad(0:I_MT)
      real*8  r_axis
      real*8  r_esqr

      real*8  r_tmp(3,4)
      real*8  r_nul(3,4)

      real*8  r_iat(3,4)

      real*8  r_aff(3,4)
      real*8  r_aff1(3,4)
      real*8  r_aff2(3,4)

      real*8  r_inv(3,4)
      real*8  r_inv1(3,4)

      real*8 r_atm(3,4,0:I_MT)
      real*8 r_mta(3,4,0:I_MT)

      real*8  v_tmp(3)

      real*8  r_lat
      real*8  r_lon
      real*8  r_hgt

      real*8  r_pi,r_dtor,r_rtod

      real*8  r_rot(3,3)
      
      real*8 v_oloc(3)
      real*8 v_oloc1(3)
      real*8 v_oloc2(3)
      real*8 v_oloc3(3)
      real*8 v_iloc(3)
      real*8 v_iloc1(3)
      real*8 v_iloc2(3)
      real*8 v_iloc3(3)

      real*8 r_mdnc(2,0:I_MT)
      real*8 r_ddnc(2,0:I_MT)

      real*4 r_data(I_MS)
      real*4 r_datb(I_MS)

      real*4 r_dmul
      real*4 r_dadd

      real*4 r1,r2,r3,r4,r5,r6,r7


C     FUNCTION STATEMENTS:
      integer*4 length
      external length

      real*8 rdir
      external rdir


c     DATA STATEMENTS:


c     PROCESSING STEPS:

      r_pi = 4.d0*atan(1.0d0)
      r_dtor = r_pi/180.d0
      r_rtod = 180.d0/r_pi

      a_outfile=' '
      a_cmdfile=' '
      i_inarg = iargc()
      if(i_inarg .ne. 1 .and. i_inarg .ne. 2 )then
         write(6,*) 'Usage 1 arguments: multitps cmd_file'
         stop
      else
         call getarg(1,a_cmdfile)
      endif

c
c  Initialize datum stuff
c
        r_axis =  6378137.0
        r_esqr = 0.00669438


c
c  Initialize input variables
c
        do i_file = 0,I_MT
          r_peg(1,i_file) = 0.
          r_peg(2,i_file) = 0.
          r_peg(3,i_file) = 0.
          r_str(1,i_file) = 0.
          r_str(2,i_file) = 0.
          r_spc(1,i_file) = 1.
          r_spc(2,i_file) = 1.
          i_pegset(i_file) = 0
          i_strset(i_file) = 0
          i_spcset(i_file) = 0
          i_sizset(i_file) = 0
          a_tpsfile(i_file) = ' '
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
          do i=1,3
            do j=1,4
              r_iat(i,j) = 0.
            enddo
          enddo
          r_iat(1,1) = 1.
          r_iat(2,2) = 1.
          r_iat(3,3) = 1.

        enddo

        do i=1,3
          do j=1,4
            r_nul(i,j) = 0.
          enddo
        enddo
        r_nul(1,1) = 1.
        r_nul(2,2) = 1.
        r_nul(3,3) = 1.
        i_mm=0


      write(6,*) ' '
      write(6,*) '     << Multitps Program >>    '
      write(6,*) ' '

c
c  Read Command file
c
        write(6,*) ' '
        write(6,*) 'Opening CMD file: ',a_cmdfile(1:52)
        open(unit=10,file=a_cmdfile,status='old',form='formatted')

        read(10,'(a)',end=110) a_outfile
        i_err = 0
        i_file = -1
        do while(i_err .eq. 0)
          read(10,'(a)',end=110) a_input
          call parse(a_input,i_vals,a_vals)     
          do i = 1,i_vals
            if (a_vals(i) .eq. ' ') then
              print *,'parse error'
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
              i_sizset(i_file) = 1
            else if (a_vals(i)(1:4) .eq. 'off=') then
              read(a_vals(i)(5:),*) r_str(1,i_file),r_str(2,i_file)
              i_strset(i_file) = 1
            else if (a_vals(i)(1:4) .eq. 'spc=') then
              read(a_vals(i)(5:),*) r_spc(1,i_file),r_spc(2,i_file)
              i_spcset(i_file) = 1
            else if (a_vals(i)(1:4) .eq. 'peg=') then
              read(a_vals(i)(5:),*) r_peg(1,i_file),r_peg(2,i_file),r_peg(3,i_file)
              r_peg(1,i_file) = r_peg(1,i_file)/r_rtod
              r_peg(2,i_file) = r_peg(2,i_file)/r_rtod
              r_peg(3,i_file) = r_peg(3,i_file)/r_rtod
              i_pegset(i_file) = 1
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
            else if (a_vals(i)(1:4) .eq. 'dma=' .or. 
     &               a_vals(i)(1:4) .eq. 'd*2=' ) then
              i_file=i_file+1
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
              a_tpsfile(i_file)=a_vals(i)(5:)
              i_pos = index(a_tpsfile(i_file),'.')
              if (i_pos .eq. 0) i_pos = length(a_tpsfile(i_file))+1
              a_hdrfile(i_file)=a_tpsfile(i_file)(1:i_pos-1)//'.hdr'
            else
              i_file=i_file+1
              a_tpsfile(i_file)=a_vals(i)
              i_pos = index(a_tpsfile(i_file),'.')
              if (i_pos .eq. 0) i_pos = length(a_tpsfile(i_file))+1
              a_hdrfile(i_file)=a_tpsfile(i_file)(1:i_pos-1)//'.hdr'
            endif
          enddo
        enddo

110     close(10)
        i_nof=i_file

          write(6,*) ' '
          write (6,*) 'Opening OUT file: ',a_outfile(1:max(length(a_outfile),1))
          open(unit=20,file=a_outfile,form='formatted',status='unknown')

          do i_file = 0,i_nof

            if (a_afffile(i_file) .ne. ' ') then  ! Read incremental affine transformation
              write (6,*) 'Opening AFF file: ',a_outfile(1:max(length(a_outfile),1))
              open(unit=40+i_file,file=a_afffile(i_file),form='formatted',status='old')
              read(40+i_file,*)
              read(40+i_file,*)
              read(40+i_file,*)
              read(40+i_file,*) r_aff(1,1),r_aff(1,2),r_aff(1,3)
              read(40+i_file,*) r_aff(2,1),r_aff(2,2),r_aff(2,3)
              read(40+i_file,*) r_aff(3,1),r_aff(3,2),r_aff(3,3)
              read(40+i_file,*)
              read(40+i_file,*) r_aff(1,4),r_aff(2,4),r_aff(3,4)
              close(40+i_file)
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

            if (i_file .eq. 0) then
              if (a_hdrfile(i_file) .ne. ' ') then  ! Read header file if exists
                if (i_pegset(i_file) .eq. 0 .and. i_strset(i_file) .eq. 0 .and. 
     &                i_sizset(i_file) .eq. 0 .and. i_spcset(i_file) .eq. 0 ) then
                  call read_hdr(a_hdrfile(i_file),i_lsize(i_file),i_ssize(i_file),
     &                r_peg(1,i_file),r_str(1,i_file),r_spc(1,i_file),
     &                i_mbytes(i_file),i_dbytes(i_file),
     &                r_mdnc(1,i_file),r_ddnc(1,i_file),i_hdrstat(i_file))
                endif
              endif

              if (a_dtefile(i_file) .ne. ' ') then
                call read_dhdr(a_dtefile(i_file),i_dlsize,i_dssize,i_dbytes(i_file),i_doff(i_file),r_dmul,r_dadd,
     &                 r_peg(1,i_file),r_str(1,i_file),r_spc(1,i_file))
                if (i_dssize .gt. 0) then
                  i_lsize(i_file)=i_dlsize
                  i_ssize(i_file)=i_dssize
                  r_ddnc(1,i_file) = r_dmul
                  r_ddnc(2,i_file) = r_dadd
                endif
              endif
            else
              call read_gcphdr(a_hdrfile(i_file),a_type(i_file),a_proj(i_file),a_units(i_file),
     &            r_peg(1,i_file),r_str(1,i_file),r_spc(1,i_file),
     &            r_ddnc(1,i_file),i_hdrstat(i_file))
              call read_gcphdr(a_tpsfile(i_file),a_type(i_file),a_proj(i_file),a_units(i_file),
     &            r_peg(1,i_file),r_str(1,i_file),r_spc(1,i_file),
     &            r_ddnc(1,i_file),i_hdrstat(i_file))
              if (a_type(i_file) .ne. ' ' .and. a_type(i_file) .ne. 'gcp' .and. a_type(i_file) .ne. 'GCP' ) then
                stop 'GCP Header Error'
              endif
              a_type(i_file) = a_proj(i_file)
              if (a_type(i_file) .eq. 'llh' .or. a_type(i_file) .eq. 'LLH') then
                a_type(i_file) = 'eqa'
                if (a_units(i_file) .eq. 'dms' .or. a_units(i_file) .eq. 'DMS' .or.
     &              a_units(i_file) .eq. 'deg' .or. a_units(i_file) .eq. 'DEG' ) then
                  r_spc(1,i_file) = 1.0
                  r_spc(2,i_file) = 1.0
                  r_str(1,i_file) = 0.0
                  r_str(2,i_file) = 0.0
                else if (a_units(i_file) .eq. 'rad' .or. a_units(i_file) .eq. 'RAD' ) then
                  r_spc(1,i_file) = r_dtor
                  r_spc(2,i_file) = r_dtor
                  r_str(1,i_file) = 0.0
                  r_str(2,i_file) = 0.0
                endif
              endif
              if (r_ddnc(1,i_file) .eq. 0.) then
                r_ddnc(1,i_file) = 1.0
                r_ddnc(2,i_file) = 1.0
              endif
              call write_hdr(20,'SRCH',a_type(i_file),i_lsize(i_file),i_ssize(i_file),
     &              r_peg(1,i_file),r_str(1,i_file),r_spc(1,i_file),i_zone(i_file),i_err)
            endif

            r_rad(i_file) = rdir(r_axis,r_esqr,r_peg(3,i_file),r_peg(1,i_file))

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
              call tcnatm(r_axis,r_esqr,r_peg(1,i_file),r_iat(1,1))
            else if (a_type(i_file).eq.'SCH' .or. a_type(i_file).eq.'sch' .or.
     &               a_type(i_file).eq.'SLH' .or. a_type(i_file).eq.'slh') then
              i_type(i_file) = 2
              call tcnatm(r_axis,r_esqr,r_peg(1,i_file),r_iat(1,1))
            else if (a_type(i_file).eq.'SRH' .or. a_type(i_file).eq.'srh') then
              i_type(i_file) = -2
              call tcnatm(r_axis,r_esqr,r_peg(1,i_file),r_iat(1,1))
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
                call enutoll(r_axis,r_esqr,i_zone(i_file),a_grid,v_oloc,r_peg(1,i_file),r_peg(2,i_file),2)
              endif
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
                call utmtoll(r_axis,r_esqr,i_zone(i_file),a_grid,v_oloc,r_peg(1,i_file),r_peg(2,i_file),2)
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
            if (i_type(i_file) .eq. 4 ) r_spc(1,i_file) = -r_spc(1,i_file)

c            type *,' '
c            type *,'r_iat(x,x)='
c            type *,r_iat(1,1),r_iat(1,2),r_iat(1,3)
c            type *,r_iat(2,1),r_iat(2,2),r_iat(2,3)
c            type *,r_iat(3,1),r_iat(3,2),r_iat(3,3)
c            type *,r_iat(1,4),r_iat(2,4),r_iat(3,4)

            call multitrn(r_iat,r_aff,r_tmp)

            if (i_file .eq. 0) then

              call invrstrn(r_tmp,r_inv)

c              type *,' '
c              type *,'r_inv='
c              type *,r_inv(1,1),r_inv(1,2),r_inv(1,3)
c              type *,r_inv(2,1),r_inv(2,2),r_inv(2,3)
c              type *,r_inv(3,1),r_inv(3,2),r_inv(3,3)
c              type *,r_inv(1,4),r_inv(2,4),r_inv(3,4)
c              type *,' '

              if (a_tpsfile(0) .ne. ' ') then
                write(6,'(x,a,a,2i6)') 'Opening RMG file: ',a_tpsfile(0)(1:40),i_ssize(0),i_lsize(0)
                open(unit=30,file=a_tpsfile(0),form='unformatted',status='unknown',
     &             access='direct',recl=8*i_ssize(0))
              else if (a_dtefile(0) .ne. ' ') then
                write(6,'(x,a,a,2i6)') 'Opening DTE file: ',a_dtefile(0)(1:40),i_ssize(0),i_lsize(0)
                open(unit=30,file=a_dtefile(0),form='unformatted',status='unknown',
     &             access='direct',recl=4*i_ssize(0))
              endif

              call write_hdr(20,'REFF',a_type(0),i_lsize(0),i_ssize(0),
     &          r_peg(1,0),r_str(1,0),r_spc(1,0),i_zone(0),i_err)

            else

              call multitrn(r_inv,r_tmp,r_atm(1,1,i_file))
              call invrstrn(r_atm(1,1,i_file),r_mta(1,1,i_file))

c              type *,' '
c              type *,'r_atm='
c              type *,r_atm(1,1,i_file),r_atm(1,2,i_file),r_atm(1,3,i_file)
c              type *,r_atm(2,1,i_file),r_atm(2,2,i_file),r_atm(2,3,i_file)
c              type *,r_atm(3,1,i_file),r_atm(3,2,i_file),r_atm(3,3,i_file)
c              type *,r_atm(1,4,i_file),r_atm(2,4,i_file),r_atm(3,4,i_file)
c              type *,'r_mta='
c              type *,r_mta(1,1,i_file),r_mta(1,2,i_file),r_mta(1,3,i_file)
c              type *,r_mta(2,1,i_file),r_mta(2,2,i_file),r_mta(2,3,i_file)
c              type *,r_mta(3,1,i_file),r_mta(3,2,i_file),r_mta(3,3,i_file)
c              type *,r_mta(1,4,i_file),r_mta(2,4,i_file),r_mta(3,4,i_file)
              call multitrn(r_mta(1,1,i_file),r_atm(1,1,i_file),r_tmp)
c              type *,'r_tmp='
c              type *,r_tmp(1,1),r_tmp(1,2),r_tmp(1,3)
c              type *,r_tmp(2,1),r_tmp(2,2),r_tmp(2,3)
c              type *,r_tmp(3,1),r_tmp(3,2),r_tmp(3,3)
c              type *,r_tmp(1,4),r_tmp(2,4),r_tmp(3,4)

              open(30+i_file,file=a_tpsfile(i_file),status='old')
              write(6,'(x,a,a)') 'Opening TPS file: ',a_tpsfile(i_file)(1:max(length(a_tpsfile(i_file)),1))
              write(6,*) ' '
              do while(.true.)
                read(30+i_file,'(a)',end=999) a_input
                if (index(a_input,';') .gt. 0) goto 910
                if (a_units(i_file) .eq. 'dms' .or. a_units(i_file) .eq. 'DMS') then
                  read(a_input,*,err=910) r1,r2,r3,r4,r5,r6,r7
                  v_iloc1(1) = r1 + sign((r2+(r3/60.))/60.,r1)
                  v_iloc1(2) = r4 + sign((r5+(r6/60.))/60.,r4)
                  v_iloc1(3) = r7
                else if (a_units(i_file) .eq. 'xdeg' .or. a_units(i_file) .eq. 'XDMS') then
                  read(a_input,*,err=910) r1,v_iloc1
                else
                  read(a_input,*,err=910) v_iloc1
                endif
                i_mm = i_mm + 1

                  v_iloc1(1) = v_iloc1(1)*r_spc(1,i_file) + r_str(1,i_file)
                  v_iloc1(2) = v_iloc1(2)*r_spc(2,i_file) + r_str(2,i_file)
                  v_iloc1(3) = v_iloc1(3)*r_ddnc(1,i_file) + r_ddnc(2,i_file)

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
                    call enutoll(r_axis,r_esqr,i_zone(i_file),a_grid,v_iloc1,r_lat,r_lon,1)
                    call latlon(r_axis,r_esqr,v_iloc2,r_lat,r_lon,r_hgt,1)
                  else if (i_type(i_file) .eq. 4) then        ! Convert input from utm to xyz
                    r_hgt=v_iloc1(3)
                    call utmtoll(r_axis,r_esqr,i_zone(i_file),a_grid,v_iloc1,r_lat,r_lon,1)
                    call latlon(r_axis,r_esqr,v_iloc2,r_lat,r_lon,r_hgt,1)
                  else if (i_type(i_file) .eq. 5) then        ! Convert input from equal angle to xyz
                    r_lat=v_iloc1(1)/r_rtod
                    r_lon=v_iloc1(2)/r_rtod
                    r_hgt=v_iloc1(3)
                    call latlon(r_axis,r_esqr,v_iloc2,r_lat,r_lon,r_hgt,1)
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
                    call latlon(r_axis,r_esqr,v_iloc3,r_lat,r_lon,r_hgt,2)
                    call enutoll(r_axis,r_esqr,i_zone(0),a_grid,v_oloc,r_lat,r_lon,2)
                    v_oloc(3) = r_hgt
                  else if (i_type(0) .eq. 4) then                ! Convert output from xyz to utm
                    call latlon(r_axis,r_esqr,v_iloc3,r_lat,r_lon,r_hgt,2)
                    call utmtoll(r_axis,r_esqr,i_zone(0),a_grid,v_oloc,r_lat,r_lon,2)
                    v_oloc(3) = r_hgt
                  else if (i_type(0) .eq. 5) then                ! Convert output from xyz to ll
                    call latlon(r_axis,r_esqr,v_iloc3,r_lat,r_lon,r_hgt,2)
                    v_oloc(1) = r_lat*r_rtod
                    v_oloc(2) = r_lon*r_rtod
                    v_oloc(3) = r_hgt
                  endif

                  v_oloc(1) = (v_oloc(1) - r_str(1,0))/r_spc(1,0)
                  v_oloc(2) = (v_oloc(2) - r_str(2,0))/r_spc(2,0)

                  i_ll = nint(v_oloc(1))
                  i_ss = nint(v_oloc(2))

c                  write( 6,'(i5,6f12.5)') i_mm,v_oloc(1), v_oloc(2),r_datb(i_ss),v_iloc1(1),v_iloc1(2),v_iloc1(3)
                  if (i_ll .ge. 1 .and. i_ll .le. i_lsize(0)) then
                    if (i_ss .ge. 1 .and. i_ss .le. i_ssize(0)) then
                      
                      if (a_tpsfile(0) .ne. ' ') then
                        read(30,rec=i_ll) (r_data(is),is=1,i_ssize(0)),(r_datb(iss),iss=1,i_ssize(0))
                        if (r_data(i_ss) .ne. 0. .and. nint(abs(r_datb(i_ss))) .ne. 10000) then
                          write( 6,'(i5,6f12.5)') i_mm,v_oloc(1), v_oloc(2),r_datb(i_ss),v_iloc1(1),v_iloc1(2),v_iloc1(3)
                          write(20,'(6f12.5)') v_oloc(1), v_oloc(2),r_datb(i_ss),v_iloc1(1),v_iloc1(2),v_iloc1(3)
                        endif
                      else if (a_dtefile(0) .ne. ' ') then
                        read(30,rec=i_ll) (r_datb(iss),iss=1,i_ssize(0))
                        if (nint(abs(r_datb(i_ss))) .ne. 10000) then
                          write( 6,'(i5,6f12.5)') i_mm,v_oloc(1), v_oloc(2),r_datb(i_ss),v_iloc1(1),v_iloc1(2),v_iloc1(3)
                          write(20,'(6f12.5)') v_oloc(1), v_oloc(2),r_datb(i_ss),v_iloc1(1),v_iloc1(2),v_iloc1(3)
                        endif
                      endif

                    endif
                  endif

910           enddo
999           close(30+i_file)

            endif

          enddo
          close(20)
          close(30)

      end

        subroutine applytrn(r_aff,r_vec1,r_vec2)
c
c         This subroutine applys an affine transforms to a vector
c
          implicit none

          real*8 r_aff(3,4)
          real*8 r_vec1(3)
          real*8 r_vec2(3)

            call vecmulti(r_aff,r_vec1,r_vec2)
            call vecaddit(r_aff(1,4),r_vec2,r_vec2)          

          return
        end

        subroutine multitrn(r_aff1,r_aff2,r_aff)
c
c         This subroutine combines two affine transforms into one
c              transform.
c
          implicit none

          real*8 r_aff(3,4)
          real*8 r_aff1(3,4)
          real*8 r_aff2(3,4)

            call matmulti(r_aff1,r_aff2,r_aff)
            call vecmulti(r_aff1,r_aff2(1,4),r_aff(1,4))
            call vecaddit(r_aff1(1,4),r_aff(1,4),r_aff(1,4))          

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

        subroutine vecmulti(r_a,r_b,r_c)
c
c           This subroutine forms the product of a matrix "a" times
c                times the vector "b"
c
          implicit none

          real*8 r_a(3,3)
          real*8 r_b(3)
          real*8 r_c(3)

          r_c(1)=r_a(1,1)*r_b(1)+r_a(1,2)*r_b(2)+r_a(1,3)*r_b(3)
          r_c(2)=r_a(2,1)*r_b(1)+r_a(2,2)*r_b(2)+r_a(2,3)*r_b(3)
          r_c(3)=r_a(3,1)*r_b(1)+r_a(3,2)*r_b(2)+r_a(3,3)*r_b(3)

          return

        end


      subroutine v_mult(i_ax,i_ay,r_a,i_bx,i_by,r_b,r_c)

        implicit none

        integer*4 ix,iy,ixy
        integer*4 i_ax,i_ay
        integer*4 i_bx,i_by
        real*8 r_a(i_ax,i_ay)
        real*8 r_b(i_bx,i_by)
        real*8 r_c(i_bx,i_ay)

        if (i_ay .eq. i_bx) then
          do iy = 1,i_ax
            do ix = 1,i_by
              r_c(ix,iy) = 0.
              do ixy=1,i_ay
                r_c(ix,iy)=r_c(ix,iy)+r_a(ix,ixy)*r_b(ixy,iy)
              enddo
            enddo
          enddo
        else
          stop 'Error in V_MULT -  Matrix sizes not compatable'
        endif

        return
      end

      subroutine v_transpose(i_ax,i_ay,r_a,r_b)

        implicit none

        integer*4 ix,iy
        integer*4 i_ax,i_ay
        real*8 r_a(i_ax,i_ay)
        real*8 r_b(I_ax,i_ay)

        do iy = 1,i_ay
          do ix = 1,i_ax
            r_b(ix,iy) = r_a(iy,ix)
          enddo
        enddo

        return
      end

      subroutine v_normalize(i_ax,r_a)

        implicit none

        integer*4 ix
        integer*4 i_ax
        real*8 r_a(i_ax)
        real*8 r_sum

        r_sum=0.
        do ix = 1,i_ax
          r_sum = r_sum+r_a(ix)**2
        enddo

        r_sum=sqrt(r_sum)

        do ix = 1,i_ax
          r_a(ix)=r_a(ix)/r_sum
        enddo

        return
      end

c ======================================================================
c
        integer*4 function length(a_string)
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

        integer*4 i_len

          i_len=len(a_string)
          do while(i_len .gt. 0 .and. a_string(i_len:i_len) .eq. ' ')
            i_len=i_len-1
          enddo

          length=i_len
          return
        end


****************************************************************
        subroutine read_hdr(a_hdrfile,i_lsize,i_ssize,r_peg,
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

        integer*4 i_err
        integer*4 i_lsize
        integer*4 i_ssize

        integer*4 i_mbytes
        integer*4 i_dbytes

        real*8 r_peg(3)
        real*8 r_str(2)
        real*8 r_spc(2)
        real*8 r_mdnc(2)
        real*8 r_ddnc(2)


c	LOCAL VARIABLES: 

        integer*4 i
        integer*4 i_cnt
        real*8 r_atm(3,4)
        real*8 r_pi
        real*8 r_rtod


        character*255 a_tmp

c	DATA STATEMENTS: none

c	FUNCTION STATEMENTS: none

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
        write(6,*) 'Opening HDR file: ',a_hdrfile(1:52)
        i_err = 0

        do i=1,10000000
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
           else if (index(a_tmp,'Elevation Scale and Shift') .gt. 0) then
             read(a_tmp,*) r_ddnc
           else if (index(a_tmp,'Magnitude Bytes per Pixel') .gt. 0) then
             read(a_tmp,*) i_mbytes
           else if (index(a_tmp,'Elevation Bytes per pixel') .gt. 0) then
             read(a_tmp,*) i_dbytes
           endif
        enddo
        close(12)
        stop 'Error reading header file, too many lines'

900     close(12,err=910)
910     continue !  i_err = 0 if file even exists  if (i_cnt .eq. 15) i_err = 0 
        return
      end

****************************************************************
        subroutine read_gcphdr(a_hdrfile,a_type,a_proj,a_units,
     &              r_peg,r_str,r_spc,r_ddnc,i_err)

c****************************************************************
c**
c**	FILE NAME: read_hdr.f
c**
c**     DATE WRITTEN: 2/15/96
c**
c**     PROGRAMMER:Scott Shaffer
c**
c** 	FUNCTIONAL DESCRIPTION: Reads an GCP header file.
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

        integer*4 i_err

        real*8 r_peg(3)
        real*8 r_str(2)
        real*8 r_spc(2)
        real*8 r_ddnc(2)

        character*255 a_type
        character*255 a_proj
        character*255 a_units

c	LOCAL VARIABLES: 

        integer*4 i
        integer*4 i_cnt

        character*255 a_tmp

        real*8 r_pi
        real*8 r_rtod

c	DATA STATEMENTS: none

c	FUNCTION STATEMENTS: none

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
        write(6,*) 'Opening HDR file: ',a_hdrfile(1:52)
        i_err = 0

        do i=1,10000000
           read(12,'(a)',end=900) a_tmp
           if (a_tmp .eq. ' ') then
             ! do nothing
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
           else if (index(a_tmp,'Data file type') .gt. 0) then
             a_type = a_tmp(1:max(index(a_tmp,';')-1,1))
           else if (index(a_tmp,'Data file projection') .gt. 0) then
             a_proj = a_tmp(1:max(index(a_tmp,';')-1,1))
           else if (index(a_tmp,'Data file horizontal units') .gt. 0) then
             a_units = a_tmp(1:max(index(a_tmp,';')-1,1))
           else if (index(a_tmp,'Elevation Scale and Shift') .gt. 0) then
             read(a_tmp,*) r_ddnc
           endif
        enddo
        close(12)
        stop 'Error reading header file, too many lines'

900     close(12,err=910)
910     continue !  i_err = 0 if file even exists  if (i_cnt .eq. 15) i_err = 0 
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

          i_dlsize = 0
          i_dssize = 0
          i_demoff = 0
          i_doff = 0

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
              else if (index(a_string(i),'ORIGIN OF AZIMUTH (M) =') .gt. 0) then
                read(a_string(i)(35:),*,err=990) r_str(1)
c                type *,'s0 =',r_str(1)
              else if (index(a_string(i),'CROSS-TRACK OFFSET C0 (M) =') .gt. 0) then
                read(a_string(i)(35:),*,err=990) r_str(2)
c                type *,'c0 =',r_str(2)
              else if (index(a_string(i),'ORIGIN OF RANGE (M) =') .gt. 0) then
                read(a_string(i)(35:),*,err=990) r_str(2)
c                type *,'c0 =',r_str(2)
              endif
              if (1.eq.2) then
990             write(6,*) 'Error - ',i,' ',a_string(i)
                write(6,*) ' '
              endif
            enddo
            
              
          endif
902       close(18) 
       return

      end

****************************************************************
        subroutine write_hdr(i_hdrfile,a_filter,a_type,i_lsize,i_ssize,r_peg,
     &              r_str,r_spc,i_zone,i_err)

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
        
        integer*4 i_hdrfile
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

        integer*4 length
        external length

c  	PROCESSING STEPS:
        
c
c  Initialize pi and conversions
c
        r_pi = 4.d0*atan(1.0d0)
        r_rtod = 180.0d0/r_pi

           i_err=1
c           write(6,*) 'Writing HDR info for file ',a_filter
           write(i_hdrfile,'(a45,3a)') a_type(:max(length(a_type),1)),' ; ',
     &               a_filter(:length(a_filter)),' Data file type '
           write(i_hdrfile,'(2i15,  15x,3a)') i_lsize,i_ssize,' ; ',a_filter(:length(a_filter)),
     &               ' Data file dimensions '
           write(i_hdrfile,'(2f15.2,15x,3a)') r_spc,' ; ',a_filter(:length(a_filter)),
     &               ' Post Spacing'
           write(i_hdrfile,'(2f15.2,15x,3a)') r_str,' ; ',a_filter(:length(a_filter)),
     &               ' Starting corner position (s,c)'
           r_p(1) = r_peg(1)*r_rtod
           r_p(2) = r_peg(2)*r_rtod
           r_p(3) = r_peg(3)*r_rtod
           write(i_hdrfile,'(3f15.7,3a)')  r_p, ' ; ',a_filter(:length(a_filter)),
     &               ' Peg position (WGS-84)'
           if (a_type .eq. 'neu' .or. a_type .eq. 'enu' .or. a_type .eq. 'utm') then 
             write(i_hdrfile,'(i15,30x,3a)') i_zone,' ; ',a_filter(:length(a_filter)),' UTM Zone '
           else
             write(i_hdrfile,'(a)') ' '
           endif
           write(i_hdrfile,'(a)') ' '
           i_err=0
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
        integer*4 i_type
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
        integer*4 i_type                   !1=lat,lon to vector,2= vector to lat,lon
        real*8 r_a                       !ellispoid semi-major axis
        real*8 r_e2                      !ellipsoid eccentricity squared  
        real*8 r_v(3)                    !geocentric vector (meters)
        real*8 r_lat                     !latitude (deg -90 to 90)
        real*8 r_lon                     !longitude (deg -180 to 180)
        real*8 r_hgt                     !height above ellipsoid (meters)
   
c       OUTPUT VARIABLES:see input

c       LOCAL VARIABLES:
        integer*4 i_ft
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
        integer*4 i_type                   !1=lat,lon to vector,2= vector to lat,lon
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
        integer*4 i_type                   !2=lat,lon to utm,1= utm to lat,lon
        real*8 r_a                       !ellispoid semi-major axis
        real*8 r_e2                      !ellipsoid eccentricity squared  
        real*8 r_vec(2)                  !Northing,Easting(m)
        real*8 r_lat                     !latitude (deg -90 to 90)
        real*8 r_lon                     !longitude (deg -180 to 180)
        integer*4 i_zone                   !UTM zone
        character*1 a_grid               !UTM North-South grid
   
c       OUTPUT VARIABLES:see input

c       LOCAL VARIABLES:
        integer*4 i_ft,i_gi
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
        integer*4 i_type                   !1=lat,lon to utm,2= utm to lat,lon
        real*8 r_a                       !ellispoid semi-major axis
        real*8 r_e2                      !ellipsoid eccentricity squared  
        real*8 r_vec(2)                    !Northing,Easting(m)
        real*8 r_lat                     !latitude (deg -90 to 90)
        real*8 r_lon                     !longitude (deg -180 to 180)
        integer*4 i_zone                   !UTM zone
        character*1 a_grid               !UTM North-South grid
   
c       OUTPUT VARIABLES:see input

c       LOCAL VARIABLES:
        integer*4 i_ft,i_gi
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
          return
        end
