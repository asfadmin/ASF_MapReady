c      This software must be linked to real*8 versions of the following 
c      Numerical Recipes (v1992) routines to work properly.

c      SUBROUTINE gaussj(a,n,np,b,m,mp)
c      SUBROUTINE svdcmp(a,m,n,mp,np,w,v)
c      SUBROUTINE svbksb(u,w,v,m,n,mp,np,b,x)
c      SUBROUTINE svdvar(v,ma,np,w,cvm,ncvm)
c      SUBROUTINE qrdcmp(a,n,np,c,d,sing)
c      REAL*8 FUNCTION pythag(a,b)

c***************************************************************

      program multiaffine3d

c****************************************************************
c**   
c**   FILE NAME: multiaffine3d.f
c**   
c**   DATE WRITTEN: 01/02/95 
c**   
c**   PROGRAMMER: Scott Hensley / Scott Shaffer
c**   
c**   FUNCTIONAL DESCRIPTION: Fits a 3 dimensional set of points 
c**   using an affine matrix and then decomposes into various 
c**   components of scale, skew, and rotation.
c**   
c**   ROUTINES CALLED: svdvecfit,QRdecomp
c**   
c**   NOTES: none
c**   
c**   UPDATE LOG:
c**   
c*****************************************************************

      IMPLICIT NONE

c     PARAMETERS:
      integer I_RDE
      integer I_MFE
      integer I_MTE
      integer I_NPE
      integer I_MPE
      parameter(I_RDE =     3) ! Max number of observations
      parameter(I_MFE =   100) ! Max number of image strips
      parameter(I_MTE =   400) ! Max number of tiepoint files
      parameter(I_NPE =   500) ! Max number of parameters to estimate
      parameter(I_MPE = 40000) ! Max number of measurements

      integer I_FPE
      parameter(I_FPE =     9) ! Number of parameters to pass to func

c     INPUT VARIABLES:
      character*80  a_cmdfile
      character*80  a_outfile
      character*80  a_tpsfile(I_MTE)
      character*255 a_temp
      character*255 a_string
      character*255 a_vals(10)
      character*80  a_filter
      character*1   a_grid       ! UTM identifier

c     OUTPUT VARIABLES:

c     LOCAL VARIABLES:
      logical sing,l_chisq

      character*80 a_type(0:I_MFE)

      integer i,j
      integer ii,jj
      integer i1,i2
      integer i11,i22
      integer ii2

      integer i_m
      integer i_tp
      integer i_mfc
      integer i_tps
      integer i_ma,i_mm
      integer i_inarg,iargc
      integer i_f1(I_MTE),i_f2(I_MTE)
      integer i_sav1(I_MPE)
      integer i_sav2(I_MPE)
      integer i_flip
      integer i_file
      integer i_mmm
      integer i_err
      integer i_vals
      integer i_dump

      integer i_ssize(0:I_MFE)
      integer i_lsize(0:I_MFE)
      integer i_type(0:I_MFE)
      integer i_zone(0:I_MFE)

      real*8  r_peg(3,0:I_MFE)
      real*8  r_str(2,0:I_MFE)
      real*8  r_spc(2,0:I_MFE)
      real*8  r_rad(0:I_MFE)
      real*8  r_ae(0:I_MFE)
      real*8  r_e2(0:I_MFE)

      real*8  r_aff(3,4)
      real*8  r_nul(3,4)

      real*8  v_tmp(3)

      real*8  r_lat
      real*8  r_lon
      real*8  r_hgt

      real*8  v_oloc(3)
      real*8  v_iloc(3)

      real*8  r_w(I_NPE)
      real*8  r_u(I_NPE*I_NPE)
      real*8  r_v(I_NPE*I_NPE)

      real*8  r_pi,r_dtor,r_rtod
      real*8  r_vecin(I_FPE,I_MPE)
      real*8  r_obs(I_RDE*I_MPE)
      real*8  r_cov(I_RDE*I_RDE*I_MPE)
      real*8  r_chisq(I_RDE*(I_MPE+1))

      real*8  r_aa(I_NPE)
      real*8  r_at2(I_NPE)
      real*8  r_c(3),r_d(3)
      real*8  r_qq1(3,3),r_qq1m
      real*8  r_qq2(3,3),r_qq2m
      real*8  r_rot(3,3)
      real*8  r_aff1(3,4)
      real*8  r_aff2(3,4)
      real*8  r_inv1(3,4)
      real*8  r_scale1,r_scale2,r_scale3
      real*8  r_skew1,r_skew2,r_skew3
      real*8  r_q1,r_q2,r_q3
      real*8  r_c1t,r_c2t,r_c3t
      real*8  r_c1t2,r_c2t2,r_c3t2
      real*8  r_rdr(3,3)
      real*8  r_rotaxis(3)
      real*8  r_rotang

      real*8  r_bb(12,I_MFE)
      integer i_bb(12,I_MFE)

      real*8  r_eulerangs(3)
      integer i_axisseq(3)

      real*8  r_vec1(3),r_vec2(3)
      real*8  r_vec11(3),r_vec22(3)
      
      integer length
      external length

      real*8 rdir
      external rdir

      real*8 r_rr(3,4,0:I_MFE)
      integer i_lko(I_MTE),i_af(I_NPE),i_ap(I_NPE)
      common/consts/ r_rr,i_lko,i_af,i_ap

      integer i_paramest(I_NPE),i_usedata(I_RDE*I_MTE),i_covopt
      common/funcom/i_paramest,i_usedata,i_covopt

c     DATA STATEMENTS:
      data i_axisseq / 1,2,3 /

      save r_cov,r_obs,r_chisq,r_vecin,r_w,r_u,r_v

C     FUNCTION STATEMENTS:

c     PROCESSING STEPS:

      write(6,*) ' '
      write(6,*) '     << Multiaffine3d19   9/18/98  >>    '
      write(6,*) ' '
      write(6,*) '     << A Three Dimensional Affine Transformation Estimation Program >>    '
      write(6,*) ' '

      r_pi   = 4.d0*atan(1.0d0)
      r_dtor = r_pi/180.d0
      r_rtod = 180.d0/r_pi

      a_temp = ' '
      a_outfile=' '
      a_cmdfile=' '
      i_covopt = 1
      i_dump = 0
      j = 0
      i_inarg = iargc()
      do i=1,i_inarg
         call getarg(i,a_temp)
c         write(6,*) a_temp(1:length(a_temp)),length(a_temp)
         if (a_temp .eq. ' ') then
            stop 'Error parsing command line'
         else if (a_temp .eq. '-i' .or. a_temp .eq. '-I') then
            i_covopt = 1
         else if (a_temp .eq. '-d' .or. a_temp .eq. '-D') then
            i_covopt = 2
         else if (a_temp .eq. '-f' .or. a_temp .eq. '-F') then
            i_covopt = 3
         else if (a_temp .eq. '-dump' .or. a_temp .eq. '-DUMP') then
            i_dump = 1
         else ! filename
            j = j+1
            if (j .gt. 2) then
               stop 'Error, too many file names on command line'
            else if (j .eq. 1) then
               a_cmdfile = a_temp
            else if (j .eq. 2) then
               a_outfile = a_temp
            endif
         endif
      enddo
      if(a_cmdfile .eq. ' ')then
         write(6,*) 'Usage 1 arguments: multiaffine3d cmdfile'
         write(6,*) 'Usage 2 arguments: multiaffine3d cmdfile outfile -x -x ...'
         write(6,*) '     where  -x can be  -i = identity matrix used for local covareance'
         write(6,*) '                       -d = diagonal matrix used for local covareance'
         write(6,*) '                       -f = full cov matrix used for local covareance'
         write(6,*) '                    -dump = displays more detailed fit info'
         write(6,*) ' '
         stop
      endif

c     
c     Initialize input variables
c     
      do i_file = 0,I_MFE
         r_peg(1,i_file) = 0.
         r_peg(2,i_file) = 0.
         r_peg(3,i_file) = 0.
         r_str(1,i_file) = 0.
         r_str(2,i_file) = 0.
         r_spc(1,i_file) = 1.
         r_spc(2,i_file) = 1.
         a_type(i_file) = ' '
         i_zone(i_file)=0
         i_ssize(i_file)=0
         i_lsize(i_file)=0
         do i=1,3
            do j=1,4
               r_rr(i,j,i_file) = 0.
            enddo
         enddo
         r_rr(1,1,i_file) = 1.
         r_rr(2,2,i_file) = 1.
         r_rr(3,3,i_file) = 1.

         r_ae(i_file) = 6378137.d0
         r_e2(i_file) = 0.00669437999015d0

      enddo

      do i=1,3
         do j=1,4
            r_nul(i,j) = 0.
         enddo
      enddo
      r_nul(1,1) = 1.
      r_nul(2,2) = 1.
      r_nul(3,3) = 1.

      do i=1,I_MTE
         do j=1,I_RDE
            i_usedata((i-1)*I_RDE + j) = 0
         enddo
      enddo


c     
c     Enter fit restrictions
c     
      write (6,*) 'Reading command file: ',a_cmdfile(1:max(1,length(a_cmdfile)))
      open(unit=21,file=a_cmdfile,form='formatted',status='old',err=910)

      read(21,*,err=911) i_mfc
      if (i_mfc .gt. I_MFE) stop 'Error - Number of affine transforms exeeds I_MFE'

      i_ma=0
      do i=1,i_mfc
         read(21,*,err=911)
         do j=1,12
            read(21,*,err=911) i_bb(j,i),r_bb(j,i)
            if (i_bb(j,i) .ne. 0 ) then
               i_ma=i_ma+1
               r_aa(i_ma) = r_bb(j,i)
               i_af(i_ma) = i
               i_ap(i_ma) = j
               i_paramest(i_ma) = 1
            endif
         enddo
      enddo

      read(21,*,err=911)
      i_tps=0
      do i=1,I_MTE
         read(21,'(a)',err=800,end=800) a_string
         a_vals(1) = ' '
         a_vals(2) = ' '
         a_vals(3) = ' '
         call parse(a_string,i_vals,a_vals)
         read(a_vals(1),*,err=800) i_f1(i)
         read(a_vals(2),*,err=800) i_f2(i)
         a_tpsfile(i) = a_vals(3)
         if (i_f1(i) .gt. i_mfc .or. i_f2(i) .gt. i_mfc) stop 'Error - Match to non existant file'
         if (i_f1(i) .eq. 0 .and. i_f2(i) .eq. 0) goto 800
         write(6,'(x,2i4,2x,a)') i_f1(i),i_f2(i),a_tpsfile(i)(1:40)
         i_tps = i
         do jj=1,2
            if (jj .eq. 1) then
               i_file = i_f1(i)
               a_filter = 'REFF'
            else
               i_file = i_f2(i)
               a_filter = 'SRCH'
            endif
            if (i_file .ne. 0) then
               call read_hdr(a_tpsfile(i),a_filter,a_type(i_file),i_lsize(i_file),i_ssize(i_file),
     &              r_peg(1,i_file),i_zone(i_file),r_str(1,i_file),r_spc(1,i_file),i_err)
               r_rad(i_file) = rdir(r_ae(i_file),r_e2(i_file),r_peg(3,i_file),r_peg(1,i_file))
               if (a_type(i_file) .eq. ' ') then
                  stop 'Error - No file type'
               else if (a_type(i_file) .eq. 'XYZ' .or. a_type(i_file) .eq. 'xyz') then
                  i_type(i_file) = 1
                  r_rr(1,1,i_file) = 1.
                  r_rr(1,2,i_file) = 0.
                  r_rr(1,3,i_file) = 0.
                  r_rr(1,4,i_file) = 0.
                  r_rr(2,1,i_file) = 0.
                  r_rr(2,2,i_file) = 1.
                  r_rr(2,3,i_file) = 0.
                  r_rr(2,4,i_file) = 0.
                  r_rr(3,1,i_file) = 0.
                  r_rr(3,2,i_file) = 0.
                  r_rr(3,3,i_file) = 1.
                  r_rr(1,4,i_file) = 0.
                  r_rr(2,4,i_file) = 0.
                  r_rr(3,4,i_file) = 0.
               else if (a_type(i_file).eq.'TCN' .or. a_type(i_file).eq.'tcn') then
                  i_type(i_file) = 1
                  call tcnatm(r_ae(i_file),r_e2(i_file),r_peg(1,i_file),r_rr(1,1,i_file))
               else if (a_type(i_file).eq.'SCH' .or. a_type(i_file).eq.'sch' .or.
     &                 a_type(i_file).eq.'SLH' .or. a_type(i_file).eq.'slh') then
                  i_type(i_file) = 2
                  call tcnatm(r_ae(i_file),r_e2(i_file),r_peg(1,i_file),r_rr(1,1,i_file))
               else if (a_type(i_file).eq.'SRH' .or. a_type(i_file).eq.'srh') then
                  i_type(i_file) = -2
                  call tcnatm(r_ae(i_file),r_e2(i_file),r_peg(1,i_file),r_rr(1,1,i_file))
               else if (a_type(i_file).eq.'ENU' .or. a_type(i_file).eq.'enu' ) then
                  i_type(i_file) = 3
                  r_rr(1,1,i_file) = 1.
                  r_rr(1,2,i_file) = 0.
                  r_rr(1,3,i_file) = 0.
                  r_rr(1,4,i_file) = 0.
                  r_rr(2,1,i_file) = 0.
                  r_rr(2,2,i_file) = 1.
                  r_rr(2,3,i_file) = 0.
                  r_rr(2,4,i_file) = 0.
                  r_rr(3,1,i_file) = 0.
                  r_rr(3,2,i_file) = 0.
                  r_rr(3,3,i_file) = 1.
                  r_rr(1,4,i_file) = 0.
                  r_rr(2,4,i_file) = 0.
                  r_rr(3,4,i_file) = 0.
                  if (i_zone(i_file) .le. 0) then
                     call enutoll(r_ae(i_file),r_e2(i_file),i_zone(i_file),a_grid,v_oloc,r_peg(1,i_file),r_peg(2,i_file),2)
                  endif
               else if (a_type(i_file).eq.'UTM' .or. a_type(i_file).eq.'utm' ) then
                  i_type(i_file) = 4
                  r_rr(1,1,i_file) = 1.
                  r_rr(1,2,i_file) = 0.
                  r_rr(1,3,i_file) = 0.
                  r_rr(1,4,i_file) = 0.
                  r_rr(2,1,i_file) = 0.
                  r_rr(2,2,i_file) = 1.
                  r_rr(2,3,i_file) = 0.
                  r_rr(2,4,i_file) = 0.
                  r_rr(3,1,i_file) = 0.
                  r_rr(3,2,i_file) = 0.
                  r_rr(3,3,i_file) = 1.
                  r_rr(1,4,i_file) = 0.
                  r_rr(2,4,i_file) = 0.
                  r_rr(3,4,i_file) = 0.
                  if (i_zone(i_file) .le. 0) then
                     call utmtoll(r_ae(i_file),r_e2(i_file),i_zone(i_file),a_grid,v_oloc,r_peg(1,i_file),r_peg(2,i_file),2)
                  endif
               else if (a_type(i_file).eq.'EQA' .or. a_type(i_file).eq.'eqa' ) then
                  i_type(i_file) = 5
                  r_rr(1,1,i_file) = 1.
                  r_rr(1,2,i_file) = 0.
                  r_rr(1,3,i_file) = 0.
                  r_rr(1,4,i_file) = 0.
                  r_rr(2,1,i_file) = 0.
                  r_rr(2,2,i_file) = 1.
                  r_rr(2,3,i_file) = 0.
                  r_rr(2,4,i_file) = 0.
                  r_rr(3,1,i_file) = 0.
                  r_rr(3,2,i_file) = 0.
                  r_rr(3,3,i_file) = 1.
                  r_rr(1,4,i_file) = 0.
                  r_rr(2,4,i_file) = 0.
                  r_rr(3,4,i_file) = 0.
               else
                  stop 'File type not recognized'
               endif

               if (i_type(i_file) .eq. -2) then
                  r_spc(2,i_file) = -r_spc(2,i_file)
                  r_str(2,i_file) = -r_str(2,i_file)
               endif
               if (i_type(i_file) .eq. 4 ) r_spc(1,i_file) = -r_spc(1,i_file)

c     type *,' '
c     type *,'r_rr(x,x,i_file)='
c     type *,r_rr(1,1,i_file),r_rr(1,2,i_file),r_rr(1,3,i_file)
c     type *,r_rr(2,1,i_file),r_rr(2,2,i_file),r_rr(2,3,i_file)
c     type *,r_rr(3,1,i_file),r_rr(3,2,i_file),r_rr(3,3,i_file)
c     type *,r_rr(1,4,i_file),r_rr(2,4,i_file),r_rr(3,4,i_file)

            endif
         enddo
      enddo      !get header info from files

      read(21,'(i1,i2,x,a80)',err=800,end=800) i_f1(I_MTE),i_f2(I_MTE),a_tpsfile(I_MTE)
      stop 'Error - Too many tie point files'
 800  close(21)
      write(6,*) ' '
      write(6,*) 'Number of tie point files: ',i_tps

c     type *,'i_paramest=',i_paramest
c     type *,'r_aa=',r_aa

      write(6,*) ' '
      write(6,*) 'Number of fit parameters:  ',i_ma

      i_mm = 0
      do i_tp=1,i_tps        !loop on tie point files

         i1=max(min(i_f1(i_tp),i_mfc),0)
         i2=max(min(i_f2(i_tp),i_mfc),0)
         if (i1 .eq. i2) stop 'Error - Cannot tie point a file to itself.'
         if (i1 .eq. 0) then
            i1 = i2
            i_flip = 1
         else
            i_flip = 0
         endif
         if (i2 .eq. 0) i2 = i1

         if (i2 .ne. i1) then
            ii2 = i2
         else
            ii2 = 0
         endif

         i_lko(i_tp) = (i_tp-1)*3

         do j=1,3
            i_usedata(i_lko(i_tp)+j) = 1
         enddo

c     type *,'r_rr(1,1,i1)',r_rr(1,1,i1)
c     type *,'r_rr(1,2,i1)',r_rr(1,2,i1)
c     type *,'r_rr(1,3,i1)',r_rr(1,3,i1)

c     type *,'r_rr(2,1,i1)',r_rr(2,1,i1)
c     type *,'r_rr(2,2,i1)',r_rr(2,2,i1)
c     type *,'r_rr(2,3,i1)',r_rr(2,3,i1)

c     type *,'r_rr(3,1,i1)',r_rr(3,1,i1)
c     type *,'r_rr(3,2,i1)',r_rr(3,2,i1)
c     type *,'r_rr(3,3,i1)',r_rr(3,3,i1)

c     type *,'r_rr(1,1,i2)',r_rr(1,1,i2)
c     type *,'r_rr(1,2,i2)',r_rr(1,2,i2)
c     type *,'r_rr(1,3,i2)',r_rr(1,3,i2)

c     type *,'r_rr(2,1,i2)',r_rr(2,1,i2)
c     type *,'r_rr(2,2,i2)',r_rr(2,2,i2)
c     type *,'r_rr(2,3,i2)',r_rr(2,3,i2)

c     type *,'r_rr(3,1,i2)',r_rr(3,1,i1)
c     type *,'r_rr(3,2,i2)',r_rr(3,2,i2)
c     type *,'r_rr(3,3,i2)',r_rr(3,3,i2)

         call calcaff(r_bb(1,i1),r_aff)
         call multitrn(r_rr(1,1,i1),r_aff,r_aff1)
c     type *,'aff1=',r_aff1


         if (i1 .ne. i2) then
            call calcaff(r_bb(1,i2),r_aff)
            call multitrn(r_rr(1,1,i2),r_aff,r_aff2)
c     type *,'aff2=',r_aff2
         else
            if (i_f1(i_tp) .eq. 0) then
               a_filter='REFF'
            else
               a_filter='SRCH'
            endif
            call read_hdr(a_tpsfile(i_tp),a_filter,a_type(0),i_lsize(0),i_ssize(0),
     &           r_peg(1,0),i_zone(0),r_str(1,0),r_spc(1,0),i_err)
            r_rad(0) = rdir(r_ae(0),r_e2(0),r_peg(3,0),r_peg(1,0))
            if (a_type(0) .eq. ' ') then
               stop 'Error - No file type'
            else if (a_type(0).eq.'XYZ' .or. a_type(0).eq.'xyz') then
               i_type(0) = 1
               r_rr(1,1,0) = 1.
               r_rr(1,2,0) = 0.
               r_rr(1,3,0) = 0.
               r_rr(1,4,0) = 0.
               r_rr(2,1,0) = 0.
               r_rr(2,2,0) = 1.
               r_rr(2,3,0) = 0.
               r_rr(2,4,0) = 0.
               r_rr(3,1,0) = 0.
               r_rr(3,2,0) = 0.
               r_rr(3,3,0) = 1.
               r_rr(1,4,0) = 0.
               r_rr(2,4,0) = 0.
               r_rr(3,4,0) = 0.
            else if (a_type(0).eq.'TCN' .or. a_type(0).eq.'tcn') then
               i_type(0) = 1
               call tcnatm(r_ae(0),r_e2(0),r_peg(1,0),r_rr(1,1,0))
            else if (a_type(0).eq.'SCH' .or. a_type(0).eq.'sch' .or.
     &              a_type(0).eq.'SLH' .or. a_type(0).eq.'slh') then
               i_type(0) = 2
               call tcnatm(r_ae(0),r_e2(0),r_peg(1,0),r_rr(1,1,0))
            else if (a_type(0).eq.'SRH' .or. a_type(0).eq.'srh') then
               i_type(0) = -2
               call tcnatm(r_ae(0),r_e2(0),r_peg(1,0),r_rr(1,1,0))
            else if (a_type(0).eq.'ENU' .or. a_type(0).eq.'enu' ) then
               i_type(0) = 3
               r_rr(1,1,0) = 1.
               r_rr(1,2,0) = 0.
               r_rr(1,3,0) = 0.
               r_rr(1,4,0) = 0.
               r_rr(2,1,0) = 0.
               r_rr(2,2,0) = 1.
               r_rr(2,3,0) = 0.
               r_rr(2,4,0) = 0.
               r_rr(3,1,0) = 0.
               r_rr(3,2,0) = 0.
               r_rr(3,3,0) = 1.
               r_rr(1,4,0) = 0.
               r_rr(2,4,0) = 0.
               r_rr(3,4,0) = 0.
               if (i_zone(0) .le. 0) then
                  call enutoll(r_ae(0),r_e2(0),i_zone(0),a_grid,v_oloc,r_peg(1,0),r_peg(2,0),2)
               endif
            else if (a_type(0).eq.'UTM' .or. a_type(0).eq.'utm' ) then
               i_type(0) = 4
               r_rr(1,1,0) = 1.
               r_rr(1,2,0) = 0.
               r_rr(1,3,0) = 0.
               r_rr(1,4,0) = 0.
               r_rr(2,1,0) = 0.
               r_rr(2,2,0) = 1.
               r_rr(2,3,0) = 0.
               r_rr(2,4,0) = 0.
               r_rr(3,1,0) = 0.
               r_rr(3,2,0) = 0.
               r_rr(3,3,0) = 1.
               r_rr(1,4,0) = 0.
               r_rr(2,4,0) = 0.
               r_rr(3,4,0) = 0.
               if (i_zone(0) .le. 0) then
                  call utmtoll(r_ae(0),r_e2(0),i_zone(0),a_grid,v_oloc,r_peg(1,0),r_peg(2,0),2)
               endif
            else if (a_type(0).eq.'EQA' .or. a_type(0).eq.'eqa' ) then
               i_type(0) = 5
               r_rr(1,1,0) = 1.
               r_rr(1,2,0) = 0.
               r_rr(1,3,0) = 0.
               r_rr(1,4,0) = 0.
               r_rr(2,1,0) = 0.
               r_rr(2,2,0) = 1.
               r_rr(2,3,0) = 0.
               r_rr(2,4,0) = 0.
               r_rr(3,1,0) = 0.
               r_rr(3,2,0) = 0.
               r_rr(3,3,0) = 1.
               r_rr(1,4,0) = 0.
               r_rr(2,4,0) = 0.
               r_rr(3,4,0) = 0.
            else
               stop 'File type not recognized'
            endif

            if (i_type(0) .eq. -2) then
               r_spc(2,0) = -r_spc(2,0)
               r_str(2,0) = -r_str(2,0)
            endif
            if (i_type(0) .eq. 4 ) r_spc(1,0) = -r_spc(1,0)

c     type *,' '
c     type *,'r_rr(x,x,0)='
c     type *,r_rr(1,1,0),r_rr(1,2,0),r_rr(1,3,0)
c     type *,r_rr(2,1,0),r_rr(2,2,0),r_rr(2,3,0)
c     type *,r_rr(3,1,0),r_rr(3,2,0),r_rr(3,3,0)
c     type *,r_rr(1,4,0),r_rr(2,4,0),r_rr(3,4,0)
            do i=1,3
               do j=1,3
                  r_aff2(i,j) = 0.
               enddo
               r_aff2(i,4) = 0.
            enddo
            r_aff2(1,1) = 1.
            r_aff2(2,2) = 1.
            r_aff2(3,3) = 1.

         endif

c                                                  File I                File II
c     read in three dimensional tie points  (across down height   across  down height)

         open(10,file=a_tpsfile(i_tp),status='old')

         do while(.true.)
            a_temp=' '
            do while(a_temp .eq. ' ' .or. index(a_temp,';') .ne. 0) 
               read(10,'(a)',end=999) a_temp
            enddo
            if (i_flip .eq. 0) then
               read(a_temp,*,end=999) r_vec1,r_vec2
            else
               read(a_temp,*,end=999) r_vec2,r_vec1
            endif

            i_mm = i_mm + 1
            if (i_mm .gt. I_MPE) stop 'Too many match points'

            i_sav1(i_mm)=i1
            i_sav2(i_mm)=i2

            do i=1,I_RDE
               r_obs(i+(i_mm-1)*3) = 0.
            enddo

            v_iloc(1) = r_spc(1,i1)*r_vec1(1)+r_str(1,i1)
            v_iloc(2) = r_spc(2,i1)*r_vec1(2)+r_str(2,i1)
            v_iloc(3) = r_vec1(3)

            if (i_type(i1) .eq. 1) then 
               r_vec1(1) = v_iloc(1)
               r_vec1(2) = v_iloc(2)
               r_vec1(3) = v_iloc(3)
            else if (i_type(i1) .eq. 2 .or. i_type(i1) .eq. -2) then ! convert input from sch to xyz
               r_lon = v_iloc(1)/r_rad(i1)
               r_lat = v_iloc(2)/r_rad(i1)
               r_hgt = v_iloc(3)
               call sch_to_tcn(r_rad(i1),r_vec1,r_lat,r_lon,r_hgt,1)
            else if (i_type(i1) .eq. 3) then ! Convert input from utm to xyz
               r_hgt = v_iloc(3)
               call enutoll(r_ae(i1),r_e2(i1),i_zone(i1),a_grid,v_iloc,r_lat,r_lon,1)
               call latlon(r_ae(i1),r_e2(i1),r_vec1,r_lat,r_lon,r_hgt,1)
            else if (i_type(i1) .eq. 4) then ! Convert input from utm to xyz
               r_hgt = v_iloc(3)
               call utmtoll(r_ae(i1),r_e2(i1),i_zone(i1),a_grid,v_iloc,r_lat,r_lon,1)
               call latlon(r_ae(i1),r_e2(i1),r_vec1,r_lat,r_lon,r_hgt,1)
            else if (i_type(i1) .eq. 5) then ! Convert input from equal angle to xyz
               r_lat = v_iloc(1)*r_dtor
               r_lon = v_iloc(2)*r_dtor
               r_hgt = v_iloc(3)
               call latlon(r_ae(i1),r_e2(i1),r_vec1,r_lat,r_lon,r_hgt,1)
            endif

            r_vecin(1,i_mm) = r_vec1(1) 
            r_vecin(2,i_mm) = r_vec1(2) 
            r_vecin(3,i_mm) = r_vec1(3) 


            v_iloc(1) = r_spc(1,ii2)*r_vec2(1)+r_str(1,ii2)
            v_iloc(2) = r_spc(2,ii2)*r_vec2(2)+r_str(2,ii2)
            v_iloc(3) = r_vec2(3)

            if (i_type(ii2) .eq. 1) then 
               r_vec2(1) = v_iloc(1)
               r_vec2(2) = v_iloc(2)
               r_vec2(3) = v_iloc(3)
            else if (i_type(ii2) .eq. 2 .or. i_type(ii2) .eq. -2) then ! convert input from sch to xyz
               r_lon = v_iloc(1)/r_rad(ii2)
               r_lat = v_iloc(2)/r_rad(ii2)
               r_hgt = v_iloc(3)
               call sch_to_tcn(r_rad(ii2),r_vec2,r_lat,r_lon,r_hgt,1)
            else if (i_type(ii2) .eq. 3) then ! Convert input from utm to xyz
               r_hgt = v_iloc(3)
               call enutoll(r_ae(ii2),r_e2(ii2),i_zone(ii2),a_grid,v_iloc,r_lat,r_lon,1)
               call latlon(r_ae(ii2),r_e2(ii2),r_vec2,r_lat,r_lon,r_hgt,1)
            else if (i_type(ii2) .eq. 4) then ! Convert input from utm to xyz
               r_hgt = v_iloc(3)
               call utmtoll(r_ae(ii2),r_e2(ii2),i_zone(ii2),a_grid,v_iloc,r_lat,r_lon,1)
               call latlon(r_ae(ii2),r_e2(ii2),r_vec2,r_lat,r_lon,r_hgt,1)
            else if (i_type(ii2) .eq. 5) then ! Convert input from equal angle to xyz
               r_lat = v_iloc(1)*r_dtor
               r_lon = v_iloc(2)*r_dtor
               r_hgt = v_iloc(3)
               call latlon(r_ae(ii2),r_e2(ii2),r_vec2,r_lat,r_lon,r_hgt,1)
            endif

            if (ii2 .eq. 0) then
               call vecmulti(r_rr(1,1,0),r_vec2,v_tmp) ! convert to wgs84 values if "truth file"
               call vecaddit(r_rr(1,4,0),v_tmp,r_vec2)
            endif

            r_vecin(4,i_mm) = r_vec2(1)
            r_vecin(5,i_mm) = r_vec2(2)
            r_vecin(6,i_mm) = r_vec2(3)

            r_vecin(7,i_mm) = i1
            r_vecin(8,i_mm) = i2
            r_vecin(9,i_mm) = i_tp

c     type *,'vec1=',r_vec1
c     type *,'vec2=',r_vec2

            call applytrn(r_aff1,r_vec1,r_vec11)
            call applytrn(r_aff2,r_vec2,r_vec22)

c     type *,'vec11=',r_vec11
c     type *,'vec22=',r_vec22

            r_obs(1+(i_mm-1)*3) = r_vec11(1) - r_vec22(1)
            r_obs(2+(i_mm-1)*3) = r_vec11(2) - r_vec22(2)
            r_obs(3+(i_mm-1)*3) = r_vec11(3) - r_vec22(3)
            
c            write(6,*) i_mm,i1,i2,sngl(r_obs(1+(i_mm-1)*3)),sngl(r_obs(2+(i_mm-1)*3)),
c     &           sngl(r_obs(3+(i_mm-1)*3))

            do i=1,I_RDE
               do j=1,I_RDE
                  if ( i .eq. j ) then
                     r_cov(i+I_RDE*(j-1+I_RDE*(i_mm-1))) = 1.0d0
                  else
                     r_cov(i+I_RDE*(j-1+I_RDE*(i_mm-1))) = 0.0d0
                  endif
               enddo
            enddo


         enddo
 999     close(10)

      enddo    !number of tie point files

      write(6,*) 'Number of tie points:      ',i_mm
      write(6,*) ' '

      do i=1,3
         do j=1,4
            r_rr(i,j,0) = 0.
         enddo
      enddo
      r_rr(1,1,0) = 1.
      r_rr(2,2,0) = 1.
      r_rr(3,3,0) = 1.

c     call svdvecfit to get the estimate for this iteration

      l_chisq = .false.
      call svdvecfit(i_mm,I_RDE,I_FPE,r_vecin,r_obs,r_cov,
     +     i_ma,r_aa,r_at2,r_u,r_v,r_w,r_chisq,l_chisq)  

      write(6,*) ' '

      do i=1,i_ma
         r_bb(i_ap(i),i_af(i)) = r_aa(i)
      enddo

      do i=1,i_mfc
         write(6,*) ' '
         write(6,*) ' '
         write(6,*) '      Matrix Results for Image ',i
         write(6,*) ' '

         do j=1,12
            if (i_bb(j,i) .ne. 0) then
               write(6,'(x,a,i3,a,i3,a,f15.5)') 'parameter(',j,',',i,') = ',r_bb(j,i)
            else
               write(6,'(x,a,i3,a,i3,a,f15.5,a)') 'parameter(',j,',',i,') = ',r_bb(j,i),'  (Fixed)'
            endif
         enddo


         call calcaff(r_bb(1,i),r_aff1)

         write(6,*) ' '
         write(6,*) ' Affine Matrix '
         write(6,*) ' '
         write(6,70) r_aff1(1,1), r_aff1(1,2),r_aff1(1,3)
         write(6,70) r_aff1(2,1), r_aff1(2,2),r_aff1(2,3)
         write(6,70) r_aff1(3,1), r_aff1(3,2),r_aff1(3,3)
 70      format(x,f20.8,x,f20.8,x,f20.8)
         write(6,*) ' '
         write(6,*) 'Translation Vector'
         write(6,*) ' '
         write(6,71) r_aff1(1,4),r_aff1(2,4),r_aff1(3,4)
 71      format(x,f20.5,x,f20.5,x,f20.5)

c     
c     decompose matrix
c     

         call swaprc(r_aff1)    ! Swap rows and columns of r_aff to get lower triangular matrix
                                !   as result.  Thanks to Ziad Haddad for deriving the following
                                !   identity
                                ! 
                                !   A = sQR(sAs)s = sOUs  = sOs sUs  =  O' L
         !
         !                    _         _
         !                   |  0  0  1  |
         !        where  s = |  0  1  0  |     QR() = QR decomp operator
         !                   |  1  0  0  |
         !                    -         -
         !
         !               0 = orthoganal matrix
         !               U = upper triangle matrix
         !               L = lower triangle matrix

         call qrdcmp(r_aff1,3,3,r_c,r_d,sing)

         r_qq1m = .5d0*(r_aff1(1,1)**2  + r_aff1(2,1)**2 + r_aff1(3,1)**2)
         r_qq1(1,1) = (1.d0 - (r_aff1(1,1)**2/r_qq1m))
         r_qq1(1,2) = (     - (r_aff1(1,1)*r_aff1(2,1)/r_qq1m))
         r_qq1(1,3) = (     - (r_aff1(1,1)*r_aff1(3,1)/r_qq1m))
         r_qq1(2,1) = (     - (r_aff1(2,1)*r_aff1(1,1)/r_qq1m))
         r_qq1(2,2) = (1.d0 - (r_aff1(2,1)**2/r_qq1m))
         r_qq1(2,3) = (     - (r_aff1(2,1)*r_aff1(3,1)/r_qq1m))
         r_qq1(3,1) = (     - (r_aff1(3,1)*r_aff1(1,1)/r_qq1m))
         r_qq1(3,2) = (     - (r_aff1(3,1)*r_aff1(2,1)/r_qq1m))
         r_qq1(3,3) = (1.d0 - (r_aff1(3,1)**2/r_qq1m))

         r_qq2m = .5d0*(r_aff1(2,2)**2 + r_aff1(3,2)**2)
         r_qq2(1,1) = (1.d0)
         r_qq2(1,2) = (0.d0)
         r_qq2(1,3) = (0.d0)
         r_qq2(2,1) = (0.d0)
         r_qq2(2,2) = (1.d0 - (r_aff1(2,2)**2/r_qq2m))
         r_qq2(2,3) = (     - (r_aff1(2,2)*r_aff1(3,2)/r_qq2m))
         r_qq2(3,1) = (0.d0)
         r_qq2(3,2) = (     - (r_aff1(3,2)*r_aff1(2,2)/r_qq2m))
         r_qq2(3,3) = (1.d0 - (r_aff1(3,2)**2/r_qq2m))

         call v_mult(3,3,r_qq1,3,3,r_qq2,r_rot)

         if(r_d(1) .lt. 0)then
            r_rot(1,1) = -r_rot(1,1)
            r_rot(2,1) = -r_rot(2,1)
            r_rot(3,1) = -r_rot(3,1)
            r_d(1) = -r_d(1)
            r_aff1(1,2) = -r_aff1(1,2)
            r_aff1(1,3) = -r_aff1(1,3)
         endif

         if(r_d(2) .lt. 0)then
            r_rot(1,2) = -r_rot(1,2)
            r_rot(2,2) = -r_rot(2,2)
            r_rot(3,2) = -r_rot(3,2)
            r_d(2) = -r_d(2)
            r_aff1(2,3) = -r_aff1(2,3)
         endif

         if(r_d(3) .lt. 0)then
            r_rot(1,3) = -r_rot(1,3)
            r_rot(2,3) = -r_rot(2,3)
            r_rot(3,3) = -r_rot(3,3)
            r_d(3) = -r_d(3)
         endif         
         
         r_rdr(1,1) = r_d(1)
         r_rdr(1,2) = r_aff1(1,2)
         r_rdr(1,3) = r_aff1(1,3)
         r_rdr(2,1) = 0.d0
         r_rdr(2,2) = r_d(2)
         r_rdr(2,3) = r_aff1(2,3)
         r_rdr(3,1) = 0.d0
         r_rdr(3,2) = 0.d0
         r_rdr(3,3) = r_d(3)

         call swaprc(r_rot)
         call swaprc(r_rdr)

c     write(6,*) ' '
c     write(6,*) ' Remainder Matrix '
c     write(6,'(x,3f12.4)') r_rdr(1,1),r_rdr(1,2),r_rdr(1,3)
c     write(6,'(x,3f12.4)') r_rdr(2,1),r_rdr(2,2),r_rdr(2,3)
c     write(6,'(x,3f12.4)') r_rdr(3,1),r_rdr(3,2),r_rdr(3,3)

         r_scale1 = abs(r_rdr(1,1))
         r_scale2 = abs(r_rdr(2,2))
         r_scale3 = abs(r_rdr(3,3))

         write(6,*) ' '
         write(6,*) ' Axis Scale Factors'
         write(6,*) ' '
         write(6,72) r_scale1,r_scale2,r_scale3
 72      format(x,f11.7,x,f11.7,x,f11.7)

         r_skew3 = r_rdr(2,1)/r_rdr(1,1)              
         r_skew1 = r_rdr(3,2)/r_rdr(2,2)              
         r_skew2 = r_rdr(3,1)/r_rdr(1,1)-r_skew1*r_skew3              

         write(6,*) ' '
         write(6,*) ' Skew Term'
         write(6,*) ' '
         write(6,72) r_skew1,r_skew2,r_skew3

         call eulerang(i_axisseq,r_rot,r_eulerangs)
         call axisrot(r_rot,r_rotaxis,r_rotang)

c     write(6,*) ' '
c     write(6,*) ' Rotation Matrix '
c     write(6,*) ' '
c     write(6,70) r_rot(1,1),r_rot(1,2),r_rot(1,3)
c     write(6,70) r_rot(2,1),r_rot(2,2),r_rot(2,3)
c     write(6,70) r_rot(3,1),r_rot(3,2),r_rot(3,3)

         write(6,*) ' '
         write(6,*) ' Rotation Angles'
         write(6,*) ' '
         write(6,'(x,a,f8.2,a,i1)')
     &        '    Euler       ',r_eulerangs(1)*r_rtod,'   Axis ',i_axisseq(1),
     &        '  Rotation    = ',r_eulerangs(2)*r_rtod,'   Axis ',i_axisseq(2),
     &        ' Angle (deg)    ',r_eulerangs(3)*r_rtod,'   Axis ',i_axisseq(3)

         write(6,*) ' '
         write(6,'(x,a,3f10.6)')  ' Rotation Axis  = ',r_rotaxis
         write(6,'(x,a,2x,f8.2)') ' Rotation Angle = ',r_rotang*r_rtod


c     
c     residual analysis
c     
         write(6,*) ' '
         write(6,*) '      Residual Analysis       '
         write(6,*) ' '
         r_c1t = 0.
         r_c2t = 0.
         r_c3t = 0.
         r_c1t2 = 0.
         r_c2t2 = 0.
         r_c3t2 = 0.
         i_mmm=0
         do i_m=1,i_mm
            if (i_sav1(i_m) .eq. i .or. i_sav2(i_m) .eq. i) then
               i1=i
               if (i_sav1(i_m) .eq. i) then
                  i2=i_sav2(i_m)
                  i11=0
                  i22=3
               else
                  i2=i_sav1(i_m)
                  i11=3
                  i22=0
               endif

               call calcaff(r_bb(1,i1),r_aff)
               call multitrn(r_rr(1,1,i1),r_aff,r_aff1)

               call invrstrn(r_aff1,r_inv1)

               if (i1 .ne. i2) then

                  call calcaff(r_bb(1,i2),r_aff)
                  call multitrn(r_rr(1,1,i2),r_aff,r_aff2)

               else
                  do ii=1,3
                     do jj=1,3
                        if (ii .eq. jj) then
                           r_aff2(ii,jj)=1.
                        else
                           r_aff2(ii,jj)=0.
                        endif
                     enddo
                     r_aff2(ii,4)=0.
                  enddo
               endif

               call multitrn(r_inv1,r_aff2,r_aff)

               i_mmm=i_mmm+1

               call applytrn(r_nul,r_vecin(i11+1,i_m),r_vec11)
               call applytrn(r_aff,r_vecin(i22+1,i_m),r_vec22)

               r_q1 = r_vec11(1)-r_vec22(1)
               r_q2 = r_vec11(2)-r_vec22(2)
               r_q3 = r_vec11(3)-r_vec22(3)
               r_c1t = r_c1t + r_q1
               r_c2t = r_c2t + r_q2
               r_c3t = r_c3t + r_q3
               r_c1t2 = r_c1t2 + r_q1**2
               r_c2t2 = r_c2t2 + r_q2**2
               r_c3t2 = r_c3t2 + r_q3**2

c     write(6,*) ' '
c     write(6,*) 'Point = ',i_mmm
c     write(6,101) 'Input Points Img1  = ',r_vecin(1,i_m),r_vecin(2,i_m),r_vecin(3,i_m)
c     write(6,101) 'Input Points Img2  = ',r_vecin(4,i_m),r_vecin(5,i_m),r_vecin(6,i_m)
c     write(6,101) 'Transformed Point1 = ',r_vec11
c     write(6,101) 'Transformed Point2 = ',r_vec22
c     write(6,101) 'Residuals          = ',r_q1,r_q2,r_q3
 101           format(x,a,x,3(f12.2,x))
               if (i_dump .eq. 1) write(6,'(x,i3,x,i3,x,3(f12.2,x))') i1,i2,r_q1,r_q2,r_q3
            endif
         enddo

         if (i_mmm .gt. 0) then
            write(6,*) ' '
            write(6,81) 'Mean error (across,down) = ',r_c1t/i_mmm,r_c2t/i_mmm,r_c3t/i_mmm
            write(6,81) ' STD  DEV  (across,down) = ',sqrt(r_c1t2/i_mmm - (r_c1t/i_mmm)**2),
     +           sqrt(r_c2t2/i_mmm - (r_c2t/i_mmm)**2),
     +           sqrt(r_c3t2/i_mmm - (r_c3t/i_mmm)**2)
            write(6,*) ' '
         endif

 81      format(x,a,x,3(f15.7,x))

         if (a_outfile .ne. ' ') then

            j=length(a_outfile)
            write(a_temp,'(a,a,i3.3)') a_outfile(1:j),'_',i
            open(unit=13,file=a_temp,form='formatted',status='unknown')

            call calcaff(r_bb(1,i),r_aff1)

            write(13,*) ' '
            write(13,*) ' Affine Matrix for file: ',i
            write(13,*) ' '
            write(13,70) r_aff1(1,1), r_aff1(1,2),r_aff1(1,3)
            write(13,70) r_aff1(2,1), r_aff1(2,2),r_aff1(2,3)
            write(13,70) r_aff1(3,1), r_aff1(3,2),r_aff1(3,3)
            write(13,*) ' '
            write(13,71) r_aff1(1,4),r_aff1(2,4),r_aff1(3,4)
            close(13)
            
         endif

      enddo

c     
c     more residual analysis
c     
      write(6,*) ' '
      write(6,*) '      More Residual Analysis       '
      write(6,*) ' '
      r_c1t = 0.
      r_c2t = 0.
      r_c3t = 0.
      r_c1t2 = 0.
      r_c2t2 = 0.
      r_c3t2 = 0.
      i_mmm=0
      do i_m=1,i_mm
         i1=i_sav1(i_m)
         i2=i_sav2(i_m)
         i11=0
         i22=3

         call calcaff(r_bb(1,i1),r_aff)
         call multitrn(r_rr(1,1,i1),r_aff,r_aff1)

         if (i1 .ne. i2) then

            call calcaff(r_bb(1,i2),r_aff)
            call multitrn(r_rr(1,1,i2),r_aff,r_aff2)

         else
            do ii=1,3
               do jj=1,3
                  if (ii .eq. jj) then
                     r_aff2(ii,jj)=1.
                  else
                     r_aff2(ii,jj)=0.
                  endif
               enddo
               r_aff2(ii,4)=0.
            enddo
         endif

         i_mmm=i_mmm+1

         call applytrn(r_aff1,r_vecin(i11+1,i_m),r_vec11)
         call applytrn(r_aff2,r_vecin(i22+1,i_m),r_vec22)

         r_q1 = r_vec11(1)-r_vec22(1)
         r_q2 = r_vec11(2)-r_vec22(2)
         r_q3 = r_vec11(3)-r_vec22(3)
         r_c1t = r_c1t + r_q1
         r_c2t = r_c2t + r_q2
         r_c3t = r_c3t + r_q3
         r_c1t2 = r_c1t2 + r_q1**2
         r_c2t2 = r_c2t2 + r_q2**2
         r_c3t2 = r_c3t2 + r_q3**2

c     write(6,*) ' '
c     write(6,*) 'Point = ',i_mmm
c     write(6,101) 'Input Points Img1  = ',r_vecin(1,i_m),r_vecin(2,i_m),r_vecin(3,i_m)
c     write(6,101) 'Input Points Img2  = ',r_vecin(4,i_m),r_vecin(5,i_m),r_vecin(6,i_m)
c     write(6,101) 'Transformed Point1 = ',r_vec11
c     write(6,101) 'Transformed Point2 = ',r_vec22
c     write(6,101) 'Residuals          = ',r_q1,r_q2,r_q3
         write(6,'(i5,2i3,3f10.2)') i_mmm,i1,i2,r_q1,r_q2,r_q3
      enddo

      if (i_mmm .gt. 0) then
         write(6,*) ' '
         write(6,81) 'Mean error (across,down) = ',r_c1t/i_mmm,r_c2t/i_mmm,r_c3t/i_mmm
         write(6,81) ' STD  DEV  (across,down) = ',sqrt(r_c1t2/i_mmm - (r_c1t/i_mmm)**2),
     +        sqrt(r_c2t2/i_mmm - (r_c2t/i_mmm)**2),
     +        sqrt(r_c3t2/i_mmm - (r_c3t/i_mmm)**2)
         write(6,*) ' '
      endif
      
      write(6,*) ' '
      stop 'Affine3d Done'

 910  stop 'Error opening command file'
 911  stop 'Error reading command file'
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
            if (a_string(i:i) .ne. ' ') then
               if (i_on .eq. 0) then
                  i_on = 1
                  i_cnt = 0
                  i_vals=min(i_vals+1,10)
                  a_vals(i_vals)=' '
               endif
               i_cnt = i_cnt+1
               a_vals(i_vals)(i_cnt:i_cnt) = a_string(i:i)
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

      subroutine calcaff(r_a,r_aff)

      real*8 r_a(12)
      real*8 r_aff(3,4)

      r_aff(1,1) =  r_a(1)
      r_aff(1,2) = -r_a(9)
      r_aff(1,3) =  r_a(8)
      r_aff(2,1) =  r_a(6) + r_a(9)
      r_aff(2,2) =  r_a(2)
      r_aff(2,3) = -r_a(7)
      r_aff(3,1) =  r_a(5) - r_a(8)
      r_aff(3,2) =  r_a(4) + r_a(7)
      r_aff(3,3) =  r_a(3)

      r_aff(1,4) =  r_a(10)
      r_aff(2,4) =  r_a(11)
      r_aff(3,4) =  r_a(12)

      return
      end

      subroutine swaprc(r_mat)
      
      implicit none

      integer i
      integer j

      real*8 r_mat(3,3)
      real*8 r_tmp(3,3)

      do i=1,3
         do j=1,3
            r_tmp(i,j) = r_mat(3+1-i,3+1-j)
         enddo
      enddo

      do i=1,3
         do j=1,3
            r_mat(i,j) = r_tmp(i,j)
         enddo
      enddo

      return

      end

      subroutine applytrn(r_aff,r_vec1,r_vec2)
c     
c     This subroutine applys an affine transforms to a vector
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
c     This subroutine combines two affine transforms into one
c     transform.
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
c     This subroutine finds the inverse of an affine transformation   
c     including the translation vector
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
     &     a13*(a21*a32-a22*a31)

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
c     This subroutine forms the product of a matrix "a" times
c     times the vector "b"
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

      integer ix,iy,ixy
      integer i_ax,i_ay
      integer i_bx,i_by
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

      integer ix,iy
      integer i_ax,i_ay
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

      integer ix
      integer i_ax
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

c****************************************************************

      subroutine euler(i_axisseq,r_eulerangs,r_tm)

c****************************************************************
c**   
c**   FILE NAME: euler.for
c**   
c**   DATE WRITTEN: 7/20/90
c**   
c**   PROGRAMMER:Scott Hensley
c**   
c**   FUNCTIONAL DESCRIPTION: Calculates the rotation matrix for an
c**   arbitary set of euler angles. Angles 1,2,3 are the counterwise
c**   rotations and Axes 1,2,3 identitfy the axes about which the 
c**   rotations are to be performed. The rotation matrix is then 
c**   passed out in the structure.
c**   
c**   ROUTINES CALLED:none
c**   
c**   NOTES: No special logic is included in this routine for the 
c**   one and two rotation cases; something must be passed that 
c**   that makes physical sense for the the other axes and angles.
c**   
c**   UPDATE LOG:
c**   
c*****************************************************************

      implicit none

c     INPUT VARIABLES:
      
      integer i_axis1           !first axis to rotate about
      integer i_axis2           !second axis to rotate about
      integer i_axis3           !third axis to rotate about

      integer i1
      integer i2                !used for eulerw
      integer i3 

      real*8 r_ang1             !angle of rotation about 1st axis
      real*8 r_ang2             !angle of rotation about 2nd axis
      real*8 r_ang3             !angle of rotation about 3rd axis

      real*8 r_s3,r_c3          !used in eulerw 

      real*8 r_tm(3,3)          !transforamtion matrix

      integer i_axisseq(3)
      real*8 r_eulerangs(3)

c     OUTPUT VARIABLES:see input

c     LOCAL VARIABLES:
      integer i_permute(3)
      real*8 r_s3s2,r_c3s2,r_s3c2,r_c3c2
      real*8 r_c1,r_c2,r_s1,r_s2

c     DATA STATEMENTS:
      data i_permute /2,3,1/         
      save i_permute

c     PROCESSING STEPS:

c     first compute some required trig values

      i_axis1=i_axisseq(1)
      i_axis2=i_axisseq(2)
      i_axis3=i_axisseq(3)

      r_ang1 = r_eulerangs(1)
      r_ang2 = r_eulerangs(2)
      r_ang3 = r_eulerangs(3)

      r_c1 = dcos(r_ang1)  
      r_c2 = dcos(r_ang2)  
      r_c3 = dcos(r_ang3)  

      i1 = i_axis1
      i2 = i_axis2
      i3 = i_permute(i2)

      if(i3 .eq. i1)then
         i3 = i_permute(i1)
         r_s1 = -dsin(r_ang1)
         r_s2 = -dsin(r_ang2)
         r_s3 = -dsin(r_ang3)
      else
         r_s1 = dsin(r_ang1)
         r_s2 = dsin(r_ang2)
         r_s3 = dsin(r_ang3)
      endif

      if(i_axis3 .eq. i_axis1)then
         
         r_s3c2 = r_s3*r_c2
         r_c3c2 = r_c3*r_c2
         
         r_tm(i1,i1) = r_c2           
         r_tm(i1,i2) = r_s2*r_s1
         r_tm(i1,i3) = -r_s2*r_c1          

         r_tm(i2,i1) = r_s3*r_s2           
         r_tm(i2,i2) = r_c3*r_c1 - r_s3c2*r_s1           
         r_tm(i2,i3) = r_c3*r_s1 + r_s3c2*r_c1           

         r_tm(i3,i1) = r_c3*r_s2           
         r_tm(i3,i2) = -r_s3*r_c1 - r_c3c2*r_s1           
         r_tm(i3,i3) = -r_s3*r_s1 + r_c3c2*r_c1          

      else

         r_s3s2 = r_s3*r_s2
         r_c3s2 = r_c3*r_s2
         
         r_tm(i1,i1) = r_c3*r_c2
         r_tm(i1,i2) = r_s3*r_c1 + r_c3s2*r_s1
         r_tm(i1,i3) = r_s3*r_s1 - r_c3s2*r_c1

         r_tm(i2,i1) = -r_s3*r_c2           
         r_tm(i2,i2) = r_c3*r_c1 - r_s3s2*r_s1           
         r_tm(i2,i3) = r_c3*r_s1 + r_s3s2*r_c1           

         r_tm(i3,i1) = r_s2
         r_tm(i3,i2) = -r_c2*r_s1
         r_tm(i3,i3) = r_c2*r_c1

      endif

      end     	

c****************************************************************

      subroutine eulerang(i_axisseq,r_rot,r_eulerangs)

c****************************************************************
c**   
c**   FILE NAME: eulerang.for
c**   
c**   DATE WRITTEN:  7/20/90 
c**   
c**   PROGRAMMER:Scott Hensley
c**   
c**   FUNCTIONAL DESCRIPTION: This routine will take a rotation matrix
c**   and factor it into an sequence of three Euler angle rotations 
c**   about a specified Euler angle sequence. 
c**   
c**   ROUTINES CALLED:none
c**   
c**   NOTES: none
c**   
c**   UPDATE LOG:
c**   
c*****************************************************************

      implicit none

c     INPUT VARIABLES:
      integer i_axisseq(3)      !Euler angle sequence
      real*8 r_rot(3,3)         !Rotation matrix
      
c     OUTPUT VARIABLES:
      real*8 r_eulerangs(3)     !Euler angles

c     LOCAL VARIABLES:
      integer m,i1,i2,i_col,i_sign,permute,j1,j2,i_s(3)
      real*8 r_trace,r_c,r_s

c     DATA STATEMENTS:

c     FUNCTION STATEMENTS:
      external permute

c     PROCESSING STEPS:

      if(i_axisseq(1) .eq. i_axisseq(3))then
         m = i_axisseq(1)
         i1 = i_axisseq(2)
         if(m .eq. 1)then
            if(i1 .eq. 2)Then
               i2 = 3
               i_s(1) = -1
               i_s(3) = 1
            else
               i2 = 2
               i_s(1) = 1
               i_s(3) = -1
            endif
         elseif(m .eq. 2)then
            if(i1 .eq. 1)then
               i2 = 3
               i_s(1) = 1
               i_s(3) = -1
            else
               i2 = 1
               i_s(1) = -1
               i_s(3) = 1
            endif
         elseif(m .eq. 3)then
            if(i1 .eq. 2)then
               i2 = 1
               i_s(1) = 1
               i_s(3) = -1
            else
               i2 = 2
               i_s(1) = -1
               i_s(3) = 1
            endif
         endif
         r_eulerangs(2) = acos(min(r_rot(m,m),1.d0))
         r_eulerangs(3) = atan2(r_rot(i1,m),i_s(3)*r_rot(i2,m))
         r_eulerangs(1) = atan2(r_rot(m,i1),i_s(1)*r_rot(m,i2))
      else
         i1 = i_axisseq(3)
         i2 = i_axisseq(1)
         m = i_axisseq(2)
         i_sign = permute(i_axisseq)
         r_eulerangs(2) = asin(max(min(r_rot(i1,i2),1.d0),-1.d0)*i_sign)
         r_eulerangs(1) = atan2(-i_sign*r_rot(i1,m),r_rot(i1,i1))
         r_eulerangs(3) = atan2(-i_sign*r_rot(m,i2),r_rot(i2,i2))
      endif
      
      end 

c***************************************************************

      integer function permute(i_a)

      implicit none
      integer i_a(3),i_b(3),i_sign,i,i_t

      i_sign = 1
      do i=1,3
         i_b(i) = i_a(i)
      enddo
      if(i_b(2) .gt. i_b(1))then
         i_t = i_b(2)
         i_b(2) = i_b(1)
         i_b(1) = i_t
         i_sign = -1*i_sign
      endif
      if(i_b(3) .gt. i_b(1))then
         i_t = i_b(3)
         i_b(3) = i_b(1)
         i_b(1) = i_t
         i_sign = -1*i_sign
      endif
      if(i_b(3) .gt. i_b(2))then
         i_t = i_b(3)
         i_b(3) = i_b(2)
         i_b(2) = i_t
         i_sign = -1*i_sign
      endif

      permute = -i_sign

      end 

c****************************************************************

      subroutine axisrot(r_rot,r_rotaxis,r_rotang)

c****************************************************************
c**   
c**   FILE NAME: axisrot.for
c**   
c**   DATE WRITTEN: 7/20/90 
c**   
c**   PROGRAMMER:Scott Hensley
c**   
c**   FUNCTIONAL DESCRIPTION: This routine will take an arbitary rotation
c**   matrix and find the Euler axis of rotation and the rotation angle
c**   about that axis. 
c**   
c**   ROUTINES CALLED:none
c**   
c**   NOTES: none
c**   
c**   UPDATE LOG:
c**   
c*****************************************************************

      implicit none

c     INPUT VARIABLES:
      real*8 r_rot(3,3)         !rotation matrix
      
c     OUTPUT VARIABLES:
      real*8 r_rotaxis(3)       !rotation axis
      real*8 r_rotang           !rotation angle

c     LOCAL VARIABLES:
      real*8 r_trace,r_cosrotang,r_sinrot

c     DATA STATEMENTS:none

C     FUNCTION STATEMENTS:none

c     PROCESSING STEPS:

c     determine the rotation angle

      r_trace = max(min(r_rot(1,1) + r_rot(2,2) + r_rot(3,3),3.0d0),-1.0d0)
      r_cosrotang = .5*(r_trace-1)
      r_rotang = acos(r_cosrotang)

c     determine the rotation axis if possible

      if(r_trace .ne. 3.0 .and. r_trace .ne. -1.0)then
         r_sinrot = sin(r_rotang)
         r_rotaxis(1) = (.5/r_sinrot)*(r_rot(2,3) - r_rot(3,2))
         r_rotaxis(2) = (.5/r_sinrot)*(r_rot(3,1) - r_rot(1,3))
         r_rotaxis(3) = (.5/r_sinrot)*(r_rot(1,2) - r_rot(2,1))
      elseif(r_trace .eq. -1)then
         r_rotaxis(1) = sqrt((1. + r_rot(1,1))/2.)
         r_rotaxis(2) = sqrt((1. + r_rot(2,2))/2.)
         r_rotaxis(3) = sqrt((1. + r_rot(3,3))/2.)
         if(r_rot(1,2) .lt. 0)then
            if(r_rot(2,3) .le. 0)then
               r_rotaxis(1) = -r_rotaxis(1)
            elseif(r_rot(2,3) .gt. 0)then
               r_rotaxis(2) = -r_rotaxis(2)
            endif
         elseif(r_rot(1,2) .gt. 0)then
            if(r_rot(2,3) .le. 0)then
               r_rotaxis(3) = -r_rotaxis(3)
            endif
         elseif(r_rot(1,2) .eq. 0)then
            if(r_rotaxis(1) .eq. 0)then
               if(r_rot(2,3) .lt. 0)then           
                  r_rotaxis(2) = -r_rotaxis(2)
               endif
            else
               if(r_rot(3,1) .lt. 0)then
                  r_rotaxis(2) = -r_rotaxis(2)
               endif
            endif   
         endif
      else                      ! r_trace = 3  out of luck
         r_rotaxis(1) = 0.
         r_rotaxis(2) = 0.
         r_rotaxis(3) = 0.
      endif
      
      end  

c*****************************************************************************

      subroutine funcs(i_ip,i_rd,i_fp,r_vecin,i_np,r_a,r_amat)

c     funcs is the partial derivatives of an affine matrix fit wrt 
c     where the affine matrix is of the form

c            _              _       _         _       _         _       _         _
c           |   L1   0   0   |     |  0  0  0  |     |  0  0  0  |     |  0  0  0  |
c     mat = |    0  L2   0   | + K1|  0  0  0  | + K2|  0  0  0  | + K3|  1  0  0  | +
c           |_   0   0  L3  _|     |_ 0  1  0 _|     |_ 1  0  0 _|     |_ 0  0  0 _|
c     
c                _         _       _         _       _         _         _    _
c               |  0  0  0  |     |  0  0  1  |     |  0 -1  0  |       |  T1  |
c             Q1|  0  0 -1  | + Q2|  0  0  0  | + Q3|  1  0  0  |       |  T2  | 
c               |_ 0  1  0 _|     |_-1  0  0 _|     |_ 0  0  0 _|,      |_ T3 _|
c     
      implicit none

c     PARAMETERS:
      integer I_RDE
      integer I_MFE
      integer I_MTE
      integer I_NPE
      integer I_MPE
      parameter(I_RDE =     3) ! Max number of observations
      parameter(I_MFE =   100) ! Max number of image strips
      parameter(I_MTE =   400) ! Max number of tiepoint files
      parameter(I_NPE =   500) ! Max number of parameters to estimate
      parameter(I_MPE = 40000) ! Max number of measurements

      integer i,j,k,l
      integer i1,i2

      integer i_ip
      integer i_rd
      integer i_fp
      integer i_np
      integer i_tp

      real*8 r_a(i_np)
      real*8 r_vecin(i_fp)
      real*8 r_amat(i_rd,i_np)

      real*8 r_temp(3,12)

      real*8 r_x,r_y,r_z

      real*8 r_rr(3,4,0:I_MFE)
      integer i_lko(I_MTE),i_af(I_NPE),i_ap(I_NPE)
      common/consts/ r_rr,i_lko,i_af,i_ap

      integer i_paramest(I_NPE),i_usedata(I_RDE*I_MTE),i_covopt
      common/funcom/i_paramest,i_usedata,i_covopt


      if (i_fp .ne.     9) stop 'ERROR - i_fp not equal to 9 in funcs'
      if (i_rd .ne. I_RDE) stop 'ERROR - i_rd not equal to I_RDE in funcs'
      if (i_np .gt. I_NPE) stop 'ERROR - i_np greater than I_NPE in funcs'

      i1=nint(r_vecin(7))
      i2=nint(r_vecin(8))
      i_tp=nint(r_vecin(9))

      do i=1,i_rd
         do j=1,i_np
            r_amat(i,j) = 0.
         enddo
      enddo

      r_x=r_vecin(1)
      r_y=r_vecin(2)
      r_z=r_vecin(3)

      r_temp(1, 1) = r_rr(1,1,i1)*r_x ! dY1/dL1
      r_temp(1, 2) = r_rr(1,2,i1)*r_y ! dY1/dL2
      r_temp(1, 3) = r_rr(1,3,i1)*r_z ! dY1/dL3
      r_temp(1, 4) = r_rr(1,3,i1)*r_y ! dY1/dK1
      r_temp(1, 5) = r_rr(1,3,i1)*r_x ! dY1/dK2
      r_temp(1, 6) = r_rr(1,2,i1)*r_x ! dY1/dk3
      r_temp(1, 7) =-r_rr(1,2,i1)*r_z + r_rr(1,3,i1)*r_y ! dY1/dQ1
      r_temp(1, 8) = r_rr(1,1,i1)*r_z - r_rr(1,3,i1)*r_x ! dY1/dQ2
      r_temp(1, 9) =-r_rr(1,1,i1)*r_y + r_rr(1,2,i1)*r_x ! dY1/dQ3
      r_temp(1,10) = r_rr(1,1,i1) ! dY1/dT1
      r_temp(1,11) = r_rr(1,2,i1) ! dY1/dT2   
      r_temp(1,12) = r_rr(1,3,i1) ! dY1/dT3
      
      r_temp(2, 1) = r_rr(2,1,i1)*r_x ! dY2/dL1
      r_temp(2, 2) = r_rr(2,2,i1)*r_y ! dY2/dL2
      r_temp(2, 3) = r_rr(2,3,i1)*r_z ! dY2/dL3
      r_temp(2, 4) = r_rr(2,3,i1)*r_y ! dY2/dK1
      r_temp(2, 5) = r_rr(2,3,i1)*r_x ! dY2/dK2
      r_temp(2, 6) = r_rr(2,2,i1)*r_x ! dY2/dk3
      r_temp(2, 7) =-r_rr(2,2,i1)*r_z + r_rr(2,3,i1)*r_y ! dY2/dQ1
      r_temp(2, 8) = r_rr(2,1,i1)*r_z - r_rr(2,3,i1)*r_x ! dY2/dQ2
      r_temp(2, 9) =-r_rr(2,1,i1)*r_y + r_rr(2,2,i1)*r_x ! dY2/dQ3
      r_temp(2,10) = r_rr(2,1,i1) ! dY2/dT1
      r_temp(2,11) = r_rr(2,2,i1) ! dY2/dT2
      r_temp(2,12) = r_rr(2,3,i1) ! dY2/dT3
      
      r_temp(3, 1) = r_rr(3,1,i1)*r_x ! dY3/dL1
      r_temp(3, 2) = r_rr(3,2,i1)*r_y ! dY3/dL2
      r_temp(3, 3) = r_rr(3,3,i1)*r_z ! dY3/dL3
      r_temp(3, 4) = r_rr(3,3,i1)*r_y ! dY3/dK1
      r_temp(3, 5) = r_rr(3,3,i1)*r_x ! dY3/dK2
      r_temp(3, 6) = r_rr(3,2,i1)*r_x ! dY3/dk3
      r_temp(3, 7) =-r_rr(3,2,i1)*r_z + r_rr(3,3,i1)*r_y ! dY3/dQ1
      r_temp(3, 8) = r_rr(3,1,i1)*r_z - r_rr(3,3,i1)*r_x ! dY3/dQ2
      r_temp(3, 9) =-r_rr(3,1,i1)*r_y + r_rr(3,2,i1)*r_x ! dY3/dQ3
      r_temp(3,10) = r_rr(3,1,i1) ! dY3/dT1
      r_temp(3,11) = r_rr(3,2,i1) ! dY3/dT2
      r_temp(3,12) = r_rr(3,3,i1) ! dY3/dT3
      
      do k=1,3
         do l=1,i_np
            if (i_af(l) .eq. i1) then
               r_amat(k,l) = r_temp(k,i_ap(l))
            endif
         enddo
      enddo

      if (i1 .ne. i2) then
         r_x=r_vecin(4)
         r_y=r_vecin(5)
         r_z=r_vecin(6)

         r_temp(1, 1) = r_rr(1,1,i2)*r_x ! dY1/dL1
         r_temp(1, 2) = r_rr(1,2,i2)*r_y ! dY1/dL2
         r_temp(1, 3) = r_rr(1,3,i2)*r_z ! dY1/dL3
         r_temp(1, 4) = r_rr(1,3,i2)*r_y ! dY1/dK1
         r_temp(1, 5) = r_rr(1,3,i2)*r_x ! dY1/dK2
         r_temp(1, 6) = r_rr(1,2,i2)*r_x ! dY1/dk3
         r_temp(1, 7) =-r_rr(1,2,i2)*r_z + r_rr(1,3,i2)*r_y ! dY1/dQ1
         r_temp(1, 8) = r_rr(1,1,i2)*r_z - r_rr(1,3,i2)*r_x ! dY1/dQ2
         r_temp(1, 9) =-r_rr(1,1,i2)*r_y + r_rr(1,2,i2)*r_x ! dY1/dQ3
         r_temp(1,10) = r_rr(1,1,i2) ! dY1/dT1
         r_temp(1,11) = r_rr(1,2,i2) ! dY1/dT2   
         r_temp(1,12) = r_rr(1,3,i2) ! dY1/dT3
         
         r_temp(2, 1) = r_rr(2,1,i2)*r_x ! dY2/dL1
         r_temp(2, 2) = r_rr(2,2,i2)*r_y ! dY2/dL2
         r_temp(2, 3) = r_rr(2,3,i2)*r_z ! dY2/dL3
         r_temp(2, 4) = r_rr(2,3,i2)*r_y ! dY2/dK1
         r_temp(2, 5) = r_rr(2,3,i2)*r_x ! dY2/dK2
         r_temp(2, 6) = r_rr(2,2,i2)*r_x ! dY2/dk3
         r_temp(2, 7) =-r_rr(2,2,i2)*r_z + r_rr(2,3,i2)*r_y ! dY2/dQ1
         r_temp(2, 8) = r_rr(2,1,i2)*r_z - r_rr(2,3,i2)*r_x ! dY2/dQ2
         r_temp(2, 9) =-r_rr(2,1,i2)*r_y + r_rr(2,2,i2)*r_x ! dY2/dQ3
         r_temp(2,10) = r_rr(2,1,i2) ! dY2/dT1
         r_temp(2,11) = r_rr(2,2,i2) ! dY2/dT2
         r_temp(2,12) = r_rr(2,3,i2) ! dY2/dT3
         
         r_temp(3, 1) = r_rr(3,1,i2)*r_x ! dY3/dL1
         r_temp(3, 2) = r_rr(3,2,i2)*r_y ! dY3/dL2
         r_temp(3, 3) = r_rr(3,3,i2)*r_z ! dY3/dL3
         r_temp(3, 4) = r_rr(3,3,i2)*r_y ! dY3/dK1
         r_temp(3, 5) = r_rr(3,3,i2)*r_x ! dY3/dK2
         r_temp(3, 6) = r_rr(3,2,i2)*r_x ! dY3/dk3
         r_temp(3, 7) =-r_rr(3,2,i2)*r_z + r_rr(3,3,i2)*r_y ! dY3/dQ1
         r_temp(3, 8) = r_rr(3,1,i2)*r_z - r_rr(3,3,i2)*r_x ! dY3/dQ2
         r_temp(3, 9) =-r_rr(3,1,i2)*r_y + r_rr(3,2,i2)*r_x ! dY3/dQ3
         r_temp(3,10) = r_rr(3,1,i2) ! dY3/dT1
         r_temp(3,11) = r_rr(3,2,i2) ! dY3/dT2
         r_temp(3,12) = r_rr(3,3,i2) ! dY3/dT3
         
         do k=1,3
            do l=1,i_np
               if (i_af(l) .eq. i2) then
                  r_amat(k,l) = -r_temp(k,i_ap(l))
               endif
            enddo
         enddo

      endif

c     set the derivatives of all the parameters not being estimated to zero 

      do i=1,i_np
         do j=1,i_rd
c            type *,i,j,i_tp,i_lko(i_tp)+j,i_usedata(i_lko(i_tp)+j)
c            type *,'amat=',i,j,i_paramest(i),i_usedata(i_lko(i_tp)+j),r_amat( j,i )
            r_amat(j,i) = r_amat(j,i)*i_paramest(i)*i_usedata(i_lko(i_tp)+j)
         enddo
      enddo

      return

      end    

c****************************************************************
      subroutine svdvecfit(i_mp,i_rd,i_fp,r_vecin,r_vobs,r_cov,
     +     i_np,r_a,r_at2,r_u,r_v,r_w,r_chisq,l_chisq)

c****************************************************************
c**   
c**   FILE NAME: svdvecfit.f
c**   
c**   DATE WRITTEN: 01/02/95 
c**   
c**   PROGRAMMER: Scott Hensley
c**   
c**   FUNCTIONAL DESCRIPTION: This routine does a least squares fit 
c**   to a vector valued observation least squares problem.
c**   
c**   ROUTINES CALLED: gaussj,svbksb,svdcmp,funcs
c**   
c**   NOTES: funcs is a user supplied function giving the jacobian
c**   of the observation parameters wrt to fit parameters. This routine
c**   is a generalization of Numerical Recipes svdfit. Note that this
c**   routine can also be used in a nonlinear least squares procedure
c**   by iterating properly.
c**   
c**   Solves the least problem 
c**   
c**   T   -1     -1     T   -1 
c**   A = (AMAT COV  AMAT)  (AMAT COV  )VOBS 
c**   
c**   where AMAT is the jacobain of the observations vs parameters,
c**   COV is the covriance matrix of observations
c**   and VOBS is the vector of observations. 
c**   
c**   r_a should be passed in with current best estimate of values
c**   
c**   UPDATE LOG: 
c**   
c**   4/17/95 - Reversed order of r_vecin, r_vobs, and r_cov    SJS
c**   revmoved r_vt, cleaned up parameter list
c**   
c*****************************************************************

      implicit none

c     PARAMETERS:
      integer I_RDE
      integer I_MFE
      integer I_MTE
      integer I_NPE
      integer I_MPE
      parameter(I_RDE =     3) ! Max number of observations
      parameter(I_MFE =   100) ! Max number of image strips
      parameter(I_MTE =   400) ! Max number of tiepoint files
      parameter(I_NPE =   500) ! Max number of parameters to estimate
      parameter(I_MPE = 40000) ! Max number of measurements

      real*8 R_TOL
      real*8 R_LAMBDA
      parameter(R_TOL = 1.0d-20)
      parameter(R_LAMBDA = 1.d0)

c     INPUT VARIABLES:
      integer i_mp              !number of input points
      integer i_rd              !number of observations each point
      integer i_fp              !number of input parameters to func
      integer i_np              !number of parameters to solve for

      real*8  r_vecin(i_fp,i_mp) !vector values for func 
      real*8  r_vobs(i_rd,i_mp) !vector of observations
      real*8  r_cov(i_rd,i_rd,i_mp) !covariance matrix of observation
      real*8  r_chisq(i_rd,0:i_mp) !chisq for solution and fit vs observation 
      real*8  r_a(i_np)         !solution to least squares
                                !for each point 
      logical l_chisq           !evaluate the chisq for this fit
      
c     OUTPUT VARIABLES:
      real*8 r_at2(i_np)        !delta to add to previous solution
      real*8 r_u(i_np,i_np)     !svd matrix, orthogonal matrix
      real*8 r_v(i_np,i_np)     !svd matrix, orthogonal matrix
      real*8 r_w(i_np)          !svd matrix, diagonal matrix

c     LOCAL VARIABLES:
      integer i,j,k,i_pts
      real*8  r_covtemp(I_RDE,I_RDE)
      real*8  r_am(I_RDE,I_NPE)
      real*8  r_amat(I_RDE,I_NPE)
      real*8  r_ptot(I_NPE)
      real*8  r_wmax,r_thres,r_b(I_RDE,1),r_chird(I_RDE)

      integer i_paramest(I_NPE),i_usedata(I_RDE*I_MTE),i_covopt
      common/funcom/i_paramest,i_usedata,i_covopt

c     DATA STATEMENTS:

C     FUNCTION STATEMENTS:

c     PROCESSING STEPS:

c     init some arrays

c     type*, ' '
c     type*, 'Inside SVDVECFIT'
c     type*, ' '

      if (i_rd .ne. I_RDE) stop 'ERROR - i_rd not equal to I_RDE in SVDVECFIT'
      if (i_np .gt. I_NPE) stop 'ERROR - i_np greater than I_NPE in SVDVECFIT'

      do i=1,i_np
         do j=1,i_np
            r_u(i,j) = 0.0
         enddo
         r_ptot(i) = 0.0
      enddo

c     loop over the input points

      do i_pts=1,i_mp

         if(mod(i_pts,1000) .eq. 0)then
            write(6,*) 'SVDVECFIT point number: ',i_pts
         endif

c     invert the covariance matrix of the observation

         if (i_covopt .eq. 1) then
            do i=1,i_rd
               do j=1,i_rd
                  if (i .eq. j) then
                     r_covtemp(i,j) = 1.0d0
                  else
                     r_covtemp(i,j) = 0.0d0
                  endif
               enddo
            enddo
         elseif(i_covopt .eq. 2)then !invert diagonal matrix
            do i=1,i_rd
               do j=1,i_rd
                  if (i .eq. j) then
                     r_covtemp(i,j) = 1.d0/r_cov(i,j,i_pts)
                  else
                     r_covtemp(i,j) = 0.d0
                  endif
               enddo
            enddo
         elseif(i_covopt .eq. 3)then !do a full matrix inversion
            do i=1,i_rd
               do j=1,i_rd
                  r_covtemp(i,j) = r_cov(i,j,i_pts)
               enddo
            enddo
            call gaussj(r_covtemp,i_rd,i_rd,r_b,1,1)
         endif

c     get the required jacobian matrix

         call funcs(i_pts,I_RDE,i_fp,r_vecin(1,i_pts),i_np,r_a,r_amat)

c         do i=1,i_rd
c            do j=1,i_np
c               type*, 'i,j,r_amat = ',i,j,r_amat(i,j)
c            enddo
c         enddo

c     multiply amat transpose by the inverse cov matrix

         do i=1,i_np
            do j=1,i_rd
               r_am(j,i) = 0.0
               do k=1,i_rd
                  r_am(j,i) = r_am(j,i) + r_amat(k,i)*r_covtemp(k,j)
               enddo
            enddo
         enddo

c     do i=1,i_np
c     do j=1,i_rd
c     type*, 'i,j,r_am = ',i,j,r_am(j,i)
c     enddo
c     enddo

c     multiply am by amat

         do i=1,i_np
            do j=1,i_np
               do k=1,i_rd
                  r_u(i,j) = r_u(i,j) + r_am(k,i)*r_amat(k,j)
               enddo
            enddo
         enddo

c     multilpy am by vobs


c     type*, 'r_vobs,i_pts = ',i_pts,r_vobs(1,i_pts),r_vobs(2,i_pts)
         do i=1,i_np
            do k=1,i_rd
               r_ptot(i) = r_ptot(i) + r_am(k,i)*r_vobs(k,i_pts)
            enddo
         enddo
         
      enddo                     !i_pts

c     find the SVD of the r_u matrix

c     do i=1,i_np
c     do j=1,i_np
c     type*, 'i,j,r_u = ',i,j,r_u(i,j)
c     enddo
c     enddo

      call svdcmp(r_u,i_np,i_np,i_np,i_np,r_w,r_v)

c     do i=1,i_np
c     do j=1,i_np
c     type*, 'i,j,r_u,r_v = ',i,j,r_u(i,j),r_v(i,j)
c     enddo
c     enddo

c     do i=1,i_np
c     type*, 'w = ',i,r_w(i)
c     enddo

c     kill off all the singular values

      r_wmax = 0.0
      do i=1,i_np
         if(r_w(i) .gt. r_wmax)then
            r_wmax = r_w(i)
         endif
      enddo
      r_thres = r_wmax*R_TOL
c     type*, 'r_thres = ',r_thres

      do i=1,i_np
         if(r_w(i) .lt. r_thres)then
            r_w(i) = 0.0
         endif
      enddo

c     do i=1,i_np
c     type*, 'w = ',i,r_w(i)
c     enddo

c     use the svbksb routine to solve for the desired parameters

      call svbksb(r_u,r_w,r_v,i_np,i_np,i_np,i_np,r_ptot,r_at2)

c     update the r_a vector

      do i=1,i_np
         r_at2(i) = -r_at2(i)*i_paramest(i)
         r_a(i) = r_at2(i)/R_LAMBDA + r_a(i)
c     type *,'a=',i,r_a(i),r_at2(i)
      enddo

c     evaluate the chisq array (linearized version)

      if(l_chisq)then

c     loop over data points


         do i=1,i_rd
            r_chird(i) = 0.
         enddo
         r_chisq(1,0) = 0.0
         do i=1,i_mp

            call funcs(i,I_RDE,i_fp,r_vecin(1,i),i_np,r_a,r_amat)

            do j=1,i_rd
               r_chisq(j,i) = 0.0
               do k=1,i_np
                  r_chisq(j,i) = r_chisq(j,i) + r_amat(j,k)*r_at2(k)
               enddo
c     type*, 'r_chisq = ',i,j,r_chisq(j,i),r_vobs(j,i)
               r_chisq(j,i) = r_covtemp(j,j)*(r_chisq(j,i) - 
     +              r_vobs(j,i))**2
               r_chisq(1,0) = r_chisq(1,0) + r_chisq(j,i)
               r_chird(j) = r_chird(j) + r_chisq(j,i)
            enddo

         enddo                  !i_pts loop for chisq

         r_chisq(1,0) = sqrt(r_chisq(1,0)/(2.*i_mp))
         print *, 'r_chisq = ',r_chisq(1,0),sqrt(r_chird(1)/i_mp),sqrt(r_chird(2)/i_mp)

      endif
      
      end  

c     ======================================================================
c     
      integer function length(a_string)
c     
c     This function returns the position of the last none blank character
c     in the string
c     
c     Date  :      6/29/93
c     Author:      Scott Shaffer
c     
c     Input:  a_string
c     
c     Output: length
c     
c     ======================================================================
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

****************************************************************
      subroutine read_hdr(a_hdrfile,a_filter,a_type,i_lsize,i_ssize,r_peg,i_zone,
     &     r_str,r_spc,i_err)

c****************************************************************
c**   
c**   FILE NAME: read_hdr.f
c**   
c**   DATE WRITTEN: 2/15/96
c**   
c**   PROGRAMMER:Scott Shaffer
c**   
c**   FUNCTIONAL DESCRIPTION: Reads some of an IFPROC header file.
c**   
c**   ROUTINES CALLED:none
c**   
c**   NOTES: 
c**   
c**   
c*****************************************************************

      implicit none

c     INPUT VARIABLES:
      
      character*(*) a_hdrfile   !header file
      character*(*) a_filter

c     OUTPUT VARIABLES: 

      character*(*) a_type
      integer*4 i_err
      integer*4 i_lsize
      integer*4 i_ssize
      integer*4 i_zone

      real*8 r_peg(3)
      real*8 r_str(2)
      real*8 r_spc(2)


c     LOCAL VARIABLES: 

      integer*4 i
      integer*4 i_cnt
      real*8 r_atm(3,4)
      real*8 r_pi
      real*8 r_rtod

      character*120 a_tmp

c     DATA STATEMENTS: none

C     FUNCTION STATEMENTS: 
      integer length
      external length

c     PROCESSING STEPS:
      
c     
c     Initialize pi and conversions
c     
      r_pi = 4.d0*atan(1.0d0)
      r_rtod = 180.0d0/r_pi

      i_err=1
      i_cnt=0

      open(12,file=a_hdrfile,status='old',form='formatted')
c      write(6,*) ' '
c      write(6,*) 'Opening hdr input  file: ',a_hdrfile(1:52)

      do i=1,10000000
         read(12,'(a)',end=900) a_tmp
         if (index(a_tmp,a_filter(:max(length(a_filter),1))) .eq. 0) then
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
         else if (index(a_tmp,'Zone') .gt. 0) then
            read(a_tmp,*) i_zone
         else if (index(a_tmp,'Starting corner position (s,c)') .gt. 0) then
            read(a_tmp,*) r_str
            i_cnt = i_cnt + 8
         else if (index(a_tmp,'Data file type') .gt. 0) then
            a_type = a_tmp(index(a_tmp,';')-4:index(a_tmp,';')-2)
         else if (index(a_tmp,'M11 M12 M13') .gt. 0) then
            read(a_tmp,*) r_atm(1,1),r_atm(1,2),r_atm(1,3)
c     i_cnt = i_cnt + 16
         else if (index(a_tmp,'M21 M22 M23') .gt. 0) then
            read(a_tmp,*) r_atm(2,1),r_atm(2,2),r_atm(2,3)
c     i_cnt = i_cnt + 32
         else if (index(a_tmp,'M31 M32 M33') .gt. 0) then
            read(a_tmp,*) r_atm(3,1),r_atm(3,2),r_atm(3,3)
c     i_cnt = i_cnt + 64
         else if (index(a_tmp,'O1 O2 O3') .gt. 0) then
            read(a_tmp,*) r_atm(1,4),r_atm(2,4),r_atm(3,4)
c     i_cnt = i_cnt + 128
         endif
      enddo
      close(12)
      stop 'Error reading header file, too many lines'

 900  close(12,err=910)
 910  i_err = 0
      return
      end

c****************************************************************

      subroutine tcnatm(r_a,r_e2,r_peg,r_atm)

c****************************************************************
c**   
c**   FILE NAME: tcnatm.for
c**   
c**   DATE WRITTEN:10/25/95 
c**   
c**   PROGRAMMER:Scott Shaffer
c**   
c**   FUNCTIONAL DESCRIPTION:This routine computes the transformation
c**   matris and translation vector needed to get between radar (t,c,n)
c**   coordinates and (x,y,z) WGS-84 coordinates.
c**   
c**   ROUTINES CALLED:
c**   
c**   NOTES: none
c**   
c**   UPDATE LOG:
c**   
c*****************************************************************

      implicit none

c     INPUT VARIABLES:
      real*8 r_a                !semimajor axis
      real*8 r_e2               !eccentricity squared
      real*8 r_peg(3)           !peg latitude,longitude,heading
      
c     OUTPUT VARIABLES:
      real*8 r_atm(3,4)         !rotation matris
      
c     LOCAL VARIABLES:
      integer i_type
      real*8 r_hgt
      real*8 r_slt,r_clt,r_clo,r_slo,r_chg,r_shg

      real*8 rdir
      external rdir

c     DATA STATEMENTS:none

c     PROCESSING STEPS:

c     first determine the rotation matris

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

c     find the translation vector

      i_type = 1
      r_hgt = 0.d0
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
      integer i_type            !1=lat,lon to vector,2= vector to lat,lon
      real*8 r_a                !ellispoid semi-major axis
      real*8 r_e2               !ellipsoid eccentricity squared  
      real*8 r_v(3)             !geocentric vector (meters)
      real*8 r_lat              !latitude (deg -90 to 90)
      real*8 r_lon              !longitude (deg -180 to 180)
      real*8 r_hgt              !height above ellipsoid (meters)
      
c     OUTPUT VARIABLES:see input

c     LOCAL VARIABLES:
      integer i_ft
      real*8 pi,r_dtor,r_re,r_q2,r_q3,r_b,r_q
      real*8 r_p,r_tant,r_theta

c     DATA STATEMENTS:
      data pi /3.141592653589793238d0/
      data r_dtor /1.74532925199d-2/
      data i_ft /0/

C     FUNCTION STATEMENTS:

c     PROCESSING STEPS:

      if(i_type .eq. 1)then     !convert lat,lon to vector
         
         r_re = r_a/sqrt(1.d0 - r_e2*sin(r_lat)**2)
         
         r_v(1) = (r_re + r_hgt)*cos(r_lat)*cos(r_lon)
         r_v(2) = (r_re + r_hgt)*cos(r_lat)*sin(r_lon)
         r_v(3) = (r_re*(1.d0-r_e2) + r_hgt)*sin(r_lat)               
         
      elseif(i_type .eq. 2)then !convert vector to lat,lon 
         
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
     +        (r_p - r_e2*r_a*cos(r_theta)**3)
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
      integer i_type            !1=lat,lon to vector,2= vector to lat,lon
      real*8 r_a                !ellispoid semi-major axis
      real*8 r_v(3)             !geocentric vector (meters)
      real*8 r_lat              !latitude (deg -90 to 90)
      real*8 r_lon              !longitude (deg -180 to 180)
      real*8 r_hgt              !height above ellipsoid (meters)
      
c     OUTPUT VARIABLES:see input

c     LOCAL VARIABLES:
      real*8 r_p

C     FUNCTION STATEMENTS:

c     PROCESSING STEPS:

      if(i_type .eq. 1)then     !convert lat,lon to vector
         
         r_v(3) = (r_a + r_hgt)*cos(r_lat)*cos(r_lon) - r_a
         r_v(1) = (r_a + r_hgt)*cos(r_lat)*sin(r_lon)
         r_v(2) = (r_a + r_hgt)*sin(r_lat)               
         
      elseif(i_type .eq. 2)then !convert vector to lat,lon, hgt
         
         r_p = sqrt(r_v(1)**2 + r_v(2)**2 + (r_v(3)+r_a)**2)
         r_lat = asin(r_v(2)/r_p)
         r_lon = atan2(r_v(1),(r_v(3)+r_a))
         r_hgt = r_p - r_a          
         
      endif
      
      return
      end  

c****************************************************************
c     
c     Various curvature functions
c     
c     
c****************************************************************
c**   
c**   FILE NAME: curvature.f
c**   
c**   DATE WRITTEN: 12/02/93
c**   
c**   PROGRAMMER:Scott Hensley
c**   
c**   FUNCTIONAL DESCRIPTION: This routine computes the curvature for 
c**   of various types required for ellipsoidal or spherical earth 
c**   calculations.  
c**   
c**   ROUTINES CALLED:none
c**   
c**   NOTES: none
c**   
c**   UPDATE LOG:
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
     1     (1.d0 - r_e2*sin(r_lat)**2)**(1.5d0) 

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
c**   FILE NAME: matvec.for
c**   
c**   DATE WRITTEN: 7/20/90
c**   
c**   PROGRAMMER:Scott Hensley
c**   
c**   FUNCTIONAL DESCRIPTION: The subroutine takes a 3x3 matris 
c**   and a 3x1 vector a multiplies them to return another 3x1
c**   vector.
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
      real*8 r_t(3,3)           !3x3 matris
      real*8 r_v(3)             !3x1 vector
      
c     OUTPUT VARIABLES:
      real*8 r_w(3)             !3x1 vector

c     LOCAL VARIABLES:none

c     PROCESSING STEPS:

c     compute matris product

      r_w(1) = r_t(1,1)*r_v(1) + r_t(1,2)*r_v(2) + r_t(1,3)*r_v(3)
      r_w(2) = r_t(2,1)*r_v(1) + r_t(2,2)*r_v(2) + r_t(2,3)*r_v(3)
      r_w(3) = r_t(3,1)*r_v(1) + r_t(3,2)*r_v(2) + r_t(3,3)*r_v(3)
      
      return
      end  

c****************************************************************

      subroutine lincomb(r_k1,r_u,r_k2,r_v,r_w)

c****************************************************************
c**   
c**   FILE NAME: lincomb.for
c**   
c**   DATE WRITTEN: 8/3/90
c**   
c**   PROGRAMMER:Scott Hensley
c**   
c**   FUNCTIONAL DESCRIPTION: The subroutine forms the linear combination
c**   of two vectors.
c**   
c**   ROUTINES CALLED:none
c**   
c**   NOTES: none
c**   
c**   UPDATE LOG:
c**   
c*****************************************************************

      implicit none

c     INPUT VARIABLES:
      real*8 r_u(3)             !3x1 vector
      real*8 r_v(3)             !3x1 vector
      real*8 r_k1               !scalar
      real*8 r_k2               !scalar
      
c     OUTPUT VARIABLES:
      real*8 r_w(3)             !3x1 vector

c     LOCAL VARIABLES:none

c     PROCESSING STEPS:

c     compute linear combination

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
      integer i_type            !2=lat,lon to utm,1= utm to lat,lon
      real*8 r_a                !ellispoid semi-major axis
      real*8 r_e2               !ellipsoid eccentricity squared  
      real*8 r_vec(2)           !Northing,Easting(m)
      real*8 r_lat              !latitude (deg -90 to 90)
      real*8 r_lon              !longitude (deg -180 to 180)
      integer i_zone            !UTM zone
      character*1 a_grid        !UTM North-South grid
      
c     OUTPUT VARIABLES:see input

c     LOCAL VARIABLES:
      integer i_ft,i_gi
      real*8 pi,r_dtor
      real*8 r_v(2)             !Northing,Easting(m)
      real*8 r_ep2,r_k0,r_k
      real*8 r_fe,r_fn(2)
      real*8 r_e4,r_e6,r_n,r_t,r_t2,r_c,r_c2,r_ba
      real*8 r_a2,r_a3,r_a4,r_a5,r_a6 
      real*8 r_d,r_d2,r_d3,r_d4,r_d5,r_d6
      real*8 r_lon0,r_lat1,r_m,r_m0,r_mu,r_lat0
      real*8 r_et,r_e1,r_e12,r_e13,r_e14,r_r
      character*1 a_griddes(20)

c     DATA STATEMENTS:
      data pi /3.141592653589793238d0/
      data r_dtor /1.74532925199d-2/
      data i_ft /0/
      data a_griddes /'C','D','E','F','G','H','J',
     +     'K','L','M','N','P','Q','R','S','T','U',
     +     'V','W','X'/
      data r_k0 /.9996d0/       !scale at center 
      data r_lat0 /0.d0/
      data r_fe,r_fn /500000.d0,0.d0,10000000.d0/

C     FUNCTION STATEMENTS:none

c     PROCESSING STEPS:

      r_ep2 = r_e2/(1.d0 - r_e2)
      r_e4 = r_e2**2
      r_e6 = r_e2**3
      pi =  4.d0*atan(1.d0)
      r_dtor = pi/180.d0

      if(i_type .eq. 2)then     !convert lat,lon to UTM

         if (i_zone .le. 0) i_zone=int(mod(r_lon+3.d0*pi,2.d0*pi)/(r_dtor*6.d0))+1             

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
     +        5.d0*r_e6/256.d0)*r_lat - (3.d0*r_e2/8.d0 + 
     +        3.d0*r_e4/32.d0 + 
     +        45.d0*r_e6/1024.d0)*sin(2.d0*r_lat) +  
     +        (15.d0*r_e4/256.d0 + 
     +        45.d0*r_e6/1024.d0)*sin(4.d0*r_lat) -
     +        (35.d0*r_e6/3072.d0)*
     +        sin(6.d0*r_lat))
         r_m0 = r_a*((1.d0-r_e2/4 - 3.d0*r_e4/64.d0 - 
     +        5.d0*r_e6/256.d0)*r_lat0 - (3.d0*r_e2/8.d0 + 
     +        3.d0*r_e4/32.d0 + 
     +        45.d0*r_e6/1024.d0)*sin(2.d0*r_lat0) + 
     +        (15.d0*r_e4/256.d0 + 
     +        45.d0*r_e6/1024.d0)*sin(4.d0*r_lat0) -
     +        (35.d0*r_e6/3072.d0)*
     +        sin(6.d0*r_lat0))
         
         r_vec(1) = r_k0*r_n*(r_ba+(1.d0-r_t+r_c)*r_a3/6.d0 + 
     +        (5.d0-18.d0*r_t+r_t2+72.d0*r_c-58.d0*r_ep2)*r_a5/120.d0)
         r_vec(1) = r_vec(1) + r_fe

         r_vec(2) = r_k0*(r_m - r_m0 + r_n*tan(r_lat)*
     +        ( r_a2/2.d0 + (5.d0-r_t+9.d0*r_c+4.d0*r_c**2)*
     +        (r_a4/24.d0) + (61.d0-58.d0*r_t+r_t2+600.d0*r_c-
     +        330.d0*r_ep2)*(r_a6/720.d0) ))
         if(r_lat .ge. 0)then
            r_vec(2) = r_vec(2) + r_fn(1)
         else
            r_vec(2) = r_vec(2) + r_fn(2)
         endif

         r_k = r_k0*(1.d0+(1.d0+r_ep2*cos(r_lat)**2)*
     +        (r_vec(1)-r_fe)**2/
     +        (2.d0*(r_k0**2)*r_n**2))

         i_gi = int((r_lat/r_dtor+80.d0)/8.d0) + 1
         i_gi = max(min(i_gi,20),1)
         a_grid = a_griddes(i_gi)
         
      elseif(i_type .eq. 1)then !convert UTM to lat,lon 

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
     +        5.d0*r_e6/256.d0))
         r_lat1 = r_mu + (3.d0*r_e1/2.d0-27.d0*r_e13/32.d0)*
     +        sin(2.d0*r_mu)+
     +        (21.d0*r_e12/16.d0-55.d0*r_e14/32.d0)*sin(4.d0*r_mu)+  
     +        (51.d0*r_e13/96.d0)*sin(6.d0*r_mu) +  
     +        (1097.d0*r_e14/512.d0)*sin(8.d0*r_mu) 

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
     +        (5.d0+3.d0*r_t+10.d0*r_c-4.d0*r_c2-9.d0*r_ep2)*
     +        r_d4/24.d0 +
     +        (61.d0+90*r_t+298.d0*r_c+45.d0*r_t2-252.d0*r_ep2-3.d0*
     +        r_c2)*
     +        (r_d6/720.d0))
         r_lon = r_lon0 + (r_d - (1.d0+2.d0*r_t+r_c)*r_d3/6.d0 + 
     +        (5.d0-2.d0*r_c+28.d0*r_t-3.d0*r_c2+8.d0*r_ep2+
     +        24.d0*r_t2)*
     +        (r_d5/120.d0))/cos(r_lat1)

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
      integer i_type            !1=lat,lon to utm,2= utm to lat,lon
      real*8 r_a                !ellispoid semi-major axis
      real*8 r_e2               !ellipsoid eccentricity squared  
      real*8 r_vec(2)           !Northing,Easting(m)
      real*8 r_lat              !latitude (deg -90 to 90)
      real*8 r_lon              !longitude (deg -180 to 180)
      integer i_zone            !UTM zone
      character*1 a_grid        !UTM North-South grid
      
c     OUTPUT VARIABLES:see input

c     LOCAL VARIABLES:
      integer i_ft,i_gi
      real*8 r_v(2)             !Northing,Easting(m)
      real*8 pi,r_dtor
      real*8 r_ep2,r_k0,r_k
      real*8 r_fe,r_fn(2)
      real*8 r_e4,r_e6,r_n,r_t,r_t2,r_c,r_c2,r_ba
      real*8 r_a2,r_a3,r_a4,r_a5,r_a6 
      real*8 r_d,r_d2,r_d3,r_d4,r_d5,r_d6
      real*8 r_lon0,r_lat1,r_m,r_m0,r_mu,r_lat0
      real*8 r_et,r_e1,r_e12,r_e13,r_e14,r_r
      character*1 a_griddes(20)

c     DATA STATEMENTS:
      data pi /3.141592653589793238d0/
      data r_dtor /1.74532925199d-2/
      data i_ft /0/
      data a_griddes /'C','D','E','F','G','H','J',
     +     'K','L','M','N','P','Q','R','S','T','U',
     +     'V','W','X'/
      data r_k0 /.9996d0/       !scale at center 
      data r_lat0 /0.d0/
      data r_fe,r_fn /500000.d0,0.d0,10000000.d0/

C     FUNCTION STATEMENTS:none

c     PROCESSING STEPS:

      r_ep2 = r_e2/(1.d0 - r_e2)
      r_e4 = r_e2**2
      r_e6 = r_e2**3
      pi =  4.d0*atan(1.d0)
      r_dtor = pi/180.d0

      if(i_type .eq. 2)then     !convert lat,lon to UTM

         if (i_zone .le. 0) i_zone=int(mod(r_lon+3.d0*pi,2.d0*pi)/(r_dtor*6.d0))+1             

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
     +        5.d0*r_e6/256.d0)*r_lat - (3.d0*r_e2/8.d0 + 
     +        3.d0*r_e4/32.d0 + 
     +        45.d0*r_e6/1024.d0)*sin(2.d0*r_lat) +  
     +        (15.d0*r_e4/256.d0 + 
     +        45.d0*r_e6/1024.d0)*sin(4.d0*r_lat) -
     +        (35.d0*r_e6/3072.d0)*
     +        sin(6.d0*r_lat))
         r_m0 = r_a*((1.d0-r_e2/4 - 3.d0*r_e4/64.d0 - 
     +        5.d0*r_e6/256.d0)*r_lat0 - (3.d0*r_e2/8.d0 + 
     +        3.d0*r_e4/32.d0 + 
     +        45.d0*r_e6/1024.d0)*sin(2.d0*r_lat0) + 
     +        (15.d0*r_e4/256.d0 + 
     +        45.d0*r_e6/1024.d0)*sin(4.d0*r_lat0) -
     +        (35.d0*r_e6/3072.d0)*
     +        sin(6.d0*r_lat0))
         
         r_vec(2) = r_k0*r_n*(r_ba+(1.d0-r_t+r_c)*r_a3/6.d0 + 
     +        (5.d0-18.d0*r_t+r_t2+72.d0*r_c-58.d0*r_ep2)*r_a5/120.d0)
         r_vec(2) = r_vec(2) + r_fe

         r_vec(1) = r_k0*(r_m - r_m0 + r_n*tan(r_lat)*
     +        ( r_a2/2.d0 + (5.d0-r_t+9.d0*r_c+4.d0*r_c**2)*
     +        (r_a4/24.d0) + (61.d0-58.d0*r_t+r_t2+600.d0*r_c-
     +        330.d0*r_ep2)*(r_a6/720.d0) ))
         if(r_lat .ge. 0)then
            r_vec(1) = r_vec(1) + r_fn(1)
         else
            r_vec(1) = r_vec(1) + r_fn(2)
         endif

         r_k = r_k0*(1.d0+(1.d0+r_ep2*cos(r_lat)**2)*
     +        (r_vec(2)-r_fe)**2/
     +        (2.d0*(r_k0**2)*r_n**2))

         i_gi = int((r_lat/r_dtor+80.d0)/8.d0) + 1
         i_gi = max(min(i_gi,20),1)
         a_grid = a_griddes(i_gi)
         
      elseif(i_type .eq. 1)then !convert UTM to lat,lon 

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
     +        5.d0*r_e6/256.d0))
         r_lat1 = r_mu + (3.d0*r_e1/2.d0-27.d0*r_e13/32.d0)*
     +        sin(2.d0*r_mu)+
     +        (21.d0*r_e12/16.d0-55.d0*r_e14/32.d0)*sin(4.d0*r_mu)+  
     +        (51.d0*r_e13/96.d0)*sin(6.d0*r_mu) +  
     +        (1097.d0*r_e14/512.d0)*sin(8.d0*r_mu) 

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
     +        (5.d0+3.d0*r_t+10.d0*r_c-4.d0*r_c2-9.d0*r_ep2)*
     +        r_d4/24.d0 +
     +        (61.d0+90*r_t+298.d0*r_c+45.d0*r_t2-252.d0*r_ep2-3.d0*
     +        r_c2)*
     +        (r_d6/720.d0))
         r_lon = r_lon0 + (r_d - (1.d0+2.d0*r_t+r_c)*r_d3/6.d0 + 
     +        (5.d0-2.d0*r_c+28.d0*r_t-3.d0*r_c2+8.d0*r_ep2+
     +        24.d0*r_t2)*
     +        (r_d5/120.d0))/cos(r_lat1)

      endif
      
      end


