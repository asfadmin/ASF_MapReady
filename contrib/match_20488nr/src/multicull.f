c      This program must link to real*8 versions of the following 
c      Numerical Recipies (v1992) routines to work properly.

c      SUBROUTINE gaussj(a,n,np,b,m,mp)
c      SUBROUTINE svdcmp(a,m,n,mp,np,w,v)
c      SUBROUTINE svdvar(v,ma,np,w,cvm,ncvm)
c      SUBROUTINE qrdcmp(a,n,np,c,d,sing)
c      SUBROUTINE svbksb(u,w,v,m,n,mp,np,b,x)
c      REAL*8 FUNCTION pythag(a,b)

c***************************************************************

      program cull_points

c****************************************************************
c**     
c**   FILE NAME: cull_points.f
c**     
c**   DATE WRITTEN: 01/02/95 
c**     
c**   PROGRAMMER: Scott Hensley
c**     
c**   FUNCTIONAL DESCRIPTION: This program will compute statics and a 
c**   linear fit of offset data and then cull points based on thresholds
c**   for the SNR, match covariances, or residuals from fits.
c**
c**   ROUTINES CALLED: svdvecfit,QRdecomp
c**   
c**   NOTES: none
c** 
c**   UPDATE LOG:
c**     
c**     Change to warn if too many points in input file
c**     
c*****************************************************************

      IMPLICIT NONE

      integer I_NP,I_MP,I_RD,I_FP

c     PARAMETER STATEMENTS:
      parameter (I_MP=10000)
      parameter (I_RD=2)
      parameter (I_FP=4) 
      parameter (I_NP=6)

c     INPUT VARIABLES:
      character*80 a_cmdfile,a_tpfile,a_outfile
      character*120 a_input
	
c     OUTPUT VARIABLES:

c     LOCAL VARIABLES:
      logical sing,l_chisq

      integer i,j
      integer i_ma,i_mm
      integer i_inarg,iargc

      real*8  r_w(I_NP)
      real*8  r_v(I_NP,I_NP)
      real*8  r_u(I_NP,I_NP)

      real*8  r_pi,r_dtor,r_rtod
      real*8  r_vecin(I_FP,I_MP)
      real*8  r_obs(I_RD,I_MP)
      real*8  r_cov(I_RD,I_RD,I_MP)
      real*8  r_chisq(I_RD,0:I_MP)

      real*8  r_a(I_NP)
      real*8  r_at2(I_NP)
      real*8  r_c(I_RD),r_d(I_RD)

      real*8  r_vec(I_RD)
      real*8  r_vec1(I_RD)
      real*8  r_vec2(I_RD)

      real*8  r_q1,r_q2
      real*8  r_c1t,r_c1t2
      real*8  r_c2t,r_c2t2
      real*8  r_uu(I_RD),r_uu2
      real*8  r_rot(I_RD,I_RD)
      real*8  r_aff(I_RD,I_RD)
      real*8  r_c1,r_c2
      real*8  r_skew
      real*8  r_rotang
      real*8  r_scale1,r_scale2
      real*8  r_h1,r_h2

      real*8 r_acloc(I_MP),r_dnloc(I_MP),r_acoff(I_MP),r_dnoff(I_MP)
      real*8 r_snrth,r_snrave,r_snrstd,r_snrmax,r_snrmin,r_snr(I_MP),r_delsnr
      real*8 r_covacth,r_covacave,r_covacstd,r_covacmax,r_covacmin,r_covac(I_MP),r_delcovac
      real*8 r_covdnth,r_covdnave,r_covdnstd,r_covdnmax,r_covdnmin,r_covdn(I_MP),r_delcovdn
      real*8 r_acres(I_MP),r_dnres(I_MP),r_acresth,r_dnresth,r_covx(I_MP)
      real*8 r_height(I_MP),r_height2(I_MP)
      real*8 r_acloc2(I_MP),r_dnloc2(I_MP)
      real*8 r_tmp1,r_tmp2
      integer i_binsnr(10),i_bincovac(10),i_bincovdn(10),i_npts,i_save(I_MP),nptsorg

      integer i_paramest(I_NP),i_usedata(I_RD)
      common/funcom/i_paramest,i_usedata

      integer length
      external length

c     DATA STATEMENTS:
      data r_pi /3.14159265359d0/

C     FUNCTION STATEMENTS:

c     PROCESSING STEPS:

      write(6,*) ' '
      write(6,*) '     << Cull Offset Points Program >>    '
      write(6,*) ' '

      i_inarg = iargc()
      if(i_inarg .eq. 0)then
         write(6,*) 'Usage 1 arguement  : cull_points tp_file '
         write(6,*) 'Usage 2 arguements : cull_points tp_file out_file'
c         stop
         write(6,*) ' '
         write(6,'(x,a,$)') 'Enter tiepoint filename: '
         read(5,'(a)') a_tpfile
         write(6,'(x,a,$)') 'Enter output   filename: '
         read(5,'(a)') a_outfile
      else
        if(i_inarg .eq. 1)then
           call getarg(1,a_tpfile)
           a_outfile=' '
        elseif(i_inarg .eq. 2)then
           call getarg(1,a_tpfile)
           call getarg(2,a_outfile)
        else 
           stop 'Too Many Arguments on command line' 
        endif
      endif

      r_dtor = r_pi/180.d0
      r_rtod = 180.d0/r_pi

      do i=1,I_NP
         i_paramest(i) = 1
      enddo
      i_ma=I_NP

      do i=1,I_RD
         i_usedata(i)=1
      enddo

      open(10,file=a_tpfile,status='old')

c      open(12,file='out.cul')
      if(a_outfile .ne. ' ' ) open(13,file=a_outfile)

c                                                File I               File II
c     read in two dimensional tie points  (across across offset    down  down offset)

      i_npts = 0
      do while(1 .eq. 1)
         read(10,'(a)',end=999) a_input
         if (a_input .ne. ' ' .and. index(a_input,';') .eq. 0) then
           i_npts = i_npts + 1
           if (i_npts .le. I_MP) then
             read(a_input,*,end=999) r_vecin(2,i_npts),r_vecin(1,i_npts),r_h1,r_tmp1,r_tmp2,r_h2,
     +       r_vecin(3,i_npts),r_vecin(4,i_npts),r_snr(i_npts),r_covac(i_npts),r_covdn(i_npts),r_covx(i_npts)

             r_dnloc(i_npts) = r_vecin(2,i_npts)
             r_acloc(i_npts) = r_vecin(1,i_npts)    
             r_height(i_npts) = r_h1
             r_dnloc2(i_npts) = r_tmp1
             r_acloc2(i_npts) = r_tmp2
             r_height2(i_npts) = r_h2
             r_acoff(i_npts) = r_vecin(3,i_npts)    
             r_dnoff(i_npts) = r_vecin(4,i_npts)
             r_covac(i_npts) = sqrt(r_covac(i_npts))
             r_covdn(i_npts) = sqrt(r_covdn(i_npts))
             if (r_h1 .eq. 0. .or. r_h2 .eq. 0.) i_npts = i_npts - 1
           end if
         else
           write(13,'(a)') a_input(:max(length(a_input),1))
         endif
      enddo

 999  i_npts = i_npts - 1
      if (i_npts .gt. I_MP-3) then
        write(6,*) '*** Error ***  To many points in input file: ',i_npts, I_MP
        i_npts = I_MP
      end if
      nptsorg = i_npts

      write(6,*) 'Number of tie points = ',i_npts
      write(6,*) ' '

c     compute stats on snr and cov measurements

      r_snrmin = r_snr(1)
      r_snrmax = r_snr(1)
      r_snrave = 0.
      r_snrstd = 0.

      r_covacmin = r_covac(1)
      r_covacmax = r_covac(1)
      r_covacave = 0.
      r_covacstd = 0.

      r_covdnmin = r_covdn(1)
      r_covdnmax = r_covdn(1)
      r_covdnave = 0.
      r_covdnstd = 0.

      do i=1,i_npts

         r_snrmin = min(r_snr(i),r_snrmin)
         r_snrmax = max(r_snr(i),r_snrmax)
         r_snrave = r_snrave + r_snr(i)
         r_snrstd = r_snrstd + r_snr(i)**2

         r_covacmin = min(r_covac(i),r_covacmin)
         r_covacmax = max(r_covac(i),r_covacmax)
         r_covacave = r_covacave + r_covac(i)
         r_covacstd = r_covacstd + r_covac(i)**2

         r_covdnmin = min(r_covdn(i),r_covdnmin)
         r_covdnmax = max(r_covdn(i),r_covdnmax)
         r_covdnave = r_covdnave + r_covdn(i)
         r_covdnstd = r_covdnstd + r_covdn(i)**2

      enddo

      r_snrave = r_snrave/i_npts
      r_snrstd = sqrt(r_snrstd/i_npts - r_snrave**2)
      r_covacave = r_covacave/i_npts
      r_covacstd = sqrt(r_covacstd/i_npts - r_covacave**2)
      r_covdnave = r_covdnave/i_npts
      r_covdnstd = sqrt(r_covdnstd/i_npts - r_covdnave**2)

c     compute histogram of snr's and covariance measurements

      do i=1,10
         i_binsnr(i) = 0
         i_bincovac(i) = 0
         i_bincovdn(i) = 0
      enddo
      i_binsnr(10) = 1
      i_bincovac(10) = 1
      i_bincovdn(10) = 1


      r_delsnr = (r_snrmax - r_snrmin)/10.
      r_delcovac = (r_covacmax - r_covacmin)/10.
      r_delcovdn = (r_covdnmax - r_covdnmin)/10.

      do i=1,i_npts

         do j=1,10

            if(r_snr(i) .ge. r_snrmin + r_delsnr*(j-1) .and. 
     +         r_snr(i) .lt. r_snrmin + r_delsnr*j)then
               i_binsnr(j) = i_binsnr(j) + 1
            endif

            if(r_covac(i) .ge. r_covacmin + r_delcovac*(j-1) .and. 
     +         r_covac(i) .lt. r_covacmin + r_delcovac*j)then
               i_bincovac(j) = i_bincovac(j) + 1
            endif

            if(r_covdn(i) .ge. r_covdnmin + r_delcovdn*(j-1) .and. 
     +         r_covdn(i) .lt. r_covdnmin + r_delcovdn*j)then
               i_bincovdn(j) = i_bincovdn(j) + 1
            endif
               
         enddo

      enddo

c     write out information to screen and prompt for thresholds

      write(6,*) ' '
      write(6,*) '  Stats Summary '
      write(6,*) ' '

      write(6,260) 'SNR Min,Max Ave ,Std = ',r_snrmin,r_snrmax,r_snrave,r_snrstd
 260  format(x,a,4(f10.5))
      do j=1,10
         write(6,261) r_snrmin + r_delsnr*(j-1),r_snrmin + r_delsnr*j,i_binsnr(j)
 261     format(x,2(f10.5,x),i5)
      enddo

      write(6,*) ' '

      write(6,260) 'Cov Ac  Min,Max Ave ,Std = ',r_covacmin,r_covacmax,r_covacave,r_covacstd
      do j=1,10
         write(6,261) r_covacmin + r_delcovac*(j-1),r_covacmin + r_delcovac*j,i_bincovac(j)
      enddo

      write(6,*) ' '

      write(6,260) 'Cov Dn  Min,Max Ave ,Std = ',r_covdnmin,r_covdnmax,r_covdnave,r_covdnstd
      do j=1,10
         write(6,261) r_covdnmin + r_delcovdn*(j-1),r_covdnmin + r_delcovdn*j,i_bincovdn(j)
      enddo

      write(6,*) ' '
      write(6,263) 'Enter SNR,Cov AC, and Cov DN thresholds ==>'
 263  format(x,a,$)
      read(5,*) r_snrth,r_covacth,r_covdnth

c     Cull data based on thresholds and then due a 2d linear fit - a final culling based
c     on residuals of the fit will then be done

      i_mm = 0 
      do i=1,i_npts
         if(r_snr(i) .ge. r_snrth .and. r_covac(i) .le. r_covacth .and. 
     +        r_covdn(i) .le. r_covdnth)then
            i_mm = i_mm + 1
            i_save(i_mm) = i
         endif
      enddo

      write(6,*) ' '
      write(6,*) ' Number of points after culling = ',i_mm

c     use the culled points to generate the fit

      do i_npts=1,i_mm
  
         r_vecin(1,i_npts) = r_acloc(i_save(i_npts))
         r_vecin(2,i_npts) = r_dnloc(i_save(i_npts))
         r_vecin(3,i_npts) = r_acloc(i_save(i_npts))+r_acoff(i_save(i_npts))
         r_vecin(4,i_npts) = r_dnloc(i_save(i_npts))+r_dnoff(i_save(i_npts))
         
         r_obs(1,i_npts) = -r_acoff(i_save(i_npts))
         r_obs(2,i_npts) = -r_dnoff(i_save(i_npts))

         r_cov(1,1,i_npts) = 1.0
         r_cov(2,2,i_npts) = 1.0
         r_cov(1,2,i_npts) = 0.0
         r_cov(2,1,i_npts) = 0.0

      enddo

c     init affine transformation

      r_a(1) = 1.0
      r_a(2) = 0.0
      r_a(3) = 0.0
      r_a(4) = 1.0
      r_a(5) = 0.0
      r_a(6) = 0.0

c     call svdvecfit to get the estimate for this iteration

      l_chisq = .false.
      call svdvecfit(i_mm,I_RD,I_FP,r_vecin,r_obs,r_cov,
     +                I_NP,r_a,r_at2,r_u,r_v,r_w,r_chisq,l_chisq)

c     Decompose matrix and examine residuals

      r_aff(1,1) = r_a(1)
      r_aff(1,2) = r_a(2)
      r_aff(2,1) = r_a(3)
      r_aff(2,2) = r_a(4)

      write(6,*) ' '
      write(6,*) '      Matrix  Analysis       '
      write(6,*) ' '

      write(6,*) ' Affine Matrix '
      write(6,*) ' '
      write(6,70) r_aff(1,1), r_aff(1,2)
      write(6,70) r_aff(2,1), r_aff(2,2)
 70   format(x,f15.10,x,f15.10)
      write(6,*) ' '
      write(6,*) 'Translation Vector'
      write(6,*) ' '
      write(6,71) r_a(5),r_a(6)
 71   format(x,f11.3,x,f11.3,x)

c     decompose matrix

      call qrdcmp(r_aff,2,2,r_c,r_d,sing)

      r_uu(1) = r_aff(1,1)
      r_uu(2) = r_aff(2,1)

      r_uu2 = .5d0*(r_uu(1)**2  + r_uu(2)**2)
      
      r_rot(1,1) = (1.d0 - (r_uu(1)**2/r_uu2))
      r_rot(1,2) = -(r_uu(1)*r_uu(2))/r_uu2
      r_rot(2,1) = -(r_uu(1)*r_uu(2))/r_uu2
      r_rot(2,2) = (1.d0 - (r_uu(2)**2/r_uu2))

      if(r_d(1) .lt. 0)then
         r_rot(1,1) = -r_rot(1,1)
         r_rot(2,1) = -r_rot(2,1)
         r_d(1) = -r_d(1)
         r_aff(1,2) = -r_aff(1,2)
      elseif(r_d(2) .lt. 0)then
         r_rot(1,2) = -r_rot(1,2)
         r_rot(2,2) = -r_rot(2,2)
         r_d(2) = -r_d(2)
      endif         
         
      r_scale1 = abs(r_d(1))
      r_scale2 = abs(r_d(2))

      r_skew = r_aff(1,2)/r_d(1)

      r_rotang = atan2(r_rot(2,1),r_rot(1,1))

      write(6,*) ' '
      write(6,*) ' Rotation Matrix '
      write(6,*) ' '
      write(6,70) r_rot(1,1),r_rot(1,2)
      write(6,70) r_rot(2,1),r_rot(2,2)
      write(6,*) ' '
      write(6,*) 'Rotation Angle (deg) = ',r_rotang*r_rtod
      write(6,*) ' '
      write(6,*) ' Axis Scale Factors'
      write(6,*) ' '
      write(6,72) r_scale1,r_scale2
 72   format(x,f11.7,x,f11.7)
      write(6,*) ' '
      write(6,*) ' Skew Term'
      write(6,*) ' '
      write(6,73) r_skew
 73   format(x,f11.7)


c     residual analysis

      write(6,*) ' '
      write(6,*) '      Residual Analysis       '
      write(6,*) ' '

      r_aff(1,1) = r_a(1)
      r_aff(1,2) = r_a(2)
      r_aff(2,1) = r_a(3)
      r_aff(2,2) = r_a(4)

      r_c1t = 0.
      r_c2t = 0.
      r_c1t2 = 0.
      r_c2t2 = 0.
      do i=1,i_mm

         r_c1 = r_vecin(1,i)*r_aff(1,1) + r_vecin(2,i)*r_aff(1,2) + r_a(5)
         r_c2 = r_vecin(1,i)*r_aff(2,1) + r_vecin(2,i)*r_aff(2,2) + r_a(6)
         r_q1 = r_c1 - r_vecin(3,i)
         r_q2 = r_c2 - r_vecin(4,i)
         r_c1t = r_c1t + r_q1
         r_c2t = r_c2t + r_q2
         r_c1t2 = r_c1t2 + r_q1**2
         r_c2t2 = r_c2t2 + r_q2**2
         r_dnres(i_save(i)) = abs(r_q2)
         r_acres(i_save(i)) = abs(r_q1)
c         write(6,*) ' '
c         write(6,*) 'Point = ',i
c         write(6,101) 'Input Points Img1  = ',r_vecin(1,i),r_vecin(2,i)
c         write(6,101) 'Input Points Img2  = ',r_vecin(3,i),r_vecin(4,i)
c         write(6,101) 'Transformed Points = ',r_c1,r_c2
c         write(6,101) 'Residuals          = ',r_q1,r_q2
c 101     format(x,a,x,2(f12.2,x))
         write(6,101)  i, r_vecin(1,i),r_vecin(2,i),r_q1,r_q2
 101     format(x,i5,x,4(f12.2,x))        
      enddo

      write(6,*) ' '
      write(6,81) 'Mean error (across,down) = ',r_c1t/i_mm,r_c2t/i_mm
      write(6,81) ' STD  DEV  (across,down) = ',sqrt(r_c1t2/i_mm - (r_c1t/i_mm)**2),
     +                                          sqrt(r_c2t2/i_mm - (r_c2t/i_mm)**2)

 81   format(x,a,x,2(f15.7,x))
c     get thresholds on fit residuals

      write(6,*) ' '
      write(6,263) 'Enter AC and DN residual thresholds ==>'
      read(5,*) r_acresth,r_dnresth

c     Cull data based on thresholds and then due a 2d linear fit - a final culling based
c     on residuals of the fit will then be done

      do i=1,nptsorg
         i_save(i) = 0
      enddo

      i_npts = 0 
      do i=1,nptsorg
         if(r_snr(i) .ge. r_snrth .and. r_covac(i) .le. r_covacth .and. 
     +        r_covdn(i) .le. r_covdnth .and. r_dnres(i) .le. r_dnresth .and.
     +        r_acres(i) .le. r_acresth)then
            i_npts = i_npts + 1
            i_save(i_npts) = i
         endif
      enddo

      write(6,*) ' '
      write(6,*) ' Number of points after all culling = ',i_npts

c     cull data based on residuals

c        do i=1,i_npts
c          j = i_save(i)
c          write(12,270) r_acloc(j),r_dnloc(j),r_height(j),
c     +                  r_acloc(j)+r_acoff(j),r_dnloc(j)+r_dnoff(j),r_height2(j)
c 270      format(x,6f13.3)  
c        enddo
c        close(12)

      if(i_inarg .ge. 2)then

         do i=1,i_npts
            j = i_save(i)
            write(13,269) r_dnloc(j),r_acloc(j),r_height(j),
     &                    r_dnloc2(j),r_acloc2(j),r_height2(j),
     &                    r_acoff(j),r_dnoff(j),r_snr(j),
     &                    r_covac(j)**2,r_covdn(j)**2,r_covx(j)
 269        format(x,2(2f10.2,f10.3),2f8.2,4f8.4)
        enddo

      close(13)
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




c*****************************************************************************

      subroutine funcs(i_ip,i_rd,i_fp,r_vecin,i_np,r_a,r_amat)

c     funcs is the partial derivatives of a affine matrix fit wrt 
c     where the affine matrix is of the form

c             _            _    _    _
c      mat = |   a11  a12   |  |  T1  |     =   
c            |_  a21  a22  _|, |_ T2 _|
c              

      implicit none

      integer I_RDE
      integer I_NPE
      parameter(I_RDE=2)
      parameter(I_NPE=6)

      integer i,j

      integer i_ip
      integer i_rd
      integer i_fp
      integer i_np

      real*8 r_a(i_np)
      real*8 r_vecin(i_fp)
      real*8 r_amat(i_rd,i_np)

      integer i_paramest(I_NPE),i_usedata(I_RDE)
      common/funcom/i_paramest,i_usedata


      if (i_fp .ne.  4)    stop 'ERROR - i_fp not equal to     4 in funcs'
      if (i_rd .ne. I_RDE) stop 'ERROR - i_rd not equal to I_RDE in funcs'
      if (i_np .ne. I_NPE) stop 'ERROR - i_np not equal to I_NPE in funcs'


      r_amat(1,1) = r_vecin(1) 
      r_amat(1,2) = r_vecin(2)
      r_amat(1,3) = 0.
      r_amat(1,4) = 0. 
      r_amat(1,5) = 1.
      r_amat(1,6) = 0.

      r_amat(2,1) = 0.
      r_amat(2,2) = 0.
      r_amat(2,3) = r_vecin(1) 
      r_amat(2,4) = r_vecin(2)  
      r_amat(2,5) = 0.
      r_amat(2,6) = 1.0
      
c     set to zero the derivatives all parameters not being estimated

      do i=1,i_np
         do j=1,i_rd
            r_amat(j,i) = r_amat(j,i)*i_paramest(i)*i_usedata(j)
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
c**             T   -1     -1     T   -1 
c**    A = (AMAT COV  AMAT)  (AMAT COV  )VOBS 
c**
c**    where AMAT is the jacobain of the observations vs parameters,
c**    COV is the covriance matrix of observations
c**    and VOBS is the vector of observations. 
c**
c**    r_a should be passed in with current best estimate of values
c**   
c**   UPDATE LOG: 
c**         
c**      4/17/95 - Reversed order of r_vecin, r_vobs, and r_cov    SJS
c**                   revmoved r_vt, cleaned up parameter list
c**   
c*****************************************************************

      implicit none

c     PARAMETERS:
      integer I_NPE                  !number of parameters to estimate = i_np
      integer I_RDE                  !number of observations per point = i_rd 
      real*8 R_TOL,R_LAMBDA
      parameter(I_NPE=6)
      parameter(I_RDE=2)
      parameter(R_TOL=1.0d-20)
      parameter (R_LAMBDA=1.d0)

c     INPUT VARIABLES:
      integer i_mp                   !number of input points
      integer i_rd                   !number of observations each point
      integer i_fp                   !number of input parameters to func
      integer i_np                   !number of parameters to solve for

      real*8  r_vecin(i_fp,i_mp) 	 !vector values for func 
      real*8  r_vobs(i_rd,i_mp) 	 !vector of observations
      real*8  r_cov(i_rd,i_rd,i_mp)  !covariance matrix of observation
      real*8  r_chisq(i_rd,0:i_mp) 	 !chisq for solution and fit vs observation 
      real*8  r_a(i_np)   			 !solution to least squares
                                     !for each point 
      logical l_chisq                !evaluate the chisq for this fit
      
c     OUTPUT VARIABLES:
      real*8 r_at2(i_np)             !delta to add to previous solution
      real*8 r_u(i_np,i_np)          !svd matrix, orthogonal matrix
      real*8 r_v(i_np,i_np)          !svd matrix, orthogonal matrix
      real*8 r_w(i_np)               !svd matrix, diagonal matrix

c     LOCAL VARIABLES:
      integer i,j,k,i_pts
      real*8  r_covtemp(I_RDE,I_RDE)
      real*8  r_am(I_NPE,I_RDE)
      real*8  r_amat(I_RDE,I_NPE)
      real*8  r_ptot(I_NPE)
      real*8  r_wmax,r_thres,r_b(I_RDE,1),r_chird(I_RDE)

      integer i_paramest(I_NPE),i_usedata(I_RDE)
      common/funcom/i_paramest,i_usedata

c     DATA STATEMENTS:

C     FUNCTION STATEMENTS:

c     PROCESSING STEPS:

c     init some arrays

c      type*, ' '
c      type*, 'Inside SVDVECFIT'
c      type*, ' '

      if (i_rd .ne. I_RDE) stop 'ERROR - i_rd not equal to I_RDE in SVDVECFIT'
      if (i_np .ne. I_NPE) stop 'ERROR - i_np not equal to I_NPE in SVDVECFIT'

      do i=1,i_np
         do j=1,i_np
            r_u(i,j) = 0.0
         enddo
         r_ptot(i) = 0.0
      enddo

c     loop over the input points

      do i_pts=1,i_mp

c         type*, 'i_pts = ',i_pts

c     invert the covariance matrix of the observation

         do i=1,i_rd
            do j=1,i_rd
               r_covtemp(i,j) = r_cov(i,j,i_pts)
            enddo
         enddo

         call gaussj(r_covtemp,i_rd,i_rd,r_b,1,1)

c     get the required jacobian matrix

         call funcs(i_pts,i_rd,i_fp,r_vecin(1,i_pts),i_np,r_a,r_amat)

c         do i=1,i_rd
c            do j=1,i_np
c               type*, 'i,j,r_amat = ',i,j,r_amat(i,j)
c            enddo
c         enddo

c     multiply amat transpose by the inverse cov matrix

         do i=1,i_np
            do j=1,i_rd
               r_am(i,j) = 0.0
               do k=1,i_rd
                  r_am(i,j) = r_am(i,j) + r_amat(k,i)*r_covtemp(k,j)
               enddo
            enddo
         enddo

c         do i=1,i_np
c            do j=1,i_rd
c               type*, 'i,j,r_am = ',i,j,r_am(i,j)
c            enddo
c         enddo

c     multiply am by amat

         do i=1,i_np
            do j=1,i_np
               do k=1,i_rd
                  r_u(i,j) = r_u(i,j) + r_am(i,k)*r_amat(k,j)
               enddo
            enddo
         enddo

c     multilpy am by vobs


c         type*, 'r_vobs,i_pts = ',i_pts,r_vobs(1,i_pts),r_vobs(2,i_pts)
         do i=1,i_np
            do k=1,i_rd
               r_ptot(i) = r_ptot(i) + r_am(i,k)*r_vobs(k,i_pts)
            enddo
         enddo
         
      enddo   !i_pts

c     find the SVD of the r_u matrix

c         do i=1,i_np
c            do j=1,i_np
c               type*, 'i,j,r_u = ',i,j,r_u(i,j)
c            enddo
c         enddo

      call svdcmp(r_u,i_np,i_np,i_np,i_np,r_w,r_v)

c         do i=1,i_np
c            do j=1,i_np
c               type*, 'i,j,r_u,r_v = ',i,j,r_u(i,j),r_v(i,j)
c            enddo
c         enddo

c         do i=1,i_np
c            type*, 'w = ',i,r_w(i)
c         enddo

c     kill off all the singular values

      r_wmax = 0.0
      do i=1,i_np
         if(r_w(i) .gt. r_wmax)then
            r_wmax = r_w(i)
         endif
      enddo
      r_thres = r_wmax*R_TOL
c      type*, 'r_thres = ',r_thres

      do i=1,i_np
         if(r_w(i) .lt. r_thres)then
            r_w(i) = 0.0
         endif
      enddo

c      do i=1,i_np
c         type*, 'w = ',i,r_w(i)
c      enddo

c     use the svbksb routine to solve for the desired parameters

      call svbksb(r_u,r_w,r_v,i_np,i_np,i_np,i_np,r_ptot,r_at2)

c     update the r_a vector

      do i=1,i_np
         r_at2(i) = -r_at2(i)*i_paramest(i)
         r_a(i) = r_at2(i)/R_LAMBDA + r_a(i)
c         type *,'a=',i,r_a(i),r_at2(i)
      enddo

c     evaluate the chisq array (linearized version)

      if(l_chisq)then

c     loop over data points


         do i=1,i_rd
            r_chird(i) = 0.
         enddo
         r_chisq(1,0) = 0.0
         do i=1,i_mp

            call funcs(i,i_rd,i_fp,r_vecin(1,i),i_np,r_a,r_amat)

            do j=1,i_rd
               r_chisq(j,i) = 0.0
               do k=1,i_np
                  r_chisq(j,i) = r_chisq(j,i) + r_amat(j,k)*r_at2(k)
               enddo
c               type*, 'r_chisq = ',i,j,r_chisq(j,i),r_vobs(j,i)
               r_chisq(j,i) = r_covtemp(j,j)*(r_chisq(j,i) - 
     +              r_vobs(j,i))**2
               r_chisq(1,0) = r_chisq(1,0) + r_chisq(j,i)
               r_chird(j) = r_chird(j) + r_chisq(j,i)
            enddo

         enddo                  !i_pts loop for chisq

         r_chisq(1,0) = sqrt(r_chisq(1,0)/(2.*i_mp))
         print*, 'r_chisq = ',r_chisq(1,0),sqrt(r_chird(1)/i_mp),sqrt(r_chird(2)/i_mp)

      endif
      
      end  

c      SUBROUTINE gaussj(a,n,np,b,m,mp)
c      SUBROUTINE svdcmp(a,m,n,mp,np,w,v)
c      SUBROUTINE svdvar(v,ma,np,w,cvm,ncvm)
c      SUBROUTINE qrdcmp(a,n,np,c,d,sing)
c      SUBROUTINE svbksb(u,w,v,m,n,mp,np,b,x)
c      REAL*8 FUNCTION pythag(a,b)
