c  program to calculate SAR processing parameters
c  from a precise ERS-1/2 orbit taken from getorb.
c
c   ref_time1    - start time of first SAR echo
c   ref_time2    - end time of last SAR echo
c   ref_identity - satellite number (1)-ERS1 (2)-ERS2 (3)-Radarsat
c   rep_time1    - start time of first SAR echo
c   ref_time1    - end time of last SAR echo
c   sc_identity  - satellite number (1)-ERS1 (2)-ERS2 (3)-Radarsat
c
c  Output
c   variety of parameters
c
c  get the earth radius and flattening
c
      implicit real*8 (a-h,o-z)
      include '/home/amelung/delft/getorb/GRS80.inc'
      character*80 cref0,creff,crep0,crepf
      character*80 cnref,cnrep
      character*80 orbpath(2)
      character*80 roiin(100),roiinname
      integer*4 getorb
      external getorb
      logical verbose,wbase
      real*8 xyz(3),vec(3)
      real*8 r_v1(2),r_v2(2)
      character*1 a_grid
      data verbose /.false./
      data wbase /.true./
      data base1,base2 /-1.,-1/
c
c  these are values for ERS-1/2
c

      data sol /299792456.0/
      data orbpath(1) /"/d0/amelung/orbits/delft/ERS1"/
      data orbpath(2) /"/d0/amelung/orbits/delft/ERS2"/
      rad=atan(1d0)/45.
      pi =atan(1d0)*4.
c
c   get values from command line
c
      narg = iargc()
      if(narg.lt.6) then
        write(*,50)
 50   format ('ers_baseline reft0 reftf ref# rept0 reptf rep# [roi.in] [y]'/
     |  '       '/
     |  '                    ref_t0: reference start time YYDDD.DDDD'/
     |  '                    ref_tf: reference end time '/
     |  '                    ref#:   spacecraft # (1)-ERS1 (2)-ERS-2'/
     |  '                    rep_t0: repeat start time YYDDD.DDDD'/
     |  '                    rep_tf: repeat end time '/
     |  '                    rep#:   spacecraft # (1)-ERS1 (2)-ERS-2'/
     |  '                    roi.in: in-file (modified if given)'/
     |  '                    y:      for changing the coarse offsets')
        stop
      else
        call getarg(1,cref0)
        call getarg(2,creff)
        call getarg(3,cnref)
        call getarg(4,crep0)
        call getarg(5,crepf)
        call getarg(6,cnrep)
      endif
c
c  convert the character strings to numbers
c
      read(cref0,*)tref0
      read(creff,*)treff
      read(cnref,*)nref
      read(crep0,*)trep0
      read(crepf,*)trepf
      read(cnrep,*)nrep
cfalk  read roi.in file and get some parameters
      if (narg.ge.7) then
        call getarg(7,roiinname)
        open(21,file=roiinname)
        do iline=1,100
           read(21,'(q,a80)',end=10)nq,roiin(iline)
           if(nq.eq.0)go to 10
        end do
10      close(21)
        read(roiin(24),*)prf
        read(roiin(29),*)range_samp_rate
        read(roiin(23),*)range0
c the next parameters are for track and squint angle
        read(roiin(34),*)wvl
        read(roiin(17),*)fdop
        fdop=fdop*prf
        type *,'roi derived parameters:'
        type *,'prf =     ',prf
        type *,'range0 =  ',range0
        type *,'fdop   =  ',fdop
      else
        PRF=1679.9024
        range0=830777.086016
	range_samp_rate=1.896e+07
        fdop=0.
      endif

c
c  print out the times and sc identity if reference and repeat times
c  are different
c
      if(tref0.ne.trep0) then
        wbase = .true.
      else
        wbase = .false.
      endif
      
      if(wbase) write(*,801)tref0
      if(wbase) write(*,802)treff
      if(wbase) write(*,803)nref
      if(wbase) write(*,804)trep0
      if(wbase) write(*,805)trepf
      if(wbase) write(*,806)nrep
  801 format('ref_clock_start		= ',f13.7)
  802 format('ref_clock_stop		= ',f13.7)
  803 format('ref_identity		= ',i4)
  804 format('SC_clock_start		= ',f13.7)
  805 format('SC_clock_stop		= ',f13.7)
  806 format('SC_identity		= ',i4)
c
c  locate targets at the start and end of the repeat orbit
c
      trep1=sec85(0,trep0)
      trep2=sec85(0,trepf)
c
c  compute the start target 
c
      iorb = getorb(trep1,rlatrep1,rlonrep1,rorb,orbpath(nrep),verbose)
      if(iorb.gt.0) write(*,'(a)')'* time outside limit of ODR files'
      if(iorb.lt.0) write(*,'(a)')'* warning outside ODR time '
      call geoxyz(rlatrep1*rad,rlonrep1*rad,rorb,xyz,rs)
      xrep1=xyz(1)
      yrep1=xyz(2)
      zrep1=xyz(3)
      rrep1=sqrt(xrep1*xrep1+yrep1*yrep1+zrep1*zrep1)
c
c  compute the end target
c
      iorb = getorb(trep2,rlatrep2,rlonrep2,rorb,orbpath(nrep),verbose)
      if(iorb.gt.0) write(*,'(a)')'* time outside limit of ODR files'
      if(iorb.lt.0) write(*,'(a)')'* warning outside ODR time '
      call geoxyz(rlatrep2*rad,rlonrep2*rad,rorb,xyz,rs)
      xrep2=xyz(1)
      yrep2=xyz(2)
      zrep2=xyz(3)
c
c  compute the sc height, velocity, and earth radius of the reference ellipsoid
c  for the repeat orbit
c
      rorb=0.
      call geoxyz(rlatrep1*rad,rlonrep1*rad,rorb,vec,ellipse)
      sc_height=rrep1-ellipse
      write(*,901)ellipse
      write(*,902)sc_height
  901 format('earth_rad		= ',f12.4)
  902 format('SC_height		= ',f12.2)
c
c  now compute the sc velocity and then the 
c  "equivalent velocity"
c
      dt=1.
      time=trep1+dt
      iorb = getorb(time,rlatnew1,rlonnew1,rorb,orbpath(nrep),verbose)
      if(iorb.gt.0) write(*,'(a)')'* time outside limit of ODR files'
      if(iorb.lt.0) write(*,'(a)')'* warning outside ODR time '
      call geoxyz(rlatnew1*rad,rlonnew1*rad,rorb,xyz,rs)
      xnew1=xyz(1)
      ynew1=xyz(2)
      znew1=xyz(3)
      dx=xnew1-xrep1
      dy=ynew1-yrep1
      dz=znew1-zrep1
      dist = sqrt(dx*dx+dy*dy+dz*dz)
c
c  compute the total velocity and then scale according to 
c  equations in McDonough et al. 1985
c
      veloc=dist/dt
      velocsav=veloc           !changed by falk
      veloc=veloc/sqrt(1+sc_height/ellipse)
      write(*,904)veloc
  904 format('SC_vel			= ',f10.4)
c
c  calculate the parameters for the reference orbit
c
      tref1=sec85(0,tref0)
      tref2=sec85(0,treff)
      tstep =.1/PRF        !falk test
      tstep =.5/PRF
      nrec=nint((tref2-tref1)/tstep)
      nrec2=nrec/2
      tstep=(tref2-tref1)/(nrec-1)
c
c   loop over time and call getorb. pad times to each end
c
      base1=-1.
      base2=-1.
      do 100 i=-nrec2,nrec+nrec2
        time=tref1+(i-1)*tstep 
        iorb = getorb(time,rlat,rlon,rorb,orbpath(nref),verbose)
        if(iorb.gt.0) write(*,'(a)')'* time outside limit of ODR files'
        if(iorb.lt.0) write(*,'(a)')'* warning outside ODR time '
        call geoxyz(rlat*rad,rlon*rad,rorb,xyz,rs)
c       write (*,*) time,i,rlat,rlon,rorb,xyz,rs
c
c  compute the target distance from the reference orbit to the repeat orbit
c
        xs=xyz(1)
        ys=xyz(2)
        zs=xyz(3)
        dx=xs-xrep1
        dy=ys-yrep1
        dz=zs-zrep1
c
c  find minimum parameters for start of repeat
c
        dist1=sqrt(dx*dx+dy*dy+dz*dz)
        if(base1.lt.0.or.dist1.lt.base1) then
           base1=dist1
           tmin1=time
           xmin1=xs
           ymin1=ys
           zmin1=zs
           rlatmin1=rlat
           rlonmin1=rlon
           imin1=i
           rorb1=rorb
        endif
c
c  find minimum parameters for end of repeat
c
        dx=xs-xrep2
        dy=ys-yrep2
        dz=zs-zrep2
        dist2= sqrt(dx*dx+dy*dy+dz*dz)
        if(base2.lt.0.or.dist2.lt.base2) then
           base2=dist2
           tmin2=time
           xmin2=xs
           ymin2=ys
           zmin2=zs
           rlatmin2=rlat
           rlonmin2=rlon
           imin2=i
           rorb2=rorb
        endif
 100  continue
c
      rmin1=sqrt(xmin1*xmin1+ymin1*ymin1+zmin1*zmin1)
      rmin2=sqrt(xmin2*xmin2+ymin2*ymin2+zmin2*zmin2)
c
c  now compute the radial unit vector
c
      xunt1=xmin1/rmin1
      yunt1=ymin1/rmin1
      zunt1=zmin1/rmin1
      xunt2=xmin2/rmin2
      yunt2=ymin2/rmin2
      zunt2=zmin2/rmin2
c
c  compute along-track shift
c
      yshift=(1-imin1)/2.
      iyshift=nint(yshift-.4999999)
      ryshift=yshift-iyshift
      write(*,903)iyshift
  903 format('shift			= ',i6)
      write(*,914)ryshift
  914 format('sub_int_a		= ',f9.7)
c
c  compute the sign of the horizontal baseline
c
      sign=1.
      if((rlatnew1-rlatrep1).lt.0.) sign=-1.*sign
      if((rlonrep1-rlonmin1).lt.0.) sign=-1.*sign
c
c  now compute the baseline components
c
      basev1=(xrep1-xmin1)*xunt1+(yrep1-ymin1)*yunt1+(zrep1-zmin1)*zunt1
      baseh1=sign*sqrt(base1*base1-basev*basev)
      baseh1=sign*sqrt(base1*base1-basev1*basev1)
      basev2=(xrep2-xmin2)*xunt2+(yrep2-ymin2)*yunt2+(zrep2-zmin2)*zunt2
      baseh2=sign*sqrt(base2*base2-basev*basev)
      baseh2=sign*sqrt(base2*base2-basev2*basev2)
      alpha1=atan2(basev1,baseh1)/rad
      alpha2=atan2(basev2,baseh2)/rad
      if(wbase) write(*,905)base1
      if(wbase) write(*,907)base2
      if(wbase) write(*,906)alpha1
      if(wbase) write(*,908)alpha2
 905  format('baseline_start		= ',f9.3)
 906  format('alpha_start		= ',f8.3)
 907  format('baseline_end		= ',f9.3)
 908  format('alpha_end		= ',f8.3)
c
c calculate the parallel baseline at the near range
c
      rc=ellipse+sc_height
      ra=ellipse
      arg=(range0**2+rc**2-ra**2)/(2.*range0*rc)
      rlook0=acos(arg)
c      rlook=20*rad
c
c  compute parallel and perpendicular baselines at near range look angle
c
      bparallel=base1*sin(rlook0-alpha1*rad)
      bperp=base1*cos(rlook0-alpha1*rad)
      if(wbase) write(*,909)bparallel
      if(wbase) write(*,910)bperp
 909  format('B_parallel		= ',f8.2)
 910  format('B_perpendicular		= ',f8.2)
      xshift = -bparallel*2.*range_samp_rate/sol
      ixshift = nint(xshift-.4999999)
      sub_int_r = xshift-ixshift
      write(*,911)ixshift
      write(*,912)sub_int_r
 911  format('xshift			= ',i5)
 912  format('sub_int_r		= ',f9.7)

cfalk  Next lines by falk, 13 FEB 98: Calculate local inclination of spacecraft track
        r_e2=(ae**2-ap**2)/ae**2                                    ! first numerical eccentricity
        time1=tref1
        iorb = getorb(time1,rlat1,rlon1,rorb,orbpath(nref),verbose)
               if(iorb.ne.0) write(*,'(a)')'* some problem with ODR file'
        time2=tref1+1.5
        iorb = getorb(time2,rlat2,rlon2,rorb,orbpath(nref),verbose)
               if(iorb.ne.0) write(*,'(a)')'* some problem with ODR file'

        call utmtoll(ae,r_e2,i_zone,a_grid,r_v1,rlat1*rad,rlon1*rad,1)
        call utmtoll(ae,r_e2,i_zone,a_grid,r_v2,rlat2*rad,rlon2*rad,1)

        beta = datan2(r_v2(2)-r_v1(2),r_v2(1)-r_v1(1))

c calculate squint angle (on ground)
       theta0 = dacos( (range0**2 + rc**2 - (ra)**2 ) / (2.*range0*rc) )
       squint=dasin(wvl*fdop/ (2*range0*sin(theta0)))
       write(*,'(a,f9.5)') 'deviation of image from North (acw): ',90.-beta/rad
       write(*,'(a,f9.5)') '                  inclusive squint : ',90.-(beta+squint)/rad
     
cfalk  Next lines by falk, 15 Sep 97
      lines=nint((tref2-tref1)*PRF)
      type *,'lines from PRF: ',lines
      type '(a,2f9.3)','veloc, eff.veloc    ', 
     |                  velocsav,veloc
      ha=0.056*range0*sin(rlook0)/2./bperp
      type '(a,f9.1)','ambiguity height: ',ha
      type '(a,f8.2)','theta_nearange',rlook0/rad
      type '(a,3f8.2)','Bperp, Bpar, alpha',bperp,bparallel,alpha1
      type *,'Parameters for simroi: bh, bv, bhdot, bvdot, iphi0'
      bh1=base1*cos(alpha1*rad)
      bv1=base1*sin(alpha1*rad)
      bh2=base2*cos(alpha2*rad)
      bv2=base2*sin(alpha2*rad)
      bvdot=(bv2-bv1)/float((lines/5))
      bhdot=(bh2-bh1)/float((lines/5))

      iphi0=nint(bparallel/0.0565647)*-1

c      delh=baseh1-baseh2 
c      delv=basev1-basev2 
c      type '(1x,a,3f10.4)','baseh1 baseh2 del:  ',baseh1,baseh2,delh 
c      type '(1x,a,3f10.4)','basev1 basev2 del:  ',basev1,basev2,delv 
c      type '(1x,a,3f10.4)','bh1 bh2 delh:  ',bh1,bh2,bh1-bh2 
c      type '(1x,a,3f10.4)','bv1 bv2 delv:  ',bv1,bv2,bv1-bv2 
      type '(1x,a,2f10.4,2f10.6,i7)','1look:  ',bh1,bv1,
     |                             bhdot,bvdot,iphi0
      type '(1x,a,2f10.4,2f10.6,i7)','2looks: ',bh1,bv1,
     |                             2*bhdot,2*bvdot,iphi0
      type '(1x,a,2f10.4,2f10.6,i7)','4looks: ',bh1,bv1,
     |                             4*bhdot,4*bvdot,iphi0
      type '(1x,a,2f10.4,2f10.6,i7)','8looks: ',bh1,bv1,
     |                             8*bhdot,8*bvdot,iphi0
c    write modified roi.in file
      if (narg.ge.7) then
      if (narg.eq.8) then
           read(roiin(16),*)iazoff,irgoff
	   if (iazoff.ne.0.or.irgoff.ne.0) 
     |     type*,'###### Attention: coarse offsets not 0 ! ######'
           write(roiin(16)(66:80),'(a,a)') 'old: ',roiin(16)(1:9)
           write(roiin(16)(1:10),'(i6,x,i3)') iyshift,ixshift
      endif
      write(roiin(20)(1:11),'(f10.2)') ellipse
      write(roiin(21)(1:16),'(2(f7.2,x))') velocsav,velocsav
      write(roiin(22)(1:18),'(2(f8.1,x))') sc_height,sc_height
      open(21,file=roiinname)
      do i=1,iline-1
         write(21,'(a)')roiin(i)
      end do
         write(21,*)
      close(21)
      endif
      stop
      end
