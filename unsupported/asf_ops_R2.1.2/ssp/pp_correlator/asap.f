c SccsId = @(#)asap.f	2.41 3/24/98
         subroutine asap(orb,tints,delta_t,x)
ccc         subroutine asap(orb,tints,delta_t,trefs,x)
ccc         subroutine asap(orb,tints,tfins,trefs,x)
ccc         subroutine asap(orb,tint,tfin,tref,x)

	implicit none

        character*128 SccsId_asap
        data SccsId_asap
     +  /'@(#)PPasap.f:2.41'/


c	----------------
c	abstract:
c	        propagate one state vector based on one kepler coordinate
c	        point and gmt time interval.
c	        
c               input: 
c	            1).orb(6): [kepler 6 elements] 
c	               a,ecc,inc,ascending node,argument periapsis &
c		       mean anomly.
c	            2).gmt time :
c	               tint: starting time
c	               tfin: ending time
c	               tref: reference time
c               output: 
c	             x(6): [state vectors]
c	             r_x,r_y,r_z, & v_x,v_y,v_z
c	----------------
c
c/*	subroutine asap  --------------------------
c
c  version of 2/29/88
c  purpose   
c    main driver for the artificial satellite analysis program (asap).
c    asap is an ephemeris propagation program for orbiting planetary
c    spacecrafts.  it uses cowell's method of special perturbation.  it
c    includes harmonics of up to 40x40 field, luni-solar gravity, drag,
c    and solar radiation pressure.  it uses a runge-kutta 8th order
c    method with step size control for numerical integration.  the
c    program is in modular form so that users can modify it to include
c    different i/o modules, or density models, or different integrators,
c    etc.  it assumes a planet mean  equator of epoch system and ignores
c    polar motion.  all inputs are either i30 for integers or d30. for
c    double precision with one value to each record.  the value must be
c    placed within the first 30 columns but there is no need to right
c    justify.  columns 31 to 80 can be used for comments.  the exception
c    is the input of the spherical harmonic coefficients.  this program
c    and its documentation are available from cosmic, cat. #npo-16731.
c    there is a companion program called long-term prediction (lop) that
c    uses an averaging method.  lop is available from cosmic, cat.
c    #npo-17052.
c  input
c    l      = degree of gravity harmonics to be included
c    m      = order of gravity harmonics to be included.  maximum for l
c             and m is 40.  l can be bigger than m.
c    ires   = an option to only include tesserals of order ires.  can be
c             used to study resonance effects whereby non-resonant
c             terresals are not included in the calculation and thus
c             cutting propagation time.  does not affect zonal terms.
c           = 0, to turn off the option.
c    isun   = 0, no solar gravity
c           = 1, with solar gravity
c    imoon  = 0, no lunar gravity
c           = 1, with lunar gravity
c    iephem = 0, use two-body ephemerides for sun and moon
c           = 1, for earth orbiting spacecraft only, use built-in
c                luni-solar ephemerides.  must use earth mean equator
c                and equinox of epoch system
c    idrag  = 0, no drag
c           = 1, with drag
c    idens  = 0, use exponential model
c           = 1, use static 1977 earth model
c    isrp   = 0, no solar radiation pressure
c           = 1, with solar radiation pressure
c    iorb     flag for inputing either mean or osculating orbital
c             elements.  uses the value of c20 to compute short period
c             effects
c           = 0, input orbital elements orb are osculating values
c           = 1, input orbital elements orb are mean values
c    iprint = 0, print at constant step as specified by step
c           = 1, also print at periapsis and apoapsis
c    inode  = 0, no nodal crossing print
c           = 1, nodal crossing time and info print
c    iplot  = 0, no output ascii file for plotting
c           = 1, write output at every 'step', apsis and nodal crossings
c                to file 8
c    orb      osculating or mean orbital elements of the spacecraft at
c             tint.  see iorb
c       (1) = a, semi-major axis (km)
c       (2) = e, eccentricity
c       (3) = i, inclination (deg)
c       (4) = capw, longitude of ascending node (deg)
c       (5) = w, argument of periapsis (deg)
c       (6) = m, mean anomaly (deg)
c    relerr = relative accuracy of the integrator.  recommended values
c             between 1.d-6 to 1.d-12
c    abserr = absolute accuracy of the integrator.  recommended values
c             between 1.d-6 to 1.d-12
c    step   = time step to print (sec).
c    tint(1)= initial calendar date of run (yyyymmdd.).  for example,
c             19880726.d0, for 26 july 1988.  all time used in this
c             program assumes ephemeris time and not universal time.
c        (2)= initial time of day of run (hhmmss.ss...d0).  for example,
c             130723.1234d0, for 13 hr 7 min 23.1234 sec
c    tfin   = same as tint except for end of run
c    tref   = same as tint except this is the time corresponding to the
c             position of the luni-solar ephemerides (es and em) and the
c             prime meridian (pm) input.  if iephem=1 to use built-in
c             luni-solar ephemerides, then this is the time
c             corresponding to the prime meridian input only
c    ge     = product of gravitational constant and mass of planet
c             (km**3/sec**2).  recommended values (jpl de118),
c           = 3.9860045d5, for earth
c           = 3.2485877d5, for venus
c           = 4.2828287d4, for mars
c    re     = radius of planet (km).  recommended values (iau 1982),
c           = 6378.140d0, for earth
c           = 6051.d0, for venus
c           = 3393.4d0, for mars
c    rate   = rotation rate of the planet (deg/sec).  recommended values
c             (iau 1982),
c           = 4.178074216d-3, for earth
c           = -1.71460706d-5, for venus
c           = 4.061249803d-3, for mars
c    pm     = location of the prime meridian relative to the inertial
c             x-axis at tref (deg).
c    ellip  = ellipticity of the reference ellipsoid.  used by drag
c             to compute geodetic altitude for atmosphere density
c             evaluation and by output routine to compute geodetic
c             altitude.  recommended values (iau 1982),
c           = 0.d0, to use a sphere
c           = .8182d-1, for earth
c           = 0.d0, for venus
c           = .1017d0, for mars
c    ratm     radius of the planet including atmospheric blockage (km).
c             it is used to compute shadow entry and exit in solar
c             radiation pressure computation.  recommended values,
c           = re+90 km for venus, earth, and mars
c           = re for mercury or the moon
c    rdens    reference density at reference height rht to be used by
c             the exponential density model (kg/km**3)
c    rht      reference height for the exponential density model (km).
c             use periapsis altitude if possible.
c    sht      scale height of the exponential density model (km)
c    altmax   maximum altitude to include drag perturbation (km)
c    wt       weight factor to be applied to the density, 1.d0 for
c             nominal, 2.d0 for twice denser, etc.
c    aread    effective spacecraft area for drag (km**2)
c    areas    effective spacecraft area for solar radiation pressure
c             (km**2)
c    scmass   effective spacecraft mass for the length of propogation
c             (kg)
c    cdrag    drag coefficient, recommended values between 2.d0 to 2.2d0
c    csrp     solar radiation pressure constant (kg/km-sec**2).
c             recommended values,
c           = g * 4.4d-3, for earth,
c           = g * 8.4d-3, for venus,
c           = g * 1.9d-3, for mars,
c             where g < 1 for translucent material
c                     = 1 for black body
c                     = 2 for perfectly reflective material
c    gs       product of gravitational constant and mass of sun
c             (km**3/sec**2).  recommended value (jpl de118),
c           = .13271244d12
c    es     = orbital elements of the sun in planet equator of epoch,
c             used in calculating point mass perturbation due to the
c             sun and solar radiation pressure as well.  see iephem for
c             built-in luni-solar ephemerides for the earth,
c      (1)  = semi-major axis (km)
c      (2)  = eccentricity
c      (3)  = inclination (deg)
c      (4)  = longitude of the ascending node (deg), equal to zero if
c             x-axis is equinox of epoch
c      (5)  = argument of periapsis (deg)
c      (6)  = mean anomaly at tref (deg)
c      (7)  = mean motion (deg/sec)
c    gm       product of gravitational constant and mass of the moon
c             (km**3/sec**2).  recommended value,
c           = .490279d4, for earth's moon
c    em       an array of 7 orbital elements of a moon in planet equator
c             of epoch similar to es.  see iephem for built-in
c             luni-solar ephemerides for the earth.
c    n, m, c, s
c             degree, order, of the spherical harmonic coefficients.
c             continue to as many spherical harmonics as the field
c             requires, up to 40x40 field.  n should be within columns
c             1 to 5, m should be within columns 6 to 10, cnm should be
c             within columns 11 to 40, and snm should be within columns
c             41 to 70.  gravity coefficients greater than l and m are
c             not computed in the force computation.  one may set up a
c             40x40 field here anyway even though a smaller field is
c             run.  the penalty is longer disk read time.
c  call subroutines  
c    julian, kozsak, kepler, coord, setsm, setthd, rk78cn, pout, rk78
c  references
c    jpl em 312/87-153, 20 april 1987
c  analysis
c    j. h. kwok - jpl  
c  programmer
c    j. h. kwok - jpl  
c  program modifications 
c    none
c  comments
c    note that the spherical harmonic coefficients are stored
c    as c22=c(3,3), etc.  coefs. are dimensioned for 40x40 field.   
c    inputs assume km, sec, kg, and degrees.
c
c    all the unit conversions should be made in this driver
c    program so that all subroutines would be consistent in units. 
c   
c*/

        real*8 en, em, gm, c, abserr, relerr, s, et, scmass, areas, 
     +   aread, 
     +   drag, es, gs, csrp, se, ea, ce, sp, t, h, dts, dtr, 
     +   zero, hlarge, hstart, delta_t, tints, rdens, ratm,
     +   rht, wt, altmax, sht, aj2, tr, tf, 
     +   ti, ge, pm, rate, re, cdrag, ellip

      integer n, neq, j, i, isrp, idens, idrag, iorb, iplot, 
     +         inode, iprint, iephem, l, m, imoon, isun, 
     +         ires

      real*8 orb(6),y(6),x(6)
      real*8 tint(2),tfin(2),tref(2)
      common/option/l,m,ires,isun,imoon,iephem,idrag,idens,isrp,iorb
     1 , iprint,inode,iplot
      common/atime/ti,tf,tr
      common/pltcon/ge,re,rate,pm,aj2,ellip,ratm
      common/atmcon/rdens,rht,sht,altmax,wt
      common/spccon/aread,areas,scmass,cdrag,csrp
      common/suncon/gs,es(7),et(7)
      common/muncon/gm,em(7),en(7)
      common/harmon/c(41,41),s(41,41)
      common/accurcy_err/relerr,abserr	!a.chu
      external der
      data neq/6/
      data dtr,dts/.1745329251994330d-1,8.64d4/
      data zero/0.d0/
      data hstart,hlarge/60.d0,1.d99/
c
c  begin reading input data.  this block can be replaced
c  if a namelist routine is available
c
cac      open(5,file='5')
cac      read(5,4000)l,m,ires,isun,imoon,iephem,idrag,idens,isrp,iorb
cac     1 , iprint,inode,iplot
cac      read(5,3000)(orb(i),i=1,6),relerr,abserr,step
cac      read(5,3000)(tint(i),i=1,2),(tfin(i),i=1,2),(tref(i),i=1,2)
cac      read(5,3000)ge,re,rate,pm,ellip,ratm
cac      read(5,3000)rdens,rht,sht,altmax,wt
cac      read(5,3000)aread,areas,scmass,cdrag,csrp
cac      read(5,3000)gs,(es(i),i=1,7)
cac      read(5,3000)gm,(em(i),i=1,7)
	 orb(1)=orb(1)/1000.d0	!km a.chu
c
c  begin output input data
c
c	write(6,*)' write i/p data'
c     	write((6,*)4000)l,m,ires,isun,imoon,iephem,idrag,idens,isrp,
c     1       iorb,iprint,inode,iplot
c     	write((6,*)3000)(orb(i),i=1,6),relerr,abserr,step
c     	write((6,*)3000)tint,tfin,tref
c     	write((6,*)3000)ge,re,rate,pm,ellip,ratm
c     	write((6,*)3000)rdens,rht,sht,altmax,wt
c     	write((6,*)3000)aread,areas,scmass,cdrag,csrp
c    	write((6,*)3000)gs,(es(i),i=1,7)
c     	write((6,*)3000)gm,(em(i),i=1,7)

c  input and output gravity field
c
      do 10 i=1,41
      do 10 j=1,41
      c(i,j)=zero
   10 s(i,j)=zero
      do 20 n=1,2000
cac      read(5,5000,end=30)i,j,c(i+1,j+1),s(i+1,j+1)
   20 continue
   30 continue
      if (l.eq.0.and.m.eq.0) go to 50
      do 40 i=2,l
      do 40 j=0,i
      if (j.gt.m) go to 40
c      write((6,*)5000)i,j,c(i+1,j+1),s(i+1,j+1)
   40 continue
   50 continue
c qdn 4/3/97 modified to add the missing input
      c(3,1) = -0.10826271d-2
      aj2=-c(3,1)
      if (ires.eq.0) ires=1
c
c  convert calendar date and time to julian date
c
cac	write(6,*)'gmt:',tint,tfin,tref
ccc      call julian(tint,tid)
ccc      call julian(tfin,tfd)
ccc      call julian(tref,trd)
ccc	write(6,*)'julian:',tid,tfd,trd
c     write(7,6000)tid,tfd,trd
c
c  unit conversion from deg to radian and day to seconds
c
      pm=pm*dtr
      rate=rate*dtr
      do 60 i=3,6
      orb(i)=orb(i)*dtr
      es(i)=es(i)*dtr
      em(i)=em(i)*dtr
   60 continue
      es(7)=es(7)*dtr
      em(7)=em(7)*dtr
cac      ti=tid*dts
cac      tf=tfd*dts
cac      tr=trd*dts
	ti=tints	!input passing a.chu 
	tf=ti+delta_t
	tr=ti		!reference time in sec
c
c  convert mean elements to osculating elements if necessary
c
      if (iorb.eq.1) then
        call kozsak(1,ge,re,aj2,orb,x)
        do 70 i=1,neq
   70   orb(i)=x(i)
      endif
c
c  change input orbital elements to cartesian coordinates
c
      do 80 i=1,neq
   80 y(i)=orb(i)
      call kepler(y(6),y(2),ea,se,ce)
      y(6)=ea
      call coord(y,ge,x)
c
c  set up rotational matrix for analytical ephemerides for the sun
c  and the moon.  besides sun and srp, setsun is required for moon
c  perturbation because the ecliptic angle is needed for transformation
c  from emo of date to eme of date
c
      if (iephem.eq.1) then
        call setsun(ti,es)
      endif
      if (isun.eq.1.or.isrp.eq.1) call setthd(es,et)      
      if (imoon.eq.1.and.iephem.eq.0) call setthd(em,en)
c
c  set up integrator parameters
c
      h=hstart
      t=ti
      call rk78cn
c
c  the variable sp gives an approx. time (sec) when the routine userop
c  is called.  userop is a user option routine.  it is used to compute
c  certain state of the trajectory to detect periapsis or apoapsis
c  passage.  when sp is set to a large number, it won't be called.  when
c  set to zero, it will be called every integration step.  userop can
c  be used to compute other quatities during the propagation such as
c  shadowing.  you can also use userop to perform maneuvers at a
c  particular time or do it automatically when certain orbital state
c  is reached such as eccentricity exceeds certain values.
c
      if (iprint.eq.0.and.inode.eq.0) then
        sp=hlarge
      else
        sp=zero
      endif
c
c  set up plotting file
c
      if (iplot.eq.1) open (8,file='8')
c
c  this is set up to print at fixed step intervals, see sp explanation
c  if you want to do advance programming
c
c	a.chu 8/31/94
cac  500 continue
      call pout(t,x)
c      tout=t+step   
cac      if (tout.gt.tf) tout=tf   
cac1      call rk78(der,t,tout,neq,x,h,relerr,abserr,sp)
        call rk78(der,t,tf,neq,x,h,relerr,abserr,sp)
	do i=1,6
	   x(i)=x(i)*1000.d0
	enddo
	
cac      if (tout.eq.tf) go to 900 
cac      go to 500 
cac  900 continue  
      call pout(t,x)
 3000 format(1p,bn,d30.16)
 4000 format(bn,i5)
 5000 format(1p,bn,2i5,2d30.16)
 6000 format(1p,1h1,/
     1      ,5x,'run starts on julian date             = ',d25.16,/
     2      ,5x,'run ends on julian date               = ',d25.16,/
     3      ,5x,'reference julian date of pm and ephem = ',d25.16)
cac      close (5)
      close (8)
      close (7)
      end
