C Alaska SAR Processor (ASP) %W% %E% %U%
	subroutine initdops (sv,roll,yaw,pitch,xlambda,re,f,range,
     .gamma,fd,fdot,v_swath,v_orbital,v_target,v_relative,istat)
c/*	subroutine initdops (sv,roll,yaw,pitch,xlambda,re,f,range, -------------
c    .gamma,fd,fdot,v_swath,v_orbital,v_target,v_relative,istat)
c
c   Author 	R.E. Carande	Date	march 12, 1989.
c
c	This subroutine is used to determine Doppler parameters, and
c	others, given a statevector, attitude, wavelength and earth model
c	for a SAR.
c
c	INPUT:		sv	6 element, double precision, statevector.
c				x,y,z,vx,vy,vz, units are m and m/s
c			roll	roll of s/c (deg)
c			yaw	yaw of s/c (deg)
c			pitch	pitch of s/c (deg)
c			xlambda	Radar wavelength (m)
c			re	Earth radius at equator (m)
c			f	1 / ellipsoidal flattening factor
c			range	slant range of interest
c			istat	=1 for verbose, otherwise non-verbose
c	
c	RETURN 		gamma	SAR look angle (deg)
c			fd	Doppler center Frequency (Hz)
c			fdot	Doppler rate (Hz/s)
c			v_swath	velocity of SAR footprint on Earth surface
c			v_orbital	orbital velocity of SAR
c			v_target	velocity of target on Earth surface
c			v_relative	relative vel betw targ and SAR
c			(last 4 velocities in m/s)
c			istat	= 0 successfull, less than 0 = unsuc.
c
c	Note:  all calling variables are floating (real*4), except for
c		sv which is double precision.   All internal variables
c		to this subrouinte are double precision.
c
c	Major external routines used:	getlook, getdop2,makes_chi
c
c   all common blocks are used internally.
c*/
	implicit double precision (a-h,o-z)
	double precision sv(6),rot(3,3),xsc(3),getlook,calcr
        real*8    p,p1,v_adj_rel
	real roll,yaw,pitch,xlambda,re,f,range,gamma,fd,fdot
	real v_swath,v_orbital,v_target,v_relative
	character*4 choice
	character*1 side
	logical verbose
	common /relative/tr1,tr2,tr3,tv1,tv2,tv3,vv1,vv2,vv3,ra1,ra2,ra3
	common /earth/ rp_d,re_d
	common /transform/ rot
	data pi /3.141592654/
	data choice /'USEA'/, side /'R'/  
	if (istat.eq.1) then 
		verbose=.true.
	else
		verbose=.false.
	end if
	xsc(1)=sv(1)
	xsc(2)=sv(2)
	xsc(3)=sv(3)
c
c  calculate common block parameters
c
	call makes_chi (sv,rot)
	re_d = dble(re)
	rp_d = re_d*(1.-1./dble(f))
	if (verbose) write(6,*)'earth model: ',re_d,rp_d
c
c   convert to double precision
c
	roll_d = dble(roll)*pi/180.
	yaw_d = dble(yaw)*pi/180.
	pitch_d = dble(pitch)*pi/180.
	range_d = dble(range)
	xlambda_d = dble(xlambda)
	if(verbose) write(6,*)'r,y,p,r,xl',
     .		roll_d,yaw_d,pitch_d,range_d,xlambda_d
c
c   calculate az angle a and look angle gamma ...
c   could improve this by iterating more than once.
c
	a_d=0.d0
	rprec=1.0d0
	g1 = getlook (xsc,range_d,rprec,side,choice,a_d)+roll_d ! Assume 0yaw,pitch 
	if (g1.ge.pi/2.1) then ! error in getlook
		istat=-1
		write(6,*)'initdops:  error in getlook call #1'
		return
	end if
	if(verbose) write(6,*)'g1',g1
	a_d = dasin(dsin(g1)*dsin(yaw_d) - dcos(g1)*dsin(pitch_d) )
	if (verbose) write(6,*)'a_d',a_d,' radians'
	gamma_d = getlook (xsc,range_d,rprec,side,choice,a_d) + roll_d  !good aprox
	if (gamma_d.ge.pi/2.1) then ! error in getlook
		istat=-2
		write(6,*)'initdops:  error in getlook call #2'
		return
	end if

	if (verbose) write(6,*)'gamma_d',gamma_d
C	a = sngl (a_d * 180./pi)
	gamma = sngl (gamma_d * 180./pi)
	if (verbose) write(6,*)'final gamma',gamma
	
c
c   calculate local altitude by calc. slant range straigh down.
c
	h=dsqrt(xsc(1)**2 + xsc(2)**2 + xsc(3)**2)
	alt=calcr(xsc,0.d0,0.d0)
	erad = dsqrt(xsc(1)**2 + xsc(2)**2 + xsc(3)**2 ) - alt
	if (verbose) write(6,*)'altitude and erad',alt,erad
c
c   get doppler parameters (and velocity info via common block relative)
c
	v_orbital= sngl( dsqrt(sv(4)**2 + sv(5)**2 + sv(6)**2) )
	call getdop2 (xlambda_d,sv,gamma_d,a_d,fd_d,fdot_d)
	fd = (-1.) * sngl(fd_d)
	fdot = sngl (fdot_d)
	v_target = sngl(dsqrt(tv1**2+tv2**2+tv3**2))
	v_relative = sngl( dsqrt(vv1**2 + vv2**2 + vv3**2) )
c Note that p is the sattelite to earth projection factor
	p = erad/h*(h**2 + erad**2 - range_d**2)/(2.*h*erad)
        p1 = 1.0/p
c Compute the adjusted relative velocity
        v_adj_rel = dsqrt( (vv1+(p1-1)*tv1)**2 +
     .                     (vv2+(p1-1)*tv2)**2 +
     .                     (vv3+(p1-1)*tv3)**2 )
        v_swath = sngl ( p*v_adj_rel )
	if(verbose) write(6,*)'velocities orb,t,rel,sw:',v_orbital,
     .v_target,v_relative,v_swath
	istat=0 
	return
	end	
