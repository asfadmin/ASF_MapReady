*.
*   GetElements1 - reads orbital elements for a platform
*
*   Args
*	a		double	output	semi-major axis (km)
*	e		double	output	eccentricity
*	i		double	output	inclination (rad)
*	node0		double	output	lon. of ascending node at t0 (rad)
*	omega0		double	output	arg. of periapsis t0 (rad)
*	m0		double	output	mean anomaly at t0 (rad)
*	t0		double	output	time since epoch (sec)
*
*	02/13/89 10:22
*..
	subroutine GetElements1(a,e,i,node0,omega0,m0,t0)

	include 'eosinclude:cbody.inc'
	include 'eosinclude:time.inc'
	include 'eosinclude:constants.inc'

	  character*100 SccsFileID
     -/'@(#)getelems1.for	5.1 98/01/08 APS/ASF\0'/
	double precision a,e,i,node0
	double precision omega0,m0,t0

	integer MAXORBITS,iorbit,NumOrbits
	parameter(MAXORBITS=99)
	character*80 FormatElements
	double precision DefaultElements(7,MAXORBITS)
	character*10 OrbitName(MAXORBITS)

	double precision lasta,laste,lasti,lastnode0,lastomega0,lastm0,lastt0
	double precision period,node0hour,sublatd,sublon,tof

	double precision dnode,domega,danom
	integer slen,j
	logical init

c Save
	save init
c Data
	data init /.true./

* Functions
	integer StringLen

	call CheckLogName

c Use default elements

	call ReadElements(OrbitName,DefaultElements,NumOrbits,int(MAXORBITS))

	if(NumOrbits.lt.1) go to 30

	write(*,*)
20	write(*,'(a,$)') '   Select orbit [none]: '
	read(*,'(i10)',err=20,end=999) iorbit

	if(iorbit.gt.0 .and. iorbit.le.NumOrbits) then
	    a      = DefaultElements(1,iorbit)
	    e      = DefaultElements(2,iorbit)
	    i      = DefaultElements(3,iorbit)
	    node0  = DefaultElements(4,iorbit)
	    omega0 = DefaultElements(5,iorbit)
	    m0     = DefaultElements(6,iorbit)
	    t0     = DefaultElements(7,iorbit)
	    go to 50
	end if

c User input elements

30	continue

	if(init) then
	    call NewPage
	    write(*,'(a,/)') ' Orbital elements input '
	    write(*,'(a)') '      a: semi-major axis (km)'
	    write(*,'(a)') '      e: eccentricity'
	    write(*,'(a)') '      i: inclination (deg)'
	    write(*,'(a)') '  node0: longitude of ascending node at t0 (deg)'
	    write(*,'(a)') ' omega0: argument of perigee at t0 (deg)'
	    write(*,'(a)') '     m0: mean anomaly at t0 (deg)'
	    write(*,'(a)') '     t0: time of these elements (relative to base gmt)
     . (hr)'
	    write(*,'(/a)') ' To enter equatorial altitude instead of semi-'
	    write(*,'(a)') '   major axis use e=0.0'
	    write(*,'(/a)') ' To specify a local time at node crossing instead'
	    write(*,'(a)') '   of longitude use node0=999.0'
	    write(*,*)
	    init = .false.
	end if

	write(*,*)
40	write(*,'(a)') ' Enter a (or h),e,i,node0,omega0,m0,t0 (km,deg,hr): '

c	if(.not.first(iplat)) then
c	    if(e.eq.0.0) then
c		lasta = a - RBody
c	    else
c		lasta = a
c	    end if
c	    laste = e
c	    lasti = i/rads
c	    lastnode0 = node0/rads
c	    lastomega0 = omega0/rads
c	    lastm0 = m0/rads
c	    lastt0 = t0/3600.0
c	    write(*,45) lasta,laste,lasti,lastnode0,lastomega0,lastm0,lastt0
c45	        format('   [',f10.4,2x,f8.6,2x,f9.4,2x,f9.4,2x,f9.4,2x,f9.4,2x,
c     .								f12.4,']: ')
c	end if

	read(*,'(7f12.0)',err=40,end=999) a,e,i,
     .			node0,omega0,m0,t0
	if(a.eq.0) then
	    a = lasta
	    e = laste
	    i = lasti
	    node0 = lastnode0
	    omega0 = lastomega0
	    m0 = lastm0
	    t0 = lastt0
	end if

50	continue

	if(e.eq.0.0) a = a + RBody

	i = i * rads
	node0 = node0 * rads
	omega0 = omega0 * rads
	m0 = m0 * rads
	t0 = t0 * 3600.0d0

	if(node0.gt.twopi) then
	    write(*,*)
60	    write(*,'(a,$)') ' Enter local node crossing time (hours): '
	    read(*,'(f20.0)',err=60,end=999) node0hour
	    call LocalTimeToNode(a,e,i,node0,omega0,m0,t0,node0hour)
	end if


999	continue
	end
