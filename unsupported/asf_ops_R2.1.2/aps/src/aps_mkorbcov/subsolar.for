
*.
*   Subsolar - find subsolar lat/lon given year and time of year
*
*   Args
*	year			integer	input	year
*	gmt			double	input	gmt (sec) (per INITEPOCH,
*						  Jan 1st at 12:01 AM is
*						  86400 + 60 sec)
*	sublatd			double	output	geod. lat. of subsolar point (deg)
*	sublon			double	output	longitude of subsolar point (deg)
*
*   Assumes
*	Earth is the central body
*
*	03/28/89 12:47
*..
	subroutine subsolar(year,gmt,sublatd,sublon)
	include 'eosinclude:constants.inc'
	include 'eosinclude:cbody.inc'
	include 'eosinclude:time.inc'

	  character*100 SccsFileID
     -/'@(#)subsolar.for	5.1 98/01/08 APS/ASF\0'/
	integer year
	double precision gmt
	double precision sublatd,sublon

	double precision sunday,sunhour

cccc SPOC - JSC Version (doesn't work) (I copied it wrong?) cccc
c
c	real*4 MagicNumber
c	parameter(MagicNumber=2444239.5)
c	real*4 julconst,juldate,daysec,msun,fsun,usun,gha
c	real*4 esun,esunsq,esuncu,epssun,rasun,dd,gmtstcon
c	real*4 sublatc
c
c	real*4 tolatd		! function
c
c	juldate = 367.0*year - int(1.75*year + 1721043.5 + gmt/86400.0)
c	julconst = juldate - MagicNumber
c	msun = (357.22274 + .0985600267*julconst)*rads
c	esun = .016717533 - 1.144e-9*julconst
c	esunsq = esun*esun
c	esuncu = esun*esunsq
c	fsun = msun + (2*esun - .25*esuncu)*sin(msun)
c     .		+ 1.25*esunsq*sin(2.0*msun) + 1.083333333*esuncu*sin(3*msun)
c	usun = fsun + (282.5964503 + 4.70684e-5*julconst)*rads
c	epssun = (23.44188347 - 3.5626e-7*julconst)*rads
c	rasun = atan2(cos(epssun)*sin(usun),cos(usun))
c	if(rasun.lt.0.0) rasun = rasun + 2.0*pi
c
c	dd = 367.0*float(year) - int(1.75*float(year)) + 1721043.5
c     .		+ int(gmt/86400.0) - 2433282.5
c	daysec = (gmt/86400.0 - int(gmt/86400.0))*86400.0
c	gmtstcon = (6.67170278 + .0657098232*dd
c     .			+ 1.0027379093*(daysec/3600.0))*15.0
c	gha = (gmtstcon/360.0 - int(gmtstcon/360.0))*360.0*rads
c
c	sublatc = asin(sin(epssun)*sin(usun))
c	sublatd = tolatd(sunlatc)/rads		! (deg)
c	sublon = (rasun - gha)/rads
c	if(sublon.gt.180.0) then
c	    sublon = sublon - 360.0
c	else if(sublon.lt.-180.0) then
c	    sublon = sublon + 360.0
c	end if
c
cccc

	sunday = gmt/86400.0d0
	sunhour = (gmt-86400.0d0*int(sunday))/3600.0d0

c	sublatd = 23.45d0 * sin((sunday-80.0d0)*twopi/365.0d0)
	sublatd = 23.45d0 * sin((sunday-80.0d0)*twopi/365.2422d0)
	sublon = -15.0d0 * (sunhour-12.0d0)

	if(sublon.gt.180.0d0) then
	    sublon = sublon - 360.0d0
	else if(sublon.lt.-180.0d0) then
	    sublon = sublon + 360.0d0
	end if

	end
