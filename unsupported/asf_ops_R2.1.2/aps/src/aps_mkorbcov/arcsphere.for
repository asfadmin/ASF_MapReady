*.
*   ArcSphere - draws a circle on an oblate sphere centered at
*	lat/lon clatd,clon of radius radius
*
*   Args
*	clatd		double	input	center latd (deg)
*	clon		double	input	center lon (deg)
*	radius		double	input	radius of circle (deg)
*
*   Assumes
*	Central body has been set
*
*   History
*	 3/20/91 Changed problem if abs(clatd) = 90
*	 8/ 2/89
*
*   Last modified
*	3/20/91 11:22
*..
	subroutine ArcSphere(clatd,clon,radius)
	include 'eosinclude:map.inc'
	include 'eosinclude:constants.inc'
	double precision clatd,clon,radius
      character*100 SccsFileID
     -/'@(#)arcsphere.for	5.1 98/01/08 APS/ASF\0'/

* (deg)
	double precision latd,lon,dlon
* (rad)
	double precision r
	double precision yrotangle,zrotangle

* number of points to plot
	double precision n

	double precision pos(3),pos0(3),pos1(3),pos2(3)

	double precision platd,plon,plastlon,paltd
	integer i,MAXI
	parameter(MAXI=100)
	integer x,y,xy(MAXI)
	logical clip,clipp,clipd

c Functions
	double precision tolatc,vMag

	i = -1

	if(radius.le.0.0) then
	    return
	else if(radius.le.90.0) then
	    n = 4.0 * radius
	else
	    n = 4.0 * (180.0-radius)
	end if

	clipd = .false.
	plastlon = 0.0

	dlon = 365.0/n
	latd = 90.0 - radius
*** start BOGUS FIX
	if(abs(clatd).ne.90.0d0) then
	    yrotangle = halfpi - tolatc(clatd)*rads
	else
	    if(clatd .eq. 90.0d0) then
		yrotangle = 0.001d0 * rads
	    else
		yrotangle = pi - 0.001d0 * rads
	    end if
	end if
*** end BOGUS FIX
	zrotangle = pi - clon*rads

	do lon=0.0,365.0,dlon

c..aap	    call SphereXYZ(latd,lon,0.0,pos)
	    call SphereXYZ(latd,lon,dzero,pos)

	    call vUnit(pos,pos0)
	    call yRot(pos0,yrotangle,pos1)
	    call zRot(pos1,zrotangle,pos2)
	    call vMult(vMag(pos),pos2,pos0)
	    call XYZSphere(pos0,platd,plon,paltd)

c..aap	    call project(platd,plon,0.0,x,y,clipp)
	    call project(platd,plon,dzero,x,y,clipp)
	    if(MapWrap) call DateLine(plastlon,plon,clipd)
	    plastlon = plon
	    clip = clipp .or. clipd

	    if(.not.clip) then
		i = i + 2
		xy(i) = x
		xy(i+1) = y
		if(i.eq.MAXI-1) then
		    call vector(xy,(i+1)/2)
		    xy(1) = xy(i)
		    xy(2) = xy(i+1)
		    i = 1
		end if
	    else
		if(i.gt.1) call vector(xy,(i+1)/2)
		i = - 1
	    end if
	end do

	if(i.gt.1) call vector(xy,(i+1)/2)

999	continue
	end
