*.
*   DrawMask - draw station masks
*
*   Args
*	altd		double	input	altitude of spacecraft (km)
*	col		integer	input	DN/line style to draw with
*
*	08/03/89 08:22
*..
	subroutine DrawMask(altd,col)

** Includes
	include 'eosinclude:cbody.inc'
	include 'eosinclude:constants.inc'
	include 'eosinclude:map.inc'
	include 'eosinclude:station.inc'
	include 'eosinclude:display.inc'

	 character*100 SccsFileID
     -/'@(#)drawmask.for	5.1 98/01/08 APS/ASF\0'/
** Variables

* Arguments
	double precision altd
	integer col

* Mask
	integer istat
	double precision MaskRadius
	integer iaz
	double precision az,daz,elev

* Plot
	integer xy(200)		! plot array
	integer x,y,j
	integer ijk,jk
	double precision stnlat(200),stnlon(200)
	double precision pos(3),pos0(3),pos1(3),pos2(3)
	double precision latd,platd,plon,plastlon,paltd
	double precision yrotangle,zrotangle
	logical clip,clipp,clipd

* Functions
	double precision tolatc,vMag


	if(disp .eq. NONE)open(unit=UnitStn,file='stnmsk.dat',form='formatted')

	call UseCol(col)

	if(NumStations.gt.0) call GrafComment('Station masks')

	do istat=1,NumStations

	    ijk=0
	    if(StationSelected(istat)) then

		call targetp(StationLatd(istat),StationLon(istat))

		if(disp .eq. NONE .and. StationLatd(istat) .ne. 0. 
	1		.and. StationLon(istat) .ne. 0.)
	1		write(UnitStn,1002)StationLatd(istat),StationLon(istat)

		j = -1
		clipd = .false.
		plastlon = 0.0
    
		daz = 360.0d0/AzSteps(istat)
		yrotangle = halfpi - tolatc(StationLatd(istat))*rads
		zrotangle = pi - StationLon(istat)*rads
    
		do iaz=1,AzSteps(istat)+1
		    if(iaz.le.AzSteps(istat)) then
			az = (iaz-1)*daz
			elev = max(MinElev(istat),MaskElev(iaz,istat))
			call FindMaskRadius(elev,altd,MaskRadius)
		    else
			az = 360.0d0
			elev = max(MinElev(istat),MaskElev(1,istat))
			call FindMaskRadius(elev,altd,MaskRadius)
		    end if

c Somehow mask is mirrored about az=0, so fix az for now and fix it later
c 3/4/88 14:25
	AZ = -AZ
c
		    latd = 90.0d0 - MaskRadius
    
c..aap		    call spherexyz(latd,az,0.0,pos)
		    call spherexyz(latd,az,dzero,pos)
		    call vunit(pos,pos0)
		    call yrot(pos0,yrotangle,pos1)
		    call zrot(pos1,zrotangle,pos2)
		    call vMult(vMag(pos),pos2,pos0)
		    call xyzsphere(pos0,platd,plon,paltd)
    
c..aap		    call project(platd,plon,0.0,x,y,clipp)
		    call project(platd,plon,dzero,x,y,clipp)
		    ijk=ijk+1
		    stnlat(ijk)=platd
		    stnlon(ijk)=plon
		    if(MapWrap) call DateLine(plastlon,plon,clipd)
		    plastlon = plon
		    clip = clipp .or. clipd
    
		    if(.not.clip) then
			j = j + 2
			xy(j) = x
			xy(j+1) = y
		    else
			if(j.gt.1) call vector(xy,(j+1)/2)
			j = - 1
		    end if
		end do
1000		format(I4)
1002		format(2f10.2)
    
c..aap		if(j.gt.1) call vector(xy,(j+1)/2)
    
	    end if	! StationSelected(istat)
	    if(disp .eq. NONE .and. ijk .gt. 0)then
	    write(UnitStn,1000)ijk
	    do jk=1,ijk
		write(UnitStn,1002)stnlat(jk),stnlon(jk)
	    end do
	    end if

	end do	! istat=1,NumStations
	if(disp .eq. NONE)close(UnitStn)

999	continue
	end


c
c   FindMaskRadius (deg)
c
c   Purpose
c	Find the geocentric angle between a point on the surface and a
c	body that is at a given elevation relative to that point

	subroutine FindMaskRadius(elev,altd,radius)
	include 'eosinclude:cbody.inc'
	include 'eosinclude:constants.inc'
	double precision elev,altd,radius

	double precision a,SlantRange,elevPrime
	double precision qa,qb,qc		! quadratic equation a,b,c

	a = RBody + altd
	elevPrime = halfpi + elev*rads

c Triangle has sides RBody, SlantRange, a and angles ?, radius, elevPrime
c Use cosine law: a**2 = RBody**2 + SlantRange**2
c                            - 2*RBody*SlantRange*cos(elevPrime)
c  and rearrange to find SlantRange with quadratic formula

	qa = 1.0
	qb = -2.0 * RBody * cos(elevPrime)
	qc = RBody*RBody - a*a

	SlantRange = (-qb + sqrt(qb*qb - 4.0*qa*qc)) / (2.0 * qa)

c use sine law: ( sin(elevPrime) / a ) = ( sin(radius) / SlantRange )

	radius = asin((SlantRange/a) * sin(elevPrime)) / rads

	end
