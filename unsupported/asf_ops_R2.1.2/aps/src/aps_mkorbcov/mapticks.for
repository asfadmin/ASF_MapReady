
*.
*   MapTicks - draw mapticks
*
*	03/18/89 20:14
*..
	subroutine MapTicks

	include 'eosinclude:display.inc'
	include 'eosinclude:map.inc'
	include 'eosinclude:constants.inc'

	double precision sgn,latd,lon,dlatd,latd1,latd2,lon1,lon2
	double precision MinWidth,TickInterval,TickSize
	double precision radius
	integer TickXY(4)
	logical clip1,clip2

	  character*100 SccsFileID
     -/'@(#)mapticks.for	5.1 98/01/08 APS/ASF\0'/
** Functions
	logical twixt

	call GrafComment('Ticks')

	if(RectProj) then

	    MinWidth = min(abs(WideX),abs(WideY))

	    if(MinWidth.gt.50.0) then
		TickInterval = 10.0
	    else if(MinWidth.gt.20.0) then
		TickInterval = 5.0
	    else if(MinWidth.gt.10.0) then
		TickInterval = 2.0
	    else
		TickInterval = 1.0
	    end if

	    TickSize = TickInterval/10.0

	    latd1 = sign(abs(winy(1)-mod(abs(winy(1)),TickInterval)),winy(1))
	    lon1 = sign(abs(winx(1)-mod(abs(winx(1)),TickInterval)),winx(1))
	    latd2 = sign(abs(winy(2)-mod(abs(winy(2)),TickInterval)),winy(2))
	    lon2 = sign(abs(winx(2)-mod(abs(winx(2)),TickInterval)),winx(2))
	    if(MapCut) lon2 = lon2 + 360.0

c Top/Bottom (Longitude) ticks

	    do lon=lon1,lon2,TickInterval
c..aap		call project(winy(1),lon,0.0,TickXY(1),TickXY(2),clip1)
		call project(winy(1),lon,dzero,TickXY(1),TickXY(2),clip1)
c..aap		call project(winy(1)-TickSize,lon,0.0,TickXY(3),TickXY(4),clip2)
		call project(winy(1)-TickSize,lon,dzero,TickXY(3),
     .		    TickXY(4),clip2)
c		if(.not.(clip1 .or. clip2)) call vector(TickXY,2)
		TickXY(2) = ScreenY(1)
		if(twixt(screenx(1),TickXY(1),screenx(2)))
     .		    call vector(TickXY,i2two)
c..aap     .		    call vector(TickXY,2)
c..aap		call project(winy(2),lon,0.0,TickXY(1),TickXY(2),clip1)
c..aap		call project(winy(2)+TickSize,lon,0.0,TickXY(3),TickXY(4),clip2)
		call project(winy(2),lon,dzero,TickXY(1),TickXY(2),clip1)
		call project(winy(2)+TickSize,lon,dzero,TickXY(3),
     .		    TickXY(4),clip2)
c		if(.not.(clip1 .or. clip2)) call vector(TickXY,2)
		TickXY(2) = ScreenY(2)
		if(twixt(screenx(1),TickXY(1),screenx(2)))
     .		    call vector(TickXY,i2two)
c..aap     .		    call vector(TickXY,2)
	    end do

c Left/Right (Latitude) ticks

	    lon1 = winx(1)
	    if(MapWrap) lon1 = lon1 + .001
	    lon2 = winx(2)
	    if(MapWrap) lon2 = lon2 - .001

	    do latd=latd1,latd2,-TickInterval
		call project(latd,lon1,dzero,TickXY(1),TickXY(2),clip1)
		call project(latd,lon1+TickSize,dzero,TickXY(3),
     .		    TickXY(4),clip2)
c..aap		call project(latd,lon1,0.0,TickXY(1),TickXY(2),clip1)
c..aap		call project(latd,lon1+TickSize,0.0,TickXY(3),TickXY(4),clip2)
c		if(.not.(clip1 .or. clip2)) call vector(TickXY,2)
		TickXY(1) = ScreenX(1)
c..aap		call vector(TickXY,2)
		call vector(TickXY,i2two)
		call project(latd,lon2,dzero,TickXY(1),TickXY(2),clip1)
		call project(latd,lon2-TickSize,dzero,TickXY(3),
     .		    TickXY(4),clip2)
c..aap		call project(latd,lon2,0.0,TickXY(1),TickXY(2),clip1)
c..aap		call project(latd,lon2-TickSize,0.0,TickXY(3),TickXY(4),clip2)
c		if(.not.(clip1 .or. clip2)) call vector(TickXY,2)
		TickXY(1) = ScreenX(2)
		call vector(TickXY,i2two)
c..aap		call vector(TickXY,2)
	    end do

c	else if(proj.eq.GLOBE .or. proj.eq.NPOLE .OR. proj.eq.SPOLE) then
c
c	    do lon = 0.0,166.0,15.0
c		call ArcSphere(0.0,lon,90.0)
c	    end do
c
c	    do radius=15.0,165.0,15.0
c		call ArcSphere(90.0,0.0,radius)
c	    end do

	end if

c	call targetp(-67.5,47.8)

	end
