*.
*   FlyItSimul
*
*   Args
*	dt			double	input	time step (sec)
*
*   History
*	06/10/93 updated to match flyit.for
*	03/06/91 Changed MASKSUN to MASKDAY
*	03/23/89
*
*   Last modified
*	03/06/91 10:16
*..
	subroutine FlyItSimul(dt)
	double precision dt

** Includes
	include 'eosinclude:instruments.inc'
	include 'eosinclude:orbit.inc'
	include 'eosinclude:eossimul.inc'
	include 'eosinclude:time.inc'
	include 'eosinclude:constants.inc'
	include 'eosinclude:map.inc'
	include 'eosinclude:station.inc'
	include 'eosinclude:display.inc'

	  character*100 SccsFileID
     -/'@(#)flyitsimul.for	5.1 98/01/08 APS/ASF\0'/
** Variables

* Graphics
	integer XYDIM
*     Make sure XYDIM is an even number
	parameter(XYDIM=1000)
	integer xy(XYDIM,MAXTRACK,MAXINSTPERPLAT,MAXPLAT)
	integer x(MAXTRACK),y(MAXTRACK)
	integer j(MAXPLAT,MAXINSTPERPLAT),jii
	integer iscan(MAXPLAT,MAXINSTPERPLAT)
	integer iplat,iinst,itrack,itype

* Orbit
	double precision latd(MAXPLAT),lon(MAXPLAT),altd(MAXPLAT)
	double precision lastlon(MAXPLAT)
	double precision r(3,MAXPLAT),v(3,MAXPLAT)
	double precision period

* Time
	double precision t,tmin,tmax

* Swath
	double precision LookAng,YawAng,beamw,bx
	double precision swlatd(MAXTRACK,MAXINSTPERPLAT,MAXPLAT)
	double precision swlon(MAXTRACK,MAXINSTPERPLAT,MAXPLAT)
	double precision swaltd(MAXTRACK,MAXINSTPERPLAT,MAXPLAT)
	double precision lastswlon(MAXTRACK,MAXINSTPERPLAT,MAXPLAT)

	integer k,l
	integer ip,ipsim1,ipsim2
	integer ppc,cpc

	logical clip,ClipProj(MAXTRACK)
	logical ClipDate,ClipSun,ClipMask,ClipAsc,ClipDsc

	integer istat
	logical inMasks

c	logical ShowMET
	integer metday,methour,metmin
	double precision metsec

	logical err
	logical swerr

	logical ShowTimeTicks,TickOnly
	double precision TickScale
	integer index
	logical AnySimul

    	logical lastanysimul
c    	integer rev			! Mod for keeping track of rev's
    	integer rev(MAXPLAT)		! of multiple platforms
    	double precision lastz
    	character asc
    	integer ixt,iyt,lent
    	character*10 revt

** AAP added

** Functions
	double precision FindBX
	logical daytime,dTwixt,InMaskR
    	integer stringlen

    	if(disp.eq.none) then 
		open(unit=unitnone,file='none.dat',
     .				form='formatted',status='new')
	    	open(unit=UnitAdd,file='overlap.dat',
     .				form='formatted',status='new')
	end if

	if(.not.SimulOk) go to 999

* Init simul stack

	ipsim1 = platform(simplat(1),siminst(1))
	ipsim2 = platform(simplat(2),siminst(2))

	call InitSimul(dt,tBias,tracks(ipsim1),tracks(ipsim2))
	call UseCol(ColSimul)

* Set plotting variables

	do index=1,2
	    j(index,siminst(index)) = -1
	end do

c	if(NumPlat.gt.1) then
c	    if(ShowSimul) then
		k = 0
c	    else
c		write(*,*)
c210	        write(*,'(a,$)') ' Speedy plotting [yes]? '
c	        read(*,'(a)',err=210,end=999) YesNo
c	        if(YesNo.eq.'N' .or. YesNo.eq.'n') then
c		    k = 0
c	        else
c		    k = -1
c	        end if
c	    end if
c	else
c	    k = -1
c	end if

c	Set ppc: ppc is the number of clicks to plot at a time
c	  e.g. if ppc is 5 we will draw the swath for 5 time intervals (dt)
c	  each time we draw

	ppc = 3 + 1

	call cleart

c	if(TwoMonitors(disp)) then
c	    call cleartty
c	    do iplat=1,numplats
c	        write(*,280) iplat,a(iplat)-RBody,i(iplat)/rads,
c     .			node0(iplat)/rads,
c     .			int(twopi * sqrt(a(iplat)**3/mu) / 60.0)
c280	        format(//' Platform',i2,'   Altd:',f9.2,'   Inc:',f8.2,
c     .			'   Node0:',f8.2,'   Period:',i4,'m',//,
c     .			'   Instrument    Look     FOV    Swath')
c	        do iinst=1,NumInstPlat(iplat)
c		    ip = platform(iplat,iinst)
c		    write(*,290) instname(ip),lookps(iplat,iinst),
c     .				beamwidps(ip),swathps(ip)
c290		    format(3x,a10,2x,f6.2,2x,f6.2,2x,f7.2)
c	        end do
c	    end do
c	    if(ShowSimul) write(*,'(/a,f6.1,a/)')
c     .				' tBias time:',tBias/60.0,'m'
c	end if

c	if(ShowMET) write(*,'(//)') 

	call GrafComment('Fly')

	tmin = 1.0e20
	tmax = -tmin
	do index=1,2
c	    if(active(simplat(index)) then
		tmin = min(tmin,tstart(simplat(index)))
		tmax = max(tmax,tend(simplat(index)))
c	    end if
	end do

	do index=1,2
		call NodePeriod(a(index),i(index),period)
		rev(index)=tstart(index)/period+1.0
	end do
	rev(2)=rev(2)-1
c    	rev = 1			! Mod .. aap .. 10/26/93
	lastz = 0.0

	do t=tmin,tmax,dt

	    call subsolar(BaseYear,BaseGMT+t,sunlatd,sunlon)

c	    if(ShowMET) then
c		if(t.ge.0.0) then
c		    call istodhms(t,metday,methour,metmin,metsec)
c		    write(*,'(''+ MET '',i4,''/'',i2.2,'':'',i2.2)') 
c     .			metday,methour,metmin
c		else
c		    call istodhms(-t,metday,methour,metmin,metsec)
c		    write(*,'(''+ MET '',i4,''/'',i2.2,'':'',i2.2)') 
c     .			-metday,methour,metmin
c		end if
c	    end if

	    if(k.ne.-1) k = k + 1

	    do index=1,2

		iplat = simplat(index)

c Too early/late for a platform to be flying

		if(dTwixt(t,tstart(iplat),tend(iplat))) then

*		  Too early - next platform
		    go to 400

		else if(dTwixt(tstart(iplat),tend(iplat),t)) then

*		  Too late
		    go to 400

		end if

		lastlon(iplat) = lon(iplat)

		call ephem2(a(iplat),e(iplat),i(iplat),node0(iplat),
     .				omega0(iplat),m0(iplat),t,t0(iplat),
     .				latd(iplat),lon(iplat),altd(iplat),
     .				r(1,iplat),v(1,iplat))

c Mod to keep track of rev's of multiple platforms .. aap .. 10/26/93
c
c    		if(iplat.eq.1 .and. r(3,1).ge.0.0 .and. lastz.lt.0.0) then
c     			rev = rev + 1
c    		endif
    		if(r(3,iplat).ge.0.0 .and. lastz.lt.0.0) then
     			rev(iplat) = rev(iplat) + 1
    		endif

c    		lastz = r(3,1)
    		lastz = r(3,iplat)

		if(NumStations.lt.1) then
		    ClipMask = .false.
		else
		    inMasks = .false.
		    do istat=1,NumStations
			if(StationSelected(istat)) then
			    inMasks = inMasks .or. InMaskR(r(1,iplat),istat)
			end if
		    end do
		    ClipMask = .not.inMasks
		end if

		do iinst=siminst(index),siminst(index)

		    ip = platform(iplat,iinst)
		    itype = InstType(ip)

		    if(swathwid(ip).ne.0.0) then
			bx = FindBX(latd(iplat),altd(iplat),
     .						LookPoint(iplat,iinst))
			beamw = (atan((bx+.5*swathwid(ip))/altd(iplat))
     .			  - atan((bx-.5*swathwid(ip))/altd(iplat)))/rads
		    else
			beamw = beamwid(ip)
		    end if

d	write(*,*) 'BX,Beamwid:',bx,beamwid

		    if(itype.ne.SCANS .and.
     .			  itype.ne.SCANR .and. itype.ne.SCANY) then
			LookAng = LookPoint(iplat,iinst)
			YawAng  = YawPoint(iplat,iinst)
		    else
			if(itype.eq.SCANS) then
			    LookAng = ScanLook(iplat,iinst,
     .						iscan(iplat,iinst))
			    YawAng = ScanYaw(iplat,iinst,
     .						iscan(iplat,iinst))
			    iscan(iplat,iinst)
     .				= mod(iscan(iplat,iinst),
     .					NumScan(iplat)) + 1
			else if(itype.eq.SCANR) then
			else if(itype.eq.SCANY) then
			end if
		    end if

		    do itrack=1,tracks(ip)
			lastswlon(itrack,iinst,iplat)
     .				= swlon(itrack,iinst,iplat)
		    end do

		    call swath(r(1,iplat),v(1,iplat),
     .				latd(iplat),lon(iplat),altd(iplat),
     .				LookAng,beamw,YawAng,
     .				iplat,iinst,
     .				swlatd(1,iinst,iplat),
     .				swlon(1,iinst,iplat),
     .				swaltd(1,iinst,iplat),
     .				itype,swerr)

		    jii = j(index,iinst)

		    if(swerr) then
			clip = .true.
			go to 300
		    else
			clip = .false.
		    end if

d	write(77,*) 'P ',iplat,latd(iplat),lon(iplat),altd(iplat)

		    do itrack=1,tracks(ip)

d	write(77,*) 'I T ',iinst,itrack
d	write(77,*) swlatd(itrack,iinst,iplat),swlon(itrack,iinst,iplat)

			if(MapWrap) then
			    call DateLine(lastswlon(itrack,iinst,iplat),
     .					swlon(itrack,iinst,iplat),ClipDate)
			else
			    ClipDate = .false.
			end if

			if(.not.mask(MASKDAYT,ip)) then
			    ClipSun = .false.
			else
			    ClipSun = .not.
     .				DayTime(swlatd(itrack,iinst,iplat),
     .					swlon(itrack,iinst,iplat),
     .					halfpi-SunElevMin(ip))
			end if

			if(.not.mask(MASKASC,ip)) then
			    ClipAsc = .false.
			else
			    ClipAsc = (v(3,iplat).lt.0.0)
			end if

			if(.not.mask(MASKDSC,ip)) then
			    ClipDsc = .false.
			else
			    ClipDsc = (v(3,iplat).ge.0.0)
			end if

			call project(   swlatd(itrack,iinst,iplat),
     .					swlon(itrack,iinst,iplat),
     .					swaltd(itrack,iinst,iplat),
     .					x(itrack),y(itrack),
     .					ClipProj(itrack))
			clip = clip .or. ClipProj(itrack)
     .					.or. ClipDate .or. ClipSun
     .					.or. (ClipMask .and. mask(MASKSTN,ip))
     .					.or. ClipAsc .or. ClipDsc

			if(itrack.eq.2 .or. itrack.eq.4) then
			    if(clip) then
c..aap				call AddNewBox(
c..aap     .					index,itrack,
c..aap     .					10000,10000,
c..aap     .					10000,10000,
c..aap     .					.true.,T,rev(iplat))
				call AddNewBox(
     .					index,itrack,
     .					i10000,i10000,
     .					i10000,i10000,
     .					.true.,T,rev(iplat))
			    else
				call AddNewBox(
     .					index,itrack,
     .					x(itrack-1),y(itrack-1),
     .					x(itrack),y(itrack),
     .					.false.,T,rev(iplat))
			    end if
			end if

		    end do

    		IF (DISP.EQ.NONE .AND. .NOT.CLIP) THEN
    			CALL ISTODHMS(T,METDAY,METHOUR,METMIN,METSEC)
    			IF (V(3,IPLAT).GE.0.0) THEN
    				ASC = 'A'
    			ELSE
    				ASC = 'D'
    			ENDIF
    			WRITE(UNITNONE,299)IPLAT,REV(IPLAT),ASC,
     .				METDAY,METHOUR,METMIN,METSEC,
     .				(SWLATD(ITRACK,IINST,IPLAT),
     .				SWLON(ITRACK,IINST,IPLAT),
     .					ITRACK=1,TRACKS(IP))
    			WRITE(*,298)IPLAT,REV(IPLAT),ASC,
     .				METDAY,METHOUR,METMIN,METSEC,
     .				(SWLATD(ITRACK,IINST,IPLAT),
     .				SWLON(ITRACK,IINST,IPLAT),
     .					ITRACK=1,TRACKS(IP))
298			FORMAT('+',' P',i2.2,'/R',I5.5,1x,A,I4,'/',I2.2,
     .				':',I2.2,':',F4.1,99(3X,F6.2,1X,F7.2))
299			FORMAT(' P',i2.2,'/R',I5.5,1x,A,I4,'/',I2.2,
     .				':',I2.2,':',F4.1,99(3X,F6.2,1X,F7.2))
    		ENDIF

c Check for line clipping

300		    continue

		end do

400		continue

	    end do

** Simul display

	    if(ShowSimul) then
		call SimulBox(AnySimul)
c begin commented section to suppress time tags for simultaneous hits
    		if(disp.eq.None) then
		if(AnySimul) then
		    call istodhms(t,metday,methour,metmin,metsec)
		    if(LastAnySimul) then
ctmp			write(*,420) metday,methour,metmin
			write(99,421),metday,methour,metmin
420			format('+ MET ',i3,'/',i2.2,':',i2.2)
421			format('  MET ',i3,'/',i2.2,':',i2.2)
		    else
ctmp			write(*,425) metday,methour,metmin
			write(99,426),metday,methour,metmin
425			format('+',t20,' MET ',i3,'/',i2.2,':',i2.2)
426			format('  MET ',i3,'/',i2.2,':',i2.2,'  On')
		    end if
		else
		    if(LastAnySimul) then
			write(99,429),metday,methour,metmin
429			format('  MET ',i3,'/',i2.2,':',i2.2,'  Off',/)
		    end if
		end if
		LastAnySimul = AnySimul
    		end if
c end commented section to suppress time tags for simultaneous hits
	    end if


	    IF(K.EQ.PPC) K = 0

	end do

999	continue

    	IF(DISP.EQ.NONE) then 
		CLOSE(UNITNONE)
		close(UnitAdd)
	end if

	end
