*.
*   FlyIt - fly
*
*   Args
*	dt		double	input	time step (sec)
*
*   History
*	 3/26/91 If using station mask, exit loop if platform is in at least one
*	12/11/89 Fix decision on when platform is too early/late
*	 5/24/89 Fix station masking for instrus that don't care about it
*	 3/31/89
*
*   Last modified
*	3/26/91 12:08
*..
	subroutine FlyIt(filename,dt)
	character*80 filename
	double precision dt

	  character*100 SccsFileID
     -/'@(#)flyit.for	5.1 98/01/08 APS/ASF\0'/
** Includes
	include 'eosinclude:instruments.inc'
	include 'eosinclude:orbit.inc'
	include 'eosinclude:time.inc'
	include 'eosinclude:constants.inc'
	include 'eosinclude:map.inc'
	include 'eosinclude:display.inc'
	include 'eosinclude:station.inc'
	include 'eosinclude:pen.inc'

** Variables

* Graphics
	integer XYDIM
*     Make sure XYDIM is an even number
	parameter(XYDIM=1000)
	integer xy(XYDIM,MAXTRACK,MAXINSTPERPLAT,MAXPLAT)
	integer x(MAXTRACK),y(MAXTRACK)
	integer j(MAXPLAT,MAXINSTPERPLAT),jii
	integer iplat,iinst,itrack,itype

* Orbit
	double precision latd(MAXPLAT),lon(MAXPLAT),altd(MAXPLAT)
	double precision lastlon(MAXPLAT)
	double precision r(3,MAXPLAT),v(3,MAXPLAT)

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
	logical ClipDate,ClipDay,ClipNite,ClipMask,ClipAsc,ClipDsc

	integer istat
	logical inMasks

c	double precision tBias

c	logical ShowMET
	integer metday,methour,metmin
	double precision metsec

	logical err
	logical swerr,firstlookinst

	logical ShowTimeTicks,TickOnly
	double precision TickScale

	INTEGER REV
	double precision LASTZ
	CHARACTER ASC
c	INTEGER DMLAT(MAXTRACK),DMLATMIN(MAXTRACK)
c	INTEGER DMLON(MAXTRACK),DMLONMIN(MAXTRACK)

    	INTEGER IXT,IYT,LENT
    	CHARACTER*10 REVT
** Functions
	double precision FindBX
	logical daytime,dTwixt
	logical InMaskR
    	INTEGER STRINGLEN

	DISP = NONE
	IF(DISP.EQ.NONE) OPEN(UNIT=UNITNONE,FILE=filename,
     .				FORM='FORMATTED')

* Set plotting variables

	do iplat=1,NumPlat
	    do iinst=1,NumInstPlat(iplat)
		j(iplat,iinst) = -1
	    end do
	end do

	if(NumPlat.gt.1) then
c	    write(*,*)
c210	    write(*,'(a,$)') ' Speedy plotting [yes]? '
c	    read(*,'(a)',err=210,end=999) YesNo
c	    if(YesNo.eq.'N' .or. YesNo.eq.'n') then
		k = 0
c	    else
c		k = -1
c	    end if
	else
	    k = -1
	end if

c	Set ppc: ppc is the number of clicks to plot at a time
c	  e.g. if ppc is 5 we will draw the swath for 5 time intervals (dt)
c	  each time we draw

	ppc = 3 + 1

	ShowTimeTicks = .true.
	TickOnly = .false.
c	TickOnly = (YesNo.eq.'T' .or. YesNo.eq.'t')
c	if(TickOnly) then
c247	    write(*,'(a,$)') '   Enter tick scaling [0.2]: '
c	    read(*,'(g20.0)',err=247,end=999) TickScale
c	    if(TickScale.le.0.0) TickScale = 0.2
c	end if
	TickScale = 0.2d0

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
c	end if

c	if(ShowMET) write(*,'(//)') 

	call GrafComment('Fly')

	tmin = 1.0d20
	tmax = -tmin
	do iplat=1,NumPlat
	    if(active(iplat)) then
		tmin = min(tmin,tstart(iplat))
		tmax = max(tmax,tend(iplat))
	    end if
	end do

	REV = 1
	LASTZ = 0.0

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

**** Begin PPC ****
	    if(k.eq.ppc) then

		k = 0

		do iplat=1,NumPlat

		    do iinst=1,NumInstPlat(iplat)

			jii = j(iplat,iinst)

			if(jii.ne.-1) then

			    ip = platform(iplat,iinst)
			    itype = InstType(ip)

			    call UseCol(color(iplat,iinst))

			    do itrack=1,tracks(ip)

d	write(*,*) 'PPC - vector'
				call vector(xy(1,itrack,iinst,iplat),
     .							(jii+1)/2)

				if(ShowTimeTicks) then
				    if((itype.eq.BEAM .or.
     .					itype.eq.LIMB2) .and.
     .							itrack.eq.2) then
d	write(*,*) 'fill'
					call fillit(xy(1,1,iinst,iplat),
     .						xy(1,2,iinst,iplat),
     .						(jii+1)/2)

				    else if(itype.eq.SCAT .and.
     .					(itrack.eq.2 .or. itrack.eq.4)) then
					call fillit(xy(1,itrack-1,iinst,iplat),
     .						xy(1,itrack,iinst,iplat),
     .						(jii+1)/2)
				    end if
				end if

				xy(1,itrack,iinst,iplat) = 
     .					xy(jii,itrack,iinst,iplat)
				xy(2,itrack,iinst,iplat) = 
     .					xy(jii+1,itrack,iinst,iplat)

			    end do

			    j(iplat,iinst) = 1

			end if

		    end do

		end do

	    end if
**** End PPC ****

	    do iplat=1,NumPlat

c Too early/late for a platform to be flying

c		if(dTwixt(t,tstart(iplat),tend(iplat))) then
c If you want to use a negative time step then this needs to check for it, too
		if(t.lt.tstart(iplat)) then

*		  Too early - next platform
		    go to 400

c		else if(dTwixt(tstart(iplat),tend(iplat),t)) then
c If you want to use a negative time step then this needs to check for it, too
		else if(t.gt.tend(iplat)) then

*		  Too late - plot remainder
		    do iinst=1,NumInstPlat(iplat)

			jii = j(iplat,iinst)

			if(jii.ne.-1) then

			    ip = platform(iplat,iinst)
			    itype = InstType(ip)

			    call UseCol(color(iplat,iinst))

			    do itrack=1,tracks(ip)

d	write(*,*) 'Early/Late - vector'
				call vector(xy(1,itrack,iinst,iplat),
     .						(jii+1)/2)

				if(ShowTimeTicks) then
				    if((itype.eq.BEAM .or.
     .					itype.eq.LIMB2) .and.
     .							itrack.eq.2) then
d	write(*,*) 'fill'
					call fillit(xy(1,1,iinst,iplat),
     .						xy(1,2,iinst,iplat),
     .						(jii+1)/2)

				    else if(itype.eq.SCAT .and.
     .					(itrack.eq.2 .or. itrack.eq.4)) then
					call fillit(xy(1,itrack-1,iinst,iplat),
     .						xy(1,itrack,iinst,iplat),
     .						(jii+1)/2)
				    end if
				end if
			    end do

			    j(iplat,iinst) = -1

			end if

		    end do

		    go to 400

		end if

		lastlon(iplat) = lon(iplat)

		call ephem2(a(iplat),e(iplat),i(iplat),node0(iplat),
     .				omega0(iplat),m0(iplat),t,t0(iplat),
     .				latd(iplat),lon(iplat),altd(iplat),
     .				r(1,iplat),v(1,iplat))

		IF(IPLAT.EQ.1 .AND. R(3,1).GE.0.0 .AND. LASTZ.LT.0.0) THEN
		    REV = REV + 1
		END IF
		LASTZ = R(3,1)

		if(NumStations.lt.1) then
		    ClipMask = .false.
		else
		    inMasks = .false.
		    do istat=1,NumStations
			if(StationSelected(istat)) then
			    inMasks = InMaskR(r(1,iplat),istat)
			    if(inMasks) goto 100
			end if
		    end do
100		    continue
		    ClipMask = .not. inMasks
		end if

		do iinst=1,NumInstPlat(iplat)

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

		    if(itype.ne.SCANR .and. itype.ne.SCANY) then
			LookAng = LookPoint(iplat,iinst)
			YawAng  = YawPoint(iplat,iinst)
		    else
			if(itype.eq.SCANR) then
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

		    jii = j(iplat,iinst)

		    if(swerr) then
			clip = .true.
			go to 300
		    else
			clip = .false.
		    end if

		    clip = .false.

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
			    ClipDay = .false.
			else
			    ClipDay = .not.
     .				DayTime(swlatd(itrack,iinst,iplat),
     .					swlon(itrack,iinst,iplat),
     .					halfpi-SunElevMin(ip))
			end if

			if(.not.mask(MASKNITET,ip)) then
			    ClipNite = .false.
			else
			    ClipNite = 
     .				DayTime(swlatd(itrack,iinst,iplat),
     .					swlon(itrack,iinst,iplat),
     .					halfpi + SunElevMin(ip))
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
     .				.or. ClipDate
     .				.or. ClipDay .or. ClipNite
     .				.or. (ClipMask .and. mask(MASKSTN,ip))
     .				.or. ClipAsc .or. ClipDsc
		    end do

		    IF(DISP.EQ.NONE .AND. .NOT.CLIP) THEN
			CALL ISTODHMS(T,METDAY,METHOUR,METMIN,METSEC)
			IF(V(3,IPLAT).GE.0.0) THEN
			    ASC = 'A'
			ELSE
			    ASC = 'D'
			END IF
			WRITE(UNITNONE,299)
     .				METDAY,METHOUR,METMIN,METSEC,REV,ASC,
     .				(SWLATD(ITRACK,IINST,IPLAT),
     .				 SWLON(ITRACK,IINST,IPLAT),
     .					ITRACK=1,TRACKS(IP))
299			FORMAT(I4,'/',I2.2,':',I2.2,':',F4.1,I5,1X,A,
     .				99(3X,F6.2,1X,F7.2))
			GO TO 500
		    END IF

* Check for line clipping

300		    continue

		    if(TickOnly .or. itype.eq.SCANS) then
			if(.not.clip) then
			    do itrack=1,tracks(ip)
				call CrossScale(x(itrack),y(itrack),TickScale)
			    end do
			end if

* Clipped by display, map, or masks - draw what we've accumulated and start over

		    else if(clip) then

			if(jii.ne.-1) then

			    call UseCol(color(iplat,iinst))

			    do itrack=1,tracks(ip)

d	write(*,*) 'Clip - vector'
			        call vector(xy(1,itrack,iinst,iplat),
     .						(jii+1)/2)

				if(ShowTimeTicks) then
				    if((itype.eq.BEAM .or.
     .					itype.eq.LIMB2) .and.
     .							itrack.eq.2) then
d	write(*,*) 'fill'
					call fillit(xy(1,1,iinst,iplat),
     .						xy(1,2,iinst,iplat),
     .						(jii+1)/2)

				    else if(itype.eq.SCAT .and.
     .					(itrack.eq.2 .or. itrack.eq.4)) then
					call fillit(
     .					    xy(1,itrack-1,iinst,iplat),
     .					    xy(1,itrack,iinst,iplat),
     .					    (jii+1)/2)
				    end if
				end if

			    end do

			    j(iplat,iinst) = -1

			end if

* Good point - add it to previous vector

		    else

			j(iplat,iinst) = j(iplat,iinst) + 2
			jii = j(iplat,iinst)

			do itrack=1,tracks(ip)
			    xy(jii,itrack,iinst,iplat) = x(itrack)
			    xy(jii+1,itrack,iinst,iplat) = y(itrack)
			end do

C statements to write the rev# in the ps file at the beginning of the pass
    		IF (DISP.EQ.POSTSCRIPT) THEN
    			IF(JII.EQ.1.AND.IINST.EQ.1)THEN
    				IXT=(XY(1,1,1,IPLAT)+XY(1,2,1,IPLAT))/2
                                IYT=(XY(2,1,1,IPLAT)+XY(2,2,1,IPLAT))/2
    			WRITE (REVT,'(I4)') REV
    			CALL NOSPACES (REVT)
    			REVT=REVT// ASC
    			LENT=STRINGLEN(REVT)
c..aap			CALL DRAWTEXT (IXT,IYT,REVT,LENT,125,0)
    			CALL DRAWTEXT (IXT,IYT,REVT,LENT,i125,izero)
    			ENDIF
    		ENDIF
C end rev# statements - see corresponding declarations

* Check plot array - if full then draw what we've accumulated, keep last point

			if(jii.eq.XYDIM-1) then

			    call UseCol(color(iplat,iinst))

			    do itrack=1,tracks(ip)

d	write(*,*) 'Array full - vector'

			        call vector(xy(1,itrack,iinst,iplat),
     .						(jii+1)/2)

				if(ShowTimeTicks) then
				    if((itype.eq.BEAM .or.
     .					itype.eq.LIMB2) .and.
     .							itrack.eq.2) then
d	write(*,*) 'fill'
					call fillit(xy(1,1,iinst,iplat),
     .						xy(1,2,iinst,iplat),
     .						(jii+1)/2)

				    else if(itype.eq.SCAT .and.
     .					(itrack.eq.2 .or. itrack.eq.4)) then
					call fillit(xy(1,itrack-1,iinst,iplat),
     .						xy(1,itrack,iinst,iplat),
     .						(jii+1)/2)
				    end if
				end if

c				- Save this point as 1st point of next vector -

				xy(1,itrack,iinst,iplat) =
     .					xy(jii,itrack,iinst,iplat)
				xy(2,itrack,iinst,iplat) =
     .					xy(jii+1,itrack,iinst,iplat)

			    end do

			    j(iplat,iinst) = 1

			end if

		    end if

		end do

400		continue

	    end do

500	    continue

	    IF(K.EQ.PPC) K = 0

	end do

* Plot leftovers - time loop is done, draw what hasn't been drawn yet

	do iplat=1,NumPlat

	    do iinst=1,NumInstPlat(iplat)

		jii = j(iplat,iinst)

		if(jii.ne.-1) then

		    ip = platform(iplat,iinst)
		    itype = InstType(ip)

		    call UseCol(color(iplat,iinst))

		    do itrack=1,tracks(ip)

d	write(*,*) 'Leftovers - vector'

			call vector(xy(1,itrack,iinst,iplat),
     .					(jii+1)/2)

			if(ShowTimeTicks) then
			    if((itype.eq.BEAM .or.
     .				itype.eq.LIMB2) .and.
     .						itrack.eq.2) then
d	write(*,*) 'fill'
				call fillit(xy(1,1,iinst,iplat),
     .					xy(1,2,iinst,iplat),
     .					(jii+1)/2)

			    else if(itype.eq.SCAT .and.
     .				(itrack.eq.2 .or. itrack.eq.4)) then
				call fillit(xy(1,itrack-1,iinst,iplat),
     .					xy(1,itrack,iinst,iplat),
     .					(jii+1)/2)
			    end if
			end if

			xy(1,itrack,iinst,iplat) = 
     .				xy(jii,itrack,iinst,iplat)
			xy(2,itrack,iinst,iplat) = 
     .				xy(jii+1,itrack,iinst,iplat)

		    end do

		    j(iplat,iinst) = 1

		end if

	    end do

	end do

999	continue

	IF(DISP.EQ.NONE) CLOSE(UNITNONE)

	end
