*.
*   Drawmap - read in vector map file and plot map
*
*   Args
*	MapFile		C**	input	filename containing map data
*	penup		double	input	longitude value indicating a penup
*	endfile		double	input	longitude value indicating end-of-file
*	err		logical	output	error flag
*
*   Notes
*	When penup = endfile MapFile is a MapMaker(R) lon/lat file
*	MapMaker penups are when a lat/lon pair is the same as the very first
*	  pair for an object
*
*	08/03/89 08:19
*..
	subroutine drawmap(MapFile,penup,endfile,err)
	include 'eosinclude:display.inc'
	include 'eosinclude:map.inc'
	include 'eosinclude:constants.inc'
	character*(*) MapFile
	double precision penup,endfile
	logical err

	 character*100 SccsFileID
     -/'@(#)drawmap.for	5.1 98/01/08 APS/ASF\0'/
	integer UnitMap
	integer j
	double precision inlatd(10),inlon(10)
	integer i,MAXI
	parameter(MAXI=1000)
	integer xy(MAXI),x,y,xc,yc
	logical clip,ClipDate,CheckLon
	double precision lastlon

	character*80 TabLine,TabFields(3),MMname1,MMname2
	integer iCoord,NumCoord,slen
	double precision FirstLatd,FirstLon,tol
	logical IsPenUp

	integer MapType,UNX,MAPMAKER
	parameter(UNX=1,MAPMAKER=2)

	logical debug

** Functions
	integer StringLen

	call GrafComment('Map')

	ClipDate = .false.
	CheckLon = .true.
	lastlon = 0.0
	err = .false.

	if(proj.eq.GLOBE .or. ((proj.eq.NPOLE .or. proj.eq.SPOLE)
     .		.and. abs(lolat).lt.0.1)) then
c	    write(*,*)
c5	    write(*,'(a,$)') ' Draw globe outline [yes]? '
c	    read(*,'(a)',err=5,end=999) yesno
c	    if(yesno.ne.'N' .and. yesno.ne.'n') then
c		call cleart
		xc = x0
		yc = y0
c..aap		call arc(xc,yc,winr*zoom,0.0,365.0)
		call arc(xc,yc,winr*zoom,dzero,d365)
c	    end if
	end if

	UnitMap = 10
	open(unit=UnitMap,file=MapFile,status='old',form='formatted',
     .		err=999)
c..aap     .		readonly,err=999)

	if(abs(penup-endfile).le.1.0d-4) then
	    MapType = MAPMAKER
	else
	    MapType = UNX
	end if

	i = -1

	if(MapType.eq.UNX) then

	    do while(.true.)
10		read(UnitMap,'(10f8.0)',end=1000)
     .				(inlon(j),inlatd(j),j=1,5)
    
		do j=1,5
		    if(abs(inlatd(j)-penup).lt.1.0d-4) then
			if(i.ne.-1) call vector(xy,(i+1)/2)
			i = -1
			CheckLon = .false.
		    else if(inlatd(j).eq.0.0 .and. inlon(j).eq.0.0) then
			go to 20
		    else if(abs(inlatd(j)-endfile).lt.1.0d-4) then
			go to 1000
		    else
			call project(inlatd(j),inlon(j),0.0,x,y,clip)
			if(MapWrap .and. CheckLon) then
			    call DateLine(lastlon,inlon(j),ClipDate)
			end if
			if(.not.(clip.or.ClipDate)) then
			    i = i + 2
			    xy(i) = x
			    xy(i+1) = y
			    if(i.eq.MAXI-1) then
				call vector(xy,(i+1)/2)
				xy(1) = x
				xy(2) = y
				i = 1
			    end if
			else
			    if(i.ne.-1) call vector(xy,(i+1)/2)
			    i = -1
			    if(ClipDate) then
				i = 1
				xy(1) = x
				xy(2) = y
			    end if
			end if
			lastlon = inlon(j)
			CheckLon = .true.
		    end if
20		    continue
		end do
	    end do

	else if(MapType.eq.MAPMAKER) then

	    tol = 1.0e-4

	    do while(.true.)
		read(UnitMap,'(a)',err=1000,end=1000) TabLine
c..aap		call ParseTabs(TabLine,TabFields,3)
		call ParseTabs(TabLine,TabFields,ithree)
		MMname1 = TabFields(1)
		MMname2 = TabFields(2)
		call NoSpaces(TabFields(3))
		slen = StringLen(TabFields(3))
		decode(slen,'(i)',TabFields(3)(1:slen)) NumCoord
c	type *,MMname1(1:StringLen(MMname1)),':',MMname2(1:StringLen(MMname2)),
c     .		NumCoord

	debug = (MMname1(1:9) .eq. 'Australia')
	if(debug) write(16,*) MMname1(1:9)

		do iCoord=1,NumCoord
		    read(UnitMap,'(a)',err=1000,end=1000) TabLine
c..aap		    call ParseTabs(TabLine,TabFields,2)
		    call ParseTabs(TabLine,TabFields,itwo)
		    decode(StringLen(TabFields(1)),'(f20.0)',TabFields(1))
     .			inlon(1)
		    decode(StringLen(TabFields(2)),'(f20.0)',TabFields(2))
     .			inlatd(1)

		    if(iCoord.eq.1) then
			FirstLatd = inlatd(1)
			FirstLon = inlon(1)
			IsPenUp = .false.
		    else
			IsPenUp = ((abs(inlatd(1)-FirstLatd).le.tol)
     .				.and. (abs(inlon(1)-FirstLon).le.tol)
     .				.and. (iCoord.ne.NumCoord))
		    end if

	if(debug) write(16,*) inlatd(1),inlon(1),IsPenUp

		    if(IsPenUp) then
			if(i.ne.-1) call vector(xy,(i+1)/2)
			i = -1
		    else
c..aap			call project(inlatd(1),inlon(1),0.0,x,y,clip)
			call project(inlatd(1),inlon(1),dzero,x,y,clip)
			if(MapWrap .and. CheckLon) then
			    call DateLine(lastlon,inlon(1),ClipDate)
			end if
			if(.not.(clip.or.ClipDate)) then
			    i = i + 2
			    xy(i) = x
			    xy(i+1) = y
			    if(i.eq.MAXI-1) then
				call vector(xy,(i+1)/2)
				xy(1) = x
				xy(2) = y
				i = 1
			    end if
			else
			    if(i.ne.-1) call vector(xy,(i+1)/2)
			    i = -1
			    if(ClipDate) then
				i = 1
				xy(1) = x
				xy(2) = y
			    end if
			end if
			lastlon = inlon(1)
			CheckLon = .true.
		    end if
		end do
		if(i.ne.-1) call vector(xy,(i+1)/2)
		i = -1
	    end do

	end if

	go to 1000

999	continue
	err = .true.
	call ErrorMessage('DrawMap - cannot open MapFile')

1000	continue
	if(i.ne.-1) call vector(xy,(i+1)/2)

	close(UnitMap)

	end
