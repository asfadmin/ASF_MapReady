
*.
*   Simul
*
*	Simultaneous coverage display routines
*
*	Method:
*		Given:	simultaneity bias time ("bias")
*			time between ephemeris points ("dt")
*		Define: NumSimBox = bias/dt + 1
*
*		Store the imaging swath coordinates for the instruments
*		  considered after each time step (i.e. t = t + dt).
*		  The coordinates go onto a stack. (Routine AddNewBox.)
*
*		If the number of coordinates stored is greater than "NumSimBox"
*		  then forget the coordinates at the bottom of the stack.
*
*		For each instrument, after each time step we check if the
*		  most recent area imaged overlaps any of the area imaged
*		  by the other instrument within the bias time (as stored
*		  on the coordinate stack) and mark the overlapping area.
*		  (Routine SimulBox.)
*
*	08/03/89 09:13
*..

*.
*   InitSimul - initialize arrays
*
*	 6/ 8/93 10:28 djc
*..
	subroutine InitSimul(dt,bias,tr1,tr2)

	  character*100 SccsFileID
     -/'@(#)simul.for	5.1 98/01/08 APS/ASF\0'/
*     dt - step time
*     bias - simultaneity bias time
*     tr1,tr2 - number of tracks in instrument 1,2
	double precision dt,bias
	integer tr1,tr2

	integer iplat,itrack,ibox

	include 'eosinclude:simul.inc'

*     Set common variables
	NumSimTracks(1) = tr1
	NumSimTracks(2) = tr2

*     Add one to NumSimBox since we need one box for 0 bias
	NumSimBox = bias/dt + 1
	NumSimBox = max(NumSimBox,1)
	if(NumSimBox.gt.MAXSIMBOX) then
	    write(*,*) 'Bias time too large'
	    bias = dt * dble(MAXSIMBOX)
	    write(*,*) 'Let''s make the bias ',bias/60.0,' minutes'
	    NumSimBox = MAXSIMBOX
	end if
	StackSize = 2*(NumSimBox+1)

	do iplat=1,2
	    do itrack=1,2
		do ibox=1,MAXSIMBOX
		    whole(iplat,itrack,ibox) = .false.
		end do
	    end do
	end do

	BoxStack(1,1,1) = -15000
	BoxStack(1,1,2) = -15000
	BoxStack(1,2,1) = -15000
	BoxStack(1,2,2) = -15000
	BoxStack(1,3,1) = -15000
	BoxStack(1,3,2) = -15000
	BoxStack(1,4,1) = -15000
	BoxStack(1,4,2) = -15000
	BoxStack(2,1,1) = -15000
	BoxStack(2,1,2) = -15000
	BoxStack(2,2,1) = -15000
	BoxStack(2,2,2) = -15000
	BoxStack(2,3,1) = -15000
	BoxStack(2,3,2) = -15000
	BoxStack(2,4,1) = -15000
	BoxStack(2,4,2) = -15000

	end

*.
*   AddNewBox - pushes new swath coordinates onto stack
*
*	11/23/88 07:37
*..
	subroutine AddNewBox(iplat,itrack,x1,y1,x2,y2,KnowItsNotWhole,
	1	T,rev)

	include 'eosinclude:display.inc'
	include 'eosinclude:simul.inc'
	integer iplat,itrack
	integer x1,y1,x2,y2
	logical KnowItsNotWhole
c
	double precision T
	integer	rev
c

	integer i,toobigx,toobigy

*     itrack is assumed to be even

c	Shift each member of the stack down one

	do i=StackSize,3,-1
	    BoxStack(iplat,itrack-1,i) = BoxStack(iplat,itrack-1,i-2)
	    BoxStack(iplat,itrack,i) = BoxStack(iplat,itrack,i-2)
	end do

	do i=NumSimBox,2,-1
	    whole(iplat,itrack/2,i) = whole(iplat,itrack/2,i-1)
	    TimeStack(iplat,i) = TimeStack(iplat,i-1)
	    RevStack(iplat,i) = RevStack(iplat,i-1)
	end do

c	Add new box to top of stack

	BoxStack(iplat,itrack-1,1) = x1
	BoxStack(iplat,itrack-1,2) = y1
	BoxStack(iplat,itrack,1) = x2
	BoxStack(iplat,itrack,2) = y2

c	Add time and rev to top of stack

	TimeStack(iplat,1) = T
	RevStack(iplat,1) = rev

c	Check if new box is entirely visible

	toobigx = abs(3*swidex/4)
	toobigy = abs(swidey/2)

	if(KnowItsNotWhole) then
	    whole(iplat,itrack/2,1) = .false.
	else if(
     .	    abs(BoxStack(iplat,itrack-1,1)-BoxStack(iplat,itrack-1,3))
     .							.gt.toobigx .or.
     .	    abs(BoxStack(iplat,itrack-1,1)-BoxStack(iplat,itrack,1))
     .							.gt.toobigx .or.
     .	    abs(BoxStack(iplat,itrack,1)-BoxStack(iplat,itrack,3))
     .							.gt.toobigx .or.
     .	    abs(BoxStack(iplat,itrack,3)-BoxStack(iplat,itrack-1,3))
     .							.gt.toobigx .or.
     .	    abs(BoxStack(iplat,itrack-1,2)-BoxStack(iplat,itrack-1,4))
     .							.gt.toobigy .or.
     .	    abs(BoxStack(iplat,itrack-1,2)-BoxStack(iplat,itrack,2))
     .							.gt.toobigy .or.
     .	    abs(BoxStack(iplat,itrack,2)-BoxStack(iplat,itrack,4))
     .							.gt.toobigy .or.
     .	    abs(BoxStack(iplat,itrack,4)-BoxStack(iplat,itrack-1,4))
     .							.gt.toobigy
     .	  ) then

	    whole(iplat,itrack/2,1) = .false.
	else
	    whole(iplat,itrack/2,1) = .true.
	end if

	end

*.
*   SimulBox - marks overlap of latest box with all previous boxes
*
*	11/23/88 07:40
*..
	subroutine SimulBox(AnySimul)
	include 'eosinclude:display.inc'
	include 'eosinclude:simul.inc'
	logical AnySimul

	integer SIDES
	parameter(SIDES=4)
	integer iplatnew,iplatold,itracknew,itrackold
	integer iboxold,pt,pt1,pt2,pt3
	integer boxnewx(SIDES),boxnewy(SIDES)
	integer boxoldx(SIDES),boxoldy(SIDES)
	integer boxx(10*SIDES),boxy(10*SIDES),numpts
	logical intersect
c
	double precision TNew(2), TOld(2)
	integer RevNew(2), RevOld(2)
	integer MDOLD(2),MHOLD(2),MMOLD(2)
	integer MDNEW(2),MHNEW(2),MMNEW(2)
	double precision MSOLD(2),MSNEW(2)
	integer it
c

    	double precision LATDBOX,LONBOX
	double precision tlatdbox(10*SIDES),tlonbox(10*SIDES)
	double precision TX,TY
	integer ib
	integer IJK

	AnySimul = .false.

c	Find overlap of each new box with each old box

	do iplatnew=1,2
	    do itracknew=2,NumSimTracks(iplatnew),2
		if(whole(iplatnew,itracknew/2,1)) then

		    boxnewx(1) = BoxStack(iplatnew,itracknew-1,3)
		    boxnewy(1) = BoxStack(iplatnew,itracknew-1,4)
		    boxnewx(2) = BoxStack(iplatnew,itracknew-1,1)
		    boxnewy(2) = BoxStack(iplatnew,itracknew-1,2)
		    boxnewx(3) = BoxStack(iplatnew,itracknew,1)
		    boxnewy(3) = BoxStack(iplatnew,itracknew,2)
		    boxnewx(4) = BoxStack(iplatnew,itracknew,3)
		    boxnewy(4) = BoxStack(iplatnew,itracknew,4)

		    TNew(1) = TimeStack(iplatnew,2)
		    TNew(2) = TimeStack(iplatnew,1)
		    RevNew(1) = RevStack(iplatnew,2)
		    RevNew(2) = RevStack(iplatnew,1)

		    iplatold = 3 - iplatnew		! 1->2, 2->1

		    do itrackold=2,NumSimTracks(iplatold),2
			do iboxold=1,NumSimBox
			    if(whole(iplatold,itrackold/2,iboxold)) then

				pt = 2 * iboxold - 1
				pt1 = pt + 1
				pt2 = pt + 2
				pt3 = pt + 3

				boxoldx(1)
     .				    = BoxStack(iplatold,itrackold-1,pt2)
				boxoldy(1)
     .				    = BoxStack(iplatold,itrackold-1,pt3)
				boxoldx(2)
     .				    = BoxStack(iplatold,itrackold-1,pt)
				boxoldy(2)
     .				    = BoxStack(iplatold,itrackold-1,pt1)
				boxoldx(3)
     .				    = BoxStack(iplatold,itrackold,pt)
				boxoldy(3)
     .				    = BoxStack(iplatold,itrackold,pt1)
				boxoldx(4)
     .				    = BoxStack(iplatold,itrackold,pt2)
				boxoldy(4)
     .				    = BoxStack(iplatold,itrackold,pt3)

				if(iboxold .ne. NumSimBox)then
		    		   TOld(1) = TimeStack(iplatold,iboxold+1)
		    		   TOld(2) = TimeStack(iplatold,iboxold)
		    		   RevOld(1) = RevStack(iplatold,iboxold+1)
		    		   RevOld(2) = RevStack(iplatold,iboxold)
				else
		    		   TOld(2) = TimeStack(iplatold,iboxold)
		    		   TOld(1) = TOld(2)
		    		   RevOld(2) = RevStack(iplatold,iboxold)
		    		   RevOld(1) = RevOld(2)
				end if

				call PolySect(  boxnewx,boxnewy,
     .						boxoldx,boxoldy,
     .						boxx,boxy,
     .						intersect,numpts)

				if(intersect .and. numpts.ge.3) then
c Draw box
				    AnySimul = .true.
				    call boxni4(boxx,boxy,numpts)
	if(disp.eq.none)then
	DO IT=1,2
	    CALL ISTODHMS(TNEW(IT),MDNEW(IT),MHNEW(IT),MMNEW(IT),MSNEW(IT))
	    CALL ISTODHMS(TOLD(IT),MDOLD(IT),MHOLD(IT),MMOLD(IT),MSOLD(IT))
	END DO
	CALL UNPROJECT(BOXX(1),BOXY(1),LATDBOX,LONBOX)
	TX=LATDBOX
	TY=LONBOX
	IB=0
	DO IJK=2,NUMPTS
	    CALL UNPROJECT(BOXX(IJK),BOXY(IJK),LATDBOX,LONBOX)
	    IF(LATDBOX.NE.TX .AND. LONBOX.NE.TY) THEN
		IB=IB+1
	    	tlatdbox(ib)=TX
	    	tlonbox(ib)=TY
		TX=LATDBOX
		TY=LONBOX
		IF(IJK.EQ.NUMPTS)THEN
		    IB=IB+1
	    	    tlatdbox(ib)=TX
	    	    tlonbox(ib)=TY
		END IF
	    END IF
	END DO
	NUMPTS=IB
	IF(NUMPTS.GE.3)THEN
	WRITE(UNITADD,1000)IPLATOLD,REVOLD(1),(MDOLD(IT),MHOLD(IT),
	1	MMOLD(IT),MSOLD(IT),IT=1,2)
	WRITE(UNITADD,1000)IPLATNEW,REVNEW(1),(MDNEW(IT),MHNEW(IT),
	1	MMNEW(IT),MSNEW(IT),IT=1,2)
1000	FORMAT(1X,'P',I2.2,'/','R',I5.5,3X,I4,'/',2(I2.2,':'),
	1	F4.1,' - ',I4,'/',2(I2.2,':'),F4.1)
	WRITE(UNITADD,1002)NUMPTS
1002	FORMAT(I4)
1004	FORMAT(2F10.2)
	DO IJK=1,NUMPTS
	    WRITE(UNITADD,1004) tlatdbox(ijk),tlonbox(ijk)
	END DO
        END IF
	end if
				end if	    ! intersect
			    end if	! whole(old)
			end do	    ! iboxold
		    end do	! itrackold
		end if	    ! whole(new)
	    end do	! itracknew
	end do	    ! iplatnew

	end


*.
*   PolySect - find the intersection of two polygons
*
*	11/23/88 07:41
*..
	subroutine polysect(box1x,box1y,box2x,box2y,boxx,boxy,intersect,
     .						numintpoints)
	integer SIDES
	parameter(SIDES=4)
	integer box1x(SIDES),box1y(SIDES)	! coords for box 1
	integer box2x(SIDES),box2y(SIDES)	! coords for box2
	integer boxx(10*SIDES),boxy(10*SIDES)	! coords for intersection box
	logical intersect			! do box1 and box2 intersect?
	integer numintpoints

	integer numinpoints,numinpoints1,numinpoints2,numint
	integer side1,side2,nextside1,nextside2,i
	integer pointx(2),pointy(2)
	integer intpointx(10*SIDES),intpointy(10*SIDES)
	logical intersect2

	integer vertex

* Functions
	logical inbox


	numinpoints = 0
	numinpoints1 = 0
	numinpoints2 = 0
	numintpoints = 0

c Find interior points

	do vertex=1,SIDES
	    if(inbox(box2x(vertex),box2y(vertex),box1x,box1y)) then
		numinpoints1= numinpoints1 + 1
		intpointx(numinpoints1) = box2x(vertex)
		intpointy(numinpoints1) = box2y(vertex)
	    end if
	end do

	numinpoints = numinpoints1

	do vertex=1,SIDES
	    if(inbox(box1x(vertex),box1y(vertex),box2x,box2y)) then
		numinpoints2 = numinpoints2 + 1
		numinpoints = numinpoints + 1
		intpointx(numinpoints) = box1x(vertex)
		intpointy(numinpoints) = box1y(vertex)
	    end if
	end do

	if(numinpoints1.eq.SIDES) then		! box2 inside box1
	    do i=1,SIDES
		boxx(i) = box2x(i)
		boxy(i) = box2y(i)
	    end do
	    intersect = .true.
	    numintpoints = SIDES
	    return
	else if(numinpoints2.eq.SIDES) then	! box1 inside box2
	    do i=1,SIDES
		boxx(i) = box1x(i)
		boxy(i) = box1y(i)
	    end do
	    intersect = .true.
	    numintpoints = SIDES
	    return
	end if

	numintpoints = numinpoints

c Find side intersection points

	do side1=1,SIDES
	    nextside1 = side1 + 1
	    if(nextside1.gt.SIDES) nextside1 = 1
	    do side2=1,SIDES
		nextside2 = side2 + 1
		if(nextside2.gt.SIDES) nextside2 = 1
		call segsect(	box1x(side1),box1y(side1),
     .				box1x(nextside1),box1y(nextside1),
     .				box2x(side2),box2y(side2),
     .				box2x(nextside2),box2y(nextside2),
     .				pointx,pointy,numint,intersect2)
		if(intersect2) then
		    do i=1,numint
			numintpoints = numintpoints + 1
			intpointx(numintpoints) = pointx(i)
			intpointy(numintpoints) = pointy(i)
		    end do
		end if
	    end do
	end do

c Sort interior and side intersection points to produce solution polygon

	if(numintpoints.ne.0 .and. numintpoints.le.10*SIDES) then
	    intersect = .true.
	    call sortpoints(intpointx,intpointy,boxx,boxy,
     .						numintpoints)
	else
	    intersect = .false.
	end if

	end

***************************************************
*
*  SegSect
*
*	find the intersection of two line segments
*
***************************************************

*	9/30/85 14:30

	subroutine segsect(x1,y1,x2,y2,x3,y3,x4,y4,x,y,numints,intersect)
	integer x1,y1,x2,y2	! segment 1
	integer x3,y3,x4,y4	! segment 2
	integer x(2),y(2)	! intersection #1, #2
	integer numints	! number of intersections (0,1,or 2)
	logical intersect	! do the segments intersect?

	integer dx1,dy1,dx2,dy2
	double precision m1,m2,m,k1,k2,k,infinity/1.0e8/

	integer twixt	! function


	numints = 0

	dx1 = x2 - x1
	dy1 = y2 - y1
	dx2 = x4 - x3
	dy2 = y4 - y3

	if(dx1.ne.0) then
	    m1 = dble(dy1)/dble(dx1)
	    k1 = dble(y1) - m1 * dble(x1)
	else
	    m1 = infinity * sign(1,dy1)
	    k1 = x1
	end if
	if(dx2.ne.0) then
	    m2 = dble(dy2)/dble(dx2)
	    k2 = dble(y3) - m2 * dble(x3)
	else
	    m2 = infinity * sign(1,dy2)
	    k2 = x3
	end if

	if(dx1.eq.0 .and. dx2.eq.0)then		! two vertical lines (aap.10/29/93)
	    numints = 0
	else if(m1.ne.m2) then			! lines not parallel

	    if(dx1.ne.0 .and. dx2.ne.0) then	    ! lines not vertical
		if(abs(m1).lt.abs(m2)) then
		    m = m1
		    k = k1
		else
		    m = m2
		    k = k2
		end if
		x(1) = (k2-k1) / (m1-m2)
		y(1) = m * dble(x(1)) + k
	    else if(dx1.eq.0) then		    ! 1st line vertical
		x(1) = x1
		y(1) = m2 * dble(x(1)) + k2
	    else if(dx2.eq.0) then		    ! 2nd line vertical
		x(1) = x3
		y(1) = m1 * dble(x(1)) + k1
	    end if

	    if(twixt(x1,x(1),x2) .and. twixt(y1,y(1),y2) .and.
     .		twixt(x3,x(1),x4) .and. twixt(y3,y(1),y4)) then
		numints = 1
	    else
		numints = 0
	    end if

	else					! lines are parallel
	    if(abs(k1-k2).lt..05) then		    ! lines may overlap
		if(dx1.ne.0) then			! lines not vertical
		    if(twixt(x1,x3,x2)) then
		        numints = numints + 1
			x(numints) = x3
			y(numints) = m1 * dble(x(numints)) + k1
		        if(numints.eq.2) go to 999
		    end if

		    if(twixt(x1,x4,x2)) then
		        numints = numints + 1
		        x(numints) = x4
		        y(numints) = m1 * dble(x(numints)) + k1
		        if(numints.eq.2) go to 999
		    end if

		    if(twixt(x3,x1,x4)) then
		        numints = numints + 1
			x(numints) = x1
			y(numints) = m1 * dble(x(numints)) + k1
		        if(numints.eq.2) go to 999
		    end if

		    if(twixt(x3,x2,x4)) then
		        numints = numints + 1
			x(numints) = x2
			y(numints) = m1 * dble(x(numints)) + k1
		        if(numints.eq.2) go to 999
		    end if

		else					! lines vertical
		    if(twixt(y1,y3,y2)) then
		        numints = numints + 1
			x(numints) = x1
			y(numints) = y3
		        if(numints.eq.2) go to 999
		    end if

		    if(twixt(y1,y4,y2)) then
		        numints = numints + 1
		        x(numints) = x1
		        y(numints) = y4
		        if(numints.eq.2) go to 999
		    end if

		    if(twixt(y3,y1,y4)) then
		        numints = numints + 1
			x(numints) = x3
			y(numints) = y1
		        if(numints.eq.2) go to 999
		    end if

		    if(twixt(y3,y2,y4)) then
		        numints = numints + 1
			x(numints) = x3
			y(numints) = y2
		        if(numints.eq.2) go to 999
		    end if
		end if
	    else				    ! lines don't intersect
		numints = 0
	    end if

	end if
		
999	continue

	if(numints.gt.0) then
	    intersect = .true.
	else
	    intersect = .false.
	end if

	end

*.
*   InBox - checks if a point is in a box
*
*	10/2/85 10:20
*..
	logical function inbox(pointx,pointy,boxx,boxy)
	include 'eosinclude:display.inc'
	integer SIDES
	parameter(SIDES=4)
	integer pointx,pointy
	integer boxx(SIDES),boxy(SIDES)

	double precision az12,az1p,az14,az32,az3p,az34
	integer q12,q1p,q14,q32,q3p,q34,maxx,minx,maxy,miny
	logical err(6)

** Functions
	logical twixt,AzTwixt


	maxx = max(boxx(1),boxx(2),boxx(3),boxx(4))
	minx = min(boxx(1),boxx(2),boxx(3),boxx(4))
	maxy = max(boxy(1),boxy(2),boxy(3),boxy(4))
	miny = min(boxy(1),boxy(2),boxy(3),boxy(4))

	if(.not.twixt(minx,pointx,maxx) .or.
     .			.not.twixt(miny,pointy,maxy)) then
	    inbox = .false.
	    return
	end if

	if(
     .		(pointx.eq.boxx(1) .and. pointy.eq.boxy(1)) .or.
     .		(pointx.eq.boxx(2) .and. pointy.eq.boxy(2)) .or.
     .		(pointx.eq.boxx(3) .and. pointy.eq.boxy(3)) .or.
     .		(pointx.eq.boxx(4) .and. pointy.eq.boxy(4))
     .	  ) then
	    inbox = .true.	! point is one of the box vertices
	    return
	end if

	call azimuth(boxx(1),boxy(1),boxx(2),boxy(2),az12,q12,err(1))
	call azimuth(boxx(1),boxy(1),pointx,pointy,az1p,q1p,err(2))
	call azimuth(boxx(1),boxy(1),boxx(4),boxy(4),az14,q14,err(3))

	call azimuth(boxx(3),boxy(3),boxx(2),boxy(2),az32,q32,err(4))
	call azimuth(boxx(3),boxy(3),pointx,pointy,az3p,q3p,err(5))
	call azimuth(boxx(3),boxy(3),boxx(4),boxy(4),az34,q34,err(6))

	if(err(1).or.err(2).or.err(3).or.err(4).or.err(5).or.err(6)) then
	    inbox = .false.
	else if(AzTwixt(az12,az1p,az14,q12,q14) .and.
     .				AzTwixt(az32,az3p,az34,q32,q34)) then
	    inbox = .true.
	else
	    inbox = .false.
	end if

	end


	subroutine SortPoints(x,y,xs,ys,n)
	integer SIDES
	parameter(SIDES=4)
	integer x(*),y(*)	! unsorted point coords
	integer xs(*),ys(*)	! sorted point coords
	integer n		! number of points in x,y and xs,ys
	integer cpx,cpy		! center of polygon

	integer sumx,sumy,i,quadrant,index(10*SIDES)
	double precision az(10*SIDES)
	logical err

c Find center of polygon (cpx,cpy)

	sumx = 0
	sumy = 0

	do i=1,n
	    sumx = sumx + x(i)
	    sumy = sumy + y(i)
	end do

	cpx = sumx / n
	cpy = sumy / n

c Find azimuth az(i) from cpx,cpy to each point

	do i=1,n
	    call azimuth(cpx,cpy,x(i),y(i),az(i),quadrant,err)
	end do

c Sort points using az(i)

	call sort2d(az,index,n)

	do i=1,n
	    xs(i) = x(index(i))
	    ys(i) = y(index(i))
	end do

	end


	subroutine sort2d(x,index,n)
	double precision x(*)
	integer index(*),n

	double precision xmin,z
	integer i,iz,j,jmin

	integer k


	do k=1,n
	    index(k) = k
	end do

	do i=1,n-1

	    xmin = 1.0e8
	    do j=i,n
		xmin = min(x(j),xmin)
		if(xmin.eq.x(j)) jmin = j
	    end do

	    z = x(i)
	    x(i) = x(jmin)
	    x(jmin) = z

	    iz = index(i)
	    index(i) = index(jmin)
	    index(jmin) = iz

	end do

	end


	subroutine azimuth(x1,y1,x2,y2,az,quadrant,err)
	include 'eosinclude:display.inc'
	include 'eosinclude:constants.inc'
	integer x1,y1,x2,y2
	double precision az
	integer quadrant
	logical err

	double precision dx,dy

	dx = (x2 - x1) * sign(1,swidex)
	dy = -(y2 - y1) * sign(1,swidey)

	if(dx.eq.0 .and. dy.eq.0) then
	    err = .true.
	    return
	else
	    err = .false.
	end if

	az = atan2(dy,dx)
	if(az.lt.0.0) az = az + twopi

	if(dx.ge.0) then
	    if(dy.ge.0) then
		quadrant = 1
	    else
		quadrant = 4
	    end if
	else
	    if(dy.ge.0) then
		quadrant = 2
	    else
		quadrant = 3
	    end if
	end if

	end


	logical function AzTwixt(x1,x,x2,q1,q2)
	include 'eosinclude:display.inc'
	include 'eosinclude:constants.inc'
	double precision x1,x,x2
	integer q1,q2

	double precision minx,maxx

	if(
     .		(q1.eq.1 .and. q2.eq.4) .or.
     .		(q1.eq.4 .and. q2.eq.1)
     .	  ) then
	    minx = min(x1,x2)
	    maxx = max(x1,x2)

	    if((x.ge.0.0.and.x.le.minx).or.(x.ge.maxx.and.x.le.twopi)) then
		AzTwixt = .true.
	    else
		AzTwixt = .false.
	    end if
	else
	    if((x.ge.x1 .and. x.le.x2) .or. (x.le.x1 .and. x.ge.x2)) then
		AzTwixt = .true.
	    else
		AzTwixt = .false.
	    end if
	end if

	end
