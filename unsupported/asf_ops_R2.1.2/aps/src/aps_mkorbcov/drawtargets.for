*.
*   DrawTargets - draw targets from a target file
*
*	04/20/89 08:44
*..
	subroutine DrawTargets

** Includes
	include 'eosinclude:cbody.inc'
	include 'eosinclude:display.inc'
	include 'eosinclude:map.inc'
	include 'eosinclude:constants.inc'	! aap..11/4/93
** Variables
* Parameters
	integer MaxPointsPerTarget
	parameter(MaxPointsPerTarget=1000)

	 character*100 SccsFileID
     -/'@(#)drawtargets.for	5.1 98/01/08 APS/ASF\0'/
* Inputs
	integer UnitInput
	character*80 FileInput
	double precision latd(MaxPointsPerTarget),lon(MaxPointsPerTarget)
	character*80 TargetName

c Program
	integer i,NumPoints			! # points defining a target
	integer col
	logical LineTarget,InitTargRead
	logical err,eof
	logical DrawName

	character*80 ShortName
	integer slen,numchars,height
	character YesNo

** Functions
	integer StringLen

** Data
	data UnitInput/41/

c Get targets

	do while(.true.)
    
	    write(*,*)
10	    write(*,'(a,$)') ' Enter target file name [quit]: '
	    read(*,'(a)',err=10,end=999) FileInput
	    if(FileInput(1:5).eq.'     ') go to 999
	    IF(FILEINPUT(1:1).EQ.'*') THEN
		LINETARGET = .TRUE.
		FILEINPUT = FILEINPUT(2:80) // ' '
	    ELSE
		LINETARGET = .FALSE.
	    END IF

c..aap	    open(unit=UnitInput,file=FileInput,status='old',readonly,err=10)
	    open(unit=UnitInput,file=FileInput,status='old',err=10)

	    write(*,'(/a,a/)') ' Opened ',FileInput
    
c..aap	    call GetCol(col,'for targets',0)
	    call GetCol(col,'for targets')
	    call UseCol(col)

	    write(*,*)
20	    write(*,'(a,$)') ' Plot target names [no]? '
	    read(*,'(a)',err=20,end=999) YesNo
	    DrawName = (YesNo.eq.'y' .or. YesNo.eq.'Y')
	    if(DrawName) then
		write(*,*)
30		write(*,'(a,$)') ' Enter text height (usual): '
		read(*,'(i10)',err=30,end=999) height
		if(height.le.0) then
		    if(disp.eq.POSTSCRIPT) then
			height = 125
		    else
		    end if
		end if
	    end if

	    call ClearTextPlane

	    slen = StringLen(FileInput)
	    call GrafComment('Targets from ' // FileInput(1:slen))
	    InitTargRead = .true.
	    eof = .false.

	    do while(.not.eof)

		call ReadTargets(UnitInput,latd,lon,NumPoints,TargetName,
     .						InitTargRead,err,eof)

c		call ParseName(TargetName,ShortName,numchars)

		if(.not.err .and. .not.eof) then
		    if(NumPoints.eq.1) then
			call TargetP(latd(1),lon(1))
		    else
			IF(.NOT.LINETARGET) THEN
			    call TargetA(latd,lon,NumPoints)
			ELSE
			    CALL TARGETL(LATD,LON,NUMPOINTS)
			END IF
		    end if

c		    call LatLonText(latd(1),lon(1),
c     .				ShortName(1:numchars),numchars,height)
		    if(DrawName) then
			numchars = StringLen(TargetName)
			call LatLonText(latd(1),lon(1),
     .			'  '//TargetName(1:numchars),numchars+2,height)
		    end if

		end if
    
	    end do
    
200	    continue
	    close(UnitInput)

    	end do

999	continue

	end


	subroutine ParseName(fullname,shortname,numchars)
	character*(*) fullname,shortname
	integer numchars

	integer slen,if,is
	logical instring
	character c

	slen = len(fullname)
	instring = .false.
	is = 0

	do if=1,slen
	    c = fullname(if:if)
	    if(instring) then
		if(c.ne.' ') then
		    is = is + 1
		    shortname(is:is) = c
		else
		    go to 999
		end if
	    else
		if(c.ne.' ') then
		    instring = .true.
		    is = is + 1
		    shortname(is:is) = c
		end if
	    end if
	end do

	is = 1
	shortname(1:1) = ' '

999	continue
	numchars = is
	end


	subroutine LatLonText(latd,lon,text,chars,height)
	include 'eosinclude:constants.inc'
	double precision latd,lon
	character*(*) text
	integer chars,height

	integer x2,y2
	integer x,y

	logical clip

	equivalence (x2,x),(y2,y)

c..aap	call project(latd,lon,0.0,x2,y2,clip)
	call project(latd,lon,dzero,x2,y2,clip)
c..aap	if(.not.clip) call DrawText(x,y,text,chars,height,0)
	if(.not.clip) call DrawText(x,y,text,chars,height,izero)

	end
