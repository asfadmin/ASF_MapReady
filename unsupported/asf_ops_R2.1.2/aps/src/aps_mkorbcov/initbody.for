*.
*   InitBody - initializes central body variables
*
*   Args
*	body		integer	input	central body index - if 0 prompt
*					  for body with earth as default,
*					  if negative prompt with abs(body)
*					  as default
*	10/05/88 12:02
*..
	subroutine InitBody(body)
	include 'eosinclude:cbody.inc'
	integer body

	integer DefaultBody
	character*20 DefaultBodyName
	integer slen,StringLen
	logical AskBody

	  character*100 SccsFileID
     -/'@(#)initbody.for	5.1 98/01/08 APS/ASF\0'/
	character input

	integer inbody
*	since body is often a parameter:

	inbody = body

* if body<0 then use |body| as the default

	AskBody = .false.

	if(inbody.lt.0) then
	    AskBody = .true.
	    inbody = -inbody
	    if(inbody.ge.1 .and. inbody.le.BODIES) then
		DefaultBody = inbody
	    else
		DefaultBody = EARTH
	    end if
	else if(inbody.eq.0 .or. inbody.gt.BODIES) then
	    AskBody = .true.
	    DefaultBody = EARTH
	end if

	if(AskBody) then

	    DefaultBodyName = NameBodies(DefaultBody)
	    slen = StringLen(DefaultBodyName)

	    write(*,*)
10	    write(*,'(a)') ' Choose central body -'
	    write(*,'(a,a,a,a,$)') '   Earth (E), Moon (L), Mars (M),',
     .		' Venus (V) [',	DefaultBodyName(1:slen),']: '

	    read(*,'(a)',err=10,end=999) input

	    if(input.eq.'E' .or. input.eq.'e') then
		CentralBody = EARTH
	    else if(input.eq.'L' .or. input.eq.'l') then
		CentralBody = MOON
	    else if(input.eq.'M' .or. input.eq.'m') then
		CentralBody = MARS
	    else if(input.eq.'V' .or. input.eq.'v') then
		CentralBody = VENUS
	    else
		CentralBody = DefaultBody
	    end if

	else
	    CentralBody = inbody
	end if

	RBody      = RBodies(CentralBody)
	mu         = mus(CentralBody)
	flat       = flats(CentralBody)
	RotRate    = RotRates(CentralBody)
	j2         = j2s(CentralBody)
	j3         = j3s(CentralBody)
	AxisTilt   = AxisTilts(CentralBody)
	SecPerYear = SecsPerYear(CentralBody)
	NameBody   = NameBodies(CentralBody)

999	continue
	end
