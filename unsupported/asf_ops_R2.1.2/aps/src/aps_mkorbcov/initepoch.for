
*.
*   InitEpoch - set base year and gmt
*
*	08/02/89 16:35
*..
	subroutine InitEpoch(year,gmt)
	include 'eosinclude:time.inc'
	include 'eosinclude:constants.inc'
	integer year
	double precision gmt

	integer day,hour,min
	double precision sec

	  character*100 SccsFileID
     -/'@(#)initepoch.for	5.1 98/01/08 APS/ASF\0'/

	BaseYear = 0
	BaseGMT = 0.0

	if(year.eq.0 .and. gmt.eq.0.0) then

	    write(*,*)
10	    write(*,'(a,$)')
     .		' Enter base year and gmt (y,d,h,m,s) [1995,91,15,20,0.0]: '
	    read(*,'(4i10,f20.0)',err=10,end=999)
     .				BaseYear,day,hour,min,sec
	    if(BaseYear.eq.0 .and. BaseDay.eq.0) then
		BaseYear = 1995
c..aap		call idhmstos(91,15,20,0.0,BaseGMT)
		call idhmstos(i91,i15,i20,dzero,BaseGMT)
	    else
		call idhmstos(day,hour,min,sec,BaseGMT)
	    end if

c	else if(year.eq.-1) then
c
c	    call gmtnow(BaseYear,day,hour,min,sec)
c	    call dhmstos(day,hour,min,sec,BaseGMT)

	else

	    BaseYear = year
	    BaseGMT = gmt

	end if

	call SubSolar(BaseYear,BaseGMT,SunLatd,SunLon)

	call istodhms(BaseGMT,BaseDay,BaseHour,BaseMin,BaseSec)
	
c	write(*,998) BaseYear,int(day),int(hour),int(min),sec
c998	format(/,1x,'>>> Epoch ',i4,1x,i3.3,'/',i2.2,':',i2.2,':',f4.1)

999	continue
	end
