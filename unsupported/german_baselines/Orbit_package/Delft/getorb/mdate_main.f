      program mdate_main

* Convert almost any type of date into others.
* Examples of the input value:
* YYMMDD.DDD       = year-month-day and fractions of day
* MJD.DDD          = Modified Julian Day and fractions of day
* YYDDD.DDD        = Year, day of the year and fractions of day
* YYMMDDHHMMSS.SSS = year-month-day hour-minute-seconds and factions of
*                    seconds
* SEC85.SSS        = UTC seconds since 1985 and fraction of seconds
*
* Requires:
* mdate, sec85, chrdat
*-
*   Jan-1996 - Created by Remko Scharroo
*-----------------------------------------------------------------------
      implicit none
      character arg*80,cdate*15
      real*8    date,sec,day,sec85,mjd85,mjd,ddd
      integer   yy,mdate
      parameter (day=86400d0,mjd85=46066d0)

      call getarg(1,arg)
      if (arg.eq.' ' .or. arg(1:1).eq.'-') then
	 write (0,610)
	 goto 9999
      endif
      read (arg,*) date
      sec=sec85(0,date)
      mjd=sec/day+mjd85
      call chrdat(nint(sec),cdate)
      yy=mdate(1,int(mjd))/10000
      ddd=mjd-mdate(2,yy*10000+0101)+1
      write (6,600) sec,mjd,yy*1000+ddd,cdate

600   format (
     |'          SEC85 =',f14.3/
     |'            MJD =',f13.6/
     |'          YYDDD =',f13.6/
     |'YYMMDD HH:MM:SS = ',a)
610   format ('mdate: convert almost any date format to others'//
     |'syntax: mdate <date>'//
     |'where <date> can be of the form:'/
     |' YYMMDD.DDD       = year-month-day and fractions of day'/
     |' YYDDD.DDD        = Year, day of the year and fractions of day'/
     |' MJD.DDD          = Modified Julian Day and fractions of day'/
     |' YYMMDDHHMMSS.SSS = year-month-day hour-minute-seconds and',
     |' factions of seconds'/
     |' SEC85.SSS        = UTC seconds since 1985 and fraction of',
     |' seconds')
9999  end
