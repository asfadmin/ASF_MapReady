
*.
*   Time - time conversions
*
*	06/29/88 8:27
*..

*.  dhmstos - converts day,hour,min,sec to sec

	subroutine dhmstos(day,hour,minu,sec,outputsec)
	double precision day,hour,minu,sec,outputsec

	  character*100 SccsFileID
     -/'@(#)time.for	5.1 98/01/08 APS/ASF\0'/
	outputsec = 86400.0d0*abs(day) + 3600.0d0*abs(hour)
     .			+ 60.0d0*abs(minu) + abs(sec)
	if(day.lt.0.0d0) outputsec = -outputsec

	end

*.  stodhms - converts sec to day,hour,min,sec

	subroutine stodhms(inputsec,day,hour,minu,sec)
	double precision inputsec,day,hour,minu,sec

	double precision worksec

	worksec = abs(inputsec)
	day = int(worksec/86400.0d0)
	if(inputsec.lt.0.0d0) day = -day
	worksec = worksec - 86400.0d0*day
	hour = int(worksec/3600.0d0)
	worksec = worksec - 3600.0d0*hour
	minu = int(worksec/60.0d0)
	sec = worksec - 60.0d0*minu

	end

*.  idhmstos - converts day,hour,minu,sec to sec

	subroutine idhmstos(day,hour,minu,sec,outputsec)
	integer day,hour,minu
	double precision sec,outputsec

	outputsec = 86400.0d0*day + 3600.0d0*hour + 60.0d0*minu + sec

	end

*.  istodhms - converts sec to day,hour,minu,sec

	subroutine istodhms(inputsec,day,hour,minu,sec)
	double precision inputsec,sec
	integer day,hour,minu

	double precision worksec

	worksec = abs(inputsec)
	day = int(worksec/86400.0d0)
	if(inputsec.lt.0.0d0) day = -day
	worksec = worksec - 86400.0d0*day
	hour = int(worksec/3600.0d0)
	worksec = worksec - 3600.0d0*hour
	minu = int(worksec/60.0d0)
	sec = worksec - 60.0d0*minu

	end
