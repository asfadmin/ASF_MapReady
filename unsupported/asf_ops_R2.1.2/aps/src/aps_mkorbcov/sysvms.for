
*.
*   SysVMS - system-specific routines for VAX/VMS
*
*	CheckLogName - make sure data directory can be found
*	GetACharU - get a character with no echo - convert to uppercase
*	GetSysDateTime - returns two strings containing the date and time
*	TerminalSet - set term/nowrap (needed by SG220?!)
*	WaitAwhile - pause for a given time (in seconds)
*
*   Common variables
*	DataDir0 - logical name of data directory (files.inc)
*
*	03/13/89 10:34
*..

	subroutine CheckLogName

	  character*100 SccsFileID
     -/'@(#)sysvms.for	5.1 98/01/08 APS/ASF\0'/
	end


	subroutine GetACharU(c,new)

	character c
	logical new

	END


	subroutine GetSysDateTime(cdate,ctime,lendate,lentime)
	character*(*) cdate,ctime
	integer lendate,lentime

	call date(cdate)
	call time(ctime)

	lendate = 9
	lentime = 8

	end


	subroutine TerminalSet

	end


	subroutine WaitAwhile(seconds)

	real seconds

	end
