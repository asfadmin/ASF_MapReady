
*.
*   LogToNum - convert TRUE/FALSE into 1/0
*
*   Args
*	ilog			logical	input	logical variable to test
*	LogToNum		integer	return
*
*	03/18/89 23:08
*..
	integer function LogToNum(ilog)
	logical ilog

	  character*100 SccsFileID
     -/'@(#)logtonum.for	5.1 98/01/08 APS/ASF\0'/
	if(ilog) then
	    LogToNum = 1
	else
	    LogToNum = 0
	end if

	end
