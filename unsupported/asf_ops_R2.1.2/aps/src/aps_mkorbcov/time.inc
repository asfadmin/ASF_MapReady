*.
*   Time.inc - Time variables
*
*	10/05/88 12:12
*..
	integer BaseYear,BaseDay,BaseHour,BaseMin
	double precision BaseSec
*     BaseGMT in sec
	double precision BaseGMT
*     Sun location in deg
	double precision SunLatd,SunLon

* Commons

	common /eos$time/ BaseYear,BaseDay,BaseHour,BaseMin,BaseSec,
     .				BaseGMT,SunLatd,SunLon
