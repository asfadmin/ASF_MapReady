
*.
*   ToMill - convert latitude to its Miller equivalent
*
*   Args
*	latd			double	input	geodetic latitude (deg)
*	ToMill			double	return
*
*	06/16/88 16:50
*..
	double precision function ToMill(latd)
	double precision latd

	  character*100 SccsFileID
     -/'@(#)tomill.for	5.1 98/01/08 APS/ASF\0'/
	include 'eosinclude:constants.inc'

	if(abs(latd) .ne. 90.0d0) then
	    ToMill = log(tan(0.4d0*latd*rads + .25*pi))/rads
     .				/0.8d0/1.46639635d0
	    if(ToMill .gt. 90.0d0) then
		ToMill = 90.0d0
	    else if(ToMill .lt. -90.0d0) then
		ToMill = -90.0d0
	    end if
	else
	    ToMill = latd
	end if

	end
