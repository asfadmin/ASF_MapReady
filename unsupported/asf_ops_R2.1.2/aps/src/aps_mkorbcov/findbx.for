*.
*   FindBX - finds the beamwidth of an instrument when only the
*	  swathwidth is known
*
*   Args
*	latd		double	input	geodetic latitude (deg)
*	altd		double	input	geodetic altitude (km)
*	look		double	input	off-nadir angle (deg)
*	FindBX		double	return
*
*	08/03/89 09:47
*..
	double precision function FindBX(latd,altd,look)
	include 'eosinclude:constants.inc'
	double precision latd,altd,look

	double precision latc,rlatc,r,r2,rnad,rlook,matharg,phi

** Functions
	double precision rnadir,tolatc

	  character*100 SccsFileID
     -/'@(#)findbx.for	5.1 98/01/08 APS/ASF\0'/

	latc = tolatc(latd)
	rlatc = latc*rads
	rlook = look*rads

	rnad = rnadir(rlatc)
	r2 = altd**2 + rnad**2 + 2*altd*rnad*cos((latd-latc)*rads)
	if(r2.ge.0.0) then
	    r = sqrt(r2)
	else
	    r = 0.0
	end if

	matharg = r * sin(rlook) / rnad
	if(abs(matharg).gt.1.0) matharg = sign(1.0d0,matharg)
	phi = asin(matharg) - rlook

c	FindBX = 2.0 * rnad * sin(0.5d0*phi)
	FindBX = rnad * sin(phi)	! 1/20/85 11:30

	end

