*.
*   GlintAng - find geocentric angle between s/c nadir and sun glitter point
*
*   Args
*	rsc		double	input	s/c position vector (km)
*	sublatd		double	input	subsolar latd (deg)
*	sublon		double	input	subsolar lon (deg)
*	GlintPhi	double	output	geoc. angle from nadir to glint (rad)
*	theta		double	output	look angle to glitter point (rad)
*	rnad		double	output	radius of earth at nadir
*
*	10/17/88 11:01
*..
	subroutine GlintAng(rsc,sublatd,sublon,GlintPhi,theta,rnad)
	include 'eosinclude:constants.inc'
	include 'eosinclude:cbody.inc'
	double precision rsc(3),sublatd,sublon,GlintPhi,theta

	  character*100 SccsFileID
     -/'@(#)glintang.for	5.1 98/01/08 APS/ASF\0'/
	double precision latdsc,lonsc,altdsc
	double precision alpha,MaxTheta
	double precision rmag,rnad

	double precision f,fprime
	double precision h,tol
	integer iter,MaxIter
	double precision coeff,sumang

c Functions
	double precision DiffAng,ToLatc,RNadir,vMag


	call XYZSphere(rsc,latdsc,lonsc,altdsc)
	alpha = DiffAng(latdsc,lonsc,sublatd,sublon)

	rnad = RNadir(ToLatc(Latdsc))
	rmag = vMag(rsc)
	MaxTheta = asin(rnad/rmag)

c Sun too far away, glitter point not visible

	if(alpha.gt.(halfpi-MaxTheta)) then
	    theta = pi
	    GlintPhi = pi
	    go to 200
	end if

c Set up guess for theta, tol for Newton iteration

	theta = MaxTheta/2.0
	tol = 0.01d0 * rads
	MaxIter = 1000
	coeff = rmag/rnad

c Use Newton iteration to find theta

	do iter=1,MaxIter
	    sumang = (alpha + theta) / 2.0
	    f = coeff * sin(theta) - sin(sumang)
	    fprime = coeff * cos(theta) - 0.5 * cos(sumang)
	    h = -f/fprime
	    if(abs(h).lt.tol) go to 100
	    theta = theta + h
	end do

	call ErrorMessage('GlintAng - no convergence')
100	continue

	GlintPhi = asin(coeff * sin(theta)) - theta

200	continue
	end
