
*.
*   QtoHSS - find sunsynchronous altitude given Q, the number of orbits per day
*
*   Args
*	Q		double	input	Q, orbits per day
*	h		double	output	average altitude (a-RBody) (km)
*	i		double	output	sun-synchrounous inclination
*
*   Assumes
*	Central body has been set (and is oblate)
*
*	06/08/90 converted QTOHTABLE to a subroutine
*	08/09/89
*
*	06/08/90
*..
	subroutine QtoHSS(Q,h,i)
** Includes
	include 'eosinclude:constants.inc'
	include 'eosinclude:cbody.inc'
** Variables
	double precision Q
	double precision NodeRate,n,k
	double precision a,h,i,Period,NodePeriod
	double precision NodePeriodguess,guessdiff

	  character*100 SccsFileID
     -/'@(#)qtohss.for	5.1 98/01/08 APS/ASF\0'/
** Begin

c Sun-synchronous orbit --> NodeRate

	NodeRate = twopi / SecPerYear

	NodePeriod = twopi / (Q * (RotRate-NodeRate))
	
c Find a
	n = twopi / NodePeriod
	
200	a = (mu/n**2)**(1.0d0/3.0d0)
	k = 1.5d0*j2*(Rbody/a)**2
	NodePeriodguess = (twopi/n)
     .		* (1.0d0-((4.0d0*NodeRate**2)/(n**2*k)-k))
	guessdiff = NodePeriod - NodePeriodguess
	if(dabs(guessdiff).gt.1.0d-2) then
	    n = n * (1.0 - guessdiff/NodePeriod)
	    go to 200
	end if

	h = a - Rbody
	i = acos(-NodeRate/(n*k))

	end
