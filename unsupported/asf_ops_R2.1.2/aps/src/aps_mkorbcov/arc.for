*.
*   Arc - draw an arc centered at display coordinates (xc,yc) of radius r
*		from theta1 to theta2
*
*   Args
*	xc,yc		integer*2	input	display coords of arc center
*	r		double		input	radius of arc
*	theta1,theta2	double		input	start and end angle (deg)
*
*	08/02/89 12:47
*..
	subroutine arc(xc,yc,r,theta1,theta2)
	include 'eosinclude:display.inc'
	include 'eosinclude:constants.inc'
	integer xc,yc
	double precision r,theta1,theta2
      character*100 SccsFileID
     -/'@(#)arc.for	5.1 98/01/08 APS/ASF\0'/

	double precision theta,th1,th2,dth
	integer i,MAXI
	parameter(MAXI=100)
	integer x,y,xy(MAXI)
	logical clip

c     dth = 3/r on RAM9460
	dth = min(abs(swidex),abs(swidey))/(300.0d0*r)
	dth = sign(dth,theta2-theta1)
	th1 = theta1*rads
	th2 = theta2*rads

	i = -1

	do theta=th1,th2,dth
	    x = devscalex * r * cos(theta) + xc
	    y = devscaley * r * sin(theta) + yc
	    call checkscreen(x,y,clip)
	    if(.not.clip) then
		i = i + 2
		xy(i) = devscalex * r * cos(theta) + xc
		xy(i+1) = -devscaley * r * sin(theta) + yc
		if(i.eq.MAXI-1) then
		    call vector(xy,(i+1)/2)
		    xy(1) = xy(i)
		    xy(2) = xy(i+1)
		    i = 1
		end if
	    else
		if(i.gt.1) call vector(xy,(i+1)/2)
		i = - 1
	    end if
	end do

	if(i.gt.1) call vector(xy,(i+1)/2)

	end
