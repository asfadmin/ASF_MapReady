
*.
*   ViewPoint - find the intersection of a ray from a point and a spheroid
*
*   Args
*	r		double	input	position vector
*	xp		double	input	x-axis orientation vector at point
*	yp		double	input	y-axis orientation vector at point
*	zp		double	input	z-axis orientation vector at point
*	look		double	input	off-z-axis angle of ray (rad)
*	yaw		double	input	angle from x-axis about z-axis of ray (rad)
*	s		double	output	intersection vector
*	err		logical	output	set true if no solution exists
*
*   Assumptions
*	Central body has been set
*	Central body is oblate - a = b > c
*
*	07/12/89
*
*	07/12/89 11:44
*..
	subroutine ViewPoint(r,xp,yp,zp,look,yaw,s,err)
* Includes
	include 'eosinclude:cbody.inc'
* Args
	double precision r(3),xp(3),yp(3),zp(3),look,yaw,s(3)
	logical err

	  character*100 SccsFileID
     -/'@(#)viewpoint.for	5.1 98/01/08 APS/ASF\0'/
	double precision px,py,pz,p(3),plen
	double precision Ra2,Rb2,Rc2
	double precision a,b,c,disc,rootdisc
	double precision t1,t2,t

	integer i


* Find pointing vector in body coordinates

	px = cos(yaw) * sin(look)
	py = sin(yaw) * sin(look)
	pz = cos(look)

* Find pointing vector in central body coordinates

	do i=1,3
	    p(i) = px * xp(i) + py * yp(i) + pz * zp(i)
	end do
	plen = sqrt(p(1)*p(1) + p(2)*p(2) + p(3)*p(3))
	do i=1,3
	    p(i) = p(i) / plen
	end do

* Find intersection: solve at**2 + bt + c = 0, where r + tp = s

	Ra2 = RBody*RBody
	Rb2 = Ra2
	Rc2 = (RBody * (1.0 - flat))**2

	a = p(1)*p(1) / Ra2 + p(2)*p(2) / Rb2 + p(3)*p(3) / Rc2
	b = 2.0 * (p(1)*r(1) / Ra2 + p(2)*r(2) / Rb2 + p(3)*r(3) / Rc2)
	c = r(1)*r(1) / Ra2 + r(2)*r(2) / Rb2 + r(3)*r(3) / Rc2 - 1.0

	disc = b*b - 4.0 * a * c

	if(disc.ge.0.0) then
	    rootdisc = sqrt(disc)
	    t1 = (-b + rootdisc) / (2.0 * a)
	    t2 = (-b - rootdisc) / (2.0 * a)
	    t = min(t1,t2)
	    do i=1,3
		s(i) = r(i) + t * p(i)
	    end do
	    err = .false.
	else
	    err = .true.
	end if

	end
