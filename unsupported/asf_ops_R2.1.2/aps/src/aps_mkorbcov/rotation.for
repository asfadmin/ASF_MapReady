
*.
*   Rotation.for - coordinate axis rotations
*
*	xRot - rotate y toward z about x by theta (rad)
*	yRot - rotate z toward x about y by theta (rad)
*	zRot - rotate x toward y about z by theta (rad)
*
*   Args
*	x(3)			double	input	3-D coordinate input
*	theta			double	input	rotation angle (rad)
*	xx(3)			double	output	3-D coordinate output
*
*	10/05/88 15:43
*..
	subroutine xRot(x,theta,xx)
	double precision x(3),theta,xx(3)

	  character*100 SccsFileID
     -/'@(#)rotation.for	5.1 98/01/08 APS/ASF\0'/
	xx(1) = x(1)
	xx(2) =  cos(theta)*x(2) + sin(theta)*x(3)
	xx(3) = -sin(theta)*x(2) + cos(theta)*x(3)

	end


	subroutine yRot(x,theta,xx)
	double precision x(3),theta,xx(3)

	xx(1) = cos(theta)*x(1) - sin(theta)*x(3)
	xx(2) = x(2)
	xx(3) = sin(theta)*x(1) + cos(theta)*x(3)

	end


	subroutine zRot(x,theta,xx)
	double precision x(3),theta,xx(3)

	xx(1) =  cos(theta)*x(1) + sin(theta)*x(2)
	xx(2) = -sin(theta)*x(1) + cos(theta)*x(2)
	xx(3) = x(3)

	end
