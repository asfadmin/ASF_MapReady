
*.
*   Vectors - 3D vector operations
*
*	vEqual - set a vector equal to another
*	vSet - set values of vector components
*	vAdd - add two vectors
*	vSub - find the difference between two vectors
*	vMult - multiply a vector by a scalar
*	vDot - find the dot product of two vectors
*	vCross - find the cross product of two vectors
*	vUnit - normalize a vector
*	vCos - find direction cosine between two vectors
*	vAng - find the angle between two vectors
*	vTriple - find the scalar triple product of three vectors
*	vProject - transform a vector into another coordinate system
*	vPerp - find a vector perpendicular to another vector
*
*	08/02/89 12:29
*..
	subroutine vEqual(x,y)
	double precision x(3),y(3)

	  character*100 SccsFileID
     -/'@(#)vectors.for	5.1 98/01/08 APS/ASF\0'/
	x(1) = y(1)
	x(2) = y(2)
	x(3) = y(3)

	end

	subroutine vSet(x1,x,y,z)
	double precision x1(3),x,y,z

	x1(1) = x
	x1(2) = y
	x1(3) = z

	end


	subroutine vAdd(x1,x2,x)
	double precision x1(3),x2(3),x(3)

	x(1) = x1(1) + x2(1)
	x(2) = x1(2) + x2(2)
	x(3) = x1(3) + x2(3)

	end


	subroutine vSub(x1,x2,x)
	double precision x1(3),x2(3),x(3)

	x(1) = x1(1) - x2(1)
	x(2) = x1(2) - x2(2)
	x(3) = x1(3) - x2(3)

	end


	subroutine vMult(c,x1,x)
	double precision c,x1(3),x(3)

	x(1) = c*x1(1)
	x(2) = c*x1(2)
	x(3) = c*x1(3)

	end


	double precision function vMag(x)
	double precision x(3)

	vMag = sqrt(x(1)*x(1) + x(2)*x(2) + x(3)*x(3))

	end


	double precision function vDot(x1,x2)
	double precision x1(3),x2(3)

	vDot = x1(1)*x2(1) + x1(2)*x2(2) + x1(3)*x2(3)

	end


	subroutine vCross(x1,x2,x)
	double precision x1(3),x2(3),x(3)

	x(1) = x1(2)*x2(3) - x1(3)*x2(2)
	x(2) = x1(3)*x2(1) - x1(1)*x2(3)
	x(3) = x1(1)*x2(2) - x1(2)*x2(1)

	end


	subroutine vUnit(x1,x)
	double precision x1(3),x(3)

	double precision vm

	double precision vMag

	vm = vMag(x1)
	if(vm.gt.0.0) then
	    call vMult(1.0d0/vm,x1,x)
	else
	    call ErrorMessage('vUnit - zero-length vector')
	end if

	end


	double precision function vCos(x1,x2)
	double precision x1(3),x2(3)

	double precision vDot,vMag

	double precision mx1,mx2

	mx1 = vMag(x1)
	mx2 = vMag(x2)

	if(mx1.ne.0.0 .and. mx2.ne.0.0) then
	    vCos = vDot(x1,x2)/(mx1*mx2)
	else
	    vCos = 0.0
	end if

	end


	double precision function vAng(x1,x2)
	double precision x1(3),x2(3)

	double precision vDot,vMag

	double precision mx1,mx2,arg

	mx1 = vMag(x1)
	mx2 = vMag(x2)

	if(mx1.ne.0.0 .and. mx2.ne.0.0) then
	    arg = vDot(x1,x2)/(mx1*mx2)
	    if(arg.gt.1.0) then
		arg = 1.0
	    else if(arg.lt.-1.0) then
		arg = -1.0
	    end if
	    vAng = acos(arg)
	else
	    vAng = 0.0
	end if

	end


	double precision function vTriple(x,y,z)
	double precision x(3),y(3),z(3)

	double precision w(3),vDot

	call vCross(x,y,w)
	vTriple = vDot(w,z)

	end


	subroutine vProject(x1,x,y,z,x2)
	double precision x1(3),x(3),y(3),z(3),x2(3)

	double precision temp1(3),temp2(3),temp3(3),temp4(3)

*	The axes of the new coordinate system are the unit vectors x,y,z (in
*	terms of the old coordinate system).  Project x1 from the old system to
*	the new one, resulting in x2

	call vMult(x1(1),x,temp1)
	call vMult(x1(2),y,temp2)
	call vMult(x1(3),z,temp3)

	call vAdd(temp1,temp2,temp4)
	call vAdd(temp3,temp4,x2)

	end


	subroutine vPerp(x,y)
	include 'eosinclude:constants.inc'
	double precision x(3),y(3)

	double precision xunit(3),xnew(3),ytemp(3)

	call vUnit(x,xunit)
	if(xunit(1).ne.1.0 .or. xunit(2).ne.0.0 .or. xunit(3).ne.0.0) then
	    call vSet(xnew,xunit(1),xunit(3),-xunit(2))
	    call vCross(xunit,xnew,ytemp)
	    call vUnit(ytemp,y)
	else
c..aap	    call vSet(y,0.0,1.0,0.0)
	    call vSet(y,dzero,d_one,dzero)
	end if

	end
