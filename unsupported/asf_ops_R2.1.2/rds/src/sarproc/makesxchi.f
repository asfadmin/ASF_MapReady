	subroutine makes_chi (statevec,s)           
c/*	subroutine makes_chi (statevec,s)   ----------------------
c
c Author:	Chong-Yung Chi
c Date	:	7-5-88
c
c
c  SUBROUTINE to construct the S transformation matrix that transforms
c  vectors from S/C centered (noninertial) to E/C (inertial) systems.
c 
c       Input:  statevec = 6 element state vector of s/c  x,y,z,xdot,ydot,zdot
c	Output: s = 3x3 rotation matrix
c 
c   Coordinate system of statevector is in Earth Centered system
c
c *****************************************************
c*/
	implicit double precision (a-h,o-z)
	double precision statevec(6),s(3,3),r(3),v(3),ax(3),ay(3),az(3)
      character*80 sccsid
      data sccsid /'@(#)makesxchi.f	1.3 96/04/09 22:51:52\0'/
	sr = 0.
	do i = 1, 3
	  r(i) = statevec(i)
	  sr = sr + r(i) * r(i)
	  v(i) = statevec(i+3)
	end do
	sr = dsqrt(sr)
c
c compute the unit vector on the Z' axis, or the third column of S.
c
	do i = 1, 3
	  az(i) = r(i) / sr
	end do
c
c compute the unit vector on the Y' axis, or the second column of S.
c
	call CROSS_2(az,v,ay)
	  sr = 0.
	do i = 1, 3
	  sr = sr + ay(i) * ay(i)
	end do
	  sr = dsqrt(sr)
	do i = 1, 3
	  ay(i) = ay(i) / sr
	end do
c
c compute the unit vector on the X' axis, or the first column of S.
c
	call cross_2(ay, az, ax)
c
	do i = 1, 3
	  s(i,1) = ax(i)
	  s(i,2) = ay(i)
	  s(i,3) = az(i)
	end do
	return
	end

