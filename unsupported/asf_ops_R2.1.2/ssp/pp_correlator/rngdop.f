c SccsId = @(#)rngdop.f	2.41 3/24/98


	subroutine rngdop(xs,ys,zs,vxs,vys,vzs,xt,yt,zt,
     1			rst,fd,lambda)

        implicit none

        character*128 SccsId_rngdop
        data SccsId_rngdop
     +  /'@(#)PPrngdop.f:2.41'/


	real*8 lambda

	real*8 vyt,vzt,drx,vxt,u1,u2,u3,dvz,vdotr,rel_r,dvy,dry
	real*8 drz,dvx,vt,vys,vzs,xt,vxs,xs,ys,zs,pi,omega_e
	real*8 rt,fd,yt,zt,rst

	pi = 4.*atan(1.)	
	omega_e = 360.9856296d0 / (24.0d0 * 3600.0d0 )
c....earth rotation rate (radian/second), angular velocity 
	omega_e = omega_e * pi / 180.  
c....satellite to target distance
	rst = sqrt((xs-xt)**2+(ys-yt)**2+(zs-zt)**2)
c....target vector projected on x-y plane
	rt = sqrt(xt**2+yt**2)
c....target speed in x-y plane 
	vt = omega_e * rt

c....unit vector perpendicular to target vector and z-axis (unit velocity 
c....vector of vt) 
	call cross(0.d0,0.d0,1.d0,xt,yt,zt,u1,u2,u3)
c....vt's x, y, and z component
	vxt = vt * u1
	vyt = vt * u2
	vzt = vt * u3

	drx = xt - xs
	dry = yt - ys
	drz = zt - zs

	dvx = vxt - vxs
	dvy = vyt - vys
	dvz = vzt - vzs

	vdotr= dvx*drx +dvy*dry +dvz*drz
	rel_r = sqrt ( drx**2 + dry**2 + drz**2 )

	fd = -2 * vdotr /(lambda * rel_r)

	return
	end

